/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#include "insieme/frontend/convert.h"

#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/annotations/c/location.h"


#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/ocl/ocl_compiler.h"

#include "insieme/frontend/pragma/insieme.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/frontend/cpp/temporary_handler.h"
#include "insieme/annotations/c/naming.h"

#include "clang/AST/StmtVisitor.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"

#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>

#include <clang/AST/CXXInheritance.h>

#include "clang/Basic/FileManager.h"

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace std {

std::ostream& operator<<(std::ostream& out, const clang::FunctionDecl* funcDecl) {
	return out << funcDecl->getNameAsString() << "(" << funcDecl->param_size() << ")";
}

} // end std namespace

#define GET_REF_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::RefType>(type)->getElementType())

#define GET_VEC_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::VectorType>(type)->getElementType())

#define GET_ARRAY_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::ArrayType>(type)->getElementType())

#define LOG_CONVERSION(retIr) \
	FinalActions attachLog( [&] () { END_LOG_EXPR_CONVERSION(retIr); } )

namespace {
// FIXME 
// Covert clang source location into a annotations::c::SourceLocation object to be inserted in an CLocAnnotation
annotations::c::SourceLocation convertClangSrcLoc(clang::SourceManager& sm, const clang::SourceLocation& loc) {

	clang::SourceLocation cloc = loc;

	if (sm.isMacroArgExpansion(cloc)) {
		cloc = sm.getExpansionLoc(cloc);
	}

	clang::FileID&& fileId = sm.getFileID( sm.getSpellingLoc(cloc) );
	const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
	assert(fileEntry && "File cannot be NULL");

	return annotations::c::SourceLocation(fileEntry->getName(), sm.getExpansionLineNumber(cloc),
			sm.getExpansionColumnNumber(cloc));
}

// Returns a string of the text within the source range of the input stream
std::string GetStringFromStream(const SourceManager& srcMgr, const SourceLocation& start) {
	/*
	 *  we use the getDecomposedSpellingLoc() method because in case we read macros values we have
	 *  to read the expanded value
	 */
	std::pair<FileID, unsigned>&& startLocInfo = srcMgr.getDecomposedSpellingLoc(start);
	llvm::StringRef&& startBuffer = srcMgr.getBufferData(startLocInfo.first);
	const char *strDataStart = startBuffer.begin() + startLocInfo.second;

	return string(strDataStart,
			clang::Lexer::MeasureTokenLength(srcMgr.getSpellingLoc(start), srcMgr, clang::LangOptions())
	);
}

/*
 * In case the the last argument of the function is a var_arg, we try pack the exceeding arguments
 * with the pack operation provided by the IR.
 */
vector<core::ExpressionPtr> tryPack(const core::IRBuilder& builder, core::FunctionTypePtr funcTy,
		const ExpressionList& args) {

	// check if the function type ends with a VAR_LIST type
	const core::TypeList& argsTy = funcTy->getParameterTypes()->getElements();
	// assert(argsTy && "Function argument is of not type TupleType");

	// if the tuple type is empty it means we cannot pack any of the arguments
	if (argsTy.empty()) {
		return args;
	}

	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	if (gen.isVarList(argsTy.back())) {
		ExpressionList ret;
		assert(args.size() >= argsTy.size()-1 && "Function called with fewer arguments than necessary");
		// last type is a var_list, we have to do the packing of arguments

		// we copy the first N-1 arguments, the remaining will be unpacked
		std::copy(args.begin(), args.begin() + argsTy.size() - 1, std::back_inserter(ret));

		ExpressionList toPack;
		if (args.size() > argsTy.size() - 1) {
			std::copy(args.begin() + argsTy.size() - 1, args.end(), std::back_inserter(toPack));
		}

		// arguments has to be packed into a tuple expression, and then inserted into a pack expression
		ret.push_back(builder.callExpr(gen.getVarList(), gen.getVarlistPack(), builder.tupleExpr(toPack)));
		return ret;
	}
	return args;
}

core::CallExprPtr getSizeOfType(const core::IRBuilder& builder, const core::TypePtr& type) {
	core::LiteralPtr size;

	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(type)) {
		return builder.callExpr(gen.getUnsignedIntMul(), builder.literal(gen.getUInt8(), toString(*(vecTy->getSize()))),
				getSizeOfType(builder, vecTy->getElementType()));
	}
	// in case of ref<'a>, recurr on 'a
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
		return getSizeOfType(builder, refTy->getElementType());
	}

	return builder.callExpr(gen.getSizeof(), builder.getTypeLiteral(type));
}

/**
 * Special method which handle malloc and calloc which need to be treated in a special way in the IR. 
 */
core::ExpressionPtr handleMemAlloc(const core::IRBuilder& builder, const core::TypePtr& type,
		const core::ExpressionPtr& subExpr) {
	if ( core::CallExprPtr&& callExpr = core::dynamic_pointer_cast<const core::CallExpr>(subExpr)) {

		if ( core::LiteralPtr&& lit = core::dynamic_pointer_cast<const core::Literal>(callExpr->getFunctionExpr())) {

			if (!(lit->getStringValue() == "malloc" || lit->getStringValue() == "calloc")) {
				return core::ExpressionPtr();
			}

			assert(
					((lit->getStringValue() == "malloc" && callExpr->getArguments().size() == 1) || (lit->getStringValue() == "calloc" && callExpr->getArguments().size() == 2)) && "malloc() and calloc() takes respectively 1 and 2 arguments");

			const core::lang::BasicGenerator& gen = builder.getLangBasic();
			// The type of the cast should be ref<array<'a>>, and the sizeof('a) need to be derived
			assert(type->getNodeType() == core::NT_RefType);
			assert(core::analysis::getReferencedType(type)->getNodeType() == core::NT_ArrayType);

			const core::RefTypePtr& refType = core::static_pointer_cast<const core::RefType>(type);
			const core::ArrayTypePtr& arrayType = refType->getElementType().as<core::ArrayTypePtr>();
			const core::TypePtr& elemType = arrayType->getElementType();

			/*
			 * The number of elements to be allocated of type 'targetType' is:
			 * 		-> 	expr / sizeof(targetType)
			 */
			core::CallExprPtr&& size = builder.callExpr(
					gen.getUInt8(),
					gen.getUnsignedIntDiv(),
					callExpr->getArguments().front(),
					getSizeOfType(builder, elemType)
			);

			// FIXME: calloc also initialize the memory to 0
			return builder.refNew(
					builder.callExpr(arrayType, gen.getArrayCreate1D(), builder.getTypeLiteral(elemType), size));
		}
	}
	return core::ExpressionPtr();
}

core::ExpressionPtr getCArrayElemRef(const core::IRBuilder& builder, const core::ExpressionPtr& expr) {
	const core::TypePtr& exprTy = expr->getType();
	if (exprTy->getNodeType() == core::NT_RefType) {
		const core::TypePtr& subTy = GET_REF_ELEM_TYPE(exprTy);

		if (subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) {
			core::TypePtr elemTy = core::static_pointer_cast<const core::SingleElementType>(subTy)->getElementType();

			return builder.callExpr(
					builder.refType(elemTy),
					(subTy->getNodeType() == core::NT_VectorType ?
							builder.getLangBasic().getVectorRefElem() : builder.getLangBasic().getArrayRefElem1D()),
					expr, builder.uintLit(0));
		}
	}
	return expr;
}

core::ExpressionPtr scalarToVector(core::ExpressionPtr scalarExpr, core::TypePtr refVecTy,
		const core::IRBuilder& builder, const frontend::conversion::ConversionFactory& convFact) {
	const core::lang::BasicGenerator& gen = builder.getNodeManager().getLangBasic();
	const core::VectorTypePtr vecTy = convFact.tryDeref(refVecTy).as<core::VectorTypePtr>();

	core::CastExprPtr cast = core::dynamic_pointer_cast<const core::CastExpr>(scalarExpr);
	core::ExpressionPtr secondArg = cast ? cast->getSubExpression() : scalarExpr; // remove wrong casts added by clang
	if (*secondArg->getType() != *vecTy->getElementType()) // add correct cast (if needed)
		secondArg = builder.castExpr(vecTy->getElementType(), secondArg);

	return builder.callExpr(gen.getVectorInitUniform(), secondArg, builder.getIntTypeParamLiteral(vecTy->getSize()));
}

} // end anonymous namespace

namespace insieme {
namespace frontend {

namespace utils {

struct CallExprVisitor: public clang::StmtVisitor<CallExprVisitor> {

	clang::idx::Indexer& indexer;
	typedef std::set<const clang::FunctionDecl*> CallGraph;
	CallGraph callGraph;

	CallExprVisitor(clang::idx::Indexer& indexer) :
			indexer(indexer) {
	}

	CallGraph getCallGraph(const clang::FunctionDecl* func) {
		assert(func->hasBody() && "Function in the dependency graph has no body");

		Visit(func->getBody());
		return callGraph;
	}

	void addFunctionDecl(FunctionDecl* funcDecl) {
		const clang::FunctionDecl* def = NULL;
		/*
		 * this will find function definitions if they are declared in  the same translation unit
		 * (also defined as static)
		 */
		if (!funcDecl->hasBody(def)) {
			/*
			 * if the function is not defined in this translation unit, maybe it is defined in another we already
			 * loaded use the clang indexer to lookup the definition for this function declarations
			 */
			clang::idx::Entity&& funcEntity = clang::idx::Entity::get( funcDecl, indexer.getProgram() );
			conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
			if ( ret.first ) {def = ret.first;}
		}

		if (def) {
			callGraph.insert(def);
		}
	}

	void VisitCallExpr(clang::CallExpr* callExpr) {
		if (FunctionDecl * funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee())) {
			addFunctionDecl(funcDecl);
		}
		VisitStmt(callExpr);
	}

	void VisitDeclRefExpr(DeclRefExpr* expr) {
		// if this variable is used to invoke a function (therefore is a
		// function pointer) and it has been defined here, we add a potentially
		// dependency to the current definition 
		//if ( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(expr->getDecl()) ) {
		// addFunctionDecl(funcDecl);
		//}
	}

	void VisitStmt(clang::Stmt* stmt) {
		std::for_each(stmt->child_begin(), stmt->child_end(),
				[ this ](clang::Stmt* curr) {if(curr) this->Visit(curr);});
	}
};

struct CXXCallExprVisitor: public CallExprVisitor {

//	clang::idx::Indexer& indexer;
//	typedef std::set<const clang::FunctionDecl*> CallGraph;
//	CallGraph callGraph;

	CXXCallExprVisitor(clang::idx::Indexer& indexer) :
			CallExprVisitor(indexer) {
	}

	void VisitCXXConstructExpr(clang::CXXConstructExpr* ctorExpr) {
		// connects the constructor expression to the function graph
		addFunctionDecl(ctorExpr->getConstructor());
		VisitStmt(ctorExpr);

		// if there is an member with an initializer in the ctor we add it to the function graph
		clang::CXXConstructorDecl* constructorDecl = dyn_cast<CXXConstructorDecl>(ctorExpr->getConstructor());
		for (clang::CXXConstructorDecl::init_iterator iit = constructorDecl->init_begin(), iend =
				constructorDecl->init_end(); iit != iend; iit++) {
			clang::CXXCtorInitializer * initializer = *iit;

			if (initializer->isMemberInitializer()) {
				Visit(initializer->getInit());
			}
		}

		// if we construct a object there should be some kind of destructor
		// we have to add it to the function graph
		if ( CXXRecordDecl* classDecl = GET_TYPE_PTR(ctorExpr)->getAsCXXRecordDecl()) {
			CXXDestructorDecl* dtorDecl = classDecl->getDestructor();
			addFunctionDecl(dtorDecl);
		}
	}

	void VisitCXXNewExpr(clang::CXXNewExpr* callExpr) {

		//if there is an member with an initializer in the ctor we add it to the function graph
		if (clang::CXXConstructorDecl * constructorDecl = dyn_cast<CXXConstructorDecl>(callExpr->getConstructor())) {
			// connects the constructor expression to the function graph
			addFunctionDecl(constructorDecl);
			for (clang::CXXConstructorDecl::init_iterator iit = constructorDecl->init_begin(), iend =
					constructorDecl->init_end(); iit != iend; iit++) {
				clang::CXXCtorInitializer * initializer = *iit;

				if (initializer->isMemberInitializer()) {
					Visit(initializer->getInit());
				}
			}
		}

		VisitStmt(callExpr);
	}

	void VisitCXXDeleteExpr(clang::CXXDeleteExpr* callExpr) {
		addFunctionDecl(callExpr->getOperatorDelete());

		// if we delete a class object -> add destructor to function call
		if ( CXXRecordDecl* classDecl = callExpr->getDestroyedType()->getAsCXXRecordDecl()) {
			CXXDestructorDecl* dtorDecl = classDecl->getDestructor();
			addFunctionDecl(dtorDecl);
		}

		VisitStmt(callExpr);
	}

	void VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* mcExpr) {
		// connects the member call expression to the function graph
		//assert(false && "in next clang version");
		addFunctionDecl(dyn_cast<FunctionDecl>(mcExpr->getCalleeDecl()));
		VisitStmt(mcExpr);
	}

};

/**
 * In order for DepGraph to build the dependency graph for functions the clang indexer is needed,
 * FunctionDependencyGraph adds the indexer to member functions of DependencyGraph
 */
class FunctionDependencyGraph: public DependencyGraph<const clang::FunctionDecl*> {
	clang::idx::Indexer& idx;
public:
	CallExprVisitor& callExprVis;
	FunctionDependencyGraph(clang::idx::Indexer& idx, CallExprVisitor& cEV) :
			DependencyGraph<const clang::FunctionDecl*>(), idx(idx), callExprVis(cEV) {
	}
	clang::idx::Indexer& getIndexer() const {
		return idx;
	}
};

template<>
void DependencyGraph<const clang::FunctionDecl*>::Handle(const clang::FunctionDecl* func,
		const DependencyGraph<const clang::FunctionDecl*>::VertexTy& v) {
	// This is potentially dangerous
	FunctionDependencyGraph& funcDepGraph = static_cast<FunctionDependencyGraph&>(*this);


//	CallExprVisitor callExprVis(funcDepGraph.getIndexer());
//	CallExprVisitor::CallGraph&& graph = callExprVis.getCallGraph(func);

	//FIXME: add getter for callgraph to funcDepGraph
	utils::CallGraph&& graph = funcDepGraph.callExprVis.getCallGraph(func);

	std::for_each(graph.begin(), graph.end(),
			[ this, v ](const clang::FunctionDecl* currFunc) {assert(currFunc); this->addNode(currFunc, &v);});
}

} // end namespace utils

namespace conversion {

#define FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(ExprTy) \
	core::ExpressionPtr Visit##ExprTy( ExprTy* exp ) { return  convFact.convertCXXExpr(exp); }

#define START_LOG_EXPR_CONVERSION(expr) \
	assert(convFact.currTU && "Translation unit not correctly set"); \
	VLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting expression [class: '" << expr->getStmtClassName() << "']\n" \
			 << "-> at location: (" <<	\
				utils::location(expr->getLocStart(), convFact.currTU->getCompiler().getSourceManager()) << "): "; \
	if( VLOG_IS_ON(2) ) { \
		VLOG(2) << "Dump of clang expression: \n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		expr->dump(); \
	}

#define END_LOG_EXPR_CONVERSION(expr) \
	VLOG(1) << "Converted into IR expression: "; \
	VLOG(1) << "\t" << *expr << " type:( " << *expr->getType() << " )";

//---------------------------------------------------------------------------------------------------------------------
//										CLANG EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------
class ConversionFactory::ClangExprConverter: public StmtVisitor<ClangExprConverter, core::ExpressionPtr> {
protected:
	ConversionFactory& convFact;
	ConversionContext& ctx;

	core::ExpressionPtr wrapVariable(clang::Expr* expr) {
		const DeclRefExpr* ref = utils::skipSugar<const DeclRefExpr>(expr);
		if (ref && isa<const ParmVarDecl>(ref->getDecl())) {
			const core::VariablePtr& parmVar = core::static_pointer_cast<const core::Variable>(
					convFact.convertExpr(ref));

			auto fit = ctx.wrapRefMap.find(parmVar);
			if (fit == ctx.wrapRefMap.end()) {
				fit = ctx.wrapRefMap.insert(
						std::make_pair(parmVar,
								convFact.builder.variable(convFact.builder.refType(parmVar->getType())))).first;
			}
			return fit->second;
		}
		return convFact.convertExpr(expr);
	}

	core::ExpressionPtr asLValue(const core::ExpressionPtr& value) {
		const core::IRBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = convFact.mgr.getLangBasic();

		// this only works for call-expressions
		if (value->getNodeType() != core::NT_CallExpr || value->getType()->getNodeType() == core::NT_RefType) {
			return value;
		}

		// extract the call
		const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(value);

		// check final state - deref has been encountered => drop
		if (core::analysis::isCallOf(call, gen.getRefDeref())) {
			return call->getArgument(0);
		}

		// check whether it is a array-subscript instruction and argument has been de-refernced
		if (core::analysis::isCallOf(value, gen.getArraySubscript1D())) {
			const core::ExpressionPtr arg = call->getArgument(0);
			const core::ExpressionPtr inner = asLValue(arg);
			if (*inner != *arg) {
				return builder.callExpr(builder.refType(value->getType()), gen.getArrayRefElem1D(), inner,
						call->getArgument(1));
			}
		}

		// check whether it is a vector-subscript instruction and argument has been de-refernced
		if (core::analysis::isCallOf(value, gen.getVectorSubscript())) {
			const core::ExpressionPtr arg = call->getArgument(0);
			const core::ExpressionPtr inner = asLValue(arg);
			if (*inner != *arg) {
				return builder.callExpr(builder.refType(value->getType()), gen.getVectorRefElem(), inner,
						call->getArgument(1));
			}
		}

		// check whether it is a struct element access
		if (core::analysis::isCallOf(value, gen.getCompositeMemberAccess())) {
			const core::ExpressionPtr arg = call->getArgument(0);
			const core::ExpressionPtr inner = asLValue(arg);
			if (*inner != *arg) {
				return builder.callExpr(builder.refType(value->getType()), gen.getCompositeRefElem(), inner,
						call->getArgument(1), call->getArgument(2));
			}
		}

		// there is nothing to do
		return value;
	}

	core::ExpressionPtr asRValue(const core::ExpressionPtr& value) {

		// check whether value is parameter to the current function
		if (value->getNodeType() == core::NT_Variable) {
			core::VariablePtr var = static_pointer_cast<const core::Variable>(value);
			if (ctx.curParameter && contains(*ctx.curParameter, var)) {
				// => parameters are always r-values
				return var;
			}
		}

		// adds a deref to expression in case expression is of a ref type
		if (core::analysis::isRefType(value->getType())) {
			return convFact.builder.deref(value);
		}
		return value;
	}

public:
	// CallGraph for functions, used to resolved eventual recursive functions
	utils::FunctionDependencyGraph funcDepGraph;

	ClangExprConverter(ConversionFactory& convFact, Program& program) :
			convFact(convFact), ctx(convFact.ctx), funcDepGraph(program.getClangIndexer(), utils::CallExprVisitor(program.getClangIndexer())) {
	}
	virtual ~ClangExprConverter();

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								INTEGER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitIntegerLiteral(clang::IntegerLiteral* intLit) {
		START_LOG_EXPR_CONVERSION(intLit);

		core::ExpressionPtr retExpr;
		LOG_CONVERSION(retExpr);

		std::string&& strVal =
		GetStringFromStream( convFact.currTU->getCompiler().getSourceManager(), intLit->getExprLoc() );

		core::GenericTypePtr intTy = core::static_pointer_cast<const core::GenericType>(
				convFact.convertType(GET_TYPE_PTR(intLit)));

		return (retExpr = convFact.builder.literal(
		// retrieve the string representation from the source code
				strVal, intTy));
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FLOATING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitFloatingLiteral(clang::FloatingLiteral* floatLit) {
		START_LOG_EXPR_CONVERSION(floatLit);

		core::ExpressionPtr retExpr;
		LOG_CONVERSION(retExpr);

		return (retExpr =
		// retrieve the string representation from the source code
				convFact.builder.literal(
						GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(), floatLit->getExprLoc()),
						convFact.convertType(GET_TYPE_PTR(floatLit))));
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CHARACTER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCharacterLiteral(CharacterLiteral* charLit) {
		START_LOG_EXPR_CONVERSION(charLit);

		core::ExpressionPtr retExpr;
		LOG_CONVERSION(retExpr);

		return (retExpr = convFact.builder.literal(
				// retrieve the string representation from the source code
				GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(), charLit->getExprLoc()),
				(charLit->getKind() == CharacterLiteral::Wide ?
						convFact.mgr.getLangBasic().getWChar() : convFact.mgr.getLangBasic().getChar())));
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								STRING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitStringLiteral(clang::StringLiteral* stringLit) {
		START_LOG_EXPR_CONVERSION(stringLit);

		core::ExpressionPtr retExpr;
		LOG_CONVERSION(retExpr);

		std::string&& strValue = GetStringFromStream(
				convFact.currTU->getCompiler().getSourceManager(), stringLit->getExprLoc()
		);
		return (retExpr = convFact.builder.literal(strValue, convFact.mgr.getLangBasic().getString()));
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PARENTESIS EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitParenExpr(clang::ParenExpr* parExpr) {
		core::ExpressionPtr retExpr;

		LOG_CONVERSION(retExpr);
		return (retExpr = Visit(parExpr->getSubExpr()));
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					      GNU NULL EXPR EXPRESSION
	//
	// GNUNullExpr - Implements the GNU __null extension, which is a name for a
	// null pointer constant that has integral type (e.g., int or long) and is
	// the same size and alignment as a pointer. The __null extension is
	// typically only used by system headers, which define NULL as __null in
	// C++ rather than using 0 (which is an integer that may not match the size
	// of a pointer).
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitGNUNullExpr(clang::GNUNullExpr* nullExpr) {
		const core::lang::BasicGenerator& gen = convFact.mgr.getLangBasic();
		core::TypePtr&& type = convFact.convertType(GET_TYPE_PTR(nullExpr));
		assert(type->getNodeType() != core::NT_ArrayType && "C pointer type must of type array<'a,1>");
		return convFact.builder.callExpr(gen.getGetNull(), convFact.builder.getTypeLiteral(type));
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  IMPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	virtual core::ExpressionPtr VisitImplicitCastExpr(clang::ImplicitCastExpr* castExpr) {
		START_LOG_EXPR_CONVERSION(castExpr);
		const core::IRBuilder& builder = convFact.builder;

		core::ExpressionPtr retIr = Visit(castExpr->getSubExpr());
		LOG_CONVERSION(retIr);

		core::TypePtr classTypePtr; // used for CK_DerivedToBase
		core::StringValuePtr ident;

		// handle implicit casts according to their kind
		switch (castExpr->getCastKind()) {
		//case CK_ArrayToPointerDecay:
		//	return retIr;
		case CK_LValueToRValue:
			return (retIr = asRValue(retIr));

		case CK_UncheckedDerivedToBase:
		case CK_DerivedToBase: {
			// add CArray access
			if (GET_TYPE_PTR(castExpr)->isPointerType() && GET_TYPE_PTR(castExpr->getSubExpr())->isPointerType()) {
				//VLOG(2) << retIr;
				// deref not needed??? (Unchecked)DerviedToBase gets deref from LValueToRValue cast?
				//retIr = builder.deref(retIr);
				retIr = getCArrayElemRef(builder, retIr);
			}

			// for an inheritance like D -> C -> B -> A , and a cast of D to A
			// there is only one ImplicitCastExpr from clang, so we walk trough the inheritance
			// and create the member access. the iterator is in order so one gets C then B then A
			for (CastExpr::path_iterator I = castExpr->path_begin(), E = castExpr->path_end(); I != E; ++I) {
				const CXXBaseSpecifier* base = *I;
				const CXXRecordDecl* recordDecl = cast<CXXRecordDecl>(base->getType()->getAs<RecordType>()->getDecl());

				// find the class type - if not converted yet, converts and adds it
				classTypePtr = convFact.convertType(GET_TYPE_PTR(base));
				assert(classTypePtr && "no class declaration to type pointer mapping");

				//VLOG(2) << "member name " << recordDecl->getName().data();
				ident = builder.stringValue(recordDecl->getName().data());

				VLOG(2)
					<< "(Unchecked)DerivedToBase Cast on " << classTypePtr;

				core::ExpressionPtr op = builder.getLangBasic().getCompositeMemberAccess();
				core::TypePtr structTy = retIr->getType();

				if (structTy->getNodeType() == core::NT_RefType) {
					// skip over reference wrapper
					structTy = core::analysis::getReferencedType(structTy);
					op = builder.getLangBasic().getCompositeRefElem();
				}
				VLOG(2)
					<< structTy;

				const core::TypePtr& memberTy =
						core::static_pointer_cast<const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);
				core::TypePtr resType = builder.refType(classTypePtr);

				retIr = builder.callExpr(resType, op, retIr, builder.getIdentifierLiteral(ident),
						builder.getTypeLiteral(memberTy));
				VLOG(2)
					<< retIr;
			}
			return retIr;
		}

		case CK_NoOp:
			//CK_NoOp - A conversion which does not affect the type other than (possibly) adding qualifiers. int -> int char** -> const char * const *
			VLOG(2)
				<< "NoOp Cast";
			return retIr;

		default:
			// use default cast expr handling (fallback)
			return (retIr = VisitCastExpr(castExpr));
		}
		assert(false);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						EXPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	virtual core::ExpressionPtr VisitExplicitCastExpr(clang::ExplicitCastExpr* castExpr) {
		START_LOG_EXPR_CONVERSION(castExpr);

		const core::IRBuilder& builder = convFact.builder;
		core::ExpressionPtr retIr = Visit(castExpr->getSubExpr());
		LOG_CONVERSION(retIr);

		core::TypePtr classTypePtr; // used for CK_DerivedToBase
		core::StringValuePtr ident;
		VLOG(2)
			<< retIr << " " << retIr->getType();
		switch (castExpr->getCastKind()) {
		//case CK_ArrayToPointerDecay:
		//	return retIr;
		case CK_NoOp:
			//CK_NoOp - A conversion which does not affect the type other than (possibly) adding qualifiers. int -> int char** -> const char * const *
			VLOG(2)
				<< "NoOp Cast";
			return retIr;
		case CK_BaseToDerived: {
			VLOG(2)
				<< convFact.convertType(GET_TYPE_PTR(castExpr));

			// find the class type - if not converted yet, converts and adds it
			classTypePtr = convFact.convertType(GET_TYPE_PTR(castExpr));
			assert(classTypePtr && "no class declaration to type pointer mapping");

			VLOG(2)
				<< "BaseToDerived Cast" << classTypePtr;

			// explicitly cast base to derived with CAST-operator in IR
			if (GET_TYPE_PTR(castExpr)->isPointerType() && GET_TYPE_PTR(castExpr->getSubExpr())->isPointerType()) {
				retIr = builder.castExpr(classTypePtr, retIr);
			} else {
				retIr = builder.castExpr(builder.refType(classTypePtr), retIr);
			}
			return retIr;
		}

		case CK_DerivedToBase: {
			// pointer types (in IR) are ref<ref<array -> get deref first ref, and add CArray access
			if (GET_TYPE_PTR(castExpr)->isPointerType() && GET_TYPE_PTR(castExpr->getSubExpr())->isPointerType()) {
				//VLOG(2) << retIr;
				retIr = builder.deref(retIr);
				retIr = getCArrayElemRef(builder, retIr);
			}

			// for an inheritance like D -> C -> B -> A , and a cast of D to A
			// there is only one ExplicitCastExpr from clang, so we walk trough the inheritance
			// and create the member access. the iterator is in order so one gets C then B then A
			for (CastExpr::path_iterator I = castExpr->path_begin(), E = castExpr->path_end(); I != E; ++I) {
				const CXXBaseSpecifier* base = *I;
				const CXXRecordDecl* recordDecl = cast<CXXRecordDecl>(base->getType()->getAs<RecordType>()->getDecl());

				// find the class type - if not converted yet, converts and adds it
				classTypePtr = convFact.convertType(GET_TYPE_PTR(base));
				assert(classTypePtr && "no class declaration to type pointer mapping");

				VLOG(2)
					<< "member name " << recordDecl->getName().data();
				ident = builder.stringValue(recordDecl->getName().data());

				VLOG(2)
					<< "DerivedToBase Cast on " << classTypePtr;

				core::ExpressionPtr op = builder.getLangBasic().getCompositeMemberAccess();
				core::TypePtr structTy = retIr->getType();

				if (structTy->getNodeType() == core::NT_RefType) {
					// skip over reference wrapper
					structTy = core::analysis::getReferencedType(structTy);
					op = builder.getLangBasic().getCompositeRefElem();
				}
				VLOG(2)
					<< structTy;

				const core::TypePtr& memberTy =
						core::static_pointer_cast<const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);

				core::TypePtr resType = builder.refType(classTypePtr);

				retIr = builder.callExpr(resType, op, retIr, builder.getIdentifierLiteral(ident),
						builder.getTypeLiteral(memberTy));
				VLOG(2)
					<< retIr;
			}
			return retIr;
		}
		case CK_ConstructorConversion: {

			return retIr;
		}
		default:
			// use default cast expr handling (fallback)
			return (retIr = VisitCastExpr(castExpr));
		}

		assert(false);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCastExpr(clang::CastExpr* castExpr) {
		START_LOG_EXPR_CONVERSION(castExpr);

		const core::lang::BasicGenerator& gen = convFact.mgr.getLangBasic();
		const core::TypePtr& type = convFact.convertType(GET_TYPE_PTR(castExpr));

		core::ExpressionPtr retIr;
		LOG_CONVERSION(retIr);

		retIr = Visit(castExpr->getSubExpr());
		core::ExpressionPtr&& nonRefExpr = convFact.tryDeref(retIr);

		// if the cast is to a 'void*' type then we simply skip it
		if (gen.isAnyRef(type)) {
			return retIr;
		}

		if ((type->getNodeType() == core::NT_RefType) && (*retIr == *convFact.builder.literal(retIr->getType(), "0"))) {
			return (retIr = convFact.builder.callExpr(gen.getGetNull(),
					convFact.builder.getTypeLiteral(GET_REF_ELEM_TYPE(type))));
		}

		// Mallocs/Allocs are replaced with ref.new expression
		if (core::ExpressionPtr&& retExpr = handleMemAlloc(convFact.getIRBuilder(), type, retIr))
			return (retIr = retExpr);

		// If the subexpression is a string, remove the implicit casts
		if (convFact.mgr.getLangBasic().isString(retIr->getType())) {
			return retIr;
		}

		const core::TypePtr& nonRefType = nonRefExpr->getType();
		// if the subexpression is an array or a vector, remove all the C implicit casts
		if (nonRefType->getNodeType() == core::NT_ArrayType || nonRefType->getNodeType() == core::NT_VectorType
				|| nonRefType->getNodeType() == core::NT_FunctionType) {
			return retIr;
		}

		// handle truncation of floating point numbers
		const core::TypePtr& subExprType = retIr->getType();
		if (subExprType->getNodeType() == core::NT_RefType) {
			// check whether it is a truncation
			if (gen.isReal(GET_REF_ELEM_TYPE(subExprType)) && gen.isSignedInt(type)) {
				const core::GenericTypePtr& intType = static_pointer_cast<const core::GenericType>(type);
				return (retIr = convFact.builder.callExpr(type, gen.getRealToInt(), nonRefExpr,
						convFact.builder.getIntTypeParamLiteral(intType->getIntTypeParameter()[0])));
			}
		}

		VLOG(2)
			<< retIr << retIr->getType();
		// LOG(DEBUG) << *subExpr << " -> " << *type;
		// Convert casts form scalars to vectors to vector init exrpessions
		return (retIr = utils::cast(retIr, type));
	}

protected:
	template<class ClangExprTy>
	ExpressionList getFunctionArguments(const core::IRBuilder& builder, ClangExprTy* callExpr,
			const core::FunctionTypePtr& funcTy) {
		ExpressionList args;
		for (size_t argId = 0, end = callExpr->getNumArgs(); argId < end; ++argId) {
			core::ExpressionPtr&& arg = Visit( callExpr->getArg(argId) );
			// core::TypePtr&& argTy = arg->getType();
			if ( argId < funcTy->getParameterTypes().size() ) {
				const core::TypePtr& funcArgTy = funcTy->getParameterTypes()[argId];
				arg = utils::cast(arg, funcArgTy);
			} else {
				arg = utils::cast(arg, builder.getNodeManager().getLangBasic().getVarList());
			}
			args.push_back( arg );
		}
		return args;
	}

public:
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	virtual core::ExpressionPtr VisitCallExpr(clang::CallExpr* callExpr) {

		START_LOG_EXPR_CONVERSION(callExpr);

		const core::IRBuilder& builder = convFact.builder;

		// return converted node
		core::ExpressionPtr irNode;
		LOG_CONVERSION(irNode);

		if (callExpr->getDirectCallee()) {

			FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee());

			core::FunctionTypePtr funcTy = core::static_pointer_cast<const core::FunctionType>(
					convFact.convertType(GET_TYPE_PTR(funcDecl)));

			// collects the type of each argument of the expression
			ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);

			assert( convFact.currTU && "Translation unit not set.");

			const TranslationUnit* oldTU = convFact.currTU;
			const FunctionDecl* definition = NULL;
			/*
			 * this will find function definitions if they are declared in  the same translation unit
			 * (also defined as static)
			 */
			if (!funcDecl->hasBody(definition)) {
				/*
				 * if the function is not defined in this translation unit, maybe it is defined in another we already
				 * loaded use the clang indexer to lookup the definition for this function declarations
				 */
				FunctionDecl* fd = funcDecl;
				const clang::idx::TranslationUnit* clangTU = convFact.getTranslationUnitForDefinition(fd);

				if (clangTU) {
					convFact.currTU = &Program::getTranslationUnit(clangTU);
				}

				if (clangTU && fd->hasBody()) {
					definition = fd;
				}
			}

			if (!definition) {
				//-----------------------------------------------------------------------------------------------------
				//     						Handle of 'special' built-in functions
				//-----------------------------------------------------------------------------------------------------
				// free(): check whether this is a call to the free() function
				if (funcDecl->getNameAsString() == "free" && callExpr->getNumArgs() == 1) {
					// in the case the free uses an input parameter
					if (args.front()->getType()->getNodeType() == core::NT_RefType) {
						return (irNode = builder.callExpr(builder.getLangBasic().getUnit(),
								builder.getLangBasic().getRefDelete(), args.front()));
					}

					// select appropriate deref operation: AnyRefDeref for void*, RefDeref for anything else
					core::ExpressionPtr arg = wrapVariable(callExpr->getArg(0));
					core::ExpressionPtr delOp =
							*arg->getType() == *builder.getLangBasic().getAnyRef() ?
									builder.getLangBasic().getAnyRefDelete() : builder.getLangBasic().getRefDelete();

					// otherwise this is not a L-Value so it needs to be wrapped into a variable
					return (irNode = builder.callExpr(builder.getLangBasic().getUnit(), delOp, arg));
				}
			}

			ExpressionList&& packedArgs = tryPack(convFact.builder, funcTy, args);

			if (!definition) {
				std::string callName = funcDecl->getNameAsString();
				// No definition has been found in any of the translation units, we mark this function as extern!
				irNode = convFact.builder.callExpr(funcTy->getReturnType(), builder.literal(callName, funcTy),
						packedArgs);

				// In the case this is a call to MPI, attach the loc annotation, handlling of those
				// statements will be then applied by mpi_sema
				if (callName.compare(0, 4, "MPI_") == 0) {
					std::pair<clang::SourceLocation, clang::SourceLocation>&& loc =
					std::make_pair(callExpr->getLocStart(), callExpr->getLocEnd());

					// add a marker node because multiple istances of the same MPI call must be distinct
					// LOG(INFO) << funcTy << std::endl;

					irNode = builder.markerExpr( core::static_pointer_cast<const core::Expression>(irNode) );

					irNode->addAnnotation( std::make_shared<annotations::c::CLocAnnotation>(
									convertClangSrcLoc(convFact.getCurrentSourceManager(), loc.first),
									convertClangSrcLoc(convFact.getCurrentSourceManager(), loc.second))
					);
				}
				convFact.currTU = oldTU;

				return irNode;
			}

			/*
			 * We find a definition, we lookup if this variable needs to access the globals, in that case the capture
			 * list needs to be initialized with the value of global variable in the current scope
			 */
			if (ctx.globalFuncMap.find(definition) != ctx.globalFuncMap.end()) {
				/*
				 * we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
				 * current context
				 */
				assert( ctx.globalVar && "No global definitions forwarded to this point");
				packedArgs.insert(packedArgs.begin(), ctx.globalVar);
			}

			/*
			 * If we are resolving the body of a recursive function we have to return the associated variable every
			 * time a function in the strongly connected graph of function calls is encountered.
			 */
			if (ctx.isResolvingRecFuncBody) {
				// check if this type has a typevar already associated, in such case return it
				ConversionContext::RecVarExprMap::const_iterator fit = ctx.recVarExprMap.find(definition);
				if (fit != ctx.recVarExprMap.end()) {
					/*
					 * we are resolving a parent recursive type, so when one of the recursive functions in the
					 * connected components are called, the introduced mu variable has to be used instead.
					 */
					convFact.currTU = oldTU;
					return (irNode = builder.callExpr(funcTy->getReturnType(),
							static_cast<core::ExpressionPtr>(fit->second), packedArgs));
				}
			}

			if (!ctx.isResolvingRecFuncBody) {

				ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(definition);

				if (fit != ctx.lambdaExprCache.end()) {

					convFact.currTU = oldTU;

					irNode = builder.callExpr(funcTy->getReturnType(), static_cast<core::ExpressionPtr>(fit->second),
							packedArgs);

					convFact.currTU = oldTU;

					return irNode;
				}
			}

			assert(definition && "No definition found for function");

			core::ExpressionPtr lambdaExpr = core::static_pointer_cast<const core::LambdaExpr>(
					convFact.convertFunctionDecl(definition));

			convFact.currTU = oldTU;

			return (irNode = builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs));

		} else if (callExpr->getCallee()) {
			core::ExpressionPtr funcPtr = convFact.tryDeref(Visit(callExpr->getCallee()));
			core::TypePtr subTy = funcPtr->getType();

			if (subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) {
				subTy = core::static_pointer_cast<const core::SingleElementType>(subTy)->getElementType();
				funcPtr = builder.callExpr(subTy, builder.getLangBasic().getArraySubscript1D(), funcPtr,
						builder.uintLit(0));
			}assert( subTy->getNodeType() == core::NT_FunctionType && "Using () operator on a non function object");

			const core::FunctionTypePtr& funcTy = core::static_pointer_cast<const core::FunctionType>(subTy);
			ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);
			return (irNode = builder.callExpr(funcPtr, args));

		} else {
			assert( false && "Call expression not referring a function");
		}
		assert(false);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PREDEFINED EXPRESSION
	//
	// [C99 6.4.2.2] - A predefined identifier such as __func__.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitPredefinedExpr(clang::PredefinedExpr* preExpr) {
		const core::lang::BasicGenerator& gen = convFact.mgr.getLangBasic();
		return convFact.builder.callExpr(gen.getGetNull(),
				convFact.builder.getTypeLiteral(convFact.convertType(GET_TYPE_PTR(preExpr))));
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						SIZEOF ALIGNOF EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//core::ExpressionPtr VisitSizeOfAlignOfExpr(clang::SizeOfAlignOfExpr* expr) {
	//START_LOG_EXPR_CONVERSION(expr);

	//core::ExpressionPtr irNode;
	//LOG_CONVERSION(irNode);

	//if ( expr->isSizeOf() ) {
	//core::TypePtr&& type = expr->isArgumentType() ?
	//convFact.convertType( expr->getArgumentType().getTypePtr() ) :
	//convFact.convertType( expr->getArgumentExpr()->getType().getTypePtr() );
	//return (irNode = getSizeOfType(convFact.getIRBuilder(), type));
	//}
	//assert(false && "SizeOfAlignOfExpr not yet supported");
	//}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						UnaryExprOrTypeTraitExpr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// UnaryExprOrTypeTraitExpr - expression with either a type or (unevaluated)
	// expression operand. Used for sizeof/alignof (C99 6.5.3.4) and vec_step
	// (OpenCL 1.1 6.11.12).
	core::ExpressionPtr VisitUnaryExprOrTypeTraitExpr(clang::UnaryExprOrTypeTraitExpr* expr) {
		START_LOG_EXPR_CONVERSION(expr);
		core::ExpressionPtr irNode;

		switch (expr->getKind()) {
		case UETT_SizeOf: {
			core::TypePtr&& type = expr->isArgumentType() ?
			convFact.convertType( expr->getArgumentType().getTypePtr() ) :
			convFact.convertType( expr->getArgumentExpr()->getType().getTypePtr() );
			return (irNode = getSizeOfType(convFact.getIRBuilder(), type));
		}
		case UETT_AlignOf:
		case UETT_VecStep:
		default:
		assert(false && "Kind of expressions not handled");
	}

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							MEMBER EXPRESSION
//
// [C99 6.5.2.3] Structure and Union Members. X->F and X.F.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr VisitMemberExpr(clang::MemberExpr* membExpr) {
	START_LOG_EXPR_CONVERSION(membExpr);

	const core::IRBuilder& builder = convFact.builder;
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	core::ExpressionPtr&& base = Visit(membExpr->getBase());

	if(membExpr->isArrow()) {
		/*
		 * we have to check whether we currently have a ref or probably an array (which is used to represent
		 * C pointers)
		 */
		//VLOG(2) << "is arrow " << base->getType();
		base = getCArrayElemRef(builder, base);
	}

	core::TypePtr structTy = base->getType();

	// Start to build the return Expression from here
	core::ExpressionPtr retIr;
	LOG_CONVERSION(retIr);

	core::ExpressionPtr op = gen.getCompositeMemberAccess();

	if (structTy->getNodeType() == core::NT_RefType) {
		// skip over reference wrapper
		structTy = core::analysis::getReferencedType( structTy );
		op = gen.getCompositeRefElem();
	}

	// There are 2 basic cases which need to be handled: Struct/Unions and Recursive Types
	assert((structTy->getNodeType() == core::NT_StructType || structTy->getNodeType() == core::NT_UnionType ||
					structTy->getNodeType() == core::NT_RecType) &&
			"Using a member access operation on a non struct/union type"
	);

	// if the inner type is a RecType then we need to unroll it to get the contained composite type
	if ( structTy->getNodeType() == core::NT_RecType ) {
		structTy = core::static_pointer_cast<const core::RecType>(structTy)->unroll(convFact.mgr);
	}
	assert(structTy && "Struct Type not being initialized");

	//identifier of the member
	core::StringValuePtr ident;
	core::NamedCompositeTypePtr compType = core::static_pointer_cast<const core::NamedCompositeType>(structTy);

	if (!membExpr->getMemberDecl()->getIdentifier()) {

		FieldDecl* field = dyn_cast<FieldDecl>(membExpr->getMemberDecl());
		assert(field && field->isAnonymousStructOrUnion());

		// Union may have anonymous member which have been tagged with a '__m' name by the type
		// convert
		ident = builder.stringValue("__m"+insieme::utils::numeric_cast<std::string>(field->getFieldIndex()));
	} else {
		ident = builder.stringValue(membExpr->getMemberDecl()->getName().data());
	}

	assert(ident);

	const core::TypePtr& memberTy =
		core::static_pointer_cast<const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);

	core::TypePtr resType = memberTy;

	//base class as member in derived class
	assert(resType);
	if (base->getType()->getNodeType() == core::NT_RefType) {
		resType = builder.refType(resType);
	}

	retIr = builder.callExpr(resType, op, base, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));

	END_LOG_EXPR_CONVERSION(retIr);
	VLOG(2) << "End of expression MemberExpr\n";
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							BINARY OPERATOR
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr VisitBinaryOperator(clang::BinaryOperator* binOp) {
	START_LOG_EXPR_CONVERSION(binOp);
	const core::IRBuilder& builder = convFact.builder;
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	core::ExpressionPtr retIr;
	LOG_CONVERSION(retIr);

	core::ExpressionPtr&& rhs = Visit(binOp->getRHS());
	core::ExpressionPtr&& lhs = Visit(binOp->getLHS());

	// handle of volatile variables
	if (binOp->getOpcode() != BO_Assign && core::analysis::isVolatileType(lhs->getType()) ) {
		lhs = builder.callExpr( builder.getLangBasic().getVolatileRead(), lhs);
	}
	if ( core::analysis::isVolatileType(rhs->getType()) ) {
		rhs = builder.callExpr( builder.getLangBasic().getVolatileRead(), rhs);
	}

	/*
	 * if the binary operator is a comma separated expression, we convert it into a function call which returns the
	 * value of the last expression
	 */
	if ( binOp->getOpcode() == BO_Comma ) {

		core::CompoundStmtPtr&& body = builder.compoundStmt(toVector<core::StatementPtr>(lhs,
						(gen.isUnit(rhs->getType()) ? static_cast<core::StatementPtr>(rhs) : builder.returnStmt(rhs)) )
		);
		return (retIr = builder.createCallExprFromBody(body, rhs->getType()));
	}

	// the type of this expression is the type of the LHS expression
	core::TypePtr exprTy = lhs->getType()->getNodeType() == core::NT_RefType ?
	GET_REF_ELEM_TYPE(lhs->getType()) : lhs->getType();

	// get basic element type
	core::ExpressionPtr&& subExprLHS = convFact.tryDeref(lhs);

	/*
	 * we take care of compound operators first, we rewrite the RHS expression in a normal form, i.e.:
	 * 		->		a op= b  ---->  a = a op b
	 */
	clang::BinaryOperatorKind baseOp;
	core::lang::BasicGenerator::Operator op;
	bool isCompound = true;

	switch ( binOp->getOpcode() ) {
		// a *= b
		case BO_MulAssign: op = core::lang::BasicGenerator::Mul; baseOp = BO_Mul; break;
		// a /= b
		case BO_DivAssign: op = core::lang::BasicGenerator::Div; baseOp = BO_Div; break;
		// a %= b
		case BO_RemAssign: op = core::lang::BasicGenerator::Mod; baseOp = BO_Rem; break;
		// a += b
		case BO_AddAssign: op = core::lang::BasicGenerator::Add; baseOp = BO_Add; break;
		// a -= b
		case BO_SubAssign: op = core::lang::BasicGenerator::Sub; baseOp = BO_Sub; break;
		// a <<= b
		case BO_ShlAssign: op = core::lang::BasicGenerator::LShift; baseOp = BO_Shl; break;
		// a >>= b
		case BO_ShrAssign: op = core::lang::BasicGenerator::RShift; baseOp = BO_Shr; break;
		// a &= b
		case BO_AndAssign: op = core::lang::BasicGenerator::And; baseOp = BO_And; break;
		// a |= b
		case BO_OrAssign: op = core::lang::BasicGenerator::Or; baseOp = BO_Or; break;
		// a ^= b
		case BO_XorAssign: op = core::lang::BasicGenerator::Xor; baseOp = BO_Xor; break;
		default:
		isCompound = false;
	}

	if ( isCompound ) {
		// we check if the RHS is a ref, in that case we use the deref operator
		rhs = convFact.tryDeref(rhs);
		core::ExpressionPtr&& opFunc = gen.getOperator(exprTy, op);
		rhs = builder.callExpr(exprTy, opFunc, subExprLHS, rhs);
	}

	bool isAssignment = false;
	bool isLogical = false;

	baseOp = binOp->getOpcode();

	core::ExpressionPtr opFunc;
	switch ( binOp->getOpcode() ) {
		case BO_PtrMemD:
		case BO_PtrMemI:
		assert(false && "Operator not yet supported!");

		// a * b
		case BO_Mul: op = core::lang::BasicGenerator::Mul; break;
		// a / b
		case BO_Div: op = core::lang::BasicGenerator::Div; break;
		// a % b
		case BO_Rem: op = core::lang::BasicGenerator::Mod; break;
		// a + b
		case BO_Add: op = core::lang::BasicGenerator::Add; break;
		// a - b
		case BO_Sub: op = core::lang::BasicGenerator::Sub; break;
		// a << b
		case BO_Shl: op = core::lang::BasicGenerator::LShift; break;
		// a >> b
		case BO_Shr: op = core::lang::BasicGenerator::RShift; break;
		// a & b
		case BO_And: op = core::lang::BasicGenerator::And; break;
		// a ^ b
		case BO_Xor: op = core::lang::BasicGenerator::Xor; break;
		// a | b
		case BO_Or: op = core::lang::BasicGenerator::Or; break;

		// Logic operators

		// a && b
		case BO_LAnd: op = core::lang::BasicGenerator::LAnd; isLogical=true; break;
		// a || b
		case BO_LOr: op = core::lang::BasicGenerator::LOr; isLogical=true; break;
		// a < b
		case BO_LT: op = core::lang::BasicGenerator::Lt; isLogical=true; break;
		// a > b
		case BO_GT: op = core::lang::BasicGenerator::Gt; isLogical=true; break;
		// a <= b
		case BO_LE: op = core::lang::BasicGenerator::Le; isLogical=true; break;
		// a >= b
		case BO_GE: op = core::lang::BasicGenerator::Ge; isLogical=true; break;
		// a == b
		case BO_EQ: op = core::lang::BasicGenerator::Eq; isLogical=true; break;
		// a != b
		case BO_NE: op = core::lang::BasicGenerator::Ne; isLogical=true; break;

		case BO_MulAssign: case BO_DivAssign: case BO_RemAssign: case BO_AddAssign: case BO_SubAssign:
		case BO_ShlAssign: case BO_ShrAssign: case BO_AndAssign: case BO_XorAssign: case BO_OrAssign:
		case BO_Assign:
		{
			baseOp = BO_Assign;
			/*
			 * poor C codes assign value to function parameters, this is not allowed here as input parameters are of
			 * non REF type. What we need to do is introduce a declaration for these variables and use the created
			 * variable on the stack instead of the input parameters
			 */
			lhs = wrapVariable(binOp->getLHS());

			// make sure the lhs is a L-Value
			lhs = asLValue(lhs);

			// This is an assignment, we have to make sure the LHS operation is of type ref<a'>
			assert( lhs->getType()->getNodeType() == core::NT_RefType && "LHS operand must be of type ref<'a>.");

			rhs = utils::cast(rhs, GET_REF_ELEM_TYPE(lhs->getType()));
			isAssignment = true;
			opFunc = gen.getRefAssign();
			exprTy = gen.getUnit();
			break;
		}
		default:
		assert(false && "Operator not supported");
	}

	// Operators && and || introduce short circuit operations, this has to be directly supported in the IR.
	if ( baseOp == BO_LAnd || baseOp == BO_LOr ) {
		lhs = utils::cast(lhs, gen.getBool());
		rhs = utils::cast(rhs, gen.getBool());
		// lazy evaluation of RHS
		exprTy = gen.getBool();
		rhs = builder.createCallExprFromBody(builder.returnStmt(rhs), gen.getBool(), true);
	}

	VLOG(2) << "LHS( " << *lhs << "[" << *lhs->getType() << "]) " << opFunc <<
	" RHS(" << *rhs << "[" << *rhs->getType() << "])";

	if( !isAssignment ) {

		core::TypePtr&& lhsTy = lhs->getType();
		core::TypePtr&& rhsTy = rhs->getType();
		VLOG(2) << "LHS( " << *lhs << "[" << *lhs->getType() << "]) " << opFunc <<
		" RHS(" << *rhs << "[" << *rhs->getType() << "])";

		if(binOp->getLHS()->getType().getUnqualifiedType()->isExtVectorType() ||
				binOp->getRHS()->getType().getUnqualifiedType()->isExtVectorType()) { // handling for ocl-vector operations
			// check if lhs is not an ocl-vector, in this case create a vector form the scalar
			if(binOp->getLHS()->getStmtClass() == Stmt::ImplicitCastExprClass) { // the rhs is a scalar, implicitly casted to a vector
				// lhs is a scalar
				lhs = scalarToVector(lhs, rhsTy, builder, convFact);
			} else
			lhs = convFact.tryDeref(lhs); // lhs is an ocl-vector

			if(binOp->getRHS()->getStmtClass() == Stmt::ImplicitCastExprClass ) { // the rhs is a scalar, implicitly casted to a vector
				// rhs is a scalar
				rhs = scalarToVector(rhs, lhsTy, builder, convFact);
			} else
			rhs = convFact.tryDeref(rhs); // rhs is an ocl-vector

			// generate a ocl_vector - scalar operation
			opFunc = gen.getOperator(exprTy, op);

			// TODO to be tested
			if (const core::FunctionTypePtr funTy = core::dynamic_pointer_cast<const core::FunctionType>(opFunc->getType()))
				if(funTy->getReturnType() == funTy->getParameterTypeList().at(0)) { // check if we can use the type of the first argument as retun type
					return (retIr = builder.callExpr(lhs->getType(), opFunc, lhs, utils::cast(rhs, lhs->getType())));
				} else { // let deduce it otherwise
					return (retIr = builder.callExpr(opFunc, lhs, utils::cast(rhs, lhs->getType())));
				}
			else {
				assert(false && "old stuff needed, tell Klaus");
				return (retIr = builder.callExpr(lhsTy, opFunc, lhs, rhs));
			}

		}
		if ( lhsTy->getNodeType() != core::NT_RefType && rhsTy->getNodeType() != core::NT_RefType) {
			// ----------------------------- Hack begin --------------------------------
			// TODO: this is a quick solution => maybe clang allows you to determine the actual type
			// => otherwise the sub-type checks within the core may be used
			//
			// Bug fixed by this:
			//		when multiplying an integer with a double, the double is casted to an integer and the
			//		results is an integer.
			//

			// check whether result type needs to be adjusted
			if (*lhsTy != *rhsTy) {
				// if second argument is a real
				if (!gen.isReal(lhsTy) && gen.isReal(rhsTy)) {
					exprTy = rhsTy;
				}
			}
			// ----------------------------- Hack end --------------------------------

			// all binary operators have the same input and result types
			lhs = utils::cast(lhs, exprTy);
			rhs = utils::cast(rhs, exprTy);

			// Handle pointers arithmetic
			VLOG(2) << "Lookup for operation: " << op << ", for type: " << *exprTy;
			opFunc = gen.getOperator(exprTy, op);

		}
		else if (lhsTy->getNodeType() == core::NT_RefType && rhsTy->getNodeType() != core::NT_RefType) {
			// LHS must be a ref<array<'a>>
			const core::TypePtr& subRefTy = GET_REF_ELEM_TYPE(lhsTy);

			if(subRefTy->getNodeType() == core::NT_VectorType)
			lhs = builder.callExpr(gen.getRefVectorToRefArray(), lhs);

			// Capture pointer arithmetics
			// 	Base op must be either a + or a -
			assert( (baseOp == BO_Add || baseOp == BO_Sub) &&
					"Operators allowed in pointer arithmetic are + and - only");

			assert(GET_REF_ELEM_TYPE(lhs->getType())->getNodeType() == core::NT_ArrayType &&
					"LHS operator must be of type ref<array<'a,#l>>");

			// check whether the RHS is of integer type
			// assert( gen.isUnsignedInt(rhsTy) && "Array displacement is of non type uint");
			return (retIr = builder.callExpr(gen.getArrayView(), lhs, rhs));

		} else {
			assert(lhsTy->getNodeType() == core::NT_RefType
					&& rhsTy->getNodeType() == core::NT_RefType && "Comparing pointers");
			retIr = builder.callExpr( gen.getBool(), gen.getPtrEq(), lhs, rhs );
			if ( baseOp == BO_NE ) {
				// comparing two refs
				retIr = builder.callExpr( gen.getBool(), gen.getBoolLNot(), retIr );
			}
			return retIr;
		}

		if ( DeclRefExpr* declRefExpr = utils::skipSugar<DeclRefExpr>(binOp->getLHS()) ) {
			if ( isa<ArrayType>(declRefExpr->getDecl()->getType().getTypePtr()) )
			assert(false && "Pointer arithmetic not yet supported");
		}

		if(isLogical) {exprTy = gen.getBool();}

	} else {
		// check if there is a kernelFile annotation
		ocl::attatchOclAnnotation(rhs, binOp, convFact);
	}
	assert(opFunc);

	/*if ( !isAssignment ) {*/
	//// We know thie operator now we have to make sure that all the arguments are of the correct type
	//core::FunctionTypePtr funcTy = core::static_pointer_cast<const core::FunctionType>( opFunc->getType() );
	//assert(funcTy->getParameterTypes().size() == 2);
	//core::TypePtr lhsFuncTy = funcTy->getParameterTypes()[0];
	//if (!gen.isUIntGen(lhsFuncTy) && !gen.isRealGen(lhsFuncTy) && !gen.isIntGen(lhsFuncTy)) {
	//lhs = cast(lhsFuncTy, lhs);
	//}
	//core::TypePtr rhsFuncTy = funcTy->getParameterTypes()[1];
	//if (!gen.isUIntGen(rhsFuncTy) && !gen.isRealGen(rhsFuncTy) && !gen.isIntGen(rhsFuncTy)) {
	//rhs = cast(rhsFuncTy, rhs);
	//}
	//if (*lhsFuncTy == *rhsFuncType && *lhs->getType() != *rhs->getType()) {
	//// we still need to adjust
	//}
	/*}*/
	// add source code annotation to the rhs if present
	VLOG(2) << "LHS( " << *lhs << "[" << *lhs->getType() << "]) " << opFunc <<
	" RHS(" << *rhs << "[" << *rhs->getType() << "])";

	return (retIr = convFact.builder.callExpr( exprTy, opFunc, lhs, rhs ));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							UNARY OPERATOR
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr VisitUnaryOperator(clang::UnaryOperator *unOp) {
	START_LOG_EXPR_CONVERSION(unOp);
	const core::IRBuilder& builder = convFact.builder;
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	core::ExpressionPtr retIr;
	LOG_CONVERSION(retIr);

	core::ExpressionPtr&& subExpr = Visit(unOp->getSubExpr());

	// build lambda expression for post/pre increment/decrement unary operators
	auto encloseIncrementOperator =
	[ this, &builder, &gen ]
	(core::ExpressionPtr subExpr, core::lang::BasicGenerator::Operator op) -> core::ExpressionPtr {
		if (subExpr->getNodeType() == core::NT_Variable && subExpr->getType()->getNodeType() != core::NT_RefType) {
			// It can happen we are incrementing a variable which is coming from an input
			// argument of a function
			core::VariablePtr var = core::static_pointer_cast<const core::Variable>(subExpr);
			assert(var->getType()->getNodeType() != core::NT_RefType);

			auto&& fit = convFact.ctx.wrapRefMap.find(var);
			if ( fit == convFact.ctx.wrapRefMap.end() ) {
				fit = convFact.ctx.wrapRefMap.insert(
						std::make_pair( var, builder.variable( builder.refType(var->getType()) ) )
				).first;
			}
			subExpr = fit->second;
		}
		core::TypePtr type = subExpr->getType();
		assert( type->getNodeType() == core::NT_RefType &&
				"Illegal increment/decrement operand - not a ref type" );
		core::TypePtr elementType = GET_REF_ELEM_TYPE(type);

		core::TypePtr genType;
		if ( gen.isSignedInt(elementType) ) {
			genType = gen.getIntGen();
		} else if ( gen.isUnsignedInt(elementType) ) {
			genType = gen.getUIntGen();
		} else {
			assert(false && "Illegal operand type for increment/decrement operator.");
		}
		return convFact.builder.callExpr(elementType, convFact.mgr.getLangBasic().getOperator(genType, op), subExpr);
	};

	switch ( unOp->getOpcode() ) {
		// conversion of post increment/decrement operation is done by creating a tuple expression i.e.:
		// a++ ==> (__tmp = a, a=a+1, __tmp)
		// ++a ==> ( a=a+1, a)
		// --a
		case UO_PreDec:
		return retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreDec);
		// a--
		case UO_PostDec:
		return (retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostDec));
		// a++
		case UO_PreInc:
		return (retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreInc));
		// ++a
		case UO_PostInc:
		return (retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostInc));
		// &a
		case UO_AddrOf:
		{
			/*
			 * We need to be careful paramvars are not dereferenced and the address passed around. If this happens
			 * we have to declare a variable holding the memory location for that value and replace every use of
			 * the paramvar with the newly generated variable: the structure needRef in the ctx is used for this
			 */
			retIr = wrapVariable(unOp->getSubExpr());

			// in the case we are getting the address of a function the & operator
			// has no effects, therefore we return
			if (retIr->getType()->getNodeType() == core::NT_FunctionType) {
				return retIr;
			}

			// make sure it is a L-Value
			retIr = asLValue(retIr);

			assert(retIr->getType()->getNodeType() == core::NT_RefType);
			return (retIr = utils::refScalarToRefArray(retIr));
		}
		// *a
		case UO_Deref: {
			// make sure it is a L-Value
			retIr = asLValue(subExpr);

			assert(retIr->getType()->getNodeType() == core::NT_RefType &&
					"Impossible to apply * operator to an R-Value");

			const core::TypePtr& subTy = GET_REF_ELEM_TYPE(retIr->getType());

			return (retIr =
					(subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) ?
					getCArrayElemRef(builder, retIr) : convFact.tryDeref(retIr)
			);
		}
		// +a
		case UO_Plus:
		return retIr = subExpr;
		// -a
		case UO_Minus:
		return (retIr = builder.invertSign( convFact.tryDeref(subExpr) ));
		// ~a
		case UO_Not:
		retIr = convFact.tryDeref(subExpr);
		return (retIr = builder.callExpr(
						retIr->getType(),
						gen.getOperator(retIr->getType(), core::lang::BasicGenerator::Not),
						retIr)
		);
		// !a
		case UO_LNot:
		if( !gen.isBool(subExpr->getType()) ) {
			subExpr = utils::cast(subExpr, gen.getBool());
		}
		assert( gen.isBool(subExpr->getType()) );

		return (retIr = builder.callExpr( subExpr->getType(), gen.getBoolLNot(), subExpr ) );
		case UO_Real:
		case UO_Imag:
		case UO_Extension: //TODO:
		default:
		assert(false && "Unary operator not supported");
	}
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							CONDITIONAL OPERATOR
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr VisitConditionalOperator(clang::ConditionalOperator* condOp) {
	START_LOG_EXPR_CONVERSION(condOp);
	const core::IRBuilder& builder = convFact.builder;
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	core::ExpressionPtr retIr;
	LOG_CONVERSION(retIr);

	core::TypePtr retTy = convFact.convertType( GET_TYPE_PTR(condOp) );
	core::ExpressionPtr&& trueExpr = Visit(condOp->getTrueExpr());
	core::ExpressionPtr&& falseExpr = Visit(condOp->getFalseExpr());
	core::ExpressionPtr&& condExpr = Visit( condOp->getCond() );

	condExpr = utils::cast(condExpr, gen.getBool());

	// Dereference eventual references
	if ( retTy->getNodeType() == core::NT_RefType ) {
		retTy = GET_REF_ELEM_TYPE(retTy);
	}

	return (retIr =
			builder.callExpr(retTy, gen.getIfThenElse(),
					condExpr, // Condition
					builder.createCallExprFromBody(
							builder.returnStmt(utils::cast(trueExpr, retTy)), retTy, true
					),// True
					builder.createCallExprFromBody(
							builder.returnStmt(utils::cast(falseExpr, retTy)), retTy, true
					)// False
			)
	);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						ARRAY SUBSCRIPT EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) {
	START_LOG_EXPR_CONVERSION(arraySubExpr);

	core::ExpressionPtr retIr;
	LOG_CONVERSION(retIr);

	const core::lang::BasicGenerator& gen = convFact.builder.getLangBasic();
	/*
	 * CLANG introduces implicit cast for the base expression of array subscripts which cast the array type into a
	 * simple pointer. As insieme supports subscripts only for array or vector types, we skip eventual implicit
	 * cast operations.
	 */
	Expr* baseExpr = arraySubExpr->getBase();

	// IDX
	core::ExpressionPtr idx = convFact.tryDeref( Visit( arraySubExpr->getIdx() ) );
	if (!gen.isUInt4(idx->getType())) {
		idx = convFact.builder.castExpr(gen.getUInt4(), idx);
	}

	// BASE
	core::ExpressionPtr base = Visit( baseExpr );

	core::TypePtr opType;
	core::LiteralPtr op;

	if ( base->getType()->getNodeType() == core::NT_RefType ) {
		// The vector/array is an L-Value so we use the array.ref.elem
		// operator to return a reference to the addressed memory location
		core::TypePtr refSubTy = GET_REF_ELEM_TYPE(base->getType());

		// TODO: we need better checking for vector type
		assert( (refSubTy->getNodeType() == core::NT_VectorType ||
						refSubTy->getNodeType() == core::NT_ArrayType) &&
				"Base expression of array subscript is not a vector/array type.");

		op = refSubTy->getNodeType() == core::NT_ArrayType ? gen.getArrayRefElem1D() : gen.getVectorRefElem();

		opType = convFact.builder.refType(
				core::static_pointer_cast<const core::SingleElementType>(refSubTy)->getElementType()
		);

	} else {

		/*
		 * The vector/array is an R-value (e.g. (int[2]){0,1}[1] ) in this case the subscript returns an R-value so
		 * the array.subscript operator must be used
		 */
		// TODO: we need better checking for vector type
		assert( (base->getType()->getNodeType() == core::NT_VectorType ||
						base->getType()->getNodeType() == core::NT_ArrayType) &&
				"Base expression of array subscript is not a vector/array type.");

		op = base->getType()->getNodeType() == core::NT_ArrayType ? gen.getArraySubscript1D() : gen.getVectorSubscript();

		opType = core::static_pointer_cast<const core::SingleElementType>(base->getType())->getElementType();
	}

	return (retIr = convFact.builder.callExpr( opType, op, base, idx) );
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						EXT VECTOR ELEMENT EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr VisitExtVectorElementExpr(ExtVectorElementExpr* vecElemExpr) {
	START_LOG_EXPR_CONVERSION(vecElemExpr);
	core::ExpressionPtr&& base = Visit( vecElemExpr->getBase() );
	const core::lang::BasicGenerator& gen = convFact.builder.getLangBasic();

	core::ExpressionPtr retIr;
	LOG_CONVERSION(retIr);

	llvm::StringRef&& accessor = vecElemExpr->getAccessor().getName();

	core::TypePtr&& exprTy = convFact.convertType( GET_TYPE_PTR(vecElemExpr) );
	unsigned int pos;

	//translate OpenCL accessor string to index
	if ( accessor == "x" ) pos = 0u;
	else if ( accessor == "y" ) pos = 1u;
	else if ( accessor == "z" ) pos = 2u;
	else if ( accessor == "w" ) pos = 3u;
	else if ( (accessor.front() == 's' || accessor.front() == 'S') && accessor.size() == 2) {
		// the input string is in a form sXXX
		// we skip the s and return the value to get the number
		llvm::StringRef numStr = accessor.substr(1,accessor.size()-1);
		std::string posStr = numStr;

		if(posStr.at(0) <= '9')
		pos = posStr.at(0) - '0';
		else if(posStr.at(0) <= 'E')
		pos = (10 + posStr.at(0) - 'A');//convert A .. E to 10 .. 15
		else if(posStr.at(0) <= 'e')
		pos = (10 + posStr.at(0) - 'a');//convert a .. e to 10 .. 15
		else
		assert(posStr.at(0) <= 'e' && "Invalid vector accessing string");
	} else if ( accessor.size() <= 16 ) { // opencl vector permutation
		vector<core::ExpressionPtr> args;

		// expression using x, y, z and w
		auto acc = accessor.begin();
		if(*acc == 'S' || *acc == 's') { // expression using s0 .. sE
			++acc;// skip the s
			for ( auto I = acc, E = accessor.end(); I != E; ++I ) {
				if(*I <= '9')
				pos = *I - '0';
				else if(*I <= 'E')
				pos = (10 + (*I)-'A'); //convert A .. E to 10 .. 15
				else if(*I <= 'e')
				pos = (10 + (*I)-'a');//convert a .. e to 10 .. 15
				else
				assert(*I <= 'e' && "Unexpected accessor in ExtVectorElementExpr");

				args.push_back(convFact.builder.uintLit(pos));
			}
			return (retIr = convFact.builder.vectorPermute(convFact.tryDeref(base), convFact.builder.vectorExpr(args)) );
		} else {
			for ( auto I = acc, E = accessor.end(); I != E; ++I ) {
				args.push_back(convFact.builder.uintLit(*I == 'w' ? 3 : (*I)-'x')); //convert x, y, z, w to 0, 1, 2, 3
			}
			return (retIr = convFact.builder.vectorPermute(convFact.tryDeref(base), convFact.builder.vectorExpr(args)) );
		}

	} else {
		assert(accessor.size() <= 16 && "ExtVectorElementExpr has unknown format");
	}

	// The type of the index is always uint<4>
	core::ExpressionPtr&& idx = convFact.builder.uintLit(pos);
	// if the type of the vector is a refType, we deref it
	base = convFact.tryDeref(base);

	return (retIr = convFact.builder.callExpr(exprTy, gen.getVectorSubscript(), base, idx));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							VAR DECLARATION REFERENCE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
virtual core::ExpressionPtr VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
	START_LOG_EXPR_CONVERSION(declRef);

	core::ExpressionPtr retIr;
	LOG_CONVERSION(retIr);

	// check whether this is a reference to a variable
	core::ExpressionPtr retExpr;
	if (ParmVarDecl* parmDecl = dyn_cast<ParmVarDecl>(declRef->getDecl())) {
		VLOG(2) << "Parameter type: " << convFact.convertType(parmDecl->getOriginalType().getTypePtr() );
		return ( retIr = convFact.lookUpVariable( parmDecl ) );
	}
	if ( VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl()) ) {

		retIr = convFact.lookUpVariable( varDecl );

		if(GET_TYPE_PTR(varDecl)->isReferenceType()) {
			retIr = convFact.tryDeref(retIr);
		}

		return retIr;
	}
	if( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(declRef->getDecl()) ) {
		return (retIr =
				core::static_pointer_cast<const core::Expression>(
						convFact.convertFunctionDecl(funcDecl)
				)
		);
	}
	if (EnumConstantDecl* enumDecl = dyn_cast<EnumConstantDecl>(declRef->getDecl() ) ) {
		return (retIr =
				convFact.builder.literal(
						enumDecl->getInitVal().toString(10),
						convFact.builder.getLangBasic().getInt4()
				)
		);
	}
	// todo: C++ check whether this is a reference to a class field, or method (function).
	assert(false && "DeclRefExpr not supported!");
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                  VECTOR/STRUCT INITALIZATION EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr VisitInitListExpr(clang::InitListExpr* initList) {
	assert(false && "Visiting of initializer list is not allowed!");
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                  	COMPOUND LITERAL EXPRESSION
// Introduced in C99 6.5.2.5, used to initialize structures or arrays with
// the { } expression, example:
// 		strcut A a;
// 		a = (struct A) { 10, 20, 30 };
//
//	or:
//		((int [3]){1,2,3})[2]  -> 2
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr VisitCompoundLiteralExpr(clang::CompoundLiteralExpr* compLitExpr) {
	START_LOG_EXPR_CONVERSION(compLitExpr);

	core::ExpressionPtr retIr;
	LOG_CONVERSION(retIr);

	if ( clang::InitListExpr* initList =
			dyn_cast<clang::InitListExpr>(compLitExpr->getInitializer())
	) {
		return (retIr =
				convFact.convertInitExpr(
						initList,
						convFact.convertType(compLitExpr->getType().getTypePtr()),
						false
				)
		);
	}
	return (retIr = Visit(compLitExpr->getInitializer()));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Visit(clang::Expr* expr) {
	core::ExpressionPtr&& retIr = StmtVisitor<ClangExprConverter, core::ExpressionPtr>::Visit(expr);

	// check for OpenMP annotations
	return omp::attachOmpAnnotation(retIr, expr, convFact);
}
};

ConversionFactory::ClangExprConverter*
ConversionFactory::makeExprConvert(ConversionFactory& fact, Program& program) {
	return new ClangExprConverter(fact, program);
}

void ConversionFactory::cleanExprConvert(ConversionFactory::ClangExprConverter* exprConv) {
	delete exprConv;
}

core::ExpressionPtr ConversionFactory::convertExpr(const clang::Expr* expr) const {
	assert(expr && "Calling convertExpr with a NULL pointer");
	return exprConv->Visit(const_cast<Expr*>(expr));
}

/**************************************************************************************************
 * InitListExpr describes an initializer list, which can be used to initialize objects of different
 * types, InitListExpr including struct/class/union types, arrays, and vectors. For example:
 *
 * struct foo x = { 1, { 2, 3 } };
 *
 * In insieme this statement has to tranformed into a StructExpr, or VectorExpr depending on the
 * type of the LHS expression.
 **************************************************************************************************/
core::ExpressionPtr 
ConversionFactory::convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type) const {
	const ConversionFactory& convFact = *this;
	START_LOG_EXPR_CONVERSION(initList);

	core::ExpressionPtr retIr;
//	ATTACH_OMP_ANNOTATIONS(retIr, initList);
	LOG_CONVERSION(retIr);

	bool isRef = false;
	core::TypePtr currType = type;
	if ( core::RefTypePtr&& refType = core::dynamic_pointer_cast<const core::RefType>(type)) {
		isRef = true;
		currType = refType->getElementType();
	}

	if (currType->getNodeType() == core::NT_VectorType || currType->getNodeType() == core::NT_ArrayType) {

		const core::TypePtr& elemTy =
				core::static_pointer_cast<const core::SingleElementType>(currType)->getElementType();

		ExpressionList elements;
		// get all values of the init expression
		for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
			const clang::Expr* subExpr = initList->getInit(i);
			core::ExpressionPtr&& convExpr = convertInitExpr(subExpr, elemTy, false);

			assert(convExpr && "convExpr is empty");
			elements.push_back(utils::cast(convExpr, elemTy));
		}

		if (elements.size() == 1 && currType->getNodeType() == core::NT_VectorType) {
			const core::VectorTypePtr& vecTy = core::static_pointer_cast<const core::VectorType>(currType);
			// In C when the initializer list contains 1 elements then all the elements of the
			// vector (or array) must be initialized with the same value
			const core::ConcreteIntTypeParamPtr& vecArgSize =
					core::static_pointer_cast<const core::ConcreteIntTypeParam>(vecTy->getSize());

			retIr = builder.callExpr(vecTy, builder.getLangBasic().getVectorInitUniform(), elements.front(),
					builder.getIntTypeParamLiteral(vecArgSize));

		} else
			retIr = builder.vectorExpr(elements);
	}

	/*
	 * in the case the initexpr is used to initialize a struct/class we need to create a structExpr to initialize the
	 * structure
	 */
	if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(currType)) {
		core::StructExpr::Members members;
		for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
			const core::NamedTypePtr& curr = structTy->getEntries()[i];
			members.push_back(
					builder.namedValue(curr->getName(), convertInitExpr(initList->getInit(i), curr->getType(), false)));
		}
		retIr = builder.structExpr(members);
	}

	/*
	 * in the case the initexpr is used to initialize a union
	 */
	if ( core::UnionTypePtr&& unionTy = core::dynamic_pointer_cast<const core::UnionType>(currType)) {
		core::ExpressionPtr ie = convertInitExpr(initList->getInit(0), unionTy->getEntries()[0]->getType(), false);
		retIr = builder.unionExpr(unionTy, unionTy->getEntries()[0]->getName(), ie);
		LOG(DEBUG) << *retIr;

	//	core::StructExpr::Members members;
	//	for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
	//		const core::NamedTypePtr& curr = structTy->getEntries()[i];
	//		members.push_back(
	//				builder.namedValue(curr->getName(), convertInitExpr(initList->getInit(i), curr->getType(), false)));
	//	}
	//	retIr = builder.structExpr(members);
	}

	assert(retIr && "Couldn't convert initialization expression");

	if (isRef) {
		retIr = builder.refVar(retIr);
	}
	// create vector initializator
	return retIr;
}

core::ExpressionPtr ConversionFactory::convertInitExpr(const clang::Expr* expr, const core::TypePtr& type,
		const bool zeroInit) const {
	core::ExpressionPtr retIr;
	// ATTACH_OMP_ANNOTATIONS(retIr, initList);
	LOG_CONVERSION(retIr);

	// get kind of initialized value
	core::NodeType&& kind =
	(type->getNodeType() != core::NT_RefType ? type->getNodeType() : GET_REF_ELEM_TYPE(type)->getNodeType() );

	if (!expr) {
		// if no init expression is provided => use undefined for given set of types
		if (kind == core::NT_StructType || kind == core::NT_UnionType || kind == core::NT_ArrayType
				|| kind == core::NT_VectorType) {
			if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
				const core::TypePtr& res = refTy->getElementType();
				return (retIr = builder.refVar(
						builder.callExpr(res,
								(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
								builder.getTypeLiteral(res))));
			}
			return (retIr = builder.callExpr(type,
					(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
					builder.getTypeLiteral(type)));
		} else {
			return (retIr = defaultInitVal(type));
		}
	}

	/*
	 * if an expression is provided as initializer first check if this is an initializer list which is used for arrays,
	 * structs and unions
	 */
	if ( const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( expr )) {
		return (retIr = convertInitializerList(listExpr, type));
	}

	// init the cpp class / struct - check here for enabled cpp in compiler lang options
	if (kind == core::NT_StructType && currTU->getCompiler().getPreprocessor().getLangOptions().CPlusPlus == 1) {

		if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
			const core::TypePtr& res = refTy->getElementType();
			retIr = builder.refVar(
					builder.callExpr(res,
							(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
							builder.getTypeLiteral(res)));
		}assert(retIr && "call expression is empty");
		return retIr;
	}

	// Convert the expression like any other expression
	retIr = convertExpr(expr);

	if (core::analysis::isCallOf(retIr, mgr.getLangBasic().getArrayCreate1D())) {
		retIr = builder.callExpr(builder.refType(retIr->getType()), mgr.getLangBasic().getRefNew(), retIr);
	}

	// fix type if necessary (also converts "Hello" into ['H','e',...])
	core::TypePtr valueType = type;
	if (type->getNodeType() == core::NT_RefType) {
		valueType = core::analysis::getReferencedType(valueType);
	}

	retIr = utils::cast(retIr, valueType);

	// if result is a reference type => create new local variable
	if (type->getNodeType() == core::NT_RefType) {
		retIr = builder.callExpr(type, mgr.getLangBasic().getRefVar(), retIr);
	}

	return retIr;
}

// the globalVar parameter is added at the FIRST position of the function parameters
core::FunctionTypePtr ConversionFactory::addGlobalsToFunctionType(const core::IRBuilder& builder,
		const core::TypePtr& globals, const core::FunctionTypePtr& funcType) {

	const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();

	std::vector<core::TypePtr> argTypes(oldArgs.size() + 1);

	std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin() + 1);
	// function is receiving a reference to the global struct as the first argument
	argTypes[0] = builder.refType(globals);
	return builder.functionType(argTypes, funcType->getReturnType());

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERT FUNCTION DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::NodePtr ConversionFactory::convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool isEntryPoint) {

	assert(currTU && funcDecl->hasBody() && "Function has no body!");

	VLOG(1) << "~ Converting function: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;

	VLOG(1) << "#----------------------------------------------------------------------------------#";
	VLOG(1)
		<< "\nVisiting Function Declaration for: " << funcDecl->getNameAsString() << std::endl << "-> at location: ("
				<< utils::location(funcDecl->getSourceRange().getBegin(), currTU->getCompiler().getSourceManager())
				<< "): " << std::endl << "\tIsRecSubType: " << ctx.isRecSubFunc << std::endl
				<< "\tisResolvingRecFuncBody: " << ctx.isResolvingRecFuncBody << std::endl << "\tEmpty map: "
				<< ctx.recVarExprMap.size();

	if (!ctx.isRecSubFunc) {
		// add this type to the type graph (if not present)
		exprConv->funcDepGraph.addNode(funcDecl);
		if (VLOG_IS_ON(2)) {
			exprConv->funcDepGraph.print(std::cout);
		}
	}

	// retrieve the strongly connected components for this type
	std::set<const FunctionDecl*>&& components = exprConv->funcDepGraph.getStronglyConnectedComponents( funcDecl );

	// save the current translation unit
	const TranslationUnit* oldTU = currTU;

	if (!components.empty()) {
		std::set<const FunctionDecl*>&& subComponents = exprConv->funcDepGraph.getSubComponents( funcDecl );

		std::for_each(subComponents.begin(), subComponents.end(),
				[&] (const FunctionDecl* cur) {

					FunctionDecl* decl = const_cast<FunctionDecl*>(cur);
					VLOG(2) << "Analyzing FuncDecl as sub component: " << decl->getNameAsString();
					const clang::idx::TranslationUnit* clangTU = this->getTranslationUnitForDefinition(decl);

					if ( clangTU && !isa<CXXConstructorDecl>(decl) ) { // not for constructors
						// update the translation unit
						this->currTU = &Program::getTranslationUnit(clangTU);
						// look up the lambda cache to see if this function has been
						// already converted into an IR lambda expression.
						ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(decl);
						if ( fit == ctx.lambdaExprCache.end() ) {
							// perform the conversion only if this is the first time this
							// function is encountred

							convertFunctionDecl(decl, false);
							ctx.recVarExprMap.clear();
						}
					}
				}
		);
	}

	// reset the translation unit
	currTU = oldTU;

	if (!components.empty()) {
		// we are dealing with a recursive type
		VLOG(1)
			<< "Analyzing FuncDecl: " << funcDecl->getNameAsString() << std::endl
					<< "Number of components in the cycle: " << components.size();
		std::for_each(components.begin(), components.end(), [ ] (std::set<const FunctionDecl*>::value_type c) {
			VLOG(2) << "\t" << c->getNameAsString( ) << "(" << c->param_size() << ")";
		});

		if (!ctx.isRecSubFunc) {
			if (ctx.recVarExprMap.find(funcDecl) == ctx.recVarExprMap.end()) {
				// we create a TypeVar for each type in the mutual dependence
				core::VariablePtr&& var = builder.variable( convertType( GET_TYPE_PTR(funcDecl) ) );
				ctx.recVarExprMap.insert( std::make_pair(funcDecl, var) );
			}
		} else {
			// we expect the var name to be in currVar
			ctx.recVarExprMap.insert( std::make_pair(funcDecl, ctx.currVar) );
		}

		// when a subtype is resolved we expect to already have these variables in the map
		if (!ctx.isRecSubFunc) {
			std::for_each(components.begin(), components.end(),
					[ this ] (std::set<const FunctionDecl*>::value_type fd) {

						if ( this->ctx.recVarExprMap.find(fd) == this->ctx.recVarExprMap.end() ) {
							core::FunctionTypePtr funcType =
							core::static_pointer_cast<const core::FunctionType>( this->convertType(GET_TYPE_PTR(fd)) );
							// In the case the function is receiving the global variables the signature needs to be
							// modified by allowing the global struct to be passed as an argument
					if ( this->ctx.globalFuncMap.find(fd) != this->ctx.globalFuncMap.end() ) {
						funcType = addGlobalsToFunctionType(this->builder, this->ctx.globalStruct.first, funcType);
					}
					core::VariablePtr&& var = this->builder.variable( funcType );
					this->ctx.recVarExprMap.insert( std::make_pair(fd, var ) );
				}
			});
		}
		if (VLOG_IS_ON(2)) {
			VLOG(2)
				<< "MAP: ";
			std::for_each(ctx.recVarExprMap.begin(), ctx.recVarExprMap.end(),
					[] (ConversionContext::RecVarExprMap::value_type c) {
						VLOG(2) << "\t" << c.first->getNameAsString() << "[" << c.first << "]";
					});
		}
	}

	// init parameter set
	vector<core::VariablePtr> params;

	/*
	 * before resolving the body we have to set the currGlobalVar accordingly depending if this function will use the
	 * global struct or not
	 */
	core::VariablePtr parentGlobalVar = ctx.globalVar;

	if (!isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		core::VariablePtr&& var = builder.variable( builder.refType(ctx.globalStruct.first) );
		params.push_back( var );
		ctx.globalVar = var;
	}

	std::for_each(funcDecl->param_begin(), funcDecl->param_end(), [ &params, this ] (ParmVarDecl* currParam) {
		params.push_back( core::static_pointer_cast<const core::Variable>( this->lookUpVariable(currParam) ) );
	});

	// this lambda is not yet in the map, we need to create it and add it to the cache
	assert(
			(components.empty() || (!components.empty() && !ctx.isResolvingRecFuncBody)) && "~~~ Something odd happened, you are allowed by all means to blame Simone ~~~");
	if (!components.empty()) {
		ctx.isResolvingRecFuncBody = true;
	}

	VLOG(2)
		<< "Visiting function body!";
	// set up context to contain current list of parameters and convert body
	ConversionContext::ParameterList oldList = ctx.curParameter;
	ctx.curParameter = &params;

	if (VLOG_IS_ON(2)) {
		VLOG(2)
			<< "Dump of stmt body: \n"
					<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		funcDecl->getBody()->dump();
	}

	core::StatementPtr&& body = convertStmt( funcDecl->getBody() );
	ctx.curParameter = oldList;

	/*
	 * if any of the parameters of this function has been marked as needRef, we need to add a declaration just before
	 * the body of this function
	 */
	vector<core::StatementPtr> decls;
	std::for_each(params.begin(), params.end(), [ &decls, &body, this ] (core::VariablePtr currParam) {
		auto fit = this->ctx.wrapRefMap.find(currParam);

		if ( fit != this->ctx.wrapRefMap.end() ) {
			// LOG(INFO) << "Replace";
			decls.push_back( this->builder.declarationStmt(fit->second, this->builder.refVar( fit->first ) ));
			/*
			 * replace this parameter in the body, example:
			 *
			 * int f(int a) {
			 *  for (...) {
			 *   x = a; <- if all the occurencies of a will not be replaced the semantics of
			 *   		   the code will not be preserved
			 *   a = i;
			 *  }
			 * }
			 *
			 *  as the variable can olny appear in the RHS of expression, we have to sobstitute it with its
			 *  dereference
			 */
			body = core::static_pointer_cast<const core::Statement>(
					core::transform::replaceAll( this->builder.getNodeManager(), body, fit->first,
							this->tryDeref(fit->second))
			);
		}

	});

	// if we introduce new decls we have to introduce them just before the body of the function
	if (!decls.empty()) {
		// push the old body
		decls.push_back(body);
		body = builder.compoundStmt(decls);
	}

	if (!components.empty()) {
		ctx.isResolvingRecFuncBody = false;
	}

	// ADD THE GLOBALS
	if (isEntryPoint && ctx.globalVar) {
		const core::CompoundStmtPtr& compStmt = builder.compoundStmt(body);
		assert(ctx.globalVar && ctx.globalStruct.second);

		const StatementList& oldStmts = compStmt->getStatements();

		std::vector<core::StatementPtr> stmts;

		stmts = std::vector<core::StatementPtr>(oldStmts.size() + 1);
		stmts[0] = builder.declarationStmt(ctx.globalVar, builder.refNew(ctx.globalStruct.second));
		std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), stmts.begin() + 1);

		body = builder.compoundStmt(stmts);
	}

	core::TypePtr convertedType = convertType(GET_TYPE_PTR(funcDecl));
	assert(convertedType->getNodeType() == core::NT_FunctionType && "Converted type has to be a function type!");
	core::FunctionTypePtr funcType = core::static_pointer_cast<const core::FunctionType>(convertedType);

	// if this function gets the globals in the capture list we have to create a different type
	if (!isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		funcType = addGlobalsToFunctionType(builder, ctx.globalStruct.first, funcType);
	}

	// reset old global var, thisVar, and offsetTable
	ctx.globalVar = parentGlobalVar;

	VLOG(2)	<< funcType << "\n" << params << "\n" << body;

	if (components.empty()) {

		core::LambdaExprPtr retLambdaExpr;

		retLambdaExpr = builder.lambdaExpr(params[params.size() - 1].getType(), body, params);

		// attach name annotation to the lambda - also done in attachFuncAnnotations()
		retLambdaExpr->getLambda()->addAnnotation(
				std::make_shared < annotations::c::CNameAnnotation > (funcDecl->getNameAsString()));

		// Adding the lambda function to the list of converted functions
		ctx.lambdaExprCache.insert(std::make_pair(funcDecl, retLambdaExpr));

		VLOG(2)
			<< retLambdaExpr << " + function declaration: " << funcDecl;
		return attachFuncAnnotations(retLambdaExpr, funcDecl);
		//return retLambdaExpr;
	}

	core::LambdaPtr&& retLambdaNode = builder.lambda( funcType, params, body );
	// attach name annotation to the lambda
	retLambdaNode->addAnnotation(std::make_shared < annotations::c::CNameAnnotation > (funcDecl->getNameAsString()));
	// this is a recurive function call
	if (ctx.isRecSubFunc) {
		/*
		 * if we are visiting a nested recursive type it means someone else will take care of building the rectype
		 * node, we just return an intermediate type
		 */
		return retLambdaNode;
	}

	// we have to create a recursive type
	ConversionContext::RecVarExprMap::const_iterator tit = ctx.recVarExprMap.find(funcDecl);
	assert(tit != ctx.recVarExprMap.end() && "Recursive function has not VarExpr associated to himself");
	core::VariablePtr recVarRef = tit->second;

	vector<core::LambdaBindingPtr> definitions;
	definitions.push_back(builder.lambdaBinding(recVarRef, retLambdaNode));

	// We start building the recursive type. In order to avoid loop the visitor
	// we have to change its behaviour and let him returns temporarely types
	// when a sub recursive type is visited.
	ctx.isRecSubFunc = true;

	std::for_each(components.begin(), components.end(),
			[ this, &definitions, &builder, &recVarRef ] (std::set<const FunctionDecl*>::value_type fd) {

				ConversionContext::RecVarExprMap::const_iterator tit = this->ctx.recVarExprMap.find(fd);
				assert(tit != this->ctx.recVarExprMap.end() && "Recursive function has no TypeVar associated");
				this->ctx.currVar = tit->second;

				// test whether function has already been resolved
			if (*tit->second == *recVarRef) {
				return;
			}

			/*
			 * we remove the variable from the list in order to fool the solver, in this way it will create a descriptor
			 * for this type (and he will not return the TypeVar associated with this recursive type). This behaviour
			 * is enabled only when the isRecSubType flag is true
			 */
			this->ctx.recVarExprMap.erase(fd);

			/*
			 * if the function is not defined in this translation unit, maybe it is defined in another we already loaded
			 * use the clang indexer to lookup the definition for this function declarations
			 */
			clang::idx::Entity&& funcEntity =
			clang::idx::Entity::get(const_cast<FunctionDecl*>(fd), this->program.getClangProgram());
			ConversionFactory::TranslationUnitPair&& ret = this->program.getClangIndexer().getDefinitionFor(funcEntity);
			const TranslationUnit* oldTU = this->currTU;
			if ( ret.first ) {
				fd = ret.first;
				assert(ret.second && "Error loading translation unit for function definition");
				this->currTU = &Program::getTranslationUnit(ret.second);
			}

			const core::LambdaPtr& lambda =
			core::static_pointer_cast<const core::Lambda>(this->convertFunctionDecl(fd));
			assert(lambda && "Resolution of sub recursive lambda yields a wrong result");
			this->currTU = oldTU;
			// attach name annotation to the lambda
			lambda->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>( fd->getNameAsString() ) );
			definitions.push_back( builder.lambdaBinding(this->ctx.currVar, lambda) );

			// reinsert the TypeVar in the map in order to solve the other recursive types
			this->ctx.recVarExprMap.insert( std::make_pair(fd, this->ctx.currVar) );
			this->ctx.currVar = NULL;
		});
	// we reset the behavior of the solver
	ctx.isRecSubFunc = false;

	core::LambdaDefinitionPtr&& definition = builder.lambdaDefinition(definitions);
	core::LambdaExprPtr&& retLambdaExpr = builder.lambdaExpr(recVarRef, definition);

	// Adding the lambda function to the list of converted functions
	ctx.lambdaExprCache.insert(std::make_pair(funcDecl, retLambdaExpr));
	// we also need to cache all the other recursive definition, so when we will resolve
	// another function in the recursion we will not repeat the process again
	std::for_each(components.begin(), components.end(),
			[ this, &definition ] (std::set<const FunctionDecl*>::value_type fd) {
				auto fit = this->ctx.recVarExprMap.find(fd);
				assert(fit != this->ctx.recVarExprMap.end());

				FunctionDecl* decl = const_cast<FunctionDecl*>(fd);
				const clang::idx::TranslationUnit* clangTU = this->getTranslationUnitForDefinition(decl);

				assert ( clangTU );
				// save old TU
			const TranslationUnit* oldTU = this->currTU;

			// update the translation unit
			this->currTU = &Program::getTranslationUnit(clangTU);

			core::ExpressionPtr&& func = builder.lambdaExpr(fit->second, definition);
			ctx.lambdaExprCache.insert( std::make_pair(decl, func) );

			func = this->attachFuncAnnotations(func, decl);

			currTU = oldTU;
		});

	VLOG(2)
		<< "Converted Into: " << *retLambdaExpr;

	return attachFuncAnnotations(retLambdaExpr, funcDecl);
}


//---------------------------------------------------------------------------------------------------------------------
//										CLANG EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------
class CXXConversionFactory::CXXExtExprConverter: public ConversionFactory::ClangExprConverter {

	CXXConversionFactory& convFact;

public:

	cpp::TemporaryHandler tempHandler;

	CXXExtExprConverter(CXXConversionFactory& convFact, Program& program) :
		ClangExprConverter(convFact, program),
		convFact(convFact),
		tempHandler(&convFact) {
		const utils::CXXCallExprVisitor& cev = utils::CXXCallExprVisitor(program.getClangIndexer());
		funcDepGraph = utils::FunctionDependencyGraph(program.getClangIndexer(), cev);
	}
	virtual ~CXXExtExprConverter();

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  IMPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitImplicitCastExpr(clang::ImplicitCastExpr* castExpr) {
		START_LOG_EXPR_CONVERSION(castExpr);
		const core::IRBuilder& builder = convFact.builder;

		core::ExpressionPtr retIr = Visit(castExpr->getSubExpr());
		LOG_CONVERSION(retIr);

		core::TypePtr classTypePtr; // used for CK_DerivedToBase
		core::StringValuePtr ident;

		// handle implicit casts according to their kind
		switch (castExpr->getCastKind()) {
		//case CK_ArrayToPointerDecay:
		//	return retIr;
		case CK_LValueToRValue:
			return (retIr = asRValue(retIr));

		case CK_UncheckedDerivedToBase:
		case CK_DerivedToBase: {
			// add CArray access
			if (GET_TYPE_PTR(castExpr)->isPointerType() && GET_TYPE_PTR(castExpr->getSubExpr())->isPointerType()) {
				//VLOG(2) << retIr;
				// deref not needed??? (Unchecked)DerviedToBase gets deref from LValueToRValue cast?
				//retIr = builder.deref(retIr);
				retIr = getCArrayElemRef(builder, retIr);
			}

			// for an inheritance like D -> C -> B -> A , and a cast of D to A
			// there is only one ImplicitCastExpr from clang, so we walk trough the inheritance
			// and create the member access. the iterator is in order so one gets C then B then A
			for (CastExpr::path_iterator I = castExpr->path_begin(), E = castExpr->path_end(); I != E; ++I) {
				const CXXBaseSpecifier* base = *I;
				const CXXRecordDecl* recordDecl = cast<CXXRecordDecl>(base->getType()->getAs<RecordType>()->getDecl());

				// find the class type - if not converted yet, converts and adds it
				classTypePtr = convFact.convertType(GET_TYPE_PTR(base));
				assert(classTypePtr && "no class declaration to type pointer mapping");

				//VLOG(2) << "member name " << recordDecl->getName().data();
				ident = builder.stringValue(recordDecl->getName().data());

				VLOG(2)
					<< "(Unchecked)DerivedToBase Cast on " << classTypePtr;

				core::ExpressionPtr op = builder.getLangBasic().getCompositeMemberAccess();
				core::TypePtr structTy = retIr->getType();

				if (structTy->getNodeType() == core::NT_RefType) {
					// skip over reference wrapper
					structTy = core::analysis::getReferencedType(structTy);
					op = builder.getLangBasic().getCompositeRefElem();
				}
				VLOG(2)
					<< structTy;

				const core::TypePtr& memberTy =
						core::static_pointer_cast<const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);
				core::TypePtr resType = builder.refType(classTypePtr);

				retIr = builder.callExpr(resType, op, retIr, builder.getIdentifierLiteral(ident),
						builder.getTypeLiteral(memberTy));
				VLOG(2)
					<< retIr;
			}
			return retIr;
		}

		case CK_NoOp:
			//CK_NoOp - A conversion which does not affect the type other than (possibly) adding qualifiers. int -> int char** -> const char * const *
			VLOG(2)
				<< "NoOp Cast";
			return retIr;

		default:
			// use default cast expr handling (fallback)
			return (retIr = VisitCastExpr(castExpr));
		}
		assert(false);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						EXPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitExplicitCastExpr(clang::ExplicitCastExpr* castExpr) {
		START_LOG_EXPR_CONVERSION(castExpr);

		const core::IRBuilder& builder = convFact.builder;
		core::ExpressionPtr retIr = Visit(castExpr->getSubExpr());
		LOG_CONVERSION(retIr);

		core::TypePtr classTypePtr; // used for CK_DerivedToBase
		core::StringValuePtr ident;
		VLOG(2)
			<< retIr << " " << retIr->getType();
		switch (castExpr->getCastKind()) {
		//case CK_ArrayToPointerDecay:
		//	return retIr;
		case CK_NoOp:
			//CK_NoOp - A conversion which does not affect the type other than (possibly) adding qualifiers. int -> int char** -> const char * const *
			VLOG(2)
				<< "NoOp Cast";
			return retIr;
		case CK_BaseToDerived: {
			VLOG(2)
				<< convFact.convertType(GET_TYPE_PTR(castExpr));

			// find the class type - if not converted yet, converts and adds it
			classTypePtr = convFact.convertType(GET_TYPE_PTR(castExpr));
			assert(classTypePtr && "no class declaration to type pointer mapping");

			VLOG(2)
				<< "BaseToDerived Cast" << classTypePtr;

			// explicitly cast base to derived with CAST-operator in IR
			if (GET_TYPE_PTR(castExpr)->isPointerType() && GET_TYPE_PTR(castExpr->getSubExpr())->isPointerType()) {
				retIr = builder.castExpr(classTypePtr, retIr);
			} else {
				retIr = builder.castExpr(builder.refType(classTypePtr), retIr);
			}
			return retIr;
		}

		case CK_DerivedToBase: {
			// pointer types (in IR) are ref<ref<array -> get deref first ref, and add CArray access
			if (GET_TYPE_PTR(castExpr)->isPointerType() && GET_TYPE_PTR(castExpr->getSubExpr())->isPointerType()) {
				//VLOG(2) << retIr;
				retIr = builder.deref(retIr);
				retIr = getCArrayElemRef(builder, retIr);
			}

			// for an inheritance like D -> C -> B -> A , and a cast of D to A
			// there is only one ExplicitCastExpr from clang, so we walk trough the inheritance
			// and create the member access. the iterator is in order so one gets C then B then A
			for (CastExpr::path_iterator I = castExpr->path_begin(), E = castExpr->path_end(); I != E; ++I) {
				const CXXBaseSpecifier* base = *I;
				const CXXRecordDecl* recordDecl = cast<CXXRecordDecl>(base->getType()->getAs<RecordType>()->getDecl());

				// find the class type - if not converted yet, converts and adds it
				classTypePtr = convFact.convertType(GET_TYPE_PTR(base));
				assert(classTypePtr && "no class declaration to type pointer mapping");

				VLOG(2)
					<< "member name " << recordDecl->getName().data();
				ident = builder.stringValue(recordDecl->getName().data());

				VLOG(2)
					<< "DerivedToBase Cast on " << classTypePtr;

				core::ExpressionPtr op = builder.getLangBasic().getCompositeMemberAccess();
				core::TypePtr structTy = retIr->getType();

				if (structTy->getNodeType() == core::NT_RefType) {
					// skip over reference wrapper
					structTy = core::analysis::getReferencedType(structTy);
					op = builder.getLangBasic().getCompositeRefElem();
				}
				VLOG(2)
					<< structTy;

				const core::TypePtr& memberTy =
						core::static_pointer_cast<const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);

				core::TypePtr resType = builder.refType(classTypePtr);

				retIr = builder.callExpr(resType, op, retIr, builder.getIdentifierLiteral(ident),
						builder.getTypeLiteral(memberTy));
				VLOG(2)
					<< retIr;
			}
			return retIr;
		}
		case CK_ConstructorConversion: {

			return retIr;
		}
		default:
			// use default cast expr handling (fallback)
			return (retIr = VisitCastExpr(castExpr));
		}

		assert(false);
	}


public:
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCallExpr(clang::CallExpr* callExpr) {

		START_LOG_EXPR_CONVERSION(callExpr);

		const core::IRBuilder& builder = convFact.builder;

		// return converted node
		core::ExpressionPtr irNode;
		LOG_CONVERSION(irNode);

		if (callExpr->getDirectCallee()) {

			FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee());

			core::FunctionTypePtr funcTy = core::static_pointer_cast<const core::FunctionType>(
					convFact.convertType(GET_TYPE_PTR(funcDecl)));

			// collects the type of each argument of the expression
			ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);

			assert( convFact.currTU && "Translation unit not set.");

			const TranslationUnit* oldTU = convFact.currTU;
			const FunctionDecl* definition = NULL;
			/*
			 * this will find function definitions if they are declared in  the same translation unit
			 * (also defined as static)
			 */
			if (!funcDecl->hasBody(definition)) {
				/*
				 * if the function is not defined in this translation unit, maybe it is defined in another we already
				 * loaded use the clang indexer to lookup the definition for this function declarations
				 */
				FunctionDecl* fd = funcDecl;
				const clang::idx::TranslationUnit* clangTU = convFact.getTranslationUnitForDefinition(fd);

				if (clangTU) {
					convFact.currTU = &Program::getTranslationUnit(clangTU);
				}

				if (clangTU && fd->hasBody()) {
					definition = fd;
				}
			}

			if (!definition) {
				//-----------------------------------------------------------------------------------------------------
				//     						Handle of 'special' built-in functions
				//-----------------------------------------------------------------------------------------------------
				// free(): check whether this is a call to the free() function
				if (funcDecl->getNameAsString() == "free" && callExpr->getNumArgs() == 1) {
					// in the case the free uses an input parameter
					if (args.front()->getType()->getNodeType() == core::NT_RefType) {
						return (irNode = builder.callExpr(builder.getLangBasic().getUnit(),
								builder.getLangBasic().getRefDelete(), args.front()));
					}

					// select appropriate deref operation: AnyRefDeref for void*, RefDeref for anything else
					core::ExpressionPtr arg = wrapVariable(callExpr->getArg(0));
					core::ExpressionPtr delOp =
							*arg->getType() == *builder.getLangBasic().getAnyRef() ?
									builder.getLangBasic().getAnyRefDelete() : builder.getLangBasic().getRefDelete();

					// otherwise this is not a L-Value so it needs to be wrapped into a variable
					return (irNode = builder.callExpr(builder.getLangBasic().getUnit(), delOp, arg));
				}
			}

			ExpressionList&& packedArgs = tryPack(convFact.builder, funcTy, args);

			if (!definition) {
				std::string callName = funcDecl->getNameAsString();
				// No definition has been found in any of the translation units, we mark this function as extern!
				irNode = convFact.builder.callExpr(funcTy->getReturnType(), builder.literal(callName, funcTy),
						packedArgs);

				// In the case this is a call to MPI, attach the loc annotation, handlling of those
				// statements will be then applied by mpi_sema
				if (callName.compare(0, 4, "MPI_") == 0) {
					std::pair<clang::SourceLocation, clang::SourceLocation>&& loc =
					std::make_pair(callExpr->getLocStart(), callExpr->getLocEnd());

					// add a marker node because multiple istances of the same MPI call must be distinct
					// LOG(INFO) << funcTy << std::endl;

					irNode = builder.markerExpr( core::static_pointer_cast<const core::Expression>(irNode) );

					irNode->addAnnotation( std::make_shared<annotations::c::CLocAnnotation>(
									convertClangSrcLoc(convFact.getCurrentSourceManager(), loc.first),
									convertClangSrcLoc(convFact.getCurrentSourceManager(), loc.second))
					);
				}
				convFact.currTU = oldTU;

				return irNode;
			}

			/*
			 * We find a definition, we lookup if this variable needs to access the globals, in that case the capture
			 * list needs to be initialized with the value of global variable in the current scope
			 */
			if (ctx.globalFuncMap.find(definition) != ctx.globalFuncMap.end()) {
				/*
				 * we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
				 * current context
				 */
				assert( ctx.globalVar && "No global definitions forwarded to this point");
				packedArgs.insert(packedArgs.begin(), ctx.globalVar);
			}

			/*
			 * If we are resolving the body of a recursive function we have to return the associated variable every
			 * time a function in the strongly connected graph of function calls is encountered.
			 */
			if (ctx.isResolvingRecFuncBody) {
				// check if this type has a typevar already associated, in such case return it
				ConversionContext::RecVarExprMap::const_iterator fit = ctx.recVarExprMap.find(definition);
				if (fit != ctx.recVarExprMap.end()) {
					/*
					 * we are resolving a parent recursive type, so when one of the recursive functions in the
					 * connected components are called, the introduced mu variable has to be used instead.
					 */
					convFact.currTU = oldTU;
					return (irNode = builder.callExpr(funcTy->getReturnType(),
							static_cast<core::ExpressionPtr>(fit->second), packedArgs));
				}
			}

			if (!ctx.isResolvingRecFuncBody) {

				ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(definition);

				if (fit != ctx.lambdaExprCache.end()) {

					std::vector<core::VariablePtr> temporaries = tempHandler.retrieveFunctionTemporaries(definition,
							convFact.ctx.fun2TempMap);

					vector<core::VariablePtr>::iterator it;

					for (it = temporaries.begin(); it < temporaries.end(); it++) {

						core::VariablePtr var = *it;
						packedArgs.push_back(var);
						convFact.ctx.scopeObjects.push(var);
						funcTy = tempHandler.addThisArgToFunctionType(builder, builder.deref(var).getType(), funcTy);

					}

					convFact.currTU = oldTU;

					irNode = builder.callExpr(funcTy->getReturnType(), static_cast<core::ExpressionPtr>(fit->second),
							packedArgs);

					convFact.currTU = oldTU;

					return irNode;
				}
			}

			assert(definition && "No definition found for function");

			ConversionFactory::ConversionContext::ScopeObjects parentScopeObjects = convFact.ctx.scopeObjects;
			while (!convFact.ctx.scopeObjects.empty()) {
				convFact.ctx.scopeObjects.pop();
			}

			core::ExpressionPtr lambdaExpr = core::static_pointer_cast<const core::LambdaExpr>(
					convFact.convertFunctionDecl(definition));

			std::vector<core::VariablePtr> temporaries = tempHandler.retrieveFunctionTemporaries(definition,
					convFact.ctx.fun2TempMap);

			vector<core::VariablePtr>::iterator it;

			for (it = temporaries.begin(); it < temporaries.end(); it++) {

				core::VariablePtr var = *it;
				packedArgs.push_back(var);
				VLOG(2)
					<< var;
				parentScopeObjects.push(var);
				funcTy = tempHandler.addThisArgToFunctionType(builder, builder.deref(var).getType(), funcTy);
			}

			convFact.currTU = oldTU;
			convFact.ctx.scopeObjects = parentScopeObjects;

			return (irNode = builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs));

		} else if (callExpr->getCallee()) {
			core::ExpressionPtr funcPtr = convFact.tryDeref(Visit(callExpr->getCallee()));
			core::TypePtr subTy = funcPtr->getType();

			if (subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) {
				subTy = core::static_pointer_cast<const core::SingleElementType>(subTy)->getElementType();
				funcPtr = builder.callExpr(subTy, builder.getLangBasic().getArraySubscript1D(), funcPtr,
						builder.uintLit(0));
			}assert( subTy->getNodeType() == core::NT_FunctionType && "Using () operator on a non function object");

			const core::FunctionTypePtr& funcTy = core::static_pointer_cast<const core::FunctionType>(subTy);
			ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);
			return (irNode = builder.callExpr(funcPtr, args));

		} else {
			assert( false && "Call expression not referring a function");
		}
		assert(false);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
		START_LOG_EXPR_CONVERSION(declRef);

		core::ExpressionPtr retIr;
		LOG_CONVERSION(retIr);

		// check whether this is a reference to a variable
		core::ExpressionPtr retExpr;
		if (ParmVarDecl* parmDecl = dyn_cast<ParmVarDecl>(declRef->getDecl())) {
			VLOG(2) << "Parameter type: " << convFact.convertType(parmDecl->getOriginalType().getTypePtr() );
			return ( retIr = convFact.lookUpVariable( parmDecl ) );
		}
		if ( VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl()) ) {

			retIr = convFact.lookUpVariable( varDecl );

			if(GET_TYPE_PTR(varDecl)->isReferenceType()) {
				retIr = convFact.tryDeref(retIr);
			}

			return retIr;
		}
		if( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(declRef->getDecl()) ) {
			return (retIr =
					core::static_pointer_cast<const core::Expression>(
							convFact.convertFunctionDecl(funcDecl)
					)
			);
		}
		if (EnumConstantDecl* enumDecl = dyn_cast<EnumConstantDecl>(declRef->getDecl() ) ) {
			return (retIr =
					convFact.builder.literal(
							enumDecl->getInitVal().toString(10),
							convFact.builder.getLangBasic().getInt4()
					)
			);
		}
		// todo: C++ check whether this is a reference to a class field, or method (function).
		assert(false && "DeclRefExpr not supported!");
	}

	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXBoolLiteralExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXMemberCallExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXOperatorCallExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXConstructExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXNewExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXDeleteExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXThisExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXThrowExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXDefaultArgExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(CXXBindTemporaryExpr)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(ExprWithCleanups)
	FORWARD_EXP_TO_CXX_EXPR_VISITOR_CALL(MaterializeTemporaryExpr)
};

CXXConversionFactory::CXXExtExprConverter*
CXXConversionFactory::makeExprConvert(CXXConversionFactory& fact, Program& program) {
	return new CXXConversionFactory::CXXExtExprConverter(fact, program);
}

void CXXConversionFactory::cleanExprConvert(CXXConversionFactory::CXXExtExprConverter* exprConv) {
	delete exprConv;
}

//
///**************************************************************************************************
// * InitListExpr describes an initializer list, which can be used to initialize objects of different
// * types, InitListExpr including struct/class/union types, arrays, and vectors. For example:
// *
// * struct foo x = { 1, { 2, 3 } };
// *
// * In insieme this statement has to tranformed into a StructExpr, or VectorExpr depending on the
// * type of the LHS expression.
// **************************************************************************************************/
//core::ExpressionPtr
//CXXConversionFactory::convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type) const {
//	const ConversionFactory& convFact = *this;
//	START_LOG_EXPR_CONVERSION(initList);
//
//	core::ExpressionPtr retIr;
////	ATTACH_OMP_ANNOTATIONS(retIr, initList);
//	LOG_CONVERSION(retIr);
//
//	bool isRef = false;
//	core::TypePtr currType = type;
//	if ( core::RefTypePtr&& refType = core::dynamic_pointer_cast<const core::RefType>(type)) {
//		isRef = true;
//		currType = refType->getElementType();
//	}
//
//	if (currType->getNodeType() == core::NT_VectorType || currType->getNodeType() == core::NT_ArrayType) {
//
//		const core::TypePtr& elemTy =
//				core::static_pointer_cast<const core::SingleElementType>(currType)->getElementType();
//
//		ExpressionList elements;
//		// get all values of the init expression
//		for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
//			const clang::Expr* subExpr = initList->getInit(i);
//			core::ExpressionPtr&& convExpr = convertInitExpr(subExpr, elemTy, false);
//
//			assert(convExpr && "convExpr is empty");
//			elements.push_back(utils::cast(convExpr, elemTy));
//		}
//
//		if (elements.size() == 1 && currType->getNodeType() == core::NT_VectorType) {
//			const core::VectorTypePtr& vecTy = core::static_pointer_cast<const core::VectorType>(currType);
//			// In C when the initializer list contains 1 elements then all the elements of the
//			// vector (or array) must be initialized with the same value
//			const core::ConcreteIntTypeParamPtr& vecArgSize =
//					core::static_pointer_cast<const core::ConcreteIntTypeParam>(vecTy->getSize());
//
//			retIr = builder.callExpr(vecTy, builder.getLangBasic().getVectorInitUniform(), elements.front(),
//					builder.getIntTypeParamLiteral(vecArgSize));
//
//		} else
//			retIr = builder.vectorExpr(elements);
//	}
//
//	/*
//	 * in the case the initexpr is used to initialize a struct/class we need to create a structExpr to initialize the
//	 * structure
//	 */
//	if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(currType)) {
//		core::StructExpr::Members members;
//		for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
//			const core::NamedTypePtr& curr = structTy->getEntries()[i];
//			members.push_back(
//					builder.namedValue(curr->getName(), convertInitExpr(initList->getInit(i), curr->getType(), false)));
//		}
//		retIr = builder.structExpr(members);
//	}
//
//	/*
//	 * in the case the initexpr is used to initialize a union
//	 */
//	if ( core::UnionTypePtr&& unionTy = core::dynamic_pointer_cast<const core::UnionType>(currType)) {
//		core::ExpressionPtr ie = convertInitExpr(initList->getInit(0), unionTy->getEntries()[0]->getType(), false);
//		retIr = builder.unionExpr(unionTy, unionTy->getEntries()[0]->getName(), ie);
//		LOG(DEBUG) << *retIr;
//
//	//	core::StructExpr::Members members;
//	//	for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
//	//		const core::NamedTypePtr& curr = structTy->getEntries()[i];
//	//		members.push_back(
//	//				builder.namedValue(curr->getName(), convertInitExpr(initList->getInit(i), curr->getType(), false)));
//	//	}
//	//	retIr = builder.structExpr(members);
//	}
//
//	assert(retIr && "Couldn't convert initialization expression");
//
//	if (isRef) {
//		retIr = builder.refVar(retIr);
//	}
//	// create vector initializator
//	return retIr;
//}

core::ExpressionPtr CXXConversionFactory::convertInitExpr(const clang::Expr* expr, const core::TypePtr& type,
		const bool zeroInit) const {
	core::ExpressionPtr retIr;
	// ATTACH_OMP_ANNOTATIONS(retIr, initList);
	LOG_CONVERSION(retIr);

	// get kind of initialized value
	core::NodeType&& kind =
	(type->getNodeType() != core::NT_RefType ? type->getNodeType() : GET_REF_ELEM_TYPE(type)->getNodeType() );

	if (!expr) {
		// if no init expression is provided => use undefined for given set of types
		if (kind == core::NT_StructType || kind == core::NT_UnionType || kind == core::NT_ArrayType
				|| kind == core::NT_VectorType) {
			if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
				const core::TypePtr& res = refTy->getElementType();
				return (retIr = builder.refVar(
						builder.callExpr(res,
								(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
								builder.getTypeLiteral(res))));
			}
			return (retIr = builder.callExpr(type,
					(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
					builder.getTypeLiteral(type)));
		} else {
			return (retIr = defaultInitVal(type));
		}
	}

	/*
	 * if an expression is provided as initializer first check if this is an initializer list which is used for arrays,
	 * structs and unions
	 */
	if ( const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( expr )) {
		return (retIr = convertInitializerList(listExpr, type));
	}

	// init the cpp class / struct - check here for enabled cpp in compiler lang options
	if (kind == core::NT_StructType && currTU->getCompiler().getPreprocessor().getLangOptions().CPlusPlus == 1) {

		if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
			const core::TypePtr& res = refTy->getElementType();
			retIr = builder.refVar(
					builder.callExpr(res,
							(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
							builder.getTypeLiteral(res)));
		}assert(retIr && "call expression is empty");
		return retIr;
	}

	// Convert the expression like any other expression
	retIr = convertExpr(expr);

	if (core::analysis::isCallOf(retIr, mgr.getLangBasic().getArrayCreate1D())) {
		retIr = builder.callExpr(builder.refType(retIr->getType()), mgr.getLangBasic().getRefNew(), retIr);
	}

	// fix type if necessary (also converts "Hello" into ['H','e',...])
	core::TypePtr valueType = type;
	if (type->getNodeType() == core::NT_RefType) {
		valueType = core::analysis::getReferencedType(valueType);
	}

	retIr = utils::cast(retIr, valueType);

	// if result is a reference type => create new local variable
	if (type->getNodeType() == core::NT_RefType) {
		retIr = builder.callExpr(type, mgr.getLangBasic().getRefVar(), retIr);
	}

	return retIr;
}

// the THIS parameter is added on the last position of the function parameters
core::FunctionTypePtr CXXConversionFactory::addThisArgToFunctionType(const core::IRBuilder& builder,
		const core::TypePtr& structTy, const core::FunctionTypePtr& funcType) {

	const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();

	std::vector<core::TypePtr> argTypes(oldArgs.size() + 1);

	std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin());
	// move THIS to the last position
	argTypes[oldArgs.size()] = builder.refType(structTy);
	return builder.functionType(argTypes, funcType->getReturnType());

}

// update __class member in all the dynamic baseClasses of the given recDecl
vector<core::StatementPtr> CXXConversionFactory::updateClassId(const clang::CXXRecordDecl* recDecl,
		core::ExpressionPtr expr,
		unsigned int classId) {
	bool hasPolymorphicBaseClass = false;
	vector<core::StatementPtr> retVec;
	core::TypePtr classTypePtr;

	ConversionContext::ClassDeclMap::const_iterator cit = ctx.classDeclMap.find(recDecl);
	if (cit != ctx.classDeclMap.end()) {
		classTypePtr = cit->second;
	}assert(classTypePtr && "no class declaration to type pointer mapping");

	for (clang::CXXRecordDecl::base_class_const_iterator bit = recDecl->bases_begin(); bit != recDecl->bases_end();
			bit++) {
		const CXXBaseSpecifier* base = bit;
		const CXXRecordDecl* baseRecord = base->getType()->getAsCXXRecordDecl();

		hasPolymorphicBaseClass |= baseRecord->isPolymorphic();
	}

	if (recDecl->isPolymorphic() && !hasPolymorphicBaseClass) {
		//update __class
		core::StringValuePtr ident = builder.stringValue("__class");
		const core::TypePtr& memberTy = classTypePtr.as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);

		expr = builder.callExpr(
				builder.refType(memberTy),
				builder.getLangBasic().getCompositeRefElem(),
				toVector<core::ExpressionPtr>(expr, builder.getIdentifierLiteral(ident),
						builder.getTypeLiteral(memberTy)));

		const core::StatementPtr& assign = builder.callExpr(builder.getLangBasic().getUnit(),
				builder.getLangBasic().getRefAssign(), expr,
				builder.literal(builder.getLangBasic().getUInt4(), toString(classId)));

		retVec.push_back(assign);
	} else {
		for (clang::CXXRecordDecl::base_class_const_iterator bit = recDecl->bases_begin(); bit != recDecl->bases_end();
				bit++) {
			const CXXBaseSpecifier* base = bit;
			const CXXRecordDecl* baseRecord = base->getType()->getAsCXXRecordDecl();

			if (baseRecord->isPolymorphic()) {
				core::StringValuePtr ident = builder.stringValue(baseRecord->getNameAsString());
				const core::TypePtr& memberTy = classTypePtr.as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);

				//expr = expr->baseRecord
				core::ExpressionPtr resExpr = builder.callExpr(
						builder.refType(memberTy),
						builder.getLangBasic().getCompositeRefElem(),
						toVector<core::ExpressionPtr>(expr, builder.getIdentifierLiteral(ident),
								builder.getTypeLiteral(memberTy)));

				const vector<core::StatementPtr>& result = CXXConversionFactory::updateClassId(baseRecord, resExpr,
						classId);
				retVec.insert(retVec.end(), result.begin(), result.end());
			}
		}
	}
	return retVec;
}

// create initializations statments for the offsetTable
vector<core::StatementPtr> CXXConversionFactory::initOffsetTable() {
	std::vector<core::StatementPtr> initOffsetTableStmts;
	//VLOG(2) << "OffsetMap: " << ctx.offsetMap;
	for (ConversionFactory::ConversionContext::OffsetMap::iterator it = ctx.offsetMap.begin();
			it != ctx.offsetMap.end(); ++it) {
		//VLOG(2) << "Offset:" << it->first.first->getNameAsString() << it->first.second->getNameAsString() << " = " << it->second;

		//access to the offset array
		core::ExpressionPtr vFuncOffset = ctx.offsetTableExpr;

		unsigned int row = ctx.polymorphicClassMap.find(it->first.first)->second.first;
		unsigned int col = ctx.polymorphicClassMap.find(it->first.second)->second.first;
		int offset = it->second;

		core::ExpressionPtr op = builder.getLangBasic().getVectorRefElem();
		core::TypePtr&& resTy = builder.refType(
				builder.vectorType(
						builder.getLangBasic().getInt4(),
						core::ConcreteIntTypeParam::get(builder.getNodeManager(), ctx.polymorphicClassMap.size())
				)
		);
		vFuncOffset = builder.callExpr(resTy, op, vFuncOffset,
				builder.literal(builder.getLangBasic().getUInt4(), toString(row)));

		op = builder.getLangBasic().getVectorRefElem();
		resTy = builder.refType(builder.getLangBasic().getInt4());
		vFuncOffset = builder.callExpr(resTy, op, vFuncOffset,
				builder.literal(builder.getLangBasic().getUInt4(), toString(col)));

		//assign the offset
		op = builder.getLangBasic().getRefAssign();
		resTy = builder.getLangBasic().getUnit();

		initOffsetTableStmts.push_back(
				builder.callExpr(resTy, op, vFuncOffset,
						builder.literal(builder.getLangBasic().getInt4(), toString(offset))));
	}
	return initOffsetTableStmts;
}

// create initializations statments for the vFuncTable
vector<core::StatementPtr> CXXConversionFactory::initVFuncTable() {
	std::vector<core::StatementPtr> initVFuncTableStmts;

	for (ConversionFactory::ConversionContext::FinalOverriderMap::iterator foit = ctx.finalOverriderMap.begin();
			foit != ctx.finalOverriderMap.end(); foit++) {
		const clang::CXXRecordDecl* recDecl = foit->first;
		const vector<std::pair<const clang::CXXMethodDecl*, const clang::CXXMethodDecl*>>& finalOverriders =
				foit->second;
		unsigned int classId = ctx.polymorphicClassMap.find(recDecl)->second.first;

		for (vector<std::pair<const clang::CXXMethodDecl*, const clang::CXXMethodDecl*>>::const_iterator it =
				finalOverriders.begin(); it != finalOverriders.end(); it++) {
			const clang::CXXMethodDecl* toBeOverriden = it->first; //the function which will be overriden by the "final overrider"
			const clang::CXXMethodDecl* overrider = it->second; //the actual function which will be dispatch for a virtual function call

			//get FunctionId
			unsigned int functionId = ctx.virtualFunctionIdMap.find(toBeOverriden)->second;

			//get Offset (offset[recDecl,toBeOverriden->parent])
			int offset = ctx.offsetMap.find(std::make_pair(recDecl, toBeOverriden->getParent()))->second;

			//create initExpr
			VLOG(2)
				<< "vfuncinit: vFuncTable[" << recDecl->getNameAsString() << "]["
						<< toBeOverriden->getParent()->getNameAsString() << "] = "
						<< overrider->getParent()->getNameAsString() << "::" << overrider->getNameAsString()
						<< " finally overrides " << toBeOverriden->getParent()->getNameAsString() << "::"
						<< toBeOverriden->getNameAsString();

			// create access to row: vfuncTable[classId]
			core::ExpressionPtr vFunctionTable = ctx.vFuncTableExpr;
			core::ExpressionPtr op = builder.getLangBasic().getVectorRefElem();
			vFunctionTable = builder.callExpr(op, vFunctionTable,
					builder.literal(builder.getLangBasic().getUInt4(), toString(classId)));

			// create access to element of row vfuncTable[classId][offset + functionId] (should be type ref<anyRef>)
			op = builder.getLangBasic().getVectorRefElem();
			vFunctionTable = builder.callExpr(op, vFunctionTable,
					builder.literal(builder.getLangBasic().getUInt4(), toString(offset + functionId)));

			core::ExpressionPtr vFuncPointerExpr;
			if (overrider->isPure()) {
				//abstract functions have no declaration
				vFuncPointerExpr = builder.getLangBasic().getNull();
			} else {
				core::TypePtr classTypePtr = convertType(recDecl->getTypeForDecl());
				assert(classTypePtr && "no class declaration to type pointer mapping");
				core::ExpressionPtr thisStack2old = ctx.thisStack2;
				ctx.thisStack2 = builder.variable(builder.refType(classTypePtr));

				//Convert virtual function, and get function pointer WITHOUT this/return-adjustment
				core::ExpressionPtr vFuncExpr = core::static_pointer_cast<const core::LambdaExpr>(
						convertFunctionDecl(overrider, false));

				if (overrider == toBeOverriden) {
					// function DOESN'T NEED this/return adjustment
					// as overrider and toBeOverriden are the same -> nothing overriden
					VLOG(2)
						<< "no this-adjustment needed:	 " << overrider->getParent()->getNameAsString() << "::"
								<< overrider->getNameAsString() << " finally overrides "
								<< toBeOverriden->getParent()->getNameAsString() << "::"
								<< toBeOverriden->getNameAsString();

					const clang::Type* overriderResultType = overrider->getResultType().getTypePtr();
					const clang::Type* toBeOverridenResultType = toBeOverriden->getResultType().getTypePtr();
					if ((overriderResultType->isPointerType() || overriderResultType->isReferenceType())
							&& overriderResultType->getPointeeType().getTypePtr()->isStructureOrClassType()) {
						clang::CXXRecordDecl* orResRecDecl =
								overriderResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl();
						clang::CXXRecordDecl* tboResRecDecl =
								toBeOverridenResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl();
						VLOG(2)
							<< "no return-adjustment needed: " << orResRecDecl->getNameAsString() << " "
									<< tboResRecDecl->getNameAsString();
					}
				} else {
					//function NEEDS THIS ADJUSTMENT
					VLOG(2)
						<< "this-adjustment needed:	" << overrider->getParent()->getNameAsString() << "::"
								<< overrider->getNameAsString() << " finally overrides "
								<< toBeOverriden->getParent()->getNameAsString() << "::"
								<< toBeOverriden->getNameAsString();

					//functions which are overriding some derived functions need this/return-adjustment
					for (clang::CXXMethodDecl::method_iterator it = overrider->begin_overridden_methods();
							it != overrider->end_overridden_methods(); it++) {
						VLOG(2)
							<< "vFuncTable[" << recDecl->getNameAsString() << "]["
									<< toBeOverriden->getParent()->getNameAsString() << "] = "
									<< overrider->getParent()->getNameAsString() << "::" << overrider->getNameAsString()
									<< " needs this-adjustment: " << "from "
									<< toBeOverriden->getParent()->getNameAsString() << " to "
									<< overrider->getParent()->getNameAsString();

						// add this adjustment from toBeOverriden->getParent to overrider->getParent -> IR: expand-operator
						// thunk(this, args, ...) {
						// 	 toType newThis = expand(this,fromType, toType, path)
						//	 vfunc(newThis, args...)
						//	 returnAdjustment /*if needed*/
						// }(this, args, ...)
						// create "thunk" for this-adjustment: new function, taking the arguments of vFuncExpr, adjust this, call vFuncExpr

						//fromRecDecl -> recDecl of toBeOverriden->getParent
						const clang::CXXRecordDecl* fromRecDecl = toBeOverriden->getParent();
						//fromTy = IR-typeOf(toBeOverriden->getParent)
						core::TypePtr fromTy = convertType(fromRecDecl->getTypeForDecl());
						assert(fromTy && "no class declaration to type pointer mapping");

						//toRecDecl -> recDecl of overrider->getParent
						const clang::CXXRecordDecl* toRecDecl = overrider->getParent();
						//toTy = IR-typeOf(overrider->getParent)
						core::TypePtr toTy = convertType(toRecDecl->getTypeForDecl());
						assert(toTy && "no class declaration to type pointer mapping");

						//get the function type for the overrider
						core::FunctionTypePtr vFuncTy = core::static_pointer_cast<const core::FunctionType>(
								convertType(GET_TYPE_PTR(overrider)));
						core::FunctionTypePtr thunkTy;

						if (ctx.globalFuncMap.find(overrider) != ctx.globalFuncMap.end()) {
							// declare a new variable that will be used to hold a reference to the global data stucture
							vFuncTy = addGlobalsToFunctionType(builder, ctx.globalStruct.first, vFuncTy);
						}

						thunkTy = addThisArgToFunctionType(builder, fromTy, vFuncTy); //function type of thunk
						vFuncTy = addThisArgToFunctionType(builder, toTy, vFuncTy); //adjusted function type of virtual function
						VLOG(2)
							<< "vFuncTy: " << vFuncTy;
						VLOG(2)
							<< "thunkTy: " << thunkTy;

						vector<core::ExpressionPtr> vFuncArgs; //arguments of vFuncExpr
						vector<core::VariablePtr> thunkParams; //parameter of thunk

						//create "new" thisVar-> with type toTy
						core::VariablePtr&& thunkThis = builder.variable( builder.refType(fromTy) ); //the "this" variable used in the thunk
						core::VariablePtr&& adjustedThis = builder.variable( builder.refType(toTy) ); //the adjusted "this" used in the vFunc

						//create variables for all parameters to be used in the thunk for vFunc
						// BUT NOT THE LAST ONE -> "this" TODO: move this to 1. position of parameters/arguments
						for (vector<core::TypePtr>::const_iterator it =
								(thunkTy->getParameterTypes()->getTypes()).begin();
								it != (thunkTy->getParameterTypes()->getTypes()).end() - 1 /*leave out the last one -> "this"*/;
								it++) {
							const core::VariablePtr& var = builder.variable(*it);
							vFuncArgs.push_back(var);
							thunkParams.push_back(var);
						}

						//add "this" AT END (TODO: put "this" at the beginning of parameters/arguments)
						thunkParams.push_back(thunkThis);
						vFuncArgs.push_back(adjustedThis);
						VLOG(2)
							<< "thunkParams: " << thunkParams;
						VLOG(2)
							<< "vFuncArgs:	 " << vFuncArgs;

						// create dataPath for expansion from fromTy to toTy
						core::datapath::DataPathBuilder dpManager(mgr);
						clang::CXXBasePaths paths;
						if (toRecDecl->isDerivedFrom(fromRecDecl, paths)) {
							for (clang::CXXBasePaths::paths_iterator bp = paths.begin(); bp != paths.end(); bp++) {
								for (clang::CXXBasePath::iterator bpe = bp->begin(); bpe != bp->end(); bpe++) {
									const CXXRecordDecl* currRecDecl = bpe->Class;
									//VLOG(2) << currRecDecl->getNameAsString();
									dpManager.member(currRecDecl->getNameAsString());
								}
								//VLOG(2) << fromRecDecl->getNameAsString();
								dpManager.member(fromRecDecl->getNameAsString());
							}
							//ref.expand(ref<'a>, datapath, type<'b>) -> ref<'b>
							//VLOG(2) << builder.getLangBasic().getRefExpand();
							//VLOG(2) << thunkThis;
							//VLOG(2) << dpManager.getPath();
							//VLOG(2) << toTy;
							//VLOG(2) << builder.callExpr(builder.refType(toTy), builder.getLangBasic().getRefExpand(), toVector<core::ExpressionPtr>(thunkThis, dpManager.getPath(), builder.getTypeLiteral(toTy)
						}

						// "expand" the actual this to the adjustedThis --> actual this-adjustment
						const core::StatementPtr& adjustedThisAssign = builder.declarationStmt(
								adjustedThis,
								builder.callExpr(
										builder.refType(toTy),
										builder.getLangBasic().getRefExpand(),
										toVector<core::ExpressionPtr>(thunkThis, dpManager.getPath(),
												builder.getTypeLiteral(toTy))));

						core::TypePtr thunkResTy; //result type of thunk
						core::TypePtr vFuncResTy = vFuncTy->getReturnType(); //result type of vFuncExpr

						//create call to vFuncExpr
						core::ExpressionPtr callVFunc = builder.callExpr(vFuncResTy, vFuncExpr, vFuncArgs);

						//check if function has a return value
						core::StatementPtr retCallVFunc;
						if (overrider->getResultType().getTypePtr()->isVoidType()) {
							//function with void as return type -> nothing to be done

							retCallVFunc = static_cast<core::StatementPtr>(callVFunc);
							// return Type of the thunk is the same as the return type of the virtual function
							thunkResTy = vFuncResTy;
						} else {
							//function has return value

							//check if return value needs return-adjustment
							const clang::Type* overriderResultType = overrider->getResultType().getTypePtr();
							const clang::Type* toBeOverridenResultType = toBeOverriden->getResultType().getTypePtr();
							if ((overriderResultType->isPointerType() || overriderResultType->isReferenceType())
									&& (toBeOverridenResultType->isPointerType()
											|| toBeOverridenResultType->isReferenceType())
									&& overriderResultType->getPointeeType().getTypePtr()->isStructureOrClassType()
									&& toBeOverridenResultType->getPointeeType().getTypePtr()->isStructureOrClassType()) {

								//	C++ Standard, 10.3.5
								//	for covariant return types we need a return-adjustment
								//	covariant:	- return types of overrider and toBeOverriden are pointer/reference of classes
								//				- class of the return type of overrider is the same as class of the return type of toBeOverriden,
								//				  or an unambigous and accessible direct or indirect base class of the return type of toBeOverriden
								//				- both pointers/references have the same cv-qualifiers, or return type of overrider has less

								//	adjust from overrider->returnType to toBeOverriden->returnType
								clang::CXXRecordDecl* orResRecDecl =
										overriderResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl();
								clang::CXXRecordDecl* tboResRecDecl =
										toBeOverridenResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl();
								clang::CXXBasePaths paths;
								if (orResRecDecl->isDerivedFrom(tboResRecDecl, paths)) {
									//we need return adjustment
									VLOG(2)
										<< "needs return-adjustment from overrider to toBeOverriden ";
									VLOG(2)
										<< "overrider: " << overriderResultType->isPointerType() << " "
												<< overriderResultType->isReferenceType() << " "
												<< overriderResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl()->getNameAsString();
									VLOG(2)
										<< "toBeOverriden: " << toBeOverridenResultType->isPointerType() << " "
												<< toBeOverridenResultType->isReferenceType() << " "
												<< toBeOverridenResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl()->getNameAsString();

									// if we have a pointer get access to the element
									if (overriderResultType->isPointerType()) {
										callVFunc = getCArrayElemRef(builder, callVFunc);
									}

									//get return value of callVFunc, and walk along path from overrider_ResultType to toBeOverriden_ResultType
									for (clang::CXXBasePaths::paths_iterator bp = paths.begin(); bp != paths.end();
											bp++) {
										for (clang::CXXBasePath::iterator bpe = bp->begin(); bpe != bp->end(); bpe++) {

											const CXXRecordDecl* baseRecDecl = bpe->Class;
											if (baseRecDecl == orResRecDecl) {
												//step over first node in path as it is the result type of overrider
												continue;
											} else {
												// find the class type - if not converted yet, converts and adds it
												core::TypePtr baseClassTypePtr = convertType(
														baseRecDecl->getTypeForDecl());
												assert(
														baseClassTypePtr && "no class declaration to type pointer mapping");

												core::StringValuePtr ident = builder.stringValue(
														baseRecDecl->getName().data());

												core::TypePtr resType = builder.refType(baseClassTypePtr);
												core::ExpressionPtr op =
														builder.getLangBasic().getCompositeMemberAccess();
												core::TypePtr structTy = callVFunc->getType();

												if (structTy->getNodeType() == core::NT_RefType) {
													// skip over reference wrapper
													structTy = core::analysis::getReferencedType(structTy);
													op = builder.getLangBasic().getCompositeRefElem();
												}

												const core::TypePtr& memberTy = core::static_pointer_cast<
														const core::NamedCompositeType>(structTy)->getTypeOfMember(
														ident);
												callVFunc = builder.callExpr(resType, op, callVFunc,
														builder.getIdentifierLiteral(ident),
														builder.getTypeLiteral(memberTy));
											}
										}
										//add the final access: FOO.Bar.toBeOverridenResultType

										// find the class type - if not converted yet, converts and adds it
										core::TypePtr baseClassTypePtr = convertType(tboResRecDecl->getTypeForDecl());
										assert(baseClassTypePtr && "no class declaration to type pointer mapping");

										core::StringValuePtr ident = builder.stringValue(
												tboResRecDecl->getName().data());
										core::TypePtr resType = builder.refType(baseClassTypePtr);

										//final return Type of the thunk
										thunkResTy = resType;
										core::ExpressionPtr op = builder.getLangBasic().getCompositeMemberAccess();

										core::TypePtr structTy = callVFunc->getType();
										if (structTy->getNodeType() == core::NT_RefType) {
											// skip over reference wrapper
											structTy = core::analysis::getReferencedType(structTy);
											op = builder.getLangBasic().getCompositeRefElem();
										}

										const core::TypePtr& memberTy = core::static_pointer_cast<
												const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);
										callVFunc = builder.callExpr(resType, op, callVFunc,
												builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));

									}
								} else {
									//we DON'T need return adjustment -> thunkResTy is the same as of vFunc
									thunkResTy = vFuncResTy;
								}
							} else {
								// no return adjustment needed -> return Type of the thunk is the same as the return type of the virtual function
								thunkResTy = vFuncResTy;
							}

							retCallVFunc = builder.returnStmt(utils::cast(callVFunc, thunkResTy));
						}

						//create thunkBody: create newThis, expand, callVfunc and if needed add return adjustment
						core::CompoundStmtPtr&& thunkBody = builder.compoundStmt(
								adjustedThisAssign,
								retCallVFunc
						);

						core::ExpressionPtr thunkExpr = builder.lambdaExpr(thunkResTy, thunkBody, thunkParams);

						vFuncExpr = thunkExpr;
					}
				}

				//build functionPointer variable out of expression
				vFuncPointerExpr = builder.refVar(vFuncExpr);

				op = builder.getLangBasic().getRefToAnyRef();
				vFuncPointerExpr = builder.callExpr(builder.getLangBasic().getAnyRef(), op, vFuncPointerExpr);
				VLOG(2)
					<< vFuncPointerExpr;

				VLOG(2)
					<< vFuncPointerExpr;
				ctx.thisStack2 = thisStack2old;
			}

			//assign the functionPointer (as anyRef)
			op = builder.getLangBasic().getRefAssign();
			core::ExpressionPtr vFunctionTableAssign = builder.callExpr(op, vFunctionTable, vFuncPointerExpr);
			VLOG(2)
				<< vFunctionTableAssign;

			initVFuncTableStmts.push_back(vFunctionTableAssign);
		}
	}
	return initVFuncTableStmts;
}

//create/update access vfunc offset table
void CXXConversionFactory::updateVFuncOffsetTableExpr() {
	VLOG(2) << ctx.offsetTableExpr;
	core::StringValuePtr ident = builder.stringValue("__vfunc_offset");
	const core::TypePtr& memberTy =
			( core::analysis::getReferencedType(ctx.globalVar->getType()) ).as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);
	//core::static_pointer_cast<const core::NamedCompositeType>( core::analysis::getReferencedType(ctx.globalVar->getType()) )->getTypeOfMember(ident);
	core::TypePtr resType = builder.refType(memberTy);
	core::ExpressionPtr op = builder.getLangBasic().getCompositeRefElem();
	ctx.offsetTableExpr = builder.callExpr(resType, op, ctx.globalVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));
	VLOG(2) << ctx.offsetTableExpr;
}

//create/update access vfunc table
void CXXConversionFactory::updateVFuncTableExpr() {
	VLOG(2) << ctx.vFuncTableExpr;
	core::StringValuePtr ident = builder.stringValue("__vfunc_table");
	const core::TypePtr& memberTy =
			( core::analysis::getReferencedType(ctx.globalVar->getType()) ).as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);
	//core::static_pointer_cast<const core::NamedCompositeType>( core::analysis::getReferencedType(ctx.globalVar->getType()) )->getTypeOfMember(ident);
	core::TypePtr resType = builder.refType(memberTy);
	core::ExpressionPtr op = builder.getLangBasic().getCompositeRefElem();
	ctx.vFuncTableExpr = builder.callExpr(resType, op, ctx.globalVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));
	VLOG(2) << ctx.vFuncTableExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERT FUNCTION DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::NodePtr CXXConversionFactory::convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool isEntryPoint) {

	//Save the scope objects created in the previous scope
	ConversionFactory::ConversionContext::ScopeObjects parentScopeObjects = ctx.scopeObjects;
	while (!ctx.scopeObjects.empty()) {
		ctx.scopeObjects.pop();
	}

	// the function is pure virtual/abstract
	if (funcDecl->isPure()) {
		//so it has no body -> need to get body from derived class
		assert(false && "Abstract (pure virtual) member function not handled yet");
	}
	VLOG(2)<<funcDecl->getNameAsString();
	// the function is not extern, a lambdaExpr has to be created
	assert(currTU && funcDecl->hasBody() && "Function has no body!");



	VLOG(1)
		<< "~ Converting function: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;

	VLOG(1)
		<< "#----------------------------------------------------------------------------------#";
	VLOG(1)
		<< "\nVisiting Function Declaration for: " << funcDecl->getNameAsString() << std::endl << "-> at location: ("
				<< utils::location(funcDecl->getSourceRange().getBegin(), currTU->getCompiler().getSourceManager())
				<< "): " << std::endl << "\tIsRecSubType: " << ctx.isRecSubFunc << std::endl
				<< "\tisResolvingRecFuncBody: " << ctx.isResolvingRecFuncBody << std::endl << "\tEmpty map: "
				<< ctx.recVarExprMap.size();

	if (!ctx.isRecSubFunc) {
		// add this type to the type graph (if not present)
		exprConv->funcDepGraph.addNode(funcDecl);
		if (VLOG_IS_ON(2)) {
			exprConv->funcDepGraph.print(std::cout);
		}
	}

	// retrieve the strongly connected components for this type
	std::set<const FunctionDecl*>&& components = exprConv->funcDepGraph.getStronglyConnectedComponents( funcDecl );

	// save the current translation unit
	const TranslationUnit* oldTU = currTU;

	if (!components.empty()) {
		std::set<const FunctionDecl*>&& subComponents = exprConv->funcDepGraph.getSubComponents( funcDecl );

		std::for_each(subComponents.begin(), subComponents.end(),
				[&] (const FunctionDecl* cur) {

					FunctionDecl* decl = const_cast<FunctionDecl*>(cur);
					VLOG(2) << "Analyzing FuncDecl as sub component: " << decl->getNameAsString();
					const clang::idx::TranslationUnit* clangTU = this->getTranslationUnitForDefinition(decl);

					if ( clangTU && !isa<CXXConstructorDecl>(decl) ) { // not for constructors
						// update the translation unit
						this->currTU = &Program::getTranslationUnit(clangTU);
						// look up the lambda cache to see if this function has been
						// already converted into an IR lambda expression.
						ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(decl);
						if ( fit == ctx.lambdaExprCache.end() ) {
							// perform the conversion only if this is the first time this
							// function is encountred

							convertFunctionDecl(decl, false);
							ctx.recVarExprMap.clear();
						}
					}
				}
		);
	}

	// reset the translation unit
	currTU = oldTU;

	// we have a c++ method declaration and the special case constructor
	bool isCXX = false;
	bool isCtor = false;
	bool isOverloadedOp = false;
	bool isDtor = false;

	// bool isCXXOperator = false;
	const CXXRecordDecl * baseClassDecl;
	if (const CXXConstructorDecl* cxxCtorDecl =dyn_cast<CXXConstructorDecl>(funcDecl)) {
		baseClassDecl = cxxCtorDecl->getParent();
		VLOG(2)
			<< "Name of the class: " << baseClassDecl->getNameAsString();
		assert(baseClassDecl->getNameAsString()==cxxCtorDecl->getNameAsString() && "wrong constructor");
		isCtor = true;
		isCXX = true;
	} else if (const CXXDestructorDecl* cxxDtorDecl =dyn_cast<CXXDestructorDecl>(funcDecl)) {

		baseClassDecl = cxxDtorDecl->getParent();
		isDtor = true;
		isCXX = true;
	}

	else if (const CXXMethodDecl* cxxMethodDecl = dyn_cast<CXXMethodDecl>(funcDecl)) {
		if (cxxMethodDecl->isInstance()) {
			baseClassDecl = cxxMethodDecl->getParent();
			VLOG(2)
				<< "Name of the class: " << baseClassDecl->getNameAsString();

			isCXX = true;
		}
	}

	// check for overloaded operator "function" (normal function has kind OO_None)
	clang::OverloadedOperatorKind operatorKind = funcDecl->getOverloadedOperator();
	if (operatorKind != OO_None) {
		// isCXXOperator = true;
		isOverloadedOp = true;
	}

	if (!(isCXX || isOverloadedOp)) {
		ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(funcDecl);
		if (fit != ctx.lambdaExprCache.end()) {
			//restore the parent scope objects first
			ctx.scopeObjects = parentScopeObjects;
			return fit->second;
		}
	}

	if (!components.empty()) {
		// we are dealing with a recursive type
		VLOG(1)
			<< "Analyzing FuncDecl: " << funcDecl->getNameAsString() << std::endl
					<< "Number of components in the cycle: " << components.size();
		std::for_each(components.begin(), components.end(), [ ] (std::set<const FunctionDecl*>::value_type c) {
			VLOG(2) << "\t" << c->getNameAsString( ) << "(" << c->param_size() << ")";
		});

		if (!ctx.isRecSubFunc) {
			if (ctx.recVarExprMap.find(funcDecl) == ctx.recVarExprMap.end()) {
				// we create a TypeVar for each type in the mutual dependence
				core::VariablePtr&& var = builder.variable( convertType( GET_TYPE_PTR(funcDecl) ) );
				ctx.recVarExprMap.insert( std::make_pair(funcDecl, var) );
			}
		} else {
			// we expect the var name to be in currVar
			ctx.recVarExprMap.insert( std::make_pair(funcDecl, ctx.currVar) );
		}

		// when a subtype is resolved we expect to already have these variables in the map
		if (!ctx.isRecSubFunc) {
			std::for_each(components.begin(), components.end(),
					[ this ] (std::set<const FunctionDecl*>::value_type fd) {

						if ( this->ctx.recVarExprMap.find(fd) == this->ctx.recVarExprMap.end() ) {
							core::FunctionTypePtr funcType =
							core::static_pointer_cast<const core::FunctionType>( this->convertType(GET_TYPE_PTR(fd)) );
							// In the case the function is receiving the global variables the signature needs to be
							// modified by allowing the global struct to be passed as an argument
					if ( this->ctx.globalFuncMap.find(fd) != this->ctx.globalFuncMap.end() ) {
						funcType = addGlobalsToFunctionType(this->builder, this->ctx.globalStruct.first, funcType);
					}
					core::VariablePtr&& var = this->builder.variable( funcType );
					this->ctx.recVarExprMap.insert( std::make_pair(fd, var ) );
				}
			});
		}
		if (VLOG_IS_ON(2)) {
			VLOG(2)
				<< "MAP: ";
			std::for_each(ctx.recVarExprMap.begin(), ctx.recVarExprMap.end(),
					[] (ConversionContext::RecVarExprMap::value_type c) {
						VLOG(2) << "\t" << c.first->getNameAsString() << "[" << c.first << "]";
					});
		}
	}

	// find the class type
	core::TypePtr classTypePtr;
	if (isCXX) {
		ConversionContext::ClassDeclMap::const_iterator cit = ctx.classDeclMap.find(baseClassDecl);
		if (cit != ctx.classDeclMap.end()) {
			classTypePtr = cit->second;
		}
		assert(classTypePtr && "no class declaration to type pointer mapping");
	}

	// init parameter set
	vector<core::VariablePtr> params;

	/*
	 * before resolving the body we have to set the currGlobalVar accordingly depending if this function will use the
	 * global struct or not
	 */
	core::VariablePtr parentGlobalVar = ctx.globalVar;
	core::ExpressionPtr parentOffsetTableExpr = ctx.offsetTableExpr;
	core::ExpressionPtr parentVFuncTableExpr = ctx.vFuncTableExpr;
	if (!isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		core::VariablePtr&& var = builder.variable( builder.refType(ctx.globalStruct.first) );
		params.push_back( var );
		ctx.globalVar = var;

		// we have polymorphicClasses -> need offset/vFuncTable
		if( !ctx.polymorphicClassMap.empty()) {
			// create/update access to offsetTable
			updateVFuncOffsetTableExpr();
//			core::StringValuePtr ident = builder.stringValue("__vfunc_offset");
//			const core::TypePtr& memberTy =
//			core::static_pointer_cast<const core::NamedCompositeType>( core::analysis::getReferencedType(ctx.globalVar->getType()) )->getTypeOfMember(ident);
//			core::TypePtr resType = builder.refType(memberTy);
//			core::ExpressionPtr op = builder.getLangBasic().getCompositeRefElem();
//			ctx.offsetTableExpr = builder.callExpr(resType, op, ctx.globalVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));

			// create/update access to vFuncTable
			updateVFuncTableExpr();
//			ident = builder.stringValue("__vfunc_table");
//			const core::TypePtr& memberTy2 =
//			core::static_pointer_cast<const core::NamedCompositeType>( core::analysis::getReferencedType(ctx.globalVar->getType()) )->getTypeOfMember(ident);
//			resType = builder.refType(memberTy2);
//			op = builder.getLangBasic().getCompositeRefElem();
//			ctx.vFuncTableExpr = builder.callExpr(resType, op, ctx.globalVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy2));
		}
	}

	std::for_each(funcDecl->param_begin(), funcDecl->param_end(), [ &params, this ] (ParmVarDecl* currParam) {
		params.push_back( core::static_pointer_cast<const core::Variable>( this->lookUpVariable(currParam) ) );
	});

	// for cpp methods add the type of THIS at the end of the parameter list
	core::ExpressionPtr parentThisVar = ctx.thisVar;
	if (isCXX) {
		core::VariablePtr&& var = builder.variable( builder.refType(classTypePtr) );
		//core::VariablePtr var = ctx.thisStack2;
		params.push_back( var );
		ctx.thisVar = var;
	}

	// this lambda is not yet in the map, we need to create it and add it to the cache
	assert(
			(components.empty() || (!components.empty() && !ctx.isResolvingRecFuncBody)) && "~~~ Something odd happened, you are allowed by all means to blame Simone ~~~");
	if (!components.empty()) {
		ctx.isResolvingRecFuncBody = true;
	}

	VLOG(2)
		<< "Visiting function body!";
	// set up context to contain current list of parameters and convert body
	ConversionContext::ParameterList oldList = ctx.curParameter;
	ctx.curParameter = &params;

	if (VLOG_IS_ON(2)) {
		VLOG(2)
			<< "Dump of stmt body: \n"
					<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		funcDecl->getBody()->dump();
	}

	//Save thisStack2
	core::ExpressionPtr thisStack2old = ctx.thisStack2;

	core::StatementPtr&& body = convertStmt( funcDecl->getBody() );
	//VLOG(2) << "convertFunctionDecl: thisStack2old " << thisStack2old << "thisStack2 " << ctx.thisStack2;
	//reset thisStack2
	ctx.thisStack2 = thisStack2old;

	ctx.curParameter = oldList;

	/*
	 * if any of the parameters of this function has been marked as needRef, we need to add a declaration just before
	 * the body of this function
	 */
	vector<core::StatementPtr> decls;
	std::for_each(params.begin(), params.end(), [ &decls, &body, this ] (core::VariablePtr currParam) {
		auto fit = this->ctx.wrapRefMap.find(currParam);

		if ( fit != this->ctx.wrapRefMap.end() ) {
			// LOG(INFO) << "Replace";
			decls.push_back( this->builder.declarationStmt(fit->second, this->builder.refVar( fit->first ) ));
			/*
			 * replace this parameter in the body, example:
			 *
			 * int f(int a) {
			 *  for (...) {
			 *   x = a; <- if all the occurencies of a will not be replaced the semantics of
			 *   		   the code will not be preserved
			 *   a = i;
			 *  }
			 * }
			 *
			 *  as the variable can olny appear in the RHS of expression, we have to sobstitute it with its
			 *  dereference
			 */
			body = core::static_pointer_cast<const core::Statement>(
					core::transform::replaceAll( this->builder.getNodeManager(), body, fit->first,
							this->tryDeref(fit->second))
			);
		}

	});

	if (isCXX) {
		assert(ctx.thisStack2 && "THIS - thisStack2 is empty");
		body = core::static_pointer_cast<const core::Statement>(
				core::transform::replaceAll(this->builder.getNodeManager(), body, ctx.thisStack2, ctx.thisVar));
	}

	// if we introduce new decls we have to introduce them just before the body of the function
	if (!decls.empty() || isCXX) {

		// update __class if constructor and has polymorphic baseclass
		if (isCtor && baseClassDecl->isPolymorphic()) {
			unsigned int classId = ctx.polymorphicClassMap.find(baseClassDecl)->second.first;

			// update "__class" in all dynamic bases classes to the given classId
			vector<core::StatementPtr> && vec = updateClassId(baseClassDecl, ctx.thisVar, classId);

			// add the declarations before the function body
			for (std::vector<core::StatementPtr>::const_iterator it = vec.begin(); it != vec.end(); it++) {
				decls.push_back(*it);
			}
		}

		// there are constructor initializers that has to be handled - these are inserted befor the body
		// they only need to be considered when we handle a ctor
		if (isCtor && !ctx.ctorInitializerMap.empty()) {
			const core::lang::BasicGenerator& gen = builder.getLangBasic();

			for (std::map<const clang::FieldDecl*, core::ExpressionPtr>::iterator iit = ctx.ctorInitializerMap.begin(),
					iend = ctx.ctorInitializerMap.end(); iit != iend; iit++) {
				const FieldDecl * fieldDecl = (*iit).first;

				core::StringValuePtr ident = builder.stringValue(fieldDecl->getNameAsString());

				const core::TypePtr& memberTy =
						core::static_pointer_cast<const core::NamedCompositeType>(classTypePtr)->getTypeOfMember(ident);

				// create access to the member of the struct/class
				core::ExpressionPtr&& init = builder.callExpr(
						builder.refType( memberTy ),
						gen.getCompositeRefElem(),
						toVector<core::ExpressionPtr>( ctx.thisVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy) )
				);

				// create the assign
				core::StatementPtr assign = builder.callExpr(gen.getUnit(), gen.getRefAssign(), init, (*iit).second);

				core::ExpressionPtr expr = (*iit).second;
				if (isCtor && dynamic_cast<const core::CallExpr*>(&(*expr))) {
					// build new constructor call for a class/struct member inside a class
					core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(expr);
					const core::ExpressionPtr function = call->getFunctionExpr();
					const vector<core::ExpressionPtr> args = call->getArguments();
					vector<core::ExpressionPtr> newArgs;

					unsigned int i = 0;
					// HACK --> Initializers reference wrong globalVar
					// if this initializer needs the globalVar leave out args[0] to get correct var
					if(	ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()
						&& (ctx.globalVar->getType() == args[0]->getType()) ) {
						VLOG(2) << ctx.globalVar << " " << args[0];
						i = 1;	//skip first arg
						newArgs.push_back(ctx.globalVar);	//push correct globalVar
					}
					// HACK END

					for (; i < args.size() - 1; i++) {
						newArgs.push_back(args[i]);
					}
					newArgs.push_back(init);

					core::ExpressionPtr&& newCall = builder.callExpr(
							/*TODO: use refType(memberTy) instead of gen.getUnit() because of changes with destructors*/builder.refType(memberTy),
							function,
							newArgs
					);
					VLOG(2) << newCall << " " << newCall->getType();

					decls.push_back(newCall);
				} else {
					// add normal assignment
					decls.push_back(assign);
				}
			}
			//we added all initializers -> empty initializerMap
			ctx.ctorInitializerMap.clear();
		}

		// push the old body
		decls.push_back(body);
		body = builder.compoundStmt(decls);
	}

	if (!components.empty()) {
		ctx.isResolvingRecFuncBody = false;
	}

	// ADD THE GLOBALS
	if (isEntryPoint && ctx.globalVar) {
		const core::CompoundStmtPtr& compStmt = builder.compoundStmt(body);
		assert(ctx.globalVar && ctx.globalStruct.second);

		const StatementList& oldStmts = compStmt->getStatements();

		std::vector<core::StatementPtr> stmts;

		if (ctx.polymorphicClassMap.empty()) {
			// there were no polymorphic classes found -> only the global variables have to be handled

			stmts = std::vector<core::StatementPtr>(oldStmts.size() + 1);
			stmts[0] = builder.declarationStmt(ctx.globalVar, builder.refNew(ctx.globalStruct.second));
			std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), stmts.begin() + 1);
		} else {
			//init the ctx variables for easier access to OffsetTable and the vfuncTable
			updateVFuncOffsetTableExpr();
			updateVFuncTableExpr();

			// polymorphic classes found: global variables + init virtual function table offset and virtual function table

			//initialize offsetTable
			std::vector<core::StatementPtr>&& initOffsetTableStmts = initOffsetTable();

			// init vFuncTable with the function pointers to the virtual functions
			std::vector<core::StatementPtr>&& initVFuncTableStmts = initVFuncTable();

			stmts = std::vector<core::StatementPtr>(oldStmts.size()+1+initOffsetTableStmts.size()+initVFuncTableStmts.size());

			stmts[0] = builder.declarationStmt(ctx.globalVar, builder.refNew( ctx.globalStruct.second ));
			std::copy(initOffsetTableStmts.begin(), initOffsetTableStmts.end(), stmts.begin()+1);
			std::copy(initVFuncTableStmts.begin(), initVFuncTableStmts.end(), stmts.begin()+1+initOffsetTableStmts.size());
			std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), stmts.begin()+1+initOffsetTableStmts.size()+initVFuncTableStmts.size());
		}
		body = builder.compoundStmt(stmts);
	}

	core::TypePtr convertedType = convertType(GET_TYPE_PTR(funcDecl));
	assert(convertedType->getNodeType() == core::NT_FunctionType && "Converted type has to be a function type!");
	core::FunctionTypePtr funcType = core::static_pointer_cast<const core::FunctionType>(convertedType);

	cxxExprConv->tempHandler.handleTemporariesinScope(funcDecl, funcType, params, ctx.scopeObjects, true, true, false);

	// if this function gets the globals in the capture list we have to create a different type
	if (!isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		funcType = addGlobalsToFunctionType(builder, ctx.globalStruct.first, funcType);
	}

	if (isCXX) {
		funcType = addThisArgToFunctionType(builder, classTypePtr, funcType);
	}

	//if this is a constructor return the objects that is passed to it
	if (isCXX) {
		const core::CompoundStmtPtr& compStmt = builder.compoundStmt(body);
		const StatementList& oldStmts = compStmt->getStatements();
		std::vector<core::StatementPtr> stmts = oldStmts;

		if (isCXX && !isDtor) {

			cxxExprConv->tempHandler.handleTemporariesinScope(params, stmts, ctx.downStreamScopeObjects, false, false);
		}
		if (isCtor) {

			stmts.push_back(builder.returnStmt(utils::cast(ctx.thisVar, ctx.thisVar.getType())));

		}
		body = builder.compoundStmt(stmts);
	}
	// reset old global var, thisVar, and offsetTable
	ctx.globalVar = parentGlobalVar;
	ctx.offsetTableExpr = parentOffsetTableExpr;
	ctx.vFuncTableExpr = parentVFuncTableExpr;
	ctx.thisVar = parentThisVar;
	ctx.scopeObjects = parentScopeObjects;

	VLOG(2)	<< funcType << "\n" << params << "\n" << body;

	if (components.empty()) {

		core::LambdaExprPtr retLambdaExpr;

		if (!isCtor) {

			retLambdaExpr = builder.lambdaExpr(funcType, params, body);

		} else {

			retLambdaExpr = builder.lambdaExpr(params[params.size() - 1].getType(), body, params);
		}

		// attach name annotation to the lambda - also done in attachFuncAnnotations()
		retLambdaExpr->getLambda()->addAnnotation(
				std::make_shared < annotations::c::CNameAnnotation > (funcDecl->getNameAsString()));

		// Adding the lambda function to the list of converted functions
		ctx.lambdaExprCache.insert(std::make_pair(funcDecl, retLambdaExpr));

		VLOG(2)
			<< retLambdaExpr << " + function declaration: " << funcDecl;
		return attachFuncAnnotations(retLambdaExpr, funcDecl);
		//return retLambdaExpr;
	}

	core::LambdaPtr&& retLambdaNode = builder.lambda( funcType, params, body );
	// attach name annotation to the lambda
	retLambdaNode->addAnnotation(std::make_shared < annotations::c::CNameAnnotation > (funcDecl->getNameAsString()));
	// this is a recurive function call
	if (ctx.isRecSubFunc) {
		/*
		 * if we are visiting a nested recursive type it means someone else will take care of building the rectype
		 * node, we just return an intermediate type
		 */
		return retLambdaNode;
	}

	// we have to create a recursive type
	ConversionContext::RecVarExprMap::const_iterator tit = ctx.recVarExprMap.find(funcDecl);
	assert(tit != ctx.recVarExprMap.end() && "Recursive function has not VarExpr associated to himself");
	core::VariablePtr recVarRef = tit->second;

	vector<core::LambdaBindingPtr> definitions;
	definitions.push_back(builder.lambdaBinding(recVarRef, retLambdaNode));

	// We start building the recursive type. In order to avoid loop the visitor
	// we have to change its behaviour and let him returns temporarely types
	// when a sub recursive type is visited.
	ctx.isRecSubFunc = true;

	std::for_each(components.begin(), components.end(),
			[ this, &definitions, &builder, &recVarRef ] (std::set<const FunctionDecl*>::value_type fd) {

				ConversionContext::RecVarExprMap::const_iterator tit = this->ctx.recVarExprMap.find(fd);
				assert(tit != this->ctx.recVarExprMap.end() && "Recursive function has no TypeVar associated");
				this->ctx.currVar = tit->second;

				// test whether function has already been resolved
			if (*tit->second == *recVarRef) {
				return;
			}

			/*
			 * we remove the variable from the list in order to fool the solver, in this way it will create a descriptor
			 * for this type (and he will not return the TypeVar associated with this recursive type). This behaviour
			 * is enabled only when the isRecSubType flag is true
			 */
			this->ctx.recVarExprMap.erase(fd);

			/*
			 * if the function is not defined in this translation unit, maybe it is defined in another we already loaded
			 * use the clang indexer to lookup the definition for this function declarations
			 */
			clang::idx::Entity&& funcEntity =
			clang::idx::Entity::get(const_cast<FunctionDecl*>(fd), this->program.getClangProgram());
			ConversionFactory::TranslationUnitPair&& ret = this->program.getClangIndexer().getDefinitionFor(funcEntity);
			const TranslationUnit* oldTU = this->currTU;
			if ( ret.first ) {
				fd = ret.first;
				assert(ret.second && "Error loading translation unit for function definition");
				this->currTU = &Program::getTranslationUnit(ret.second);
			}

			const core::LambdaPtr& lambda =
			core::static_pointer_cast<const core::Lambda>(this->convertFunctionDecl(fd));
			assert(lambda && "Resolution of sub recursive lambda yields a wrong result");
			this->currTU = oldTU;
			// attach name annotation to the lambda
			lambda->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>( fd->getNameAsString() ) );
			definitions.push_back( builder.lambdaBinding(this->ctx.currVar, lambda) );

			// reinsert the TypeVar in the map in order to solve the other recursive types
			this->ctx.recVarExprMap.insert( std::make_pair(fd, this->ctx.currVar) );
			this->ctx.currVar = NULL;
		});
	// we reset the behavior of the solver
	ctx.isRecSubFunc = false;

	core::LambdaDefinitionPtr&& definition = builder.lambdaDefinition(definitions);
	core::LambdaExprPtr&& retLambdaExpr = builder.lambdaExpr(recVarRef, definition);

	// Adding the lambda function to the list of converted functions
	ctx.lambdaExprCache.insert(std::make_pair(funcDecl, retLambdaExpr));
	// we also need to cache all the other recursive definition, so when we will resolve
	// another function in the recursion we will not repeat the process again
	std::for_each(components.begin(), components.end(),
			[ this, &definition ] (std::set<const FunctionDecl*>::value_type fd) {
				auto fit = this->ctx.recVarExprMap.find(fd);
				assert(fit != this->ctx.recVarExprMap.end());

				FunctionDecl* decl = const_cast<FunctionDecl*>(fd);
				const clang::idx::TranslationUnit* clangTU = this->getTranslationUnitForDefinition(decl);

				assert ( clangTU );
				// save old TU
			const TranslationUnit* oldTU = this->currTU;

			// update the translation unit
			this->currTU = &Program::getTranslationUnit(clangTU);

			core::ExpressionPtr&& func = builder.lambdaExpr(fit->second, definition);
			ctx.lambdaExprCache.insert( std::make_pair(decl, func) );

			func = this->attachFuncAnnotations(func, decl);

			currTU = oldTU;
		});

	VLOG(2)
		<< "Converted Into: " << *retLambdaExpr;

	return attachFuncAnnotations(retLambdaExpr, funcDecl);
}



class CXXConversionFactory::CXXExprConverter : StmtVisitor<CXXExprConverter, core::ExpressionPtr> {
	CXXConversionFactory& convFact;
	CXXConversionFactory::CXXConversionContext& ctx;
	utils::FunctionDependencyGraph funcDepGraph;

public:
	cpp::TemporaryHandler tempHandler;

	CXXExprConverter(CXXConversionFactory& cxxConvFact, Program& program) :
		convFact(cxxConvFact),
		ctx(cxxConvFact.ctx),
		funcDepGraph(program.getClangIndexer(), utils::CXXCallExprVisitor(program.getClangIndexer())),
		tempHandler(&cxxConvFact) {
		}
	virtual ~CXXExprConverter();

private:
	template<class ClangExprTy>
	ExpressionList getFunctionArguments(const core::IRBuilder& builder, ClangExprTy* callExpr,
			const core::FunctionTypePtr& funcTy) {
		ExpressionList args;
		for (size_t argId = 0, end = callExpr->getNumArgs(); argId < end; ++argId) {
			core::ExpressionPtr&& arg = Visit( callExpr->getArg(argId) );
			// core::TypePtr&& argTy = arg->getType();
			if ( argId < funcTy->getParameterTypes().size() ) {
				const core::TypePtr& funcArgTy = funcTy->getParameterTypes()[argId];
				arg = utils::cast(arg, funcArgTy);
			} else {
				arg = utils::cast(arg, builder.getNodeManager().getLangBasic().getVarList());
			}
			args.push_back( arg );
		}
		return args;
	}

	ExpressionList getFunctionArguments(const core::IRBuilder& builder, clang::CXXNewExpr* callExpr,
			const core::FunctionTypePtr& funcTy) {
		ExpressionList args;
		for (size_t argId = 0, end = callExpr->getNumConstructorArgs(); argId < end; ++argId) {
			core::ExpressionPtr&& arg = Visit( callExpr->getConstructorArg(argId) );
			// core::TypePtr&& argTy = arg->getType();
			if ( argId < funcTy->getParameterTypes().size() ) {
				const core::TypePtr& funcArgTy = funcTy->getParameterTypes()[argId];
				arg = utils::cast(arg, funcArgTy);
			} else {
				arg = utils::cast(arg, builder.getNodeManager().getLangBasic().getVarList());
			}
			args.push_back( arg );
		}
		return args;
	}

	ExpressionList getFunctionArguments(const core::IRBuilder& builder, clang::CXXOperatorCallExpr* callExpr,
			const core::FunctionTypePtr& funcTy, bool isMember = false) {
		if (isMember) {
			ExpressionList args;
			// because CXXOperatorCallExpr has as arg0 for operators defined as member functions
			// this == arg(0) == left, arg(1) == right, but funcTy is only for op@( argTy )
			// so skip over arg(0)
			for (size_t argId = 1, end = callExpr->getNumArgs(); argId < end; ++argId) {
				core::ExpressionPtr&& arg = Visit( callExpr->getArg(argId) );
				// core::TypePtr&& argTy = arg->getType();
				if ( argId-1 < funcTy->getParameterTypes().size() ) {
					const core::TypePtr& funcArgTy = funcTy->getParameterTypes()[argId-1];
					arg = utils::cast(arg, funcArgTy);
				} else {
					arg = utils::cast(arg, builder.getNodeManager().getLangBasic().getVarList());
				}
				args.push_back( arg );
			}
			return args;
		} else {
			//use the getFunctionArguments for "normal" CallExpr
			return getFunctionArguments(builder, dynamic_cast<clang::CallExpr*>(callExpr), funcTy);
		}
	}

	//ExpressionList getFunctionArguments(const core::IRBuilder& 		builder,
	//clang::CXXConstructExpr* 	callExpr,
	//const core::FunctionTypePtr& funcTy)
	//{
	//ExpressionList args;
	//for ( size_t argId = 0, end = callExpr->getNumArgs(); argId < end; ++argId ) {
	//core::ExpressionPtr&& arg = Visit( callExpr->getArg(argId) );
	//// core::TypePtr&& argTy = arg->getType();
	//if ( argId < funcTy->getParameterTypes().size() ) {
	//const core::TypePtr& funcArgTy = funcTy->getParameterTypes()[argId];
	//arg = cast(funcArgTy, arg);
	//} else {
	//arg = cast(builder.getNodeManager().getLangBasic().getVarList(), arg);
	//}
	//args.push_back( arg );
	//}
	//return args;
	//}

	// get the classId from the left-most dynamic base of recDecl
	core::ExpressionPtr getClassId(const clang::CXXRecordDecl* recDecl, core::ExpressionPtr thisExpr) {
		const core::IRBuilder& builder = convFact.builder;
		bool hasPolymorphicBaseClass = false;
		core::TypePtr classTypePtr;
		core::ExpressionPtr retExpr;

		VLOG(2) << "GetClassId of class " << recDecl->getNameAsString() << " from var "<< thisExpr;
		ConversionContext::ClassDeclMap::const_iterator cit = ctx.classDeclMap.find(recDecl);
		if(cit != ctx.classDeclMap.end()) {
			classTypePtr = cit->second;
		}
		assert(classTypePtr && "no class declaration to type pointer mapping");

		// check if there are any polymorphic base classes of recDecl
		for(clang::CXXRecordDecl::base_class_const_iterator bit = recDecl->bases_begin(); bit != recDecl->bases_end(); bit++) {
			const CXXBaseSpecifier * base = bit;
			const CXXRecordDecl* baseRecord = base->getType()->getAsCXXRecordDecl();

			hasPolymorphicBaseClass |= baseRecord->isPolymorphic();
		}

		if( recDecl->isPolymorphic() && !hasPolymorphicBaseClass) {
			//access __class
			core::StringValuePtr ident = builder.stringValue("__class");
			const core::TypePtr& memberTy = classTypePtr.as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);

			retExpr = builder.callExpr(
					builder.refType( memberTy ),
					builder.getLangBasic().getCompositeRefElem(),
					toVector<core::ExpressionPtr>( thisExpr, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy) )
			);
		} else {
			//has polymorphic base classes -> take left most (first) polymorphic base class to access __classId
			for(clang::CXXRecordDecl::base_class_const_iterator bit = recDecl->bases_begin(); bit != recDecl->bases_end(); bit++) {
				const CXXBaseSpecifier * base = bit;
				const CXXRecordDecl* baseRecord = base->getType()->getAsCXXRecordDecl();

				if(baseRecord->isPolymorphic()) {
					core::StringValuePtr ident = builder.stringValue(baseRecord->getNameAsString());
					const core::TypePtr& memberTy = classTypePtr.as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);

					//expr = expr->baseRecord
					thisExpr = builder.callExpr(
							builder.refType( memberTy ),
							builder.getLangBasic().getCompositeRefElem(),
							toVector<core::ExpressionPtr>( thisExpr, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy) )
					);

					retExpr = getClassId(baseRecord, thisExpr);
					break;
				}
			}
		}

		return retExpr;
	}

	// takes the recordDecl of this argument of the called function, the methodDecl of the called function,
	// and the "this" object and gets the according functionPointer from the vFuncTable
	// (function Pointer is stored as AnyRef, gets already casted to the correct function type)
	// and is deRef --> ready to use. the resulting ExpressionPtr can be used as Argument to a callExpr
	core::ExpressionPtr createCastedVFuncPointer(
		const clang::CXXRecordDecl* recordDecl,
		const clang::CXXMethodDecl* methodDecl,
		core::ExpressionPtr thisPtr) {
	const core::IRBuilder& builder = convFact.builder;
	core::FunctionTypePtr funcTy = 	( convFact.convertType( GET_TYPE_PTR(methodDecl) ) ).as<core::FunctionTypePtr>();

	// get the classId of the pointer/reference
	unsigned int classIdOfThis = ctx.polymorphicClassMap.find(recordDecl)->second.first;

	// get the classId of the object behind pointer/reference (get access to member __class)
	core::ExpressionPtr classIdExpr = getClassId(recordDecl, thisPtr);

	// deRef the __class access Expr
	classIdExpr = builder.callExpr( builder.getLangBasic().getUInt4(), builder.getLangBasic().getRefDeref(), classIdExpr );

	// get functionId of the called function
	unsigned int functionId = ctx.virtualFunctionIdMap.find(methodDecl)->second;

	if( VLOG_IS_ON(2) ) {
		VLOG(2) << "Virtual Call:						" << methodDecl->getNameAsString() << " via " << recordDecl->getNameAsString();
		VLOG(2) << "	classIdExpr:					" << classIdExpr;
		VLOG(2) << "	classId (of pointer/reference)	" << classIdOfThis;
		VLOG(2) << "	functionId:						" << functionId;
		VLOG(2) << "	offsetTableExpr:				" << ctx.offsetTableExpr;
		VLOG(2) << "	vFuncTableExpr:					" << ctx.vFuncTableExpr;
	}

	// create call to function stored at:
	// vfunctTable[ classId ][ offsetTable[ classId ][ classIdOfThis ] + functionId ](packedArgs)

	core::ExpressionPtr op;
	core::ExpressionPtr vFuncOffset;
	core::ExpressionPtr vtableAccess;
	core::TypePtr offsetTableTy;
	core::TypePtr vFuncTableTy;
	core::TypePtr exprTy;

	//get IR type of offsetTable (ref<vector<vector<uint8, #polymorphicClasses>,#polymorphicClasses>>)
	offsetTableTy = ctx.offsetTableExpr->getType();
	if(offsetTableTy->getNodeType() == core::NT_RefType) {
		exprTy = GET_REF_ELEM_TYPE(offsetTableTy);
		exprTy = GET_VEC_ELEM_TYPE(exprTy);
		exprTy = builder.refType(exprTy);
	}
	op = builder.getLangBasic().getVectorRefElem();
	//get access to row of classId of the actual Object: offsetTable[classId]
	vFuncOffset = builder.callExpr(exprTy, op, ctx.offsetTableExpr, classIdExpr);

	//get IR type of offsetTable (ref<vector<uint8, #polymorphicClasses>>)
	offsetTableTy = vFuncOffset->getType();
	if(offsetTableTy->getNodeType() == core::NT_RefType) {
		exprTy = GET_REF_ELEM_TYPE(offsetTableTy);
		exprTy = GET_VEC_ELEM_TYPE(exprTy);
		exprTy = builder.refType(exprTy);
	}
	op = builder.getLangBasic().getVectorRefElem();
	//get element of row to classId of the reference/pointer of this: offsetTable[classId][classIdOfThis]
	vFuncOffset = builder.callExpr(exprTy, op, vFuncOffset, builder.literal(builder.getLangBasic().getUInt4(), toString(classIdOfThis)));

	//deref vFuncOffset
	if(exprTy ->getNodeType() == core::NT_RefType) {
		//exprTy = core::static_pointer_cast<const core::RefType>(exprTy)->getElementType();
		// TODO: change static_pointer_cast<typePtr>(foo) to ...foo.as<typePtr>
		exprTy = exprTy.as<core::RefTypePtr>()->getElementType();
	}
	vFuncOffset = builder.callExpr( exprTy, builder.getLangBasic().getRefDeref(), vFuncOffset );

	//calculate index in vFuncTable[classId] row: (offset[classId][classIdOfThis]+funcId)
	op = builder.getLangBasic().getOperator(exprTy, core::lang::BasicGenerator::Add);
	core::ExpressionPtr vTableColIdx = builder.callExpr(exprTy, op, vFuncOffset, builder.literal(builder.getLangBasic().getInt4(), toString(functionId)));

	//get acces to the row of vfunctable[classId]
	vFuncTableTy = ctx.vFuncTableExpr->getType();
	//get IR type of vFuncTable (ref<vector<vector<AnyRef, #maxFunctionCount>, #polymorphicClasses>)
	if(vFuncTableTy->getNodeType() == core::NT_RefType) {
		exprTy = GET_REF_ELEM_TYPE(vFuncTableTy);
		exprTy = GET_VEC_ELEM_TYPE(exprTy);
		exprTy = builder.refType(exprTy);
	}
	op = builder.getLangBasic().getVectorRefElem();
	//get access to row of classId of the actual Object: vFuncTable[classId]
	vtableAccess = builder.callExpr(exprTy, op, ctx.vFuncTableExpr, builder.castExpr(builder.getLangBasic().getUInt4(), classIdExpr) );

	vFuncTableTy = vtableAccess->getType();
	//get IR type of vFuncTable (ref<vector<AnyRef, #maxFunctionCount>>)
	if(vFuncTableTy->getNodeType() == core::NT_RefType) {
		exprTy = GET_REF_ELEM_TYPE(vFuncTableTy);
		exprTy = GET_VEC_ELEM_TYPE(exprTy);
		exprTy = builder.refType(exprTy);
	}

	// get access to the functionpointer (void*) vfunctable[ classId ][ vTableColIdx ]
	op = builder.getLangBasic().getVectorRefElem();
	vtableAccess = builder.callExpr(exprTy, op, vtableAccess, builder.castExpr(builder.getLangBasic().getUInt4(), vTableColIdx) );

	// if this function gets the globals in the capture list we have to create a different type
	if ( ctx.globalFuncMap.find(methodDecl) != ctx.globalFuncMap.end() ) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		funcTy = convFact.addGlobalsToFunctionType(builder, ctx.globalStruct.first, funcTy);
	}

	core::TypePtr classType;
	ConversionContext::ClassDeclMap::const_iterator cit = convFact.ctx.classDeclMap.find(recordDecl);
	if(cit != convFact.ctx.classDeclMap.end()) {
		classType = cit->second;
	}
	//add the "this" object as parameter to the virtual function
	funcTy = convFact.addThisArgToFunctionType(builder, classType, funcTy);

	// deRef the AnyRef from the vFuncTable
	vtableAccess = builder.callExpr(builder.getLangBasic().getAnyRef(), builder.getLangBasic().getRefDeref(), vtableAccess);

	//cast void* (anyRef) to funcType
	op = builder.getLangBasic().getAnyRefToRef();
	core::ExpressionPtr castedVFuncPointer = builder.callExpr(builder.refType(funcTy), op, vtableAccess, builder.getTypeLiteral(funcTy));

	// deRef the functionPointer
	castedVFuncPointer = builder.callExpr(funcTy, builder.getLangBasic().getRefDeref(), castedVFuncPointer);

	return castedVFuncPointer;
}

	// takes the given "this" of the CXXMemberCall
	// the callee of the CXXMemberCall
	// and the CXXMethodDecl of the called method
	// returns if a virtual func can be called non-virtual
	bool canDevirtualizeCXXMemberCall(
			const clang::Expr* thisArg,
			const clang::MemberExpr* memberExpr,
			const clang::CXXMethodDecl* methodDecl) {

		//TODO support for "final" keyword needed?

		//check if we have an actual object
		thisArg = thisArg->IgnoreParenImpCasts();
		if (const DeclRefExpr* declExpr = dyn_cast<DeclRefExpr>(thisArg)) {
			if (const VarDecl* varDecl = dyn_cast<VarDecl>(declExpr->getDecl())) {
				// This is a record decl. We know the type and can devirtualize it.
				return varDecl->getType()->isRecordType();
			}
			return false;
		}

		// We can always devirtualize calls on temporary object expressions.
		if (isa<CXXConstructExpr>( memberExpr->getBase() ))
			return true;

		// And calls on bound temporaries.
		if (isa<CXXBindTemporaryExpr>( memberExpr->getBase() ))
			return true;

		// can't devirtualize call
		return false;
	}

public:

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CXX BOOLEAN LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXBoolLiteralExpr(CXXBoolLiteralExpr* boolLit) {
		START_LOG_EXPR_CONVERSION(boolLit);

		core::ExpressionPtr retExpr;
		LOG_CONVERSION(retExpr);

		return (retExpr =
		// retrieve the string representation from the source code
				convFact.builder.literal(
						GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(), boolLit->getExprLoc()),
						convFact.mgr.getLangBasic().getBool()));
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX MEMBER CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* callExpr) {
		START_LOG_EXPR_CONVERSION(callExpr);
		//const core::lang::BasicGenerator& gen = cxxConvFact.builder.getLangBasic();

		// get record decl and store it
		core::TypePtr classType;
		// getRecordDecl() returns the RecordDecl where the method is declared
		ConversionContext::ClassDeclMap::const_iterator cit =
		convFact.ctx.classDeclMap.find(callExpr->getRecordDecl());

		if(cit != convFact.ctx.classDeclMap.end()) {
			classType = cit->second;
		}

		//store previous curTy
		core::TypePtr parentCurTy = convFact.ctx.curTy;
		convFact.ctx.curTy = classType;

		// store previous THIS
		core::ExpressionPtr parentThisStack = convFact.ctx.thisStack2;

		// getting variable of THIS and store it in context
		const clang::Expr* thisArg = callExpr->getImplicitObjectArgument();
		core::ExpressionPtr thisPtr = convFact.convertExpr( thisArg );

		// get type from thisArg or if there are ImpliciCasts get Type from DeclRef
		const clang::Type* thisType = GET_TYPE_PTR(thisArg);

		// there can be several ImplicitCastExpr before a DeclRefExpr (for example with const member func)
		thisArg = thisArg->IgnoreParenImpCasts();

		if( GET_TYPE_PTR(thisArg)->isPointerType() ) {
			thisPtr = getCArrayElemRef(convFact.builder, thisPtr);
		}

		assert(thisArg && "THIS can not be retrieved");

		// THIS can be retrieved by calling the underlying declaration reference
		if( const DeclRefExpr* declExpr = dyn_cast<const DeclRefExpr>(thisArg) ) {
			const VarDecl* definition = dyn_cast<const VarDecl>(declExpr->getDecl());

			assert(definition && "Declaration is of non type VarDecl");

			clang::QualType&& clangType = definition->getType();
			if( !clangType.isCanonical() ) {
				clangType = clangType->getCanonicalTypeInternal();
			}

			// We are accessing a global variable
			if ( definition->hasGlobalStorage() ) {
				throw GlobalVariableDeclarationException();
			}

			// lookup THIS according to its definition
			core::ExpressionPtr parentThisStack = convFact.ctx.thisStack2;

			core::VariablePtr var =
			core::static_pointer_cast<const core::Variable>( convFact.lookUpVariable(definition) );

			convFact.ctx.thisStack2 = var;
			assert(var && "Variable for THIS not set");

			//get clang type of THIS object --> needed for virtual functions
			thisType = GET_TYPE_PTR(definition);
		} else {
			convFact.ctx.thisStack2 = thisPtr;
		}

		core::ExpressionPtr retExpr;
		const core::IRBuilder& builder = convFact.builder;

		const Expr* callee = callExpr->getCallee()->IgnoreParens();
		const MemberExpr* memberExpr = cast<const MemberExpr>(callee);
		const CXXMethodDecl* methodDecl = cast<const CXXMethodDecl>(memberExpr->getMemberDecl());

		assert(methodDecl && "there is no method declaration");

		if (methodDecl->isStatic()) {
			// static method
			assert(false && "Static methods not yet supported!");
		}

		const clang::FunctionDecl* funcDecl = methodDecl;
		core::FunctionTypePtr funcTy =
		core::static_pointer_cast<const core::FunctionType>( convFact.convertType(GET_TYPE_PTR(funcDecl)) );

		// get the arguments of the function
		ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);
		assert(convFact.currTU && "Translation unit not set.");

		// convert the function declaration
		ExpressionList&& packedArgs = tryPack(builder, funcTy, args);

		const FunctionDecl* definition = funcDecl;
		/*
		 * We find a definition, we lookup if this variable needs to access the globals, in that case the capture
		 * list needs to be initialized with the value of global variable in the current scope
		 */
		if ( ctx.globalFuncMap.find(definition) != ctx.globalFuncMap.end() ) {
			/*
			 * we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
			 * current context
			 */
			assert(ctx.globalVar && "No global definitions forwarded to this point");
			packedArgs.insert(packedArgs.begin(), ctx.globalVar);
		}

		assert(convFact.ctx.thisStack2 && "thisStack2 empty!");

		assert(thisPtr && "thisPtr empty");
		packedArgs.push_back(thisPtr);

		// use virtual function table if virtual function is called via pointer or reference
		// and methodcall can't be devirtualized (check for devirtualization is rather simple for now (TODO))
		core::ExpressionPtr lambdaExpr;
		if( methodDecl->isVirtual() && !canDevirtualizeCXXMemberCall(thisArg, memberExpr, methodDecl) ) {

			//use the implicit object argument to determine type
			clang::Expr* thisArg = callExpr->getImplicitObjectArgument();

			clang::CXXRecordDecl* recordDecl;
			if( thisArg->getType()->isPointerType() ) {
				recordDecl = thisArg->getType()->getPointeeType()->getAsCXXRecordDecl();
				VLOG(2) << "Pointer of type " << recordDecl->getNameAsString();
			} else if( thisArg->getType()->isReferenceType() ) {
				recordDecl = thisArg->getType()->getAsCXXRecordDecl();
				VLOG(2) << "Reference of type "<< recordDecl->getNameAsString();
			} else {
				recordDecl = thisArg->getType()->getAsCXXRecordDecl();
				VLOG(2) << "Possibly devirtualizeable CALL -- Object of type "<< recordDecl->getNameAsString();
			}

			// get the deRef'd function pointer for methodDecl accessed via a ptr/ref of recordDecl
			lambdaExpr = createCastedVFuncPointer(recordDecl, methodDecl, thisPtr);
		} else {
			//non-virtual method called or virtual func which can be devirtualized
			//example: virtual func called via object -> normal function call
			//VLOG(2) << "Object of type "<< thisArg->getType()->getAsCXXRecordDecl()->getNameAsString();
			lambdaExpr = core::static_pointer_cast<const core::LambdaExpr>( convFact.convertFunctionDecl(funcDecl) );
		}

		//the final callExpr
		retExpr = convFact.builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs);

		// reset previous CurTy
		convFact.ctx.curTy = parentCurTy;

		// reset previous THIS
		convFact.ctx.thisStack2 = parentThisStack;

		VLOG(2) << "End of expression CXXMemberCallExpr \n";
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX OPERATOR CALL EXPRESSION
	//
	//  A call to an overloaded operator written using operator syntax.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* callExpr) {
		START_LOG_EXPR_CONVERSION(callExpr);

		core::ExpressionPtr retExpr;
		const core::IRBuilder& builder = convFact.builder;
		core::ExpressionPtr lambdaExpr;
		core::FunctionTypePtr funcTy;
		ExpressionList args;
		ExpressionList packedArgs;
		core::ExpressionPtr parentThisStack;

		const FunctionDecl* definition;

		clang::OverloadedOperatorKind operatorKind = callExpr->getOperator();
		VLOG(2) << "operator" << getOperatorSpelling(operatorKind) << " " << operatorKind;

		if( const CXXMethodDecl* methodDecl = dyn_cast<CXXMethodDecl>(callExpr->getCalleeDecl()) ) {

			//operator defined as member function
			VLOG(2) << "Operator defined as member function";
			VLOG(2) << methodDecl->getParent()->getNameAsString() << "::" << methodDecl->getNameAsString() << " isVirtual: " << methodDecl->isVirtual();

			const MemberExpr* memberExpr = dyn_cast<const MemberExpr>(callExpr->getCallee()->IgnoreParens());

			// possible member operators: +,-,*,/,%,^,&,|,~,!,<,>,+=,-=,*=,/=,%=,^=,&=,|=,<<,>>,>>=,<<=,==,!=,<=,>=,&&,||,++,--,','
			// overloaded only as member function: '=', '->', '()', '[]', '->*', 'new', 'new[]', 'delete', 'delete[]'
			//unary:	X::operator@();	left == CallExpr->arg(0) == "this"
			//binary:	X::operator@( right==arg(1) ); left == CallExpr->arg(0) == "this"
			//else functioncall: ():		X::operator@( right==arg(1), args ); left == CallExpr->arg(0) == "this"

			funcTy = core::static_pointer_cast<const core::FunctionType>(convFact.convertType(GET_TYPE_PTR(methodDecl)) );

			// get the arguments of the function (for operators defined as member function
			args = getFunctionArguments(builder, callExpr, funcTy , true);

			// convert the function declaration
			packedArgs = tryPack(builder, funcTy, args);

			// store THIS
			parentThisStack = convFact.ctx.thisStack2;

			VLOG(2) << "funcTy: " << funcTy;
			VLOG(2) << "packedArgs: " << packedArgs;

			//used to determine if global struct is needed as parameter
			definition = methodDecl;

			// get the lhs-this
			convFact.ctx.lhsThis = Visit(callExpr->getArg(0));
			convFact.ctx.thisStack2 = convFact.ctx.lhsThis;
			//cxxConvFact.ctx.rhsThis = Visit(callExpr->getArg(1));

			//add the "this" as arg as we have an operator as a member-function
			packedArgs.push_back(convFact.ctx.lhsThis);

			assert(convFact.ctx.thisStack2);
			convFact.ctx.isCXXOperator=true;

			core::ExpressionPtr thisPtr = convFact.ctx.lhsThis;
			const clang::Expr* thisArg = callExpr->getArg(0);

			// get type from thisArg or if there are ImpliciCasts get Type from DeclRef
			const clang::Type* thisType = GET_TYPE_PTR(thisArg);

			// there can be several ImplicitCastExpr before a DeclRefExpr (for example with const member func)
			thisArg = thisArg->IgnoreImpCasts();

			//determine the type of the thisPointee
			if( const DeclRefExpr* declExpr = dyn_cast<const DeclRefExpr>(thisArg) ) {
				const VarDecl* definition = dyn_cast<const VarDecl>(declExpr->getDecl());

				assert(definition && "Declaration is of non type VarDecl");
				//get clang type of THIS object --> needed for virtual functions
				thisType = GET_TYPE_PTR(definition);
			} else {
				convFact.ctx.thisStack2 = thisPtr;
			}

			//if virtual --> build virtual call
			// and methodcall can't be devirtualized (check for devirtualization is rather simple for now (TODO))
			if( methodDecl->isVirtual() && !canDevirtualizeCXXMemberCall(thisArg, memberExpr, methodDecl) ) {

				clang::CXXRecordDecl* recordDecl;
				if( thisType->isPointerType() ) {
					recordDecl = thisArg->getType()->getPointeeType()->getAsCXXRecordDecl();
					VLOG(2) << "Pointer of type " << recordDecl->getNameAsString();
				} else if( thisType->isReferenceType() ) {
					recordDecl = thisArg->getType()->getAsCXXRecordDecl();
					VLOG(2) << "Reference of type "<< recordDecl->getNameAsString();
				} else {
					recordDecl = thisArg->getType()->getAsCXXRecordDecl();
					VLOG(2) << "Possible devirtualizeable CALL -- Object of type "<< recordDecl->getNameAsString();
				}

				VLOG(2) << recordDecl->getNameAsString() << " " << methodDecl->getParent()->getNameAsString();
				lambdaExpr = createCastedVFuncPointer(recordDecl, methodDecl, thisPtr);
			} else {
				//else --> build normal call
				lambdaExpr = core::static_pointer_cast<const core::LambdaExpr>( convFact.convertFunctionDecl(methodDecl) );
			}

		} else if(const FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getCalleeDecl()) ) {

			//operator defined as non-member function
			VLOG(2) << "Operator defined as non-member function";

			//possible non-member operators:
			//unary:	operator@( left==arg(0) )
			//binary:	operator@( left==arg(0), right==arg(1) )
			funcTy = core::static_pointer_cast<const core::FunctionType>(convFact.convertType(GET_TYPE_PTR(funcDecl)) );

			// get the arguments of the function -- differentiate between member/non-member operator
			args = getFunctionArguments(builder, callExpr, funcTy /*, true*/);

			// convert the function declaration
			packedArgs = tryPack(builder, funcTy, args);

			// store THIS
			parentThisStack = convFact.ctx.thisStack2;

			VLOG(2) << "funcTy: " << funcTy;
			VLOG(2) << "packedArgs: " << packedArgs;

			lambdaExpr = core::static_pointer_cast<const core::LambdaExpr>( convFact.convertFunctionDecl(funcDecl) );

			//used to determine if global struct is needed as parameter
			definition = funcDecl;
		} else {
			assert(false && "CXXOperatorCall - operator not defined as non-member or member function");
		}

		// make a copy of the current scopeObjects stack and empty the stack
		ConversionFactory::ConversionContext::ScopeObjects parentScopeObjects =
		convFact.ctx.scopeObjects;
		while (!convFact.ctx.scopeObjects.empty()) {
			convFact.ctx.scopeObjects.pop();
		}

		std::vector<core::VariablePtr> temporaries =
		tempHandler.retrieveFunctionTemporaries(definition,
				convFact.ctx.fun2TempMap);

		vector<core::VariablePtr>::iterator it;

		//	 We add each temporary to the packed arguments, and the scope objects of the parent
		//The type of each temporary is added to the type of the function
		for (it = temporaries.begin(); it < temporaries.end(); it++) {

			core::VariablePtr var = *it;
			packedArgs.push_back(var);
			parentScopeObjects.push(var);

			funcTy = tempHandler.addThisArgToFunctionType(builder, builder.deref(var).getType(),
					funcTy);

		}

		convFact.ctx.scopeObjects = parentScopeObjects;

		core::TypePtr resultType = funcTy->getReturnType();

		if (resultType->getNodeType() == core::NT_StructType) {
			resultType = convFact.builder.refType(resultType);
		}

		//		clang::FunctionDecl * funcDecl = dyn_cast<clang::FunctionDecl>(callExpr->getCalleeDecl());
		//		core::FunctionTypePtr funcTy =
		//				core::static_pointer_cast<const core::FunctionType>(cxxConvFact.convertType(GET_TYPE_PTR(funcDecl)) );
		//
		//		// get the arguments of the function
		//		ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);
		//
		//		// convert the function declaration
		//		ExpressionList&& packedArgs = tryPack(builder, funcTy, args);
		//
		//		// store THIS
		//		core::ExpressionPtr parentThisStack = cxxConvFact.ctx.thisStack2;
		//
		//		VLOG(2) << "funcTy: " << funcTy;
		//		VLOG(2) << "packedArgs: " << packedArgs;
		//
		//
		//		for (unsigned int i=0; i<callExpr->getNumArgs(); i++){
		//			VLOG(2) << Visit(callExpr->getArg(i));
		//		}
		//
		//		int numOfArgs = callExpr->getNumArgs();
		//		if(numOfArgs == 2) {
		//			cxxConvFact.ctx.lhsThis = Visit(callExpr->getArg(0));
		//			VLOG(2)<<cxxConvFact.ctx.lhsThis << "  " << cxxConvFact.ctx.lhsThis->getType();
		//			cxxConvFact.ctx.thisStack2 = cxxConvFact.ctx.lhsThis;
		//			VLOG(2)<<cxxConvFact.ctx.thisStack2;
		//			if ( dyn_cast<CXXConstructExpr>(callExpr->getArg(1)) ){
		//				// do nothing
		//			} else {
		//				cxxConvFact.ctx.rhsThis = Visit(callExpr->getArg(1));
		//			}
		//			VLOG(2)<<cxxConvFact.ctx.rhsThis << "  " << cxxConvFact.ctx.rhsThis->getType();
		//
		//			// swap the called arguments
		//			core::ExpressionPtr swapTmp = packedArgs[0];
		//			packedArgs[0] = builder.refVar(packedArgs[1]);  // refVar: a gets to &a
		//			packedArgs[1] = swapTmp;
		//		}
		//
		//		assert(cxxConvFact.ctx.thisStack2);
		//		cxxConvFact.ctx.isCXXOperator=true;
		//
		//		lambdaExpr = core::static_pointer_cast<const core::LambdaExpr>( cxxConvFact.convertFunctionDecl(funcDecl) );
		//		if(args.size()<2){
		//			packedArgs.push_back(cxxConvFact.ctx.thisStack2);
		//		}

		/*
		 * We find a definition, we lookup if this variable needs to access the globals, in that case the capture
		 * list needs to be initialized with the value of global variable in the current scope
		 */
		if ( ctx.globalFuncMap.find(definition) != ctx.globalFuncMap.end() ) {
			/*
			 * we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
			 * current context
			 */
			assert(ctx.globalVar && "No global definitions forwarded to this point");
			packedArgs.insert(packedArgs.begin(), ctx.globalVar);
		}

		VLOG(2) << "funcTy: " << funcTy;
		VLOG(2) << "packedArgs: " << packedArgs;

		retExpr = convFact.builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs);

		// reset to parent THIS
		convFact.ctx.thisStack2 = parentThisStack;

		convFact.ctx.isCXXOperator=false;
		convFact.ctx.lhsThis = 0;
		convFact.ctx.rhsThis = 0;

		//assert(false && "CXXOperatorCallExpr not yet handled");
		VLOG(2) << "End of expression CXXOperatorCallExpr \n";
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX CONSTRUCTOR CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXConstructExpr(clang::CXXConstructExpr* callExpr) {

		START_LOG_EXPR_CONVERSION(callExpr);
		const core::IRBuilder& builder = convFact.builder;

		// We get a pointer to the object that is constructed and we store the pointer to tv8he scope objects stack
		//that holds the objects that are constructed in the current scope
		core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(convFact.ctx.thisStack2);
		CXXRecordDecl* classDecl = 0;

		if(callExpr->getType()->getAs<RecordType>()){
			classDecl = cast<CXXRecordDecl>(callExpr->getType()->getAs<RecordType>()->getDecl());
		}

		if(classDecl){
			if (classDecl->getDestructor()) {
					convFact.ctx.scopeObjects.push(var);
					convFact.ctx.objectMap.insert(std::make_pair(var,classDecl));
			}
		}

		//	const core::lang::BasicGenerator& gen = builder.getLangBasic();
		core::ExpressionPtr retExpr;
		CXXConstructorDecl* constructorDecl = dyn_cast<CXXConstructorDecl>(callExpr->getConstructor());

		bool isArrayType = false;
		unsigned int arraySize;
		const clang::Type* arrayType;
		const clang::Type* arrayElemType;

		//code for handling object array creation
		const Type* constructedType = callExpr->getType().getTypePtr();
		isArrayType = constructedType->isArrayType();

		if( isArrayType ) {
			//if(const clang::ConstantArrayType* cat = dyn_cast<const clang::ConstantArrayType*>(classDecl->getAsArrayTypeUnsafe()) ) {
			if(isa<clang::ConstantArrayType>(constructedType) ) {
				//const clang::ConstantArrayType* cat = cast<const clang::ConstantArrayType>(classDecl);
				const clang::ConstantArrayType* cat = convFact.currTU->getCompiler().getASTContext().getAsConstantArrayType(callExpr->getType());
				arraySize = convFact.currTU->getCompiler().getASTContext().getConstantArrayElementCount(cat);
				arrayType = constructedType;
				arrayElemType = cat->getElementType().getTypePtr();
				VLOG(2) << "ConstantArrayType size: " << arraySize << " type: " << arrayElemType->getAsCXXRecordDecl()->getNameAsString();
			} else if(isa<clang::DependentSizedArrayType>(constructedType) ) {
				VLOG(2) << "DependentSizedArrayType";
				assert(false && "DependentSizedArrayType - not supported");
			} else if(isa<clang::IncompleteArrayType>(constructedType) ) {
				VLOG(2) << "IncompleteArrayType";
				assert(false && "IncompleteArrayType - not supported");
			} else if(isa<clang::VariableArrayType>(constructedType) ) {
				VLOG(2) << "VariableArrayType";
				assert(false && "VariableArrayType - not supported");
			}
		}

		//	cxxConvFact.ctx.objectMap.insert(std::make_pair(var,constructorDecl->getParent()));

		assert(constructorDecl);

		FunctionDecl* funcDecl = constructorDecl;
		core::FunctionTypePtr funcTy =
				core::static_pointer_cast<const core::FunctionType>(convFact.convertType(GET_TYPE_PTR(funcDecl)));

		// collects the type of each argument of the expression
		ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);

		// convert the function declaration and add THIS as last parameter
		ExpressionList&& packedArgs = tryPack(builder, funcTy, args);

		//parameter for ctorForLoop lambdaExpr
		vector<core::VariablePtr> params;
		core::VariablePtr isArrayTempGlobalVar = 0;
		core::VariablePtr parentGlobalVar = ctx.globalVar;
		const FunctionDecl* definition = funcDecl;
		/*
		 * We find a definition, we lookup if this variable needs to access the globals, in that case the capture
		 * list needs to be initialized with the value of global variable in the current scope
		 */
		if ( ctx.globalFuncMap.find(definition) != ctx.globalFuncMap.end() ) {
			/*
			 * we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
			 * current context
			 */
			assert(ctx.globalVar && "No global definitions forwarded to this point");
			packedArgs.insert(packedArgs.begin(), ctx.globalVar);

			isArrayTempGlobalVar = builder.variable(ctx.globalVar->getType());
			params.insert(params.begin(), isArrayTempGlobalVar);
		}

		assert( convFact.currTU && "Translation unit not set.");

		ConversionContext::CtorInitializerMap parentCtorInitializerMap = convFact.ctx.ctorInitializerMap;
		convFact.ctx.ctorInitializerMap.clear();

		// preserve THIS
		core::ExpressionPtr parentThisStack = convFact.ctx.thisStack2;

		if(isArrayType) {
			ctx.thisStack2 = builder.variable(convFact.convertType(arrayElemType));
			ctx.globalVar = isArrayTempGlobalVar;
			//packedArgs.push_back(cxxConvFact.ctx.thisStack2);
		} else {
			packedArgs.push_back(parentThisStack);
		}
		VLOG(2)<<parentThisStack;

		// handle initializers
		for (clang::CXXConstructorDecl::init_iterator iit =
				constructorDecl->init_begin(), iend =
				constructorDecl->init_end(); iit != iend; iit++) {
			clang::CXXCtorInitializer * initializer = *iit;

			if (initializer->isMemberInitializer()) {
				FieldDecl *fieldDecl = initializer->getMember();

				VLOG(2) << initializer << " -> " << fieldDecl->getNameAsString() << " = " << Visit(initializer->getInit());
				convFact.ctx.ctorInitializerMap.insert( std::make_pair(fieldDecl, Visit(initializer->getInit())));
			}
		}

		ConversionFactory::ConversionContext::ScopeObjects downStreamSScopeObjectsCopy =
		convFact.ctx.downStreamScopeObjects;

		while (!downStreamSScopeObjectsCopy.empty()) {
			core::VariablePtr downstreamVar =
			downStreamSScopeObjectsCopy.top();
			downStreamSScopeObjectsCopy.pop();
			const ValueDecl* varDecl = tempHandler.getVariableDeclaration(
					downstreamVar, convFact.ctx.varDeclMap);
			if (!GET_TYPE_PTR(varDecl)->isReferenceType()) {
				VLOG(2)<<downstreamVar;
				packedArgs.push_back(downstreamVar);
			}
		}
		VLOG(2) << "pushed" ;

		core::ExpressionPtr ctorExpr = core::static_pointer_cast<const core::LambdaExpr>(convFact.convertFunctionDecl(funcDecl));

		convFact.ctx.thisStack2 = parentThisStack;
		convFact.ctx.ctorInitializerMap = parentCtorInitializerMap;
		ctx.globalVar = parentGlobalVar;
		VLOG(2)<<parentThisStack;

		if(isArrayType) {
			// if we create an array of objects we can use only the default Ctor
			// without any arguments!
			core::TypePtr arrElemTypePtr = convFact.convertType(arrayElemType);
			core::TypePtr arrTypePtr = convFact.convertType(arrayType);

			//create undefined vector for object array
			core::ExpressionPtr newArr = builder.refVar(
					builder.callExpr(
							arrTypePtr,
							builder.getLangBasic().getUndefined(),
							builder.getTypeLiteral(arrTypePtr)
					)
				);

			packedArgs.push_back(newArr);

			// internal var for ctorForLoop lambdaExpr
			core::VariablePtr tempArr = builder.variable(builder.refType(arrTypePtr));
			params.push_back( tempArr );

			// variable to iterate over vector
			core::VariablePtr itVar = builder.variable(builder.getLangBasic().getUInt4());

			// access to element at position itVar -- newArr[itVar]
			core::ExpressionPtr elem = builder.callExpr(builder.getLangBasic().getVectorRefElem(), tempArr, itVar);

			// if we create an array of objects we can use only the default Ctor
			// without any arguments!
			// call ctorExpr with elem as argument
			core::ExpressionPtr ctorCall;
			if(isArrayTempGlobalVar) {
				ctorCall = builder.callExpr(ctorExpr, isArrayTempGlobalVar, elem);
			} else {
				ctorCall = builder.callExpr(ctorExpr, elem);
			}

			// loop over all elements of the newly created vector
			core::ForStmtPtr ctorLoop = builder.forStmt(
					itVar,
					builder.literal(builder.getLangBasic().getUInt4(), toString(0)),
					builder.literal(builder.getLangBasic().getUInt4(), toString(arraySize)),
					builder.literal(builder.getLangBasic().getUInt4(), toString(1)),
					ctorCall
			);

			core::CompoundStmtPtr body = builder.compoundStmt(
					ctorLoop,
					builder.returnStmt(tempArr)
			);

			core::LambdaExprPtr ctorForLoop =
					builder.lambdaExpr(
							builder.refType(arrTypePtr),
							body,
							params
					);

			//final call for the construction of an object array
			retExpr =  builder.callExpr(builder.refType(arrTypePtr), ctorForLoop, packedArgs);

		} else {
			//the constructor returns the object that we pass to it
			retExpr = builder.callExpr(	parentThisStack.getType(),
										ctorExpr,
										packedArgs);
		}

		END_LOG_EXPR_CONVERSION(retExpr);

		VLOG(2) << "End of CXXConstructExpr \n";
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX NEW CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXNewExpr(clang::CXXNewExpr* callExpr) {
		START_LOG_EXPR_CONVERSION(callExpr);

		const core::IRBuilder& builder = convFact.getIRBuilder();
		const core::lang::BasicGenerator& gen = builder.getLangBasic();
		bool isBuiltinType = callExpr->getAllocatedType().getTypePtr()->isBuiltinType();
		bool isArray = callExpr->isArray();

		core::ExpressionPtr retExpr;
		core::TypePtr type;
		FunctionDecl* funcDecl;
		CXXConstructorDecl* constructorDecl;
		CXXRecordDecl * baseClassDecl;
		core::FunctionTypePtr funcTy;

		if(isBuiltinType) {
			type = convFact.convertType(callExpr->getAllocatedType().getTypePtr());
		} else {
			constructorDecl = callExpr->getConstructor();
			assert(constructorDecl);

			funcDecl = constructorDecl;

			//TODO: remove -> problem with globalVar if cached Expr is used as the call to the CTor is called with wrong globalVar
			// find the function in cache
	//			ConversionContext::LambdaExprMap::const_iterator fit = cxxConvFact.ctx.lambdaExprCacheNewObject.find( funcDecl );
	//			if ( fit != cxxConvFact.ctx.lambdaExprCacheNewObject.end() ) {
	//				VLOG(2) << "Already cached";
	//				return fit->second;
	//			}

			funcTy =
			core::static_pointer_cast<const core::FunctionType>( convFact.convertType( GET_TYPE_PTR(funcDecl) ) );

			// class to generate
			baseClassDecl = constructorDecl->getParent();
			type = convFact.convertType(baseClassDecl->getTypeForDecl());
		}
		assert(type && "need type for object to be created");

		// build the malloc
		const core::RefTypePtr& refType = builder.refType(builder.arrayType(type));
		const core::ArrayTypePtr& arrayType = core::static_pointer_cast<const core::ArrayType>(refType->getElementType());
		const core::TypePtr& elemType = arrayType->getElementType();
		core::ExpressionPtr malloced;

		if(isArray) {
			core::ExpressionPtr&& arrSizeExpr = convFact.convertExpr( callExpr->getArraySize() );

			//TODO: need probaly pointer artihmetics...
			// if struct/class type with non-trivial destructors we need to store size of
			// array somewhere to support delete[] (and the call dtor per element)
			if(!isBuiltinType && !baseClassDecl->hasTrivialDestructor() ) {
				//malloc t=tuple(int<4>, ref<array<elementType, 1>>)
				vector<core::TypePtr> t;
				t.push_back( gen.getUInt4() );
				t.push_back( builder.refType( arrayType ) );

				//init for tuple(arraySize, newArray[arraySize])
				ExpressionList e;
				e.push_back(utils::cast(arrSizeExpr, gen.getUInt4()));
				e.push_back(
					builder.refNew(
							builder.callExpr(
									arrayType, gen.getArrayCreate1D(),
									builder.getTypeLiteral(elemType),
									utils::cast(arrSizeExpr, gen.getUInt4())
							)
						)
				);

				//return the alloced array
				malloced = builder.callExpr(
					gen.getTupleRefElem(),
					builder.refNew( builder.tupleExpr(e) ),
					builder.literal("1", gen.getUInt4()),
					builder.getTypeLiteral( builder.refType(arrayType) )
				);
			} else {
				malloced = builder.refNew(
					builder.callExpr( arrayType, gen.getArrayCreate1D(),
							builder.getTypeLiteral(elemType),
							utils::cast(arrSizeExpr, gen.getUInt4())
					)
				);
			}
		} else {
			malloced = builder.refNew(
				builder.callExpr( arrayType, gen.getArrayCreate1D(),
						builder.getTypeLiteral(elemType),
						builder.literal("1", gen.getUInt4())
				)
			);
		}

		malloced = utils::cast(malloced, refType);

		// create new Variable
		core::VariablePtr&& var = builder.variable( refType );
		core::StatementPtr assign = builder.declarationStmt(var, malloced);
		VLOG(2)<< var << " with assignment " << assign;

		// preserve THIS
		core::ExpressionPtr parentThisStack = convFact.ctx.thisStack2;
		convFact.ctx.thisStack2 = var;

		if(isBuiltinType) {
			// build new Function
			core::CompoundStmtPtr&& body = builder.compoundStmt(
					assign,
					builder.returnStmt(var)
			);
			retExpr = builder.createCallExprFromBody(body, refType);
		} else {
			// convert the constructor
			ExpressionList args = getFunctionArguments(convFact.builder, callExpr, funcTy);

			// convert the function declaration and add THIS as last parameter
			ExpressionList packedArgs = tryPack(builder, funcTy, args);

			const FunctionDecl* definition = funcDecl;
			/*
			 * We find a definition, we lookup if this variable needs to access the globals, in that case the capture
			 * list needs to be initialized with the value of global variable in the current scope
			 */
			if ( ctx.globalFuncMap.find(definition) != ctx.globalFuncMap.end() ) {
				/*
				 * we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
				 * current context
				 */
				assert(ctx.globalVar && "No global definitions forwarded to this point");
				packedArgs.insert(packedArgs.begin(), ctx.globalVar);
			}

			ConversionContext::CtorInitializerMap parentCtorInitializerMap = convFact.ctx.ctorInitializerMap;
			convFact.ctx.ctorInitializerMap.clear();

			// handle initializers
			for (clang::CXXConstructorDecl::init_iterator iit = constructorDecl->init_begin(),
					iend = constructorDecl->init_end(); iit!=iend; iit++) {
				clang::CXXCtorInitializer * initializer = *iit;

				if(initializer->isMemberInitializer()) {
					FieldDecl *fieldDecl = initializer->getMember();
					RecordDecl *recordDecl = fieldDecl->getParent();

					core::TypePtr recordTypePtr;
					recordTypePtr = convFact.convertType(recordDecl->getTypeForDecl());

					core::ExpressionPtr initExpr = Visit(initializer->getInit());
					VLOG(2) << initializer << " -> " << fieldDecl->getNameAsString() << " = "<< initExpr;
					convFact.ctx.ctorInitializerMap.insert( std::make_pair(fieldDecl, initExpr) );
				}
			}

			core::ExpressionPtr ctorExpr = core::static_pointer_cast<const core::LambdaExpr>( convFact.convertFunctionDecl(funcDecl) );

			convFact.ctx.thisStack2 = parentThisStack;
			convFact.ctx.ctorInitializerMap = parentCtorInitializerMap;

			if(isArray) {
				// variable to iterate over array
				core::VariablePtr itVar = builder.variable(builder.getLangBasic().getUInt4());

				// thisPtr is pointing to elements of the array
				core::ExpressionPtr&& thisPtr = builder.callExpr(
						builder.refType(type),
						gen.getArrayRefElem1D(),
						var,
						itVar
				);

				packedArgs.push_back( thisPtr );

				// build the ctor Call
				core::ExpressionPtr ctorCall = builder.callExpr(
						builder.refType(type),
						ctorExpr,
						packedArgs
					);
				core::ExpressionPtr&& arrSizeExpr = convFact.convertExpr( callExpr->getArraySize() );

				// loop over all elements of the newly created vector
				core::ForStmtPtr ctorLoop = builder.forStmt(
					itVar,
					builder.literal(gen.getUInt4(), toString(0)),
					utils::cast(arrSizeExpr, gen.getUInt4()),
					builder.literal(gen.getUInt4(), toString(1)),
					ctorCall
				);

				// build new Function
				core::CompoundStmtPtr&& body = builder.compoundStmt(
						assign,
						ctorLoop,
						builder.returnStmt(var)
				);

				retExpr = builder.createCallExprFromBody(body, refType);
			} else {
				// prepare THIS to match the constructor call
				core::ExpressionPtr&& thisPtr = builder.callExpr(
						builder.refType(type),
						gen.getArrayRefElem1D(),
						var,
						builder.literal("0", gen.getUInt4())
					);

				packedArgs.push_back( thisPtr );

				//the IR ctorExpr returns a object of the class in baseClassDecl (irType == type)
				ctorExpr = builder.callExpr(builder.refType(type), ctorExpr, packedArgs);

				// build new Function
				core::CompoundStmtPtr&& body = builder.compoundStmt(
						assign,
						ctorExpr,
						builder.returnStmt(var)
				);

				retExpr = builder.createCallExprFromBody(body, refType);
			}
			//TODO: remove -> problem with globalVar if cached Expr is used as the call to the CTor is called with wrong globalVar
			//cxxConvFact.ctx.lambdaExprCacheNewObject.insert( std::make_pair(funcDecl, retExpr) );
		}

		VLOG(2) << "End of expression CXXNewExpr \n";
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX DELETE CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXDeleteExpr(clang::CXXDeleteExpr* deleteExpr) {
		START_LOG_EXPR_CONVERSION(deleteExpr);

		core::ExpressionPtr retExpr;
		const core::IRBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = builder.getLangBasic();

		//check if argument is class/struct (with non-trivial dtor), otherwise just call "free" for builtin types
		if(deleteExpr->getDestroyedType().getTypePtr()->isStructureOrClassType()
				&& !deleteExpr->getDestroyedType()->getAsCXXRecordDecl()->hasTrivialDestructor() ) {
			/* the call of the dtor and the "free" of the destroyed object is done in an
			 * lambdaExpr so we have to pass the object we destroy and if we have a virtual dtor
			 * also the globalVar to the lambdaExpr
			 */

			core::ExpressionPtr delOpIr;
			core::ExpressionPtr dtorIr;
			core::ExpressionPtr parentThisStack = convFact.ctx.thisStack2;

			const FunctionDecl* operatorDeleteDecl = deleteExpr->getOperatorDelete();

			//get the destructor decl
			const CXXRecordDecl* classDecl = deleteExpr->getDestroyedType()->getAsCXXRecordDecl();
			const CXXDestructorDecl* dtorDecl = classDecl->getDestructor();

			//use the implicit object argument to determine type
			clang::Expr* thisArg = deleteExpr->getArgument()->IgnoreParenImpCasts();

			// delete gets only pointertypes
			const clang::CXXRecordDecl* recordDecl = thisArg->getType()->getPointeeType()->getAsCXXRecordDecl();
			VLOG(2) << "Pointer of type " << recordDecl->getNameAsString();

			bool isArray = deleteExpr->isArrayForm();
			bool isVirtualDtor = dtorDecl->isVirtual();
			bool isDtorUsingGlobals = false;
			//check if dtor uses globals
			if ( ctx.globalFuncMap.find(dtorDecl) != ctx.globalFuncMap.end() ) {
				isDtorUsingGlobals=true;
			}

			// new variable for the object to be destroied, inside the lambdaExpr
			core::TypePtr classTypePtr = convFact.convertType( deleteExpr->getDestroyedType().getTypePtr() );

			core::VariablePtr&& var = builder.variable( builder.refType( builder.refType( builder.arrayType( classTypePtr ))));
			convFact.ctx.thisStack2 = var;

			// for virtual dtor's globalVar, offsetTable and vfuncTable need to be updated
			const core::VariablePtr parentGlobalVar = ctx.globalVar;
			const core::ExpressionPtr parentOffsetTableExpr = ctx.offsetTableExpr;
			const core::ExpressionPtr parentVFuncTableExpr = ctx.vFuncTableExpr;

			if( isVirtualDtor || isDtorUsingGlobals ) {
				//"new" globalVar for arguments
				ctx.globalVar = builder.variable( ctx.globalVar->getType());
			}

			if( isVirtualDtor ) {
				// create/update access to offsetTable
				convFact.updateVFuncOffsetTableExpr();

				// create/update access to vFuncTable
				convFact.updateVFuncTableExpr();
			}

			core::CompoundStmtPtr body;
			core::StatementPtr tupleVarAssign;	//only for delete[]
			core::VariablePtr tupleVar;			//only for delete[]
			core::VariablePtr itVar;			//only for delete[]
			core::ExpressionPtr thisPtr;
			if(isArray) {
				VLOG(2) << classDecl->getNameAsString() << " " << "has trivial Destructor " << classDecl->hasTrivialDestructor();

				//adjust the given pointer
				core::datapath::DataPathBuilder dpManager(convFact.mgr);
				dpManager.element(1);

				// the adjust pointer to free the correct memory -> arg-1
				vector<core::TypePtr> tupleTy;
				tupleTy.push_back( gen.getUInt4() );
				tupleTy.push_back( builder.refType( builder.arrayType( classTypePtr ) ) );

				tupleVar =	builder.variable( builder.refType( builder.tupleType(tupleTy) ) );

				//(ref<'a>, datapath, type<'b>) -> ref<'b>
				tupleVarAssign = builder.declarationStmt(
					tupleVar,
					builder.callExpr(
						builder.refType( builder.tupleType(tupleTy) ),
						builder.getLangBasic().getRefExpand(),
						toVector<core::ExpressionPtr>(var, dpManager.getPath(), builder.getTypeLiteral( builder.tupleType(tupleTy) ) )
					)
				);

				// variable to iterate over array
				itVar = builder.variable(builder.getLangBasic().getUInt4());

				// thisPtr is pointing to elements of the array
				thisPtr = builder.callExpr(
						builder.refType(classTypePtr),
						gen.getArrayRefElem1D(),
						builder.deref(
							builder.callExpr(
									gen.getTupleRefElem(),
									tupleVar,
									builder.literal("1", gen.getUInt4()),
									builder.getTypeLiteral(builder.refType(builder.arrayType( classTypePtr )))
							)
						),
						itVar
					);
			} else {
				thisPtr = getCArrayElemRef(convFact.builder, builder.deref(var) );
			}

			if( isVirtualDtor ) {
				// get the deRef'd function pointer for methodDecl accessed via a ptr/ref of recordDecl
				dtorIr = createCastedVFuncPointer(recordDecl, dtorDecl, thisPtr );
			} else {
				dtorIr = core::static_pointer_cast<const core::LambdaExpr>( convFact.convertFunctionDecl(dtorDecl) );
			}

			//TODO: Dtor has no arguments... (except the "this", and globals, which are added by us)
			core::FunctionTypePtr funcTy =
				core::static_pointer_cast<const core::FunctionType>( convFact.convertType( GET_TYPE_PTR(dtorDecl) ) );
			ExpressionList args;
			ExpressionList packedArgs = tryPack(builder, funcTy, args);

			if( isDtorUsingGlobals ) {
				packedArgs.insert(packedArgs.begin(), ctx.globalVar);
			}
			packedArgs.push_back(thisPtr);

			// build the dtor Call
			core::ExpressionPtr&& dtorCall = builder.callExpr(
					gen.getUnit(),
					dtorIr,
					//thisPtr
					packedArgs
				);

			//create delete call
			if( operatorDeleteDecl ->hasBody() ) {
				//if we have an overloaded delete operator
				//				delOpIr = core::static_pointer_cast<const core::LambdaExpr>( cxxConvFact.convertFunctionDecl(funcDecl) );
				//TODO: add support for overloaded delete operator
				assert(false && "Overloaded delete operator not supported at the moment");
			} else {
				if( isArray ) {
					//call delOp on the tupleVar
					delOpIr = builder.callExpr(
						builder.getLangBasic().getRefDelete(),
						getCArrayElemRef(builder, tupleVar)
					);
				} else {
					//call delOp on the object
					delOpIr = builder.callExpr(
							builder.getLangBasic().getRefDelete(),
							getCArrayElemRef(builder, builder.deref(var))
						);
				}
			}

			if(isArray) {
				// read arraysize from extra element for delete[]
				core::ExpressionPtr&& arraySize =
					builder.callExpr(
						gen.getUInt4(),
						gen.getTupleMemberAccess(),
						builder.deref( tupleVar ),
						builder.literal("0", gen.getUInt4()),
						builder.getTypeLiteral(gen.getUInt4())
					);

				// loop over all elements of array and call dtor
				core::ForStmtPtr dtorLoop = builder.forStmt(
					itVar,
					builder.literal(gen.getUInt4(), toString(0)),
					arraySize,
					builder.literal(gen.getUInt4(), toString(1)),
					dtorCall
				);

				body = builder.compoundStmt(
						tupleVarAssign,
						dtorLoop,
						delOpIr
					);

			} else {
				//add destructor call of class/struct before free-call
				body = builder.compoundStmt(
						dtorCall,
						delOpIr
					);
			}

			vector<core::VariablePtr> params;
			params.push_back(var);

			//we need access to globalVar -> add globalVar to the parameters
			if( isVirtualDtor || isDtorUsingGlobals ) {
				params.insert(params.begin(), ctx.globalVar);
			}

			core::LambdaExprPtr&& lambdaExpr = builder.lambdaExpr( body, params);

			//thisPtr - argument to be deleted
			core::ExpressionPtr argToDelete = convFact.convertExpr( deleteExpr->getArgument() );
			if( isVirtualDtor || isDtorUsingGlobals ) {
				ctx.globalVar = parentGlobalVar;
				ctx.offsetTableExpr = parentOffsetTableExpr;
				ctx.vFuncTableExpr = parentVFuncTableExpr;
				retExpr = builder.callExpr(lambdaExpr, ctx.globalVar, argToDelete);
			} else {
				retExpr = builder.callExpr(lambdaExpr, argToDelete);
			}

			convFact.ctx.thisStack2 = parentThisStack;
		} else {
			// build the free statement with the correct variable
			retExpr = builder.callExpr(
					builder.getLangBasic().getRefDelete(),
					builder.deref( Visit(deleteExpr->getArgument()) )
			);
		}

		VLOG(2) << "End of expression CXXDeleteExpr \n";
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX THIS CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXThisExpr(clang::CXXThisExpr* callExpr) {
		START_LOG_EXPR_CONVERSION(callExpr);

	//		VLOG(2) << "thisStack2: " << cxxConvFact.ctx.thisStack2;
	//		VLOG(2) << "thisVar: " << cxxConvFact.ctx.thisVar;

		//Need thisVar not Stack2 //assert(cxxConvFact.ctx.thisStack2 && "THIS is empty");
		assert(convFact.ctx.thisVar && "THIS is empty");

		VLOG(2) << "CXXThisExpr: \n";
		if( VLOG_IS_ON(2) ) {
			callExpr->dump();
		}

		VLOG(2) << "End of expression CXXThisExpr \n";
		//Need thisVar not Stack2 //return cxxConvFact.ctx.thisStack2;
		return convFact.ctx.thisVar;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					EXCEPTION CXX THROW EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXThrowExpr(clang::CXXThrowExpr* throwExpr) {
		START_LOG_EXPR_CONVERSION(throwExpr);
		assert(false && "VisitCXXThrowExpr not yet handled");
		VLOG(2) << "End of expression\n";
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX DEFAULT ARG EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXDefaultArgExpr(clang::CXXDefaultArgExpr* defaultArgExpr) {
		assert(convFact.currTU && "Translation unit not correctly set");
		VLOG(1) << "\n****************************************************************************************\n"
		<< "Converting expression [class: '" << defaultArgExpr->getStmtClassName() << "']\n"
		<< "-> at location: (" <<
		utils::location(defaultArgExpr->getUsedLocation(), convFact.currTU->getCompiler().getSourceManager()) << "): ";
		if( VLOG_IS_ON(2) ) {
			VLOG(2) << "Dump of clang expression: \n"
			<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
			defaultArgExpr->dump();
		}
		assert(defaultArgExpr->getExpr() && "no default value");
		VLOG(2) << "Default value: " << Visit(defaultArgExpr->getExpr());
		VLOG(2) << "End of expression CXXDefaultArgExpr\n";

		return Visit(defaultArgExpr->getExpr());
	}

	core::ExpressionPtr VisitCXXBindTemporaryExpr(
			clang::CXXBindTemporaryExpr* bindTempExpr) {

		core::IRBuilder& builder =
		const_cast<core::IRBuilder&>(convFact.builder);

		core::ExpressionPtr retExpr;

		core::ExpressionPtr parentThisStack = convFact.ctx.thisStack2;

		const Type* classDecl = bindTempExpr->getType().getTypePtr();
		const core::TypePtr& classTypePtr = convFact.convertType(classDecl);

		core::VariablePtr var = builder.variable(builder.refType(classTypePtr));
		ctx.thisStack2 = var;

		retExpr = Visit(bindTempExpr->getSubExpr());

		convFact.ctx.thisStack2 = parentThisStack;

		return retExpr;

	}

	core::ExpressionPtr VisitExprWithCleanups(
			clang::ExprWithCleanups* cleanupExpr) {

		core::IRBuilder& builder =
		const_cast<core::IRBuilder&>(convFact.builder);

		ConversionFactory::ConversionContext::ScopeObjects parentScopeObjects =
		convFact.ctx.scopeObjects;
		ConversionFactory::ConversionContext::ScopeObjects parentScopeObjectsCopy =
		parentScopeObjects;

		while (!convFact.ctx.scopeObjects.empty()) {
			convFact.ctx.scopeObjects.pop();
		}

		core::ExpressionPtr retExpr;
		retExpr = Visit(cleanupExpr->getSubExpr());

		vector<core::StatementPtr> stmtList;
		stmtList.push_back(retExpr);

		vector<core::VariablePtr> params;
		vector<core::ExpressionPtr> args;
		core::VariablePtr var = 0;
		core::VariablePtr result = 0;
		bool addReturn = false;

		//if this stack is reference (this means that the temporary expression is bound to a reference)
		// we pass it to the upper scope.
		if (core::dynamic_pointer_cast<const core::Variable>(
						convFact.ctx.thisStack2)) {

			const ValueDecl* varDecl = tempHandler.getVariableDeclaration(
					core::dynamic_pointer_cast<const core::Variable>(
							convFact.ctx.thisStack2),
					convFact.ctx.varDeclMap);

			if (varDecl) {
				if (GET_TYPE_PTR(varDecl)->isReferenceType()) {

					if (!convFact.ctx.scopeObjects.empty()) {

						var = convFact.ctx.scopeObjects.top();
						convFact.ctx.scopeObjects.pop();
						params.push_back(var);
						args.push_back(builder.undefinedVar(var.getType()));
						result = var;
						addReturn = true;
					}
				}
			} else {
				result = core::dynamic_pointer_cast<const core::Variable>(
						convFact.ctx.thisStack2);
			}
		}

		tempHandler.handleTemporariesinScope(params,
				stmtList, args, convFact.ctx.scopeObjects,
				parentScopeObjects, true, true);

		convFact.ctx.scopeObjects = parentScopeObjectsCopy;

		while (!convFact.ctx.scopeObjects.empty()) {

			var = convFact.ctx.scopeObjects.top();
			convFact.ctx.scopeObjects.pop();
			const ValueDecl* varDecl = tempHandler.getVariableDeclaration(
					var, convFact.ctx.varDeclMap);
			if (!GET_TYPE_PTR(varDecl)->isReferenceType()) {
				params.push_back(var);
				args.push_back(var);
			}
		}

		convFact.ctx.scopeObjects = parentScopeObjects;

		core::TypePtr funcType;

		if (result) {
			if (addReturn) {
				stmtList.push_back(
						convFact.builder.returnStmt(result));
			}
			funcType = result.getType();

		} else {

			funcType = convFact.builder.getLangBasic().getUnit();
		}

		if (core::StructTypePtr globalStruct= convFact.ctx.globalStruct.first){

			params.push_back(convFact.ctx.globalVar);
			args.push_back(convFact.ctx.globalVar);
		}

		core::StatementPtr body = convFact.builder.compoundStmt(stmtList);
		core::LambdaExprPtr&& lambdaExpr = convFact.builder.lambdaExpr(funcType,body, params);

		return convFact.builder.callExpr(funcType, lambdaExpr, args);

	}

	core::ExpressionPtr VisitMaterializeTemporaryExpr(
			clang::MaterializeTemporaryExpr* materTempExpr) {

		core::ExpressionPtr retExpr;
		retExpr = Visit(materTempExpr->GetTemporaryExpr());

		return retExpr;
	}

	core::ExpressionPtr Visit(clang::Expr* expr) {
		return convFact.convertExpr(expr);
	}
};

CXXConversionFactory::CXXExprConverter*
CXXConversionFactory::makeCXXExprConvert(CXXConversionFactory& fact, Program& program) {
	return new CXXConversionFactory::CXXExprConverter(fact, program);
}

void CXXConversionFactory::cleanCXXExprConvert(CXXConversionFactory::CXXExprConverter* exprConv) {
	delete exprConv;
}

core::ExpressionPtr CXXConversionFactory::convertCXXExpr(const clang::Expr* expr) const {
	assert(expr && "Calling convertExpr with a NULL pointer");
	return cxxExprConv->Visit(const_cast<Expr*>(expr));
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
