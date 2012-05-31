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

virtual core::ExpressionPtr ConversionFactory::convertInitExpr(const clang::Expr* expr, const core::TypePtr& type,
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
virtual core::NodePtr ConversionFactory::convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool isEntryPoint) {

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

	exprConv->tempHandler.handleTemporariesinScope(funcDecl, funcType, params, ctx.scopeObjects, true, true, false);

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

			exprConv->tempHandler.handleTemporariesinScope(params, stmts, ctx.downStreamScopeObjects, false, false);
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

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
