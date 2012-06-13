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
#include "insieme/frontend/expr_converter.h"

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

//namespace std {
//
//std::ostream& operator<<(std::ostream& out, const clang::FunctionDecl* funcDecl) {
//	return out << funcDecl->getNameAsString() << "(" << funcDecl->param_size() << ")";
//}
//
//} // end std namespace

#define GET_REF_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::RefType>(type)->getElementType())

#define GET_VEC_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::VectorType>(type)->getElementType())

#define GET_ARRAY_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::ArrayType>(type)->getElementType())

#define LOG_CONVERSION(retIr) \
	FinalActions attachLog( [&] () { END_LOG_EXPR_CONVERSION(retIr); } )
//
//namespace {
//// FIXME
//// Covert clang source location into a annotations::c::SourceLocation object to be inserted in an CLocAnnotation
//annotations::c::SourceLocation convertClangSrcLoc(clang::SourceManager& sm, const clang::SourceLocation& loc) {
//
//	clang::SourceLocation cloc = loc;
//
//	if (sm.isMacroArgExpansion(cloc)) {
//		cloc = sm.getExpansionLoc(cloc);
//	}
//
//	clang::FileID&& fileId = sm.getFileID( sm.getSpellingLoc(cloc) );
//	const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
//	assert(fileEntry && "File cannot be NULL");
//
//	return annotations::c::SourceLocation(fileEntry->getName(), sm.getExpansionLineNumber(cloc),
//			sm.getExpansionColumnNumber(cloc));
//}
//
//// Returns a string of the text within the source range of the input stream
//std::string GetStringFromStream(const SourceManager& srcMgr, const SourceLocation& start) {
//	/*
//	 *  we use the getDecomposedSpellingLoc() method because in case we read macros values we have
//	 *  to read the expanded value
//	 */
//	std::pair<FileID, unsigned>&& startLocInfo = srcMgr.getDecomposedSpellingLoc(start);
//	llvm::StringRef&& startBuffer = srcMgr.getBufferData(startLocInfo.first);
//	const char *strDataStart = startBuffer.begin() + startLocInfo.second;
//
//	return string(strDataStart,
//			clang::Lexer::MeasureTokenLength(srcMgr.getSpellingLoc(start), srcMgr, clang::LangOptions())
//	);
//}
//
///*
// * In case the the last argument of the function is a var_arg, we try pack the exceeding arguments
// * with the pack operation provided by the IR.
// */
//vector<core::ExpressionPtr> tryPack(const core::IRBuilder& builder, core::FunctionTypePtr funcTy,
//		const ExpressionList& args) {
//
//	// check if the function type ends with a VAR_LIST type
//	const core::TypeList& argsTy = funcTy->getParameterTypes()->getElements();
//	// assert(argsTy && "Function argument is of not type TupleType");
//
//	// if the tuple type is empty it means we cannot pack any of the arguments
//	if (argsTy.empty()) {
//		return args;
//	}
//
//	const core::lang::BasicGenerator& gen = builder.getLangBasic();
//	if (gen.isVarList(argsTy.back())) {
//		ExpressionList ret;
//		assert(args.size() >= argsTy.size()-1 && "Function called with fewer arguments than necessary");
//		// last type is a var_list, we have to do the packing of arguments
//
//		// we copy the first N-1 arguments, the remaining will be unpacked
//		std::copy(args.begin(), args.begin() + argsTy.size() - 1, std::back_inserter(ret));
//
//		ExpressionList toPack;
//		if (args.size() > argsTy.size() - 1) {
//			std::copy(args.begin() + argsTy.size() - 1, args.end(), std::back_inserter(toPack));
//		}
//
//		// arguments has to be packed into a tuple expression, and then inserted into a pack expression
//		ret.push_back(builder.callExpr(gen.getVarList(), gen.getVarlistPack(), builder.tupleExpr(toPack)));
//		return ret;
//	}
//	return args;
//}
//
//core::CallExprPtr getSizeOfType(const core::IRBuilder& builder, const core::TypePtr& type) {
//	core::LiteralPtr size;
//
//	const core::lang::BasicGenerator& gen = builder.getLangBasic();
//	if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(type)) {
//		return builder.callExpr(gen.getUnsignedIntMul(), builder.literal(gen.getUInt8(), toString(*(vecTy->getSize()))),
//				getSizeOfType(builder, vecTy->getElementType()));
//	}
//	// in case of ref<'a>, recurr on 'a
//	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
//		return getSizeOfType(builder, refTy->getElementType());
//	}
//
//	return builder.callExpr(gen.getSizeof(), builder.getTypeLiteral(type));
//}
//
///**
// * Special method which handle malloc and calloc which need to be treated in a special way in the IR.
// */
//core::ExpressionPtr handleMemAlloc(const core::IRBuilder& builder, const core::TypePtr& type,
//		const core::ExpressionPtr& subExpr) {
//	if ( core::CallExprPtr&& callExpr = core::dynamic_pointer_cast<const core::CallExpr>(subExpr)) {
//
//		if ( core::LiteralPtr&& lit = core::dynamic_pointer_cast<const core::Literal>(callExpr->getFunctionExpr())) {
//
//			if (!(lit->getStringValue() == "malloc" || lit->getStringValue() == "calloc")) {
//				return core::ExpressionPtr();
//			}
//
//			assert(
//					((lit->getStringValue() == "malloc" && callExpr->getArguments().size() == 1) || (lit->getStringValue() == "calloc" && callExpr->getArguments().size() == 2)) && "malloc() and calloc() takes respectively 1 and 2 arguments");
//
//			const core::lang::BasicGenerator& gen = builder.getLangBasic();
//			// The type of the cast should be ref<array<'a>>, and the sizeof('a) need to be derived
//			assert(type->getNodeType() == core::NT_RefType);
//			assert(core::analysis::getReferencedType(type)->getNodeType() == core::NT_ArrayType);
//
//			const core::RefTypePtr& refType = core::static_pointer_cast<const core::RefType>(type);
//			const core::ArrayTypePtr& arrayType = refType->getElementType().as<core::ArrayTypePtr>();
//			const core::TypePtr& elemType = arrayType->getElementType();
//
//			/*
//			 * The number of elements to be allocated of type 'targetType' is:
//			 * 		-> 	expr / sizeof(targetType)
//			 */
//			core::CallExprPtr&& size = builder.callExpr(
//					gen.getUInt8(),
//					gen.getUnsignedIntDiv(),
//					callExpr->getArguments().front(),
//					getSizeOfType(builder, elemType)
//			);
//
//			// FIXME: calloc also initialize the memory to 0
//			return builder.refNew(
//					builder.callExpr(arrayType, gen.getArrayCreate1D(), builder.getTypeLiteral(elemType), size));
//		}
//	}
//	return core::ExpressionPtr();
//}
//
//core::ExpressionPtr getCArrayElemRef(const core::IRBuilder& builder, const core::ExpressionPtr& expr) {
//	const core::TypePtr& exprTy = expr->getType();
//	if (exprTy->getNodeType() == core::NT_RefType) {
//		const core::TypePtr& subTy = GET_REF_ELEM_TYPE(exprTy);
//
//		if (subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) {
//			core::TypePtr elemTy = core::static_pointer_cast<const core::SingleElementType>(subTy)->getElementType();
//
//			return builder.callExpr(
//					builder.refType(elemTy),
//					(subTy->getNodeType() == core::NT_VectorType ?
//							builder.getLangBasic().getVectorRefElem() : builder.getLangBasic().getArrayRefElem1D()),
//					expr, builder.uintLit(0));
//		}
//	}
//	return expr;
//}
//
//core::ExpressionPtr scalarToVector(core::ExpressionPtr scalarExpr, core::TypePtr refVecTy,
//		const core::IRBuilder& builder, const frontend::conversion::ConversionFactory& convFact) {
//	const core::lang::BasicGenerator& gen = builder.getNodeManager().getLangBasic();
//	const core::VectorTypePtr vecTy = convFact.tryDeref(refVecTy).as<core::VectorTypePtr>();
//
//	core::CastExprPtr cast = core::dynamic_pointer_cast<const core::CastExpr>(scalarExpr);
//	core::ExpressionPtr secondArg = cast ? cast->getSubExpression() : scalarExpr; // remove wrong casts added by clang
//	if (*secondArg->getType() != *vecTy->getElementType()) // add correct cast (if needed)
//		secondArg = builder.castExpr(vecTy->getElementType(), secondArg);
//
//	return builder.callExpr(gen.getVectorInitUniform(), secondArg, builder.getIntTypeParamLiteral(vecTy->getSize()));
//}
//
//} // end anonymous namespace

namespace insieme {
namespace frontend {

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
//						ConversionFactory
//---------------------------------------------------------------------------------------------------------------------

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
//						CXXConversionFactory
//---------------------------------------------------------------------------------------------------------------------

//---------------------------------------------------------------------------------------------------------------------
//										CXX Extension Converter UTILITY FUNCTIONS
//---------------------------------------------------------------------------------------------------------------------
CXXConversionFactory::CXXExtExprConverter*
CXXConversionFactory::makeExprConvert(CXXConversionFactory& fact, Program& program) {
	return new CXXConversionFactory::CXXExtExprConverter(fact, program);
}

void CXXConversionFactory::cleanExprConvert(CXXConversionFactory::CXXExtExprConverter* exprConv) {
	delete exprConv;
}

//---------------------------------------------------------------------------------------------------------------------
//										CXX EXPRESSION Converter UTILITY FUNCTIONS
//---------------------------------------------------------------------------------------------------------------------
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

// create initializations statements for the offsetTable
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

// create initializations statements for the vFuncTable
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
										callVFunc = exprutils::getCArrayElemRef(builder, callVFunc);
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
//						CXX CONVERT FUNCTION DECLARATION
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

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
