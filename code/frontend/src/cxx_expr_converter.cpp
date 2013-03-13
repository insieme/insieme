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

#include "insieme/frontend/expr_converter.h"

#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/annotations/c/location.h"
#include "insieme/annotations/c/naming.h"

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


// [3.0]
//#include "clang/Index/Entity.h"
//#include "clang/Index/Indexer.h"

#include "clang/AST/StmtVisitor.h"
#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/CXXInheritance.h>

#include "clang/Basic/FileManager.h"

using namespace clang;
using namespace insieme;
using namespace exprutils;

namespace insieme {
namespace frontend {

namespace {

} // end anonymous namespace 


namespace conversion {

//---------------------------------------------------------------------------------------------------------------------
//										CXX EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						  IMPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr) {
	START_LOG_EXPR_CONVERSION(castExpr);
	core::ExpressionPtr retIr;

	switch (castExpr->getCastKind()) {
		case CK_UncheckedDerivedToBase:
			//A conversion from a C++ class pointer/reference to a base class that can assume that
			//the derived pointer is not null. const A &a = B(); b->method_from_a(); 
			{
				// if is a derived class, we will return a narrow expression with the datapath
				// to access the right superclass
				core::TypePtr targetTy;
				retIr = Visit(castExpr->getSubExpr());
			
				// in case of pointer, the inner expression is modeled as ref< array < C, 1> >
				// it is needed to deref the first element
				retIr = getCArrayElemRef(builder, retIr);

				clang::CastExpr::path_const_iterator it;
				for (it = castExpr->path_begin(); it!= castExpr->path_end(); ++it){
					targetTy = convFact.convertType((*it)->getType().getTypePtr());
					retIr = convFact.builder.refParent(retIr, targetTy);
				}
				break;
			}
		case CK_DerivedToBase:  
			//A conversion from a C++ class pointer to a base class pointer. A *a = new B();
			{
				retIr = Visit(castExpr->getSubExpr());
				break;
		//		assert(false && "derived to base cast  not implementd");
			}
		
		case CK_BaseToDerived: 
			//A conversion from a C++ class pointer/reference to a derived class pointer/reference. B *b = static_cast<B*>(a); 
			{
				assert(false && "base to derived cast  not implementd B* b = static_cast<B*>(A)");
				break;
			}
		default:
			retIr = ExprConverter::VisitImplicitCastExpr(castExpr);
			break;
	}
	END_LOG_EXPR_CONVERSION(retIr);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						EXPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr) {
	assert(false && "explicit cast cast expression");
	/*START_LOG_EXPR_CONVERSION(castExpr);

	const core::IRBuilder& builder = convFact.builder;
	core::ExpressionPtr retIr = Visit(castExpr->getSubExpr());
	LOG_EXPR_CONVERSION(retIr);

	core::TypePtr classTypePtr; // used for CK_DerivedToBase
	core::StringValuePtr ident;
	VLOG(2) << retIr << " " << retIr->getType();
	switch (castExpr->getCastKind()) {

	case CK_BaseToDerived: {
		// find the class type - if not converted yet, converts and adds it
		classTypePtr = convFact.convertType(GET_TYPE_PTR(castExpr));
		assert(classTypePtr && "no class declaration to type pointer mapping");

		VLOG(2) << "BaseToDerived Cast" << classTypePtr;

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

			VLOG(2) << "member name " << recordDecl->getName().data();
			ident = builder.stringValue(recordDecl->getName().data());

			VLOG(2) << "DerivedToBase Cast on " << classTypePtr;

			core::ExpressionPtr op = builder.getLangBasic().getCompositeMemberAccess();
			core::TypePtr structTy = retIr->getType();

			if (structTy->getNodeType() == core::NT_RefType) {
				// skip over reference wrapper
				structTy = core::analysis::getReferencedType(structTy);
				op = builder.getLangBasic().getCompositeRefElem();
			}
			VLOG(2) << structTy;

			const core::TypePtr& memberTy =
					core::static_pointer_cast<const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);

			core::TypePtr resType = builder.refType(classTypePtr);

			retIr = builder.callExpr(resType, op, retIr, builder.getIdentifierLiteral(ident),
					builder.getTypeLiteral(memberTy));
			VLOG(2) << retIr;
		}
		return retIr;
	}
	case CK_ConstructorConversion: {
		return retIr;
	}
	default:
		// call base Visitor for ExplicitCastExpr
		return (retIr = ExprConverter::VisitExplicitCastExpr(castExpr));
	}

	assert(false); */
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							FUNCTION CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCallExpr(const clang::CallExpr* callExpr) {
	return ExprConverter::VisitCallExpr(callExpr);
/*
	START_LOG_EXPR_CONVERSION(callExpr);

	const core::IRBuilder& builder = convFact.builder;

	// return converted node
	core::ExpressionPtr irNode;
	LOG_EXPR_CONVERSION(irNode);

	if (callExpr->getDirectCallee()) {

		FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee());

		core::FunctionTypePtr funcTy = core::static_pointer_cast<const core::FunctionType>(
				convFact.convertType(GET_TYPE_PTR(funcDecl)));

		// collects the type of each argument of the expression
		ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);

		assert( convFact.currTU && "Translation unit not set.");

		const TranslationUnit* oldTU = convFact.currTU;
		const FunctionDecl* definition = NULL;

		 // this will find function definitions if they are declared in  the same translation unit
		 // (also defined as static)
		 
		if (!funcDecl->hasBody(definition)) {
			// if the function is not defined in this translation unit, maybe it is defined in another we already
			// loaded use the clang indexer to lookup the definition for this function declarations
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

		// We find a definition, we lookup if this variable needs to access the globals, in that case the capture
		// list needs to be initialized with the value of global variable in the current scope
		if (ctx.globalFuncSet.find(definition) != ctx.globalFuncSet.end()) {
			// we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
			// current context
			assert( ctx.globalVar && "No global definitions forwarded to this point");
			packedArgs.insert(packedArgs.begin(), ctx.globalVar);
		}

		// If we are resolving the body of a recursive function we have to return the associated variable every
		// time a function in the strongly connected graph of function calls is encountered.
		if (ctx.isResolvingRecFuncBody) {
			// check if this type has a typevar already associated, in such case return it
			ConversionContext::RecVarExprMap::const_iterator fit = ctx.recVarExprMap.find(definition);
			if (fit != ctx.recVarExprMap.end()) {
				// we are resolving a parent recursive type, so when one of the recursive functions in the
				// connected components are called, the introduced mu variable has to be used instead.
				convFact.currTU = oldTU;
				return (irNode = builder.callExpr(funcTy->getReturnType(),
						static_cast<core::ExpressionPtr>(fit->second), packedArgs));
			}
		}

		if (!ctx.isResolvingRecFuncBody) {

			ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(definition);

			if (fit != ctx.lambdaExprCache.end()) {

				std::vector<core::VariablePtr> temporaries = tempHandler.retrieveFunctionTemporaries(definition,
						convFact.cxxCtx.fun2TempMap);

				vector<core::VariablePtr>::iterator it;

				for (it = temporaries.begin(); it < temporaries.end(); it++) {

					core::VariablePtr var = *it;
					packedArgs.push_back(var);
					convFact.cxxCtx.scopeObjects.push(var);
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

		ConversionFactory::ConversionContext::ScopeObjects parentScopeObjects = convFact.cxxCtx.scopeObjects;
		while (!convFact.cxxCtx.scopeObjects.empty()) {
			convFact.cxxCtx.scopeObjects.pop();
		}

		core::ExpressionPtr lambdaExpr = core::static_pointer_cast<const core::LambdaExpr>(
				convFact.convertFunctionDecl(definition));

		std::vector<core::VariablePtr> temporaries = tempHandler.retrieveFunctionTemporaries(definition,
				convFact.cxxCtx.fun2TempMap);

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
		convFact.cxxCtx.scopeObjects = parentScopeObjects;

		return (irNode = builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs));

	} else if (callExpr->getCallee()) {
		core::ExpressionPtr funcPtr = convFact.tryDeref(Visit(callExpr->getCallee()));
		core::TypePtr subTy = funcPtr->getType();

		if (subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) {
			subTy = core::static_pointer_cast<const core::SingleElementType>(subTy)->getElementType();
			funcPtr = builder.callExpr(subTy, builder.getLangBasic().getArraySubscript1D(), funcPtr,
					builder.uintLit(0));
		}
		assert( subTy->getNodeType() == core::NT_FunctionType && "Using () operator on a non function object");

		const core::FunctionTypePtr& funcTy = core::static_pointer_cast<const core::FunctionType>(subTy);
		ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);
		return (irNode = builder.callExpr(funcPtr, args));

	} else {
		assert( false && "Call expression not referring a function");
	}
	assert(false);*/
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						  MEMBER EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitMemberExpr(const clang::MemberExpr* memExpr){
	START_LOG_EXPR_CONVERSION(memExpr);
	core::ExpressionPtr retIr;

	// we have the situation here in which we might want to access a field of a superclass
	// this will not be resolved by the C frontend. and we need to build the right datapath to
	// reach the definition
	
	return ConversionFactory::ExprConverter::VisitMemberExpr(memExpr);
		/*

	
	if (llvm::isa<clang::FieldDecl>(memExpr->getMemberDecl())){
		clang::FieldDecl *fieldDecl = llvm::cast<clang::FieldDecl>(memExpr->getMemberDecl());
		
		fieldDecl->dump();
		assert(false);


	}
	

	END_LOG_EXPR_CONVERSION(retIr);
	return retIr;
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							VAR DECLARATION REFERENCE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitDeclRefExpr(const clang::DeclRefExpr* declRef) {

	//FIXME:  this may be related with c++ references
	return ExprConverter::VisitDeclRefExpr (declRef);

	/*START_LOG_EXPR_CONVERSION(declRef);

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(retIr);

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
	assert(false && "DeclRefExpr not supported!");*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								CXX BOOLEAN LITERAL
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXBoolLiteralExpr(const clang::CXXBoolLiteralExpr* boolLit) {
	START_LOG_EXPR_CONVERSION(boolLit);

	core::ExpressionPtr retExpr =
			convFact.builder.literal(
					(boolLit->getValue())? std::string("true"): std::string("false"),
					convFact.mgr.getLangBasic().getBool());

	END_LOG_EXPR_CONVERSION(retExpr);
	return retExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX MEMBER CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXMemberCallExpr(const clang::CXXMemberCallExpr* callExpr) {
	START_LOG_EXPR_CONVERSION(callExpr);
	const core::IRBuilder& builder = convFact.builder;

	// TODO: static methods
	
	const CXXMethodDecl* methodDecl = callExpr->getMethodDecl();

	// to begin with we translate the constructor as a regular function
	auto f = convFact.convertFunctionDecl(llvm::cast<clang::FunctionDecl> (methodDecl), false);
	assert(f.isa<core::LambdaExprPtr>());

	core::ExpressionPtr ownerObj = Visit(callExpr->getImplicitObjectArgument());
	core::TypePtr&& irClassType = ownerObj->getType();
	core::LambdaExprPtr newFunc = convFact.memberize(llvm::cast<FunctionDecl>(methodDecl), 
													 f.as<core::ExpressionPtr>(),
													 irClassType, 
													 core::FK_MEMBER_FUNCTION);
 
	// correct the owner object reference, in case of pointer (ref<array<struct<...>,1>>) we need to
	// index the first element
	ownerObj = getCArrayElemRef(builder, ownerObj);

	// reconstruct Arguments list, fist one is a scope location for the object 
	// because is a member call, it should exist an instance of it somewhere
	core::ExpressionList args;
	args.push_back (ownerObj);

	// append globalVar to arguments if needed
	if ( ctx.globalFuncSet.find(methodDecl) != ctx.globalFuncSet.end() ) {
		args.push_back(ctx.globalVar);
	}

	// afterwards come the original arguments in the order AST specifies
	clang::CXXMemberCallExpr::const_arg_iterator arg = callExpr->arg_begin();
	clang::CXXMemberCallExpr::const_arg_iterator end = callExpr->arg_end();
	for (; arg!=end; ++arg){
		args.push_back(Visit(*arg));
	}

	// build expression and we are done!!!
	core::TypePtr retTy = newFunc.as<core::LambdaExprPtr>().getType().as<core::FunctionTypePtr>().getReturnType();
	core::CallExprPtr      ret  = builder.callExpr   (retTy, newFunc, args);
	if (VLOG_IS_ON(2)){
		dumpPretty(&(*ret));
	}
	END_LOG_EXPR_CONVERSION(ret);
	return ret;

	/*
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
	core::TypePtr parentCurTy = convFact.cxxCtx.curTy;
	convFact.cxxCtx.curTy = classType;

	// store previous THIS
	core::ExpressionPtr parentThisStack = convFact.cxxCtx.thisStack2;

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
		core::ExpressionPtr parentThisStack = convFact.cxxCtx.thisStack2;

		core::VariablePtr var =
		core::static_pointer_cast<const core::Variable>( convFact.lookUpVariable(definition) );

		convFact.cxxCtx.thisStack2 = var;
		assert(var && "Variable for THIS not set");

		//get clang type of THIS object --> needed for virtual functions
		thisType = GET_TYPE_PTR(definition);
	} else {
		convFact.cxxCtx.thisStack2 = thisPtr;
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
	// We find a definition, we lookup if this variable needs to access the globals, in that case the capture
	// list needs to be initialized with the value of global variable in the current scope
	if ( ctx.globalFuncSet.find(definition) != ctx.globalFuncSet.end() ) {
		// we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
		// current context
		assert(ctx.globalVar && "No global definitions forwarded to this point");
		packedArgs.insert(packedArgs.begin(), ctx.globalVar);
	}

	assert(convFact.cxxCtx.thisStack2 && "thisStack2 empty!");

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
	convFact.cxxCtx.curTy = parentCurTy;

	// reset previous THIS
	convFact.cxxCtx.thisStack2 = parentThisStack;

	VLOG(2) << "End of expression CXXMemberCallExpr \n";
	return retExpr;
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX OPERATOR CALL EXPRESSION
//
//  A call to an overloaded operator written using operator syntax.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXOperatorCallExpr(const clang::CXXOperatorCallExpr* callExpr) {
	assert (false && "operator call expr");
	/*
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
		parentThisStack = convFact.cxxCtx.thisStack2;

		VLOG(2) << "funcTy: " << funcTy;
		VLOG(2) << "packedArgs: " << packedArgs;

		//used to determine if global struct is needed as parameter
		definition = methodDecl;

		// get the lhs-this
		convFact.cxxCtx.lhsThis = Visit(callExpr->getArg(0));
		convFact.cxxCtx.thisStack2 = convFact.cxxCtx.lhsThis;
		//cxxConvFact.ctx.rhsThis = Visit(callExpr->getArg(1));

		//add the "this" as arg as we have an operator as a member-function
		packedArgs.push_back(convFact.cxxCtx.lhsThis);

		assert(convFact.cxxCtx.thisStack2);
		convFact.cxxCtx.isCXXOperator=true;

		core::ExpressionPtr thisPtr = convFact.cxxCtx.lhsThis;
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
			convFact.cxxCtx.thisStack2 = thisPtr;
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
		args = getFunctionArguments(builder, callExpr, funcTy);  //, true);

		// convert the function declaration
		packedArgs = tryPack(builder, funcTy, args);

		// store THIS
		parentThisStack = convFact.cxxCtx.thisStack2;

		VLOG(2) << "funcTy: " << funcTy;
		VLOG(2) << "packedArgs: " << packedArgs;

		lambdaExpr = core::static_pointer_cast<const core::LambdaExpr>( convFact.convertFunctionDecl(funcDecl) );

		//used to determine if global struct is needed as parameter
		definition = funcDecl;
	} else {
		assert(false && "CXXOperatorCall - operator not defined as non-member or member function");
	}

	// make a copy of the current scopeObjects stack and empty the stack
	ConversionFactory::XConversionContext::ScopeObjects parentScopeObjects =
	convFact.cxxCtx.scopeObjects;
	while (!convFact.cxxCtx.scopeObjects.empty()) {
		convFact.cxxCtx.scopeObjects.pop();
	}

	std::vector<core::VariablePtr> temporaries =
	tempHandler.retrieveFunctionTemporaries(definition,
			convFact.cxxCtx.fun2TempMap);

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

	convFact.cxxCtx.scopeObjects = parentScopeObjects;

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

	// We find a definition, we lookup if this variable needs to access the globals, in that case the capture
	// list needs to be initialized with the value of global variable in the current scope
	if ( ctx.globalFuncSet.find(definition) != ctx.globalFuncSet.end() ) {
		// we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
		// current context
		assert(ctx.globalVar && "No global definitions forwarded to this point");
		packedArgs.insert(packedArgs.begin(), ctx.globalVar);
	}

	VLOG(2) << "funcTy: " << funcTy;
	VLOG(2) << "packedArgs: " << packedArgs;

	retExpr = convFact.builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs);

	// reset to parent THIS
	convFact.cxxCtx.thisStack2 = parentThisStack;

	convFact.cxxCtx.isCXXOperator=false;
	convFact.cxxCtx.lhsThis = 0;
	convFact.cxxCtx.rhsThis = 0;

	//assert(false && "CXXOperatorCallExpr not yet handled");
	VLOG(2) << "End of expression CXXOperatorCallExpr \n";
	return retExpr;
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX CONSTRUCTOR CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXConstructExpr(const clang::CXXConstructExpr* callExpr) {
	START_LOG_EXPR_CONVERSION(callExpr);
	const core::IRBuilder& builder = convFact.builder;

// TODO:  array constructor

	const CXXConstructorDecl* ctorDecl = callExpr->getConstructor();

	const clang::Type* classType= callExpr->getType().getTypePtr();
	core::TypePtr&& irClassType = convFact.convertType(classType);

	// to begin with we translate the constructor as a regular function but with initialization list
	auto f = convFact.convertCtor(ctorDecl, irClassType);
	assert(f.isa<core::LambdaExprPtr>());

	// with the transformed lambda, we can extract the body and re-type it into a constructor type
	core::StatementPtr body = f.as<core::LambdaExprPtr>()->getBody();
	auto params = f.as<core::LambdaExprPtr>()->getParameterList();

	// update parameter list with a class-typed parameter in the first possition
	core::TypePtr&&  refToClass = builder.refType(irClassType);
	core::LambdaExprPtr newFunc = convFact.memberize(llvm::cast<FunctionDecl>(ctorDecl), 
													 f.as<core::ExpressionPtr>(),
													 refToClass, 
													 core::FK_CONSTRUCTOR);

	// reconstruct Arguments list, fist one is a scope location for the object
	core::ExpressionList args;
	args.push_back (builder.undefinedVar(refToClass));

	// append globalVar to arguments if needed
	if ( ctx.globalFuncSet.find(ctorDecl) != ctx.globalFuncSet.end() ) {
		args.push_back(ctx.globalVar);
	}

	// afterwards come the original arguments in the order AST specifies
	clang::CXXConstructExpr::const_arg_iterator arg = callExpr->arg_begin();
	clang::CXXConstructExpr::const_arg_iterator end = callExpr->arg_end();
	for (; arg!=end; ++arg){
		args.push_back(Visit(*arg));
	}

	// build expression and we are done!!!
	core::CallExprPtr ret  = builder.callExpr (refToClass, newFunc, args);
	if (VLOG_IS_ON(2)){
		dumpPretty(&(*ret));
	}
	END_LOG_EXPR_CONVERSION(ret);
	return ret;

	/*
	// We get a pointer to the object that is constructed and we store the pointer to tv8he scope objects stack
	//that holds the objects that are constructed in the current scope
	core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(convFact.cxxCtx.thisStack2);
	CXXRecordDecl* classDecl = 0;

	if(callExpr->getType()->getAs<RecordType>()){
		classDecl = cast<CXXRecordDecl>(callExpr->getType()->getAs<RecordType>()->getDecl());
	}

	if(classDecl){
		if (classDecl->getDestructor()) {
				convFact.cxxCtx.scopeObjects.push(var);
				convFact.cxxCtx.objectMap.insert(std::make_pair(var,classDecl));
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

	// We find a definition, we lookup if this variable needs to access the globals, in that case the capture
	// list needs to be initialized with the value of global variable in the current scope
	if ( ctx.globalFuncSet.find(definition) != ctx.globalFuncSet.end() ) {

		// we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
		// current context
		assert(ctx.globalVar && "No global definitions forwarded to this point");
		packedArgs.insert(packedArgs.begin(), ctx.globalVar);

		isArrayTempGlobalVar = builder.variable(ctx.globalVar->getType());
		params.insert(params.begin(), isArrayTempGlobalVar);
	}

	assert( convFact.currTU && "Translation unit not set.");

	// preserve THIS
	core::ExpressionPtr parentThisStack = convFact.cxxCtx.thisStack2;

	if(isArrayType) {
		cxxCtx.thisStack2 = builder.variable(convFact.convertType(arrayElemType));
		ctx.globalVar = isArrayTempGlobalVar;
		//packedArgs.push_back(cxxConvFact.ctx.thisStack2);
	} else {
		packedArgs.push_back(parentThisStack);
	}
	VLOG(2) << cxxCtx.thisStack2 << parentThisStack;

	CXXConversionFactory::CXXConversionContext::ScopeObjects downStreamSScopeObjectsCopy =
	convFact.cxxCtx.downStreamScopeObjects;

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

	convFact.cxxCtx.thisStack2 = parentThisStack;
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
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX NEW CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXNewExpr(const clang::CXXNewExpr* callExpr) {
	START_LOG_EXPR_CONVERSION(callExpr);

	//TODO:  - array allocation - inplace allocation
	core::ExpressionPtr retExp;

	if (callExpr->getAllocatedType().getTypePtr()->isBuiltinType()){

		core::TypePtr type = convFact.convertType(callExpr->getAllocatedType().getTypePtr());
		core::ExpressionPtr placeHolder = builder.undefinedNew(type);

		if (callExpr->isArray()){
			core::ExpressionPtr&& arrSizeExpr = convFact.convertExpr( callExpr->getArraySize() );
			placeHolder = builder.callExpr( builder.arrayType(type), 
											builder.getLangBasic().getArrayCreate1D(),
											builder.getTypeLiteral(type),
											utils::cast(arrSizeExpr, gen.getUInt4()));
		}

		// fixme, array size
		retExp = builder.refNew(builder.refVar(placeHolder));
	}
	else{
		core::ExpressionPtr ctorCall = Visit(callExpr->getConstructExpr());
		assert(ctorCall.isa<core::CallExprPtr>() && "aint no constructor call in here, no way to translate NEW");


		core::TypePtr type = ctorCall->getType();
		core::ExpressionPtr newCall = builder.undefinedNew(type);
		
		if (callExpr->isArray()){
			core::ExpressionPtr&& arrSizeExpr = convFact.convertExpr( callExpr->getArraySize() );
			newCall = builder.callExpr( builder.arrayType(type), 
											builder.getLangBasic().getArrayCreate1D(),
											builder.getTypeLiteral(type),
											utils::cast(arrSizeExpr, gen.getUInt4()));
		}

		// the basic constructor translation defines a stack variable as argument for the call
		// in order to turn this into a diynamic memory allocation, we only need to substitute 
		// the first argument for a heap location
		core::CallExprAddress addr(ctorCall.as<core::CallExprPtr>());
		core::CallExprPtr newCtor = core::transform::replaceNode (convFact.mgr, 
																  addr->getArgument(0), 
																  newCall ).as<core::CallExprPtr>();
		retExp = builder.refVar(newCtor);
	}

	END_LOG_EXPR_CONVERSION(retExp);
	return retExp;

	/*
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
	core::ExpressionPtr parentThisStack = convFact.cxxCtx.thisStack2;
	convFact.cxxCtx.thisStack2 = var;

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
		// We find a definition, we lookup if this variable needs to access the globals, in that case the capture
		// list needs to be initialized with the value of global variable in the current scope
		if ( ctx.globalFuncSet.find(definition) != ctx.globalFuncSet.end() ) {
			// we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
			// current context
			assert(ctx.globalVar && "No global definitions forwarded to this point");
			packedArgs.insert(packedArgs.begin(), ctx.globalVar);
		}

		core::ExpressionPtr ctorExpr = core::static_pointer_cast<const core::LambdaExpr>( convFact.convertFunctionDecl(funcDecl) );

		convFact.cxxCtx.thisStack2 = parentThisStack;

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
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX DELETE CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXDeleteExpr(const clang::CXXDeleteExpr* deleteExpr) {
	START_LOG_EXPR_CONVERSION(deleteExpr);
	core::ExpressionPtr retExpr;
	core::ExpressionPtr deleteExp = Visit(deleteExpr->getArgument());

	retExpr = builder.callExpr (builder.getLangBasic().getRefDelete(),
								deleteExp);
	
	END_LOG_EXPR_CONVERSION(retExpr);
	return retExpr;

	/*
	core::ExpressionPtr retExpr;
	const core::IRBuilder& builder = convFact.builder;
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	//check if argument is class/struct (with non-trivial dtor), otherwise just call "free" for builtin types
	if(deleteExpr->getDestroyedType().getTypePtr()->isStructureOrClassType()
			&& !deleteExpr->getDestroyedType()->getAsCXXRecordDecl()->hasTrivialDestructor() ) {
		// the call of the dtor and the "free" of the destroyed object is done in an
		// lambdaExpr so we have to pass the object we destroy and if we have a virtual dtor
		// also the globalVar to the lambdaExpr

		core::ExpressionPtr delOpIr;
		core::ExpressionPtr dtorIr;
		core::ExpressionPtr parentThisStack = convFact.cxxCtx.thisStack2;

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
		if ( ctx.globalFuncSet.find(dtorDecl) != ctx.globalFuncSet.end() ) {
			isDtorUsingGlobals=true;
		}

		// new variable for the object to be destroied, inside the lambdaExpr
		core::TypePtr classTypePtr = convFact.convertType( deleteExpr->getDestroyedType().getTypePtr() );

		core::VariablePtr&& var = builder.variable( builder.refType( builder.refType( builder.arrayType( classTypePtr ))));
		convFact.cxxCtx.thisStack2 = var;

		// for virtual dtor's globalVar, offsetTable and vfuncTable need to be updated
		const core::VariablePtr parentGlobalVar = ctx.globalVar;
		const core::ExpressionPtr parentOffsetTableExpr = cxxCtx.offsetTableExpr;
		const core::ExpressionPtr parentVFuncTableExpr = cxxCtx.vFuncTableExpr;

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
			// Dtors are called in reverse order of construction!
			core::ForStmtPtr dtorLoop = builder.forStmt(
				itVar,
				arraySize,
				builder.literal(gen.getUInt4(), toString(0)),
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
			cxxCtx.offsetTableExpr = parentOffsetTableExpr;
			cxxCtx.vFuncTableExpr = parentVFuncTableExpr;
			retExpr = builder.callExpr(lambdaExpr, ctx.globalVar, argToDelete);
		} else {
			retExpr = builder.callExpr(lambdaExpr, argToDelete);
		}

		convFact.cxxCtx.thisStack2 = parentThisStack;
	} else {
		// build the free statement with the correct variable
		retExpr = builder.callExpr(
				builder.getLangBasic().getRefDelete(),
				builder.deref( Visit(deleteExpr->getArgument()) )
		);
	}

	VLOG(2) << "End of expression CXXDeleteExpr \n";
	return retExpr;
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX THIS CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXThisExpr(const clang::CXXThisExpr* thisExpr) {
	START_LOG_EXPR_CONVERSION(thisExpr);

	//figure out the type of the expression
	core::TypePtr&& irType = convFact.convertType( llvm::cast<clang::TypeDecl>(thisExpr->getBestDynamicClassType())->getTypeForDecl() );
	irType = builder.refType(irType);

	// build a literal as a placeholder (has to be substituted later by function call expression)
	auto ret =  builder.literal("this", irType);
	END_LOG_EXPR_CONVERSION(ret);
	return ret;

	/*irType = bu
	ilder.refType(irType);
//		VLOG(2) << "thisStack2: " << cxxConvFact.ctx.thisStack2;
//		VLOG(2) << "thisVar: " << cxxConvFact.ctx.thisVar;

	//Need thisVar not Stack2 //assert(cxxConvFact.ctx.thisStack2 && "THIS is empty");
	assert(convFact.cxxCtx.thisVar && "THIS is empty");

	VLOG(2) << "CXXThisExpr: \n";
	if( VLOG_IS_ON(2) ) {
		callExpr->dump();
	}

	VLOG(2) << "End of expression CXXThisExpr \n";
	//Need thisVar not Stack2 //return cxxConvFact.ctx.thisStack2;
	return convFact.cxxCtx.thisVar;
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					EXCEPTION CXX THROW EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXThrowExpr(const clang::CXXThrowExpr* throwExpr) {
	assert (false && "throw expr");
	/*
	START_LOG_EXPR_CONVERSION(throwExpr);
	assert(false && "VisitCXXThrowExpr not yet handled");
	VLOG(2) << "End of expression\n";
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					CXX DEFAULT ARG EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXDefaultArgExpr(const clang::CXXDefaultArgExpr* defaultArgExpr) {
	START_LOG_EXPR_CONVERSION(defaultArgExpr);
	auto ret = Visit(defaultArgExpr->getExpr());
	END_LOG_EXPR_CONVERSION(ret);
	return ret;
	/*
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
	*/
}

core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXBindTemporaryExpr(const clang::CXXBindTemporaryExpr* bindTempExpr) {
	assert (false && "bind temporary expr");
	/*

	core::IRBuilder& builder =
	const_cast<core::IRBuilder&>(convFact.builder);

	core::ExpressionPtr retExpr;

	core::ExpressionPtr parentThisStack = convFact.cxxCtx.thisStack2;

	const Type* classDecl = bindTempExpr->getType().getTypePtr();
	const core::TypePtr& classTypePtr = convFact.convertType(classDecl);

	core::VariablePtr var = builder.variable(builder.refType(classTypePtr));
	cxxCtx.thisStack2 = var;

	retExpr = Visit(bindTempExpr->getSubExpr());

	convFact.cxxCtx.thisStack2 = parentThisStack;

	return retExpr;
	*/
}

core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitExprWithCleanups(const clang::ExprWithCleanups* cleanupExpr) {
	assert (false && "exp with cleanpus expr");
	/*

	core::IRBuilder& builder =
	const_cast<core::IRBuilder&>(convFact.builder);

	CXXConversionFactory::CXXConversionContext::ScopeObjects parentScopeObjects =
	convFact.cxxCtx.scopeObjects;
	CXXConversionFactory::CXXConversionContext::ScopeObjects parentScopeObjectsCopy =
	parentScopeObjects;

	while (!convFact.cxxCtx.scopeObjects.empty()) {
		convFact.cxxCtx.scopeObjects.pop();
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
					convFact.cxxCtx.thisStack2)) {

		const ValueDecl* varDecl = tempHandler.getVariableDeclaration(
				core::dynamic_pointer_cast<const core::Variable>(
						convFact.cxxCtx.thisStack2),
				convFact.ctx.varDeclMap);

		if (varDecl) {
			if (GET_TYPE_PTR(varDecl)->isReferenceType()) {

				if (!convFact.cxxCtx.scopeObjects.empty()) {

					var = convFact.cxxCtx.scopeObjects.top();
					convFact.cxxCtx.scopeObjects.pop();
					params.push_back(var);
					args.push_back(builder.undefinedVar(var.getType()));
					result = var;
					addReturn = true;
				}
			}
		} else {
			result = core::dynamic_pointer_cast<const core::Variable>(
					convFact.cxxCtx.thisStack2);
		}
	}

	tempHandler.handleTemporariesinScope(params,
			stmtList, args, convFact.cxxCtx.scopeObjects,
			parentScopeObjects, true, true);

	convFact.cxxCtx.scopeObjects = parentScopeObjectsCopy;

	while (!convFact.cxxCtx.scopeObjects.empty()) {

		var = convFact.cxxCtx.scopeObjects.top();
		convFact.cxxCtx.scopeObjects.pop();
		const ValueDecl* varDecl = tempHandler.getVariableDeclaration(
				var, convFact.ctx.varDeclMap);
		if (!GET_TYPE_PTR(varDecl)->isReferenceType()) {
			params.push_back(var);
			args.push_back(var);
		}
	}

	convFact.cxxCtx.scopeObjects = parentScopeObjects;

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
	*/
}

core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitMaterializeTemporaryExpr(
		const clang::MaterializeTemporaryExpr* materTempExpr) {
	assert(false && "materialize temp");
	/*

	core::ExpressionPtr retExpr;
	retExpr = Visit(materTempExpr->GetTemporaryExpr());

	return retExpr;
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::Visit(const clang::Expr* expr) {
	
	VLOG(2) << "CXX";
	core::ExpressionPtr&& retIr = ConstStmtVisitor<ConversionFactory::CXXExprConverter, core::ExpressionPtr>::Visit(expr);

	// check for OpenMP annotations
	return omp::attachOmpAnnotation(retIr, expr, convFact);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
