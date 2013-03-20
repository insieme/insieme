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
#include "insieme/frontend/utils/temporariesLookup.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/pragma/insieme.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
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
			// cast which should look like C unleas is a C++ Ref type
			{
				auto targetType = convFact.convertType(GET_TYPE_PTR(castExpr));
				if (core::analysis::isCppRef( targetType )){
					assert(false);
				}

				retIr = ExprConverter::VisitImplicitCastExpr(castExpr);
			}
			break;
	}
	END_LOG_EXPR_CONVERSION(retIr);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						EXPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr) {
// FIXME: do the thing here
	return (ExprConverter::VisitExplicitCastExpr(castExpr));

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
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						  MEMBER EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitMemberExpr(const clang::MemberExpr* memExpr){
//	START_LOG_EXPR_CONVERSION(memExpr);
//	core::ExpressionPtr retIr;

	// we have the situation here in which we might want to access a field of a superclass
	// this will not be resolved by the C frontend. and we need to build the right datapath to
	// reach the definition
	
	return ConversionFactory::ExprConverter::VisitMemberExpr(memExpr);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							VAR DECLARATION REFERENCE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitDeclRefExpr(const clang::DeclRefExpr* declRef) {
	return ConversionFactory::ExprConverter::VisitDeclRefExpr (declRef);

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
	auto f = convFact.convertFunctionDecl(llvm::cast<clang::FunctionDecl> (methodDecl), false).as<core::ExpressionPtr>();

	core::ExpressionPtr ownerObj = Visit(callExpr->getImplicitObjectArgument());
	core::TypePtr&& irClassType = ownerObj->getType();
	
	core::LambdaExprPtr newFunc;
	if(f.isa<core::LambdaExprPtr>()) {
		newFunc = convFact.memberize(llvm::cast<FunctionDecl>(methodDecl), 
													 f.as<core::ExpressionPtr>(),
													 irClassType, 
													 core::FK_MEMBER_FUNCTION);
	}
 
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
	core::TypePtr retTy;
	core::CallExprPtr ret;
	if(f.isa<core::LambdaExprPtr>()) {
		retTy = newFunc.as<core::LambdaExprPtr>().getType().as<core::FunctionTypePtr>().getReturnType();
		ret  = builder.callExpr   (retTy, newFunc, args);
	} else {
		retTy = f.as<core::ExpressionPtr>().getType().as<core::FunctionTypePtr>().getReturnType();
		ret  = builder.callExpr (retTy, f, args);
	}
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

// TODO:  array constructor with no default initialization (CXX11)

	const CXXConstructorDecl* ctorDecl = callExpr->getConstructor();

	const clang::Type* classType= callExpr->getType().getTypePtr();
	core::TypePtr&& irClassType = convFact.convertType(classType);

	// it might be an array construction
	size_t numElements =0;
	if (irClassType->getNodeType() == core::NT_VectorType) {
		numElements = irClassType.as<core::VectorTypePtr>()->getSize().as<core::ConcreteIntTypeParamPtr>()->getValue();
		irClassType	= irClassType.as<core::VectorTypePtr>()->getElementType();
	}

	// to begin with we translate the constructor as a regular function but with initialization list
	auto f = convFact.convertFunctionDecl(ctorDecl);

	// update parameter list with a class-typed parameter in the first possition
	core::TypePtr&&  refToClassTy = builder.refType(irClassType);

	// reconstruct Arguments list, fist one is a scope location for the object
	core::ExpressionList args;
	args.push_back (builder.undefinedVar(refToClassTy));

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

	core::ExpressionPtr ctorFunc;
	if(!f.isa<core::LambdaExprPtr>()) { 
		//intercepted if !lambdaexpr
		ctorFunc = f; 
	} else {
		ctorFunc = convFact.memberize(llvm::cast<FunctionDecl>(ctorDecl), 
													 f.as<core::ExpressionPtr>(),
													 refToClassTy, 
													 core::FK_CONSTRUCTOR);
	}

	// build expression and we are done!!!
	core::ExpressionPtr ret;
	if (numElements){
		ret = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getVectorCtor(),
								mgr.getLangBasic().getRefVar(), ctorFunc, builder.getIntParamLiteral(numElements));
	}
	else{
		//single object constructor
		ret = builder.callExpr (refToClassTy, ctorFunc, args);
	}

	if (VLOG_IS_ON(2)){
		dumpPretty(ret);
	}
	END_LOG_EXPR_CONVERSION(ret);
	return ret;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX NEW CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXNewExpr(const clang::CXXNewExpr* callExpr) {
	START_LOG_EXPR_CONVERSION(callExpr);

	//TODO:  - inplace allocation - non default ctor array?
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

		// FIXME, array size
		retExp = builder.refNew(builder.refVar(placeHolder));
	}
	else{
		core::ExpressionPtr ctorCall = Visit(callExpr->getConstructExpr());
		assert(ctorCall.isa<core::CallExprPtr>() && "aint no constructor call in here, no way to translate NEW");

		core::TypePtr type = ctorCall->getType();
		core::ExpressionPtr newCall = builder.undefinedNew(type);
		
		if (callExpr->isArray()){
			core::ExpressionPtr arrSizeExpr = convFact.convertExpr( callExpr->getArraySize() );
 			arrSizeExpr =  builder.callExpr(mgr.getLangBasic().getTypeCast(), 
											arrSizeExpr,
											builder.getTypeLiteral(mgr.getLangBasic().getUInt8()) );

			// extract only the ctor function from the converted ctor call
			core::ExpressionPtr ctorFunc = ctorCall.as<core::CallExprPtr>().getFunctionExpr();

			retExp = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getArrayCtor(),
			 						   mgr.getLangBasic().getRefNew(), ctorFunc, arrSizeExpr);
		}
		else{

			// the basic constructor translation defines a stack variable as argument for the call
			// in order to turn this into a diynamic memory allocation, we only need to substitute 
			// the first argument for a heap location
			core::CallExprAddress addr(ctorCall.as<core::CallExprPtr>());
			retExp = core::transform::replaceNode (convFact.mgr, 
												  addr->getArgument(0), 
												  newCall ).as<core::CallExprPtr>();
		}
		retExp = builder.refVar(retExp);
	}

	END_LOG_EXPR_CONVERSION(retExp);
	return retExp;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX DELETE CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXDeleteExpr(const clang::CXXDeleteExpr* deleteExpr) {
	START_LOG_EXPR_CONVERSION(deleteExpr);

	if (deleteExpr->isArrayForm () )
		assert(false && "array delete not supported yet");

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


	 const clang::CXXTemporary* temp = bindTempExpr->getTemporary();

	 // we may visit the BindTemporaryExpr twice. Once in the temporary lookup and
	 // then when we visit the subexpr of the expression with cleanups. If this is the second time that we
	 // visit the expr do not create a new declaration statement and just return the previous one.
	 ConversionFactory::ConversionContext::TemporaryInitMap::const_iterator fit = convFact.ctx.tempInitMap.find(temp);
	 if (fit != convFact.ctx.tempInitMap.end()) {
		// variable found in the map
		return(fit->second.getVariable());
	 }

	 const clang::CXXDestructorDecl* dtorDecl = temp->getDestructor();
	 const clang::CXXRecordDecl* classDecl = dtorDecl->getParent();

	 core::TypePtr&& irType = convFact.convertType(classDecl->getTypeForDecl());


	 // create a new var for the temporary and initialize it with the inner expr IR
	 const clang::Expr * inner = bindTempExpr->getSubExpr();
	 core::ExpressionPtr body = convFact.convertExpr(inner);

	 core::DeclarationStmtPtr declStmt;

	  declStmt = convFact.builder.declarationStmt(convFact.builder.refType(irType),(body));

	 // store temporary and declaration stmt in Map
	 convFact.ctx.tempInitMap.insert(std::make_pair(temp,declStmt));

	 return declStmt.getVariable();

}

core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitExprWithCleanups(const clang::ExprWithCleanups* cleanupExpr) {

	// perform subtree traversal and get the temporaries that the cleanup expression creates
	std::vector<const clang::CXXTemporary*>&& tmps = utils::lookupTemporaries (cleanupExpr->getSubExpr ());

	// convert the subexpr to IR
	const Expr* inner = cleanupExpr->getSubExpr();
	core::ExpressionPtr innerIR = convFact.convertExpr(inner);

	vector<core::StatementPtr> stmtList;

	// for each of the temporaries(reverse) create an IR var decl and push it at the beginning of the
	// lambda body
	for (std::vector<const clang::CXXTemporary*>::reverse_iterator it = tmps.rbegin() ; it != tmps.rend(); ++it) {

		ConversionFactory::ConversionContext::TemporaryInitMap::const_iterator fit = convFact.ctx.tempInitMap.find(*it);
	    if (fit != convFact.ctx.tempInitMap.end()) {
		       // variable found in the map
		       stmtList.push_back(fit->second);
		}
	 }



	core::TypePtr lambdaType = convFact.convertType(cleanupExpr->getType().getTypePtr());
	stmtList.push_back(convFact.builder.returnStmt(innerIR));

	//build the lambda and its parameters

	core::StatementPtr&& lambdaBody = convFact.builder.compoundStmt(stmtList);
	vector<core::VariablePtr> params = core::analysis::getFreeVariables(lambdaBody);
	core::LambdaExprPtr lambda = convFact.builder.lambdaExpr(lambdaType, lambdaBody, params);


	//build the lambda call and its arguments
	vector<core::ExpressionPtr> packedArgs;

	std::for_each(params.begin(), params.end(), [&packedArgs] (core::VariablePtr varPtr) {
		 packedArgs.push_back(varPtr);
	});

	core::ExpressionPtr irNode = convFact.builder.callExpr(lambdaType, lambda, packedArgs);

	return irNode;
}


core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitMaterializeTemporaryExpr(
		const clang::MaterializeTemporaryExpr* materTempExpr) {

	return Visit(materTempExpr->GetTemporaryExpr());


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
