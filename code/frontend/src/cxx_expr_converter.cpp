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

// defines which are needed by LLVM
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include "clang/AST/StmtVisitor.h"
#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/CXXInheritance.h>

#include <clang/Basic/FileManager.h>
#pragma GCC diagnostic pop


#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/utils/temporaries_lookup.h"
#include "insieme/frontend/utils/cast_tool.h"
#include "insieme/frontend/utils/macros.h"

#include "insieme/frontend/utils/debug.h"

#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/utils/stmt_wrapper.h"


#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/ir_class_info.h"

#include "insieme/core/encoder/lists.h"
#include "insieme/core/annotations/source_location.h"

using namespace clang;
using namespace insieme;
using namespace exprutils;

namespace insieme {
namespace frontend {
namespace conversion {

//---------------------------------------------------------------------------------------------------------------------
//										CXX EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						  IMPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr) {
	core::ExpressionPtr retIr = ExprConverter::VisitImplicitCastExpr(castExpr);
	LOG_EXPR_CONVERSION(castExpr, retIr);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						EXPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr) {
	core::ExpressionPtr retIr = ExprConverter::VisitExplicitCastExpr(castExpr);
	LOG_EXPR_CONVERSION(castExpr, retIr);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							FUNCTION CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCallExpr(const clang::CallExpr* callExpr) {
	core::CallExprPtr irCall = ExprConverter::VisitCallExpr(callExpr).as<core::CallExprPtr>();
    LOG_EXPR_CONVERSION(callExpr, irCall);

	// if any of the parameters is an object, and is pass by value
	// Clang likes to implement a copy constructor, ignore it, it will be handled by the be compiler
	for (unsigned i=0; i<callExpr->getNumArgs(); i++){
		if (const clang::CXXConstructExpr* ctor = llvm::dyn_cast<clang::CXXConstructExpr>(callExpr->getArg(i))){
			// is a constructor, if is a copy ctor, we ignore it and return the origina object
			if(ctor->getConstructor()->isCopyConstructor()){
				core::ExpressionPtr tmp;
				// copy constructor has one argumnet (the object to be copied) -> index is 0
				const clang::DeclRefExpr* param= utils::skipSugar<DeclRefExpr> (ctor->getArg(0));
				if (param){

					tmp = convFact.lookUpVariable( param->getDecl() );
					if (!core::analysis::isAnyCppRef(tmp->getType())){
						tmp = convFact.tryDeref(tmp);
					}
					else{
						tmp = builder.toIRRef(tmp);
						tmp = convFact.tryDeref(tmp);
					}

					core::CallExprAddress addr(irCall);
					irCall = core::transform::replaceNode (mgr, addr->getArgument(i), tmp).as<core::CallExprPtr>();
				}
			}
		}
	}
	return irCall;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						  Unarty operator EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitUnaryOperator(const clang::UnaryOperator *unOp) {
	core::ExpressionPtr retIr;
    LOG_EXPR_CONVERSION(unOp, retIr);

	// member pointers are a spetial kind of expression that needs to be handled differently.
	// we do not retreieve the address of it since there is no address
	if ( unOp->getOpcode() == clang::UO_AddrOf) {
		core::ExpressionPtr&& subExpr = Visit(unOp->getSubExpr());
		if(	core::analysis:: isMemberPointer (subExpr->getType()))
			return subExpr;
	}

	return retIr = Converter::ExprConverter::VisitUnaryOperator (unOp);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						  MEMBER EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitMemberExpr(const clang::MemberExpr* membExpr){
	core::ExpressionPtr retIr;
    LOG_EXPR_CONVERSION(membExpr, retIr);

	// get the base we want to access to
	core::ExpressionPtr&& base = Visit(membExpr->getBase());
	if (core::analysis::isAnyCppRef(convFact.lookupTypeDetails(base->getType()))){
		base = builder.toIRRef(base);
	}

	// it might be that is a function, therefore we retrieve a callable expression
	const clang::ValueDecl *valDecl = membExpr->getMemberDecl ();
	if (valDecl && llvm::isa<clang::FunctionDecl>(valDecl)){
		return convFact.getCallableExpression(llvm::cast<clang::FunctionDecl>(valDecl));
	}

	retIr = getMemberAccessExpr(convFact, builder, base, membExpr);

	// if the  resulting expression is a ref to cpp ref, we remove one ref, no need to provide one extra ref
	if (retIr->getType().isa<core::RefTypePtr>() && core::analysis::isAnyCppRef(retIr->getType().as<core::RefTypePtr>()->getElementType()))
		retIr = builder.deref(retIr);

	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							VAR DECLARATION REFERENCE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitDeclRefExpr(const clang::DeclRefExpr* declRef) {
	core::ExpressionPtr retIr;
    LOG_EXPR_CONVERSION(declRef, retIr);

	// if is a prameter and is a cpp ref, avoid going further to avoid wrapping issues
	if (const ParmVarDecl* parmDecl = dyn_cast<ParmVarDecl>(declRef->getDecl())) {
		retIr = convFact.lookUpVariable( parmDecl );
		if (core::analysis::isAnyCppRef(retIr->getType())){
			return retIr;
		}
	}

	if (const clang::FieldDecl* field = llvm::dyn_cast<clang::FieldDecl>(declRef->getDecl() ) ) {
		// this is the direct access to a member field in a generic way: something like Obj::a
		core::TypePtr classTy = convFact.convertType(field->getParent()->getTypeForDecl()->getCanonicalTypeInternal());
		core::TypePtr membType = convFact.convertType(declRef->getType());
    	return retIr = core::analysis::getMemberPointerValue(classTy, field->getNameAsString(), membType);
	}

	return retIr = Converter::ExprConverter::VisitDeclRefExpr (declRef);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								CXX BOOLEAN LITERAL
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXBoolLiteralExpr(const clang::CXXBoolLiteralExpr* boolLit) {
	core::ExpressionPtr retExpr =
			convFact.builder.literal(
					(boolLit->getValue())? std::string("true"): std::string("false"),
					convFact.mgr.getLangBasic().getBool());

	LOG_EXPR_CONVERSION(boolLit, retExpr);
	return retExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX MEMBER CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXMemberCallExpr(const clang::CXXMemberCallExpr* callExpr) {
	core::ExpressionPtr ret;
	LOG_EXPR_CONVERSION(callExpr, ret);
	// TODO: static methods

	const core::IRBuilder& builder = convFact.builder;
	const CXXMethodDecl* methodDecl = callExpr->getMethodDecl();

	if (!methodDecl){
		// no method declaration... this is call to something else.
		// what else is callable??? just a pointer,
		//  INTRODUCING: member function pointer!

		// now we have the function to call, we create the call expression with the right paramenters
		// the member pointer executor operator will return to us a fucntion call with the "this" parameter
		// as first param
		frontend_assert(callExpr->getCallee ());
		core::CallExprPtr inner = convFact.convertExpr(callExpr->getCallee()).as<core::CallExprPtr>();
		ExpressionList&& args = ExprConverter::getFunctionArguments(callExpr, inner->getFunctionExpr()->getType().as<core::FunctionTypePtr>());
		core::ExpressionPtr thisArg =  inner->getArgument(0);
		args.insert (args.begin(), thisArg);

		ret = builder.callExpr( inner->getType() , inner->getFunctionExpr(), args);
	}
	else{
		// to begin with we translate the constructor as a regular function
		auto func = convFact.getCallableExpression(llvm::cast<clang::FunctionDecl> (methodDecl));
		core::FunctionTypePtr funcTy = func.getType().as<core::FunctionTypePtr>();

		// get the this-Object
		core::ExpressionPtr ownerObj = Visit(callExpr->getImplicitObjectArgument());
		// correct the owner object reference, in case of pointer (ref<array<struct<...>,1>>) we need to
		// index the first element
		ownerObj = getCArrayElemRef(builder, ownerObj);
		if (core::analysis::isAnyCppRef(convFact.lookupTypeDetails(ownerObj->getType())))
			ownerObj = builder.toIRRef(ownerObj);

		// if owner object is not a ref is the case of a call on a return value which has not
		// being identified as temporary expression, because in IR classes are always a left side
		// we have to materialize
		if(!ownerObj.getType().isa<core::RefTypePtr>()){
			ownerObj =  builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), ownerObj);
		}

		// reconstruct Arguments list, fist one is a scope location for the object
		ExpressionList&& args = ExprConverter::getFunctionArguments(callExpr, llvm::cast<clang::FunctionDecl>(methodDecl) );
		args.insert (args.begin(), ownerObj);
		core::TypePtr retTy = funcTy.getReturnType();

		// build expression and we are done!!!
		ret = builder.callExpr(retTy, func, args);

	}
	if(VLOG_IS_ON(2)){
		dumpPretty(&(*ret));
	}
	return ret;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX OPERATOR CALL EXPRESSION
//
//  A call to an overloaded operator written using operator syntax.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXOperatorCallExpr(const clang::CXXOperatorCallExpr* callExpr) {
	core::ExpressionPtr retIr;
    LOG_EXPR_CONVERSION(callExpr, retIr);
	core::ExpressionPtr func;
	core::ExpressionPtr convertedOp;
	ExpressionList args;
	core::FunctionTypePtr funcTy;

	if( const clang::CXXMethodDecl* methodDecl = llvm::dyn_cast<clang::CXXMethodDecl>(callExpr->getCalleeDecl()) ) {
		//operator defined as member function
		VLOG(2) << "Operator defined as member function "
				<< methodDecl->getParent()->getNameAsString() << "::"
				<< methodDecl->getNameAsString();


		convertedOp =  convFact.getCallableExpression(methodDecl);

		// possible member operators: +,-,*,/,%,^,&,|,~,!,<,>,+=,-=,*=,/=,%=,^=,&=,|=,<<,>>,>>=,<<=,==,!=,<=,>=,&&,||,++,--,','
		// overloaded only as member function: '=', '->', '()', '[]', '->*', 'new', 'new[]', 'delete', 'delete[]'
		//unary:	X::operator@();	left == CallExpr->arg(0) == "this"
		//binary:	X::operator@( right==arg(1) ); left == CallExpr->arg(0) == "this"
		//else functioncall: ():		X::operator@( right==arg(1), args ); left == CallExpr->arg(0) == "this"

		// get "this-object"
		core::ExpressionPtr ownerObj = convFact.convertExpr(callExpr->getArg(0));

		if (core::analysis::isAnyCppRef(ownerObj->getType()))
			ownerObj = builder.toIRRef(ownerObj);

		// get arguments
		funcTy = convertedOp.getType().as<core::FunctionTypePtr>();
		args = getFunctionArguments(callExpr, funcTy);

		//  the problem is, we call a memeber function over a value, the owner MUST be always a ref,
		//  is not a expression with cleanups because this object has not need to to be destucted,
		//  no used defined dtor.
		// some constructions might return an instance, incorporate a materialize
		if (!ownerObj->getType().isa<core::RefTypePtr>()){
			ownerObj =  builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), ownerObj);
		}

		// incorporate this to the begining of the args list
		args.insert (args.begin(), ownerObj);
	}
	else if(const clang::FunctionDecl* funcDecl = llvm::dyn_cast<clang::FunctionDecl>(callExpr->getCalleeDecl()) ) {
		// operator defined as non-member function
		VLOG(2) << "Operator defined as non-member function " << funcDecl->getNameAsString();

		// possible non-member operators:
		// unary:	operator@( left==arg(0) )
		// binary:	operator@( left==arg(0), right==arg(1) )

		convertedOp = convFact.convertExpr(callExpr->getCallee());

		funcTy = convertedOp.getType().as<core::FunctionTypePtr>();
		args = getFunctionArguments(callExpr, funcDecl);
	}

	retIr = builder.callExpr(funcTy->getReturnType(), convertedOp, args);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX CONSTRUCTOR CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXConstructExpr(const clang::CXXConstructExpr* callExpr) {

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(callExpr, retIr);
	const core::IRBuilder& builder = convFact.builder;

// TODO:  array constructor with no default initialization (CXX11)
    const CXXConstructorDecl* ctorDecl = callExpr->getConstructor();
	core::TypePtr&& irClassType = convFact.convertType(callExpr->getType());
	core::TypePtr&&  refToClassTy = builder.refType(irClassType);

	// it might be an array construction
	size_t numElements =0;
	if (irClassType->getNodeType() == core::NT_VectorType) {
		numElements = irClassType.as<core::VectorTypePtr>()->getSize().as<core::ConcreteIntTypeParamPtr>()->getValue();
		irClassType	= irClassType.as<core::VectorTypePtr>()->getElementType();
	}

	// we do NOT instantiate elidable ctors, this will be generated and ignored if needed by the
	// back end compiler, unleast there are more than one parameter, this case we have no way to express the
	// sematincs in IR. the constructor will unify those expressions into one
	if (callExpr->isElidable () && (callExpr->getNumArgs() == 1)){

		// if is an elidable constructor, we should return a refvar, not what the parameters say
		retIr = (Visit(callExpr->getArg (0)));
		if (core::analysis::isCallOf(retIr, mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize()))
			retIr = builder.refVar(retIr.as<core::CallExprPtr>()->getArgument(0));

		// a constructor returns a reference of the class type, but we might need to fix types
		// do a ref reinterpret to the target type
		if (gen.isRef(retIr->getType()) ) {
			if( retIr->getType() != refToClassTy) {
				retIr = builder.deref(builder.callExpr(refToClassTy, gen.getRefReinterpret(), retIr, builder.getTypeLiteral(irClassType)));
			}
		} else {
			//carefull we have to handle const variable
			if( retIr->getType() != irClassType) {
				//carefull we have to handle const variable
				retIr = utils::cast(retIr, irClassType);
			}
		}

		return retIr;
	}

	if( !ctorDecl->isUserProvided() ) {
		if(	ctorDecl->isDefaultConstructor() ) {
			//TODO find better solution to sovle problems with standard-layout/trivial-copyable
			//classes
			//if it is a POD we do not call ctor, we just
			//instantiate enough memory to hold the structure
			if(ctorDecl->getParent()->isPOD()) {
				if (numElements)
					return (retIr = builder.callExpr(
							builder.getLangBasic().getVectorInitUniform(),
							builder.undefinedVar(irClassType),
							builder.getIntParamLiteral(numElements)
						));
				else {
					retIr = builder.undefinedVar(refToClassTy);
					return retIr;
				}
			} else {
				VLOG(2) << "HERE";
			    //if not POD we are forced to call the constructor
			    core::ExpressionPtr ctor = core::analysis::createDefaultConstructor(irClassType);
				//return (retIr = (builder.callExpr(refToClassTy, ctor, builder.undefinedVar(refToClassTy))));
			}
		}
		else if( ctorDecl->isCopyConstructor() && ctorDecl->getParent()->isPOD() ) {
			//if not userprovided we don't need to add a constructor just create the object to work
			//with -- for the rest the BE-compiler takes care of
			return (retIr = Visit(callExpr->getArg(0)));
		}
	}

	// to begin with we translate the constructor as a regular function but with initialization list
	core::ExpressionPtr ctorFunc = convFact.getCallableExpression(ctorDecl);

	// update parameter list with a class-typed parameter in the first possition
	core::FunctionTypePtr funcTy = ctorFunc.getType().as<core::FunctionTypePtr>();

	// reconstruct Arguments list, fist one is a scope location for the object
	ExpressionList&& args = ExprConverter::getFunctionArguments(callExpr, llvm::cast<clang::FunctionDecl>(ctorDecl));

	// first paramenter is the memory storage, the this location
	args.insert (args.begin(), builder.undefinedVar(refToClassTy));

	// build expression and we are done!!!
	if (numElements){
		retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getVectorCtor(),
								mgr.getLangBasic().getRefVar(), ctorFunc, builder.getIntParamLiteral(numElements));
	} else{
		//single object constructor
		retIr = builder.callExpr (funcTy.getReturnType(), ctorFunc, args);
	}

	if (VLOG_IS_ON(2)){
		dumpPretty(retIr);
	}

    frontend_assert(retIr) << "ConstructExpr could not be translated\n";
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX NEW CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXNewExpr(const clang::CXXNewExpr* callExpr) {
	//TODO:  - inplace allocation - non default ctor array?
	core::ExpressionPtr retExpr;
	LOG_EXPR_CONVERSION(callExpr, retExpr);

	// if no constructor is found, it is a new over an non-class type, can be any kind of pointer of array
	// spetialy double pointer
	if (!callExpr->getConstructExpr() ){

		core::TypePtr type = convFact.convertType(callExpr->getAllocatedType());
		core::ExpressionPtr placeHolder;
		if(callExpr->hasInitializer()) {
            const clang::Expr * initializer = callExpr->getInitializer();
		    core::ExpressionPtr initializerExpr = convFact.convertExpr(initializer);
			frontend_assert(initializerExpr);
            placeHolder = initializerExpr;
		}
        else {
            placeHolder  = builder.undefinedNew(type);
        }

		if (callExpr->isArray()){
			core::ExpressionPtr&& arrSizeExpr = convFact.convertExpr( callExpr->getArraySize() );
			placeHolder = builder.callExpr( builder.arrayType(type),
											builder.getLangBasic().getArrayCreate1D(),
											builder.getTypeLiteral(type),
											utils::cast(arrSizeExpr, gen.getUInt4()));
			retExpr = builder.refNew(placeHolder);
		} else {
			retExpr = builder.callExpr(builder.getLangBasic().getScalarToArray(), builder.refNew(placeHolder));
		}
	}
	else{
		//assert(callExpr->getConstructExpr() && "class need to have Constructor of any kind");
		// is a class, handle construction
		core::ExpressionPtr ctorCall = Visit(callExpr->getConstructExpr());
		if(ctorCall.isa<core::VariablePtr>()) {
			//we know that the constructor call delivered a variable
			//lets check if we can find it in the tempInit map
			if (llvm::isa<clang::MaterializeTemporaryExpr>(callExpr->getConstructExpr()->getArg(0))) {
				const clang::MaterializeTemporaryExpr * exp = llvm::cast<clang::MaterializeTemporaryExpr>(callExpr->getConstructExpr()->getArg(0));
				if(llvm::isa<clang::CXXBindTemporaryExpr>(exp->GetTemporaryExpr()->IgnoreImpCasts())) {
					clang::CXXTemporary * tmp = llvm::cast<clang::CXXBindTemporaryExpr>(exp->GetTemporaryExpr()->IgnoreImpCasts())->getTemporary();
					Converter::TemporaryInitMap::const_iterator fit = convFact.tempInitMap.find(tmp);
					if(fit != convFact.tempInitMap.end())
						ctorCall = (fit->second.getInitialization());
				}
			}
		}
		frontend_assert(ctorCall.isa<core::CallExprPtr>()) << "aint no constructor call in here, no way to translate NEW\n";

		core::TypePtr type = ctorCall->getType();
		core::ExpressionPtr newCall = builder.undefinedNew(type);

		if (callExpr->isArray()){
			core::ExpressionPtr arrSizeExpr = convFact.convertExpr( callExpr->getArraySize() );
			arrSizeExpr = utils::castScalar(mgr.getLangBasic().getUInt8(), arrSizeExpr);

			// extract only the ctor function from the converted ctor call
			core::ExpressionPtr ctorFunc = ctorCall.as<core::CallExprPtr>().getFunctionExpr();

			frontend_assert( ctorFunc->getType().as<core::FunctionTypePtr>()->getParameterTypes().size()) << "not default ctor used in array construction\n";

			retExpr = ( builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getArrayCtor(),
			 						   				   mgr.getLangBasic().getRefNew(), ctorFunc, arrSizeExpr));
		}
		else{
			// the basic constructor translation defines a stack variable as argument for the call
			// in order to turn this into a diynamic memory allocation, we only need to substitute
			// the first argument for a heap location
			core::CallExprAddress addr(ctorCall.as<core::CallExprPtr>());
			if(insieme::core::analysis::isConstructorCall(ctorCall)) {
				VLOG(2) << addr->getArgument(0).as<core::CallExprPtr>();
				retExpr = core::transform::replaceNode (convFact.mgr,
													addr->getArgument(0),
													newCall ).as<core::CallExprPtr>();

				retExpr = builder.callExpr(builder.getLangBasic().getScalarToArray(), retExpr);
			}
			else {
				//if constructor of is NOT userprovided we get back the "plain" object, no wrapping
				//ctor to take care of for the exchange of "refVar" with "newCall"
				if(!callExpr->getConstructExpr()->getConstructor()->isUserProvided()) {
                    VLOG(2) << addr.as<core::CallExprPtr>();
                    retExpr = core::transform::replaceNode (convFact.mgr,
                                                            addr,
                                                            newCall ).as<core::CallExprPtr>();

                    retExpr = builder.callExpr(builder.getLangBasic().getScalarToArray(), retExpr);
				} else {
                    retExpr = builder.callExpr(builder.getLangBasic().getScalarToArray(), builder.refNew(addr->getArgument(0)));
				}
			}
		}
	}

	return retExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX DELETE CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXDeleteExpr(const clang::CXXDeleteExpr* deleteExpr) {
	core::ExpressionPtr retExpr;
	LOG_EXPR_CONVERSION(deleteExpr, retExpr);

	// convert the target of our delete expr
	core::ExpressionPtr exprToDelete = Visit(deleteExpr->getArgument());

	core::ExpressionPtr dtor;
	// since destructor might be defined in a different translation unit or even in this one but after the usage
	// we should retrieve a callable symbol and delay the conversion
	if (const clang::TagType* record = llvm::dyn_cast<clang::TagType>(deleteExpr->getDestroyedType().getTypePtr())){
		if (const clang::CXXRecordDecl* classDecl = llvm::dyn_cast<clang::CXXRecordDecl>(record->getDecl())){
			if ( classDecl->getDestructor())
				dtor = convFact.getCallableExpression(classDecl->getDestructor());
		}
	}

	if (deleteExpr->isArrayForm () ){

		// we need to call arratDtor, with the object, refdelete and the dtorFunc
		if(dtor){

			//FIXME: why mem_alloc dtor has being marked as virtual????
			core::TypePtr desTy = convFact.convertType( deleteExpr->getDestroyedType());
			desTy = convFact.lookupTypeDetails(desTy);
			frontend_assert(!core::getMetaInfo(desTy).isDestructorVirtual()) << "no virtual dtor allowed for array dtor\n";

			std::vector<core::ExpressionPtr> args;
			args.push_back(exprToDelete);
			args.push_back( builder.getLangBasic().getRefDelete());
			args.push_back( dtor);
			retExpr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getArrayDtor(), args);
		}
		else{
			exprToDelete = getCArrayElemRef(builder, exprToDelete);
			VLOG(2) << exprToDelete->getType();

			// this is a built in type, we need to build a empty dtor with the right type
			retExpr = builder.callExpr ( builder.getLangBasic().getRefDelete(), exprToDelete);
		}
	}
	else{
		exprToDelete = getCArrayElemRef(builder, exprToDelete);
		VLOG(2) << exprToDelete->getType();

		if(dtor){
			retExpr = builder.callExpr ( builder.getLangBasic().getRefDelete(), builder.callExpr(dtor, toVector(exprToDelete)));
		}
		else{
			retExpr = builder.callExpr ( builder.getLangBasic().getRefDelete(), exprToDelete);
		}
	}

	return retExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX THIS CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXThisExpr(const clang::CXXThisExpr* thisExpr) {
	//figure out the type of the expression
	core::TypePtr&& irType = convFact.convertType( llvm::cast<clang::TypeDecl>(thisExpr->getBestDynamicClassType())->getTypeForDecl()->getCanonicalTypeInternal());
	frontend_assert(irType.isa<core::GenericTypePtr>() ) << "for convention, all this operators deal with generic types\n";
	irType = builder.refType(irType);

	// build a variable as a placeholder (has to be substituted later by function call expression)
	// convFact.thisVariable returns alwasy a variable with id 0 as placeholder!
	core::ExpressionPtr ret = convFact.thisVariable(irType);

	// this is a pointer, make it pointer
	ret =  builder.callExpr(builder.getLangBasic().getScalarToArray(), ret);

	LOG_EXPR_CONVERSION(thisExpr, ret);
	return ret;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					EXCEPTION CXX THROW EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXThrowExpr(const clang::CXXThrowExpr* throwExpr) {

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(throwExpr, retIr);

	core::ExpressionPtr subExpr;
	core::TypePtr       targetTy;
	if(throwExpr->getSubExpr()){
		subExpr = Visit(throwExpr->getSubExpr());
		targetTy = convFact.convertType(throwExpr->getSubExpr()->getType());
		core::TypePtr srcTy = subExpr->getType();
		if(targetTy != srcTy) {
			subExpr = convFact.tryDeref(subExpr);
		}
	}
	else{
		// a throw without expression rethrows the expression captured in the container catch.
		// if no expressesion, calls terminate(). but we delegate this to the output compiler
		targetTy = gen.getUnit();
		subExpr = builder.literal("__insieme__rethrow", targetTy);
	}

	return retIr = convFact.createCallExprFromBody(builder.throwStmt(subExpr), gen.getUnit());
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					CXX DEFAULT ARG EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXDefaultArgExpr(const clang::CXXDefaultArgExpr* defaultArgExpr) {
	//START_LOG_EXPR_CONVERSION(defaultArgExpr);
	auto ret = Visit(defaultArgExpr->getExpr());
	LOG_EXPR_CONVERSION(defaultArgExpr, ret);
	return ret;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					CXX Bind Temporary expr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXBindTemporaryExpr(const clang::CXXBindTemporaryExpr* bindTempExpr) {
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(bindTempExpr, retIr);

	const clang::CXXTemporary* temp = bindTempExpr->getTemporary();

	// we may visit the BindTemporaryExpr twice. Once in the temporary lookup and
	// then when we visit the subexpr of the expression with cleanups. If this is the second time that we
	// visit the expr do not create a new declaration statement and just return the previous one.
	Converter::TemporaryInitMap::const_iterator fit = convFact.tempInitMap.find(temp);
	if (fit != convFact.tempInitMap.end()) {
		// variable found in the map
		return(fit->second.getVariable());
	}

	const clang::CXXDestructorDecl* dtorDecl = temp->getDestructor();
	const clang::CXXRecordDecl* classDecl = dtorDecl->getParent();

	core::TypePtr&& irType = convFact.convertType(classDecl->getTypeForDecl()->getCanonicalTypeInternal());

	// create a new var for the temporary and initialize it with the inner expr IR
	const clang::Expr * inner = bindTempExpr->getSubExpr();
	core::ExpressionPtr body = convFact.convertExpr(inner);
	if (!gen.isRef(body->getType()))
		body = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), body);

	core::DeclarationStmtPtr declStmt;
	declStmt = convFact.builder.declarationStmt(convFact.builder.refType(irType),(body));

	// store temporary and declaration stmt in Map
	convFact.tempInitMap.insert(std::make_pair(temp,declStmt));
	return retIr = declStmt.getVariable();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					CXX Expression with cleanups
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitExprWithCleanups(const clang::ExprWithCleanups* cleanupExpr) {
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(cleanupExpr, retIr);

	// perform subtree traversal and get the temporaries that the cleanup expression creates
	std::vector<const clang::CXXTemporary*>&& tmps = utils::lookupTemporaries (cleanupExpr->getSubExpr ());

	// convert the subexpr to IR
	const clang::Expr* inner = cleanupExpr->getSubExpr();
	core::ExpressionPtr innerIr = convFact.convertExpr(inner);

	// for each of the temporaries create an IR var decl and push it at the beginning of the
	// lambda body
	vector<core::StatementPtr> stmtList;
	for (std::vector<const clang::CXXTemporary*>::iterator it = tmps.begin() ; it != tmps.end(); ++it) {

		Converter::TemporaryInitMap::const_iterator fit = convFact.tempInitMap.find(*it);
	    if (fit != convFact.tempInitMap.end()) {
			// if the cleanup obj is a const_ref, we dont need the cleanup expr
			core::DeclarationStmtPtr decl = fit->second.as<core::DeclarationStmtPtr>();
			core::VariablePtr        var  = decl->getVariable();
			core::ExpressionPtr      init = decl->getInitialization();

			VLOG(2) << " expr: " << innerIr;
			VLOG(2) << " cleanup: " << var << " (type: " << var->getType() << ")  init: " << init << std::endl;
			stmtList.insert(stmtList.begin(),decl);  // insert with reverse order
		}
	}

	core::TypePtr lambdaRetType = convFact.convertType(cleanupExpr->getType());

	if (innerIr->getType() != lambdaRetType && !gen.isRef(lambdaRetType)){
		if (core::analysis::isCallOf(innerIr, mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize()))
			innerIr = innerIr.as<core::CallExprPtr>().getArgument(0);
		else {
            if(core::analysis::isAnyCppRef(innerIr->getType()))
                innerIr = core::analysis::unwrapCppRef(innerIr);

            innerIr = convFact.tryDeref(innerIr);
		}
	}

	if (stmtList.empty()){
		// we avoided all expressions to be cleanup, no extra lambda needed
		VLOG(2) << "	cleanup expression is simplyfied and avoided";
		return retIr = innerIr;
	}

	// if the expression does not return anything, do not add return stmt
	if (gen.isUnit(innerIr->getType())){
		stmtList.push_back(innerIr);
	}
	else{
		if (core::encoder::isListType(innerIr->getType())) {
			vector<core::ExpressionPtr> retList = core::encoder::toValue<vector<core::ExpressionPtr>>(innerIr);
			if (core::encoder::isListType(retList[0]->getType())) 
				retList = core::encoder::toValue<vector<core::ExpressionPtr>>(retList[0]);
			for (core::ExpressionPtr& expr : retList){
				expr = builder.deref(expr);
			}

			innerIr = builder.callExpr (lambdaRetType, mgr.getLangBasic().getGenInit(), builder.getTypeLiteral(lambdaRetType),  builder.tupleExpr(retList));
		}
		stmtList.push_back(convFact.builder.returnStmt(innerIr));
	}

	return retIr =  convFact.createCallExprFromBody(stmtutils::StmtWrapper(stmtList), lambdaRetType);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					ScalarValueInitExpr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXScalarValueInitExpr(const clang::CXXScalarValueInitExpr* scalarValueInit){
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(scalarValueInit, retIr);

	core::TypePtr elemType =convFact.convertType ( scalarValueInit->getTypeSourceInfo()->getType());
	retIr = convFact.defaultInitVal(elemType);
	return retIr;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					Materialize temporary expr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitMaterializeTemporaryExpr( const clang::MaterializeTemporaryExpr* materTempExpr) {
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(materTempExpr, retIr);
	retIr =  Visit(materTempExpr->GetTemporaryExpr());

	if(VLOG_IS_ON(2)){
		std::cout << " =============== Materialize! =================" << std::endl;
		materTempExpr->dump();
		std::cout << "inner: " << std::endl;
		dumpPretty(retIr);
		std::cout << "type: " << std::endl;
		dumpPretty(retIr->getType());
		std::cout << "expected: " << std::endl;
		core::TypePtr t = convFact.convertType(materTempExpr->getType());
		dumpPretty(t);
	}

	//if (! t.isa<core::RefTypePtr>())
	//	return retIr;

	// if inner expression is a bind temporary, we do not need to materialize, is IR-correct
	if (llvm::isa<clang::CXXBindTemporaryExpr>(materTempExpr->GetTemporaryExpr()))
		return retIr;
	if (llvm::isa<clang::CXXNewExpr>(materTempExpr->GetTemporaryExpr()))
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), retIr));

	// inner type is a pointer? materialize
	if((retIr->getType().isa<core::RefTypePtr>()) && (retIr->getType().as<core::RefTypePtr>()->getElementType()->getNodeType() == core::NT_ArrayType))
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), retIr));



	if(core::analysis::isAnyCppRef(retIr->getType()) || gen.isRef(retIr->getType()))
		return retIr;
	else
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), retIr));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					Typeid expr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitCXXTypeidExpr(const clang::CXXTypeidExpr* typeidExpr) {
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(typeidExpr, retIr);
	//auto retTy = builder.refType(convFact.convertType(typeidExpr->getType().getTypePtr()));
	core::ExpressionPtr expr;
	if(typeidExpr->isTypeOperand()) {
		expr = builder.getTypeLiteral(convFact.convertType(typeidExpr->getTypeOperand(convFact.getCompiler().getASTContext())));
	} else {
		expr = Visit(typeidExpr->getExprOperand());
	}
	retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getTypeid(), expr);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//			Substituted non type template parameter expression
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitSubstNonTypeTemplateParmExpr(const clang::SubstNonTypeTemplateParmExpr* substExpr) {
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(substExpr, retIr);
	frontend_assert(substExpr->getReplacement()) << "template parameter cannot be substituted by nothing\n";
	retIr = Visit(substExpr->getReplacement());
	return retIr;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//			memeber function pointer executors
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

namespace {

	core::ExpressionPtr convertMemberFuncExecutor (const clang::BinaryOperator* clangExpr,  const frontend::conversion::Converter& convFact){
		core::ExpressionPtr papa    = convFact.convertExpr(clangExpr->getLHS());
		core::ExpressionPtr trgPtr = convFact.convertExpr(clangExpr->getRHS());

		// unwrap pointer kind
		if (clangExpr->getOpcode() == clang::BO_PtrMemI)
			papa = getCArrayElemRef(convFact.getIRBuilder(), papa);

		// check whenever function of member data pointer
		if (trgPtr->getType().isa<core::FunctionTypePtr>())
			return convFact.getIRBuilder().callExpr(trgPtr->getType().as<core::FunctionTypePtr>()->getReturnType(), trgPtr, toVector(papa));
		else{
			frontend_assert(core::analysis::isMemberPointer (trgPtr->getType()) ) << " not a memberPointer? " << trgPtr << " : " << trgPtr->getType();
			return core::analysis::getMemberPointerAccess(papa,trgPtr);
		}
	}

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//			binary member pointer Direct
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitBinPtrMemD(const clang::BinaryOperator* exprD) {
	// direct, ->*
	return convertMemberFuncExecutor(exprD,convFact);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//			binary member pointer indirect
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitBinPtrMemI(const clang::BinaryOperator* exprI) {
	// indirect, ->*
	return convertMemberFuncExecutor(exprI,convFact);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//		binary trait
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitBinaryTypeTraitExpr		(const clang::BinaryTypeTraitExpr* binTypeTraitExpr){

	// this is found when using __base_of operator, clang gives already the boolean expression evaluated,
	// is an static type resolutions, we just forward this result
	core::ExpressionPtr retExpr =
			convFact.builder.literal(
					(binTypeTraitExpr->getValue())? std::string("true"): std::string("false"),
					convFact.mgr.getLangBasic().getBool());

	LOG_EXPR_CONVERSION(binTypeTraitExpr, retExpr);
	return retExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//		SizeOfPack expr
//		basically a cpp11 feature but tends to end up also in cpp03
//		DUPLICATED INTO CPP11extension
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::VisitSizeOfPackExpr(const clang::SizeOfPackExpr* sizeOfPackExpr) {
	convFact.warnings.insert("SizeOfPack -(sizeof...) is supported from c++11 on");
	//sizeOf... returns size_t --> use unsigned int
	core::ExpressionPtr retExpr = builder.uintLit(sizeOfPackExpr->getPackLength());
	LOG_EXPR_CONVERSION(sizeOfPackExpr, retExpr);
	return retExpr;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr Converter::CXXExprConverter::Visit(const clang::Expr* expr) {

	//iterate clang handler list and check if a handler wants to convert the expr
	core::ExpressionPtr retIr;
	//call frontend plugin visitors
	for(auto plugin : convFact.getConversionSetup().getPlugins()) {
		retIr = plugin->Visit(expr, convFact);
		if(retIr)
			break;
    }
    if(!retIr){
		convFact.trackSourceLocation(expr);
        retIr = ConstStmtVisitor<Converter::CXXExprConverter, core::ExpressionPtr>::Visit(expr);
		convFact.untrackSourceLocation();
	}

	// print diagnosis messages
	convFact.printDiagnosis(expr->getLocStart());

    // call frontend plugin post visitors
	for(auto plugin : convFact.getConversionSetup().getPlugins()) {
        retIr = plugin->PostVisit(expr, retIr, convFact);
	}

	// attach location annotation
	if (expr->getLocStart().isValid()){
		auto presStart =  convFact.getSourceManager().getPresumedLoc(expr->getLocStart());
		auto presEnd =  convFact.getSourceManager().getPresumedLoc(expr->getLocEnd());
		core::annotations::attachLocation(retIr, std::string (presStart.getFilename()), presStart.getLine(), presStart.getColumn(), presEnd.getLine(), presEnd.getColumn());
	}

	// check for OpenMP annotations
	return omp::attachOmpAnnotation(retIr, expr, convFact);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
