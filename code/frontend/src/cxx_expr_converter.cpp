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
#include "insieme/frontend/utils/ir++_utils.h"

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
#include "insieme/core/ir_class_info.h"


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
		case CK_LValueToRValue:
			{
				// this is CppRef -> ref
				core::ExpressionPtr expr = Visit(castExpr->getSubExpr ());
				if (core::analysis::isCppRef(expr->getType())){
				// unwrap and deref the variable
					return builder.deref( builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), expr));
				}
				else if (core::analysis::isConstCppRef(expr->getType())){
					return builder.deref( builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), expr));
				}

				// if not, fallthrow default case
			}
		case CK_NoOp:
			{
				core::ExpressionPtr expr = Visit(castExpr->getSubExpr ());
				core::TypePtr type = expr->getType();
				if (core::analysis::isCppRef(type)){
					dumpDetail (type);
					return builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToConstCpp(), expr);
				}
				// FIXME: it might be the cast from any left side to a cpp const reference,

				// do no break, otherwhise continue to default
			}

		default:
			// cast which should look like C 
			{
				retIr = ExprConverter::VisitImplicitCastExpr(castExpr);
				break;
			}
	}
	END_LOG_EXPR_CONVERSION(retIr);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						EXPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr) {
// FIXME: all CPP casts

	if (castExpr->getCastKind() == CK_NoOp) {
		core::ExpressionPtr&& exp = Visit(castExpr->getSubExpr ());
		return exp;
	}


	return (ExprConverter::VisitExplicitCastExpr(castExpr));
/*
	START_LOG_EXPR_CONVERSION(castExpr);

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
*/
	assert(false); 
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
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitMemberExpr(const clang::MemberExpr* membExpr){
	START_LOG_EXPR_CONVERSION(membExpr);

	// get the base we want to access to
	core::ExpressionPtr&& base = Visit(membExpr->getBase());

	// if is not a pointer member, it might be that is a CPP ref
	core::TypePtr irType = base->getType();
	if (core::analysis::isCppRef(irType)) {
		base = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), base);
	}
	else if (core::analysis::isConstCppRef(irType)) {
		base = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), base);
	}

	// TODO: we have the situation here in which we might want to access a field of a superclass
	// this will not be resolved by the C frontend. and we need to build the right datapath to
	// reach the definition

	core::ExpressionPtr retIr = exprutils::getMemberAccessExpr(builder, base, membExpr);

	END_LOG_EXPR_CONVERSION(retIr);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							VAR DECLARATION REFERENCE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitDeclRefExpr(const clang::DeclRefExpr* declRef) {
	// if is a prameter and is a cpp ref, avoid going further to avoid wrapping issues
	if (const ParmVarDecl* parmDecl = dyn_cast<ParmVarDecl>(declRef->getDecl())) {
		core::ExpressionPtr retIr = convFact.lookUpVariable( parmDecl );
		if (IS_CPP_REF_EXPR(retIr)){
			return retIr;
		}
	}

	return ConversionFactory::ExprConverter::VisitDeclRefExpr (declRef);
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
	auto newFunc = convFact.convertFunctionDecl(llvm::cast<clang::FunctionDecl> (methodDecl), false).as<core::ExpressionPtr>();

	// get type of this
	const clang::Type* classType= methodDecl->getParent()->getTypeForDecl();
	core::TypePtr&& irClassType = builder.refType( convFact.convertType(classType) );

	newFunc = convFact.memberize(llvm::cast<FunctionDecl>(methodDecl), 
									newFunc.as<core::ExpressionPtr>(),
									irClassType, 
									core::FK_MEMBER_FUNCTION);

	core::FunctionTypePtr funcTy = newFunc.getType().as<core::FunctionTypePtr>();
 
	// get the this-Object
	core::ExpressionPtr ownerObj = Visit(callExpr->getImplicitObjectArgument());
	// correct the owner object reference, in case of pointer (ref<array<struct<...>,1>>) we need to
	// index the first element
	ownerObj = getCArrayElemRef(builder, ownerObj);

	// reconstruct Arguments list, fist one is a scope location for the object
	ExpressionList&& args = ExprConverter::getFunctionArguments(builder, callExpr, funcTy);
	args.insert (args.begin(), ownerObj);

	// append globalVar to arguments if needed
	if( ctx.globalFuncSet.find(methodDecl) != ctx.globalFuncSet.end() ) {
		args.push_back(ctx.globalVar);
	}

	core::TypePtr retTy = funcTy.getReturnType();

	// build expression and we are done!!!
	core::CallExprPtr ret = builder.callExpr(retTy, newFunc, args);
	
	if(VLOG_IS_ON(2)){
		dumpPretty(&(*ret));
	}
	END_LOG_EXPR_CONVERSION(ret);
	return ret;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX OPERATOR CALL EXPRESSION
//
//  A call to an overloaded operator written using operator syntax.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXOperatorCallExpr(const clang::CXXOperatorCallExpr* callExpr) {
	START_LOG_EXPR_CONVERSION(callExpr);
	core::ExpressionPtr retIr;
	core::ExpressionPtr func;
	core::ExpressionPtr convertedOp;
	ExpressionList args;
	core::FunctionTypePtr funcTy;

	if( const clang::CXXMethodDecl* methodDecl = llvm::dyn_cast<clang::CXXMethodDecl>(callExpr->getCalleeDecl()) ) {
		//operator defined as member function
		VLOG(2) << "Operator defined as member function " 
				<< methodDecl->getParent()->getNameAsString() << "::" 
				<< methodDecl->getNameAsString();
	
		convertedOp =  convFact.convertFunctionDecl(methodDecl).as<core::ExpressionPtr>();
				
		// possible member operators: +,-,*,/,%,^,&,|,~,!,<,>,+=,-=,*=,/=,%=,^=,&=,|=,<<,>>,>>=,<<=,==,!=,<=,>=,&&,||,++,--,','
		// overloaded only as member function: '=', '->', '()', '[]', '->*', 'new', 'new[]', 'delete', 'delete[]'
		//unary:	X::operator@();	left == CallExpr->arg(0) == "this"
		//binary:	X::operator@( right==arg(1) ); left == CallExpr->arg(0) == "this"
		//else functioncall: ():		X::operator@( right==arg(1), args ); left == CallExpr->arg(0) == "this"

		// get "this-object"
		core::ExpressionPtr ownerObj = Visit(callExpr->getArg(0));
	
		// get type of this
		const clang::Type* classType= methodDecl->getParent()->getTypeForDecl();
		core::TypePtr&& irClassType = builder.refType( convFact.convertType(classType) );

		convertedOp = convFact.memberize(llvm::cast<FunctionDecl>(methodDecl), 
											convertedOp,
											irClassType, 
											core::FK_MEMBER_FUNCTION);

		funcTy = convertedOp.getType().as<core::FunctionTypePtr>();
		
		// get arguments
		args = getFunctionArguments(builder, callExpr, funcTy);
		
		//unwrap if is a cpp reference, we dont use cpp references for this
		if (core::analysis::isCppRef(ownerObj->getType())){
		// unwrap and deref the variable
			ownerObj =  builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), ownerObj);
		}
		else if (core::analysis::isConstCppRef(ownerObj->getType())){
			ownerObj =  builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), ownerObj);
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
		
		convertedOp =  convFact.convertFunctionDecl(funcDecl).as<core::ExpressionPtr>();

		funcTy = convertedOp.getType().as<core::FunctionTypePtr>();
		args = getFunctionArguments(builder, callExpr, funcTy);
	}
///
//	FIXME: this was ment to be used with the non implemented asign operator, now is being
//	implemented, terefore the asigment has to be handled as a function call
//
//	once tested, cleanup the code
//
///	switch (callExpr->getOperator()){
///
///		case OO_None:
///			assert(false && "no operator!!");
///
///		case OO_New:
///		case OO_Delete         :
///		case OO_Array_New      :
///		case OO_Array_Delete   :
///			assert(false && " new and delete overload not implemented");
///
///		case OO_Plus                 :
///		case OO_Minus                :
///		case OO_Star                 :
///		case OO_Slash                :
///		case OO_Percent              :
///		case OO_Caret                :
///		case OO_Amp                  :
///		case OO_Pipe                 :
///		case OO_Tilde                :
///		case OO_Exclaim              :
///		case OO_Less                 :
///		case OO_Greater              :
///		case OO_PlusEqual            :
///		case OO_MinusEqual           :
///		case OO_StarEqual            :
///		case OO_SlashEqual           :
///		case OO_PercentEqual         :
///		case OO_CaretEqual           :
///		case OO_AmpEqual             :
///		case OO_PipeEqual            :
///		case OO_LessLess             :
///		case OO_GreaterGreater       :
///		case OO_LessLessEqual        :
///		case OO_GreaterGreaterEqual  :
///		case OO_EqualEqual           :
///		case OO_ExclaimEqual         :
///		case OO_LessEqual            :
///		case OO_GreaterEqual         :
///		case OO_AmpAmp               :
///		case OO_PipePipe             :
///		case OO_PlusPlus             :
///		case OO_MinusMinus           :
///		case OO_Comma                :
///		case OO_ArrowStar            :
///		case OO_Arrow               :
///		case OO_Call           :
///		case OO_Subscript      :
///		case OO_Equal                :
///			func = convertedOp;
///			//assert(false && "CXXOperator not implemented yet");
///			break;
///		case OO_Equal                :
///			func = gen.getRefAssign();
///			break;
///
///		default:
///			assert(false && " no specified operator, did u upgraded clang from 3.2? ");
///
///	}

	retIr = builder.callExpr(funcTy->getReturnType(), convertedOp, args);
	return retIr;
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

	// we do NOT instantiate elidable ctors, this will be generated and ignored if needed by the
	// back end compiler
	if (callExpr->isElidable () ){
		return (Visit(callExpr->getArg (0)));
	}

	// it might be an array construction
	size_t numElements =0;
	if (irClassType->getNodeType() == core::NT_VectorType) {
		numElements = irClassType.as<core::VectorTypePtr>()->getSize().as<core::ConcreteIntTypeParamPtr>()->getValue();
		irClassType	= irClassType.as<core::VectorTypePtr>()->getElementType();
	}

	// to begin with we translate the constructor as a regular function but with initialization list
	core::ExpressionPtr ctorFunc = convFact.convertFunctionDecl(ctorDecl);

	// update parameter list with a class-typed parameter in the first possition
	core::TypePtr&&  refToClassTy = builder.refType(irClassType);

	ctorFunc = convFact.memberize(llvm::cast<FunctionDecl>(ctorDecl), 
									ctorFunc,
									refToClassTy, 
									core::FK_CONSTRUCTOR);
	core::FunctionTypePtr funcTy = ctorFunc.getType().as<core::FunctionTypePtr>();
	
	// reconstruct Arguments list, fist one is a scope location for the object
	ExpressionList&& args = ExprConverter::getFunctionArguments(builder, callExpr, funcTy);
	args.insert (args.begin(), builder.undefinedVar(refToClassTy));

	//  if needed: append globalVar to arguments as last argument
	if ( ctx.globalFuncSet.find(ctorDecl) != ctx.globalFuncSet.end() ) {
		args.push_back(ctx.globalVar);
	}
		

	// build expression and we are done!!!
	core::ExpressionPtr ret;
	if (numElements){
		ret = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getVectorCtor(),
								mgr.getLangBasic().getRefVar(), ctorFunc, builder.getIntParamLiteral(numElements));
	}
	else{
		//single object constructor
		ret = builder.callExpr (funcTy.getReturnType(), ctorFunc, args);
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
		// is a class, handle construction
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

			assert( ctorFunc.as<core::LambdaExprPtr>()->getParameterList().size() > 0 && "not default ctor used in array construction");

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
	}

	END_LOG_EXPR_CONVERSION(retExp);
	return retExp;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX DELETE CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXDeleteExpr(const clang::CXXDeleteExpr* deleteExpr) {
	START_LOG_EXPR_CONVERSION(deleteExpr);

	core::ExpressionPtr retExpr;
	core::ExpressionPtr exprToDelete = Visit(deleteExpr->getArgument());

	if (deleteExpr->isArrayForm () ){

		// we need to call arratDtor, with the object, refdelete and the dtorFunc
		core::TypePtr desTy = convFact.convertType( deleteExpr->getDestroyedType().getTypePtr());
		if( core::hasMetaInfo(desTy)){
			const core::ClassMetaInfo& info = core::getMetaInfo (desTy);
			assert(!info.isDestructorVirtual() && "no virtual dtor allowed for array dtor");

			core::ExpressionPtr dtor;
			if (info.hasDestructor())
				dtor = info.getDestructor();
			else{
				// build a fake default dtor
				// FIXME: this is just a work around, it might not be correct with base clases
				auto thisVar = builder.variable(builder.refType(desTy));
				core::VariableList paramList;
				paramList.push_back( thisVar);

				auto newFunctionType = builder.functionType(core::extractTypes(paramList), 
															builder.refType(desTy),
															core::FK_DESTRUCTOR);

				vector<core::StatementPtr> stmtList;
				dtor =  builder.lambdaExpr (newFunctionType, paramList, builder.compoundStmt(stmtList));
			}
		
			std::vector<core::ExpressionPtr> args;
			args.push_back(exprToDelete);
			args.push_back( builder.getLangBasic().getRefDelete());
			args.push_back( dtor);

			retExpr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getArrayDtor(), args);
		}
		else{
			// this is a built in type, we need to build a empty dtor with the right type
			// FIXME: is backend does not reproduce the right operator we might have memory leaks
			// maybe is better to call arraydtor with a fake dtor
			retExpr = builder.callExpr ( builder.getLangBasic().getRefDelete(), exprToDelete);
		}
	}
	else{
		retExpr = builder.callExpr ( builder.getLangBasic().getRefDelete(), exprToDelete);
	}
		
	END_LOG_EXPR_CONVERSION(retExpr);
	return retExpr;
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
	core::ExpressionPtr ret =  builder.literal("this", irType);

	// this is a pointer, make it pointer
	ret =  builder.callExpr(builder.getLangBasic().getScalarToArray(), ret);

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
	return core::ExpressionPtr();
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

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					CXX Bind Temporary expr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					CXX Expression with cleanups
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					Materialize temporary expr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitMaterializeTemporaryExpr(
																const clang::MaterializeTemporaryExpr* materTempExpr) {


	core::ExpressionPtr expr =  Visit(materTempExpr->GetTemporaryExpr());
//	core::ExpressionPtr ptr;
//	if (expr.isa<core::CallExprPtr>() &&
//		(ptr = expr.as<core::CallExprPtr>().getFunctionExpr()).isa<core::LambdaExprPtr>() && 
//		 ptr.as<core::LambdaExprPtr>().getType().as<core::FunctionTypePtr>().isConstructor()){
//		dumpPretty(expr);
//		return expr;
//	}
//	else 
		return builder.refVar( expr);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::Visit(const clang::Expr* expr) {
	core::ExpressionPtr&& retIr = ConstStmtVisitor<ConversionFactory::CXXExprConverter, core::ExpressionPtr>::Visit(expr);

	// check for OpenMP annotations
	return omp::attachOmpAnnotation(retIr, expr, convFact);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
