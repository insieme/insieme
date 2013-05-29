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
#include "insieme/frontend/utils/castTool.h"

#include "insieme/frontend/utils/debug.h"

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

#include <clang/Basic/FileManager.h>


using namespace clang;
using namespace insieme;
using namespace exprutils;

namespace insieme {
namespace frontend {

namespace {

// unwraps cppRef/constCppRef
core::ExpressionPtr unwrapCppRef(const core::IRBuilder& builder, const core::ExpressionPtr& expr) {
	
	core::NodeManager& mgr = builder.getNodeManager();	
	core::TypePtr irType = expr->getType();
	if (core::analysis::isCppRef(irType)) {
		return builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), expr);
	}
	else if (core::analysis::isConstCppRef(irType)) {
		return builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), expr);
	}

	return expr;
}

} // end anonymous namespace


namespace conversion {

//---------------------------------------------------------------------------------------------------------------------
//										CXX EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						  IMPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr) {
	core::ExpressionPtr retIr = ExprConverter::VisitImplicitCastExpr(castExpr);
	LOG_EXPR_CONVERSION(castExpr, retIr);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						EXPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr) {
	core::ExpressionPtr retIr = ExprConverter::VisitExplicitCastExpr(castExpr);
	LOG_EXPR_CONVERSION(castExpr, retIr);
	return retIr;
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
	core::ExpressionPtr retIr = ExprConverter::VisitCallExpr(callExpr);
    LOG_EXPR_CONVERSION(callExpr, retIr);

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
					if (!IS_CPP_REF_EXPR(tmp)){
						tmp = convFact.tryDeref(tmp);
					}
					else{
						/*
						if (core::analysis::isCppRef(tmp->getType())) {
							tmp = builder.deref(builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), tmp));
						}
						else if (core::analysis::isConstCppRef(tmp->getType())) {
							tmp = builder.deref(builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), tmp));
						}
						*/
						tmp = unwrapCppRef(builder, tmp);
						tmp = convFact.tryDeref(tmp);
					}

					core::CallExprAddress addr(retIr.as<core::CallExprPtr>());
					retIr = core::transform::replaceNode (mgr, addr->getArgument(i), tmp).as<core::ExpressionPtr>();
				}
			}
		}
	}
	return retIr;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						  MEMBER EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitMemberExpr(const clang::MemberExpr* membExpr){
	core::ExpressionPtr retIr;
    LOG_EXPR_CONVERSION(membExpr, retIr);
	// get the base we want to access to
	core::ExpressionPtr&& base = Visit(membExpr->getBase());

	// if is not a pointer member, it might be that is a CPP ref
	/*
	core::TypePtr irType = base->getType();
	if (core::analysis::isCppRef(irType)) {
		base = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), base);
	}
	else if (core::analysis::isConstCppRef(irType)) {
		base = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), base);
	}
	*/
	base = unwrapCppRef(builder, base);

	// TODO: we have the situation here in which we might want to access a field of a superclass
	// this will not be resolved by the C frontend. and we need to build the right datapath to
	// reach the definition
	retIr = getMemberAccessExpr(builder, base, membExpr);
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							VAR DECLARATION REFERENCE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitDeclRefExpr(const clang::DeclRefExpr* declRef) {
	core::ExpressionPtr retIr;
    LOG_EXPR_CONVERSION(declRef, retIr);
	// if is a prameter and is a cpp ref, avoid going further to avoid wrapping issues
	if (const ParmVarDecl* parmDecl = dyn_cast<ParmVarDecl>(declRef->getDecl())) {
		retIr = convFact.lookUpVariable( parmDecl );
		if (IS_CPP_REF_EXPR(retIr)){
			return retIr;
		}
	}

	return retIr = ConversionFactory::ExprConverter::VisitDeclRefExpr (declRef);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								CXX BOOLEAN LITERAL
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXBoolLiteralExpr(const clang::CXXBoolLiteralExpr* boolLit) {
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
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXMemberCallExpr(const clang::CXXMemberCallExpr* callExpr) {
	core::CallExprPtr ret;
	LOG_EXPR_CONVERSION(callExpr, ret);

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
									core::FK_MEMBER_FUNCTION).as<core::ExpressionPtr>();

	core::FunctionTypePtr funcTy = newFunc.getType().as<core::FunctionTypePtr>();

	// get the this-Object
	core::ExpressionPtr ownerObj = Visit(callExpr->getImplicitObjectArgument());
	// correct the owner object reference, in case of pointer (ref<array<struct<...>,1>>) we need to
	// index the first element
	ownerObj = getCArrayElemRef(builder, ownerObj);

	//unwrap if is a cpp reference, we dont use cpp references for this
	/*
	if (core::analysis::isCppRef(ownerObj->getType())){
	// unwrap and deref the variable
		ownerObj =  builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), ownerObj);
	}
	else if (core::analysis::isConstCppRef(ownerObj->getType())){
		ownerObj =  builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), ownerObj);
	} 
	*/
	ownerObj = unwrapCppRef(builder, ownerObj);

	// reconstruct Arguments list, fist one is a scope location for the object
	ExpressionList&& args = ExprConverter::getFunctionArguments(callExpr, llvm::cast<clang::FunctionDecl>(methodDecl) );
	args.insert (args.begin(), ownerObj);

	core::TypePtr retTy = funcTy.getReturnType();

	// build expression and we are done!!!
	ret = builder.callExpr(retTy, newFunc, args);

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
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXOperatorCallExpr(const clang::CXXOperatorCallExpr* callExpr) {
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
											core::FK_MEMBER_FUNCTION).as<core::ExpressionPtr>();

		// get arguments
		funcTy = convertedOp.getType().as<core::FunctionTypePtr>();
		args = getFunctionArguments(callExpr, funcTy, llvm::cast<clang::FunctionDecl>(methodDecl));

		//unwrap if is a cpp reference, we dont use cpp references for this
		/*
		if (core::analysis::isCppRef(ownerObj->getType())){
		// unwrap and deref the variable
			ownerObj =  builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), ownerObj);
		}
		else if (core::analysis::isConstCppRef(ownerObj->getType())){
			ownerObj =  builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), ownerObj);
		}
		*/ 
		ownerObj = unwrapCppRef(builder, ownerObj);


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
		args = getFunctionArguments(callExpr, funcDecl);
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
	const core::IRBuilder& builder = convFact.builder;

// TODO:  array constructor with no default initialization (CXX11)

	const CXXConstructorDecl* ctorDecl = callExpr->getConstructor();

	const clang::Type* classType= callExpr->getType().getTypePtr();
	core::TypePtr&& irClassType = convFact.convertType(classType);

	// we do NOT instantiate elidable ctors, this will be generated and ignored if needed by the
	// back end compiler
	if (callExpr->isElidable () ){
		// if is an elidable constructor, we should return a refvar, not what the parameters say
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
									core::FK_CONSTRUCTOR).as<core::ExpressionPtr>();
	core::FunctionTypePtr funcTy = ctorFunc.getType().as<core::FunctionTypePtr>();

	// reconstruct Arguments list, fist one is a scope location for the object
	ExpressionList&& args = ExprConverter::getFunctionArguments(callExpr, llvm::cast<clang::FunctionDecl>(ctorDecl));

	// first paramenter is the memory storage, the this location
	args.insert (args.begin(), builder.undefinedVar(refToClassTy));

	// build expression and we are done!!!
	core::ExpressionPtr ret;
	LOG_EXPR_CONVERSION(callExpr, ret);

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

	return ret;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX NEW CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXNewExpr(const clang::CXXNewExpr* callExpr) {
	//TODO:  - inplace allocation - non default ctor array?
	core::ExpressionPtr retExpr;
	LOG_EXPR_CONVERSION(callExpr, retExpr);

	if (callExpr->getAllocatedType().getTypePtr()->isBuiltinType()){

		core::TypePtr type = convFact.convertType(callExpr->getAllocatedType().getTypePtr());
		core::ExpressionPtr placeHolder = builder.undefinedNew(type);

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
		// is a class, handle construction
		core::ExpressionPtr ctorCall = Visit(callExpr->getConstructExpr());
		assert(ctorCall.isa<core::CallExprPtr>() && "aint no constructor call in here, no way to translate NEW");

		core::TypePtr type = ctorCall->getType();
		core::ExpressionPtr newCall = builder.undefinedNew(type);

		if (callExpr->isArray()){
			core::ExpressionPtr arrSizeExpr = convFact.convertExpr( callExpr->getArraySize() );
			arrSizeExpr = utils::castScalar(mgr.getLangBasic().getUInt8(), arrSizeExpr);

			// extract only the ctor function from the converted ctor call
			core::ExpressionPtr ctorFunc = ctorCall.as<core::CallExprPtr>().getFunctionExpr();

			assert( ctorFunc.as<core::LambdaExprPtr>()->getParameterList().size() > 0 && "not default ctor used in array construction");

			retExpr = ( builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getArrayCtor(),
			 						   				   mgr.getLangBasic().getRefNew(), ctorFunc, arrSizeExpr));
		}
		else{

			// the basic constructor translation defines a stack variable as argument for the call
			// in order to turn this into a diynamic memory allocation, we only need to substitute
			// the first argument for a heap location
			core::CallExprAddress addr(ctorCall.as<core::CallExprPtr>());
			retExpr = core::transform::replaceNode (convFact.mgr,
												  addr->getArgument(0),
												  newCall ).as<core::CallExprPtr>();

			retExpr = builder.callExpr(builder.getLangBasic().getScalarToArray(), retExpr);
		}
	}

	return retExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX DELETE CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXDeleteExpr(const clang::CXXDeleteExpr* deleteExpr) {

	core::ExpressionPtr retExpr;
	LOG_EXPR_CONVERSION(deleteExpr, retExpr);

	core::ExpressionPtr exprToDelete = Visit(deleteExpr->getArgument());
	core::TypePtr desTy = convFact.convertType( deleteExpr->getDestroyedType().getTypePtr());

	VLOG(2) << exprToDelete->getType();
	

	core::ExpressionPtr dtor;
	if( core::hasMetaInfo(desTy)){
		const core::ClassMetaInfo& info = core::getMetaInfo (desTy);
		if (!info.hasDestructor())
			assert(false && "empty dtor should be synthetized by cxx_type_convert");
		dtor = info.getDestructor();
	}

	if (deleteExpr->isArrayForm () ){

		// we need to call arratDtor, with the object, refdelete and the dtorFunc
		if(dtor){
			assert(!core::getMetaInfo(desTy).isDestructorVirtual() && "no virtual dtor allowed for array dtor");

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
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXThisExpr(const clang::CXXThisExpr* thisExpr) {
	//figure out the type of the expression
	core::TypePtr&& irType = convFact.convertType( llvm::cast<clang::TypeDecl>(thisExpr->getBestDynamicClassType())->getTypeForDecl() );
	irType = builder.refType(irType);

	// build a literal as a placeholder (has to be substituted later by function call expression)
	core::ExpressionPtr ret =  builder.literal("this", irType);

	// this is a pointer, make it pointer
	ret =  builder.callExpr(builder.getLangBasic().getScalarToArray(), ret);

	LOG_EXPR_CONVERSION(thisExpr, ret);
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
	//START_LOG_EXPR_CONVERSION(defaultArgExpr);
	auto ret = Visit(defaultArgExpr->getExpr());
	LOG_EXPR_CONVERSION(defaultArgExpr, ret);


	return ret;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					CXX Bind Temporary expr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXBindTemporaryExpr(const clang::CXXBindTemporaryExpr* bindTempExpr) {
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(bindTempExpr, retIr);

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
	if (!gen.isRef(body->getType()))
		body = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), body);

	core::DeclarationStmtPtr declStmt;

	declStmt = convFact.builder.declarationStmt(convFact.builder.refType(irType),(body));

	// store temporary and declaration stmt in Map
	convFact.ctx.tempInitMap.insert(std::make_pair(temp,declStmt));

	return retIr = declStmt.getVariable();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					CXX Expression with cleanups
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitExprWithCleanups(const clang::ExprWithCleanups* cleanupExpr) {
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(cleanupExpr, retIr);

	// perform subtree traversal and get the temporaries that the cleanup expression creates
	std::vector<const clang::CXXTemporary*>&& tmps = utils::lookupTemporaries (cleanupExpr->getSubExpr ());

	// convert the subexpr to IR
	const clang::Expr* inner = cleanupExpr->getSubExpr();
	core::ExpressionPtr innerIR = convFact.convertExpr(inner);

	// for each of the temporaries(reverse) create an IR var decl and push it at the beginning of the
	// lambda body
	vector<core::StatementPtr> stmtList;
	for (std::vector<const clang::CXXTemporary*>::reverse_iterator it = tmps.rbegin() ; it != tmps.rend(); ++it) {

		ConversionFactory::ConversionContext::TemporaryInitMap::const_iterator fit = convFact.ctx.tempInitMap.find(*it);
	    if (fit != convFact.ctx.tempInitMap.end()) {
			// if the cleanup obj is a const_ref, we dont need the cleanup expr
			core::DeclarationStmtPtr decl = fit->second.as<core::DeclarationStmtPtr>();
			core::VariablePtr        var  = decl->getVariable();
			core::ExpressionPtr      init = decl->getInitialization();

			core::ExpressionPtr trg = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), var);
			core::ExpressionPtr subst = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), init);
			core::ExpressionPtr newIr = core::transform::replaceAllGen (mgr, innerIR, trg, subst, false);

			if (newIr == innerIR)
				stmtList.push_back(fit->second);
			else
				innerIR = newIr;
		}
	}

	if (stmtList.empty()){
		// we avoided all expressions to be cleanup, no extra lambda needed
		return innerIR;
	}

	core::TypePtr lambdaRetType = convFact.convertType(cleanupExpr->getType().getTypePtr());
	if (innerIR->getType() != lambdaRetType && !gen.isRef(lambdaRetType))
		innerIR = convFact.tryDeref(innerIR);

	// if the expression does not return anything, do not add return stmt
	if (gen.isUnit(innerIR->getType())){
		stmtList.push_back(innerIR);
	}
	else{
		stmtList.push_back(convFact.builder.returnStmt(innerIR));
	}

	//build the lambda and its parameters
	core::StatementPtr&& lambdaBody = convFact.builder.compoundStmt(stmtList);
	vector<core::VariablePtr> params = core::analysis::getFreeVariables(lambdaBody);
	core::LambdaExprPtr lambda = convFact.builder.lambdaExpr(lambdaRetType, lambdaBody, params);

	//build the lambda call and its arguments
	vector<core::ExpressionPtr> packedArgs;
	std::for_each(params.begin(), params.end(), [&packedArgs] (core::VariablePtr varPtr) {
		 packedArgs.push_back(varPtr);
	});

	return retIr = builder.callExpr(lambdaRetType, lambda, packedArgs);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					ScalarValueInitExpr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitCXXScalarValueInitExpr(const clang::CXXScalarValueInitExpr* scalarValueInit){
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(scalarValueInit, retIr);

	core::TypePtr elemType =convFact.convertType ( scalarValueInit->getTypeSourceInfo()->getType().getTypePtr());
	retIr = convFact.defaultInitVal(elemType);
	return retIr;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					Materialize temporary expr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::VisitMaterializeTemporaryExpr( const clang::MaterializeTemporaryExpr* materTempExpr) {
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(materTempExpr, retIr);
	retIr =  Visit(materTempExpr->GetTemporaryExpr());

	// is a left side value, no need to materialize. has being handled by a temporary expression
	if(IS_CPP_REF_EXPR(retIr) || gen.isRef(retIr->getType()))
		return retIr;
	else
		return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), retIr));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CXXExprConverter::Visit(const clang::Expr* expr) {
	core::ExpressionPtr&& retIr = ConstStmtVisitor<ConversionFactory::CXXExprConverter, core::ExpressionPtr>::Visit(expr);

	// print diagnosis messages
	convFact.printDiagnosis(expr->getLocStart());

	// check for OpenMP annotations
	return omp::attachOmpAnnotation(retIr, expr, convFact);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
