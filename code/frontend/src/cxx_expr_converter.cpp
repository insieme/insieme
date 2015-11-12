/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/clang.h"
#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/state/record_manager.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/stmt_wrapper.h"
#include "insieme/frontend/utils/temporaries_lookup.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/transform/node_replacer.h"

using namespace clang;
using namespace insieme;

namespace insieme {
namespace frontend {
namespace conversion {

	//---------------------------------------------------------------------------------------------------------------------
	//										CXX EXPRESSION CONVERTER
	//---------------------------------------------------------------------------------------------------------------------

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::Visit(const clang::Expr* expr) {
		// iterate clang handler list and check if a handler wants to convert the expr
		core::ExpressionPtr retIr;
		// call frontend extension visitors
		for (auto extension : converter.getConversionSetup().getExtensions()) {
			retIr = extension->Visit(expr, converter);
			if (retIr) { break; }
		}
		if (!retIr) {
			converter.trackSourceLocation(expr);
			retIr = ConstStmtVisitor<Converter::CXXExprConverter, core::ExpressionPtr>::Visit(expr);
			converter.untrackSourceLocation();
		}
		else {
			VLOG(2) << "CXXExprConverter::Visit handled by plugin";
		}

		// print diagnosis messages
		converter.printDiagnosis(expr->getLocStart());

		// call frontend extension post visitors
		for (auto extension : converter.getConversionSetup().getExtensions()) {
			retIr = extension->PostVisit(expr, retIr, converter);
		}

		// attach location annotation
		if (expr->getLocStart().isValid()) {
			auto presStart = converter.getSourceManager().getPresumedLoc(expr->getLocStart());
			auto presEnd = converter.getSourceManager().getPresumedLoc(expr->getLocEnd());
			core::annotations::attachLocation(retIr, std::string(presStart.getFilename()), presStart.getLine(), presStart.getColumn(), presEnd.getLine(),
				presEnd.getColumn());
		}

		return retIr;
	}

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
		
		return irCall;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  UNARY OPERATOR EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitUnaryOperator(const clang::UnaryOperator* unOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(unOp, retIr);

		assert_not_implemented();

		return retIr = Converter::ExprConverter::VisitUnaryOperator(unOp);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  MEMBER EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitMemberExpr(const clang::MemberExpr* membExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(membExpr, retIr);

		//// get the base we want to access to
		//core::ExpressionPtr&& base = Visit(membExpr->getBase());

		//// it might be that is a function, therefore we retrieve a callable expression
		//const clang::ValueDecl* valDecl = membExpr->getMemberDecl();
		//if(valDecl && llvm::isa<clang::FunctionDecl>(valDecl)) {
		//	retIr = converter.getCallableExpression(llvm::cast<clang::FunctionDecl>(valDecl));
		//	// exceptional handling if the val decl is a static function -> outline code (see CallExprVisitor)
		//	const clang::CXXMethodDecl* mdecl = llvm::dyn_cast<clang::CXXMethodDecl>(valDecl);
		//	if(mdecl && mdecl->isStatic() && base.isa<core::CallExprPtr>()) {
		//		// create a function that calls a function like fun() { base(); return call; }
		//		core::CompoundStmtPtr comp = builder.compoundStmt({base, builder.returnStmt(retIr)});
		//		retIr = builder.createCallExprFromBody(comp, retIr->getType());
		//	}
		//	return retIr;
		//}

		//if(core::analysis::isAnyCppRef(converter.lookupTypeDetails(base->getType()))) { base = builder.toIRRef(base); }

		//retIr = getMemberAccessExpr(converter, builder, base, membExpr);

		//// if the  resulting expression is a ref to cpp ref, we remove one ref, no need to provide one extra ref
		//if(retIr->getType().isa<core::RefTypePtr>() && core::analysis::isAnyCppRef(retIr->getType().as<core::RefTypePtr>()->getElementType())) {
		//	retIr = builder.deref(retIr);
		//}

		assert_not_implemented();
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitDeclRefExpr(const clang::DeclRefExpr* declRef) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(declRef, retIr);
		
		if(const clang::FieldDecl* field = llvm::dyn_cast<clang::FieldDecl>(declRef->getDecl())) {
			field->dump(); // prevent unused warning
			assert_not_implemented();
			// this is the direct access to a member field in a generic way: something like Obj::a
		}

		return retIr = Converter::ExprConverter::VisitDeclRefExpr(declRef);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CXX BOOLEAN LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXBoolLiteralExpr(const clang::CXXBoolLiteralExpr* boolLit) {
		core::ExpressionPtr retExpr = converter.builder.boolLit(boolLit->getValue());
		LOG_EXPR_CONVERSION(boolLit, retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX MEMBER CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXMemberCallExpr(const clang::CXXMemberCallExpr* callExpr) {
		core::ExpressionPtr ret;
		LOG_EXPR_CONVERSION(callExpr, ret);

		const core::FrontendIRBuilder& builder = converter.builder;
		const CXXMethodDecl* methodDecl = callExpr->getMethodDecl();

		if(!methodDecl) {
			// member function pointer call
			frontend_assert(false) << "Member function pointer call not implemented";
		} else {			
			// get method lambda
			auto methodLambda = converter.getDeclConverter()->convertMethodDecl(methodDecl)->getImplementation();

			// get the "this" object and add to arguments
			core::ExpressionPtr thisObj = Visit(callExpr->getImplicitObjectArgument());
			core::ExpressionList arguments { thisObj };
			for(auto arg : callExpr->arguments()) {
				arguments.push_back(converter.convertExpr(arg));
			}

			// build call and we are done
			auto retType = methodLambda->getType().as<core::FunctionTypePtr>()->getReturnType();
			ret = builder.callExpr(retType, methodLambda, arguments);
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
		//core::ExpressionPtr func;
		//core::ExpressionPtr convertedOp;
		//ExpressionList args;
		//core::FunctionTypePtr funcTy;

		//if(const clang::CXXMethodDecl* methodDecl = llvm::dyn_cast<clang::CXXMethodDecl>(callExpr->getCalleeDecl())) {
		//	// operator defined as member function
		//	VLOG(2) << "Operator defined as member function " << methodDecl->getParent()->getNameAsString() << "::" << methodDecl->getNameAsString();


		//	convertedOp = converter.getCallableExpression(methodDecl);

		//	// possible member operators: +,-,*,/,%,^,&,|,~,!,<,>,+=,-=,*=,/=,%=,^=,&=,|=,<<,>>,>>=,<<=,==,!=,<=,>=,&&,||,++,--,','
		//	// overloaded only as member function: '=', '->', '()', '[]', '->*', 'new', 'new[]', 'delete', 'delete[]'
		//	// unary:	X::operator@();	left == CallExpr->arg(0) == "this"
		//	// binary:	X::operator@( right==arg(1) ); left == CallExpr->arg(0) == "this"
		//	// else functioncall: ():		X::operator@( right==arg(1), args ); left == CallExpr->arg(0) == "this"

		//	// get "this-object"
		//	core::ExpressionPtr ownerObj = converter.convertExpr(callExpr->getArg(0));

		//	if(core::analysis::isAnyCppRef(ownerObj->getType())) { ownerObj = builder.toIRRef(ownerObj); }

		//	// get arguments
		//	funcTy = convertedOp.getType().as<core::FunctionTypePtr>();
		//	args = getFunctionArguments(callExpr, funcTy);

		//	//  the problem is, we call a memeber function over a value, the owner MUST be always a ref,
		//	//  is not a expression with cleanups because this object has not need to to be destucted,
		//	//  no used defined dtor.
		//	// some constructions might return an instance, incorporate a materialize
		//	if(!ownerObj->getType().isa<core::RefTypePtr>()) {
		//		// ownerObj =  builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), ownerObj);
		//		ownerObj = builder.refVar(ownerObj);
		//	}

		//	// incorporate this to the beginning of the args list
		//	args.insert(args.begin(), ownerObj);
		//} else if(const clang::FunctionDecl* funcDecl = llvm::dyn_cast<clang::FunctionDecl>(callExpr->getCalleeDecl())) {
		//	// operator defined as non-member function
		//	VLOG(2) << "Operator defined as non-member function " << funcDecl->getNameAsString();

		//	// possible non-member operators:
		//	// unary:	operator@( left==arg(0) )
		//	// binary:	operator@( left==arg(0), right==arg(1) )

		//	convertedOp = converter.convertExpr(callExpr->getCallee());

		//	funcTy = convertedOp.getType().as<core::FunctionTypePtr>();
		//	args = getFunctionArguments(callExpr, funcDecl);
		//}

		//retIr = builder.callExpr(funcTy->getReturnType(), convertedOp, args);

		assert_not_implemented();

		return retIr;
	}

	namespace {
		core::ExpressionPtr convertConstructExprInternal(Converter& converter, const clang::CXXConstructExpr* constructExpr, bool onStack) {
			auto& builder = converter.getIRBuilder();
			core::TypePtr resType = converter.convertType(constructExpr->getType());

			VLOG(2) << "convertConstructExprInternal - ResType: \n" << dumpDetailColored(resType) << " recType:\n"
				    << (*converter.getIRTranslationUnit().getTypes().find(resType.as<core::GenericTypePtr>())).second << "\n";

			// first constructor argument is the object memory location -- we need to build this either on the stack or heap
			auto irMemLoc = onStack ? builder.undefinedVar(resType) : builder.undefinedNew(resType);

			// the constructor is then simply a call with the mem location and all its arguments
			core::ExpressionList arguments { irMemLoc };
			for(auto arg : constructExpr->arguments()) {
				arguments.push_back(converter.convertExpr(arg));
			}

			// get constructor lambda
			auto constructorLambda = converter.getDeclConverter()->convertMethodDecl(constructExpr->getConstructor())->getImplementation();

			// return call
			auto retType = constructorLambda->getType().as<core::FunctionTypePtr>()->getReturnType();
			return builder.callExpr(retType, constructorLambda, arguments);
		}
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX CONSTRUCTOR CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXConstructExpr(const clang::CXXConstructExpr* callExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(callExpr, retIr);

		retIr = convertConstructExprInternal(converter, callExpr, true);
		
		frontend_assert(retIr) << "ConstructExpr could not be translated\n";
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX NEW CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXNewExpr(const clang::CXXNewExpr* callExpr) {
		// TODO:  - inplace allocation - non default ctor array?
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(callExpr, retExpr);

		//// if no constructor is found, it is a new over an non-class type, can be any kind of pointer of array
		//// spetialy double pointer
		//if(!callExpr->getConstructExpr()) {
		//	core::TypePtr type = converter.convertType(callExpr->getAllocatedType());
		//	core::ExpressionPtr placeHolder;
		//	if(callExpr->hasInitializer()) {
		//		const clang::Expr* initializer = callExpr->getInitializer();
		//		core::ExpressionPtr initializerExpr = converter.convertExpr(initializer);
		//		frontend_assert(initializerExpr);
		//		placeHolder = initializerExpr;
		//	} else {
		//		placeHolder = builder.undefinedNew(type);
		//	}

		//	if(callExpr->isArray()) {
		//		core::ExpressionPtr&& arrSizeExpr = converter.convertExpr(callExpr->getArraySize());
		//		placeHolder = builder.callExpr(builder.arrayType(type), builder.getLangBasic().getArrayCreate1D(), builder.getTypeLiteral(type),
		//		                               core::types::smartCast(arrSizeExpr, gen.getUInt4()));
		//		retExpr = builder.refNew(placeHolder);
		//	} else {
		//		retExpr = builder.callExpr(builder.getLangBasic().getScalarToArray(), builder.refNew(placeHolder));
		//	}
		//} else {
		//	core::ExpressionPtr ctorCall = Visit(callExpr->getConstructExpr());
		//	frontend_assert(ctorCall.isa<core::CallExprPtr>()) << "aint constructor call in here, no way to translate NEW\n";

		//	core::TypePtr type = converter.convertType(callExpr->getAllocatedType());
		//	core::ExpressionPtr newCall = builder.undefinedNew(ctorCall->getType());

		//	if(callExpr->isArray()) {
		//		core::ExpressionPtr arrSizeExpr = converter.convertExpr(callExpr->getArraySize());
		//		arrSizeExpr = core::types::castScalar(mgr.getLangBasic().getUInt8(), arrSizeExpr);

		//		// if the object is a POD the construct expression visitor will return a variable
		//		// which is of course no ctor. if this is the case we create a default constructor
		//		// and use this one to initalize the new elements
		//		if(!core::analysis::isConstructorCall(ctorCall)) {
		//			auto ctor = core::analysis::createDefaultConstructor(type);
		//			auto ret =
		//			    builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getArrayCtor(), mgr.getLangBasic().getRefNew(), ctor, arrSizeExpr);
		//			return ret;
		//		}

		//		// extract only the ctor function from the converted ctor call
		//		core::ExpressionPtr ctorFunc = ctorCall.as<core::CallExprPtr>().getFunctionExpr();

		//		frontend_assert(ctorFunc->getType().as<core::FunctionTypePtr>()->getParameterTypes().size()) << "not default ctor used in array construction\n";

		//		retExpr =
		//		    builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getArrayCtor(), mgr.getLangBasic().getRefNew(), ctorFunc, arrSizeExpr);
		//	} else {
		//		// the basic constructor translation defines a stack variable as argument for the call
		//		// in order to turn this into a diynamic memory allocation, we only need to substitute
		//		// the first argument for a heap location
		//		core::CallExprAddress addr(ctorCall.as<core::CallExprPtr>());
		//		if(insieme::core::analysis::isConstructorCall(ctorCall)) {
		//			VLOG(2) << addr->getArgument(0).as<core::CallExprPtr>();
		//			retExpr = core::transform::replaceNode(converter.mgr, addr->getArgument(0), newCall).as<core::CallExprPtr>();

		//			retExpr = builder.callExpr(builder.getLangBasic().getScalarToArray(), retExpr);
		//		} else {
		//			// if constructor of is NOT userprovided we get back the "plain" object, no wrapping
		//			// ctor to take care of for the exchange of "refVar" with "newCall"
		//			if(!callExpr->getConstructExpr()->getConstructor()->isUserProvided()) {
		//				VLOG(2) << addr.as<core::CallExprPtr>();
		//				retExpr = core::transform::replaceNode(converter.mgr, addr, newCall).as<core::CallExprPtr>();

		//				retExpr = builder.callExpr(builder.getLangBasic().getScalarToArray(), retExpr);
		//			} else {
		//				retExpr = builder.callExpr(builder.getLangBasic().getScalarToArray(), builder.refNew(addr->getArgument(0)));
		//			}
		//		}
		//	}
		//}

		assert_not_implemented();

		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX DELETE CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXDeleteExpr(const clang::CXXDeleteExpr* deleteExpr) {
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(deleteExpr, retExpr);

		//// convert the target of our delete expr
		//core::ExpressionPtr exprToDelete = Visit(deleteExpr->getArgument());

		//core::ExpressionPtr dtor;
		//clang::CXXDestructorDecl* dtorDecl = nullptr;
		//// since destructor might be defined in a different translation unit or even in this one but after the usage
		//// we should retrieve a callable symbol and delay the conversion
		//auto ty = deleteExpr->getDestroyedType().getTypePtr();
		//const clang::CXXRecordDecl* classDecl = nullptr;
		//// if it is a tag type everything is straight forward, but
		//// if we have a template specialization here we need to call something different to get the record decl
		//if(const clang::TagType* record = llvm::dyn_cast<clang::TagType>(ty)) {
		//	classDecl = llvm::dyn_cast<clang::CXXRecordDecl>(record->getDecl());
		//} else if(const clang::TemplateSpecializationType* templType = llvm::dyn_cast<clang::TemplateSpecializationType>(ty)) {
		//	classDecl = llvm::dyn_cast<clang::CXXRecordDecl>(templType->getAsCXXRecordDecl());
		//}

		//if(classDecl) {
		//	dtorDecl = classDecl->getDestructor();
		//	if(dtorDecl) { dtor = converter.getCallableExpression(dtorDecl); }
		//}

		//if(deleteExpr->isArrayForm()) {
		//	// we need to call arratDtor, with the object, refdelete and the dtorFunc
		//	if(dtor) {
		//		// FIXME: why mem_alloc dtor has being marked as virtual????
		//		frontend_assert(dtorDecl && !dtorDecl->isVirtual()) << "no virtual dtor allowed for array dtor\n";

		//		std::vector<core::ExpressionPtr> args;
		//		args.push_back(exprToDelete);
		//		args.push_back(builder.getLangBasic().getRefDelete());
		//		args.push_back(dtor);
		//		retExpr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getArrayDtor(), args);
		//	} else {
		//		exprToDelete = getCArrayElemRef(builder, exprToDelete);

		//		// this is a built in type, we need to build a empty dtor with the right type
		//		retExpr = builder.callExpr(builder.getLangBasic().getRefDelete(), exprToDelete);
		//	}
		//} else {
		//	exprToDelete = getCArrayElemRef(builder, exprToDelete);

		//	if(dtor) {
		//		retExpr = builder.callExpr(builder.getLangBasic().getRefDelete(), builder.callExpr(dtor, toVector(exprToDelete)));
		//	} else {
		//		retExpr = builder.callExpr(builder.getLangBasic().getRefDelete(), exprToDelete);
		//	}
		//}

		assert_not_implemented();

		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX THIS CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXThisExpr(const clang::CXXThisExpr* thisExpr) {
		core::ExpressionPtr retIr;
		//// figure out the type of the expression
		//core::TypePtr&& irType =
		//    converter.convertType(llvm::cast<clang::TypeDecl>(thisExpr->getBestDynamicClassType())->getTypeForDecl()->getCanonicalTypeInternal());
		//frontend_assert(irType.isa<core::GenericTypePtr>()) << "for convention, all this operators deal with generic types\n";
		//irType = builder.refType(irType);

		//// build a variable as a placeholder (has to be substituted later by function call expression)
		//// converter.thisVariable returns alwasy a variable with id 0 as placeholder!
		//core::ExpressionPtr ret = converter.thisVariable(irType);

		//// this is a pointer, make it pointer
		//ret = builder.callExpr(builder.getLangBasic().getScalarToArray(), ret);

		//LOG_EXPR_CONVERSION(thisExpr, ret);

		assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					EXCEPTION CXX THROW EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXThrowExpr(const clang::CXXThrowExpr* throwExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(throwExpr, retIr);

		//core::ExpressionPtr subExpr;
		//core::TypePtr targetTy;
		//if(throwExpr->getSubExpr()) {
		//	subExpr = Visit(throwExpr->getSubExpr());
		//	targetTy = converter.convertType(throwExpr->getSubExpr()->getType());
		//	core::TypePtr srcTy = subExpr->getType();
		//	if(targetTy != srcTy) { subExpr = converter.tryDeref(subExpr); }
		//} else {
		//	// a throw without expression rethrows the expression captured in the container catch.
		//	// if no expressesion, calls terminate(). but we delegate this to the output compiler
		//	targetTy = gen.getUnit();
		//	subExpr = builder.literal("__insieme__rethrow", targetTy);
		//}
		//retIr = converter.createCallExprFromBody(builder.throwStmt(subExpr), gen.getUnit());

		assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX DEFAULT ARG EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXDefaultArgExpr(const clang::CXXDefaultArgExpr* defaultArgExpr) {
		auto retIr = Visit(defaultArgExpr->getExpr());
		LOG_EXPR_CONVERSION(defaultArgExpr, retIr);

		assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX Bind Temporary expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXBindTemporaryExpr(const clang::CXXBindTemporaryExpr* bindTempExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(bindTempExpr, retIr);

		//const clang::CXXTemporary* temp = bindTempExpr->getTemporary();

		//// we may visit the BindTemporaryExpr twice. Once in the temporary lookup and
		//// then when we visit the subexpr of the expression with cleanups. If this is the second time that we
		//// visit the expr do not create a new declaration statement and just return the previous one.
		//Converter::TemporaryInitMap::const_iterator fit = converter.tempInitMap.find(temp);
		//if(fit != converter.tempInitMap.end()) {
		//	// variable found in the map
		//	return (fit->second.getVariable());
		//}

		//const clang::CXXDestructorDecl* dtorDecl = temp->getDestructor();
		//const clang::CXXRecordDecl* classDecl = dtorDecl->getParent();

		//core::TypePtr&& irType = converter.convertType(classDecl->getTypeForDecl()->getCanonicalTypeInternal());

		//// create a new var for the temporary and initialize it with the inner expr IR
		//const clang::Expr* inner = bindTempExpr->getSubExpr();
		//core::ExpressionPtr body = converter.convertExpr(inner);
		//if(!gen.isRef(body->getType())) {
		//	// body = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), body);
		//	body = builder.refVar(body);
		//}

		//core::DeclarationStmtPtr declStmt;
		//declStmt = converter.builder.declarationStmt(converter.builder.refType(irType), (body));

		//// store temporary and declaration stmt in Map
		//converter.tempInitMap.insert(std::make_pair(temp, declStmt));
		//return retIr = declStmt.getVariable();

		assert_not_implemented();

		return retIr;

	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX Expression with cleanups
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitExprWithCleanups(const clang::ExprWithCleanups* cleanupExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(cleanupExpr, retIr);

		//// perform subtree traversal and get the temporaries that the cleanup expression creates
		//std::vector<const clang::CXXTemporary*>&& tmps = utils::lookupTemporaries(cleanupExpr->getSubExpr());

		//// convert the subexpr to IR
		//const clang::Expr* inner = cleanupExpr->getSubExpr();
		//core::ExpressionPtr innerIr = converter.convertExpr(inner);

		//// for each of the temporaries create an IR var decl and push it at the beginning of the
		//// lambda body
		//vector<core::StatementPtr> stmtList;
		//for(std::vector<const clang::CXXTemporary*>::iterator it = tmps.begin(); it != tmps.end(); ++it) {
		//	Converter::TemporaryInitMap::const_iterator fit = converter.tempInitMap.find(*it);
		//	if(fit != converter.tempInitMap.end()) {
		//		// if the cleanup obj is a const_ref, we dont need the cleanup expr
		//		core::DeclarationStmtPtr decl = fit->second.as<core::DeclarationStmtPtr>();
		//		core::VariablePtr var = decl->getVariable();
		//		core::ExpressionPtr init = decl->getInitialization();

		//		VLOG(2) << " expr: " << innerIr;
		//		VLOG(2) << " cleanup: " << var << " (type: " << var->getType() << ")  init: " << init << std::endl;
		//		stmtList.insert(stmtList.begin(), decl); // insert with reverse order
		//	}
		//}

		//core::TypePtr lambdaRetType = converter.convertType(cleanupExpr->getType());

		//if(innerIr->getType() != lambdaRetType && !gen.isRef(lambdaRetType)) {
		//	// if (core::analysis::isCallOf(innerIr, mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize()))
		//	if(core::analysis::isCallOf(innerIr, mgr.getLangBasic().getRefVar())) {
		//		innerIr = innerIr.as<core::CallExprPtr>().getArgument(0);
		//	} else {
		//		if(core::analysis::isAnyCppRef(innerIr->getType())) { innerIr = core::analysis::unwrapCppRef(innerIr); }

		//		innerIr = converter.tryDeref(innerIr);
		//	}
		//}

		//if(stmtList.empty()) {
		//	// we avoided all expressions to be cleanup, no extra lambda needed
		//	VLOG(2) << "	cleanup expression is simplyfied and avoided";
		//	return retIr = innerIr;
		//}

		//// if the expression does not return anything, do not add return stmt
		//if(gen.isUnit(innerIr->getType())) {
		//	stmtList.push_back(innerIr);
		//} else {
		//	if(core::encoder::isListType(innerIr->getType())) {
		//		vector<core::ExpressionPtr> retList = core::encoder::toValue<vector<core::ExpressionPtr>, core::encoder::DirectExprListConverter>(innerIr);
		//		if(core::encoder::isListType(retList[0]->getType())) {
		//			retList = core::encoder::toValue<vector<core::ExpressionPtr>, core::encoder::DirectExprListConverter>(retList[0]);
		//		}
		//		for(core::ExpressionPtr& expr : retList) {
		//			expr = builder.deref(expr);
		//		}

		//		innerIr = builder.callExpr(lambdaRetType, mgr.getLangBasic().getGenInit(), builder.getTypeLiteral(lambdaRetType), builder.tupleExpr(retList));
		//	}
		//	stmtList.push_back(converter.builder.returnStmt(innerIr));
		//}

		//// inline the list,
		//return retIr = detail::inlineExpressionWithCleanups(stmtList);

		assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					ScalarValueInitExpr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXScalarValueInitExpr(const clang::CXXScalarValueInitExpr* scalarValueInit) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(scalarValueInit, retIr);

		//core::TypePtr elemType = converter.convertType(scalarValueInit->getTypeSourceInfo()->getType());
		//retIr = converter.defaultInitVal(elemType);

		assert_not_implemented();

		return retIr;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					Materialize temporary expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitMaterializeTemporaryExpr(const clang::MaterializeTemporaryExpr* materTempExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(materTempExpr, retIr);
		retIr = Visit(materTempExpr->GetTemporaryExpr());

		// we don't need to materialize POD types, Inspire semantics allow implicit materialization
		if(materTempExpr->getType().isPODType(converter.getCompiler().getASTContext())) {
			return retIr;
		}

		//if(VLOG_IS_ON(2)) {
		//	VLOG(2) << " =============== Materialize! =================" << std::endl;
		//	materTempExpr->dump();
		//	VLOG(2) << "inner: ";
		//	dumpPretty(retIr);
		//	VLOG(2) << "type: ";
		//	dumpPretty(retIr->getType());
		//	VLOG(2) << "expected: ";
		//	core::TypePtr t = converter.convertType(materTempExpr->getType());
		//	VLOG(2) << dumpPretty(t);
		//}

		//// if (! t.isa<core::RefTypePtr>())
		////	return retIr;

		//// if inner expression is a bind temporary, we do not need to materialize, is IR-correct
		//if(llvm::isa<clang::CXXBindTemporaryExpr>(materTempExpr->GetTemporaryExpr())) { return retIr; }
		//if(llvm::isa<clang::CXXNewExpr>(materTempExpr->GetTemporaryExpr())) { return (retIr = builder.refVar(retIr)); }
		//// return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), retIr));

		//// inner type is a pointer? materialize
		//if((retIr->getType().isa<core::RefTypePtr>()) && (retIr->getType().as<core::RefTypePtr>()->getElementType()->getNodeType() == core::NT_ArrayType)) {
		//	return (retIr = builder.refVar(retIr));
		//}
		//// return (retIr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), retIr));


		//if(core::analysis::isAnyCppRef(retIr->getType()) || gen.isRef(retIr->getType())) {
		//	return retIr;
		//} else {
		//	return (retIr = builder.refVar(retIr));
		//}

		assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					Typeid expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXTypeidExpr(const clang::CXXTypeidExpr* typeidExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(typeidExpr, retIr);

		//core::ExpressionPtr expr;
		//if(typeidExpr->isTypeOperand()) {
		//	expr = builder.getTypeLiteral(converter.convertType(typeidExpr->getTypeOperand(converter.getCompiler().getASTContext())));
		//} else {
		//	expr = Visit(typeidExpr->getExprOperand());
		//}
		//retIr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getTypeid(), expr);

		assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					Default init expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXDefaultInitExpr(const clang::CXXDefaultInitExpr* initExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(initExpr, retIr);
		retIr = converter.convertExpr(initExpr->getExpr());
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//			Substituted non type template parameter expression
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitSubstNonTypeTemplateParmExpr(const clang::SubstNonTypeTemplateParmExpr* substExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(substExpr, retIr);
		//frontend_assert(substExpr->getReplacement()) << "template parameter cannot be substituted by nothing\n";
		//retIr = Visit(substExpr->getReplacement());

		assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//			Binary member pointer Direct
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitBinPtrMemD(const clang::BinaryOperator* exprD) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(exprD, retIr);
		assert_not_implemented();
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//			Binary member pointer indirect
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitBinPtrMemI(const clang::BinaryOperator* exprI) {
		// indirect, ->*
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(exprI, retIr);
		assert_not_implemented();
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//		    Binary trait
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitTypeTraitExpr(const clang::TypeTraitExpr* typeTraitExpr) {
		// this is found when using __base_of operator, clang gives already the boolean expression evaluated,
		// is an static type resolutions, we just forward this result
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(typeTraitExpr, retIr);
		retIr = converter.builder.boolLit(typeTraitExpr->getValue());
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//		SizeOfPack expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitSizeOfPackExpr(const clang::SizeOfPackExpr* sizeOfPackExpr) {
		// sizeOf... returns size_t --> use unsigned int
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(sizeOfPackExpr, retIr);
		retIr = builder.uintLit(sizeOfPackExpr->getPackLength());
		return retIr;
	}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
