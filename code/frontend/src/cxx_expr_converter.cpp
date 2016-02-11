/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/state/record_manager.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/expr_to_bool.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/stmt_wrapper.h"
#include "insieme/frontend/utils/temporaries_lookup.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/name_mangling.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"


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

		auto funExp = irCall->getFunctionExpr();
		auto funType = funExp->getType().as<core::FunctionTypePtr>();

		// in Inspire 2.0, copy and move constructor calls are implicit on function calls
		ExpressionList newArgs;
		size_t i = 0;
		std::transform(callExpr->arg_begin(), callExpr->arg_end(), std::back_inserter(newArgs), [&](const clang::Expr* clangArgExpr) {
			return convertCxxArgExpr(clangArgExpr, funType->getParameterType(i++));
		});

		return builder.callExpr(convertExprType(callExpr), funExp, newArgs);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  MEMBER EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitMemberExpr(const clang::MemberExpr* membExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(membExpr, retIr);

		retIr = ExprConverter::VisitMemberExpr(membExpr);

		// TODO: does this need to do anything special? If not, remove and add to header

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

		const core::IRBuilder& builder = converter.builder;
		const CXXMethodDecl* methodDecl = callExpr->getMethodDecl();

		if(!methodDecl) {
			// member function pointer call
			frontend_assert(false) << "Member function pointer call not implemented";
		} else {
			// get method lambda
			auto methodLambda = converter.getFunMan()->lookup(methodDecl);

			// get the "this" object and add to arguments
			core::ExpressionPtr thisObj = Visit(callExpr->getImplicitObjectArgument());
			if(core::lang::isPointer(thisObj)) {
				thisObj = core::lang::buildPtrToRef(thisObj);
			}
			core::ExpressionList arguments { thisObj };

			// in Inspire 2.0, copy and move constructor calls are implicit on function calls, and ref kind needs to be adapted
			auto funType = methodLambda->getType().as<core::FunctionTypePtr>();
			size_t i = 1;
			std::transform(callExpr->arg_begin(), callExpr->arg_end(), std::back_inserter(arguments), [&](const clang::Expr* clangArgExpr) {
				auto targetType = funType->getParameterType(i++);
				auto ret = convertCxxArgExpr(clangArgExpr, targetType);
				VLOG(2) << "====================\n\nconvert method argument:\n"
					    << "\n - from: " << dumpClang(clangArgExpr, converter.getCompiler().getSourceManager()) << "\n - to: " << dumpPretty(ret)
					    << "\n - of type: " << dumpPretty(ret->getType()) << "\n - target T: " << dumpPretty(targetType);
				return ret;
			});

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

		retIr = VisitCallExpr(callExpr);

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

		//assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX CONSTRUCTOR CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	namespace {
		/// Convert a ConstructExpr to an IR constructor call, allocating the required memory either on the stack (default) or on the heap
		core::ExpressionPtr convertConstructExprInternal(Converter& converter, const clang::CXXConstructExpr* constructExpr, bool onStack) {
			auto& builder = converter.getIRBuilder();
			core::TypePtr resType = converter.convertType(constructExpr->getType());

			VLOG(2) << "convertConstructExprInternal - ResType: \n" << dumpDetailColored(resType) << " recType:\n"
				    << (*converter.getIRTranslationUnit().getTypes().find(resType.as<core::GenericTypePtr>())).second << "\n";

			// first constructor argument is the object memory location -- we need to build this either on the stack or heap
			auto irMemLoc = onStack ? core::lang::buildRefTemp(resType) : builder.undefinedNew(resType);

			// get constructor lambda
			auto constructorLambda = converter.getFunMan()->lookup(constructExpr->getConstructor());
			auto lambdaParamTypes = constructorLambda.getType().as<core::FunctionTypePtr>().getParameterTypeList();
			VLOG(2) << "constructor lambda literal " << *constructorLambda << " of type " << dumpColor(constructorLambda->getType());

			// the constructor is then simply a call with the mem location and all its arguments
			core::ExpressionList arguments { irMemLoc };
			size_t i = 1;
			for(auto arg : constructExpr->arguments()) {
				arguments.push_back(converter.convertCxxArgExpr(arg, lambdaParamTypes[i++]));
			}

			// return call
			auto retType = constructorLambda->getType().as<core::FunctionTypePtr>()->getReturnType();
			return builder.callExpr(retType, constructorLambda, arguments);
		}
	}

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
	namespace {
		/// Resize the array created by an array create call (required due to mismatched size reported for initializer expressions in new[])
		core::ExpressionPtr resizeArrayCreate(const Converter& converter, core::ExpressionPtr createExpr, const core::ExpressionPtr& newSize) {
			auto& nodeMan = createExpr->getNodeManager();
			auto& arrExt = nodeMan.getLangExtension<core::lang::ArrayExtension>();
			auto& refExt = nodeMan.getLangExtension<core::lang::ReferenceExtension>();
			bool hasDeref = refExt.isCallOfRefDeref(createExpr);
			if(hasDeref) createExpr = core::analysis::getArgument(createExpr, 0);
			
			auto initExpr = createExpr.isa<core::InitExprPtr>();
			frontend_assert(initExpr) << "Trying to resize array creation, but expression is not init";

			auto arrGt = initExpr.getType();
			frontend_assert(core::lang::isReference(arrGt)) << "Inited type must be reference";
			auto subTy = core::analysis::getReferencedType(arrGt);
			frontend_assert(core::lang::isArray(subTy)) << "Expected array type, got " << *subTy;
			auto arrTy = core::lang::ArrayType(subTy);
			
			// need to implement this for non-const size (approach same as with C VLAs)
			auto form = core::arithmetic::toFormula(newSize);
			frontend_assert(form.isConstant()) << "Non const-sized new[] not yet implemented";
			arrTy.setSize(form.getIntegerValue());

			core::ExpressionPtr retIr = converter.getIRBuilder().initExprTemp(arrTy, initExpr.getInitExprList());
			if(hasDeref) retIr = converter.getIRBuilder().deref(retIr);
			return retIr;
		}
	}

	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXNewExpr(const clang::CXXNewExpr* newExpr) {
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(newExpr, retExpr);

		frontend_assert(newExpr->getNumPlacementArgs() == 0) << "Placement new not yet supported";

		// if no constructor is found, it is a new over a non-class type, can be any kind of pointer of array
		core::TypePtr type = converter.convertType(newExpr->getAllocatedType());
		if(!newExpr->getConstructExpr()) {
			// for arrays, we need to allocate the right size
			if(newExpr->isArray()) type = core::lang::ArrayType::create(type, converter.convertExpr(newExpr->getArraySize()));

			// build new expression depending on whether or not we have an initializer expression
			core::ExpressionPtr newExp;
			if(newExpr->hasInitializer()) {
				const clang::Expr* initializer = newExpr->getInitializer();
				core::ExpressionPtr initializerExpr = converter.convertInitExpr(initializer);
				frontend_assert(initializerExpr);
				// make sure we allocate the correct amount of array elements with partial initialization
				if(newExpr->isArray()) initializerExpr = resizeArrayCreate(converter, initializerExpr, converter.convertExpr(newExpr->getArraySize()));
				newExp = builder.refNew(initializerExpr);
			} else {
				newExp = builder.undefinedNew(type);
			}

			// initialize pointer either from ref for scalars or from array
			retExpr = newExpr->isArray() ? core::lang::buildPtrFromArray(newExp) : core::lang::buildPtrFromRef(newExp);
		}
		// we have a constructor, so we are building a class
		else {
			if(newExpr->isArray()) {
				retExpr = utils::buildObjectArrayNew(type, converter.convertExpr(newExpr->getArraySize()),
					                                 converter.getFunMan()->lookup(newExpr->getConstructExpr()->getConstructor()));
			} else {
				retExpr = core::lang::buildPtrFromRef(convertConstructExprInternal(converter, newExpr->getConstructExpr(), false));
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
		frontend_assert(core::lang::isPointer(exprToDelete)) << "\"delete\" called on non-pointer, not supported.";

		// make sure we delete a ref array if we have an array delete
		auto toDelete = core::lang::buildPtrToRef(exprToDelete);
		if(deleteExpr->isArrayForm()) toDelete = core::lang::buildPtrToArray(exprToDelete);

		// destructor calls are implicit in inspire 2.0, just like C++
		retExpr = converter.getIRBuilder().refDelete(toDelete);

		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX THIS EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXThisExpr(const clang::CXXThisExpr* thisExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(thisExpr, retIr);

		// obtain current "this" from variable manager
		auto thisRef = converter.getVarMan()->getThis();
		// this is a pointer, not a reference
		retIr = core::lang::buildPtrFromRef(converter.getIRBuilder().deref(thisRef));

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

		// default arguments are handled just like any other argument
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX Bind Temporary expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXBindTemporaryExpr(const clang::CXXBindTemporaryExpr* bindTempExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(bindTempExpr, retIr);

		// temporary creation/destruction is implicit
		retIr = converter.convertExpr(bindTempExpr->getSubExpr());

		return retIr;

	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX Expression with cleanups
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitExprWithCleanups(const clang::ExprWithCleanups* cleanupExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(cleanupExpr, retIr);
		
		// temporary creation/destruction is implicit
		retIr = converter.convertExpr(cleanupExpr->getSubExpr());

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

		// if we are materializing the rvalue result of a function call, do it
		auto subCall = retIr.isa<core::CallExprPtr>();
		if(subCall) {
			auto materializedType = convertExprType(materTempExpr);
			if(core::analysis::isRefType(materializedType)) {
				core::lang::ReferenceType matRefType(materializedType);
				// if we are already materialized everything is fine
				if(core::lang::isPlainReference(retIr->getType())) return retIr;
				// otherwise, materialize
				frontend_assert(matRefType.getElementType() == retIr->getType()) << "Materializing to unexpected type " << dumpColor(matRefType.toType())
					                                                            << " from " << dumpColor(retIr->getType());
				// if call to deref, simply remove it
				auto& refExt = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
				if(refExt.isCallOfRefDeref(retIr)) {
					retIr = subCall->getArgument(0);
					return retIr;
				}
				// otherwise, materialize call
				retIr = builder.callExpr(builder.refType(retIr->getType()), subCall->getFunctionExpr(), subCall->getArguments());
			}
		}
		
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
	//							COMPOUND ASSINGMENT OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCompoundAssignOperator(const clang::CompoundAssignOperator* compOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(compOp, retIr);

		core::ExpressionPtr lhs = Visit(compOp->getLHS());
		core::ExpressionPtr rhs = Visit(compOp->getRHS());
		core::TypePtr exprTy = converter.convertType(compOp->getType());

		assert_true(core::lang::isReference(lhs->getType())) << "left side must be assignable";

		retIr = createBinaryExpression(exprTy, builder.deref(lhs), rhs, compOp);
		retIr = frontend::utils::buildCxxStyleAssignment(lhs, retIr);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  UNARY OPERATOR EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitUnaryOperator(const clang::UnaryOperator* unOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(unOp, retIr);

		auto subExpr = Visit(unOp->getSubExpr());
		auto exprType = convertExprType(unOp);

		if(core::lang::isCppReference(subExpr->getType())) {
			core::lang::ReferenceType plainRef(subExpr->getType());
			plainRef.setKind(core::lang::ReferenceType::Kind::Plain);
			subExpr = core::lang::buildRefCast(subExpr, plainRef.toType());
		}

		return retIr = createUnaryExpression(exprType, subExpr, unOp->getOpcode());
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitBinaryOperator(const clang::BinaryOperator* binOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(binOp, retIr);

		core::ExpressionPtr lhs = Visit(binOp->getLHS());
		core::ExpressionPtr rhs = Visit(binOp->getRHS());
		core::TypePtr exprTy = converter.convertType(binOp->getType());

		// we need to translate the semantics of Cxx-style assignments to a function call --------------------------------------------------------- ASSIGNMENT -
		if(binOp->getOpcode() == clang::BO_Assign) {
			retIr = utils::buildCxxStyleAssignment(lhs, rhs);
		}
		else {
			retIr = createBinaryExpression(exprTy, lhs, rhs, binOp);
		}

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

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//		CXXStdInitializerListExpr expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXStdInitializerListExpr(const clang::CXXStdInitializerListExpr* stdInitListExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(stdInitListExpr, retIr);
		//convert sub expression and types
		auto subEx = converter.convertExpr(stdInitListExpr->getSubExpr());
		auto subExType = stdInitListExpr->getSubExpr()->getType().getTypePtr();
		auto initListIRType = converter.convertType(stdInitListExpr->getType());
		auto recordType = llvm::dyn_cast<clang::RecordType>(stdInitListExpr->getType().getTypePtr()->getUnqualifiedDesugaredType());
		auto recordDecl = recordType->getAsCXXRecordDecl();
		frontend_assert(recordType && recordDecl) << "failed to get the std::initializer_list type declaration.";
		auto thisVar = core::lang::buildRefTemp(initListIRType);

		//extract size of sub expr
		frontend_assert(llvm::isa<clang::ConstantArrayType>(subExType)) << "std::initializer_list sub expression has no constant size array type.";
		auto numElements = llvm::cast<clang::ConstantArrayType>(subExType)->getSize().getSExtValue();

		//generate and insert mandatory std::initializer_list<T> ctors
		for(auto ctorDecl : recordDecl->ctors()) {
			core::LiteralPtr ctorLiteral = converter.getFunMan()->lookup(ctorDecl);
			core::LambdaExprPtr lam;
			if(ctorDecl->getNumParams() == 2) {
				//ctor decl: constexpr initializer_list<T>(T* x, size_t s)
				//create list of arguments (this type, array sub expression, and size of array)
				core::ExpressionList args { thisVar, core::lang::buildPtrFromArray(subEx),
				builder.numericCast(builder.uintLit(numElements), builder.getLangBasic().getUInt8()) };
				auto funType = ctorLiteral->getType().as<core::FunctionTypePtr>();
				core::VariableList params;
				for(const core::TypePtr& type : funType->getParameterTypes()) {
					params.push_back(builder.variable(core::transform::materialize(type)));
				}
				//add the ctor implementation to the IR TU --> std::initializer_list<T>(T* t, size_t s) { }
				lam = builder.lambdaExpr(funType, params, builder.compoundStmt());
				//build call to the specific constructor
				retIr = builder.callExpr(funType->getReturnType(), ctorLiteral, args);
			} else if(ctorDecl->isDefaultConstructor()) {
				// ctor decl: constexpr initializer_list<T>()
				auto typeMap = converter.getIRTranslationUnit().getTypes();
				const auto typeMapIt = typeMap.find(initListIRType.as<core::GenericTypePtr>());
				frontend_assert(typeMapIt != typeMap.end()) << "cannot extract IR tag type for std::initializer_list<T>.";
				const core::TagTypePtr tagType = typeMapIt->second;
				//create and add default constructor
				lam = builder.getDefaultConstructor(thisVar->getType(), {}, tagType->getStruct()->getFields());
			}
			if(lam) converter.getIRTranslationUnit().addFunction(ctorLiteral, lam);
		}

		//generate dtor
		if(recordDecl->getDestructor()) {
			core::LiteralPtr dtorLiteral = converter.getFunMan()->lookup(recordDecl->getDestructor());
			//create and add default destructor
			auto lam = builder.getDefaultDestructor(thisVar->getType());
			converter.getIRTranslationUnit().addFunction(dtorLiteral, lam);
		}

		//return call to special constructor
		frontend_assert(retIr) << "failed to convert std::initializer_list expression.";
		return retIr;
	}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
