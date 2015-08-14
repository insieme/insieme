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

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/macros.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/unused.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/frontend_ir_builder.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/lang/ir++_extension.h"

using namespace insieme;
using namespace insieme::frontend::utils;

namespace insieme {
namespace frontend {
namespace utils {
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Takes a clang::CastExpr, converts its subExpr into IR and wraps it with the necessary IR casts
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	core::ExpressionPtr performClangCastOnIR(insieme::frontend::conversion::Converter& converter, const clang::CastExpr* castExpr) {
		core::ExpressionPtr expr = converter.convertExpr(castExpr->getSubExpr());
		//core::TypePtr targetTy = converter.convertType(castExpr->getType());
		//core::TypePtr exprTy = expr->getType();

		//const core::FrontendIRBuilder& builder = converter.getIRBuilder();
		//const core::lang::BasicGenerator& gen = builder.getLangBasic();
		//core::NodeManager& mgr = converter.getNodeManager();

		//if(VLOG_IS_ON(2)) {
		//	VLOG(2) << "####### Expr: #######";
		//	VLOG(2) << (expr);
		//	VLOG(2) << "####### Expr Type: #######";
		//	VLOG(2) << (exprTy);
		//	VLOG(2) << "####### cast result Type: #######";
		//	VLOG(2) << (targetTy);
		//	VLOG(2) << "####### clang: #######" << std::endl;
		//	castExpr->dump();
		//}

		//// it might be that the types are already fixed:
		//// like LtoR in arrays, they will always be a ref<...>
		//if(*exprTy == *targetTy) { return expr; }

		//// handle implicit casts according to their kind
		//switch(castExpr->getCastKind()) {
		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_LValueToRValue:
		//	// A conversion which causes the extraction of an r-value from the operand gl-value.
		//	// The result of an r-value conversion is always unqualified.
		//	//
		//	// IR: this is the same as out ref deref ref<a'> -> a'
		//	{
		//		// we use by value a member accessor. we have a better operation for this
		//		// instead of derefing the memberRef
		//		//  refElem   (ref<owner>, elemName) -> ref<member>   => this is composite ref
		//		//  membAcces ( owner, elemName) -> member            => uses read-only, returns value
		//		if(core::CallExprPtr call = expr.isa<core::CallExprPtr>()) {
		//			if(core::analysis::isCallOf(call, gen.getCompositeRefElem())
		//			   && (!core::analysis::isCallOf(call, mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR())
		//			       && !core::analysis::isCallOf(call, mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR()))) {
		//				expr = builder.callExpr(gen.getCompositeMemberAccess(), builder.deref(call[0]), call[1], builder.getTypeLiteral(targetTy));
		//			}
		//			// TODO: we can do something similar and turn vector ref elem into vectorSubscript
		//			// else if (core::analysis::isCallOf(call, gen.getVectorRefElem())) {
		//			else if(expr->getType().isa<core::RefTypePtr>()) {
		//				expr = builder.deref(expr);
		//			}
		//		} else if(expr->getType().isa<core::RefTypePtr>()) {
		//			expr = builder.deref(expr);
		//		}

		//		// this is CppRef -> ref
		//		if(core::analysis::isAnyCppRef(expr->getType())) { expr = builder.deref(builder.toIRRef(expr)); }

		//		return expr;
		//		break;
		//	}
		////////////////////////////////////////////////////////////////////////////////////////////////////////////

		//case clang::CK_IntegralCast:
		//// A cast between integral types (other than to boolean). Variously a bitcast, a truncation,
		//// a sign-extension, or a zero-extension. long l = 5; (unsigned) i
		//// IR: convert to int or uint
		//case clang::CK_IntegralToBoolean:
		//// Integral to boolean. A check against zero. (bool) i
		//case clang::CK_IntegralToFloating:
		//// Integral to floating point. float f = i;
		//case clang::CK_FloatingToIntegral:
		//// Floating point to integral. Rounds towards zero, discarding any fractional component.
		//// (int) f
		//case clang::CK_FloatingToBoolean:
		//// Floating point to boolean. (bool) f
		//case clang::CK_FloatingCast:
		//	// Casting between floating types of different size. (double) f (float) ld
		//	{
		//		assert(
		//		    builder.getLangBasic().isPrimitive(expr->getType())
		//		    || (core::analysis::isVolatileType(expr->getType()) && builder.getLangBasic().isPrimitive(core::analysis::getVolatileType(expr->getType())))
		//		    || mgr.getLangExtension<core::lang::EnumExtension>().isEnumType(expr->getType()));
		//		assert(builder.getLangBasic().isPrimitive(targetTy) || mgr.getLangExtension<core::lang::EnumExtension>().isEnumType(targetTy));
		//		return core::types::smartCast(targetTy, expr);
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_NoOp:
		//	// A conversion which does not affect the type other than (possibly) adding qualifiers. i
		//	// int -> int char** -> const char * const *
		//	{
		//		if(core::analysis::isCppRef(exprTy)) {
		//			return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToConstCpp(), expr);
		//		}
		//		if(core::analysis::isRValCppRef(exprTy)) {
		//			return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefRValCppToConstCpp(), expr);
		//		}

		//		// types equality has been already checked, if is is a NoOp is because clang identifies
		//		// this as the same type. but we might intepret it in a diff way. (ex, char literals are
		//		// int for clang, we build a char type
		//		if(gen.isPrimitive(exprTy)) {
		//			return core::types::castScalar(targetTy, expr);
		//		} else {
		//			return expr;
		//		}
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_ArrayToPointerDecay:
		//	// Array to pointer decay. int[10] -> int* char[5][6] -> char(*)[6]
		//	{
		//		// if inner expression is not ref.... it might be a compound initializer
		//		if(!IS_IR_REF(exprTy)) { expr = builder.callExpr(gen.getRefVar(), expr); }

		//		if(core::analysis::isAnyCppRef(exprTy)) { expr = core::analysis::unwrapCppRef(expr); }

		//		if(core::types::isRefArray(expr->getType())) { return expr; }

		//		if(targetTy.isa<core::FunctionTypePtr>()) { return expr; }

		//		if(targetTy.as<core::RefTypePtr>()->isSource()) {
		//			return builder.callExpr(gen.getRefVectorToSrcArray(), expr);
		//		} else {
		//			return builder.callExpr(gen.getRefVectorToRefArray(), expr);
		//		}
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_BitCast:
		//	// A conversion which causes a bit pattern of one type to be reinterpreted as a bit pattern
		//	// of another type.
		//	// Generally the operands must have equivalent size and unrelated types.  The pointer conversion
		//	// char* -> int* is a bitcast. A conversion from any pointer type to a C pointer type is a bitcast unless
		//	// it's actually BaseToDerived or DerivedToBase. A conversion to a block pointer or ObjC pointer type is a
		//	// bitcast only if the operand has the same type kind; otherwise, it's one of the specialized casts below.
		//	// Vector coercions are bitcasts.
		//	{
		//		// char* -> const char* is a bitcast in clang, and not a Noop, but we drop qualifiers
		//		if(core::types::isSubTypeOf(exprTy, targetTy)) { return expr; }

		//		// cast to void*
		//		if(gen.isAnyRef(targetTy)) { return builder.callExpr(targetTy, gen.getRefReinterpret(), expr, builder.getTypeLiteral(targetTy)); }

		//		// is a cast of Null to another pointer type:
		//		// we rebuild null
		//		if(*expr == *builder.literal(expr->getType(), "0") || gen.isRefNull(expr)) { return builder.getZero(targetTy); }

		//		if(targetTy->getNodeType() == core::NT_FunctionType) { return builder.castExpr(targetTy, expr); }

		//		// cast from void*
		//		if(gen.isAnyRef(exprTy)) {
		//			core::TypePtr elementType = core::analysis::getReferencedType(targetTy);
		//			return builder.callExpr(targetTy, gen.getRefReinterpret(), expr, builder.getTypeLiteral(elementType));
		//		}

		//		// otherwise, we just reinterpret
		//		core::ExpressionPtr innerExpr = expr;
		//		if(gen.isRefDeref(expr)) {
		//			// clang does LtoR always, but we want refs in the cast. if there is a deref in the inner expression remove it
		//			innerExpr = expr.as<core::LambdaExprPtr>()->getParameterList()[0];
		//		}
		//		return builder.callExpr(targetTy, gen.getRefReinterpret(), innerExpr, builder.getTypeLiteral(GET_REF_ELEM_TYPE(targetTy)));
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_VectorSplat:
		//	// A conversion from an arithmetic type to a vector of that element type. Fills all elements
		//	//("splats") with the source value. __attribute__((ext_vector_type(4))) int v = 5;
		//	{ return builder.callExpr(gen.getVectorInitUniform(), expr, builder.getIntTypeParamLiteral(targetTy.as<core::VectorTypePtr>()->getSize())); }

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_IntegralToPointer:
		//	// Integral to pointer. A special kind of reinterpreting conversion. Applies to normal,
		//	// ObjC, and block pointers. (char*) 0x1001aab0 reinterpret_cast<int*>(0)
		//	{
		//		// is a cast of Null to another pointer type:
		//		// we rebuild null
		//		if(*expr == *builder.literal(expr->getType(), "0") || gen.isRefNull(expr)) {
		//			return builder.getZero(targetTy);
		//		} else {
		//			assert(targetTy.isa<core::RefTypePtr>());
		//			core::TypePtr elemTy = targetTy.as<core::RefTypePtr>()->getElementType();
		//			return builder.callExpr(targetTy, gen.getIntToRef(), toVector(expr, builder.getTypeLiteral(elemTy)));
		//		}
		//		break;
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_ConstructorConversion:
		//	// Conversion by constructor. struct A { A(int); }; A a = A(10);
		//	{
		//		// this should be handled by backend compiler
		//		// http://stackoverflow.com/questions/1384007/conversion-constructor-vs-conversion-operator-precedence
		//		return expr;
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_PointerToBoolean:
		//	// case clang::CK_PointerToBoolean - Pointer to boolean conversion. A check against null. Applies to normal, ObjC,
		//	// and block pointers.
		//	{
		//		if(expr->getType().isa<core::FunctionTypePtr>()) {
		//			return builder.callExpr(gen.getBoolLNot(), builder.callExpr(gen.getBool(), gen.getFuncIsNull(), expr));
		//		}
		//		return builder.callExpr(gen.getBoolLNot(), builder.callExpr(gen.getBool(), gen.getRefIsNull(), expr));
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_FloatingRealToComplex:
		//case clang::CK_IntegralRealToComplex:
		//	return builder.callExpr(mgr.getLangExtension<core::lang::ComplexExtension>().getConstantToComplex(), expr);
		///*A conversion of a floating point real to a floating point complex of the original type. Injects the value as the
		//* real component with a zero imaginary component. float -> _Complex float.
		//* */
		///*Converts from an integral real to an integral complex whose element type matches the source. Injects the value as the
		//* real component with a zero imaginary component. long -> _Complex long.
		//* */

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_FloatingComplexCast:
		//case clang::CK_FloatingComplexToIntegralComplex:
		//case clang::CK_IntegralComplexCast:
		//case clang::CK_IntegralComplexToFloatingComplex:
		//	return mgr.getLangExtension<core::lang::ComplexExtension>().castComplexToComplex(expr, targetTy);
		///*Converts between different floating point complex types. _Complex float -> _Complex double.
		//* */
		///*Converts from a floating complex to an integral complex. _Complex float -> _Complex int.
		//* */
		///*Converts between different integral complex types. _Complex char -> _Complex long long _Complex unsigned int ->
		//* _Complex signed int.
		//* */
		///*Converts from an integral complex to a floating complex. _Complex unsigned -> _Complex float.
		//* */

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_FloatingComplexToReal:
		//case clang::CK_IntegralComplexToReal:
		//	return mgr.getLangExtension<core::lang::ComplexExtension>().getReal(expr);
		///*Converts a floating point complex to floating point real of the source's element type. Just discards the imaginary
		//* component. _Complex long double -> long double.
		//* */
		///*Converts an integral complex to an integral real of the source's element type by discarding the imaginary component.
		//* _Complex short -> short.
		//* */


		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_IntegralComplexToBoolean:
		//case clang::CK_FloatingComplexToBoolean:
		//	/*Converts a complex to bool by comparing against 0+0i.
		//	* */
		//	return mgr.getLangExtension<core::lang::ComplexExtension>().castComplexToBool(expr);

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_LValueBitCast:
		//	/* case clang::CK_LValueBitCast - A conversion which reinterprets the address of an l-value as an l-value of a different
		//	* kind. Used for reinterpret_casts of l-value expressions to reference types. bool b; reinterpret_cast<char&>(b) = 'a';
		//	* */
		//	{
		//		// if we have a cpp ref we have to unwrap the expression
		//		if(core::analysis::isAnyCppRef(expr->getType())) { expr = builder.toIRRef(expr); }
		//		// the target type is a ref type because lvalue
		//		// bitcasts look like reinterpret_cast<type&>(x)
		//		targetTy = builder.refType(targetTy);
		//		core::CallExprPtr newExpr = builder.callExpr(targetTy, gen.getRefReinterpret(), expr, builder.getTypeLiteral(GET_REF_ELEM_TYPE(targetTy)));
		//		// wrap it as cpp ref
		//		return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), newExpr);
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_NullToPointer:
		//	// Null pointer constant to pointer, ObjC pointer, or block pointer. (void*) 0;
		//	{
		//		if(gen.isAnyRef(targetTy)) { return gen.getRefNull(); }

		//		// if( targetTy.isa<core::RefTypePtr>() && core::analysis::getReferencedType(targetTy).isa<core::FunctionTypePtr>() ) {
		//		if(targetTy.isa<core::FunctionTypePtr>()) { return builder.callExpr(targetTy, gen.getNullFunc(), builder.getTypeLiteral(targetTy)); }

		//		// cast NULL to anything else
		//		if(((targetTy->getNodeType() == core::NT_RefType)) && (*expr == *builder.literal(expr->getType(), "0") || gen.isRefNull(expr))) {
		//			return builder.getZero(targetTy);
		//		} else {
		//			// it might be that is a reinterpret cast already, we can avoid chaining
		//			if(core::analysis::isCallOf(expr, gen.getRefReinterpret())) { expr = expr.as<core::CallExprPtr>()[0]; }

		//			// it might be a function type e.g. fun(void *(**) (size_t)) {} called with fun(__null)
		//			if(core::dynamic_pointer_cast<const core::FunctionType>(targetTy)) { return builder.getZero(targetTy); }

		//			core::TypePtr elementType = core::analysis::getReferencedType(targetTy);
		//			assert_true(elementType) << "cannot build ref reinterpret without a type";
		//			return builder.callExpr(targetTy, gen.getRefReinterpret(), expr, builder.getTypeLiteral(elementType));
		//		}
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_MemberPointerToBoolean:
		//	/*case clang::CK_MemberPointerToBoolean - Member pointer to boolean. A check against the null member pointer.
		//	* */
		//	{
		//		if(expr->getType().isa<core::FunctionTypePtr>()) {
		//			return builder.callExpr(gen.getBoolLNot(), builder.callExpr(gen.getBool(), gen.getFuncIsNull(), expr));
		//		}
		//		frontend_assert(core::analysis::isMemberPointer(expr->getType())) << " not a memberPointer? " << expr << " : " << expr->getType();

		//		return core::analysis::getMemberPointerCheck(expr);
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		////  PARTIALY IMPLEMENTED
		////////////////////////////////////////////////////////////////////////////////////////////////////////////

		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_ToVoid:
		//	// Cast to void, discarding the computed value. (void) malloc(2048)
		//	{
		//		// this cast ignores inner expression, is not very usual, but might show off when dealing
		//		// with null pointer.
		//		if(gen.isUnit(targetTy)) { return gen.getUnitConstant(); }
		//	}

		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_UncheckedDerivedToBase:
		//case clang::CK_DerivedToBase:
		//	// A conversion from a C++ class pointer to a base class pointer. A *a = new B();
		//	{
		//		// TODO: do we need to check if is pointerType?
		//		// in case of pointer, the inner expression is modeled as ref< array < C, 1> >
		//		// it is needed to deref the first element
		//		expr = getCArrayElemRef(builder, expr);

		//		// unwrap CppRef if CppRef
		//		if(core::analysis::isAnyCppRef(exprTy)) { expr = core::analysis::unwrapCppRef(expr); }

		//		clang::CastExpr::path_const_iterator it;
		//		for(it = castExpr->path_begin(); it != castExpr->path_end(); ++it) {
		//			core::TypePtr targetTy = converter.convertType((*it)->getType());
		//			// if it is no ref we have to materialize it, otherwise refParent cannot be called
		//			if(expr->getType()->getNodeType() != core::NT_RefType) {
		//				// expr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getMaterialize(), expr);
		//				expr = builder.refVar(expr);
		//			}
		//			expr = builder.refParent(expr, targetTy);
		//		}

		//		if(castExpr->getType().getTypePtr()->isPointerType()) {
		//			// is a pointer type -> return pointer
		//			expr = builder.callExpr(gen.getScalarToArray(), expr);
		//		}

		//		VLOG(2) << "cast resoult: \n" << dumpPretty(expr) << " \n of type: " << expr->getType();
		//		return expr;
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_BaseToDerived:
		//	// A conversion from a C++ class pointer/reference to a derived class pointer/reference. B *b = static_cast<B*>(a);
		//	{
		//		// we want to know the TYPE of static_cast<TYPE>()
		//		targetTy = converter.convertType(llvm::dyn_cast<clang::ExplicitCastExpr>(castExpr)->getType());
		//		VLOG(2) << exprTy << " " << targetTy;

		//		core::ExpressionPtr retIr;

		//		// pointers:
		//		if(core::analysis::isPointerType(exprTy)) {
		//			assert_true(core::analysis::isPointerType(targetTy)) << "from pointer to non pointer is not possible";
		//			targetTy = targetTy.as<core::RefTypePtr>()->getElementType();
		//			return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getStaticCast(), expr, builder.getTypeLiteral((targetTy)));
		//		}

		//		// NORE: All value casts are upgraded to CPP ref, this has no implications for the generated code since lvalues in clang are already refs.
		//		// 		 and makes everithing smoother and easyer
		//		if(exprTy.isa<core::RefTypePtr>()) {
		//			expr = builder.toCppRef(expr);
		//			exprTy = expr.getType();
		//		}

		//		if(core::analysis::isCppRef(exprTy)) {
		//			if(!core::analysis::isCppRef(targetTy)) { targetTy = core::analysis::getCppRef(targetTy); }
		//			retIr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getStaticCastRefCppToRefCpp(), expr,
		//			                         builder.getTypeLiteral((targetTy)));
		//		} else if(core::analysis::isConstCppRef(exprTy)) {
		//			if(!core::analysis::isCppRef(targetTy)) { targetTy = core::analysis::getConstCppRef(targetTy); }
		//			retIr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getStaticCastConstCppToConstCpp(), expr,
		//			                         builder.getTypeLiteral((targetTy)));
		//		} else {
		//			std::cerr << " === BASE TO DERIVED FAILED ===========" << std::endl;
		//			std::cerr << "####### Expr: #######" << std::endl;
		//			std::cerr << (expr);
		//			std::cerr << "\n####### Expr Type: #######" << std::endl;
		//			std::cerr << (exprTy);
		//			std::cerr << "\n####### cast Type: #######" << std::endl;
		//			std::cerr << (targetTy);
		//			std::cerr << "\n####### clang: #######" << std::endl;
		//			castExpr->dump();
		//			abort();
		//		}

		//		return retIr;
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_Dynamic:
		//	// A C++ dynamic_cast.
		//	{
		//		// we want to know the TYPE of static_cast<TYPE>()
		//		targetTy = converter.convertType(llvm::dyn_cast<clang::ExplicitCastExpr>(castExpr)->getType());
		//		VLOG(2) << exprTy << " " << targetTy;

		//		core::ExpressionPtr retIr;

		//		// pointers:
		//		if(core::analysis::isPointerType(exprTy)) {
		//			assert_true(core::analysis::isPointerType(targetTy)) << "from pointer to non pointer is not possible";
		//			targetTy = targetTy.as<core::RefTypePtr>()->getElementType();
		//			return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getDynamicCast(), expr, builder.getTypeLiteral((targetTy)));
		//		}

		//		// NORE: All value casts are upgraded to CPP ref, this has no implications for the generated code since lvalues in clang are already refs.
		//		// 		 and makes everithing smoother and easyer
		//		if(exprTy.isa<core::RefTypePtr>()) {
		//			expr = builder.toCppRef(expr);
		//			exprTy = expr.getType();
		//		}

		//		if(core::analysis::isCppRef(exprTy)) {
		//			if(!core::analysis::isCppRef(targetTy)) { targetTy = core::analysis::getCppRef(targetTy); }
		//			retIr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getDynamicCastRefCppToRefCpp(), expr,
		//			                         builder.getTypeLiteral((targetTy)));
		//		} else if(core::analysis::isConstCppRef(exprTy)) {
		//			if(!core::analysis::isCppRef(targetTy)) { targetTy = core::analysis::getConstCppRef(targetTy); }
		//			retIr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getDynamicCastConstCppToConstCpp(), expr,
		//			                         builder.getTypeLiteral((targetTy)));
		//		} else {
		//			std::cerr << " === Dynamic cast  FAILED ===========" << std::endl;
		//			std::cerr << "####### Expr: #######" << std::endl;
		//			std::cerr << (expr);
		//			std::cerr << "\n####### Expr Type: #######" << std::endl;
		//			std::cerr << (exprTy);
		//			std::cerr << "\n####### cast Type: #######" << std::endl;
		//			std::cerr << (targetTy);
		//			std::cerr << "\n####### clang: #######" << std::endl;
		//			castExpr->dump();
		//			abort();
		//		}

		//		return retIr;
		//	}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_PointerToIntegral:
		//	// CK_PointerToIntegral - Pointer to integral. A special kind of reinterpreting conversion. Applies to normal,
		//	// ObjC, and block pointers. (intptr_t) "help!"
		//	{ return builder.callExpr(gen.getUInt8(), gen.getRefToInt(), expr); }

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_FunctionToPointerDecay:
		//	// CK_FunctionToPointerDecay - Function to pointer decay. void(int) -> void(*)(int)
		//	{ return expr; }

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_NullToMemberPointer:
		//	/*case clang::CK_NullToMemberPointer - Null pointer constant to member pointer.
		//	 * int A::*mptr = 0; int (A::*fptr)(int) = nullptr;
		//	 */
		//	{ return builder.callExpr(targetTy, gen.getNullFunc(), builder.getTypeLiteral(targetTy)); }

		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//case clang::CK_UserDefinedConversion:
		//	/*case clang::CK_UserDefinedConversion - Conversion using a user defined type conversion function.i
		//	* struct A { operator int(); }; int i = int(A());
		//	* */
		//	{ return converter.convertExpr(castExpr->getSubExpr()); }

		//case clang::CK_BuiltinFnToFnPtr: {
		//	return expr;
		//}


		////////////////////////////////////////////////////////////////////////////////////////////////////////////
		////  NOT IMPLEMENTED
		////////////////////////////////////////////////////////////////////////////////////////////////////////////

		//case clang::CK_Dependent:
		///*case clang::CK_Dependent - A conversion which cannot yet be analyzed because either the expression
		//* or target type is dependent. These are created only for explicit casts; dependent ASTs aren't
		//* required to even approximately type-check. (T*) malloc(sizeof(T)) reinterpret_cast<intptr_t>(A<T>::alloc());
		//* */

		//case clang::CK_ToUnion:
		///*case clang::CK_ToUnion - The GCC cast-to-union extension. i
		//* int -> union { int x; float y; } float -> union { int x; float y; }
		//* */

		//case clang::CK_BaseToDerivedMemberPointer:
		///*case clang::CK_BaseToDerivedMemberPointer - Member pointer in base class to member pointer in derived class.
		// * int B::*mptr = &A::member;
		//* */

		//case clang::CK_DerivedToBaseMemberPointer:
		///*case clang::CK_DerivedToBaseMemberPointer - Member pointer in derived class to member pointer in base class.
		//* int A::*mptr = static_cast<int A::*>(&B::member);
		//* */

		//case clang::CK_ReinterpretMemberPointer:
		///*case clang::CK_ReinterpretMemberPointer - Reinterpret a member pointer as a different kind of member pointer.
		//* C++ forbids this from crossing between function and object types, but otherwise does not restrict it.
		//* However, the only operation that is permitted on a "punned" member pointer is casting it back to the original type,
		//* which is required to be a lossless operation (although many ABIs do not guarantee this on all possible intermediate types).
		//* */

		//case clang::CK_CPointerToObjCPointerCast:
		///*case clang::CK_CPointerToObjCPointerCast - Casting a C pointer kind to an Objective-C pointer.
		//* */

		//case clang::CK_BlockPointerToObjCPointerCast:
		///*case clang::CK_BlockPointerToObjCPointerCast - Casting a block pointer to an ObjC pointer.
		//* */

		//case clang::CK_AnyPointerToBlockPointerCast:
		///*case clang::CK_AnyPointerToBlockPointerCast - Casting any non-block pointer to a block pointer. Block-to-block casts are bitcasts.
		//* */

		//case clang::CK_ObjCObjectLValueCast:
		///*Converting between two Objective-C object types, which can occur when performing reference binding to an Objective-C object.
		//* */

		//case clang::CK_ARCProduceObject:
		///*[ARC] Produces a retainable object pointer so that it may be consumed, e.g. by being passed to a consuming parameter.
		//* Calls objc_retain.
		//* */

		//case clang::CK_ARCConsumeObject:
		///*[ARC] Consumes a retainable object pointer that has just been produced, e.g. as the return value of a retaining call.
		//* Enters a cleanup to call objc_release at some indefinite time.
		//* */

		//case clang::CK_ARCReclaimReturnedObject:
		///*[ARC] Reclaim a retainable object pointer object that may have been produced and autoreleased as part of a function
		//* return sequence.
		//* */

		//case clang::CK_ARCExtendBlockObject:
		///*[ARC] Causes a value of block type to be copied to the heap, if it is not already there. A number of other operations in
		//* ARC cause blocks to be copied; this is for cases where that would not otherwise be guaranteed, such as when casting to a
		//* non-block pointer type.
		//* */

		//case clang::CK_AtomicToNonAtomic:
		//case clang::CK_NonAtomicToAtomic:
		//case clang::CK_CopyAndAutoreleaseBlockObject:
		//	std::cout << " \nCAST: " << castExpr->getCastKindName() << " not supported!!" << std::endl;
		//	std::cout << " at location: " << frontend::utils::location(castExpr->getLocStart(), converter.getSourceManager()) << std::endl;
		//	castExpr->dump();
		//	assert_fail();
		//default: assert_fail() << "not all options listed, is this clang 3.2? maybe should upgrade Clang support";
		//}

		assert_fail() << "control reached an invalid point!";

		return expr;
	}

} // end utils namespace
} // end frontend namespace
} // end insieme namespace
