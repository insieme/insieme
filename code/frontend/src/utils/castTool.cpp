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

#include "insieme/utils/logging.h"
#include "insieme/utils/unused.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"


// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include <clang/AST/Expr.h>

#define IS_IR_REF(type) \
	(type->getNodeType() == core::NT_RefType)

using namespace insieme;
using namespace insieme::frontend::utils;

namespace insieme {
namespace frontend {
namespace utils {


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
core::ExpressionPtr performClangCastOnIR (const insieme::core::IRBuilder& builder, 
										  const clang::CastExpr* castExpr, 
										  const core::TypePtr    targetTy,
										  const core::ExpressionPtr& expr){
	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	core::TypePtr&& exprTy = expr->getType();

	// handle implicit casts according to their kind
	switch (castExpr->getCastKind()) {

		case clang::CK_LValueToRValue 	:
		//CLANG: case clang::CK_LValueToRValue - A conversion which causes the extraction of an r-value from the operand gl-value. 
		// The result of an r-value conversion is always unqualified.
		//
		// IR: this is the same as out ref deref ref<a'> -> a'
		{
			if(IS_IR_REF(exprTy))
				return builder.deref(expr);

			std::cout << std::endl;
			dumpDetail (exprTy);
			assert(false && "try to deref a non valid type");
			break;
		}

		case clang::CK_IntegralCast 	:
		//CLANG: - A cast between integral types (other than to boolean). Variously a bitcast, a truncation,
		//a sign-extension, or a zero-extension. long l = 5; (unsigned) i
		//IR: convert to int or uint
		case clang::CK_IntegralToBoolean 	:
		// - Integral to boolean. A check against zero. (bool) i
		case clang::CK_IntegralToFloating 	:
		// - Integral to floating point. float f = i;
		case clang::CK_FloatingToIntegral 	:
		//case clang::CK_FloatingToIntegral - Floating point to integral. Rounds towards zero, discarding any fractional component.
		// (int) f
		case clang::CK_FloatingToBoolean 	:
		//case clang::CK_FloatingToBoolean - Floating point to boolean. (bool) f
		case clang::CK_FloatingCast 	:
		//case clang::CK_FloatingCast - Casting between floating types of different size. (double) f (float) ld
		{
			unsigned char code;
			// identify source type, to write right cast
			if (gen.isSignedInt (exprTy)) 	code = 1;
			if (gen.isUnsignedInt (exprTy)) code = 2;
			if (gen.isReal (exprTy))		code = 3;
			if (gen.isChar (exprTy))		code = 4;
			if (gen.isBool (exprTy))		code = 5;

			// identify target type
			if (gen.isSignedInt (targetTy)) 	code += 10;
			if (gen.isUnsignedInt (targetTy)) 	code += 20;
			if (gen.isReal (targetTy))			code += 30;
			if (gen.isChar (targetTy))			code += 40;
			if (gen.isBool (targetTy))			code += 50;

			core::ExpressionPtr op;
			switch(code){
				case 11: case 22: case 33:
				case 44: case 55: // no cast;
					assert(false && "no cast, something should be expected");
					break;
				case 12: op = gen.getUnsignedToInt(); break;
				case 13: op = gen.getRealToInt(); break;
				case 14: op = gen.getCharToInt(); break;
				case 15: op = gen.getBoolToInt(); break;

				case 21: op = gen.getSignedToUnsigned(); break;
				case 23: op = gen.getRealToUnsigned(); break;
				case 24: op = gen.getCharToUnsigned(); break;
				case 25: op = gen.getBoolToUnsigned(); break;

				case 31: op = gen.getSignedToReal(); break;
				case 32: op = gen.getUnsignedToReal(); break;
				case 34: op = gen.getCharToReal(); break;
				case 35: op = gen.getBoolToReal(); break;

				case 41: op = gen.getSignedToChar(); break;
				case 42: op = gen.getUnsignedToChar(); break;
				case 43: op = gen.getRealToChar(); break;
				case 45: op = gen.getBoolToChar(); break;

				case 51: op = gen.getSignedToBool(); break;
				case 52: op = gen.getUnsignedToBool(); break;
				case 53: op = gen.getRealToBool(); break;
				case 54: op = gen.getCharToBool(); break;

				default: assert(false && "cast not defined");
			}


			return builder.callExpr(targetTy, op, expr);
		}

		case clang::CK_Dependent 	:
		/*case clang::CK_Dependent - A conversion which cannot yet be analyzed because either the expression 
		* or target type is dependent. These are created only for explicit casts; dependent ASTs aren't 
		* required to even approximately type-check. (T*) malloc(sizeof(T)) reinterpret_cast<intptr_t>(A<T>::alloc());
		* */
		case clang::CK_BitCast 	:
		/*case clang::CK_BitCast - A conversion which causes a bit pattern of one type to be reinterpreted as a bit pattern 
		* of another type. 
		* Generally the operands must have equivalent size and unrelated types.  The pointer conversion 
		* char* -> int* is a bitcast. A conversion from any pointer type to a C pointer type is a bitcast unless 
		* it's actually BaseToDerived or DerivedToBase. A conversion to a block pointer or ObjC pointer type is a 
		* bitcast only if the operand has the same type kind; otherwise, it's one of the specialized casts below.  
		* Vector coercions are bitcasts.
		* */

		case clang::CK_LValueBitCast 	:
		/* case clang::CK_LValueBitCast - A conversion which reinterprets the address of an l-value as an l-value of a different 
		* kind. Used for reinterpret_casts of l-value expressions to reference types. bool b; reinterpret_cast<char&>(b) = 'a';
		* */

		case clang::CK_NoOp 	:

		/*case clang::CK_NoOp - A conversion which does not affect the type other than (possibly) adding qualifiers. i
		* int -> int char** -> const char * const *
		* */
		case clang::CK_BaseToDerived 	:

		/*case clang::CK_BaseToDerived - A conversion from a C++ class pointer/reference to a derived class
		* pointer/reference. B *b = static_cast<B*>(a);
		* */
		case clang::CK_DerivedToBase 	:

		/*case clang::CK_DerivedToBase - A conversion from a C++ class pointer to a base class pointer. A *a = new B();
		* */
		case clang::CK_UncheckedDerivedToBase 	:

		/*case clang::CK_UncheckedDerivedToBase - A conversion from a C++ class pointer/reference to a base class 
		* that can assume that the derived pointer is not null. const A &a = B(); b->method_from_a();
		* */
		case clang::CK_Dynamic 	:

		/*case clang::CK_Dynamic - A C++ dynamic_cast.
		* */
		case clang::CK_ToUnion 	:

		/*case clang::CK_ToUnion - The GCC cast-to-union extension. i
		* int -> union { int x; float y; } float -> union { int x; float y; }
		* */
		case clang::CK_ArrayToPointerDecay 	:

		/*case clang::CK_ArrayToPointerDecay - Array to pointer decay. int[10] -> int* char[5][6] -> char(*)[6]
		* */
		case clang::CK_FunctionToPointerDecay 	:

		/*case clang::CK_FunctionToPointerDecay - Function to pointer decay. void(int) -> void(*)(int)
		* */
		case clang::CK_NullToPointer 	:

		/*case clang::CK_NullToPointer - Null pointer constant to pointer, ObjC pointer, or block pointer.
		 * (void*) 0 void (^block)() = 0;
		* */
		case clang::CK_NullToMemberPointer 	:

		/*case clang::CK_NullToMemberPointer - Null pointer constant to member pointer.
		 * int A::*mptr = 0; int (A::*fptr)(int) = nullptr;
		* */
		case clang::CK_BaseToDerivedMemberPointer 	:

		/*case clang::CK_BaseToDerivedMemberPointer - Member pointer in base class to member pointer in derived class.
		 * int B::*mptr = &A::member;
		* */
		case clang::CK_DerivedToBaseMemberPointer 	:

		/*case clang::CK_DerivedToBaseMemberPointer - Member pointer in derived class to member pointer in base class.
		* int A::*mptr = static_cast<int A::*>(&B::member);
		* */
		case clang::CK_MemberPointerToBoolean 	:

		/*case clang::CK_MemberPointerToBoolean - Member pointer to boolean. A check against the null member pointer.
		* */
		case clang::CK_ReinterpretMemberPointer 	:

		/*case clang::CK_ReinterpretMemberPointer - Reinterpret a member pointer as a different kind of member pointer.
		* C++ forbids this from crossing between function and object types, but otherwise does not restrict it. 
		* However, the only operation that is permitted on a "punned" member pointer is casting it back to the original type,
		* which is required to be a lossless operation (although many ABIs do not guarantee this on all possible intermediate types).
		* */
		case clang::CK_UserDefinedConversion 	:

		/*case clang::CK_UserDefinedConversion - Conversion using a user defined type conversion function.i
		* struct A { operator int(); }; int i = int(A());
		* */
		case clang::CK_ConstructorConversion 	:

		/*case clang::CK_ConstructorConversion - Conversion by constructor. struct A { A(int); }; A a = A(10);
		* */
		case clang::CK_IntegralToPointer 	:

		/*case clang::CK_IntegralToPointer - Integral to pointer. A special kind of reinterpreting conversion. Applies to normal,
		* ObjC, and block pointers. (char*) 0x1001aab0 reinterpret_cast<int*>(0)
		* */
		case clang::CK_PointerToIntegral 	:

		/*case clang::CK_PointerToIntegral - Pointer to integral. A special kind of reinterpreting conversion. Applies to normal, 
		* ObjC, and block pointers. (intptr_t) "help!"
		* */
		case clang::CK_PointerToBoolean 	:

		/*case clang::CK_PointerToBoolean - Pointer to boolean conversion. A check against null. Applies to normal, ObjC, 
		 * and block pointers.
		* */
		case clang::CK_ToVoid 	:

		/*case clang::CK_ToVoid - Cast to void, discarding the computed value. (void) malloc(2048)
		* */
		case clang::CK_VectorSplat 	:
		/*case clang::CK_VectorSplat - A conversion from an arithmetic type to a vector of that element type. Fills all elements 
		* ("splats") with the source value. __attribute__((ext_vector_type(4))) int v = 5;
		* */

		case clang::CK_CPointerToObjCPointerCast 	:

		/*case clang::CK_CPointerToObjCPointerCast - Casting a C pointer kind to an Objective-C pointer.
		* */
		case clang::CK_BlockPointerToObjCPointerCast 	:

		/*case clang::CK_BlockPointerToObjCPointerCast - Casting a block pointer to an ObjC pointer.
		* */
		case clang::CK_AnyPointerToBlockPointerCast 	:

		/*case clang::CK_AnyPointerToBlockPointerCast - Casting any non-block pointer to a block pointer. Block-to-block casts are bitcasts.
		* */
		case clang::CK_ObjCObjectLValueCast 	:

		/*Converting between two Objective-C object types, which can occur when performing reference binding to an Objective-C object.
		* */
		case clang::CK_FloatingRealToComplex 	:

		/*A conversion of a floating point real to a floating point complex of the original type. Injects the value as the
		* real component with a zero imaginary component. float -> _Complex float.
		* */
		case clang::CK_FloatingComplexToReal 	:

		/*Converts a floating point complex to floating point real of the source's element type. Just discards the imaginary 
		* component. _Complex long double -> long double.
		* */
		case clang::CK_FloatingComplexToBoolean 	:

		/*Converts a floating point complex to bool by comparing against 0+0i.
		* */
		case clang::CK_FloatingComplexCast 	:

		/*Converts between different floating point complex types. _Complex float -> _Complex double.
		* */
		case clang::CK_FloatingComplexToIntegralComplex 	:

		/*Converts from a floating complex to an integral complex. _Complex float -> _Complex int.
		* */
		case clang::CK_IntegralRealToComplex 	:

		/*Converts from an integral real to an integral complex whose element type matches the source. Injects the value as the 
		* real component with a zero imaginary component. long -> _Complex long.
		* */
		case clang::CK_IntegralComplexToReal 	:

		/*Converts an integral complex to an integral real of the source's element type by discarding the imaginary component. 
		* _Complex short -> short.
		* */
		case clang::CK_IntegralComplexToBoolean 	:

		/*Converts an integral complex to bool by comparing against 0+0i.
		* */
		case clang::CK_IntegralComplexCast 	:

		/*Converts between different integral complex types. _Complex char -> _Complex long long _Complex unsigned int -> 
		* _Complex signed int.
		* */
		case clang::CK_IntegralComplexToFloatingComplex 	:

		/*Converts from an integral complex to a floating complex. _Complex unsigned -> _Complex float.
		* */
		case clang::CK_ARCProduceObject 	:

		/*[ARC] Produces a retainable object pointer so that it may be consumed, e.g. by being passed to a consuming parameter.
		* Calls objc_retain.
		* */
		case clang::CK_ARCConsumeObject 	:

		/*[ARC] Consumes a retainable object pointer that has just been produced, e.g. as the return value of a retaining call. 
		* Enters a cleanup to call objc_release at some indefinite time.
		* */
		case clang::CK_ARCReclaimReturnedObject 	:

		/*[ARC] Reclaim a retainable object pointer object that may have been produced and autoreleased as part of a function 
		* return sequence.
		* */
		case clang::CK_ARCExtendBlockObject 	:
  
		/*[ARC] Causes a value of block type to be copied to the heap, if it is not already there. A number of other operations in
		* ARC cause blocks to be copied; this is for cases where that would not otherwise be guaranteed, such as when casting to a 
		* non-block pointer type.
		* */
		case clang::CK_AtomicToNonAtomic 	:
		case clang::CK_NonAtomicToAtomic 	:
		case clang::CK_CopyAndAutoreleaseBlockObject 	:
		case clang::CK_BuiltinFnToFnPtr 	:
			std::cout << " \nCAST: " << castExpr->getCastKindName () << " not supported!!"<< std::endl;
			assert(false);
		default:
			assert(false && "not all options listed, is this clang 3.2? maybe should upgrade Clang support");
	}

}



} // end utils namespace
} // end frontend namespace 
} // end insieme namespace 

