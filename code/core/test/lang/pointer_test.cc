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

#include <gtest/gtest.h>

#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/test/test_utils.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace lang {

	TEST(Pointer, SemanticChecks) {
		NodeManager nm;
		auto& ext = nm.getLangExtension<PointerExtension>();
		semanticCheckSecond(ext.getSymbols());
	}

	TEST(Pointer, StructSubstitute) {
		NodeManager nm;
		const PointerExtension& ext = nm.getLangExtension<PointerExtension>();

		// the generic pointer template should be a struct
		EXPECT_EQ(NT_TupleType, ext.getGenPtr()->getNodeType());

		// the arguments in the functions accepting a pointer should be expanded to a struct
		EXPECT_EQ(NT_TupleType, ext.getPtrCast()->getType().as<FunctionTypePtr>()->getParameterTypes()[0]->getNodeType());
	}

	TEST(Pointer, Alias) {

		// test whether the pointer alias is working

		NodeManager nm;
		IRBuilder builder(nm);

		auto t1 = builder.parseType("ptr<int<4>>");
		auto t2 = builder.parseType("ptr<int<4>,f,f>");
		auto t3 = builder.parseType("ptr<int<4>,f,t>");

		EXPECT_EQ(t1->getNodeType(), NT_TupleType);
		EXPECT_EQ(t2->getNodeType(), NT_TupleType);
		EXPECT_EQ(t3->getNodeType(), NT_TupleType);

		EXPECT_EQ(t1, t2);
		EXPECT_NE(t1, t3);
	}

	TEST(Pointer, IsPointer) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto A = builder.parseType("A");
		EXPECT_TRUE(isPointer(PointerType::create(A)));
		EXPECT_TRUE(isPointer(PointerType::create(A, false, true)));

		EXPECT_FALSE(isPointer(builder.parseType("A")));
		EXPECT_TRUE(isPointer(builder.parseType("ptr<A>")));
		EXPECT_TRUE(isPointer(builder.parseType("ptr<A,f,t>")));

		EXPECT_TRUE(isPointer(builder.parseType("ptr<A,f,t>")));

		EXPECT_FALSE(isPointer(builder.parseType("ptr<A,c,t>")));
		EXPECT_FALSE(isPointer(builder.parseType("ptr<A,f,t,c>")));

	}

	TEST(Pointer, Ptr_ref_convert) {

		NodeManager nm;
		IRBuilder builder(nm);
        auto& ext = nm.getLangExtension<PointerExtension>();

        auto A = builder.parseType("A");

        auto refExpr = builder.literal( ReferenceType::create( A), "expr");
        auto constRefExpr = builder.literal( ReferenceType::create( A, true), "expr");
        auto volatileRefExpr = builder.literal( ReferenceType::create( A, false, true), "expr");

            // Ptr from ref
        EXPECT_EQ(builder.callExpr(ext.getPtrFromRef(), refExpr)->getType(), builder.parseType("ptr<A>"));
        EXPECT_EQ(builder.callExpr(ext.getPtrFromRef(), constRefExpr)->getType(), builder.parseType("ptr<A,t,f>"));
        EXPECT_EQ(builder.callExpr(ext.getPtrFromRef(), volatileRefExpr)->getType(), builder.parseType("ptr<A,f,t>"));

            // Ptr to ref
        EXPECT_EQ(builder.callExpr(ext.getPtrToRef(), builder.literal(builder.parseType("ptr<A>"), "dummyPtr")    )->getType(), refExpr->getType());
        EXPECT_EQ(builder.callExpr(ext.getPtrToRef(), builder.literal(builder.parseType("ptr<A,t,f>"), "dummyPtr"))->getType(), constRefExpr->getType());
        EXPECT_EQ(builder.callExpr(ext.getPtrToRef(), builder.literal(builder.parseType("ptr<A,f,t>"), "dummyPtr"))->getType(), volatileRefExpr->getType());

    }

	TEST(Pointer, Ptr_array_convert) {

		NodeManager nm;
		IRBuilder builder(nm);
        auto& ext = nm.getLangExtension<PointerExtension>();

        auto A = builder.parseType("A");

		auto l = builder.literal(builder.parseType("uint<inf>"), "4");
		auto v = builder.variable(builder.parseType("uint<inf>"), 0);

        {

            auto array1 = builder.literal(ReferenceType::create(ArrayType::create(A)), "dummyArray");
            auto array2 = builder.literal(ReferenceType::create(ArrayType::create(A, 10)), "dummyArray");
            auto array3 = builder.literal(ReferenceType::create(ArrayType::create(A, l)), "dummyArray");
            auto array4 = builder.literal(ReferenceType::create(ArrayType::create(A, v)), "dummyArray");

            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array1)->getType(), builder.parseType("ptr<A>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array2)->getType(), builder.parseType("ptr<A>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array3)->getType(), builder.parseType("ptr<A>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array4)->getType(), builder.parseType("ptr<A>"));

        }

        {

            auto array1 = builder.literal(ReferenceType::create(ArrayType::create(A), true), "dummyArray");
            auto array2 = builder.literal(ReferenceType::create(ArrayType::create(A, 10), true), "dummyArray");
            auto array3 = builder.literal(ReferenceType::create(ArrayType::create(A, l), true), "dummyArray");
            auto array4 = builder.literal(ReferenceType::create(ArrayType::create(A, v), true), "dummyArray");

            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array1)->getType(), builder.parseType("ptr<A,t,f>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array2)->getType(), builder.parseType("ptr<A,t,f>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array3)->getType(), builder.parseType("ptr<A,t,f>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array4)->getType(), builder.parseType("ptr<A,t,f>"));

        }

        {

            auto array1 = builder.literal(ReferenceType::create(ArrayType::create(A), false, true), "dummyArray");
            auto array2 = builder.literal(ReferenceType::create(ArrayType::create(A, 10), false, true), "dummyArray");
            auto array3 = builder.literal(ReferenceType::create(ArrayType::create(A, l), false, true), "dummyArray");
            auto array4 = builder.literal(ReferenceType::create(ArrayType::create(A, v), false, true), "dummyArray");

            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array1)->getType(), builder.parseType("ptr<A,f,t>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array2)->getType(), builder.parseType("ptr<A,f,t>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array3)->getType(), builder.parseType("ptr<A,f,t>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array4)->getType(), builder.parseType("ptr<A,f,t>"));

        }

        {

            auto array1 = builder.literal(ReferenceType::create(ArrayType::create(A), true, true), "dummyArray");
            auto array2 = builder.literal(ReferenceType::create(ArrayType::create(A, 10), true, true), "dummyArray");
            auto array3 = builder.literal(ReferenceType::create(ArrayType::create(A, l), true, true), "dummyArray");
            auto array4 = builder.literal(ReferenceType::create(ArrayType::create(A, v), true, true), "dummyArray");

            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array1)->getType(), builder.parseType("ptr<A,t,t>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array2)->getType(), builder.parseType("ptr<A,t,t>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array3)->getType(), builder.parseType("ptr<A,t,t>"));
            EXPECT_EQ(builder.callExpr(ext.getPtrFromArray(), array4)->getType(), builder.parseType("ptr<A,t,t>"));

        }

    }

	TEST(Pointer, Ptr_function_convert) {
//		/**
//		 * A built-in derived operator for obtaining pointers to functions.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(PtrOfFunction, "ptr_of_function",
//				"(fun : 'a) -> ptr<'a,t,f> {"
//				"	return ptr_from_ref(ref_of_function(fun));"
//				"}"
//		)
		NodeManager nm;
		IRBuilder builder(nm);
        auto& ext = nm.getLangExtension<PointerExtension>();

        auto f = builder.parseExpr("() -> unit { } ");
        ASSERT_TRUE(f);
        auto fType = f->getType();

        EXPECT_TRUE(builder.callExpr(ext.getPtrOfFunction(), f));
        EXPECT_EQ(builder.callExpr(ext.getPtrOfFunction(), f)->getType(), PointerType::create(fType, true));


    }
    
	TEST(Pointer, Cast) {

		NodeManager nm;
		IRBuilder builder(nm);
        auto& ext = nm.getLangExtension<PointerExtension>();


        auto A = builder.parseType("A");
        auto B = builder.parseType("B");

        auto ptrExpr = builder.literal(PointerType::create(A), "dummy");
        auto constPtrExpr = builder.literal(PointerType::create(A, true), "dummy");
        auto volatilePtrExpr = builder.literal(PointerType::create(A, false, true), "dummy");

        EXPECT_EQ( builder.callExpr( ext.getPtrReinterpret(), ptrExpr, builder.getTypeLiteral(B))->getType(), PointerType::create(B) );
        EXPECT_EQ( builder.callExpr( ext.getPtrReinterpret(), constPtrExpr, builder.getTypeLiteral(B))->getType(), PointerType::create(B, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrReinterpret(), volatilePtrExpr, builder.getTypeLiteral(B))->getType(), PointerType::create(B, false, true) );

		auto t = builder.getTypeLiteral(builder.parseType("t"));
		auto f = builder.getTypeLiteral(builder.parseType("f"));

            // Cast
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), ptrExpr, t, t)->getType(), PointerType::create(A, true, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), ptrExpr, t, f)->getType(), PointerType::create(A, true, false) );
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), ptrExpr, f, t)->getType(), PointerType::create(A, false, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), ptrExpr, f, f)->getType(), PointerType::create(A, false, false) );

        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), constPtrExpr, t, t)->getType(), PointerType::create(A, true, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), constPtrExpr, t, f)->getType(), PointerType::create(A, true, false) );
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), constPtrExpr, f, t)->getType(), PointerType::create(A, false, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), constPtrExpr, f, f)->getType(), PointerType::create(A, false, false) );

        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), volatilePtrExpr, t, t)->getType(), PointerType::create(A, true, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), volatilePtrExpr, t, f)->getType(), PointerType::create(A, true, false) );
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), volatilePtrExpr, f, t)->getType(), PointerType::create(A, false, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrCast(), volatilePtrExpr, f, f)->getType(), PointerType::create(A, false, false) );

            // Constant Cast
        EXPECT_EQ( builder.callExpr( ext.getPtrConstCast(), ptrExpr, t)->getType(), PointerType::create(A, true, false) );
        EXPECT_EQ( builder.callExpr( ext.getPtrConstCast(), ptrExpr, f)->getType(), PointerType::create(A, false, false) );

        EXPECT_EQ( builder.callExpr( ext.getPtrConstCast(), constPtrExpr, t)->getType(), PointerType::create(A, true, false) );
        EXPECT_EQ( builder.callExpr( ext.getPtrConstCast(), constPtrExpr, f)->getType(), PointerType::create(A, false, false) );

        EXPECT_EQ( builder.callExpr( ext.getPtrConstCast(), volatilePtrExpr, t)->getType(), PointerType::create(A, true, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrConstCast(), volatilePtrExpr, f)->getType(), PointerType::create(A, false, true) );
            
            // Volatile Cast
        EXPECT_EQ( builder.callExpr( ext.getPtrVolatileCast(), ptrExpr, t)->getType(), PointerType::create(A, false, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrVolatileCast(), ptrExpr, f)->getType(), PointerType::create(A, false, false) );

        EXPECT_EQ( builder.callExpr( ext.getPtrVolatileCast(), constPtrExpr, t)->getType(), PointerType::create(A, true, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrVolatileCast(), constPtrExpr, f)->getType(), PointerType::create(A, true, false) );

        EXPECT_EQ( builder.callExpr( ext.getPtrVolatileCast(), volatilePtrExpr, t)->getType(), PointerType::create(A, false, true) );
        EXPECT_EQ( builder.callExpr( ext.getPtrVolatileCast(), volatilePtrExpr, f)->getType(), PointerType::create(A, false, false) );

            // integral (virtualy any type)
        auto address = builder.literal(builder.getLangBasic().getUInt8(), "addr");
        EXPECT_TRUE ( isPointer( builder.callExpr(ext.getPtrFromIntegral(), address, builder.getTypeLiteral(PointerType::create(A)))->getType()));
        EXPECT_EQ ( builder.callExpr(ext.getPtrToIntegral(), ptrExpr, builder.getTypeLiteral(builder.getLangBasic().getUInt8()))->getType(), builder.getLangBasic().getUInt8());
        EXPECT_EQ ( builder.callExpr(ext.getPtrToIntegral(), constPtrExpr, builder.getTypeLiteral(builder.getLangBasic().getUInt8()))->getType(), builder.getLangBasic().getUInt8());
        EXPECT_EQ ( builder.callExpr(ext.getPtrToIntegral(), volatilePtrExpr, builder.getTypeLiteral(builder.getLangBasic().getUInt8()))->getType(), builder.getLangBasic().getUInt8());
    
    }
    
	TEST(Pointer, SubReferencing) {
//		/**
//		 * The narrow operation is obtaining a reference to a sub-object within a referenced object.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(PtrNarrow, "ptr_narrow",
//				"  (p : ptr<'a,'c,'v>, dp : datapath<'a,'b>) -> ptr<'b,'c,'v> {                 "
//				"		return ptr_from_ref(ref_narrow(ptr_to_ref(p), dp));                        "
//				"  }                                                                               "
//		)
//
//		/**
//		 * The expand operation is the inverse operation of the narrow operation.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(PtrExpand, "ptr_expand",
//				"  (p : ptr<'b,'c,'v>, dp : datapath<'a,'b>) -> ptr<'a,'c,'v> {                 "
//				"		return ptr_from_ref(ref_expand(ptr_to_ref(p), dp));                        "
//				"  }                                                                               "
//		)
//
//		/**
//		 * A derived operator providing access to an element in an array.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(
//		    PtrArrayElement, "ptr_array_elem",
//		    "(r : ptr<array<'a,'s>,'c,'v>, i : int<8>) -> ptr<'a,'c,'v> { return ptr_narrow(r, dp_element(dp_root(type_lit(array<'a,'s>)),i)); }"
//		)
//
//		/**
//		 * A derived reference navigation operator providing access to a member of a struct / union.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(
//		    PtrMemberAccess, "ptr_member_access",
//		    "(r : ptr<'a,'c,'v>, name : identifier, type : type<'b>) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_member(dp_root(type_lit('a)),name,type)); }"
//		)
//
//		/**
//		 * A derived reference navigation operator providing access to a components of a tuple.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(
//		    PtrComponentAccess, "ptr_component_access",
//		    "(r : ptr<'a,'c,'v>, pos : uint<8>, type : type<'b>) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_component(dp_root(type_lit('a)),pos,type)); }"
//		)
//
//		/**
//		 * A derived reference-navigation operation providing an array view on a scalar.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrScalarToPtrArray, "ptr_scalar_to_ptr_array",
//		    "(a : ptr<'a,'c,'v>) -> ptr<array<'a>,'c,'v> { return ptr_expand(a, dp_element(dp_root(type_lit(array<'a>)),0u)); }"
//		)
//
//		/**
//		 * A derived operator accessing a element addressed by a pointer + some offset.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrSubscript, "ptr_subscript",
//			"(p : ptr<'a,'c,'v>, i : int<8>) -> ref<'a,'c,'v> { return p.0[p.1 + i]; }"
//		)
//
//		/**
//		 * A derived operator accessing a element addressed by a pointer.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrDeref, "ptr_deref",
//			"(p : ptr<'a,'c,'v>) -> 'a { return ref_deref(ptr_to_ref(p)); }"
//		)
//
    }
    
	TEST(Pointer, NullPtr) {
		NodeManager nm;
		IRBuilder builder(nm);
        auto& ext = nm.getLangExtension<PointerExtension>();

		auto A = builder.getTypeLiteral(builder.parseType("A"));
		auto t = builder.getTypeLiteral(builder.parseType("t"));
		auto f = builder.getTypeLiteral(builder.parseType("f"));

		    // Null 
        EXPECT_TRUE(isPointer(builder.callExpr(ext.getPtrNull(), A, t, t)->getType()));
        EXPECT_TRUE(isPointer(builder.callExpr(ext.getPtrNull(), A, t, f)->getType()));
        EXPECT_TRUE(isPointer(builder.callExpr(ext.getPtrNull(), A, f, t)->getType()));
        EXPECT_TRUE(isPointer(builder.callExpr(ext.getPtrNull(), A, f, f)->getType()));

    }
    
	TEST(Pointer, Comparison) {
//
//		// -- comparison operators --
//
//		/**
//		 * An operator to compare two references on equality.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrEqual, "ptr_eq",
//			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 == p2.1; }"
//		)
//
//		/**
//		 * An operator to compare two references for inequality.
//		 */
//		LANG_EXT_DERIVED_WITH_NAME(PtrNotEqual, "ptr_ne", "(a : ptr<'a,'c1,'v1>, b : ptr<'a,'c2,'v2>) -> bool { return !ptr_eq(a,b); }")
//
//
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrLessThan, "ptr_lt",
//			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 < p2.1; }"
//		)
//
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrLessEqual, "ptr_le",
//			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 <= p2.1; }"
//		)
//
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrGreaterEqual, "ptr_ge",
//			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 >= p2.1; }"
//		)
//
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrGreaterThan, "ptr_gt",
//			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 > p2.1; }"
//		)
//
//
    }
    
	TEST(Pointer, Arithmetic) {
//		// -- pointer arithmetic --
//
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrAdd, "ptr_add",
//			"(p : ptr<'a,'c,'v>, i : int<8>) -> ptr<'a,'c,'v> { return ( p.0, p.1 + i ); }"
//		)
//
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrSub, "ptr_sub",
//			"(p : ptr<'a,'c,'v>, i : int<8>) -> ptr<'a,'c,'v> { return ( p.0, p.1 - i ); }"
//		)
//
//		// pointer difference is not defined for unrelated pointers
//		LANG_EXT_DERIVED_WITH_NAME(
//			PtrDiff, "ptr_diff",
//			"(l : ptr<'a,'c,'v>, r : ptr<'a,'c,'v>) -> int<8> { return l.1 - r.1; }"
//		)
//
//		LANG_EXT_DERIVED_WITH_NAME(PtrPostInc, "ptr_post_inc", "(p : ref<ptr<'a,'c,'v>>) -> ptr<'a,'c,'v> { var ptr<'a,'c,'v> temp = *p; p = ptr_add(*p, 1l); return temp; }")
//
//		LANG_EXT_DERIVED_WITH_NAME(PtrPostDec, "ptr_post_dec", "(p : ref<ptr<'a,'c,'v>>) -> ptr<'a,'c,'v> { var ptr<'a,'c,'v> temp = *p; p = ptr_sub(*p, 1l); return temp; }")
//
//		LANG_EXT_DERIVED_WITH_NAME(PtrPreInc, "ptr_pre_inc", "(p : ref<ptr<'a,'c,'v>>) -> ptr<'a,'c,'v> { p = ptr_add(*p, 1l); return *p; }")
//
//		LANG_EXT_DERIVED_WITH_NAME(PtrPreDec, "ptr_pre_dec", "(p : ref<ptr<'a,'c,'v>>) -> ptr<'a,'c,'v> { p = ptr_sub(*p, 1l); return *p; }")
//
//	};
//
    }
    
	TEST(Pointer, Utilities) {
//	/**
//	 * Tests whether the given node is a pointer type or an expression of a pointer type.
//	 */
//	bool isPointer(const NodePtr& node);
//	
//	/**
//	 * Creates a new pointer type based on the given specification.
//	 */
//	TypePtr buildPtrType(const TypePtr& elementType, bool _const = false, bool _volatile = false);
//
//	// constructors and conversions
//	ExpressionPtr buildPtrNull(const TypePtr& type);
//	ExpressionPtr buildPtrFromRef(const ExpressionPtr& refExpr);
//	ExpressionPtr buildPtrToRef(const ExpressionPtr& ptrExpr);
//	ExpressionPtr buildPtrFromArray(const ExpressionPtr& arrExpr);
//	ExpressionPtr buildPtrFromIntegral(const ExpressionPtr& intExpr, const TypePtr& ptrType);
//	ExpressionPtr buildPtrToIntegral(const ExpressionPtr& ptrExpr, const TypePtr& intType);
//	ExpressionPtr buildPtrOfFunction(const ExpressionPtr& funExpr);
//
//	// casts
//	ExpressionPtr buildPtrCast(const ExpressionPtr& ptrExpr, bool newConst, bool newVolatile);
//	ExpressionPtr buildPtrReinterpret(const ExpressionPtr& ptrExpr, const TypePtr& newElementType);
//
//	// operations
//	ExpressionPtr buildPtrDeref(const ExpressionPtr& ptrExpr);
//	ExpressionPtr buildPtrSubscript(const ExpressionPtr& ptrExpr, const ExpressionPtr& subscriptExpr);
//	ExpressionPtr buildPtrOperation(BasicGenerator::Operator op, const ExpressionPtr& lhs, const ExpressionPtr& rhs);
//	ExpressionPtr buildPtrOperation(BasicGenerator::Operator op, const ExpressionPtr& ptrExpr);

    } 

} // end namespace lang
} // end namespace core
} // end namespace insieme
