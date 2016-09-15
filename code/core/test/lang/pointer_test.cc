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

} // end namespace lang
} // end namespace core
} // end namespace insieme
