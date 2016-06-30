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

#include "insieme/core/lang/array.h"
#include "insieme/core/test/test_utils.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace lang {

	TEST(Array, SemanticChecks) {
		NodeManager nm;
		auto& ext = nm.getLangExtension<ArrayExtension>();
		semanticCheckSecond(ext.getSymbols());
	}

	TEST(Array, setSize) {
		NodeManager nm;
		IRBuilder b(nm);

		auto A = b.parseType("A");
		auto B = b.parseType("array<A>");
		auto C = b.parseType("array<A,5>");
		auto D = b.parseType("array<A,v0>");


		auto array = ArrayType(B);

		auto lit = b.parseExpr("lit(\"5\" : uint<4>)");
		array.setSize(lit);
		EXPECT_EQ(5, array.getNumElements());

		VariablePtr var = Variable::get(nm, nm.getLangBasic().getUIntInf());
		array.setSize(var);
		EXPECT_EQ(var, array.getSize().as<VariablePtr>());

	}

	TEST(Array, IsArray) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto A = builder.parseType("A");
		ExpressionPtr l = builder.literal(builder.parseType("uint<inf>"), "4");
		ExpressionPtr v = builder.variable(builder.parseType("uint<inf>"), 0);

		std::map<string,NodePtr> symbols;
		symbols["x"] = v;

		EXPECT_TRUE(isArray(ArrayType::create(A)));
		EXPECT_TRUE(isArray(ArrayType::create(A, 15)));
		EXPECT_TRUE(isArray(ArrayType::create(A, l)));
		EXPECT_TRUE(isArray(ArrayType::create(A, v)));

		EXPECT_FALSE(isArray(builder.parseType("A")));
		EXPECT_TRUE(isArray(builder.parseType("array<A>")));
		EXPECT_TRUE(isArray(builder.parseType("array<A,12>")));
		EXPECT_TRUE(isArray(builder.parseType("array<A,#x>", symbols)));

		EXPECT_EQ(ArrayType::create(A), builder.parseType("array<A>"));
		EXPECT_EQ(ArrayType::create(A), builder.parseType("array<A,inf>"));
		EXPECT_EQ(ArrayType::create(A,15), builder.parseType("array<A,15>"));
		EXPECT_EQ(ArrayType::create(A,v), builder.parseType("array<A,#x>", symbols));

		EXPECT_PRED1(isUnknownSizedArray, builder.parseType("array<A>"));
		EXPECT_PRED1(isFixedSizedArray, builder.parseType("array<A,12>"));
		EXPECT_PRED1(isVariableSizedArray, builder.parseType("array<A,#x>", symbols));
		EXPECT_PRED1(isGenericSizedArray, builder.parseType("array<A,'a>"));

		EXPECT_FALSE(isUnknownSizedArray(builder.parseType("array<A,12>")));
		EXPECT_FALSE(isUnknownSizedArray(builder.parseType("array<A,#x>", symbols)));
		EXPECT_FALSE(isUnknownSizedArray(builder.parseType("array<A,'a>")));

		EXPECT_FALSE(isFixedSizedArray(builder.parseType("array<A>")));
		EXPECT_FALSE(isFixedSizedArray(builder.parseType("array<A,#x>", symbols)));
		EXPECT_FALSE(isFixedSizedArray(builder.parseType("array<A,'a>")));

		EXPECT_FALSE(isVariableSizedArray(builder.parseType("array<A>")));
		EXPECT_FALSE(isVariableSizedArray(builder.parseType("array<A,12>")));
		EXPECT_FALSE(isVariableSizedArray(builder.parseType("array<A,'a>")));

		EXPECT_FALSE(isGenericSizedArray(builder.parseType("array<A>")));
		EXPECT_FALSE(isGenericSizedArray(builder.parseType("array<A,12>")));
		EXPECT_FALSE(isGenericSizedArray(builder.parseType("array<A,#x>", symbols)));
	}

	TEST(Array, Alias) {

		// test whether the array alias is working

		NodeManager nm;
		IRBuilder builder(nm);

		auto t1 = builder.parseType("array<int<4>>");
		auto t2 = builder.parseType("array<int<4>,5>");
		auto t3 = builder.parseType("array<int<4>,inf>");

		EXPECT_PRED1(isArray, t1);
		EXPECT_PRED1(isArray, t2);
		EXPECT_PRED1(isArray, t3);

		EXPECT_EQ("array<int<4>,inf>", toString(*t1));
		EXPECT_EQ("array<int<4>,5>", toString(*t2));
		EXPECT_EQ("array<int<4>,inf>", toString(*t3));

	}

	TEST(Array, Reduce) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto& ext = mgr.getLangExtension<ArrayExtension>();

		auto l = builder.literal(builder.parseType("uint<inf>"), "4");
		auto v = builder.variable(builder.parseType("uint<inf>"), 0);

        auto A     = builder.parseType("A");
        auto B     = builder.parseType("B");
        auto array1 = builder.literal(ReferenceType::create(ArrayType::create(A)), "dummy");
        auto array2 = builder.literal(ReferenceType::create(ArrayType::create(A, 10)), "dummy");
        auto array3 = builder.literal(ReferenceType::create(ArrayType::create(A, l)), "dummy");
        auto array4 = builder.literal(ReferenceType::create(ArrayType::create(A, v)), "dummy");

        auto size = builder.literal(builder.getLangBasic().getInt8(), "10");
        auto init = builder.literal(A, "initVal");
        auto init2= builder.literal(B, "initVal");

        IRBuilder::EagerDefinitionMap symbols;
        symbols["A"] = A;
        symbols["B"] = B;
        auto f = builder.parseExpr("(a : A, b : A) -> A { }", symbols);
        auto f2 = builder.parseExpr("(a : B, b : A) -> B { }", symbols);

        // with generic binary operator
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array1, size, builder.getLangBasic().getGenAdd(), init));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array1, size, builder.getLangBasic().getGenAdd(), init)->getType(), A);

        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array2, size, builder.getLangBasic().getGenAdd(), init));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array2, size, builder.getLangBasic().getGenAdd(), init)->getType(), A);

        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array3, size, builder.getLangBasic().getGenAdd(), init));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array3, size, builder.getLangBasic().getGenAdd(), init)->getType(), A);

        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array4, size, builder.getLangBasic().getGenAdd(), init));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array4, size, builder.getLangBasic().getGenAdd(), init)->getType(), A);

        // in/out same type
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array1, size, f, init));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array1, size, f, init)->getType(), A);

        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array2, size, f, init));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array2, size, f, init)->getType(), A);

        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array3, size, f, init));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array3, size, f, init)->getType(), A);

        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array4, size, f, init));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array4, size, f, init)->getType(), A);

        // in/out different types
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array1, size, f2, init2));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array1, size, f2, init2)->getType(), B);

        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array2, size, f2, init2));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array2, size, f2, init2)->getType(), B);

        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array3, size, f2, init2));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array3, size, f2, init2)->getType(), B);

        EXPECT_TRUE (builder.callExpr(A, ext.getArrayReduce(), array4, size, f2, init2));
        EXPECT_EQ   (builder.callExpr(ext.getArrayReduce(), array4, size, f2, init2)->getType(), B);

    }

	TEST(Array, Fold) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto& ext = mgr.getLangExtension<ArrayExtension>();

		auto l = builder.literal(builder.parseType("uint<inf>"), "4");
		auto v = builder.variable(builder.parseType("uint<inf>"), 0);

        auto A     = builder.parseType("A");
        auto B     = builder.parseType("B");
        auto array1 = builder.literal(ArrayType::create(A), "dummy");
        auto array2 = builder.literal(ArrayType::create(A, 10), "dummy");
        auto array3 = builder.literal(ArrayType::create(A, l), "dummy");
        auto array4 = builder.literal(ArrayType::create(A, v), "dummy");

        auto size = builder.literal(builder.getLangBasic().getInt8(), "10");
        auto init = builder.literal(A, "initVal");
        auto init2= builder.literal(B, "initVal");

        IRBuilder::EagerDefinitionMap symbols;
        symbols["A"] = A;
        symbols["B"] = B;
        auto f = builder.parseExpr("(a : A, b : A) -> A { }", symbols);
        auto f2 = builder.parseExpr("(a : B, b : A) -> B { }", symbols);

        // with generic binary operator
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array1, init, builder.getLangBasic().getGenAdd()));
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array2, init, builder.getLangBasic().getGenAdd()));
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array3, init, builder.getLangBasic().getGenAdd()));
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array4, init, builder.getLangBasic().getGenAdd()));

        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array1, init, builder.getLangBasic().getGenAdd())->getType(), A);
        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array2, init, builder.getLangBasic().getGenAdd())->getType(), A);
        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array3, init, builder.getLangBasic().getGenAdd())->getType(), A);
        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array4, init, builder.getLangBasic().getGenAdd())->getType(), A);

        // in/out same type
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array1, init, f));
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array2, init, f));
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array3, init, f));
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array4, init, f));

        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array1, init, f)->getType(), A);
        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array2, init, f)->getType(), A);
        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array3, init, f)->getType(), A);
        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array4, init, f)->getType(), A);
        
        // in/out different types
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array1, init2, f2));
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array2, init2, f2));
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array3, init2, f2));
        EXPECT_TRUE (builder.callExpr(A, ext.getArrayFold(), array4, init2, f2));

        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array1, init2, f2)->getType(), B);
        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array2, init2, f2)->getType(), B);
        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array3, init2, f2)->getType(), B);
        EXPECT_EQ (builder.callExpr(ext.getArrayFold(), array4, init2, f2)->getType(), B);
      
    }

	TEST(Array, Pointwise) {

		// test the type of a pointwise operator
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto& basic = mgr.getLangBasic();
		auto& ext = mgr.getLangExtension<ArrayExtension>();

		// check the resulting type of the pointwise operator
		EXPECT_EQ("((array<int<'a>,'l>,array<int<'a>,'l>)->array<int<'a>,'l>)", toString(*builder.callExpr(ext.getArrayPointwise(), basic.getSignedIntAdd())->getType()));
		EXPECT_EQ("((array<bool,'l>,array<bool,'l>)->array<bool,'l>)", toString(*builder.callExpr(ext.getArrayPointwise(), basic.getBoolEq())->getType()));
		EXPECT_EQ("((array<uint<'a>,'l>,array<uint<'a>,'l>)->array<bool,'l>)", toString(*builder.callExpr(ext.getArrayPointwise(), basic.getUnsignedIntGt())->getType()));
	}

	TEST(Array, IsArrayInit) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto notArrayInit = [](const NodePtr& node){ return !isArrayInit(node); };

		EXPECT_PRED1(notArrayInit, builder.parseExpr("5"));
		EXPECT_PRED1(notArrayInit, NodePtr());
		EXPECT_PRED1(notArrayInit, builder.parseType("struct A {}"));
		EXPECT_PRED1(notArrayInit, builder.parseExpr("<ref<struct A {}>>{}"));

		EXPECT_PRED1(isArrayInit, builder.parseExpr("<ref<array<int<4>,5>>>{}"));
		EXPECT_PRED1(isArrayInit, builder.parseExpr("<ref<array<int<4>,5>>>{1,2,3,4,5}"));
		EXPECT_EQ(*isArrayInit(builder.parseExpr("<ref<array<int<4>,5>>>{1,2,3,4,5}")), ArrayType(builder.parseType("array<int<4>,5>")));
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
