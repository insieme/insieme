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

} // end namespace lang
} // end namespace core
} // end namespace insieme
