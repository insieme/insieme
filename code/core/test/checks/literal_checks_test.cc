/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace checks {

	TEST(LiteralFormat, BoolLiterals) {
		NodeManager manager;
		IRBuilder builder(manager);


		// there are two correct literals
		NodePtr ok1 = builder.boolLit(true);
		NodePtr ok2 = builder.boolLit(false);

		// those should be fine
		EXPECT_TRUE(check(ok1).empty()) << check(ok1);
		EXPECT_TRUE(check(ok2).empty()) << check(ok2);

		TypePtr boolType = manager.getLangBasic().getBool();
		NodePtr err1 = builder.literal(boolType, "0");
		NodePtr err2 = builder.literal(boolType, "1");
		NodePtr err3 = builder.literal(boolType, "True");
		NodePtr err4 = builder.literal(boolType, "False");

		// those should all be wrong
		EXPECT_EQ(1u, check(err1).size());
		EXPECT_EQ(1u, check(err2).size());
		EXPECT_EQ(1u, check(err3).size());
		EXPECT_EQ(1u, check(err4).size());
	}

	TEST(LiteralFormat, FloatLiterals) {
		NodeManager manager;
		IRBuilder builder(manager);

		// there are two correct literals
		NodePtr ok1 = builder.floatLit(0.3f);

		// those should be fine
		EXPECT_TRUE(check(ok1).empty()) << check(ok1);

		TypePtr floatType = manager.getLangBasic().getReal4();
		NodePtr err1 = builder.literal(floatType, "ad");
		NodePtr err2 = builder.literal(floatType, "'a'");
		NodePtr err3 = builder.literal(floatType, "0.03");
		NodePtr err4 = builder.literal(floatType, ".03");

		// those should all be wrong
		EXPECT_EQ(1u, check(err1).size());
		EXPECT_EQ(1u, check(err2).size());
		EXPECT_EQ(0, check(err3).size()); // lets just ignore this for a chance
		EXPECT_EQ(1u, check(err4).size());
	}

	TEST(LiteralFormat, DoubleLiterals) {
		NodeManager manager;
		IRBuilder builder(manager);

		// there are two correct literals
		NodePtr ok1 = builder.doubleLit(0.3);

		// those should be fine
		EXPECT_TRUE(check(ok1).empty()) << check(ok1);

		TypePtr doubleType = manager.getLangBasic().getReal8();
		NodePtr err1 = builder.literal(doubleType, "ad");
		NodePtr err2 = builder.literal(doubleType, "'a'");
		NodePtr err3 = builder.literal(doubleType, "0.03f");
		NodePtr err4 = builder.literal(doubleType, ".03");

		// those should all be wrong
		EXPECT_EQ(1u, check(err1).size());
		EXPECT_EQ(1u, check(err2).size());
		EXPECT_EQ(1u, check(err3).size());
		EXPECT_EQ(1u, check(err4).size());

		// test de-normalized values
		ok1 = builder.doubleLit(5e-324);
		EXPECT_TRUE(check(ok1).empty()) << check(ok1);
	}

	TEST(LiteralFormat, IntLiterals) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto& basic = manager.getLangBasic();

		// there are two correct literals
		NodePtr node;

		// stuff that should work
		node = builder.intLit(12);
		EXPECT_TRUE(check(node).empty()) << "\nNode: " << node << "\nError: " << check(node);

		node = builder.uintLit(12);
		EXPECT_TRUE(check(node).empty()) << "\nNode: " << node << "\nError: " << check(node);

		node = builder.uintLit(0);
		EXPECT_TRUE(check(node).empty()) << "\nNode: " << node << "\nError: " << check(node);


		// stuff that should not
		node = builder.literal(basic.getUInt4(), "ab");
		EXPECT_EQ(1u, check(node).size());

		node = builder.literal(basic.getUInt4(), "'a'");
		EXPECT_EQ(1u, check(node).size());

		node = builder.literal(basic.getUInt4(), "0.9");
		EXPECT_EQ(1u, check(node).size());


		// stuff that is out of bound
		node = builder.literal(basic.getUInt1(), "1000");
		EXPECT_EQ(1u, check(node).size());
	}


	TEST(LiteralFormat, limitsMax) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();
		std::vector<std::pair<TypePtr, std::string>> types;


		EXPECT_EQ(0, check(builder.literal(basic.getInt1(), toString(INT8_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getInt2(), toString(INT16_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getInt4(), toString(INT32_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getInt8(), toString(INT64_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getInt16(), toString(INT64_MAX))).size());

		EXPECT_EQ(0, check(builder.literal(basic.getInt1(), toString(INT8_MIN))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getInt2(), toString(INT16_MIN))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getInt4(), toString(INT32_MIN))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getInt8(), toString(INT64_MIN))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getInt16(), toString(INT64_MIN))).size());

		EXPECT_EQ(0, check(builder.literal(basic.getUInt1(), toString(UINT8_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getUInt2(), toString(UINT16_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getUInt4(), toString(UINT32_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getUInt8(), toString(UINT64_MAX) + "u")).size());
		EXPECT_EQ(1, check(builder.literal(basic.getUInt8(), toString(UINT64_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getUInt8(), toString(UINT64_MAX) + "u")).size());
		EXPECT_EQ(1, check(builder.literal(basic.getUInt8(), toString(UINT64_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getUInt16(), toString(UINT64_MAX) + "u")).size());
		EXPECT_EQ(1, check(builder.literal(basic.getUInt16(), toString(UINT64_MAX))).size());

		EXPECT_EQ(0, check(builder.literal(basic.getReal4(), toString(0.0))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getReal4(), toString(.0))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getReal4(), toString(190))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getReal4(), toString(-190.0))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getReal4(), toString(-.01))).size());

		EXPECT_EQ(0, check(builder.literal(basic.getReal4(), toString(FLT_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getReal4(), toString(FLT_MIN))).size());

		EXPECT_EQ(0, check(builder.literal(basic.getReal8(), toString(-1e-10))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getReal8(), toString(DBL_MAX))).size());
		EXPECT_EQ(0, check(builder.literal(basic.getReal8(), toString(DBL_MIN))).size());
	}

} // end namespace checks
} // end namespace core
} // end namespace insieme
