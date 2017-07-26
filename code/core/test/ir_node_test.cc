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
 */

#include <gtest/gtest.h>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_values.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/timer.h"

namespace insieme {
namespace core {
namespace new_core {


	TEST(NodeType, Print) {
		EXPECT_EQ("TypeVariable", toString(NT_TypeVariable));
	}

	TEST(NodePtr, Casts) {
		NodeManager manager;

		ValuePtr value = BoolValue::get(manager, true);
		ValuePtr value2 = IntValue::get(manager, 12);

		EXPECT_TRUE(static_pointer_cast<const BoolValue>(value));
		EXPECT_TRUE(static_pointer_cast<BoolValuePtr>(value));

		EXPECT_TRUE(dynamic_pointer_cast<const BoolValue>(value));
		EXPECT_TRUE(dynamic_pointer_cast<BoolValuePtr>(value));

		EXPECT_FALSE(dynamic_pointer_cast<const BoolValue>(value2));
		EXPECT_FALSE(dynamic_pointer_cast<BoolValuePtr>(value2));
	}

	TEST(NodePtr, Access) {
		NodeManager manager;

		GenericTypePtr p = GenericType::get(manager, "A");

		EXPECT_TRUE(typeid(p->getName()) == typeid(StringValuePtr));
		EXPECT_TRUE(p->getName());
		EXPECT_EQ(StringValue::get(manager, "A"), p->getName());

		EXPECT_EQ(sizeof(void*), sizeof(NodePtr));
		EXPECT_EQ(sizeof(void*), sizeof(TupleTypePtr));
	}

	TEST(AddressPtr, Casts) {
		NodeManager manager;

		ValueAddress value(BoolValue::get(manager, true));
		ValueAddress value2(IntValue::get(manager, 12));

		EXPECT_TRUE(static_address_cast<const BoolValue>(value));
		EXPECT_TRUE(static_address_cast<BoolValueAddress>(value));

		EXPECT_TRUE(dynamic_address_cast<const BoolValue>(value));
		EXPECT_TRUE(dynamic_address_cast<BoolValueAddress>(value));

		EXPECT_FALSE(dynamic_address_cast<const BoolValue>(value2));
		EXPECT_FALSE(dynamic_address_cast<BoolValueAddress>(value2));

		//		EXPECT_EQ(sizeof(void*), sizeof(NodeAddress));
	}

	TEST(AddressPtr, Access) {
		NodeManager manager;

		GenericTypeAddress a(GenericType::get(manager, "A"));

		EXPECT_TRUE(typeid(a->getName()) == typeid(StringValueAddress));
		EXPECT_TRUE(a->getName());
		EXPECT_EQ("0", toString(a));
		EXPECT_EQ("0-0", toString(a->getName()));
		EXPECT_EQ(StringValue::get(manager, "A"), a->getName().getAddressedNode());
	}

	TEST(DISABLED_NodePtr, HashMapSpeed) {
		NodeManager manager;
		IRBuilder builder(manager);

		int N = 100; // 0000;
		bool showTimes = false;

		// create a million nodes
		vector<IntValuePtr> list;
		{
			utils::Timer timer("create");
			for(int i = 0; i < N; i++) {
				list.push_back(builder.intValue(i));
			}
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}

		{
			vector<IntValuePtr> list2;
			utils::Timer timer("vector");
			for_each(list, [&](const IntValuePtr& cur) { list2.push_back(cur); });
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}

		{
			boost::unordered_set<NodePtr, hash_target<NodePtr>, equal_target<NodePtr>> set;
			utils::Timer timer("boost::unordered_set");
			for_each(list, [&](const IntValuePtr& cur) { set.insert(cur); });
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}

		{
			std::unordered_set<NodePtr, hash_target<NodePtr>, equal_target<NodePtr>> set;
			utils::Timer timer("std::unordered_set");
			for_each(list, [&](const IntValuePtr& cur) { set.insert(cur); });
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}

		{
			std::set<IntValuePtr, compare_target<IntValuePtr>> set;
			utils::Timer timer("std::set");
			for_each(list, [&](const IntValuePtr& cur) { set.insert(cur); });
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}
	}

	TEST(Node, DumpTest) {
		// just create some node and dump it
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B", toVector(A));
		auto node = B;

		// to selected output stream
		{
			// dump(node);
			std::stringstream buf;
			dump(node, buf);
			EXPECT_EQ("B<A>\n", buf.str());
		}

		{
			// dumpText(node);
			std::stringstream buf;
			dumpText(node, buf);
			EXPECT_EQ("(GenericType |\n    (StringValue \"B\")\n    (Parents )\n    (Types |\n        (GenericType |\n            (StringValue \"A\")\n        "
			          "    (Parents )\n            (Types )\n        )\n    )\n)\n\n",
			          buf.str());
		}

		{
			std::stringstream buf;
			dumpDetail(node, buf);
			EXPECT_EQ("B<A>\n", buf.str());
		}

		// test literal printing
		ExpressionPtr E = builder.literal("x", B);
		{
			std::stringstream buf;
			dump(E, buf);
			EXPECT_EQ("x\n", buf.str());
		}

		{
			std::stringstream buf;
			dumpDetail(E, buf);
			EXPECT_EQ("x:B<A>\n", buf.str());
		}

		{
			std::stringstream buf;
			dumpDetailColored(E, buf);
			EXPECT_EQ("\x1B[0mx\x1B[0m\x1B[33m:\x1B[0m\x1B[0mB\x1B[0m\x1B[33m<\x1B[0m\x1B[0mA\x1B[0m\x1B[33m>\x1B[0m\n", buf.str());
		}

		ExpressionPtr F = builder.parseExpr("def fun = function () -> unit {}; fun()");

		{
			std::stringstream buf;
			dumpOneLine(F, buf);
			EXPECT_EQ("fun()", buf.str());
		}
	}

	TEST(Node, DumpPlainTest) {
		// just create some node and dump it
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B");
		TypePtr C = builder.genericType("C");

		std::stringstream buf;

		dump(A, buf);
		buf << "test\n";
		dump(B, buf);
		buf << "test\n";
		buf << dump(C);

		EXPECT_EQ("A\ntest\nB\ntest\nC\n", buf.str());
	}

	TEST(Node, DumpInStreamTest) {
		// just create some node and dump it
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B", toVector(A));
		auto node = B;

		// to selected output stream
		{
			// dump(node);
			std::stringstream buf;
			buf << dump(node);
			EXPECT_EQ("B<A>\n", buf.str());
		}

		{
			// dumpText(node);
			std::stringstream buf;
			buf << dumpText(node);
			EXPECT_EQ("(GenericType |\n    (StringValue \"B\")\n    (Parents )\n    (Types |\n        (GenericType |\n            (StringValue \"A\")\n        "
			          "    (Parents )\n            (Types )\n        )\n    )\n)\n\n",
			          buf.str());
		}

		{
			std::stringstream buf;
			buf << dumpDetail(node);
			EXPECT_EQ("B<A>\n", buf.str());
		}

		// test literal printing
		ExpressionPtr E = builder.literal("x", B);
		{
			std::stringstream buf;
			buf << dump(E);
			EXPECT_EQ("x\n", buf.str());
		}

		{
			std::stringstream buf;
			buf << dumpDetail(E);
			EXPECT_EQ("x:B<A>\n", buf.str());
		}

		// test re-using dump object
		auto d = dump(E);
		{
			std::stringstream buf;
			buf << d;
			EXPECT_EQ("x\n", buf.str());
		}
		{
			std::stringstream buf;
			buf << d;
			EXPECT_EQ("x\n", buf.str());
		}
	}

} // end namespace new_core
} // end namespace core
} // end namespace insieme
