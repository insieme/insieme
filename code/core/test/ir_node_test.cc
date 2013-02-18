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

#include <gtest/gtest.h>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_values.h"
#include "insieme/core/ir_int_type_param.h"
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

		ConcreteIntTypeParamPtr p = ConcreteIntTypeParam::get(manager, 12);

		EXPECT_TRUE(typeid(p->getParam()) == typeid(UIntValuePtr));
		EXPECT_TRUE(p->getParam());
		EXPECT_EQ(UIntValue::get(manager, 12), p->getParam());

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

		ConcreteIntTypeParamAddress a(ConcreteIntTypeParam::get(manager, 12));

		EXPECT_TRUE(typeid(a->getParam()) == typeid(UIntValueAddress));
		EXPECT_TRUE(a->getParam());
		EXPECT_EQ("0", toString(a));
		EXPECT_EQ("0-0", toString(a->getParam()));
		EXPECT_EQ(UIntValue::get(manager, 12), a->getParam().getAddressedNode());
	}

	TEST(FixedSizeNode, ChildListTest) {
		NodeManager manager;

		EXPECT_TRUE(ConcreteIntTypeParam::get(manager, toVector<NodePtr>(UIntValue::get(manager, 12))));

		// death tests do not pass valgrind test
//		EXPECT_DEATH(ConcreteIntTypeParam::get(manager, toVector<NodePtr>()), ".*");
//		EXPECT_DEATH(ConcreteIntTypeParam::get(manager, toVector<NodePtr>(BoolValue::get(manager, false))), ".*");
	}


	TEST(NodePtr, HashMapSpeed) {

		NodeManager manager;
		IRBuilder builder(manager);

		int N = 100; //0000;
		bool showTimes = false;

		// create a million nodes
		vector<IntValuePtr> list;
		{
			utils::Timer timer("create");
			for (int i=0; i<N; i++) {
				list.push_back(builder.intValue(i));
			}
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}

		{
			vector<IntValuePtr> list2;
			utils::Timer timer("vector");
			for_each(list, [&](const IntValuePtr& cur){
				list2.push_back(cur);
			});
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}

		{
			boost::unordered_set<NodePtr, hash_target<NodePtr>, equal_target<NodePtr>> set;
			utils::Timer timer("boost::unordered_set");
			for_each(list, [&](const IntValuePtr& cur){
				set.insert(cur);
			});
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}

		{
			std::unordered_set<NodePtr, hash_target<NodePtr>, equal_target<NodePtr>> set;
			utils::Timer timer("std::unordered_set");
			for_each(list, [&](const IntValuePtr& cur){
				set.insert(cur);
			});
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}

		{
			std::set<IntValuePtr, compare_target<IntValuePtr>> set;
			utils::Timer timer("std::set");
			for_each(list, [&](const IntValuePtr& cur){
				set.insert(cur);
			});
			timer.stop();
			EXPECT_FALSE(showTimes) << timer;
		}

	}

	TEST(Node, DumpTest) {

		// just create some node and dump it
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto node = builder.genericType("A");

		// to std::cout
		dump(node);

		// to selected output stream
		std::stringstream buf;
		dump(node, buf);
		EXPECT_EQ("A\n", buf.str());

	}

} // end namespace new_core
} // end namespace core
} // end namespace insieme


