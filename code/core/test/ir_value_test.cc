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

#include "insieme/core/ir_values.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/container_utils.h"

#include "ir_node_test.inc"

namespace insieme {
namespace core {
namespace new_core {

	// TODO: add general IR node tests

	TEST(ValueNodes, Bool) {
		NodeManager manager;

		BoolValuePtr trueValue = BoolValue::get(manager, true);
		BoolValuePtr falseValue = BoolValue::get(manager, false);

		BoolValuePtr trueValue2 = BoolValue::get(manager, true);

		EXPECT_NE(trueValue, falseValue);
		EXPECT_NE(*trueValue, *falseValue);

		EXPECT_EQ(trueValue, trueValue2);
		EXPECT_EQ(*trueValue, *trueValue2);

		EXPECT_TRUE(typeid(trueValue->getValue()) == typeid(bool)) << typeid(trueValue->getValue()).name();

		NodePtr node = trueValue;
		EXPECT_EQ(node, trueValue);
	}

	TEST(ValueNodes, Char) {
		NodeManager manager;

		CharValuePtr a = CharValue::get(manager, 'a');
		CharValuePtr b = CharValue::get(manager, 'b');

		CharValuePtr c = CharValue::get(manager, 'a');

		EXPECT_NE(a, b);
		EXPECT_NE(*a, *b);

		EXPECT_EQ(a, c);
		EXPECT_EQ(*a, *c);

		EXPECT_TRUE(typeid(a->getValue()) == typeid(char));

		NodePtr node = a;
		EXPECT_EQ(node, a);
	}

	TEST(ValueNodes, Int) {
		NodeManager manager;

		IntValuePtr one = IntValue::get(manager, 1);
		IntValuePtr two = IntValue::get(manager, 2);

		IntValuePtr one2 = IntValue::get(manager, 1);

		EXPECT_NE(one, two);
		EXPECT_NE(*one, *two);

		EXPECT_EQ(one, one2);
		EXPECT_EQ(*one, *one2);

		EXPECT_TRUE(typeid(one->getValue()) == typeid(int));

		NodePtr node = one;
		EXPECT_EQ(node, one);
	}

	TEST(ValueNodes, UInt) {
		NodeManager manager;

		UIntValuePtr one = UIntValue::get(manager, 1);
		UIntValuePtr two = UIntValue::get(manager, 2);

		UIntValuePtr one2 = UIntValue::get(manager, 1);

		EXPECT_NE(one, two);
		EXPECT_NE(*one, *two);

		EXPECT_EQ(one, one2);
		EXPECT_EQ(*one, *one2);

		EXPECT_TRUE(typeid(one->getValue()) == typeid(unsigned));

		NodePtr node = one;
		EXPECT_EQ(node, one);
	}

	TEST(ValueNodes, String) {
		NodeManager manager;

		StringValuePtr a = StringValue::get(manager, "Hello");
		StringValuePtr b = StringValue::get(manager, "World");

		StringValuePtr c = StringValue::get(manager, "Hello");

		EXPECT_NE(a, b);
		EXPECT_NE(*a, *b);

		EXPECT_EQ(a, c);
		EXPECT_EQ(*a, *c);

		EXPECT_TRUE(typeid(a->getValue()) == typeid(string));

		NodePtr node = a;
		EXPECT_EQ(node, a);
	}

	TEST(Value, Addresses) {
		NodeManager manager;

		BoolValuePtr value = BoolValue::get(manager, true);
		BoolValueAddress adr(value);

		EXPECT_TRUE(value->getValue());
		EXPECT_TRUE(adr->getValue());
	}


	TEST(Value, DuplicateTest) {
		NodeManager manager;

		// check names
		StringValuePtr identA = StringValue::get(manager, "A");
		EXPECT_EQ("A", identA->getValue());

		StringValuePtr identB = StringValue::get(manager, "B");
		EXPECT_EQ("B", identB->getValue());

		StringValuePtr identA2 = StringValue::get(manager, "A");
		EXPECT_EQ("A", identA2->getValue());

		// check equality operator
		EXPECT_NE(identA, identB);
		EXPECT_EQ(identA, identA2);
		EXPECT_NE(identB, identA2);

		EXPECT_NE(identB, identA);
		EXPECT_EQ(identA2, identA);
		EXPECT_NE(identA2, identB);

		// check hash
		boost::hash<StringValue> hasher;
		StringValuePtr all[] = {identA, identB, identA2};
		for(int i = 0; i < 3; i++) {
			for(int j = 0; j < 3; j++) {
				StringValuePtr a = all[i];
				StringValuePtr b = all[j];
				EXPECT_EQ(*a == *b, hasher(*a) == hasher(*b));
			}
		}

		// Tests whether hash function for identifiers is properly working

		// create list of identifiers
		vector<StringValuePtr> identifier;
		identifier.push_back(StringValue::get(manager, "A"));
		identifier.push_back(StringValue::get(manager, "B"));
		EXPECT_FALSE(hasDuplicates(identifier));

		identifier.push_back(StringValue::get(manager, "A"));
		EXPECT_TRUE(hasDuplicates(identifier));

		basicNodeTests(identA);
		basicNodeTests(identB);
	}


} // end namespace new_core
} // end namespace core
} // end namespace insieme
