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
#include <string>

#include <gtest/gtest.h>

#include "insieme/core/ir_pointer.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_expressions.h"

#include "ir_dummy_annotations.inc"

using std::string;

namespace insieme {
namespace core {

	// ------------- utility classes required for the test case --------------

	struct A {
		void f(){};
	};
	struct B : public A {};

	// specialize node_type trait for this test (to support pointers to int)
	template <>
	struct node_type<int> {
		typedef empty ptr_accessor_type;
	};
	template <>
	struct node_type<A> {
		typedef empty ptr_accessor_type;
	};
	template <>
	struct node_type<B> {
		typedef empty ptr_accessor_type;
	};

	// testing basic properties
	TEST(Pointer, Basic) {
		// Size has been reduced from 40 bytes + a unordered map to 8 bytes (64-bit)
		EXPECT_EQ(sizeof(Pointer<int>), sizeof(int*));
		EXPECT_EQ(sizeof(Pointer<const Node>), sizeof(int*));
		EXPECT_EQ(sizeof(Pointer<const LambdaExpr>), sizeof(int*));

		int a = 10;
		int b = 15;

		// test simple creation
		Pointer<int> refA(&a);
		EXPECT_EQ(*refA, a);

		// ... and for another element
		Pointer<int> refB(&b);
		EXPECT_EQ(*refB, b);

		// test whether modifications are reflected
		a++;
		EXPECT_EQ(*refA, a);
	}

	TEST(Pointer, UpCast) {
		// create two related instances
		A a;
		B b;

		// create references
		Pointer<A> refA(&a);
		Pointer<B> refB(&b);

		// make assignment (if it compiles, test passed!)
		refA = refB;
	}

	TEST(Pointer, SimplePointerTest) {
		int value = 3;
		Pointer<int> ptr(&value);

		EXPECT_EQ(3, value);
		EXPECT_EQ(3, *ptr);

		value = 4;
		EXPECT_EQ(4, value);
		EXPECT_EQ(4, *ptr);

		*ptr = 5;
		EXPECT_EQ(5, value);
		EXPECT_EQ(5, *ptr);
	}

	TEST(Pointer, As) {
		NodeManager manager;
		IRBuilder builder(manager);

		NodePtr node = builder.genericType("A");

		// check target node type
		TypePtr type = node.as<TypePtr>();
		GenericTypePtr genType = node.as<GenericTypePtr>();

		EXPECT_EQ(type, node);
		EXPECT_EQ(type, genType);
	}

} // end namespace core
} // end namespace insieme
