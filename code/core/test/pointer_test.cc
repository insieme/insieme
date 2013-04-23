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
	void f() {};
};
struct B : public A { };

// specialize node_type trait for this test (to support pointers to int)
template<> struct node_type<int> {
	typedef empty ptr_accessor_type;
};
template<> struct node_type<A> {
	typedef empty ptr_accessor_type;
};
template<> struct node_type<B> {
	typedef empty ptr_accessor_type;
};

// testing basic properties
TEST(Pointer, Basic) {

	// Size has been reduced from 40 bytes + a unordered map to 8 bytes (64-bit)
	EXPECT_EQ ( sizeof(Pointer<int>) , sizeof(int*) );
	EXPECT_EQ ( sizeof(Pointer<const Node>) , sizeof(int*) );
	EXPECT_EQ ( sizeof(Pointer<const LambdaExpr>) , sizeof(int*) );

	int a = 10;
	int b = 15;

	// test simple creation
	Pointer<int> refA(&a);
	EXPECT_EQ (*refA, a);

	// ... and for another element
	Pointer<int> refB(&b);
	EXPECT_EQ (*refB, b);

	// test whether modifications are reflected
	a++;
	EXPECT_EQ (*refA, a);

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

	EXPECT_EQ( 3, value);
	EXPECT_EQ( 3, *ptr);

	value = 4;
	EXPECT_EQ( 4, value);
	EXPECT_EQ( 4, *ptr);

	*ptr = 5;
	EXPECT_EQ( 5, value);
	EXPECT_EQ( 5, *ptr);
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

