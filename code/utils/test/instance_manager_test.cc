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
#include <iostream>

#include <gtest/gtest.h>
#include "instance_manager.h"
#include "instance_ptr.h"

using std::string;
using std::cout;
using std::endl;


class CloneableString : public string {
public:
	CloneableString(const char* c) : string(c) {};
	CloneableString(const string& str) : string(str) {};
//	CloneableString(const CloneableString& str) : string(str.c_str()) {};

	CloneableString* clone() const {
		return new CloneableString(*this);
	}
};
size_t hash_value(const CloneableString& str) {
	return hash_value((string)str);
}


TEST(InstanceManager, Basic) {

	// create a new instance manager
	InstanceManager<const CloneableString> manager;
	EXPECT_EQ (manager.size(), 0);

	// add and retrieve first element
	CloneableString strA  = "Hello World";
	InstancePtr<const CloneableString> refA = manager.get(&strA);
	EXPECT_EQ (*refA, "Hello World");
	EXPECT_EQ (manager.size(), 1);

	// add and retrieve second element
	CloneableString strB = "Hello World 2";
	InstancePtr<const CloneableString> refB = manager.get(&strB);
	EXPECT_EQ (*refB, "Hello World 2");
	EXPECT_EQ (manager.size(), 2);

	// add and retrieve third element (which is equivalent to first element)
	CloneableString strC = "Hello World";
	InstancePtr<const CloneableString> refC = manager.get(&strC);
	EXPECT_EQ (manager.size(), 2);

	// ensure compiler is not reusing identical CloneableStrings
	EXPECT_NE (&strA, &strC);

	// check whether references are pointing to equivalent values
	EXPECT_EQ (*refA, *refC);

	// check whether references are pointing to same data location
	EXPECT_TRUE (refA == refC);


	// check whether -> operator is working ...
	EXPECT_STREQ (refA->c_str(), refC->c_str());
}

TEST(InstancePtr, Size) {

	// just ensures
	EXPECT_LE ( sizeof (InstancePtr<int>), sizeof(int*) );
}

typedef float real;

class A {
	// required to be polymorphic (dynamic cast)
	virtual real hell() { return 66.6; };
};
class B : public A {};
class C : public A {};


TEST(InstancePtr, Casts) {

	A a;
	B b;
	C c;

	InstancePtr<const A> refA(&a);
	InstancePtr<const B> refB(&b);
	InstancePtr<const C> refC(&c);

	refA = refB;
	// refB = refA;
	refB = dynamic_pointer_cast<const B >(refA);
	EXPECT_FALSE( refB == InstancePtr<B>(NULL) );

	// should not compile ...
//	refC = dynamic_pointer_cast<const C >(refA);
//	EXPECT_TRUE( refC == InstancePtr<C>(NULL) );

	refA = refC;
	refB = dynamic_pointer_cast<const B >(refA);
	EXPECT_TRUE( refB == InstancePtr<B>(NULL) );
}
