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

#include <boost/functional/hash.hpp>

#include "insieme/utils/pointer.h"
#include "insieme/utils/instance_manager.h"

using std::string;
using std::cout;
using std::endl;

class CloneableString;

struct CloneableStringManager : public InstanceManager<CloneableString> {

	CloneableStringManager() {}
	CloneableStringManager(CloneableStringManager& manager) : InstanceManager<CloneableString>(manager) {}

};

class CloneableString : public string {
public:

	typedef InstanceManager<CloneableString> Manager;

	CloneableString(const char* c) : string(c) {};
	CloneableString(const string& str) : string(str) {};

	CloneableString* cloneTo(InstanceManager<CloneableString>& manager) const {
		return new CloneableString(*this);
	}
};
std::size_t hash_value(const CloneableString& str) {
	return boost::hash_value(str);
}

typedef Ptr<const CloneableString> MyPtr;

TEST(InstanceManager, Basic) {

	// create a new instance manager
	CloneableStringManager manager;
	EXPECT_EQ (0u, manager.size());

	// add and retrieve first element
	CloneableString strA  = "Hello World";
	MyPtr refA = manager.get(&strA);
	EXPECT_EQ (*refA, "Hello World");
	EXPECT_EQ (1u, manager.size());

	MyPtr refA2 = manager.get(&strA);
	EXPECT_EQ (*refA2, "Hello World");
	EXPECT_EQ (1u, manager.size());
	EXPECT_EQ (refA, refA2);

	// add and retrieve second element
	CloneableString strB = "Hello World 2";
	MyPtr refB = manager.get(&strB);
	EXPECT_EQ (*refB, "Hello World 2");
	EXPECT_EQ (2u, manager.size());

	// add and retrieve third element (which is equivalent to first element)
	CloneableString strC = "Hello World";
	MyPtr refC = manager.get(&strC);
	EXPECT_EQ (2u, manager.size());

	// ensure compiler is not reusing identical CloneableStrings
	EXPECT_NE (&strA, &strC);

	// check whether references are pointing to equivalent values
	EXPECT_EQ (*refA, *refC);

	// check whether references are pointing to same data location
	EXPECT_TRUE (refA == refC);


	// check whether -> operator is working ...
	EXPECT_STREQ (refA->c_str(), refC->c_str());
}


TEST(InstanceManager, GetTests) {

	// create a new instance manager
	CloneableStringManager manager;
	EXPECT_EQ (0u, manager.size());

	MyPtr a = manager.get(CloneableString("A"));
	MyPtr b = manager.get(CloneableString("B"));
	MyPtr c = manager.get(CloneableString("C"));
	MyPtr d = manager.get(b);

	EXPECT_EQ ( *a , "A" );
	EXPECT_EQ ( *b , "B" );
	EXPECT_EQ ( *c , "C" );
	EXPECT_EQ ( *d , "B" );

	EXPECT_EQ ( 3u, manager.size());

	vector<MyPtr> list;
	list.push_back(a);
	list.push_back(b);
	list.push_back(c);
	list.push_back(d);

	vector<MyPtr> listA = manager.getAll(list);
	EXPECT_EQ ( list , listA );

	CloneableStringManager manager2;
	EXPECT_EQ ( 0u, manager2.size() );
	vector<MyPtr> listB = manager2.getAll(list);
	EXPECT_EQ ( 3u, manager2.size() );
	EXPECT_NE ( list , listB );
}


TEST(InstanceManager, ContainsTests) {

	// create a new instance manager
	CloneableStringManager manager;
	EXPECT_EQ (0u, manager.size());

	// check null pointer
	MyPtr nPtr(NULL);
	EXPECT_TRUE( manager.contains(nPtr) );
	EXPECT_TRUE( manager.addressesLocal(nPtr));

	CloneableString strA("A");
	CloneableString strB("B");

	MyPtr strPtrA(&strA);
	MyPtr strPtrB(&strB);

	vector<MyPtr> list;
	list.push_back(strPtrA);
	list.push_back(strPtrB);

	EXPECT_FALSE( manager.contains(strA) );
	EXPECT_FALSE( manager.contains(strB) );
	EXPECT_FALSE( manager.contains(strPtrA) );
	EXPECT_FALSE( manager.contains(strPtrB) );
	EXPECT_FALSE( manager.containsAll(list) );
	EXPECT_FALSE( manager.addressesLocal(strPtrA) );
	EXPECT_FALSE( manager.addressesLocal(strPtrB) );
	EXPECT_FALSE( manager.addressesLocalAll(list) );

	MyPtr a = manager.get(&strA);

	EXPECT_TRUE( manager.contains(strA) );
	EXPECT_FALSE( manager.contains(strB) );
	EXPECT_TRUE( manager.contains(strPtrA) );
	EXPECT_FALSE( manager.contains(strPtrB) );
	EXPECT_FALSE( manager.containsAll(list) );
	EXPECT_FALSE( manager.addressesLocal(strPtrA) );
	EXPECT_FALSE( manager.addressesLocal(strPtrB) );
	EXPECT_FALSE( manager.addressesLocalAll(list) );

	MyPtr b = manager.get(&strB);

	EXPECT_TRUE( manager.contains(strA) );
	EXPECT_TRUE( manager.contains(strB) );
	EXPECT_TRUE( manager.contains(strPtrA) );
	EXPECT_TRUE( manager.contains(strPtrB) );
	EXPECT_TRUE( manager.containsAll(list) );
	EXPECT_FALSE( manager.addressesLocal(strPtrA) );
	EXPECT_FALSE( manager.addressesLocal(strPtrB) );
	EXPECT_FALSE( manager.addressesLocalAll(list) );

	vector<MyPtr> ptrList1;
	EXPECT_TRUE( manager.addressesLocalAll(ptrList1) );
	ptrList1.push_back(nPtr);
	EXPECT_TRUE( manager.addressesLocalAll(ptrList1) );
	ptrList1.push_back(a);
	EXPECT_TRUE( manager.addressesLocalAll(ptrList1) );
	ptrList1.push_back(b);
	EXPECT_TRUE( manager.addressesLocalAll(ptrList1) );
	ptrList1.push_back(strPtrA);
	EXPECT_FALSE( manager.addressesLocalAll(ptrList1) );
	ptrList1.pop_back();
	EXPECT_TRUE( manager.addressesLocalAll(ptrList1) );

	// create new manager
	CloneableStringManager manager2;
	vector<MyPtr> ptrList2;
	ptrList2.push_back(manager2.get(nPtr));
	ptrList2.push_back(manager2.get(a));
	ptrList2.push_back(manager2.get(b));

	EXPECT_TRUE ( manager.containsAll(ptrList1));
	EXPECT_TRUE ( manager.containsAll(ptrList2));
	EXPECT_TRUE ( manager2.containsAll(ptrList1));
	EXPECT_TRUE ( manager2.containsAll(ptrList2));

	EXPECT_TRUE ( manager.addressesLocalAll(ptrList1));
	EXPECT_FALSE ( manager.addressesLocalAll(ptrList2));
	EXPECT_FALSE ( manager2.addressesLocalAll(ptrList1));
	EXPECT_TRUE ( manager2.addressesLocalAll(ptrList2));
}


TEST(InstanceManager, LookupTests) {

	CloneableStringManager manager;

	CloneableString strA = "Hello";
	MyPtr strPtr(&strA);
	MyPtr nulPtr(NULL);


	EXPECT_EQ ( nulPtr, manager.lookup(strA) );
	EXPECT_EQ ( nulPtr, manager.lookup(strPtr) );

	std::pair<MyPtr, bool> addRes = manager.add(strA);
	EXPECT_TRUE (addRes.second);
	EXPECT_EQ ( strA, *addRes.first );

	EXPECT_NE ( strPtr, manager.lookup(strA) );
	EXPECT_NE ( strPtr, manager.lookup(strPtr) );

	EXPECT_EQ ( *strPtr, *manager.lookup(strA) );
	EXPECT_EQ ( *strPtr, *manager.lookup(strPtr) );

	CloneableString strB = "World";
	MyPtr strPtrB(&strB);
	EXPECT_EQ ( nulPtr, manager.lookup(strB) );

	vector<MyPtr> list;
	list.push_back(strPtr);
	list.push_back(strPtrB);
	list.push_back(nulPtr);

	EXPECT_EQ ( 1u , manager.size() );
	vector<MyPtr> res = manager.lookupAll(list);
	EXPECT_EQ ( 1u , manager.size() );
	EXPECT_EQ ( *strPtr, *res[0] );
	EXPECT_EQ ( nulPtr, res[1] );
	EXPECT_EQ ( nulPtr, res[2] );
}


TEST(InstanceManager, IteratorTest) {

	CloneableStringManager manager;

	CloneableString strA = "Hello";
	CloneableString strB = "World";
	MyPtr strPtrA(&strA);
	MyPtr strPtrB(&strB);
	MyPtr nulPtr(NULL);

	// manager should still be empty
	EXPECT_TRUE(manager.begin() == manager.end());

	{
		// add first element
		manager.add(strA);
		vector<MyPtr> list(manager.begin(), manager.end());
		EXPECT_EQ(toVector(manager.get(strA)), list);
	}

	{
		// add second element
		manager.add(strB);
		vector<MyPtr> list(manager.begin(), manager.end());
		EXPECT_EQ(2u, list.size());
		EXPECT_TRUE(contains(list, manager.get(strA)));
		EXPECT_TRUE(contains(list, manager.get(strB)));
	}

}

TEST(InstanceManager, Chaining) {

	// create a manager and a derived instance
	CloneableStringManager managerA;
	CloneableStringManager managerB(managerA);

	// create some nodes
	CloneableString strA = "Hello";
	CloneableString strB = "World";
	MyPtr strPtrA(&strA);
	MyPtr strPtrB(&strB);
	MyPtr nulPtr(NULL);


	// -- check contains --

	EXPECT_FALSE(managerA.contains(strA));
	EXPECT_FALSE(managerA.contains(strB));
	EXPECT_FALSE(managerB.contains(strA));
	EXPECT_FALSE(managerB.contains(strB));

	EXPECT_TRUE(managerA.add(strA).second);

	EXPECT_TRUE(managerA.contains(strA));
	EXPECT_FALSE(managerA.contains(strB));
	EXPECT_TRUE(managerB.contains(strA));
	EXPECT_FALSE(managerB.contains(strB));

	EXPECT_TRUE(managerA.addressesLocal(managerB.get(strA)));
	EXPECT_FALSE(managerB.addressesLocal(managerB.get(strA)));

	EXPECT_EQ(1u, managerA.size());
	EXPECT_EQ(0u, managerB.size());

	EXPECT_TRUE(managerB.add(strB).second);

	EXPECT_TRUE(managerA.contains(strA));
	EXPECT_FALSE(managerA.contains(strB));
	EXPECT_TRUE(managerB.contains(strA));
	EXPECT_TRUE(managerB.contains(strB));

	EXPECT_FALSE(managerA.addressesLocal(managerB.get(strB)));
	EXPECT_TRUE(managerB.addressesLocal(managerB.get(strB)));

	EXPECT_EQ(1u, managerA.size());
	EXPECT_EQ(1u, managerB.size());


	// -- check gets --

	EXPECT_NE(strPtrA, strPtrB);
	EXPECT_EQ(managerA.get(strA), managerB.get(strA));


	// check adding stuff included in base manager

	EXPECT_FALSE(managerB.add(strA).second);

	EXPECT_EQ(1u, managerA.size());
	EXPECT_EQ(1u, managerB.size());


}

