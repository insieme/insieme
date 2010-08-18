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
#include <vector>

#include "container_utils.h"

using std::vector;
using std::string;

TEST(ContainerUtils, Singleton) {
	// Obtain two instances
	vector<int> testInt = toVector(14);
	vector<string> testString = toVector(string("Hello"));

	EXPECT_EQ( (std::size_t)1, testInt.size());
	EXPECT_EQ( (std::size_t)1, testString.size() );

	EXPECT_EQ ((*testInt.cbegin()), 14);
	EXPECT_EQ ((*testString.cbegin()), "Hello");
}

TEST(ContainerUtils, addAll) {

	vector<int> listA;
	vector<int> listB;

	for (int i=0; i<5; i++) {
		listA.push_back(i);
		listB.push_back(i*10);
	}

	EXPECT_EQ ( (std::size_t)5 , listA.size() );
	EXPECT_EQ ( (std::size_t)5 , listB.size() );

	addAll<int>(listA, listB);

	EXPECT_EQ ( (std::size_t)10, listA.size() );
	EXPECT_EQ ( (std::size_t)5, listB.size() );

	for (int i=0; i<5; i++) {
		EXPECT_EQ ( i, listA[i] );
		EXPECT_EQ ( i*10 , listB[i] );
		EXPECT_EQ ( i*10 , listA[i+5] );
	}
}

TEST(ContainerUtils, AnyAll) {

	// create list of integers
	vector<int> list;

	// simple property: even
	auto even = [](int x) { return x%2==0; };

	// check empty lists
	EXPECT_FALSE ( any(list, even) );
	EXPECT_TRUE ( all(list, even) );

	// check remaining cases
	list.push_back(1);
	EXPECT_FALSE ( any(list, even) );
	EXPECT_FALSE ( all(list, even) );

	list.push_back(2);
	EXPECT_TRUE ( any(list, even) );
	EXPECT_FALSE ( all(list, even) );

	list.clear();
	list.push_back(2);
	EXPECT_TRUE ( any(list, even) );
	EXPECT_TRUE( all(list, even) );

}

TEST(ContainerUtils, Duplicates) {

	// check some basic properties
	vector<int> list;
	EXPECT_FALSE ( hasDuplicates(list) );

	list.push_back(2);
	EXPECT_FALSE ( hasDuplicates(list) );

	list.push_back(4);
	EXPECT_FALSE ( hasDuplicates(list) );

	list.push_back(4);
	EXPECT_TRUE ( hasDuplicates(list) );

	list.pop_back();
	EXPECT_FALSE ( hasDuplicates(list) );

	list.push_back(2);
	EXPECT_TRUE ( hasDuplicates(list) );

	// check a large list
	int N = 10000;
	list.clear();
	for (int i=0; i<N; i++) {
		list.push_back(i);
	}
	EXPECT_FALSE ( hasDuplicates(list) );

	list.push_back(N/2);
	EXPECT_TRUE ( hasDuplicates(list) );
}
