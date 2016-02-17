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

#include <algorithm>
#include <list>
#include <vector>
#include <map>
#include <set>

#include <iostream>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/string_utils.h"

using std::pair;
using std::string;
using std::vector;
using std::list;

TEST(ContainerUtils, Singleton) {
	// Obtain two instances
	vector<int> testInt = toVector(14);
	vector<string> testString = toVector(string("Hello"));

	EXPECT_EQ((std::size_t)1, testInt.size());
	EXPECT_EQ((std::size_t)1, testString.size());

	EXPECT_EQ((*testInt.cbegin()), 14);
	EXPECT_EQ((*testString.cbegin()), "Hello");
}

TEST(ContainerUtils, addAll) {
	vector<int> listA;
	vector<int> listB;

	for(int i = 0; i < 5; i++) {
		listA.push_back(i);
		listB.push_back(i * 10);
	}

	EXPECT_EQ((std::size_t)5, listA.size());
	EXPECT_EQ((std::size_t)5, listB.size());

	addAll<int>(listA, listB);

	EXPECT_EQ((std::size_t)10, listA.size());
	EXPECT_EQ((std::size_t)5, listB.size());

	for(int i = 0; i < 5; i++) {
		EXPECT_EQ(i, listA[i]);
		EXPECT_EQ(i * 10, listB[i]);
		EXPECT_EQ(i * 10, listA[i + 5]);
	}
}

TEST(ContainerUtils, AnyAll) {
	// create list of integers
	vector<int> list;

	// simple property: even
	auto even = [](int x) { return x % 2 == 0; };

	// check empty lists
	EXPECT_FALSE(any(list, even));
	EXPECT_TRUE(all(list, even));

	// check remaining cases
	list.push_back(1);
	EXPECT_FALSE(any(list, even));
	EXPECT_FALSE(all(list, even));

	list.push_back(2);
	EXPECT_TRUE(any(list, even));
	EXPECT_FALSE(all(list, even));

	list.clear();
	list.push_back(2);
	EXPECT_TRUE(any(list, even));
	EXPECT_TRUE(all(list, even));
}

TEST(ContainerUtils, Duplicates) {
	// check some basic properties
	vector<int> list;
	EXPECT_FALSE(hasDuplicates(list));

	list.push_back(2);
	EXPECT_FALSE(hasDuplicates(list));

	list.push_back(4);
	EXPECT_FALSE(hasDuplicates(list));

	list.push_back(4);
	EXPECT_TRUE(hasDuplicates(list));

	list.pop_back();
	EXPECT_FALSE(hasDuplicates(list));

	list.push_back(2);
	EXPECT_TRUE(hasDuplicates(list));

	// check a large list
	int N = 10000;
	list.clear();
	for(int i = 0; i < N; i++) {
		list.push_back(i);
	}
	EXPECT_FALSE(hasDuplicates(list));

	list.push_back(N / 2);
	EXPECT_TRUE(hasDuplicates(list));

	std::vector<std::pair<int,int>> testPairs { {1, 1}, {2, 2}, {3, 3}, {3, 4} };
	EXPECT_FALSE(hasDuplicates(testPairs));
	EXPECT_TRUE(hasDuplicates(testPairs, [](const std::pair<int,int>& p) { return p.first; }));
	EXPECT_FALSE(hasDuplicates(testPairs, [](const std::pair<int,int>& p) { return p.second; }));

	std::map<int,string> testMap { {1, "bla"}, {2, "bla"} };
	EXPECT_FALSE(hasDuplicates(testMap));
	EXPECT_FALSE(hasDuplicates(testMap, [](const std::pair<int,string>& p) { return p.first; }));
	EXPECT_TRUE(hasDuplicates(testMap, [](const std::pair<int,string>& p) { return p.second; }));
}

TEST(ContainerUtils, Projection) {
	vector<int> listA;
	listA.push_back(1);
	listA.push_back(2);
	listA.push_back(3);

	vector<char> listB;
	listB.push_back('a');
	listB.push_back('b');
	listB.push_back('c');

	vector<pair<int, char>> list;
	list.push_back(std::make_pair(1, 'a'));
	list.push_back(std::make_pair(2, 'b'));
	list.push_back(std::make_pair(3, 'c'));

	EXPECT_EQ(listA, projectToFirst(list));
	EXPECT_EQ(listB, projectToSecond(list));

	vector<pair<int, char>> emptyList;

	EXPECT_EQ(vector<int>(), projectToFirst(emptyList));
	EXPECT_EQ(vector<char>(), projectToSecond(emptyList));
}

TEST(ContainerUtils, ExtractFirstSecond) {
	typedef std::pair<int, string> Entry;
	vector<Entry> list;
	list.push_back(std::make_pair(1, "one"));
	list.push_back(std::make_pair(2, "two"));

	vector<int> firstVec;
	std::transform(list.cbegin(), list.cend(), back_inserter(firstVec), extractFirst<Entry>());
	EXPECT_EQ(firstVec[0], 1);
	EXPECT_EQ(firstVec[1], 2);

	vector<string> secondVec;
	std::transform(list.cbegin(), list.cend(), back_inserter(secondVec), extractSecond<Entry>());
	EXPECT_EQ(secondVec[0], "one");
	EXPECT_EQ(secondVec[1], "two");
}

TEST(ContainerUtils, equal) {
	vector<int> vectorA;
	vectorA.push_back(1);
	vectorA.push_back(2);
	vectorA.push_back(3);

	vector<int> vectorB;
	vectorB.push_back(1);
	vectorB.push_back(2);

	EXPECT_FALSE(equals(vectorA, vectorB));
	vectorB.push_back(3);
	EXPECT_TRUE(equals(vectorA, vectorB));
	vectorB.push_back(4);
	EXPECT_FALSE(equals(vectorA, vectorB));
}

TEST(ContainerUtils, transform) {
	vector<int> vectorA;
	vectorA.push_back(1);
	vectorA.push_back(2);
	vectorA.push_back(3);

	vector<int> vectorB;
	vectorB.push_back(2);
	vectorB.push_back(3);
	vectorB.push_back(4);

	vector<char> vectorC;
	vectorC.push_back('A');
	vectorC.push_back('B');
	vectorC.push_back('C');

	EXPECT_FALSE(equals(vectorA, vectorB));
	EXPECT_FALSE(equals(vectorA, vectorC));

	// same member type
	auto result1 = transform(vectorA, [](const int val) { return val + 1; });
	EXPECT_TRUE(equals(vectorB, result1));
	// changed member type
	auto result2 = transform(vectorA, [](const int val) { return 'A' + (char)(val)-1; });
	EXPECT_TRUE(equals(vectorC, result2));


	// different container type
	list<int> listA(vectorA.begin(), vectorA.end());
	list<int> listB(vectorB.begin(), vectorB.end());
	list<char> listC(vectorC.begin(), vectorC.end());

	// same member type
	auto result3 = transform(listA, [](const int val) { return val + 1; });
	EXPECT_TRUE(equals(listB, result3));
	// changed member type
	auto result4 = transform(listA, [](const int val) { return 'A' + (char)(val)-1; });
	EXPECT_TRUE(equals(listC, result4));

	// changed container type (list -> vector)
	auto result5 = transform<vector>(listA, [](const int val) { return val; });
	static_assert(std::is_same<decltype(result5), vector<int>>::value, "Unexpected type from transform list -> vector");
	EXPECT_TRUE(equals(listA, result5));

	// changed container type (list -> map)
	auto result6 = transform<std::map>(listA, [](const int val) { return std::make_pair(val, val); });
	std::map<int, int> refMapA{{1, 1}, {2, 2}, {3, 3}};
	// does not compile w/ GCC 4.9.1
	// static_assert(std::is_same<decltype(result6)::key_type, decltype(refMapA)::key_type>::value, "Unexpected key type from transform list -> map");
	// static_assert(std::is_same<decltype(result6)::mapped_type, decltype(refMapA)::mapped_type>::value, "Unexpected value type from transform list -> map");
	EXPECT_TRUE(equals(refMapA, result6));

	// changed container type (map -> vector)
	auto result7 = transform<std::vector>(refMapA, [](const pair<int, int> val) { return val.first; });
	static_assert(std::is_same<decltype(result7), vector<int>>::value, "Unexpected type from transform map -> vector");
	EXPECT_TRUE(equals(listA, result7));

	// changed container type (vector -> set)
	auto result8 = transform<std::set>(result7, [](const int val) { return val; });
	static_assert(std::is_same<decltype(result8), std::set<int>>::value, "Unexpected type from transform vector -> set");
	EXPECT_TRUE(equals(listA, result8));

	// maps
	std::map<string, int> mapA{{"one", 1}, {"two", 2}, {"Jorg", 42}};
	std::map<int, string> mapB{{1, "one"}, {2, "two"}, {42, "Jorg"}};
	auto mapResult = transform(mapA, [](const pair<string, int>& p) { return std::make_pair(p.second, p.first); });
	EXPECT_TRUE(equals(mapB, mapResult));
}

TEST(ContainerUtils, IsContainer) {
	using AContainer = std::vector<int>;
	using AlsoAContainer = std::map<char, string>;
	using NotAContainer = struct bla { };

	EXPECT_TRUE(is_container<AContainer>::value);
	EXPECT_TRUE(is_container<AlsoAContainer>::value);
	EXPECT_FALSE(is_container<NotAContainer>::value);
}

TEST(Tuple, RemoveHead) {
	std::tuple<int, int, bool> t{2, 2, false};
	EXPECT_EQ(3u, std::tuple_size<decltype(t)>::value);

	auto tt = removeFirst(t);

	EXPECT_EQ(2u, std::tuple_size<decltype(tt)>::value);
	EXPECT_EQ(tt, std::make_tuple(2, false));
}

TEST(Printer, Array) {
	std::array<int, 3> a;
	a[0] = 1;
	a[1] = 2;
	a[2] = 3;

	EXPECT_EQ("[1,2,3]", toString(a));
}

TEST(Printer, Vector) {
	std::vector<int> v;
	v.push_back(1);
	v.push_back(2);
	v.push_back(3);

	EXPECT_EQ("[1,2,3]", toString(v));
}

TEST(Printer, Tuple) {
	std::tuple<> t1 = std::make_tuple();
	EXPECT_EQ("()", toString(t1));

	std::tuple<int> t2 = std::make_tuple(1);
	EXPECT_EQ("(1)", toString(t2));

	std::tuple<int, float> t3 = std::make_tuple(1, 2.1f);
	EXPECT_EQ("(1,2.1)", toString(t3));

	std::tuple<int, float, string> t4 = std::make_tuple(1, 2.1f, "hello");
	EXPECT_EQ("(1,2.1,hello)", toString(t4));
}
