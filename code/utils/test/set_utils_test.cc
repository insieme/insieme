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

#include <ctime>

#include "set_utils.h"

using std::unordered_set;

TEST(SetUtilsTest, Merge) {

	unordered_set<int> setA;
	setA.insert(1);
	setA.insert(2);

	unordered_set<int> setB;
	setB.insert(3);

	unordered_set<int> merged = *merge(setA,setB);

	unordered_set<int> setRef;
	setRef.insert(1);
	setRef.insert(2);
	setRef.insert(3);

	// NOTE: assumes that == is implemented (optional in std)
	EXPECT_EQ ( setRef, merged );
}

TEST(SetUtilsTest, Intersect) {

	unordered_set<int> setA;
	setA.insert(1);
	setA.insert(2);

	unordered_set<int> setB;
	setB.insert(1);

	unordered_set<int> res = *intersect(setA,setB);

	unordered_set<int> setRef;
	setRef.insert(1);

	// NOTE: assumes that == is implemented (optional in std)
	EXPECT_EQ ( setRef, res );
}

TEST(SetUtilsTest, Difference) {

	unordered_set<int> setA;
	setA.insert(1);
	setA.insert(2);

	unordered_set<int> setB;
	setB.insert(1);

	unordered_set<int> res = *difference(setA,setB);

	unordered_set<int> setRef;
	setRef.insert(2);

	// NOTE: assumes that == is implemented (optional in std)
	EXPECT_EQ ( setRef, res );
}


TEST(SetUtilsTest, ExecutionTime) {

	const int N = 1000*10;

	unordered_set<int> ref;
	for (int i=1; i<=N; i++) {
		ref.insert(i);
	}

	// test case: merging the numbers from 1-100, each value individually
	unordered_set<int> res;

	clock_t timeUP = -clock();
	res.clear();
	for (int i=1; i<=N; i++) {
		res = *merge(res, toSet(i));
	}
	timeUP += clock();
	EXPECT_EQ( ref, res);

//	std::shared_ptr<unordered_set<int>> resSP;
//	clock_t timeSP = -clock();
//	resSP->clear();
//	for (int i=1; i<=N; i++) {
//		resSP = merge_SP(*resSP, toSet(i));
//	}
//	timeSP += clock();
//	EXPECT_EQ( ref, *resSP);

	clock_t timeBV = -clock();
	res.clear();
	for (int i=1; i<=N; i++) {
		res = merge_V(res, toSet(i));
	}
	timeBV += clock();
	EXPECT_EQ( ref, res);

	clock_t timeBR = -clock();
	res.clear();
	for (int i=1; i<=N; i++) {
		unordered_set<int> tmp;
		merge_R(tmp, res, toSet(i));
		res = tmp;
	}
	timeBR += clock();
	EXPECT_EQ( ref, res);

	clock_t timeBR2 = -clock();
	res.clear();
	for (int i=1; i<=N; i++) {
		merge_R2(res, res, toSet(i));
	}
	timeBR2 += clock();
	EXPECT_EQ( ref, res);

	EXPECT_EQ ( -1, timeUP );
//	EXPECT_EQ ( -1, timeSP );
	EXPECT_EQ ( -1, timeBV );
	EXPECT_EQ ( -1, timeBR );
	EXPECT_EQ ( -1, timeBR2 );
}
