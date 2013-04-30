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
#include <memory>

#include <boost/functional/hash.hpp>

#include "insieme/utils/set_utils.h"

//using boost::unordered_set;
using namespace insieme::utils::set;

typedef std::unordered_set<int, boost::hash<int>> Set;
//typedef std::unordered_set<int> Set;
//typedef boost::unordered_set<int> Set;

TEST(SetUtilsTest, toSet) {

	Set set = toSet<Set>(1,3,4,2,1);

	Set ref;
	ref.insert(1);
	ref.insert(3);
	ref.insert(4);
	ref.insert(2);
	ref.insert(1);

	EXPECT_EQ(static_cast<std::size_t>(4), set.size());
	EXPECT_EQ(static_cast<std::size_t>(4), ref.size());
	EXPECT_EQ(set, ref);
}

TEST(SetUtilsTest, Merge) {

	Set setA;
	setA.insert(1);
	setA.insert(2);

	Set setB;
	setB.insert(3);

	Set merged = merge(setA,setB);

	Set setRef;
	setRef.insert(1);
	setRef.insert(2);
	setRef.insert(3);

	// NOTE: assumes that == is implemented (optional in std)
	EXPECT_EQ ( setRef, merged );
}

TEST(SetUtilsTest, Intersect) {

	Set setA;
	setA.insert(1);
	setA.insert(2);

	Set setB;
	setB.insert(1);

	Set res = intersect(setA,setB);

	Set setRef;
	setRef.insert(1);

	// NOTE: assumes that == is implemented (optional in std)
	EXPECT_EQ ( setRef, res );
}

TEST(SetUtilsTest, Difference) {

	Set setA;
	setA.insert(1);
	setA.insert(2);

	Set setB;
	setB.insert(1);

	Set res = difference(setA,setB);

	Set setRef;
	setRef.insert(2);

	// NOTE: assumes that == is implemented (optional in std)
	EXPECT_EQ ( setRef, res );
}

