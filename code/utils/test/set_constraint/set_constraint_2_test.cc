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

#include "insieme/utils/set_constraint/solver2.h"

namespace insieme {
namespace utils {
namespace set_constraint_2 {

	TEST(Assignment, IndexOf) {

		// just test the type-trait utility
		EXPECT_EQ(0, (detail::index_of<int, int, float>::value));
		EXPECT_EQ(1, (detail::index_of<float, int, float>::value));

	}

	TEST(Assignment, Check) {

		auto a = createAssignment<int, float>();

		TypedSetID<int> i1(1);
		TypedSetID<float> i2(2);

		EXPECT_EQ("({},{})", toString(a));

		a[i1].insert(1);
		a[i2].insert(2.3f);

		EXPECT_EQ("({s1={1}},{s2={2.3}})", toString(a));

		a[i1] = { 1, 2, 3 };
		a[i2] = { 1, 3 };
		EXPECT_EQ("({s1={1,2,3}},{s2={1,3}})", toString(a));

	}

	TEST(Constraint, Basic) {

		typedef TypedSetID<int> Set;
		typedef Constraints<int> Constraints;

		ConstraintFactory<int> cf;

		// simple set tests
		Set a = 1;
		Set b = 2;
		Set c = 3;

		EXPECT_EQ("s1", toString(a));

		EXPECT_EQ("s1 sub s2", toString(*cf.subset(a,b)));

		EXPECT_EQ("5 in s1 => s2 sub s3", toString(*cf.subsetIf(5,a,b,c)));

		EXPECT_EQ("|s1| > 5 => s2 sub s3", toString(*cf.subsetIfBigger(a,5,b,c)));

		// constraint test
		Constraints problem = {
				cf.elem(3, a),
				cf.subset(a,b),
				cf.subsetIf(5,a,b,c),
				cf.subsetIfBigger(a,5,b,c),
				cf.subsetIfReducedBigger(a, 3, 2, b, c)
		};

		EXPECT_EQ("{3 in s1,s1 sub s2,5 in s1 => s2 sub s3,|s1| > 5 => s2 sub s3,|s1 - {3}| > 2 => s2 sub s3}", toString(problem));

	}


	TEST(Constraint, Check) {

		auto s = [](int id) { return TypedSetID<int>(id); };

		auto s1 = TypedSetID<int>(1);
		auto s2 = TypedSetID<int>(2);

		Assignment<int> a;
		a[s1] = { 1, 2 };
		a[s2] = { 1, 2, 3 };

		ConstraintFactory<int> cf;

		auto c = cf.subset(s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;

		c = cf.subset(s2,s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;

		c = cf.elem( 3, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;

		c = cf.elem( 3, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;

		c = cf.subsetIf( 3, s2, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;

		c = cf.subsetIf( 3, s2, s2, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;

		c = cf.subsetIf( 3, s1, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;

		c = cf.subsetIf( 3, s1, s2, s1);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;

		c = cf.subsetIfBigger(s1, 1, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;

		c = cf.subsetIfBigger(s1, 5, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;

		c = cf.subsetIfBigger(s1, 1, s2, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;

	}


//	TEST(Solver, Basic) {
//
//		Constraints problem = {
//				elem(5,1),
//				elem(6,1),
//				subset(1,2),
//				subset(2,3),
//				subset(4,3),
//
//				elem(7,5),
//				subsetIf(6,3,5,3),
//				subsetIfBigger(2,1,3,6),
//				subsetIfBigger(2,3,3,7),
//				subsetIfBigger(2,4,3,8),
//
//				subsetIfReducedBigger(2,5,0,3,9),
//				subsetIfReducedBigger(2,5,1,3,10),
//				subsetIfReducedBigger(3,5,1,3,11),
//
//		};
//
//		auto res = solve(problem);
//		EXPECT_EQ("{s1={5,6}, s2={5,6}, s3={5,6,7}, s5={7}, s6={5,6,7}, s9={5,6,7}, s11={5,6,7}}", toString(res));
//
//		EXPECT_EQ("{5,6}", toString(res[1])) << res;
//		EXPECT_EQ("{5,6}", toString(res[2])) << res;
//		EXPECT_EQ("{5,6,7}", toString(res[3])) << res;
//		EXPECT_EQ("{}", toString(res[4])) << res;
//		EXPECT_EQ("{7}", toString(res[5])) << res;
//		EXPECT_EQ("{5,6,7}", toString(res[6])) << res;
//		EXPECT_EQ("{}", toString(res[7])) << res;
//		EXPECT_EQ("{}", toString(res[8])) << res;
//		EXPECT_EQ("{5,6,7}", toString(res[9])) << res;
//		EXPECT_EQ("{}", toString(res[10])) << res;
//		EXPECT_EQ("{5,6,7}", toString(res[11])) << res;
//
//		// check the individual constraints
//		for (const auto& cur : problem) {
//			EXPECT_TRUE(cur.check(res)) << cur;
//		}
//
//	}

} // end namespace set_constraint
} // end namespace utils
} // end namespace insieme
