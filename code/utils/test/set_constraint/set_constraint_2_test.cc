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

	TEST(Assignment, Check) {

		Assignment a;

		TypedSetID<int> i1(1);
		TypedSetID<float> i2(2);

		EXPECT_EQ("{}", toString(a));

		a[i1].insert(1);
		a[i2].insert(2.3f);

		EXPECT_EQ("{s2={2.3},s1={1}}", toString(a));

		a[i1] = { 1, 2, 3 };
		a[i2] = { 1, 3 };
		EXPECT_EQ("{s2={1,3},s1={1,2,3}}", toString(a));

	}

	TEST(Constraint, Basic) {

		typedef TypedSetID<int> Set;

		// simple set tests
		Set a = 1;
		Set b = 2;
		Set c = 3;

		EXPECT_EQ("s1", toString(a));

		EXPECT_EQ("s1 sub s2", toString(*subset(a,b)));

		EXPECT_EQ("5 in s1 => s2 sub s3", toString(*subsetIf(5,a,b,c)));

		EXPECT_EQ("|s1| > 5 => s2 sub s3", toString(*subsetIfBigger(a,5,b,c)));

		// constraint test
		Constraints problem = {
				elem(3, a),
				subset(a,b),
				subsetIf(5,a,b,c),
				subsetIfBigger(a,5,b,c),
				subsetIfReducedBigger(a, 3, 2, b, c)
		};

		EXPECT_EQ("{3 in s1,s1 sub s2,5 in s1 => s2 sub s3,|s1| > 5 => s2 sub s3,|s1 - {3}| > 2 => s2 sub s3}", toString(problem));

	}


	TEST(Solver, ConstraintInputSetsIfElem) {

		typedef TypedSetID<int> Set;

		Set a = 1;
		Set b = 2;
		Set c = 3;

		// create constraint to be tested
		auto constraint = subsetIf(0,a,b,c);

		Assignment ass;

		EXPECT_TRUE(constraint->hasAssignmentDependentDependencies());

		EXPECT_EQ("[s1,s2]", toString(constraint->getInputs()));
		EXPECT_EQ("[s3]", toString(constraint->getOutputs()));
		EXPECT_EQ("{s1}", toString(constraint->getUsedInputs(ass)));

		ass[a].insert(0);
		EXPECT_EQ("{s1,s2}", toString(constraint->getUsedInputs(ass)));

	}


	TEST(Constraint, Check) {

		auto s1 = TypedSetID<int>(1);
		auto s2 = TypedSetID<int>(2);

		Assignment a;
		a[s1] = { 1, 2 };
		a[s2] = { 1, 2, 3 };

		auto c = subset(s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subset(s2,s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = elem( 3, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(0u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = elem( 3, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(0u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIf( 3, s2, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(2u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIf( 3, s2, s2, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIf( 3, s1, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIf( 3, s1, s2, s1);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIfBigger(s1, 1, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIfBigger(s1, 5, s1, s2);
		EXPECT_TRUE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(1u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

		c = subsetIfBigger(s1, 1, s2, s1);
		EXPECT_FALSE(c->check(a)) << *c << " on " << a;
		EXPECT_EQ(2u, c->getUsedInputs(a).size()) << c->getUsedInputs(a);

	}


	TEST(Solver, Basic) {

		auto s = [](int id) { return TypedSetID<int>(id); };

		Constraints problem = {
				elem(5,s(1)),
				elem(6,s(1)),
				subset(s(1),s(2)),
				subset(s(2),s(3)),
				subset(s(4),s(3)),

				elem(7,s(5)),
				subsetIf(6,s(3),s(5),s(3)),
				subsetIfBigger(s(2),1,s(3),s(6)),
				subsetIfBigger(s(2),3,s(3),s(7)),
				subsetIfBigger(s(2),4,s(3),s(8)),

				subsetIfReducedBigger(s(2),5,0,s(3),s(9)),
				subsetIfReducedBigger(s(2),5,1,s(3),s(10)),
				subsetIfReducedBigger(s(3),5,1,s(3),s(11)),

		};

		auto res = solve(problem);
		EXPECT_EQ("{s1={5,6},s2={5,6},s3={5,6,7},s5={7},s6={5,6,7},s9={5,6,7},s11={5,6,7}}", toString(res));

		EXPECT_EQ("{5,6}", toString(res[s(1)])) << res;
		EXPECT_EQ("{5,6}", toString(res[s(2)])) << res;
		EXPECT_EQ("{5,6,7}", toString(res[s(3)])) << res;
		EXPECT_EQ("{}", toString(res[s(4)])) << res;
		EXPECT_EQ("{7}", toString(res[s(5)])) << res;
		EXPECT_EQ("{5,6,7}", toString(res[s(6)])) << res;
		EXPECT_EQ("{}", toString(res[s(7)])) << res;
		EXPECT_EQ("{}", toString(res[s(8)])) << res;
		EXPECT_EQ("{5,6,7}", toString(res[s(9)])) << res;
		EXPECT_EQ("{}", toString(res[s(10)])) << res;
		EXPECT_EQ("{5,6,7}", toString(res[s(11)])) << res;

		// check the individual constraints
		for (const auto& cur : problem) {
			EXPECT_TRUE(cur->check(res)) << cur;
		}

	}

	TEST(Solver, Functions) {

		auto s = [](int id) { return TypedSetID<int>(id); };

		auto inc = [](const std::set<int>& a)->std::set<int> {
			std::set<int> res;
			for(auto x : a) { res.insert(x+1); }
			return res;
		};

		auto add = [](const std::set<int>& a, const std::set<int>& b)->std::set<int> {
			std::set<int> res;
			for(auto x : a) {
				for (auto y : b) {
					res.insert(x+y);
				}
			}
			return res;
		};

		Constraints problem = {

				elem(5, s(1)),
				elem(6, s(2)),
				elem(7, s(2)),

				subsetUnary(s(1), s(3), inc),
				subsetUnary(s(2), s(4), inc),

				subsetBinary(s(1), s(2), s(5), add),
		};

		auto res = solve(problem);
		EXPECT_EQ("{s1={5},s2={6,7},s3={6},s4={7,8},s5={11,12}}", toString(res));

		EXPECT_EQ("{5}", toString(res[s(1)])) << res;
		EXPECT_EQ("{6,7}", toString(res[s(2)])) << res;
		EXPECT_EQ("{6}", toString(res[s(3)])) << res;
		EXPECT_EQ("{7,8}", toString(res[s(4)])) << res;
		EXPECT_EQ("{11,12}", toString(res[s(5)])) << res;

	}

	TEST(Solver, Lazy) {

		// lazy-evaluated faculty values
		auto resolver = [](const std::set<SetID>& sets)->Constraints {
			Constraints res;
			for(auto cur : sets) {
				int id = cur.getID();
				if (id == 0) {
					res.add(elem(0, TypedSetID<int>(id)));
				} else if (id == 1 || id == 2) {
					res.add(elem(1, TypedSetID<int>(id)));
				} else {
					TypedSetID<int> a(id-1);
					TypedSetID<int> b(id-2);
					TypedSetID<int> r(id);
					res.add(subsetBinary(a, b, r, [](const std::set<int>& a, const std::set<int>& b)->std::set<int> {
						std::set<int> res;
						for( int x : a) for (int y : b) res.insert(x+y);
						return res;
					}));
				}
			}
			return res;
		};

		// see whether we can compute something
		auto res = solve(TypedSetID<int>(4), resolver);
//		std::cout << res << "\n";
		EXPECT_EQ("{3}", toString(res[TypedSetID<int>(4)]));

		// see whether we can compute something
		res = solve(TypedSetID<int>(46), resolver);
//		std::cout << res << "\n";
		EXPECT_EQ("{1836311903}", toString(res[TypedSetID<int>(46)]));
	}


} // end namespace set_constraint
} // end namespace utils
} // end namespace insieme
