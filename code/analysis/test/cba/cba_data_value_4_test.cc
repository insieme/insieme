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

#include <array>

#include "insieme/analysis/cba/framework/data_value_4.h"
#include "insieme/analysis/cba/framework/data_index.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace std;
	using namespace core;


	TEST(DataValue, AtomicElements) {

		typedef Element<int> Element;
		EXPECT_EQ(sizeof(int*), sizeof(Element));

		DataManager<int> mgr;

		Element a = mgr.atomic(1);
		Element b = mgr.atomic(2);
		Element c = mgr.atomic(3);

		EXPECT_TRUE(a.isSingle());
		EXPECT_TRUE(b.isSingle());
		EXPECT_TRUE(c.isSingle());

		EXPECT_EQ("1", toString(a));
		EXPECT_EQ("2", toString(b));
		EXPECT_EQ("3", toString(c));

		EXPECT_EQ(a, mgr.atomic(1));
		EXPECT_EQ(b, mgr.atomic(2));
		EXPECT_EQ(c, mgr.atomic(3));

		EXPECT_EQ(a.getPtr(), mgr.atomic(1).getPtr());
		EXPECT_EQ(b.getPtr(), mgr.atomic(2).getPtr());
		EXPECT_EQ(c.getPtr(), mgr.atomic(3).getPtr());
	}

	TEST(DataValue, DataSet) {

		typedef Set<int> Set;
		EXPECT_EQ(sizeof(int*), sizeof(Set));

		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);

		EXPECT_EQ("{0}", toString(Set(e0)));
		EXPECT_EQ("{1}", toString(Set(e1)));

		EXPECT_EQ("{}", toString(mgr.set()));
		EXPECT_EQ("{0}", toString(mgr.set(e0)));
		EXPECT_EQ("{1}", toString(mgr.set(e1)));
		EXPECT_EQ("{0,1}", toString(mgr.set(e0,e1)));
		EXPECT_EQ("{0,1}", toString(mgr.set(e0,e1,e0)));

		// check pointer-equality
		EXPECT_EQ(mgr.set(e0,e1).getPtr(), mgr.set(e1,e0).getPtr());

		// empty check
		EXPECT_TRUE(Set().empty());
		EXPECT_TRUE(mgr.set().empty());

		EXPECT_FALSE(mgr.set(e0).empty());
		EXPECT_FALSE(mgr.set(e0,e1).empty());

		// single check
		EXPECT_FALSE(Set().isSingle());
		EXPECT_TRUE(Set(e0).isSingle());
		EXPECT_TRUE(Set(e1).isSingle());
		EXPECT_TRUE(mgr.set(e0).isSingle());
		EXPECT_TRUE(mgr.set(e1).isSingle());
		EXPECT_FALSE(mgr.set(e0,e1).isSingle());
	}


	TEST(DataValue, CompoundElements) {

		typedef Set<int> Set;
		typedef Element<int> Element;

		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);
		auto e2 = mgr.atomic(2);

		std::map<UnitIndex, Set> m1;
		m1[UnitIndex()] = e1;

		Element a = mgr.compound(m1);
		Element b = mgr.compound(m1);

		EXPECT_EQ(a,b);
		EXPECT_EQ(a.getPtr(), b.getPtr());

		EXPECT_EQ("[*={1}]", toString(a));
		EXPECT_EQ("[*={1}]", toString(b));

		std::map<SingleIndex, Set> m2;
		m2[1] = e1;
		m2[2] = mgr.set(e0,e2);
		m2[SingleIndex()] = e2;

		Element c = mgr.compound(m2);
		EXPECT_EQ("[1={1},2={0,2},*={2}]", toString(c));
		EXPECT_NE(a,c);
		EXPECT_NE(b,c);


		Element d = mgr.compound(
				entry(NominalIndex("a"), mgr.set(e1,e2)),
				entry(NominalIndex("b"), mgr.set(e0,e2))
		);

		EXPECT_EQ("[a={1,2},b={0,2}]", toString(d));

		// empty check
		EXPECT_FALSE(d.empty());

		EXPECT_FALSE(mgr.compound(
				entry(NominalIndex("a"), mgr.set(e1,e2)),
				entry(NominalIndex("b"), mgr.set(e0,e2))
		).empty());

		EXPECT_TRUE(mgr.compound(
				entry(NominalIndex("a"), mgr.set()),
				entry(NominalIndex("b"), mgr.set(e0,e2))
		).empty());

		EXPECT_TRUE(mgr.compound(
				entry(NominalIndex("a"), mgr.set(e1,e2)),
				entry(NominalIndex("b"), mgr.set())
		).empty());

		EXPECT_TRUE(mgr.compound(
				entry(NominalIndex("a"), mgr.set()),
				entry(NominalIndex("b"), mgr.set())
		).empty());

		m1.clear();
		EXPECT_TRUE(mgr.compound(m1).empty());


		// single check
		EXPECT_TRUE(a.isSingle());
		EXPECT_TRUE(b.isSingle());
		EXPECT_FALSE(c.isSingle());
		EXPECT_FALSE(d.isSingle());

	}

	TEST(DataValue, Membership) {

		typedef Set<int> Set;
		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);
		auto e2 = mgr.atomic(2);

		// simple set stuff
		Set s1;
		Set s2 = mgr.set(e0);
		Set s3 = mgr.set(e1);
		Set s4 = mgr.set(e1,e2);

		EXPECT_FALSE(isMember(s1, e0));
		EXPECT_TRUE (isMember(s2, e0));
		EXPECT_FALSE(isMember(s3, e0));
		EXPECT_FALSE(isMember(s4, e0));

		// more complex stuff
		Set c1 = mgr.compound(
				entry(NominalIndex("a"), mgr.set(e1,e2)),
				entry(NominalIndex("b"), mgr.set(e0,e2))
		);

		EXPECT_TRUE(isMember(c1, mgr.compound(
				entry(NominalIndex("a"), mgr.set(e1)),
				entry(NominalIndex("b"), mgr.set(e0))
		)));

		EXPECT_TRUE(isMember(c1, mgr.compound(
				entry(NominalIndex("a"), mgr.set(e2)),
				entry(NominalIndex("b"), mgr.set(e0))
		)));

		EXPECT_FALSE(isMember(c1, mgr.compound(
				entry(NominalIndex("a"), mgr.set(e2)),
				entry(NominalIndex("b"), mgr.set(e1))
		)));
	}

	TEST(DataValue, ArrayMemberShip) {

		typedef Set<int> Set;
		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);
		auto e2 = mgr.atomic(2);

		Set c1 = mgr.compound(
				entry(SingleIndex(1), mgr.set(e1)),
				entry(SingleIndex(2), mgr.set(e2)),
				entry(SingleIndex(), mgr.set(e0))
		);

		std::map<SingleIndex,Set> m;
		m[SingleIndex(0)] = mgr.atomic(0);
		m[SingleIndex(1)] = mgr.atomic(1);
		m[SingleIndex(2)] = mgr.atomic(2);
		m[SingleIndex(3)] = mgr.atomic(0);
		m[SingleIndex(4)] = mgr.atomic(0);
		auto x = mgr.compound(m);

		EXPECT_TRUE(isMember(c1, x)) << x << " in " << c1;

	}

	namespace {

		template<int base, unsigned dim>
		void inc(array<int,dim>& counter) {
			for(auto it = counter.rbegin(); it != counter.rend(); it++) {
				if (*it == base) {
					*it = 0;
					continue;
				}
				(*it)++;
				return;
			}
		}


		template<unsigned dim, typename F1, typename F2>
		void empiricArrayEqualityTest(DataManager<int>& mgr, const F1& a, const F2& b, int max_value=3) {
			array<int,dim> zero;
			for(int& c : zero) c = 0;

			array<int,dim> counter = zero;
			do {

				// create an element
				std::map<SingleIndex,Set<int>> m;
				for(unsigned i=0; i<dim; i++) {
					m[SingleIndex(i)] = mgr.atomic(counter[i]);
				}

				auto d = mgr.compound(m);
				EXPECT_EQ(a(d), b(d)) << "Element: " << d;

				inc<3,dim>(counter);
			} while(counter != zero);
		}

	}


	TEST(DataValue, Merge) {

		typedef Set<int> Set;
		typedef Element<int> Element;

		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);
		auto e2 = mgr.atomic(2);

		Set s1;
		Set s2 = mgr.set(e0);
		Set s3 = mgr.set(e1);
		Set s4 = mgr.set(e2);
		Set s5 = mgr.set(e0,e2);

		EXPECT_EQ("{}", toString(setUnion(s1,s1)));
		EXPECT_EQ("{0}", toString(setUnion(s1,s2)));
		EXPECT_EQ("{1}", toString(setUnion(s1,s3)));
		EXPECT_EQ("{0,1}", toString(setUnion(s2,s3)));
		EXPECT_EQ("{1,2}", toString(setUnion(s3,s4)));
		EXPECT_EQ("{0,2}", toString(setUnion(s2,s5)));
		EXPECT_EQ("{0,1,2}", toString(setUnion(s3,s5)));

		Set c1 = mgr.compound(
				entry(NominalIndex("a"), mgr.set(e1,e2)),
				entry(NominalIndex("b"), mgr.set(e0,e2))
		);

		Set c2 = mgr.compound(
				entry(NominalIndex("a"), mgr.set(e0,e1)),
				entry(NominalIndex("b"), mgr.set(e1,e2))
		);

		EXPECT_EQ("{[a={1,2},b={0,2}]}", toString(setUnion(s1,c1)));
		EXPECT_EQ("{[a={0,1},b={1,2}]}", toString(setUnion(s1,c2)));
		EXPECT_EQ("{[a={0,1},b={1,2}],[a={1,2},b={0,2}]}", toString(setUnion(c1,c2)));
		EXPECT_EQ("{[a={1,2},b={0,2}]}", toString(setUnion(c1,c1)));
		EXPECT_EQ("{[a={0,1},b={1,2}]}", toString(setUnion(c2,c2)));

		// TODO: ensure structure is identical within sets


		// some empirical tests with arrays
		Set c3 = mgr.compound(
				entry(SingleIndex(1), mgr.set(e1)),
				entry(SingleIndex(2), mgr.set(e2)),
				entry(SingleIndex(), mgr.set(e0))
		);

		Set c4 = mgr.compound(
				entry(SingleIndex(1), mgr.set(e2)),
				entry(SingleIndex(), mgr.set(e0))
		);

		auto cS = setUnion(c3, c4);

		empiricArrayEqualityTest<5>(mgr,
			[&](const Element& e)->bool { return isMember(c3, e) || isMember(c4, e); },
			[&](const Element& e)->bool { return isMember(cS, e); }
		);
	}

	TEST(DataValue, Intersect) {

		typedef Set<int> Set;

		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);
		auto e2 = mgr.atomic(2);

		Set s0 = mgr.set();
		Set s1 = mgr.set(e0);
		Set s2 = mgr.set(e1);
		Set s3 = mgr.set(e1, e2);

		// equal sets => no change
		EXPECT_EQ(s0, setIntersect(s0,s0));
		EXPECT_EQ(s1, setIntersect(s1,s1));
		EXPECT_EQ(s2, setIntersect(s2,s2));
		EXPECT_EQ(s3, setIntersect(s3,s3));

		// interacting with the empty set => empty set
		EXPECT_EQ(s0, setIntersect(s0,s1));
		EXPECT_EQ(s0, setIntersect(s1,s0));
		EXPECT_EQ(s0, setIntersect(s0,s2));
		EXPECT_EQ(s0, setIntersect(s2,s0));
		EXPECT_EQ(s0, setIntersect(s0,s3));
		EXPECT_EQ(s0, setIntersect(s3,s0));

		// intersecting non-intersecting sets
		EXPECT_EQ(s0, setIntersect(s1,s3));
		EXPECT_EQ(s0, setIntersect(s3,s1));

		// now something harder - intersecting elements
		EXPECT_EQ(s2, setIntersect(s2,s3));
		EXPECT_EQ(s2, setIntersect(s3,s2));

	}

	TEST(DataValue, IntersectElements) {

		typedef Set<int> Set;
		typedef Element<int> Element;

		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);
		auto e2 = mgr.atomic(2);

		Set c1 = mgr.compound(
				entry(SingleIndex(), mgr.set(e0,e1,e2))
		);

		Set c2 = mgr.compound(
				entry(SingleIndex(1), mgr.set(e1)),
				entry(SingleIndex(2), mgr.set(e2)),
				entry(SingleIndex(), mgr.set(e0))
		);

		Set c3 = mgr.compound(
				entry(SingleIndex(1), mgr.set(e2)),
				entry(SingleIndex(), mgr.set(e0))
		);

		// check the sets
		EXPECT_EQ("{[*={0,1,2}]}", toString(c1));
		EXPECT_EQ("{[1={1},2={2},*={0}]}", toString(c2));
		EXPECT_EQ("{[1={2},*={0}]}", toString(c3));

		Set s0;

		// check intersecting equal sets
		EXPECT_EQ(c1, setIntersect(c1, c1));
		EXPECT_EQ(c2, setIntersect(c2, c2));
		EXPECT_EQ(c3, setIntersect(c3, c3));

		// check intersecting distinct sets
		EXPECT_EQ(c2, setIntersect(c1, c2));
		EXPECT_EQ(c2, setIntersect(c2, c1));
		EXPECT_EQ(c3, setIntersect(c1, c3));
		EXPECT_EQ(c3, setIntersect(c3, c3));

		// check intersecting disjunct sets
		EXPECT_EQ(s0, setIntersect(c2, c3));
		EXPECT_EQ(s0, setIntersect(c3, c2));


		// check intersection of two sets forming a third set
		Set c4 = mgr.compound(
				entry(SingleIndex(1), mgr.set(e0,e1,e2)),
				entry(SingleIndex(), mgr.set(e0,e1))
		);
		Set c5 = mgr.compound(
				entry(SingleIndex(2), mgr.set(e0,e1,e2)),
				entry(SingleIndex(), mgr.set(e1,e2))
		);

		EXPECT_EQ("{[1={1,2},2={0,1},*={1}]}", toString(setIntersect(c4,c5)));
		EXPECT_EQ("{[1={1,2},2={0,1},*={1}]}", toString(setIntersect(c5,c4)));

		// run an empirical test
		Set cI = setIntersect(c4,c5);
		empiricArrayEqualityTest<5>(mgr,
			[&](const Element& e)->bool { return isMember(c4, e) && isMember(c5, e); },
			[&](const Element& e)->bool { return isMember(cI, e); }
		);
	}

//	TEST(DataValue, Diff) {
//
//		typedef Set<int> Set;
//
//		DataManager<int> mgr;
//
//		auto e0 = mgr.atomic(0);
//		auto e1 = mgr.atomic(1);
//		auto e2 = mgr.atomic(2);
//		auto e3 = mgr.atomic(3);
//		auto e4 = mgr.atomic(4);
//
//		Set s0 = mgr.set();
//		Set s1 = mgr.set(e0);
//		Set s2 = mgr.set(e1);
//		Set s3 = mgr.set(e1, e2);
//		Set s4 = mgr.set(e1, e3, e4);
//		Set s5 = mgr.set(e1, e2, e3, e4);
//
//		// some simple cases - involving an empty set
//		EXPECT_EQ(s0, setDiff(s0,s0));
//		EXPECT_EQ(s0, setDiff(s0,s1));
//		EXPECT_EQ(s0, setDiff(s0,s2));
//		EXPECT_EQ(s0, setDiff(s0,s3));
//
//		// also, the other way around
//		EXPECT_EQ(s0, setDiff(s0,s0));
//		EXPECT_EQ(s1, setDiff(s1,s0));
//		EXPECT_EQ(s2, setDiff(s2,s0));
//		EXPECT_EQ(s3, setDiff(s3,s0));
//
//		// subtract something that is not there
//		EXPECT_EQ(s3, setDiff(s3,s1));
//
//		EXPECT_EQ("{1,2}", toString(s3));
//		EXPECT_EQ("{1,3,4}", toString(s4));
//		EXPECT_EQ("{1,2,3,4}", toString(s5));
//
//		// and now some real stuff
//		EXPECT_EQ("{2}", toString(setDiff(s3,s2)));
//		EXPECT_EQ("{3,4}", toString(setDiff(s4,s3)));
//		EXPECT_EQ("{3,4}", toString(setDiff(s5,s3)));
//
//	}
//
//	TEST(DataValue, DiffComposed) {
//
//		typedef Set<int> Set;
//		typedef Element<int> Element;
//
//		DataManager<int> mgr;
//
//		auto e0 = mgr.atomic(0);
//		auto e1 = mgr.atomic(1);
//		auto e2 = mgr.atomic(2);
//
//		Set s0;
//		Set c1 = mgr.compound(
//				entry(SingleIndex(), mgr.set(e0,e1,e2))
//		);
//
//		Set c2 = mgr.compound(
//				entry(SingleIndex(1), mgr.set(e1)),
//				entry(SingleIndex(2), mgr.set(e2)),
//				entry(SingleIndex(), mgr.set(e0))
//		);
//
//		Set c3 = mgr.compound(
//				entry(SingleIndex(1), mgr.set(e2)),
//				entry(SingleIndex(), mgr.set(e0))
//		);
//
//		Set c4 = mgr.compound(
//				entry(SingleIndex(1), mgr.set(e2)),
//				entry(SingleIndex(), mgr.set(e0,e1))
//		);
//
//		// check complete annihilation
//		EXPECT_EQ(s0, setDiff(c1,c1));
//		EXPECT_EQ(s0, setDiff(c2,c2));
//		EXPECT_EQ(s0, setDiff(c3,c3));
//
//		// removing a super-set from a sub-set
//		EXPECT_EQ(s0, setDiff(c2,c1));
//		EXPECT_EQ(s0, setDiff(c3,c1));
//
//		// check complex cases
//		EXPECT_EQ("{[1={0,1,2},2={0,1},*={0,1,2}],[1={0,1,2},2={0,1,2},*={1,2}],[1={0,2},2={0,1,2},*={0,1,2}]}", toString(setDiff(c1,c2)));
//		EXPECT_EQ("{[1={0,1},*={0,1,2}],[1={0,1,2},*={1,2}]}", toString(setDiff(c1,c3)));
//		EXPECT_EQ("{[1={0,1},*={0,1,2}],[1={0,1,2},*={2}]}", toString(setDiff(c1,c4)));
//
//
//		// some empirical tests with arrays
//		auto cS = setDiff(c1, c2);
//		empiricArrayEqualityTest<5>(mgr,
//			[&](const Element& e)->bool { return isMember(c1, e) && !isMember(c2, e); },
//			[&](const Element& e)->bool { return isMember(cS, e); }
//		);
//
//		std::cout << "c1 = " << c1 << "\n";
//		std::cout << "c2 = " << c2 << "\n";
//		std::cout << "cS = " << cS << "\n";
//
////
////		cS = setDiff(c1, c3);
////		empiricArrayEqualityTest<5>(mgr,
////			[&](const Element& e)->bool { return isMember(c1, e) && !isMember(c3, e); },
////			[&](const Element& e)->bool { return isMember(cS, e); }
////		);
////
////		cS = setDiff(c1, c4);
////		empiricArrayEqualityTest<5>(mgr,
////			[&](const Element& e)->bool { return isMember(c1, e) && !isMember(c4, e); },
////			[&](const Element& e)->bool { return isMember(cS, e); }
////		);
//
//		// -- a special case - cut out the center pice of a triple --
//		NominalIndex na("a");
//		NominalIndex nb("b");
//
//		Set n1 = mgr.compound(
//				entry(na, mgr.set(e0,e1,e2)),
//				entry(nb, mgr.set(e0,e1,e2))
//		);
//
//		Set n2 = mgr.compound(
//				entry(na, mgr.set(e1)),
//				entry(nb, mgr.set(e1))
//		);
//
//		EXPECT_EQ("{[a={0,1,2},b={0,1,2}]}", toString(n1));
//		EXPECT_EQ("{[a={1},b={1}]}", toString(n2));
//
//		EXPECT_EQ(s0, setDiff(n2,n1));
//		EXPECT_EQ("{[a={0,1,2},b={0,2}],[a={0,2},b={0,1,2}]}", toString(setDiff(n1,n2)));
//	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
