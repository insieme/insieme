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

#include "insieme/analysis/cba/framework/data_index.h"
#include "insieme/analysis/cba/framework/data_value.h"

#include "insieme/core/ir_builder.h"
#include "insieme/utils/unused.h"

namespace insieme {
namespace analysis {
namespace cba {

#define DEBUG if(false) std::cout


	using namespace std;
	using namespace core;

	template<typename ... Args>
	std::set<int> toSet(Args ... args) {
		return utils::set::toSet<std::set<int>>(args...);
	}

	template<template<typename L> class S>
	void testSetLattice() {

		typedef utils::constraint::SetLattice<int> SetLattice;
		typedef S<SetLattice> Lattice;
		typedef typename Lattice::manager_type mgr_type;
		typedef typename Lattice::value_type value_type;

		typename Lattice::meet_assign_op_type meet_assign_op;
		typename Lattice::meet_op_type meet_op;
		typename Lattice::less_op_type less_op;
		typename Lattice::projection_op_type projection_op;
		typename Lattice::ground_op_type ground_op;

		auto not_less_op = [&](const value_type& a, const value_type& b)->bool {
			return !less_op(a,b);
		};

		// create an manager instance
		mgr_type mgr;

		// create atomic values
		auto s1 = toSet(1,3);
		auto s2 = toSet(2,5);

		value_type a = mgr.atomic(s1);
		value_type b = mgr.atomic(s2);

		// check ground operation
		decltype(s1) s3 = ground_op(a);
		EXPECT_PRED2(less_op, a, mgr.atomic(s3));

		// check whether values can be assigned
		value_type c = a;

		// check meet operator
		bool change = meet_assign_op(c,b);
		EXPECT_TRUE(change);

		// check equivalence between meet and meet assign operation
		EXPECT_EQ(c, meet_op(a,b));
		EXPECT_EQ(c, meet_op(a,s2));		// should also work with base type directly
		meet_op(a,2);						// and even with the base type of the base type

		// check that meet is a sub-type
		EXPECT_PRED2(less_op, a, c);
		EXPECT_PRED2(less_op, b, c);

		EXPECT_PRED2(less_op, s1, a);
		EXPECT_PRED2(less_op, s2, b);

		EXPECT_PRED2(less_op, s1, c);
		EXPECT_PRED2(less_op, s2, c);

		EXPECT_PRED2(less_op, 1, c);		// should also work for element of element type


		// create an test empty instance
		value_type empty;

		EXPECT_PRED2(less_op, empty, empty);

		EXPECT_PRED2(less_op, empty, a);
		EXPECT_PRED2(less_op, empty, b);
		EXPECT_PRED2(less_op, empty, c);

		EXPECT_PRED2(not_less_op, a, empty);
		EXPECT_PRED2(not_less_op, b, empty);
		EXPECT_PRED2(not_less_op, c, empty);

		// check index structure - first: nominal index
		{
			NominalIndex nA("a");
			NominalIndex nB("b");

			auto a1 = mgr.atomic(toSet(2));
			auto a2 = mgr.atomic(toSet(6,3));
			auto a3 = mgr.atomic(toSet(4,5));
			auto a4 = mgr.atomic(toSet(5,2));

			auto d = mgr.compound(
					entry(nA, a1),
					entry(nB, a2)
			);

			auto e = mgr.compound(
					entry(nA, a3),
					entry(nB, a4)
			);

			auto f = meet_op(d,e);

			DEBUG << "\nNominal:\n";
			DEBUG << d << "\n";
			DEBUG << e << "\n";
			DEBUG << f << "\n";

			EXPECT_PRED2(less_op, d, d);
			EXPECT_PRED2(less_op, e, e);

			EXPECT_PRED2(less_op, d, f);
			EXPECT_PRED2(less_op, e, f);

			// test projection
			EXPECT_PRED2(less_op, a1, projection_op(d, nA));
			EXPECT_PRED2(less_op, a2, projection_op(d, nB));
			EXPECT_PRED2(less_op, a3, projection_op(e, nA));
			EXPECT_PRED2(less_op, a4, projection_op(e, nB));

			EXPECT_PRED2(less_op, meet_op(a1, a3), projection_op(f, nA));
			EXPECT_PRED2(less_op, meet_op(a2, a4), projection_op(f, nB));

			// empty-comparison
			EXPECT_PRED2(less_op, empty, d);
			EXPECT_PRED2(less_op, empty, e);
			EXPECT_PRED2(less_op, empty, f);

			EXPECT_PRED2(not_less_op, d, empty);
			EXPECT_PRED2(not_less_op, e, empty);
			EXPECT_PRED2(not_less_op, f, empty);

		}

		// check UnitIndex
		{
			UnitIndex uI;

			auto a1 = mgr.atomic(toSet(2));
			auto a2 = mgr.atomic(toSet(1,4,5));

			auto d = mgr.compound(
					entry(uI, a1)
			);

			auto e = mgr.compound(
					entry(uI, a2)
			);

			auto f = meet_op(d,e);

			DEBUG << "\nUnitIndex:\n";
			DEBUG << d << "\n";
			DEBUG << e << "\n";
			DEBUG << f << "\n";

			EXPECT_PRED2(less_op, d, d);
			EXPECT_PRED2(less_op, e, e);

			EXPECT_PRED2(less_op, d, f);
			EXPECT_PRED2(less_op, e, f);

			// test projection
			EXPECT_PRED2(less_op, a1, projection_op(d, uI));
			EXPECT_PRED2(less_op, a2, projection_op(e, uI));

			EXPECT_PRED2(less_op, a1, projection_op(f, uI));
			EXPECT_PRED2(less_op, a2, projection_op(f, uI));

			// empty-comparison
			EXPECT_PRED2(less_op, empty, d);
			EXPECT_PRED2(less_op, empty, e);
			EXPECT_PRED2(less_op, empty, f);

			EXPECT_PRED2(not_less_op, d, empty);
			EXPECT_PRED2(not_less_op, e, empty);
			EXPECT_PRED2(not_less_op, f, empty);

		}

		// check SingleIndex
		{
			SingleIndex s1(1);
			SingleIndex s2(2);
			SingleIndex sR;

			auto a1 = mgr.atomic(toSet(2));
			auto a2 = mgr.atomic(toSet(6,3));
			auto a3 = mgr.atomic(toSet(4,5));
			auto a4 = mgr.atomic(toSet(5,2));

			auto d = mgr.compound(
					entry(s1, a1),
					entry(sR, a2)
			);

			auto e = mgr.compound(
					entry(s2, a3),
					entry(sR, a4)
			);

			auto f = meet_op(d,e);

			DEBUG << "\nSingleIndex:\n";
			DEBUG << d << "\n";
			DEBUG << e << "\n";
			DEBUG << f << "\n";
			DEBUG << "1=" << projection_op(f, s1) << "\n";
			DEBUG << "2=" << projection_op(f, s2) << "\n";
			DEBUG << "*=" << projection_op(f, sR) << "\n";

			EXPECT_PRED2(less_op, d, d);
			EXPECT_PRED2(less_op, e, e);

			EXPECT_PRED2(less_op, d, f);
			EXPECT_PRED2(less_op, e, f);

			// test projection
			EXPECT_PRED2(less_op, a1, projection_op(d, s1));
			EXPECT_PRED2(less_op, a2, projection_op(d, s2));
			EXPECT_PRED2(less_op, a2, projection_op(d, sR));

			EXPECT_PRED2(less_op, a4, projection_op(e, s1));
			EXPECT_PRED2(less_op, a3, projection_op(e, s2));
			EXPECT_PRED2(less_op, a4, projection_op(e, sR));

			EXPECT_PRED2(less_op, meet_op(a1, a4), projection_op(f, s1));
			EXPECT_PRED2(less_op, meet_op(a2, a3), projection_op(f, s2));
			EXPECT_PRED2(less_op, meet_op(a2, a4), projection_op(f, sR));

			// empty-comparison
			EXPECT_PRED2(less_op, empty, d);
			EXPECT_PRED2(less_op, empty, e);
			EXPECT_PRED2(less_op, empty, f);

			EXPECT_PRED2(not_less_op, d, empty);
			EXPECT_PRED2(not_less_op, e, empty);
			EXPECT_PRED2(not_less_op, f, empty);

		}
	}


	namespace {

		struct Pair : public std::pair<int,int> {
			Pair(int a = 10, int b = 10) : std::pair<int,int>(a,b) {}
		};

		struct pair_meet_assign_op {
			bool operator()(Pair& a, const Pair& b) const {
				bool res = false;
				if (a.first > b.first) {
					a.first = b.first;
					res = true;
				}
				if (a.second > b.second) {
					a.second = b.second;
					res = true;
				}
				return res;
			}
		};

		struct pair_less_op {
			bool operator()(const Pair& a, const Pair& b) const {
				return a.first >= b.first && a.second >= b.second;
			}
		};
	}

	// define a test lattice
	typedef utils::constraint::Lattice<Pair, pair_meet_assign_op, pair_less_op> PairLattice;


	template<template<typename L> class S>
	void testPairLattice() {

		typedef S<PairLattice> Lattice;
		typedef typename Lattice::manager_type mgr_type;
		typedef typename Lattice::value_type value_type;

		typename Lattice::meet_assign_op_type meet_assign_op;
		typename Lattice::meet_op_type meet_op;
		typename Lattice::less_op_type less_op;
		typename Lattice::projection_op_type projection_op;
		typename Lattice::ground_op_type ground_op;

		auto not_less_op = [&](const value_type& a, const value_type& b)->bool {
			return !less_op(a,b);
		};

		// create an manager instance
		mgr_type mgr;

		Pair p1(4,6);
		Pair p2(6,4);

		// create atomic values
		value_type a = mgr.atomic(p1);
		value_type b = mgr.atomic(p2);

		// check ground operation
		Pair p3 = ground_op(a);
		EXPECT_PRED2(less_op, a, mgr.atomic(p3));

		// check whether values can be assigned
		value_type c = a;

		// check meet operator
		bool change = meet_assign_op(c,b);
		EXPECT_TRUE(change);

		// check equivalence between meet and meet assign operation
		EXPECT_EQ(c, meet_op(a,b));
		EXPECT_EQ(c, meet_op(a,p2));		// should also work with base type directly

		// check that meet is a sub-type
		EXPECT_PRED2(less_op, a, c);
		EXPECT_PRED2(less_op, b, c);

		EXPECT_PRED2(less_op, p1, a);
		EXPECT_PRED2(less_op, p2, b);

		EXPECT_PRED2(less_op, p1, c);
		EXPECT_PRED2(less_op, p2, c);

		// create an test empty instance
		value_type empty;

		EXPECT_PRED2(less_op, empty, empty);

		EXPECT_PRED2(less_op, empty, a);
		EXPECT_PRED2(less_op, empty, b);
		EXPECT_PRED2(less_op, empty, c);

		EXPECT_PRED2(not_less_op, a, empty);
		EXPECT_PRED2(not_less_op, b, empty);
		EXPECT_PRED2(not_less_op, c, empty);

		// check index structure - first: nominal index
		{
			NominalIndex nA("a");
			NominalIndex nB("b");

			auto a1 = mgr.atomic(Pair(2,6));
			auto a2 = mgr.atomic(Pair(6,3));
			auto a3 = mgr.atomic(Pair(4,5));
			auto a4 = mgr.atomic(Pair(5,2));

			auto d = mgr.compound(
					entry(nA, a1),
					entry(nB, a2)
			);

			auto e = mgr.compound(
					entry(nA, a3),
					entry(nB, a4)
			);

			auto f = meet_op(d,e);

			DEBUG << "\nNominal:\n";
			DEBUG << d << "\n";
			DEBUG << e << "\n";
			DEBUG << f << "\n";

			EXPECT_PRED2(less_op, d, d);
			EXPECT_PRED2(less_op, e, e);

			EXPECT_PRED2(less_op, d, f);
			EXPECT_PRED2(less_op, e, f);

			// test projection
			EXPECT_PRED2(less_op, a1, projection_op(d, nA));
			EXPECT_PRED2(less_op, a2, projection_op(d, nB));
			EXPECT_PRED2(less_op, a3, projection_op(e, nA));
			EXPECT_PRED2(less_op, a4, projection_op(e, nB));

			EXPECT_PRED2(less_op, meet_op(a1, a3), projection_op(f, nA));
			EXPECT_PRED2(less_op, meet_op(a2, a4), projection_op(f, nB));

			// empty-comparison
			EXPECT_PRED2(less_op, empty, d);
			EXPECT_PRED2(less_op, empty, e);
			EXPECT_PRED2(less_op, empty, f);

			EXPECT_PRED2(not_less_op, d, empty);
			EXPECT_PRED2(not_less_op, e, empty);
			EXPECT_PRED2(not_less_op, f, empty);

		}

		// check UnitIndex
		{
			UnitIndex uI;

			auto a1 = mgr.atomic(Pair(2,6));
			auto a2 = mgr.atomic(Pair(4,5));

			auto d = mgr.compound(
					entry(uI, a1)
			);

			auto e = mgr.compound(
					entry(uI, a2)
			);

			auto f = meet_op(d,e);

			DEBUG << "\nUnitIndex:\n";
			DEBUG << d << "\n";
			DEBUG << e << "\n";
			DEBUG << f << "\n";

			EXPECT_PRED2(less_op, d, d);
			EXPECT_PRED2(less_op, e, e);

			EXPECT_PRED2(less_op, d, f);
			EXPECT_PRED2(less_op, e, f);

			// test projection
			EXPECT_PRED2(less_op, a1, projection_op(d, uI));
			EXPECT_PRED2(less_op, a2, projection_op(e, uI));

			EXPECT_PRED2(less_op, a1, projection_op(f, uI));
			EXPECT_PRED2(less_op, a2, projection_op(f, uI));

			// empty-comparison
			EXPECT_PRED2(less_op, empty, d);
			EXPECT_PRED2(less_op, empty, e);
			EXPECT_PRED2(less_op, empty, f);

			EXPECT_PRED2(not_less_op, d, empty);
			EXPECT_PRED2(not_less_op, e, empty);
			EXPECT_PRED2(not_less_op, f, empty);

		}

		// check SingleIndex
		{
			SingleIndex s1(1);
			SingleIndex s2(2);
			SingleIndex sR;

			auto a1 = mgr.atomic(Pair(2,6));
			auto a2 = mgr.atomic(Pair(6,3));
			auto a3 = mgr.atomic(Pair(4,5));
			auto a4 = mgr.atomic(Pair(5,2));

			auto d = mgr.compound(
					entry(s1, a1),
					entry(sR, a2)
			);

			auto e = mgr.compound(
					entry(s2, a3),
					entry(sR, a4)
			);

			auto f = meet_op(d,e);

			DEBUG << "\nSingleIndex:\n";
			DEBUG << d << "\n";
			DEBUG << e << "\n";
			DEBUG << f << "\n";
			DEBUG << "1=" << projection_op(f, s1) << "\n";
			DEBUG << "2=" << projection_op(f, s2) << "\n";
			DEBUG << "*=" << projection_op(f, sR) << "\n";

			EXPECT_PRED2(less_op, d, d);
			EXPECT_PRED2(less_op, e, e);

			EXPECT_PRED2(less_op, d, f);
			EXPECT_PRED2(less_op, e, f);

			// test projection
			EXPECT_PRED2(less_op, a1, projection_op(d, s1));
			EXPECT_PRED2(less_op, a2, projection_op(d, s2));
			EXPECT_PRED2(less_op, a2, projection_op(d, sR));

			EXPECT_PRED2(less_op, a4, projection_op(e, s1));
			EXPECT_PRED2(less_op, a3, projection_op(e, s2));
			EXPECT_PRED2(less_op, a4, projection_op(e, sR));

			EXPECT_PRED2(less_op, meet_op(a1, a4), projection_op(f, s1));
			EXPECT_PRED2(less_op, meet_op(a2, a3), projection_op(f, s2));
			EXPECT_PRED2(less_op, meet_op(a2, a4), projection_op(f, sR));

			// empty-comparison
			EXPECT_PRED2(less_op, empty, d);
			EXPECT_PRED2(less_op, empty, e);
			EXPECT_PRED2(less_op, empty, f);

			EXPECT_PRED2(not_less_op, d, empty);
			EXPECT_PRED2(not_less_op, e, empty);
			EXPECT_PRED2(not_less_op, f, empty);

		}
	}

	template<template<typename L> class S>
	void testStructure() {
		testSetLattice<S>();
		testPairLattice<S>();
	}

	template<template<typename L> class S>
	void testDataSharing() {

		typedef S<utils::constraint::SetLattice<int>> Lattice;
		typedef typename Lattice::manager_type mgr_type;
		typedef typename Lattice::value_type value_type;

		mgr_type mgr;

		// check empty-value
		value_type empty;
		EXPECT_FALSE(empty.getPtr());

		auto e1 = utils::set::toSet<std::set<int>>(12);
		auto e2 = utils::set::toSet<std::set<int>>(14,16);

		// check atomic-value sharing
		{
			value_type a = mgr.atomic(e1);
			value_type b = mgr.atomic(e2);
			value_type c = mgr.atomic(e2);

			EXPECT_TRUE(a.getPtr());
			EXPECT_TRUE(b.getPtr());
			EXPECT_TRUE(c.getPtr());

			EXPECT_NE(a, b);
			EXPECT_NE(a, c);
			EXPECT_EQ(b, c);

			EXPECT_NE(a.getPtr(), b.getPtr());
			EXPECT_NE(a.getPtr(), c.getPtr());
			EXPECT_EQ(b.getPtr(), c.getPtr());
		}

		// check compound-value sharing
		{
			NominalIndex nA("a");
			NominalIndex nB("b");

			auto a1 = mgr.atomic(e1);
			auto a2 = mgr.atomic(e2);

			value_type a = mgr.compound(entry(nA, a1), entry(nB, a2));
			value_type b = mgr.compound(entry(nA, a2), entry(nB, a1));
			value_type c = mgr.compound(entry(nB, a2), entry(nA, a1));

			EXPECT_TRUE(a.getPtr());
			EXPECT_TRUE(b.getPtr());
			EXPECT_TRUE(c.getPtr());

			EXPECT_NE(a, b);
			EXPECT_EQ(a, c);
			EXPECT_NE(b, c);

			EXPECT_NE(a.getPtr(), b.getPtr());
			EXPECT_EQ(a.getPtr(), c.getPtr());
			EXPECT_NE(b.getPtr(), c.getPtr());
		}
	}


	TEST(DataStructure, UnitStructure) {
		testStructure<UnionStructureLattice>();
	}

	TEST(DataStructure, FirstOrderStructure) {
		testStructure<FirstOrderStructureLattice>();
		testDataSharing<FirstOrderStructureLattice>();
	}

	TEST(DataStructure, SecondOrderStructure) {
		testStructure<SecondOrderStructureLattice>();
		testDataSharing<SecondOrderStructureLattice>();
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

namespace std {

	template<>
	struct hash<insieme::analysis::cba::Pair> {
		std::size_t operator()(const insieme::analysis::cba::Pair& pair) const {
			return pair.first + pair.second;
		}
	};

	std::ostream& operator<<(std::ostream& out, const insieme::analysis::cba::Pair& pair) {
		return out << "[" << pair.first << "," << pair.second << "]";
	}
}
