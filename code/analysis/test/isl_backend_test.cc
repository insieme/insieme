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

#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "insieme/core/ir_expressions.h"

#include "isl/map.h"
#include "isl/union_map.h"

using namespace insieme;
using namespace insieme::analysis;
using namespace insieme::core;
using insieme::utils::ConstraintType;

typedef std::vector<int> CoeffVect;
typedef std::vector<CoeffVect> CoeffMatrix;

#define CREATE_ITER_VECTOR \
	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); \
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); \
	\
	poly::IterationVector iterVec; \
	\
	iterVec.add( poly::Iterator(iter1) ); \
	EXPECT_EQ(static_cast<size_t>(2), iterVec.size()); \
	iterVec.add( poly::Parameter(param) ); \
	EXPECT_EQ(static_cast<size_t>(3), iterVec.size()); \


TEST(IslBackend, SetCreation) {
	
	NodeManager mgr;	
	CREATE_ITER_VECTOR; 

	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& set = poly::makeSet<poly::ISL>(*ctx, poly::IterationDomain(iterVec));

	std::ostringstream ss;
	set->printTo(ss);
	EXPECT_EQ("[v3] -> { [v1] }", ss.str());
	
}

TEST(IslBackend, SetConstraint) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, CoeffVect({0,3,10}) );
	poly::AffineConstraint c(af, ConstraintType::LT);

	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& set = poly::makeSet<poly::ISL>(*ctx, poly::IterationDomain(c));

	std::ostringstream ss;
	set->printTo(ss);
	EXPECT_EQ("[v3] -> { [v1] : v3 <= -4 }", ss.str());

	// Build directly the ISL set
	isl_set* refSet = isl_set_read_from_str(ctx->getRawContext(), 
			"[v3] -> { [v1] : 3v3 + 10 < 0}", -1
		);
	isl_union_set* tmp = isl_union_set_from_set(refSet);
	// check for equality
	EXPECT_TRUE( isl_union_set_is_equal(tmp , const_cast<isl_union_set*>(set->getAsIslSet())) );
	
	isl_union_set_free(tmp);
}

TEST(IslBackend, SetConstraintNormalized) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, CoeffVect({1,0,10}) );
	
	// 1*v1 + 0*v2  + 10 != 0
	poly::AffineConstraint c(af, ConstraintType::NE);

	// 1*v1 + 10 > 0 && 1*v1 +10 < 0
	// 1*v1 + 9 >= 0 & -1*v1 -11 >= 0
	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& set = poly::makeSet<poly::ISL>(*ctx, poly::IterationDomain(c));

	std::ostringstream ss;
	set->printTo(ss);
	EXPECT_EQ("[v3] -> { [v1] : v1 <= -11 or v1 >= -9 }", ss.str());

	// Build directly the ISL set
	isl_set* refSet = isl_set_read_from_str(ctx->getRawContext(), 
			"[v3] -> {[v1] : v1 + 10 < 0 or v1 + 10 > 0}", -1
		);
	
	isl_union_set* tmp = isl_union_set_from_set(refSet);
	// check for equality
	EXPECT_TRUE(isl_union_set_is_equal(tmp, const_cast<isl_union_set*>(set->getAsIslSet())));
	
	isl_union_set_free(tmp);
}

TEST(IslBackend, FromCombiner) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	// 0*v1 + 2*v2 + 10
	poly::AffineFunction af(iterVec, CoeffVect({0,2,10}) );

	// 0*v1 + 2*v3 + 10 == 0
	poly::AffineConstraint c1(af, ConstraintType::EQ);

	// 2*v1 + 3*v3 +10 
	poly::AffineFunction af2(iterVec, CoeffVect({2,3,10}) );
	
	// 2*v1 + 3*v3 +10 < 0
	poly::AffineConstraint c2(af2, ConstraintType::LT);

	// 2v3+10 == 0 OR !(2v1 + 3v3 +10 < 0)
	
	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& set = poly::makeSet<poly::ISL>(*ctx, poly::IterationDomain( c1 or (not_(c2)) ));

	std::ostringstream ss;
	set->printTo(ss);
	EXPECT_EQ("[v3] -> { [v1] : v3 = -5 or 2v1 >= -10 - 3v3 }", ss.str());

	// Build directly the ISL set
	isl_set* refSet = isl_set_read_from_str(ctx->getRawContext(), 
			"[v3] -> {[v1] : 2*v3 + 10 = 0 or 2*v1 +3*v3 +10 >= 0}", -1
		);
	
	isl_union_set* tmp = isl_union_set_from_set(refSet);
	// check for equality
	EXPECT_TRUE( isl_union_set_is_equal(tmp, const_cast<isl_union_set*>(set->getAsIslSet())) );
	
	isl_union_set_free(tmp);
}

TEST(IslBackend, SetUnion) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;
	poly::AffineConstraint c1(poly::AffineFunction(iterVec, CoeffVect({0,3,10})), ConstraintType::LT);
	poly::AffineConstraint c2(poly::AffineFunction(iterVec, CoeffVect({1,-1,0})), ConstraintType::EQ);

	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& set1 = poly::makeSet<poly::ISL>(*ctx, poly::IterationDomain(makeCombiner(c1)) );
	auto&& set2 = poly::makeSet<poly::ISL>(*ctx, poly::IterationDomain(makeCombiner(c2)) );

	auto&& set = set_union(*ctx, *set1, *set2);
	
	// Build directly the ISL set
	isl_union_set* refSet = isl_union_set_read_from_str(ctx->getRawContext(), 
			"[v3] -> { [v1] : 3*v3 + 10 < 0; [v1] : 1*v1 -1*v3 = 0}"
		);

	EXPECT_TRUE( isl_union_set_is_equal(refSet, const_cast<isl_union_set*>(set->getAsIslSet())) );

	isl_union_set_free(refSet);
}

TEST(IslBackend, SimpleMap) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineSystem affSys(iterVec, CoeffMatrix({ { 0, 2, 10 }, 
		  										     { 1, 1,  0 },
												     { 1,-1,  8 } } ) );
	// 0*v1 + 2*v2 + 10
	
	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& map = poly::makeMap<poly::ISL>(*ctx, affSys);

	std::ostringstream ss;
	map->printTo(ss);
	EXPECT_EQ("[v3] -> { [v1] -> [10 + 2v3, v3 + v1, 8 - v3 + v1] }", ss.str());

	isl_union_map* refMap = isl_union_map_read_from_str(ctx->getRawContext(), 
			"[v3] -> {[v1] -> [10 + 2v3, v3 + v1, 8 - v3 + v1]; }"
		);

	EXPECT_TRUE( isl_union_map_is_equal(refMap, const_cast<isl_union_map*>(map->getAsIslMap())) );

	isl_union_map_free(refMap);
}

TEST(IslBackend, MapUnion) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineSystem affSys(iterVec, CoeffMatrix({ { 0, 2, 10 }, 
		  										     { 1, 1,  0 }, 
												     { 1,-1,  8 } } ));
	// 0*v1 + 2*v2 + 10
	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& map = poly::makeMap<poly::ISL>(*ctx, affSys);

	std::ostringstream ss;
	map->printTo(ss);
	EXPECT_EQ("[v3] -> { [v1] -> [10 + 2v3, v3 + v1, 8 - v3 + v1] }", ss.str());
	
	poly::AffineSystem affSys2(iterVec, CoeffMatrix({ { 1,-2, 0 }, 
		 										      { 1, 8, 4 }, 
												      {-5,-1, 4 } } ) );
	// 0*v1 + 2*v2 + 10
	auto&& map2 = poly::makeMap<poly::ISL>( *ctx, affSys2 );

	std::ostringstream ss2;
	map2->printTo(ss2);
	EXPECT_EQ("[v3] -> { [v1] -> [-2v3 + v1, 4 + 8v3 + v1, 4 - v3 - 5v1] }", ss2.str());

	auto&& mmap = map_union(*ctx, *map, *map2);
	std::ostringstream  ss3;
	mmap->printTo(ss3);
	EXPECT_EQ("[v3] -> { [v1] -> [10 + 2v3, v3 + v1, 8 - v3 + v1]; [v1] -> [-2v3 + v1, 4 + 8v3 + v1, 4 - v3 - 5v1] }", ss3.str());
}

namespace {

poly::IterationDomain createFloor( poly::IterationVector& iterVec, int N, int D ) {

	return poly::IterationDomain(
		poly::AffineConstraint(
			// p - exist1*D - exist2 == 0
			poly::AffineFunction( iterVec, CoeffVect({ -D, -1,  1,  0 }) ), poly::ConstraintType::EQ) and 
		poly::AffineConstraint(
			// exist2 - D < 0
			poly::AffineFunction( iterVec, CoeffVect({  0,  1,  0, -D }) ), utils::ConstraintType::LT) and
		poly::AffineConstraint(
			// exist2 >= 0
			poly::AffineFunction( iterVec, CoeffVect({  0,  1,  0,  0 }) ), utils::ConstraintType::GE) and
		poly::AffineConstraint(
			// N == N
			poly::AffineFunction( iterVec, CoeffVect({  0,  0,  1, -N })), utils::ConstraintType::EQ)
	);

}

poly::IterationDomain createCeil( poly::IterationVector& iterVec, int N, int D ) {

	return poly::IterationDomain(
		poly::AffineConstraint(
			// p - exist1*D + exist2 == 0
			poly::AffineFunction( iterVec, CoeffVect({ -D, +1,  1,  0 }) ), poly::ConstraintType::EQ) and 
		poly::AffineConstraint(
			// exist2 - D < 0
			poly::AffineFunction( iterVec, CoeffVect({  0,  1,  0, -D }) ), utils::ConstraintType::LT) and
		poly::AffineConstraint(
			// exist2 >= 0
			poly::AffineFunction( iterVec, CoeffVect({  0,  1,  0,  0 }) ), utils::ConstraintType::GE) and
		poly::AffineConstraint(
			// N == N
			poly::AffineFunction( iterVec, CoeffVect({  0,  0,  1, -N })), utils::ConstraintType::EQ)
	);

}
} // end anonymous namespace

TEST(IslBackend, Floor) {
	NodeManager mgr;

	VariablePtr exist1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1);
	VariablePtr exist2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2);
	VariablePtr p = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3);

	poly::IterationVector iterVec;
	iterVec.add( poly::Iterator(exist1) );
	iterVec.add( poly::Iterator(exist2, true) );
	iterVec.add( poly::Parameter(p) );

	{
		poly::IterationDomain dom = createFloor(iterVec, 24, 5);
		auto&& ctx = poly::createContext<poly::ISL>();
		auto&& set = poly::makeSet<poly::ISL>(*ctx, dom);

		std::ostringstream ss;
		set->printTo(ss);
		EXPECT_EQ(ss.str(), "[v3] -> { [4] : v3 = 24 }");
	}
	{
		poly::IterationDomain dom = createFloor(iterVec, 40, 5);
		auto&& ctx = poly::createContext<poly::ISL>();
		auto&& set = poly::makeSet<poly::ISL>(*ctx, dom);

		std::ostringstream ss;
		set->printTo(ss);
		EXPECT_EQ(ss.str(), "[v3] -> { [8] : v3 = 40 }");
	}
}

TEST(IslBackend, Ceil) {
	NodeManager mgr;

	VariablePtr exist1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1);
	VariablePtr exist2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2);
	VariablePtr p = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3);

	poly::IterationVector iterVec;
	iterVec.add( poly::Iterator(exist1) );
	iterVec.add( poly::Iterator(exist2, true) );
	iterVec.add( poly::Parameter(p) );

	{
		poly::IterationDomain dom = createCeil(iterVec, 24, 5);
		auto&& ctx = poly::createContext<poly::ISL>();
		auto&& set = poly::makeSet<poly::ISL>(*ctx, dom);

		std::ostringstream ss;
		set->printTo(ss);
		EXPECT_EQ(ss.str(), "[v3] -> { [5] : v3 = 24 }");
	}
	{
		poly::IterationDomain dom = createCeil(iterVec, 40, 5);
		auto&& ctx = poly::createContext<poly::ISL>();
		auto&& set = poly::makeSet<poly::ISL>(*ctx, dom);

		std::ostringstream ss;
		set->printTo(ss);
		EXPECT_EQ(ss.str(), "[v3] -> { [8] : v3 = 40 }");
	}
}



