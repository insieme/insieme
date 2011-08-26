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

#include "insieme/analysis/polyhedral/iter_vec.h"
#include "insieme/analysis/polyhedral/affine_func.h"
#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "insieme/core/expressions.h"

#include "isl/map.h"
#include "isl/union_map.h"

using namespace insieme;
using namespace insieme::analysis;
using namespace insieme::core;

#define CREATE_ITER_VECTOR \
	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1); \
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3); \
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
	auto&& set = poly::makeSet<poly::ISL>(*ctx, iterVec, poly::ConstraintCombinerPtr(), "S0");

	std::ostringstream ss;
	set->printTo(ss);
	EXPECT_EQ("[v3] -> { S0[v1] }", ss.str());
	
}

TEST(IslBackend, SetConstraint) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, {0,3,10} );
	poly::Constraint c(af, poly::Constraint::LT);

	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& set = poly::makeSet<poly::ISL>(*ctx, iterVec, makeCombiner(c), "S0");

	std::ostringstream ss;
	set->printTo(ss);
	EXPECT_EQ("[v3] -> { S0[v1] : v3 <= -4 }", ss.str());

	// Build directly the ISL set
	isl_set* refSet = isl_set_read_from_str(ctx->getRawContext(), 
			"[v3] -> {S0[v1] : 3v3 + 10 < 0}", -1
		);
	isl_union_set* tmp = isl_union_set_from_set(refSet);
	// check for equality
	EXPECT_TRUE( isl_union_set_is_equal(tmp , const_cast<isl_union_set*>(set->getAsIslSet())) );
	
	isl_union_set_free(tmp);
}

TEST(IslBackend, SetConstraintNormalized) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, {1,0,10});
	
	// 1*v1 + 0*v2  + 10 != 0
	poly::Constraint c(af, poly::Constraint::NE);

	// 1*v1 + 10 > 0 && 1*v1 +10 < 0
	// 1*v1 + 9 >= 0 & -1*v1 -11 >= 0
	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& set = poly::makeSet<poly::ISL>(*ctx, iterVec, makeCombiner(c), "S0");

	std::ostringstream ss;
	set->printTo(ss);
	EXPECT_EQ("[v3] -> { S0[v1] : v1 <= -11 or v1 >= -9 }", ss.str());

	// Build directly the ISL set
	isl_set* refSet = isl_set_read_from_str(ctx->getRawContext(), 
			"[v3] -> {S0[v1] : v1 + 10 < 0 or v1 + 10 > 0}", -1
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
	poly::AffineFunction af(iterVec, {0,2,10});

	// 0*v1 + 2*v3 + 10 == 0
	poly::Constraint c1(af, poly::Constraint::EQ);

	// 2*v1 + 3*v3 +10 
	poly::AffineFunction af2(iterVec, {2,3,10});
	
	// 2*v1 + 3*v3 +10 < 0
	poly::Constraint c2(af2, poly::Constraint::LT);

	// 2v3+10 == 0 OR !(2v1 + 3v3 +10 < 0)
	poly::ConstraintCombinerPtr ptr =  c1 or (not_(c2));
	
// 	std::cout << *ptr << std::endl;

	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& set = poly::makeSet<poly::ISL>(*ctx, iterVec, ptr, "S0");

	std::ostringstream ss;
	set->printTo(ss);
	EXPECT_EQ("[v3] -> { S0[v1] : v3 = -5 or 2v1 >= -10 - 3v3 }", ss.str());

	// Build directly the ISL set
	isl_set* refSet = isl_set_read_from_str(ctx->getRawContext(), 
			"[v3] -> {S0[v1] : 2*v3 + 10 = 0 or 2*v1 +3*v3 +10 >= 0}", -1
		);
	
	isl_union_set* tmp = isl_union_set_from_set(refSet);
	// check for equality
	EXPECT_TRUE( isl_union_set_is_equal(tmp, const_cast<isl_union_set*>(set->getAsIslSet())) );
	
	isl_union_set_free(tmp);
}

TEST(IslBackend, SetUnion) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;
	poly::Constraint c1(poly::AffineFunction(iterVec, {0,3,10}), poly::Constraint::LT);
	poly::Constraint c2(poly::AffineFunction(iterVec, {1,-1,0}), poly::Constraint::EQ);

	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& set1 = poly::makeSet<poly::ISL>(*ctx, iterVec, makeCombiner(c1), "S0");
	auto&& set2 = poly::makeSet<poly::ISL>(*ctx, iterVec, makeCombiner(c2), "S1");

	auto&& set = set_union(*ctx, *set1, *set2);
	
	// Build directly the ISL set
	isl_union_set* refSet = isl_union_set_read_from_str(ctx->getRawContext(), 
			"[v3] -> {S0[v1] : 3*v3 + 10 < 0; S1[v1] : 1*v1 -1*v3 = 0}"
		);

	EXPECT_TRUE( isl_union_set_is_equal(refSet, const_cast<isl_union_set*>(set->getAsIslSet())) );

	isl_union_set_free(refSet);
}

TEST(IslBackend, SimpleMap) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineSystemPtr affSys = std::make_shared<poly::AffineSystem>(iterVec);
	// 0*v1 + 2*v2 + 10
	affSys->appendRow( poly::AffineFunction(iterVec, {0,2,10}) );
	affSys->appendRow( poly::AffineFunction(iterVec, {1,1,0}) );
	affSys->appendRow( poly::AffineFunction(iterVec, {1,-1,8}) );
	
	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& map = poly::makeMap<poly::ISL>(*ctx, *affSys, "S0");

	std::ostringstream ss;
	map->printTo(ss);
	EXPECT_EQ("[v3] -> { S0[v1] -> [10 + 2v3, v3 + v1, 8 - v3 + v1] }", ss.str());

	isl_union_map* refMap = isl_union_map_read_from_str(ctx->getRawContext(), 
			"[v3] -> {S0[v1] -> [10 + 2v3, v3 + v1, 8 - v3 + v1]; }"
		);

	EXPECT_TRUE( isl_union_map_is_equal(refMap, const_cast<isl_union_map*>(map->getAsIslMap())) );

	isl_union_map_free(refMap);
}

TEST(IslBackend, MapUnion) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineSystemPtr affSys = std::make_shared<poly::AffineSystem>(iterVec);
	// 0*v1 + 2*v2 + 10
	affSys->appendRow( poly::AffineFunction(iterVec, {0,2,10}) );
	affSys->appendRow( poly::AffineFunction(iterVec, {1,1,0}) );
	affSys->appendRow( poly::AffineFunction(iterVec, {1,-1,8}) );
	
	auto&& ctx = poly::createContext<poly::ISL>();
	auto&& map = poly::makeMap<poly::ISL>(*ctx, *affSys, "S0");

	std::ostringstream ss;
	map->printTo(ss);
	EXPECT_EQ("[v3] -> { S0[v1] -> [10 + 2v3, v3 + v1, 8 - v3 + v1] }", ss.str());
	
	poly::AffineSystemPtr affSys2 = std::make_shared<poly::AffineSystem>(iterVec);
	// 0*v1 + 2*v2 + 10
	affSys2->appendRow( poly::AffineFunction(iterVec, {1,-2,0}) );
	affSys2->appendRow( poly::AffineFunction(iterVec, {1,8,4}) );
	affSys2->appendRow( poly::AffineFunction(iterVec, {-5,-1,4}) );

	auto&& map2 = poly::makeMap<poly::ISL>(*ctx, *affSys2, "S1");

	std::ostringstream ss2;
	map2->printTo(ss2);
	EXPECT_EQ("[v3] -> { S1[v1] -> [-2v3 + v1, 4 + 8v3 + v1, 4 - v3 - 5v1] }", ss2.str());

	auto&& mmap = map_union(*ctx, *map, *map2);
	std::ostringstream  ss3;
	mmap->printTo(ss3);
	EXPECT_EQ("[v3] -> { S0[v1] -> [10 + 2v3, v3 + v1, 8 - v3 + v1]; S1[v1] -> [-2v3 + v1, 4 + 8v3 + v1, 4 - v3 - 5v1] }", ss3.str());

}


