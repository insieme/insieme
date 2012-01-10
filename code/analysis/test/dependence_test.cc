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
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/analysis/dep_graph.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_builder.h"

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::poly;

using insieme::utils::ConstraintType;

TEST(DependenceAnalysis, NoDeps) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	// Build the Iteration Vector
	poly::IterationVector iterVec( { iter1, iter2 } );  // (i,j||1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	poly::IterationDomain domain( iterVec, { {  0, 0,   0 },     	// v1 >= 0
		  								     { -1, 0, 100 }, 		// -v1 + 100 >= 0
  										     {  0, 1,   0 },		// v2 >= 0
										  	 {  0,-1, 100 } } );	// -v2 + 100 >= 0
	//~~~~~~~~~~~~
	// Scheduling 
	//~~~~~~~~~~~~
	poly::AffineSystem sched(iterVec, { {1, 0, 0}, 
									    {0, 1, 0} } );

	//~~~~~~~~~~~~~~~~~
	// ACCESS FUNCTIONS 
	//~~~~~~~~~~~~~~~~~
	// Read Access Function A[i][j]
	poly::AffineSystem read_access(iterVec, { {1, 0, 0}, 
									     	  {0, 1, 0} } );
	// Write Access Function A[i][j-1]
	poly::AffineSystem write_access(iterVec, { {1, 0, 0}, 
										       {0, 1, -1} } );

	// Create ISL context
	auto ctx = makeCtx();

	// Compute Dependence Analysis Read After Writes
	DependenceInfo<ISL> deps = 
		buildDependencies(
			ctx, 					 	// ctx 
			makeSet(ctx, domain),   	// domain
			makeMap(ctx, sched),		// sched
			makeMap(ctx, read_access), 	// sink
			makeMap(ctx, write_access),	// source
			makeEmptyMap(ctx, iterVec)
		);

	// There are no dependencies 
	EXPECT_TRUE( deps.mustDep->empty() );
}

TEST(DependenceAnalysis, TrueDep) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	// Build the Iteration Vector
	poly::IterationVector iterVec( { iter1, iter2 } );  // (i,j||1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	poly::IterationDomain domain( iterVec, { {  0, 0,   0 },     	// v1 >= 0
		  								     { -1, 0, 100 }, 		// -v1 + 100 >= 0
  										     {  0, 1,   0 },		// v2 >= 0
										  	 {  0,-1, 100 } } );	// -v2 + 100 >= 0
	//~~~~~~~~~~~~
	// Scheduling 
	//~~~~~~~~~~~~
	poly::AffineSystem sched(iterVec, { {1, 0, 0}, 
									    {0, 1, 0} } );

	//~~~~~~~~~~~~~~~~~
	// ACCESS FUNCTIONS 
	//~~~~~~~~~~~~~~~~~
	// Read Access Function A[i][j]
	poly::AffineSystem read_access(iterVec, { {1, 0, 0}, 
									     	  {0, 1, 0} } );
	// Write Access Function A[i+1][j-1]
	poly::AffineSystem write_access(iterVec, { {1, 0, 1}, 
										       {0, 1, -1} } );

	// Create ISL context
	auto ctx = makeCtx();

	// Compute Dependence Analysis Read After Writes
	DependenceInfo<ISL> deps = 
		buildDependencies(
			ctx, 					 	// ctx 
			makeSet(ctx, domain),   	// domain
			makeMap(ctx, sched),		// sched
			makeMap(ctx, read_access), 	// sink
			makeMap(ctx, write_access),	// source
			makeEmptyMap(ctx, iterVec)
		);

	// There is a dependence
	EXPECT_FALSE( deps.mustDep->empty() );
	
	SetPtr<> deltas = deps.mustDep->deltas();

	AffineConstraintPtr c = deltas->toConstraint(mgr, iterVec);
	auto&& dist = dep::extractDistanceVector(mgr, iterVec, c); 
	// 2 elements in the distance vector
	EXPECT_EQ(dist.first.size(), 2u);
	// [-1, param3]
	EXPECT_EQ(dist.first[0], 1);
	EXPECT_EQ(dist.first[1], -1);
	EXPECT_TRUE( !dist.second );
}

TEST(DependenceAnalysis, TrueDep2) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	using insieme::core::arithmetic::Formula;

	LiteralPtr  lit = builder.intLit(0);

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 

	VariablePtr param1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	// Build the Iteration Vector
	poly::IterationVector iterVec( { iter1, iter2 }, {param1} );  // (i,j||1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	poly::IterationDomain domain( iterVec, { {  0, 0, 0,   0 },     // v1 >= 0
		  								     { -1, 0, 0, 100 }, 	// -v1 + 100 >= 0
  										     {  0, 1, 1,   0 },		// v2 + p1 >= 0
										  	 {  0,-1, 0, 100 } } );	// -v2 + 100 >= 0
	//~~~~~~~~~~~~
	// Scheduling 
	//~~~~~~~~~~~~
	poly::AffineSystem sched(iterVec, { {1, 0, 0, 0}, 
									    {0, 1, 0, 0} } );

	//~~~~~~~~~~~~~~~~~
	// ACCESS FUNCTIONS 
	//~~~~~~~~~~~~~~~~~
	// Read Access Function A[i][j]
	poly::AffineSystem read_access(iterVec, { {1, 0, 0, 0 }, 
									     	  {0, 1, 0, 0 } } );
	// Write Access Function A[i+1][j-1]
	poly::AffineSystem write_access(iterVec, { {1, 0, 0, 1}, 
										       {0, 1, -1, 0} } );

	// Create ISL context
	auto ctx = makeCtx();

	TupleName tn(NodeAddress(lit), "S0");
	// Compute Dependence Analysis Read After Writes
	//DependenceInfo<ISL> deps = 
		//buildDependencies(
			//ctx, 					 	// ctx 
			//makeSet(ctx, domain, tn),	// domain
			//makeMap(ctx, sched,  tn),		// sched
			//makeMap(ctx, read_access, tn, TupleName(NodeAddress(lit), "A")), 	// sink
			//makeMap(ctx, write_access,tn, TupleName(NodeAddress(lit), "A")),	// source
			//makeEmptyMap(ctx, iterVec)
		//);

	// There is a dependence
	//EXPECT_FALSE( deps.mustDep->empty() );
	
	//SetPtr<> deltas = deps.mustDep->deltas();

	//AffineConstraintPtr c = deltas->toConstraint(mgr, iterVec);
	//auto&& dist = dep::extractDistanceVector(mgr, iterVec, c); 

	// 2 elements in the distance vector
	//EXPECT_EQ(dist.first.size(), 2u);
	// [-1, param3]
	//EXPECT_EQ(Formula(1), dist.first[0]);
	//EXPECT_EQ(Formula()-param1, dist.first[1]);
	//EXPECT_TRUE(static_cast<bool>(dist.second));

	// build the dependence graph
//	dep::DependenceGraph dg( mgr, 1, ctx, deps.mustDep );
//	std::cout << dg << std::endl;
}
