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

#include "insieme/core/ir_builder.h"

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::polyhedral;

using insieme::utils::ConstraintType;

TEST(DependenceAnalysis, NoDeps) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	// Build the Iteration Vector
	IterationVector iterVec( { iter1, iter2 } );  // (i,j||1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	IterationDomain domain( iterVec, { {  1, 0,   0 },     	// v1 >= 0
		  							   { -1, 0, 100 }, 		// -v1 + 100 >= 0
  									   {  0, 1,   0 },		// v2 >= 0
									   {  0,-1, 100 } } );	// -v2 + 100 >= 0
	//~~~~~~~~~~~~
	// Scheduling 
	//~~~~~~~~~~~~
	AffineSystem sched(iterVec, { {1, 0, 0}, 
								  {0, 1, 0} } );

	//~~~~~~~~~~~~~~~~~
	// ACCESS FUNCTIONS 
	//~~~~~~~~~~~~~~~~~
	// Read Access Function A[i][j]
	AffineSystem read_access(iterVec, { {1, 0, 0}, 
							       	    {0, 1, 0} } );
	// Write Access Function A[i][j-1]
	AffineSystem write_access(iterVec, { {1, 0, 0}, 
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

	LiteralPtr	lit = builder.intLit(10);
	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	// Build the Iteration Vector
	IterationVector iterVec( { iter1, iter2 } );  // (i,j||1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	IterationDomain domain( iterVec, { {  1, 0,   0 },     	// v1 >= 0
									   { -1, 0, 100 }, 		// -v1 + 100 >= 0
									   {  0, 1,   0 },		// v2 >= 0
									   {  0,-1, 100 } } );	// -v2 + 100 >= 0
	//~~~~~~~~~~~~
	// Scheduling 
	//~~~~~~~~~~~~
	AffineSystem sched(iterVec, { {1, 0, 0}, 
								  {0, 1, 0} } );

	//~~~~~~~~~~~~~~~~~
	// ACCESS FUNCTIONS 
	//~~~~~~~~~~~~~~~~~
	// Read Access Function A[i][j]
	AffineSystem read_access(iterVec, { {1, 0, 0}, 
										{0, 1, 0} } );
	// Write Access Function A[i+1][j-1]
	AffineSystem write_access(iterVec, { {1, 0, 1}, 
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
	auto&& dist = dep::extractDistanceVector({iter1, iter2}, mgr, iterVec, c); 
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
	IterationVector iterVec( { iter1, iter2 } );  // (i,j||1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	IterationDomain domain( iterVec, { {  1, 0,   0 },     // v1 >= 0
		  							   { -1, 0, 100 }, 	// -v1 + 100 >= 0
  									   {  0, 1,   0 },		// v2 + p1 >= 0
									   {  0,-1, 100 } } );	// -v2 + 100 >= 0
	//~~~~~~~~~~~~
	// Scheduling 
	//~~~~~~~~~~~~
	AffineSystem sched(iterVec, { {1, 0, 0}, 
								  {0, 1, 0} } );

	//~~~~~~~~~~~~~~~~~
	// ACCESS FUNCTIONS 
	//~~~~~~~~~~~~~~~~~
	// Read Access Function A[i][j]
	AffineSystem read_access(iterVec, { {1, 0, 0 }, 
									    {0, 1, 0 } } );
	// Write Access Function A[i+1][j-1]
	AffineSystem write_access(iterVec, { {1, 0, 1}, 
									     {0, 1, 1} } );

	// Create ISL context
	auto ctx = makeCtx();

	TupleName tn(NodeAddress(lit), "S0");
	// Compute Dependence Analysis Read After Writes
	DependenceInfo<ISL> deps = 
		buildDependencies(
			ctx, 					 	// ctx 
			makeSet(ctx, domain, tn),	// domain
			makeMap(ctx, sched,  tn),		// sched
			makeMap(ctx, read_access, tn, TupleName(NodeAddress(lit), "A")), 	// sink
			makeMap(ctx, write_access,tn, TupleName(NodeAddress(lit), "A")),	// source
			makeEmptyMap(ctx, iterVec)
		);

	// There is a dependence
	EXPECT_FALSE( deps.mustDep->empty() );
	
	SetPtr<> deltas = deps.mustDep->deltas();

	AffineConstraintPtr c = deltas->toConstraint(mgr, iterVec);
	auto&& dist = dep::extractDistanceVector({iter1, iter2}, mgr, iterVec, c); 

//	std::cout << toString(dist.first) << std::endl;

	// 2 elements in the distance vector
	EXPECT_EQ(dist.first.size(), 2u);
	// [-1, param3]
	EXPECT_EQ(Formula(1), dist.first[0]);
	EXPECT_EQ(Formula(1), dist.first[1]);
	EXPECT_FALSE(static_cast<bool>(dist.second));
}

TEST(DependenceAnalysis, TrueDep3) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	using insieme::core::arithmetic::Formula;

	LiteralPtr  lit1 = builder.intLit(1);
	LiteralPtr	lit2 = builder.intLit(2);
	LiteralPtr	lit3 = builder.intLit(3);

	VariablePtr arr = builder.variable(mgr.getLangBasic().getInt4());

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter3 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr param1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	// Build the Iteration Vector
	IterationVector iterVec( { iter1, iter2} );  // (i,j||1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	IterationDomain domain( iterVec, { {  1, 0,   0 },     // v1 >= 0
		  						       { -1, 0, 100 }, 	// -v1 + 100 >= 0
  									   {  0, 1,   0 },		// v2 + p1 >= 0
									   {  0,-1, 100 } } );	// -v2 + 100 >= 0
	//~~~~~~~~~~~~
	// Scheduling 
	//~~~~~~~~~~~~
	AffineSystem sched(iterVec, { {1, 0, 0},
								  {0, 1, 0} } );

	//~~~~~~~~~~~~~~~~~
	// ACCESS FUNCTIONS 
	//~~~~~~~~~~~~~~~~~
	// Read Access Function A[i][j]
	AffineSystem read_access(iterVec, { {1, 0, 0 }, 
							     	    {0, 1, 0 } } );
	// Write Access Function A[i+1][j-1]
	AffineSystem write_access(iterVec, { {0, 1, 1}, 
								         {0, 0, 1} } );

	// Create ISL context
	auto ctx = makeCtx();

	// Compute Dependence Analysis Read After Writes
	Scop scop(iterVec);

	scop.push_back( Stmt( 0, StatementAddress(lit1), domain, sched, 
				{ std::make_shared<AccessInfo>(ExpressionAddress(arr), Ref::ARRAY, Ref::USE, read_access, IterationDomain(iterVec)) } ) 
			);
	scop.push_back( Stmt( 1, StatementAddress(lit2), domain, sched, 
				{ std::make_shared<AccessInfo>(ExpressionAddress(arr), Ref::ARRAY, Ref::DEF, write_access, IterationDomain(iterVec))}) );

	//~~~~~~~~~~~~
	// Scheduling 
	//~~~~~~~~~~~~
	AffineSystem sched1(iterVec, { {1, 0, 0} } );

	//~~~~~~~~~~~~~~~~~
	// ACCESS FUNCTIONS 
	//~~~~~~~~~~~~~~~~~
	AffineSystem write_access1(iterVec, { {1, 0,  1 }, 
										  {0, 0, -1 } } );

	scop.push_back( Stmt( 2, StatementAddress(lit3), domain, sched1,
				{ std::make_shared<AccessInfo>(ExpressionAddress(arr), Ref::ARRAY, Ref::DEF, write_access1, IterationDomain(iterVec))}) );
	
	{
		dep::DependenceGraph depGraph(mgr, scop, dep::ALL);
	
		// Let's print the graph
		// std::cout << depGraph << std::endl;

		EXPECT_EQ(3u, depGraph.size());

		// There is only 1 dependence between 0 and 1
		EXPECT_EQ(2u, std::distance(depGraph.deps_begin(0,1), depGraph.deps_end(0,1)));
		// the type of the dependence is write-after-read
		EXPECT_EQ(dep::WAR, depGraph.deps_begin(0,1)->type());
		
		// Two dependenceis between 1 and 0
		EXPECT_EQ(2u, std::distance(depGraph.deps_begin(1,0), depGraph.deps_end(1,0)));
		dep::DependenceGraph::DependenceIterator it = depGraph.deps_begin(1,0);
		EXPECT_EQ(dep::RAW, it->type());
		EXPECT_EQ(dep::RAW, (++it)->type());
		//EXPECT_EQ(++it, depGraph.deps_end(1,0));
		
		EXPECT_EQ(1u, std::distance(depGraph.deps_begin(1,1), depGraph.deps_end(1,1)));
		EXPECT_EQ(dep::WAW, depGraph.deps_begin(1,1)->type());

		auto fit = depGraph.getStatementID( StatementAddress(lit2) );
		EXPECT_TRUE(fit);
		EXPECT_EQ(1u, *fit);

		EXPECT_EQ(lit3, depGraph.getStatementAddress(2));
		
		const dep::Stmt& s2 = depGraph.getStatement(2);
		EXPECT_EQ(0u, s2.in_degree());
		
		for_each(s2.incoming_begin(), s2.incoming_end(), [&](const dep::Dependence& cur) {
				EXPECT_EQ(s2, cur.sink());
			});

		EXPECT_EQ(0u, s2.out_degree());
		for_each(s2.outgoing_begin(), s2.outgoing_end(), [&](const dep::Dependence& cur) {
				EXPECT_EQ(s2, cur.source());
			});
		
		auto&& sc = depGraph.strongComponents();
		EXPECT_EQ(2u, sc.size());

		EXPECT_EQ(2u, sc[0].second.size());
		EXPECT_EQ(1u, sc[1].second.size());

	}

}


