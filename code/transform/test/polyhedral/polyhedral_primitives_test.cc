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

#include "insieme/core/ir_builder.h"

#include "insieme/transform/polyhedral/primitives.h"
#include "insieme/analysis/polyhedral/scop.h"

using namespace insieme::core;
using namespace insieme::analysis;

using namespace insieme::analysis::polyhedral;
using namespace insieme::transform::polyhedral;

Scop getScop(NodeManager& mgr) {
	IRBuilder builder(mgr);
	
	VariablePtr iter1 = builder.variable(mgr.getLangBasic().getInt4());
	VariablePtr iter2 = builder.variable(mgr.getLangBasic().getInt4());
	VariablePtr iter3 = builder.variable(mgr.getLangBasic().getInt4());

	StatementPtr nop = builder.getNoOp();

	IterationVector iterVec( { iter1, iter2, iter3 } );  // (i,j,1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	IterationDomain domain( iterVec, { { 1, 0, 0,   0 }, 
		 							   {-1, 0, 0, 100 }, 
								       { 0, 1, 0,   0 }, 
							  	       { 0,-1, 0, 100 }
								      } );
	domain &= IterationDomain( 
			AffineConstraint( AffineFunction(iterVec, {0,0,1,0}), 
			ConstraintType::EQ) 
		); 

	AffineSystem sched1( iterVec, { { 0, 0, 0, 0 }, 
								    { 1, 0, 0, 0 }, 
								    { 0, 1, 0, 0 },
								    { 0, 0, 0, 0 }
								  } );

	IterationDomain domain2( iterVec, { { 0, 0, 1, -10 }, 
		 							    { 0, 0,-1,  20 }
									  } );
	domain2 &= IterationDomain( 
			AffineConstraint( AffineFunction(iterVec, {1,0,0,0}), 
			ConstraintType::EQ) 
		);
	domain2 &= IterationDomain( 
			AffineConstraint( AffineFunction(iterVec, {0,1,0,0}), ConstraintType::EQ) 
		);

	AffineSystem sched2( iterVec, { {0, 0, 0, 1}, { 0, 0, 1, 0 } } );

	AffineSystem sched3( iterVec, { { 0, 0, 0, 0 },
									{ 1, 0, 0, 0 }, 
									{ 0, 1, 0, 0 },
									{ 0, 0, 0, 1 }
							      } );


	Scop scop(iterVec);
	scop.push_back( Stmt( 0, StatementAddress( nop ), domain, sched1 ) );
	scop.push_back( Stmt( 1, StatementAddress( nop ), domain, sched3 ) );
	scop.push_back( Stmt( 2, StatementAddress( nop ), domain2, sched2 ) );

	return scop;
}

TEST(Primitive, AddIterator) {

	using namespace insieme::analysis;
	
	NodeManager mgr;
	Scop&& scop = getScop( mgr );

	size_t iterNum = scop.getIterationVector().getIteratorNum();
	size_t paramNum = scop.getIterationVector().getParameterNum();

	IRBuilder builder(mgr);
	addTo( scop, Iterator( builder.variable(mgr.getLangBasic().getInt4()) ) );
	
	EXPECT_EQ(iterNum+1, scop.getIterationVector().getIteratorNum());
	EXPECT_EQ(paramNum, scop.getIterationVector().getParameterNum());

}

TEST(Primitive, AddParameter) {

	using namespace insieme::analysis;
	
	NodeManager mgr;
	Scop&& scop = getScop( mgr );

	size_t iterNum = scop.getIterationVector().getIteratorNum();
	size_t paramNum = scop.getIterationVector().getParameterNum();

	IRBuilder builder(mgr);
	addTo( scop, Parameter( builder.variable(mgr.getLangBasic().getInt4()) ) );
	
	EXPECT_EQ(iterNum, scop.getIterationVector().getIteratorNum());
	EXPECT_EQ(paramNum+1, scop.getIterationVector().getParameterNum());

}

TEST(Primitive, GetSubStmt) {
	NodeManager mgr;
	Scop&& scop = getScop( mgr );
	IRBuilder builder(mgr);

	{ 
		VariablePtr iter1 = builder.variable(mgr.getLangBasic().getInt4(), 1);
		std::vector<std::reference_wrapper<Stmt>>&& stmts = getLoopSubStatements(scop, iter1);

		EXPECT_EQ(2u, stmts.size());
	}

	{ 
		VariablePtr iter2 = builder.variable(mgr.getLangBasic().getInt4(), 2);
		std::vector<std::reference_wrapper<Stmt>>&& stmts = getLoopSubStatements(scop, iter2);

		EXPECT_EQ(2u, stmts.size());
	}

	{ 
		VariablePtr iter3 = builder.variable(mgr.getLangBasic().getInt4(), 3);
		std::vector<std::reference_wrapper<Stmt>>&& stmts = getLoopSubStatements(scop, iter3);

		EXPECT_EQ(1u, stmts.size());
	}
}

TEST(Primitive, ScheduleLoop) {
	NodeManager mgr;
	Scop&& scop = getScop( mgr );
	IRBuilder builder(mgr);

	VariablePtr newIter = builder.variable(mgr.getLangBasic().getInt4());
	VariablePtr iter2 = builder.variable(mgr.getLangBasic().getInt4(), 2);
	std::cout << *scop.toIR(mgr) << std::endl;

	addTo(scop, newIter);
	scheduleLoopBefore(scop, iter2, newIter);
	addConstraint(scop, newIter, IterationDomain(scop.getIterationVector(), {{0,0,0,1,0},{0,0,0,-1,100}}));
	setZeroOtherwise(scop, newIter);
	std::cout << *scop.toIR(mgr) << std::endl;

}
