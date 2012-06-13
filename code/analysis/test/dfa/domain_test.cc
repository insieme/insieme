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

#include "insieme/analysis/dfa/domain.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;

TEST(Set, StdSet) {

	Set<int> simpleSet{ 2, 3, 5, 2 };

	EXPECT_EQ(3u, simpleSet.size());

	EXPECT_TRUE(simpleSet.contains(3));

	EXPECT_FALSE(simpleSet.contains(6));
}

TEST(Set, PointerSet) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr a = builder.variable(builder.getLangBasic().getInt4()),
				b = builder.variable(builder.getLangBasic().getInt4()),
				c = builder.variable(builder.getLangBasic().getInt4()),
				d = builder.variable(builder.getLangBasic().getInt4());

	Set<VariablePtr, utils::set::PointerSet> ptrSet{ a, b, c };

	EXPECT_EQ(3u, ptrSet.size());

	EXPECT_TRUE(ptrSet.contains(a));

	EXPECT_FALSE(ptrSet.contains(d));
}

TEST(PowerSet, StdSet) {

	auto stdSet = makePowerSet( Set<int>{2,3,5,2} );

	EXPECT_EQ(8u, stdSet.size());

	EXPECT_TRUE( stdSet.contains( {2,3} ) );
	EXPECT_TRUE( stdSet.contains( {2} ) );
	EXPECT_TRUE( stdSet.contains( {3} ) );
	EXPECT_TRUE( stdSet.contains( {2,3,5} ) );
	EXPECT_TRUE( stdSet.contains( { } ) );

	EXPECT_FALSE( stdSet.contains( {2,1} ) );
	EXPECT_FALSE( stdSet.contains( {1} ) );
	EXPECT_FALSE( stdSet.contains( {2,3,5,1} ) );

}

TEST(PowerSet, CustomSet) {

	auto stdSet = makePowerSet( Set<int>{2,3,5,2} );
	EXPECT_EQ(8u, stdSet.size());

	EXPECT_TRUE( stdSet.contains( {2,3} ) );
	EXPECT_TRUE( stdSet.contains( {2} ) );
	EXPECT_TRUE( stdSet.contains( {3} ) );
	EXPECT_TRUE( stdSet.contains( {2,3,5} ) );
	EXPECT_TRUE( stdSet.contains( { } ) );

	EXPECT_FALSE( stdSet.contains( {2,1} ) );
	EXPECT_FALSE( stdSet.contains( {1} ) );
	EXPECT_FALSE( stdSet.contains( {2,3,5,1} ) );

}

TEST(PowerSet, PointerSet) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr a = builder.variable(builder.getLangBasic().getInt4()),
				b = builder.variable(builder.getLangBasic().getInt4()),
				c = builder.variable(builder.getLangBasic().getInt4()),
				d = builder.variable(builder.getLangBasic().getInt4());

	auto ptrSet = makePowerSet( utils::set::PointerSet<VariablePtr>{ a, b } );

	EXPECT_EQ(4u, ptrSet.size());

	EXPECT_TRUE( ptrSet.contains( { a } ) );

	EXPECT_FALSE( ptrSet.contains( { d,b } ) );
}

TEST(ProdSet, StdSet) {

	auto stdSet = makeCartProdSet( std::set<int>{ 2, 3 }, 
							   std::set<bool>{true, false} 
							 ) ;

	EXPECT_EQ(4u, stdSet.size());

	EXPECT_TRUE( stdSet.contains( std::make_tuple(3,true) ) );
	EXPECT_TRUE( stdSet.contains( std::make_tuple(2,false) ) );
	EXPECT_FALSE( stdSet.contains( std::make_tuple(1,false) ) );

}

TEST(ProdSet, StdSet2) {

	auto stdSet = makeCartProdSet( std::set<int>{ 2, 3 }, 
							   std::set<bool>{true, false} 
							 ) ;

	auto ppset = makeCartProdSet(stdSet, stdSet);

	EXPECT_EQ(16u, ppset.size());

	EXPECT_TRUE( ppset.contains( std::make_tuple( 3,true,2,false) ) );
	EXPECT_TRUE( ppset.contains( std::make_tuple( 2,false,3,true) ) );
	EXPECT_FALSE( ppset.contains( std::make_tuple( 1,false,2,true) ) );
}


TEST(ProdSet, StdSet3) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr a = builder.variable(builder.getLangBasic().getInt4()),
				b = builder.variable(builder.getLangBasic().getInt4()),
				c = builder.variable(builder.getLangBasic().getInt4()),
				d = builder.variable(builder.getLangBasic().getInt4());

	auto stdSet = makeCartProdSet( 
			std::set<int>{ 2, 3 }, 
			std::set<bool>{true, false} 
		);

	{
		auto ppset = makeCartProdSet(stdSet,  utils::set::PointerSet<VariablePtr>{ a, b });

		EXPECT_EQ(2u*2u*2u, ppset.size());

		EXPECT_TRUE( ppset.contains( std::make_tuple( 3,true,a ) ) );
		EXPECT_TRUE( ppset.contains( std::make_tuple( 2,false,b ) ) );
		EXPECT_FALSE( ppset.contains( std::make_tuple( 1,false,c ) ) );
	}

	{
		auto ppset = makeCartProdSet(utils::set::PointerSet<VariablePtr>{ a, b, c }, stdSet);

		EXPECT_EQ(3u*2u*2u, ppset.size());

		EXPECT_TRUE( ppset.contains( std::make_tuple( a,3,true ) ) );
		EXPECT_FALSE( ppset.contains( std::make_tuple( d,2,false ) ) );
		EXPECT_TRUE( ppset.contains( std::make_tuple( c,3,false ) ) );
	}
}

TEST(ProdSet, PowerSet) {

	auto pset = makeCartProdSet( makePowerSet( Set<int>{2,3} ), 
								 makePowerSet( std::set<int>{3,4} )
						       );

	EXPECT_EQ(16u, pset.size());

	EXPECT_TRUE( pset.contains( std::make_tuple(Set<int>{2}, std::set<int>{}) ) );

}

TEST(ProdSet, Composed) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr a = builder.variable(builder.getLangBasic().getInt4()),
				b = builder.variable(builder.getLangBasic().getInt4()),
				c = builder.variable(builder.getLangBasic().getInt4()),
				d = builder.variable(builder.getLangBasic().getInt4());
	

	auto prod = makeCartProdSet( 
			utils::set::PointerSet<VariablePtr>{a,b,c}, 
			std::set<int>{0,1,2,3,4} 
		);

	EXPECT_EQ(15u, prod.size());
	EXPECT_TRUE( prod.contains( std::make_tuple(a, 1) ) );

	// make powerset
	auto pset = makePowerSet( prod );
 	EXPECT_EQ(pow(2,15u), pset.size());

	EXPECT_TRUE( pset.contains( { std::make_tuple(a,0), std::make_tuple(a,1) } ) );
	EXPECT_FALSE( pset.contains( { std::make_tuple(a,5), std::make_tuple(b,0) } ) );
}

