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
#include <limits>
#include <algorithm>

#include "insieme/analysis/dfa/lattice.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/utils/logging.h"

#include "insieme/utils/set_utils.h"

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;

TEST(Lattice, Create) {
	
	auto max = std::numeric_limits<unsigned>::max();
	auto meet = [](const unsigned& lhs, const unsigned& rhs) { return std::max<unsigned>(lhs,rhs); };
	auto sl1 = makeLowerSemilattice(std::numeric_limits<unsigned>::max(), meet);

	EXPECT_EQ(3u, sl1.meet(2,3));
	EXPECT_EQ(max, sl1.meet(max, max));

	// Idenpotence 
	EXPECT_EQ(3u, sl1.meet(3,3));
	// Commutativity
	EXPECT_EQ(sl1.meet(3,9), sl1.meet(9,3));
	// Transitivity
	EXPECT_EQ(sl1.meet(1,sl1.meet(3,5)), sl1.meet(sl1.meet(1,3),5));

	auto min = std::numeric_limits<unsigned>::min();
	auto join =  [](const unsigned& lhs, const unsigned& rhs) { return std::min<unsigned>(lhs,rhs); };
	auto sl2 = makeUpperSemilattice(min, join);
	
	EXPECT_EQ(2u, sl2.join(2,3));
	EXPECT_EQ(min, sl2.join(min, min));

	// Idenpotence 
	EXPECT_EQ(3u, sl2.join(3,3));
	// Commutativity
	EXPECT_EQ(sl2.join(3,9), sl2.join(9,3));
	// Transitivity
	EXPECT_EQ(sl2.join(1,sl2.join(3,5)), sl2.join(sl2.join(1,3),5));

	auto lattice = makeLattice(min, max, join, meet);
	EXPECT_EQ(4u, lattice.join(4,8));
	EXPECT_EQ(8u, lattice.meet(8,1));

}


TEST(Lattice, CreateIR) {

	using insieme::utils::set::toSet;

	NodeManager mgr;
	IRBuilder builder(mgr);

	typedef insieme::utils::set::PointerSet<VariablePtr> VarSet;

	VariablePtr a = builder.variable(builder.getLangBasic().getInt4()),
			 	b = builder.variable(builder.getLangBasic().getInt4()),
				c = builder.variable(builder.getLangBasic().getInt4()),
				d = builder.variable(builder.getLangBasic().getInt4());

	VarSet varset{ a, b, c, d };
	
	auto join = [](const VarSet& lhs, const VarSet& rhs) { 
		VarSet res;
		std::set_intersection(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), std::inserter(res,res.begin()));
		return res;
	};
	auto meet = [](const VarSet& lhs, const VarSet& rhs) { 
		VarSet res;
		std::set_union(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), std::inserter(res,res.begin()));
		return res;
	};

	auto lattice = makeLattice(VarSet(), varset, join, meet);

	VarSet set1{a,c}, set2{b,d}, set3{a,d};

	EXPECT_EQ( lattice.bottom(), lattice.meet(set1, set2) );
	EXPECT_EQ( VarSet({a,c,d}), lattice.meet(set1, set3) );

	EXPECT_EQ( lattice.top(), lattice.join(set1, set2) );
	EXPECT_EQ( VarSet({a}), lattice.join(set1, set3) );
}



typedef std::pair<int,std::string> Data;

struct comp1 {
	bool operator()(const Data& lhs, const Data& rhs) { 
		return lhs.second == rhs.second ? lhs.first > rhs.first : lhs.second < rhs.second;
	}
};

bool comp2(const Data& lhs, const Data& rhs) { 
	return lhs.second < rhs.second;
}

TEST(Lattice, CreateCompound) {

	using insieme::utils::set::toSet;

	NodeManager mgr;
	IRBuilder builder(mgr);


	typedef std::set<Data,comp1> DataSet;

	auto join = [](const DataSet& lhs, const DataSet& rhs) { 
		DataSet tmp, res;
		std::set_union(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), std::inserter(tmp,tmp.begin()), comp1());
		
		DataSet::const_iterator it = tmp.begin();
		while ( it != tmp.end() ) {
			res.insert(*it);
			it = std::upper_bound(it, tmp.end(), *it, comp2);
		}
		return res;
	};

	auto lattice = makeLowerSemilattice(DataSet(), join);
	//VarSet set1{a,c}, set2{b,d}, set3{a,d};

	EXPECT_EQ( DataSet( { 
						  std::make_pair(36, "c++"), 
						  std::make_pair(30, "java") 
						}), 

			lattice.meet(
				DataSet({ 
						  std::make_pair(29, "c++"), 
						  std::make_pair(15, "c++"), 
						  std::make_pair(24, "java")
						}), 
				DataSet({ 
					      std::make_pair(36, "c++"), 
						  std::make_pair(30, "java")
						})
			) 
		);

}

