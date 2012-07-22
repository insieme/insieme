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

TEST(Lattice, CreateLowerSemilattice) {
	
	DomainSet<unsigned> intDom;

	auto min = std::numeric_limits<unsigned>::min();
	auto meet =  [](const unsigned& lhs, const unsigned& rhs) -> unsigned { 
		return std::min<unsigned>(lhs,rhs); 
	};

	auto l = makeLowerSemilattice(intDom, std::numeric_limits<unsigned>::max(), min, meet);
	
	EXPECT_EQ(2u, l.meet(2u,3u));
	EXPECT_EQ(min,l.meet(min,min));
	EXPECT_EQ(l.bottom(), l.meet(min,min));

	EXPECT_TRUE(l.isLowerSemilattice());
	EXPECT_FALSE(l.isUpperSemilattice());
	EXPECT_FALSE(l.isLattice());

	// Idenpotence 
	EXPECT_EQ(value(3u), l.meet(3,3));
	// Commutativity
	EXPECT_EQ(l.meet(3,9), l.meet(9,3));
	// Transitivity
	EXPECT_EQ(l.meet(1,l.meet(3,5)), l.meet(l.meet(1,3),5));

}

TEST(Lattice, CreateUpperSemilattice) {

	DomainSet<unsigned> intDom;

	auto max = std::numeric_limits<unsigned>::max();
	auto join = [](const unsigned& lhs, const unsigned& rhs) -> unsigned { 
		return std::max<unsigned>(lhs,rhs); 
	};

	auto l = makeUpperSemilattice(intDom, max, std::numeric_limits<unsigned>::min(), join);
	
	EXPECT_EQ(3u, 	   l.join(2,3));
	EXPECT_EQ(max, 	   l.join(max, max));
	EXPECT_EQ(l.top(), l.join(max, max));

	// The Bottom is the BOTTOM element
	// EXPECT_TRUE(l.bottom().isBottom());

	EXPECT_FALSE(l.isLowerSemilattice());
	EXPECT_TRUE(l.isUpperSemilattice());
	EXPECT_FALSE(l.isLattice());

	// Idenpotence 
	EXPECT_EQ(3u, l.join(3,3));
	// Commutativity
	EXPECT_EQ(l.join(3,9), l.join(9,3));
	// Transitivity
	EXPECT_EQ(l.join(1, l.join(3,5)), l.join(l.join(1,3),5));

}

TEST(Lattice, CreateLattice) {

	DomainSet<dfa::Value<unsigned>> boundIntSet;

	//auto max = std::numeric_limits<unsigned>::max();
	//auto min = std::numeric_limits<unsigned>::min();

	auto join = [](const dfa::Value<unsigned>& lhs, const dfa::Value<unsigned>& rhs) -> dfa::Value<unsigned> { 
		return std::max<unsigned>(lhs.value(),rhs.value()); 
	};

	auto meet =  [](const dfa::Value<unsigned>& lhs, const dfa::Value<unsigned>& rhs) -> dfa::Value<unsigned> { 
		return std::min<unsigned>(lhs.value(),rhs.value()); 
	};


	auto l = makeLattice(boundIntSet, dfa::top, dfa::bottom, join, meet);
	EXPECT_EQ(value(4u), l.meet(4,8));
	EXPECT_EQ(value(8u), l.join(8,1));
	
	EXPECT_EQ(dfa::top, l.meet(top,top));
	EXPECT_EQ(dfa::bottom, l.meet(top,bottom));

	EXPECT_TRUE(l.isLowerSemilattice());
	EXPECT_TRUE(l.isUpperSemilattice());
	EXPECT_TRUE(l.isLattice());
}


TEST(Lattice, CreateIR) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	typedef std::set<VariablePtr> VarSet;

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

	auto lattice = makeLattice(makePowerSet(varset), VarSet(), varset, join, meet);

	VarSet set1{a,c}, set2{b,d}, set3{a,d};

	EXPECT_EQ( lattice.bottom(), lattice.meet(set1, set2) );
	EXPECT_EQ( value(VarSet({a,c,d})), lattice.meet(set1, set3) );

	EXPECT_EQ( lattice.top(), lattice.join(set1, set2) );
	EXPECT_EQ( value(VarSet({a})), lattice.join(set1, set3) );
}


namespace {

typedef std::tuple<int,std::string> Data;

struct comp1 {
	bool operator()(const Data& lhs, const Data& rhs) const { 
		return std::get<1>(lhs) == std::get<1>(rhs) ? 
					std::get<0>(lhs) > std::get<0>(rhs) : 
					std::get<1>(lhs) < std::get<1>(rhs);
	}
};

bool comp2(const Data& lhs, const Data& rhs) {  return std::get<1>(lhs) < std::get<1>(rhs); }

} // end anonymous namespace 

TEST(Lattice, CreateCompound) {

	using std::make_tuple;

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

	DataSet ds { std::make_tuple(36,"c++"), std::make_tuple(30,"java"), make_tuple(15, "c++"),
				 make_tuple(29, "c++"), make_tuple(24, "java") };

	// std::cout << elem( DataSet( { make_pair(36, "c++"), make_pair(30, "java") }) ) << std::endl;

	auto lattice = makeLowerSemilattice(makePowerSet(ds), ds, DataSet(), join);

	//VarSet set1{a,c}, set2{b,d}, set3{a,d};

	EXPECT_EQ( DataSet( { make_tuple(36, "c++"), make_tuple(30, "java") }), 
			lattice.meet(
				DataSet({ make_tuple(29, "c++"), make_tuple(15, "c++"), make_tuple(24, "java") }), 
				DataSet({ make_tuple(36, "c++"), make_tuple(30, "java") })
			) 
	);

	// EXPECT_EQ( lattice.top(), top );
	//EXPECT_EQ( top, lattice.top() );
	// EXPECT_EQ( DataSet(), lattice.bottom() );

}

namespace {


/** 
 * Given 2 dataflow values associated to a variable, returns the new dataflow value 
 * after the meet operator is applied according to the following table:
 *
 * ---------------------
 * TOP ^ x      = x 
 * TOP ^ TOP    = TOP
 * x   ^ y      = BOTTOM
 * x   ^ BOTTOM = BOTTOM
 * x   ^ x      = x 
 *----------------------
 */
dfa::Value<int> evaluate(const dfa::Value<int>& lhs, const dfa::Value<int>& rhs) {

	if (lhs == dfa::top) { return rhs; }
	if (rhs == dfa::top) { return lhs; }

	if (lhs == dfa::bottom || rhs == dfa::bottom) { 
		return dfa::bottom; 
	}

	if (lhs == rhs ) { return lhs; }

	return dfa::bottom;
}

} // end anonymous namespace 


namespace std {

template <>
struct hash<std::tuple<insieme::core::VariablePtr, dfa::Value<int>> > {
	size_t operator()(const std::tuple<insieme::core::VariablePtr, dfa::Value<int>>& v) const {
		const dfa::Value<int>& c = std::get<1>(v);
		if ( c.isTop() ) { 
			return std::get<0>(v)->getId() ^ std::numeric_limits<unsigned>::max();
		}

		if ( c.isBottom() ) {
			return std::get<0>(v)->getId() ^ std::numeric_limits<unsigned>::min();
		}

		return std::get<0>(v)->getId() ^ c.value();
	}
};

} // end std namespace


#define MT(a,b) std::make_tuple(a,b)

TEST(Lattice, CreateConstantPropagationLattice) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	typedef insieme::utils::set::PointerSet<VariablePtr> VarSet;

	VariablePtr a = builder.variable(builder.getLangBasic().getInt4(), 1),
				b = builder.variable(builder.getLangBasic().getInt4()),
				c = builder.variable(builder.getLangBasic().getInt4()),
				d = builder.variable(builder.getLangBasic().getInt4());

	VarSet varset{ a, b, c };
	/**
	 * Set of integer values including top and bottom element 
	 * where the semantics of TOP => "undefined", and BOTTOM => "not a constant"
	 */
	DomainSet<dfa::Value<int>> values; // { dfa::bottom, 0, 1, 2, dfa::top };

	typedef std::tuple<VariablePtr,dfa::Value<int>> tuple_element;
	typedef std::set<tuple_element> element_type;
	
	// Needs to be simplified 
	auto meet = [](const element_type& lhs, const element_type& rhs) { 

		std::cout << "Meet (" << lhs << ", " << rhs << ") -> " << std::flush;
		
		element_type ret;
		element_type::const_iterator lhs_it = lhs.begin(), rhs_it = rhs.begin(), it, end;

		while(lhs_it != lhs.end() && rhs_it != rhs.end()) {
			if(std::get<0>(*lhs_it) == std::get<0>(*rhs_it)) {
				ret.insert( std::make_tuple(std::get<0>(*lhs_it), 
							evaluate(std::get<1>(*lhs_it), std::get<1>(*rhs_it))) 
						  );
				++lhs_it; ++rhs_it;
				continue;
			}
			if (*lhs_it < *rhs_it) { ret.insert( *(lhs_it++) ); continue; }
			if (*lhs_it > *rhs_it) { ret.insert( *(rhs_it++) ); continue; }
		}

		// Take care of the remaining elements which have to be written back to the result 
		std::tie(it,end) = lhs_it == lhs.end() ? 
							 std::make_tuple(rhs_it, rhs.end()) : 
							 std::make_tuple(lhs_it, lhs.end());

		while( it != end) { ret.insert( *(it++) ); }
		std::cout << ret << std::endl;

		return ret;
	};

	/**
	 * Define the TOP element of the lattice
	 */
	element_type lattice_top { 
		std::make_tuple(a, dfa::top), 
		std::make_tuple(b, dfa::top), 
		std::make_tuple(c, dfa::top) 
	};

	/** 
	 * Build the lattice
	 */
	auto lattice = makeLowerSemilattice(
			makePowerSet( makeCartProdSet(varset, values) ), 
			lattice_top, element_type({}), meet);

	EXPECT_EQ( element_type({ MT(a, 2) }), 
			lattice.meet( 
				element_type({ MT(a, 2) }), 
				element_type({ MT(a, dfa::top) }) 
			)
		);

	EXPECT_EQ( element_type({ MT(a, dfa::bottom) }), 
			lattice.meet( 
				element_type({ MT(a, 2) }), 
				element_type({ MT(a, 1) }) 
			)
		);

	EXPECT_EQ( element_type({ MT(a, 2) }), 
			lattice.meet( 
				element_type({ MT(a, 2) }), 
				element_type({ MT(a, 2) }) 
			)
		);

	EXPECT_EQ( element_type({ MT(a, dfa::bottom) }), 
			lattice.meet( 
				element_type({ MT(a, dfa::bottom) }), 
				element_type({ MT(a, 2) }) 
			)
		);

	EXPECT_EQ( element_type({ MT(a, dfa::bottom), MT(b, 2) }), 
			lattice.meet( 
				element_type({ MT(a, dfa::bottom) }), 
				element_type({ MT(b, 2) }) 
			)
		);

	EXPECT_EQ( element_type( { MT(a, dfa::bottom), MT(b, 2), MT(c, 0) }), 
			lattice.meet( 
				element_type({ MT(b, 2), MT(c, 0) }), 
				element_type({ MT(a, dfa::bottom), MT(b, 2), MT(c, dfa::top) }) 
			)
		);

	VariablePtr v1 = IRBuilder(mgr).variable(mgr.getLangBasic().getInt4(), 1);

	EXPECT_EQ( element_type({ MT(v1, 2) }), 
			lattice.meet( 
				element_type({ MT(a, 2) }), 
				element_type({ MT(v1, dfa::top) }) 
			)
		);

}
