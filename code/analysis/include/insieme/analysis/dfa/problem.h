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

#pragma once

#include "insieme/analysis/dfa/entity.h"
#include "insieme/analysis/dfa/lattice.h"

#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace analysis {
namespace dfa {


template <class... E>
struct entity_type_traits;


template <class E>
struct entity_type_traits<Entity<E>> { 

	typedef Value<typename container_type_traits<E>::type> type;

};


template <class... E>
struct entity_type_traits<Entity<E...>> { 

	typedef std::tuple< typename entity_type_traits<Entity<E>>::type... > type;

};


template <class Impl, class E>
class Problem {

protected:
	typedef typename entity_type_traits<E>::type value_type;

public:
	typedef typename container_type_traits<E>::type container_type;


	Problem(const CFG& cfg) : extracted( extract(E(), cfg) ) {  }


	virtual value_type top() const = 0;

	virtual value_type bottom() const = 0;

	virtual value_type meet(const container_type& lhs, const container_type& rhs) const = 0;

	//virtual Element<container_type> 
	//	join(const container_type& lhs, const container_type& rhs) const = 0;


	Lattice<container_type> makeLattice() const {
		return Lattice<container_type>(top(), bottom());
	}

	LowerSemilattice<container_type> makeLowerSemilattice() const {
		return LowerSemilattice<container_type>(top(), bottom(), 
				std::bind(
					std::mem_fn(&Impl::meet), 
					static_cast<const Impl&>(*this), 
					std::placeholders::_1, 
					std::placeholders::_2
				)
			);
	}

protected:
	container_type extracted;
};




/**
 * Define the DataFlowProblem for Live variables 
 */
class LiveVariables: public Problem<LiveVariables, Entity<dfa::elem<core::VariablePtr>> > {

	typedef Problem<LiveVariables,Entity<dfa::elem<core::VariablePtr>>> Base;
	
public:

	LiveVariables(const CFG& cfg): Base(cfg) { }

	virtual Value<typename Base::container_type> top() const { 
		// the top element is the set of all variable present in the program
		return extracted;
	}

	virtual Value<typename Base::container_type> bottom() const {
		// the bottom element is the empty set 
		return typename Base::container_type();
	}

	Value<typename Base::container_type> 
		meet(const typename Base::container_type& lhs, const typename Base::container_type& rhs) const 
	{
		typedef typename Base::container_type ResultType;

		ResultType ret;
		std::set_intersection(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), std::inserter(ret,ret.begin()));
		return ret;
	}

};


/**
 * Define the DataFlowProblem for Live variables 
 */
//class ConstPropagation: 
	//public Problem<ConstPropagation, Entity<dfa::elem<core::VariablePtr>,dfa::dom<dfa::Value<core::LiteralPtr>>>> 
//{

	//typedef Problem<ConstPropagation,Entity<dfa::elem<core::VariablePtr>,dfa::dom<dfa::Value<core::LiteralPtr>>>> Base;
	
//public:

	//ConstPropagation(const CFG& cfg): Base(cfg) { }

	//virtual Value<typename Base::container_type> top() const { 
		//// the top element is the set of all variable present in the program
		//return extracted;
	//}

	//virtual Value<typename Base::container_type> bottom() const {
		//// the bottom element is the empty set 
		//return typename Base::container_type();
	//}

	//Value<typename Base::container_type> 
		//meet(const typename Base::container_type& lhs, const typename Base::container_type& rhs) const 
	//{
		//return lhs;
	//}

//};

} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace 
