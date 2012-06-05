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

#include <stdexcept>

namespace insieme { 
namespace analysis { 
namespace dfa {

struct NotAValidOperator : std::logic_error { 
	NotAValidOperator() : std::logic_error("Operator not defined") { }
};

struct BoundNotDefined : std::logic_error {
	BoundNotDefined() : std::logic_error("Bound not defined") { } 
};

namespace {

/**
 * Define an empty operator in the case the user wants to define a semilattice. 
 * Dependning on the fact the semilattice is an upper or lower semilattice, respectively the join
 * and meet operator are set to NoOp. 
 *
 * Trying to invoke such operators will generate an exception.
 */
template <class T>
struct NoOp {
	T operator()(const T& lhs, const T& rhs) const { throw NotAValidOperator();	}
};

/**
 * Define bounds element of the Lattice. The second element of the pair states
 * whether this bound is defined or not (used in the case of semilattices)
 */
template <class T>
class Bound : private std::pair<T, bool> {

public:
	Bound() : std::pair<T,bool>(T(),false) { }
	Bound(const T& value) : std::pair<T,bool>(value,true) { }

	bool isDefined() const { return this->second; }

	operator T() const {  return value();  }

	const T& value() const { 
		if (!isDefined()) {
			// this guard avoids to access to an undefined bound
			throw BoundNotDefined();
		}
		return this->first; 
	}
};

} // end anonymous namespace 


/**
 * Implementation of a generic lattice.
 */
template <class Dom, class Join, class Meet>
struct Lattice {

	Lattice(const Bound<Dom>& top, const Bound<Dom>& bottom, const Join& join, const Meet& meet) : 
		m_top(top), m_bottom(bottom), m_join(join), m_meet(meet) 
	{ 
		assert((m_top.isDefined() || m_bottom.isDefined()) && "A Semilattice must have at least one bound");
	}

	inline Dom join(const Dom& lhs, const Dom& rhs) const { 
		return m_join(lhs, rhs); 
	}

	inline Dom meet(const Dom& lhs, const Dom& rhs) const { 
		return m_meet(lhs, rhs); 
	}

	inline const Dom& top() const { return m_top.value(); }

	inline const Dom& bottom() const { return m_bottom.value(); }

	/** 
	 * Test weather this is a semilattice
	 */
	inline bool isSemilattice() const { 
		return !m_top.isDefined() || !m_bottom.isDefined(); 
	}

	inline bool isLowerSemilattice() const {
		return !m_top.isDefined();
	}

	inline bool isUpperSemilattice() const {
		return !m_bottom.isDefined();
	}

	/**
	 * Test weather this is a complete lattice with lub and glb elements 
	 */
	inline bool isLattice() const { 
		return m_top.isDefined() && m_bottom.isDefined();
	}

private:

	Bound<Dom> 	m_top, m_bottom;
	Join 		m_join;
	Meet 		m_meet;
};

template <class Dom, class Meet>
Lattice<Dom, NoOp<Dom>, Meet> makeLowerSemilattice(const Dom& glb, const Meet& meet) {
	return Lattice<Dom, NoOp<Dom>, Meet>(Bound<Dom>(), 	  // lub
										 Bound<Dom>(glb), // glb
										 NoOp<Dom>(), 	  // join
										 meet);			  // meet
}

template <class Dom, class Join>
Lattice<Dom, Join, NoOp<Dom>> makeUpperSemilattice(const Dom& lub, const Join& join) {
	return Lattice<Dom, Join, NoOp<Dom>>(Bound<Dom>(lub), // lub
										 Bound<Dom>(), 	  // glb
										 join, 			  // join
										 NoOp<Dom>());	  // meet
}

template <class Dom, class Join, class Meet>
Lattice<Dom, Join, Meet> makeLattice(const Dom& lub, const Dom& glb, const Join& join, const Meet& meet) {
	return Lattice<Dom, Join, Meet>(Bound<Dom>(lub), // lub
									Bound<Dom>(glb), // glb
									join, 			  // join
									meet);			  // meet
}


} // end dfa namespace 
} // end analysis namespace
} // end insieme namepace 
