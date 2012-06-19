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

#include <functional>
#include <stdexcept>

#include "insieme/analysis/dfa/value.h"
#include "insieme/analysis/dfa/domain.h"

#include "insieme/utils/printable.h"

namespace insieme { 
namespace analysis { 
namespace dfa {

struct NotAValidOperator : std::logic_error { 
	NotAValidOperator() : std::logic_error("Operator not defined") { }
};

struct BoundNotDefined : std::logic_error {
	BoundNotDefined() : std::logic_error("Bound not defined") { } 
};


namespace detail {

template <class Dom>
struct LatticeImpl {

	typedef typename Dom::value_type element_type;

	/** 
	 * Define the type of the MEET (^)and JOIN (v) operations allowed for this lattice
	 */
	typedef std::function<element_type (const element_type& lhs, const element_type& rhs)> Operation;


	LatticeImpl(const Dom& domain, const element_type& top, const element_type& bottom) : 
		domainSet(domain), m_top(top), m_bottom(bottom) 
	{

		assert(dfa::contains(domain, top));
		assert(dfa::contains(domain, bottom));
		
	}

	/**
	 * This is the Least Upper Bound of the lattice (also called TOP element)
	 */
	inline const element_type& top() const { return m_top; }

	/**
	 * Thsi is the Greates Lower Bound of the lattice (also called BOTTOM element)
	 */
	inline const element_type& bottom() const { return m_bottom; }

	bool isSemilattice() const { 
		return isLowerSemilattice() || isUpperSemilattice();
	}

	virtual bool isLatticeElement(const element_type& elem) const { 
		return dfa::contains(domainSet, elem);
	}

	virtual bool isLowerSemilattice() const { return false; }

	virtual bool isUpperSemilattice() const { return false; }

	bool isLattice() const { 
		return isLowerSemilattice() && isUpperSemilattice(); 
	}

	virtual ~LatticeImpl() { }

protected:

	inline bool isLatticeTop(const element_type& elem) const { return elem == m_top; }

	inline bool isLatticeBottom(const element_type& elem) const { return elem == m_bottom; }

private:
	Dom domainSet;
	element_type m_top, m_bottom;
};



} // end detail namespace 

template <class Dom>
struct LowerSemilattice : public virtual detail::LatticeImpl<Dom> {

	typedef detail::LatticeImpl<Dom> Base;
	typedef typename Base::element_type element_type;
	typedef typename Base::Operation Operation;

	LowerSemilattice(const Dom& domain, 
					 const element_type& top, 
				     const element_type& bottom, 
					 const Operation& meet ) : 
		Base(domain, top, bottom), m_meet(meet) { }

	inline bool is_weaker_than(const element_type& lhs, const element_type& rhs) const {
		if (lhs == rhs) { return true; }
		return meet_impl(lhs, rhs) == lhs;
	}

	element_type meet(const element_type& lhs, const element_type& rhs) const {
		element_type&& ret = meet_impl(lhs, rhs);

		assert( is_weaker_than(ret, lhs) && is_weaker_than(ret, rhs)  && 
				"Error satisfying post-conditions of meet (^) operator");

		/**
		 * Check whether the returned element is still an element of this lattice
		 */	
		assert( isLatticeElement(ret) && "Error: generated element is not part of the lattice");
		return ret;
	}

	virtual bool isLatticeElement(const element_type& elem) const {
		return Base::isLatticeElement(elem) &&
			   meet_impl(elem, Base::top()) == elem && 
			   meet_impl(elem, Base::bottom()) == Base::bottom();
	}

	virtual bool isLowerSemilattice() const { return true; }

protected:

	inline element_type meet_impl(const element_type& lhs, const element_type& rhs) const {
		assert( Base::isLatticeElement(lhs) && Base::isLatticeElement(rhs) && 
				"Operands of the meet (^) operator are not part of the lattice, operation not defined");
		/**
		 * Bottom ^ Bottom = Bottom
		 * X      ^ Bottom = Bottom
		 * Bottom ^ X      = Bottom
		 */
		if (isLatticeBottom(lhs) || isLatticeBottom(rhs)) { return Base::bottom(); }
		
		/**
		 * Top ^ Top = Top
		 * Top ^ X   = X 
		 * X   ^ Top = X
		 */
		if (isLatticeTop(lhs)) { return rhs; }
		if (isLatticeTop(rhs)) { return lhs; }

		/**
		 * Invoke the user function for the MEET operator 
		 */
		return m_meet(lhs, rhs); 
	}

private:
	Operation m_meet;

};

template <class Dom>
struct UpperSemilattice : public virtual detail::LatticeImpl<Dom> {

	typedef detail::LatticeImpl<Dom> Base;
	typedef typename Base::element_type element_type;
	typedef typename Base::Operation Operation;

	UpperSemilattice(const Dom& domain,
					 const element_type& top, 
					 const element_type& bottom, 
					 const Operation& join) : 
		Base(domain, top, bottom), m_join(join) { }

	inline bool is_stronger_than(const element_type& lhs, const element_type& rhs) const {
		if (lhs == rhs) { return true; }
		return join_impl(lhs, rhs) == lhs;
	}

	element_type join(const element_type& lhs, const element_type& rhs) const {
		element_type&& ret = join_impl(lhs, rhs);
		
		assert( is_stronger_than(ret, lhs) && is_stronger_than(ret, rhs)  &&
				"Error satisfying post-conditions of join (v) operator");

		/**
		 * Check whether the returned element is still an element of this lattice
		 */	
		assert( isLatticeElement(ret) && "Error: generated element is not part of the lattice");

		return ret;
	}

	virtual bool isLatticeElement(const element_type& elem) const {
		return Base::isLatticeElement(elem) &&
			   join_impl(elem, Base::top()) == Base::top() && 
			   join_impl(elem, Base::bottom()) == elem;
	}

	virtual bool isUpperSemilattice() const { return true; }

protected:

	inline element_type join_impl(const element_type& lhs, const element_type& rhs) const { 
		assert( Base::isLatticeElement(lhs) && Base::isLatticeElement(rhs) && 
				"Operands of the join (v) operator are not part of the lattice, operation not defined");
		/**
		 * Top v X   = Top
		 * X   v Top = Top
		 * Top v Top = Top
		 */
		if (isLatticeTop(lhs) || isLatticeTop(rhs)) { return Base::top(); }

		/**
		 * Bottom v Bottom = Bottom
		 * Bottom v X      = X
		 * X      v Bottom = X
		 */
		if (isLatticeBottom(lhs)) { return rhs; }
		if (isLatticeBottom(rhs)) { return lhs; }

		/**
		 * Invoke the user function for the Join 		
		 */
		return m_join(lhs, rhs); 
	}

private:
	Operation m_join;
};

/**
 * Implementation of a lattice.
 *
 * By definition, a lattice is simulataneously an upper semilattatice and a lowe semilattice. 
 * Therefore it is defined by both the Join (v) and Meet (^) operations. 
 *
 */
template <class Dom>
struct Lattice :  public LowerSemilattice<Dom>, public UpperSemilattice<Dom> {

	typedef detail::LatticeImpl<Dom> Base;
	typedef typename Base::element_type element_type;
	typedef typename Base::Operation Operation;

	Lattice(const Dom& domain,
			const element_type& top, 
			const element_type& bottom, 
			const Operation& join, 
			const Operation& meet) : 
		Base(domain, top, bottom),
		LowerSemilattice<Dom>(domain, top, bottom, meet), 
		UpperSemilattice<Dom>(domain, top, bottom, join) 
	{ }

	virtual bool isLatticeElement(const element_type& elem) const {
		return Base::isLatticeElement(elem) &&
			   join_impl(elem, Base::top()) == Base::top() && 
			   meet_impl(elem, Base::bottom()) == Base::bottom();
	}

};

template <class Dom>
LowerSemilattice<Dom> makeLowerSemilattice(
		const Dom& dom, 
		const typename Dom::value_type& top, 
		const typename Dom::value_type& bottom, 
		const typename Lattice<Dom>::Operation& meet)
{
	return LowerSemilattice<Dom>(dom, top, bottom, meet);
}

template <class Dom>
UpperSemilattice<Dom> makeUpperSemilattice(
		const Dom& dom, 
		const typename Dom::value_type& top, 
		const typename Dom::value_type& bottom, 
		const typename Lattice<Dom>::Operation& join) {
	return UpperSemilattice<Dom>(dom, top, bottom, join);
}

template <class Dom>
Lattice<Dom> makeLattice(
		const Dom& dom,
		const typename Dom::value_type& top, 
		const typename Dom::value_type& bottom, 
		const typename Lattice<Dom>::Operation& join, 
		const typename Lattice<Dom>::Operation& meet) 
{
	return Lattice<Dom>(dom, top, bottom, join, meet);
}


} // end dfa namespace 
} // end analysis namespace
} // end insieme namepace 



