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

#include <boost/variant.hpp>

#include <functional>
#include <stdexcept>

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

/**
 * Defines a generic TOP element of a lattice to be used as artificial element
 */
class Top : public utils::Printable { 
	Top() { } // avoid instances of Top to be creates outside the class
public:

	/**
	 * Prints the UTF symbol for the TOP (see http://www.unicode.org/charts/PDF/U2300.pdf)
	 */
	std::ostream& printTo(std::ostream& out) const { return out << "\u23C9"; }

	static Top instance; 
};

/**
 * Defines the only allowed instance of the Top class
 */
extern const Top& top;


/**
 * Defines a genetic BOTTOM element of a lattice to be used as artifical bottom element
 */
class Bottom : public utils::Printable { 
	Bottom() { } // avoid instances of Bottom to be creates outside the class
public:

	/** 
	 * Prints the UTF symbol for the BOTTOM (see http://www.unicode.org/charts/PDF/U2300.pdf)
	 */
	std::ostream& printTo(std::ostream& out) const { return out << "\u23CA"; }

	static Bottom instance; 
};

/**
 * Defines the only allowed instance of the Bottom class
 */
extern const Bottom& bottom;


namespace detail {

template <class T>
struct LatticeImpl {

	/**
	 * The Element class represents an element of a lattice. 
	 *
	 * By definition, an element of a Lattice (or semi-lattice) is any element of the Domain defined
	 * by the type T, or the two ``special'' elements TOP and BOTTOM of the lattice. 
	 *
	 * We internally represent an element as a variant type which can be either:
	 * 	T, Top or Bottom
	 */
	class Element : public utils::Printable {

		boost::variant<T, Top, Bottom> m_value;

		/**
		 * Boost::Variant visitor for determining wheter this Element is a value type 
		 * or the element TOP or BOTTOM
		 */
		struct visit_prop: public boost::static_visitor<bool> {
		
			typedef enum { TOP, BOTTOM, VALUE } PropName;

			visit_prop(const PropName& prop) : prop(prop) { }

			bool operator()( const Top& ) const 	{ return prop==TOP; }
			bool operator()( const Bottom& ) const 	{ return prop==BOTTOM; }
			bool operator()( const T& ) const 		{ return prop==VALUE; }

		private:
			PropName prop;
		};

		struct print_visitor: public boost::static_visitor<> {
		
			print_visitor(std::ostream& out) : out(out) { }
	
			template <class R>
			void operator()( const R& t ) const    { out << t; }

		private:
			std::ostream& out ;
		};
	public:

		Element(const T& val) : m_value(val) { }
		Element(const Top& t) : m_value(t) { }
		Element(const Bottom& b): m_value(b) { }

		bool isTop() const {
			return boost::apply_visitor( visit_prop(visit_prop::TOP), m_value );
		}

		bool isBottom() const {
			return boost::apply_visitor( visit_prop(visit_prop::BOTTOM), m_value );
		}

		bool isValue() const {
			return boost::apply_visitor( visit_prop(visit_prop::VALUE), m_value );
		}

		/**
		 * Implements the equality operator which says whether 2 Elements are the same
		 */
		bool operator==(const Element& other) const { 
			return (isTop() && other.isTop()) ||
				   (isBottom() && other.isBottom()) ||
				   (isValue() && other.isValue() && (value() == other.value()));
		}

		const T& value() const { return boost::get<const T&>(m_value); }

		std::ostream& printTo(std::ostream& out) const { 
			boost::apply_visitor( print_visitor(out), m_value );
			return out;
		}
	};

	/** 
	 * Define the type of the MEET (^)and JOIN (v) operations allowed for this lattice
	 */
	typedef std::function<LatticeImpl<T>::Element (const T& lhs, const T& rhs)> Operation;


	LatticeImpl(const Element& top, const Element& bottom) : m_top(top), m_bottom(bottom) { }

	/**
	 * This is the Least Upper Bound of the lattice (also called TOP element)
	 */
	inline const Element& top() const { return m_top; }

	/**
	 * Thsi is the Greates Lower Bound of the lattice (also called BOTTOM element)
	 */
	inline const Element& bottom() const { return m_bottom; }

	bool isSemilattice() const { 
		return isLowerSemilattice() || isUpperSemilattice();
	}

	virtual bool isLatticeElement(const Element& elem) const = 0;

	virtual bool isLowerSemilattice() const { return false; }

	virtual bool isUpperSemilattice() const { return false; }

	bool isLattice() const { 
		return isLowerSemilattice() && isUpperSemilattice(); 
	}

	virtual ~LatticeImpl() { }

protected:

	inline bool isLatticeTop(const Element& elem) const { return elem == m_top; }

	inline bool isLatticeBottom(const Element& elem) const { return elem == m_bottom; }

private:
	Element m_top, m_bottom;

};

template <class T>
inline bool operator==(const Top& t, const typename detail::LatticeImpl<T>::Element& elem) {
	return elem.isTop();
}

template <class T>
inline bool operator==(const Bottom& b, const typename LatticeImpl<T>::Element& elem) {
	return elem.isBottom();
}

} // end detail namespace 

template <class T>
struct LowerSemilattice : public virtual detail::LatticeImpl<T> {

	typedef detail::LatticeImpl<T> Base;
	typedef typename Base::Element Element;
	typedef typename Base::Operation Operation;

	LowerSemilattice(const Element& top, 
				     const Element& bottom, 
					 const Operation& meet ) : Base(top, bottom), m_meet(meet) { }


	inline bool is_weaker_than(const Element& lhs, const Element& rhs) const {
		if (lhs == rhs) { return true; }
		return meet_impl(lhs, rhs) == lhs;
	}

	Element meet(const Element& lhs, const Element& rhs) const {
		Element&& ret = meet_impl(lhs, rhs);

		assert( is_weaker_than(ret, lhs) && is_weaker_than(ret, rhs)  && 
				"Error satisfying post-conditions of meet (^) operator");

		/**
		 * Check whether the returned element is still an element of this lattice
		 */	
		assert( isLatticeElement(ret) && "Error: generated element is not part of the lattice");
		return ret;
	}

	virtual bool isLatticeElement(const Element& elem) const {
		return meet_impl(elem, Base::top()) == elem && 
			   meet_impl(elem, Base::bottom()) == Base::bottom();
	}

	virtual bool isLowerSemilattice() const { return true; }

protected:

	inline Element meet_impl(const Element& lhs, const Element& rhs) const {
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


		assert(!rhs.isTop() && !rhs.isBottom() && 
			   !lhs.isTop() && !rhs.isBottom() && "Elements are not part of the lattice");

		/**
		 * Invoke the user function for the MEET operator 
		 */
		return m_meet(lhs.value(), rhs.value()); 
	}

private:
	Operation m_meet;

};

template <class T>
struct UpperSemilattice : public virtual detail::LatticeImpl<T> {

	typedef detail::LatticeImpl<T> Base;
	typedef typename Base::Element Element;
	typedef typename Base::Operation Operation;

	UpperSemilattice(const Element& top, 
					 const Element& bottom, 
					 const Operation& join) : Base(top, bottom), m_join(join) { }

	inline bool is_stronger_than(const Element& lhs, const Element& rhs) const {
		if (lhs == rhs) { return true; }
		return join_impl(lhs, rhs) == lhs;
	}

	Element join(const Element& lhs, const Element& rhs) const {
		Element&& ret = join_impl(lhs, rhs);
		
		assert( is_stronger_than(ret, lhs) && is_stronger_than(ret, rhs)  &&
				"Error satisfying post-conditions of join (v) operator");

		/**
		 * Check whether the returned element is still an element of this lattice
		 */	
		assert( isLatticeElement(ret) && "Error: generated element is not part of the lattice");

		return ret;
	}

	virtual bool isLatticeElement(const Element& elem) const {
		return join_impl(elem, Base::top()) == Base::top() && join_impl(elem, Base::bottom()) == elem;
	}

	virtual bool isUpperSemilattice() const { return true; }

protected:

	inline Element join_impl(const Element& lhs, const Element& rhs) const { 
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

		assert(!rhs.isTop() && !rhs.isBottom() && 
			   !lhs.isTop() && !rhs.isBottom() && "Elements are not part of the lattice");

		/**
		 * Invoke the user function for the Join 		
		 */
		return m_join(lhs.value(), rhs.value()); 
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
template <class T>
struct Lattice :  public LowerSemilattice<T>, public UpperSemilattice<T> {

	typedef detail::LatticeImpl<T> Base;
	typedef typename Base::Element Element;
	typedef typename Base::Operation Operation;

	Lattice(const Element& top, 
			const Element& bottom, 
			const Operation& join, 
			const Operation& meet) : 
		Base(top, bottom),
		LowerSemilattice<T>(top, bottom, meet), 
		UpperSemilattice<T>(top, bottom, join) 
	{ }

	virtual bool isLatticeElement(const Element& elem) const {
		return join_impl(elem, Base::top()) == Base::top() && 
			   meet_impl(elem, Base::bottom()) == Base::bottom();
	}

};


template <class T>
inline typename Lattice<T>::Element elem(const T& e) { 
	return typename Lattice<T>::Element(e); 
}

template <class Dom>
LowerSemilattice<Dom> makeLowerSemilattice(const Dom& bottom, const typename Lattice<Dom>::Operation& meet) {
	return LowerSemilattice<Dom>(top,bottom,meet);
}

template <class Dom>
UpperSemilattice<Dom> makeUpperSemilattice(const Dom& top, const typename Lattice<Dom>::Operation& join) {
	return UpperSemilattice<Dom>(top,bottom,join);
}

template <class Dom>
Lattice<Dom> makeLattice(const Dom& top, const Dom& bottom, 
						 const typename Lattice<Dom>::Operation& join, 
						 const typename Lattice<Dom>::Operation& meet) 
{
	return Lattice<Dom>(top, bottom, join, meet);
}


} // end dfa namespace 
} // end analysis namespace
} // end insieme namepace 



