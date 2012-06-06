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
class Top { 
	Top() { } // avoid instances of Top to be creates outside the class
public:
	static Top instance; 
};
extern const Top& top;


/**
 * Defines a genetic BOTTOM element of a lattice to be used as artifical bottom element
 */
class Bottom { 
	Bottom() { } // avoid instances of Bottom to be creates outside the class
public:
	static Bottom instance; 
};
extern const Bottom& bottom;


typedef enum { TOP, BOTTOM, VALUE } ElementType;


namespace detail {

template <class T>
struct LatticeImpl {

	class Element : public boost::variant<T, Top, Bottom> {

		struct visit_prop: public boost::static_visitor<bool> {
		
			typedef enum { TOP, BOTTOM, VALUE } PropName;

			visit_prop(const PropName& prop) : prop(prop) { }

			bool operator()( const Top& ) const 	{ return prop==TOP; }
			bool operator()( const Bottom& ) const 	{ return prop==BOTTOM; }
			bool operator()( const T& ) const 	{ return prop==VALUE; }

		private:
			PropName prop;
		};

	public:

		Element(const T& val) : boost::variant<T,Top,Bottom>(val) { }
		Element(const Top& t)   : boost::variant<T,Top,Bottom>(t) { }
		Element(const Bottom& b): boost::variant<T,Top,Bottom>(b) { }

		bool isTop() const {
			return boost::apply_visitor( visit_prop(visit_prop::TOP), *this );
		}

		bool isBottom() const {
			return boost::apply_visitor( visit_prop(visit_prop::BOTTOM), *this );
		}

		bool isValue() const {
			return boost::apply_visitor( visit_prop(visit_prop::VALUE), *this );
		}

		bool operator==(const Element& other) const { 
			return (isTop() && other.isTop()) ||
				   (isBottom() && other.isBottom()) ||
				   (isValue() && other.isValue() && (value() == other.value()));
		}

		const T& value() const { return boost::get<const T&>(*this); }
	};


	/** 
	 * Define the type of the MEET (^)and JOIN (v) operations allowed for this lattice
	 */
	typedef std::function<LatticeImpl<T>::Element (const T& lhs, const T& rhs)> Operation;

	/** 
	 * Defines an empty join or meet operation which is used to build semi-lattices
	 *
	 * If the function is invoked an excpetion is thrown
	 */
	struct NoOp {
		Element operator()(const T& lhs, const T& rhs) { throw NotAValidOperator(); }
	};


	inline bool isLatticeTop(const Element& elem) const { 
		return elem == m_top; 
	}

	inline bool isLatticeBottom(const Element& elem) const { 
		return elem == m_bottom; 
	}


	LatticeImpl(const Element& top, const Element& bottom) : m_top(top), m_bottom(bottom) { }

	/**
	 * This is the Least Upper Bound of the lattice (also called TOP element)
	 */
	inline const Element& top() const { return m_top; }

	/**
	 * Thsi is the Greates Lower Bound of the lattice (also called BOTTOM element)
	 */
	inline const Element& bottom() const { return m_bottom; }

	virtual bool isSemilattice() const { 
		return isLowerSemilattice() || isUpperSemilattice();
	}

	virtual bool isLowerSemilattice() const { return false; }

	virtual bool isUpperSemilattice() const { return false; }

	virtual bool isLattice() const { return !isSemilattice(); }

private:
	Element m_top, m_bottom;

};

} // end detail namespace 


template <class T>
struct LowerSemilattice : public virtual detail::LatticeImpl<T> {

	typedef detail::LatticeImpl<T> Base;
	typedef typename detail::LatticeImpl<T>::Element Element;
	typedef typename detail::LatticeImpl<T>::Operation Operation;

	LowerSemilattice(const Element& top=top, const Element& bottom=bottom, const Operation& meet=Base::NoOp() ) :
		detail::LatticeImpl<T>(top, bottom), m_meet(meet) 
	{ }

	inline Element meet(const Element& lhs, const Element& rhs) const { 
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
		Element&& ret = m_meet(lhs.value(), rhs.value()); 
		return ret;
	}

	virtual bool isLowerSemilattice() const { return true; }

private:
	Operation m_meet;

};

template <class T>
struct UpperSemilattice : public virtual detail::LatticeImpl<T> {

	typedef detail::LatticeImpl<T> Base;
	typedef typename detail::LatticeImpl<T>::Element Element;
	typedef typename detail::LatticeImpl<T>::Operation Operation;

	UpperSemilattice(const Element& top=top, const Element& bottom=bottom, const Operation& join=Base::NoOp()) :
		detail::LatticeImpl<T>(top, bottom), m_join(join) 
	{ }

	inline Element join(const Element& lhs, const Element& rhs) const { 
		/**
		 * Top ^ X   = Top
		 * X   ^ Top = Top
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
		 * Invoke the user function for the Join operator
		 */
		Element&& ret = m_join(lhs.value(), rhs.value()); 

		return ret;
	}

	virtual bool isUpperSemilattice() const { return true; }

private:
	typename detail::LatticeImpl<T>::Operation m_join;

};

/**
 * Implementation of a generic lattice.
 */
template <class T>
struct Lattice :  public LowerSemilattice<T>, public UpperSemilattice<T> {

	typedef detail::LatticeImpl<T> Base;
	typedef typename detail::LatticeImpl<T>::Element Element;
	typedef typename detail::LatticeImpl<T>::Operation Operation;

	Lattice(const Element& top, const Element& bottom, 
			const Operation& join=Base::NoOp(), const Operation& meet=Base::NoOp()) : 
		Base(top, bottom),
		LowerSemilattice<T>(top, bottom, meet), 
		UpperSemilattice<T>(top, bottom, join) 
	{ 
	//	assert((m_top.isDefined() || m_bottom.isDefined()) && "A Semilattice must have at least one bound");
	}

	virtual bool isLattice() const { return true; }
};


template <class T>
inline typename Lattice<T>::Element elem(const T& e) { return typename Lattice<T>::Element(e); }

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
