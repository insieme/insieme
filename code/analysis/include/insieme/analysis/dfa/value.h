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

#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace dfa {

/**
 * Defines a generic TOP element of a lattice to be used as artificial value 
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
 * Defines a genetic BOTTOM element of a lattice to be used as artificial bottom value
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

/**
 * The Value class represents an element of a lattice. 
 *
 * By definition, an element of a Lattice (or semi-lattice) is any element of the Domain defined
 * by the type T, or the two ``special'' elements TOP and BOTTOM of the lattice. 
 *
 * We internally represent an element as a variant type which can be either:
 * 	T, Top or Bottom
 */
template <class T>
class Value : public utils::Printable {

	boost::variant<T, Top, Bottom> m_value;

	/**
	 * Boost::Variant visitor for determining whether this Value is a value type 
	 * or the element TOP or BOTTOM
	 */
	struct visit_prop: public boost::static_visitor<bool> {
	
		typedef enum { TOP, BOTTOM, VALUE } PropName;

		visit_prop(const PropName& prop) : prop(prop) { }

		bool operator()( const Top& ) const    { return prop==TOP; }
		bool operator()( const Bottom& ) const { return prop==BOTTOM; }
		bool operator()( const T& ) const 	   { return prop==VALUE; }

	private:
		PropName prop;
	};

	struct print_visitor: public boost::static_visitor<> {
	
		print_visitor(std::ostream& out) : out(out) { }

		template <class R>
		void operator()( const R& t ) const { out << t; }

	private:
		std::ostream& out ;
	};
public:

	Value(const T& val) : m_value(val) { }
	Value(const Top& t) : m_value(t) { }
	Value(const Bottom& b): m_value(b) { }

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
	bool operator==(const Value& other) const { 
		return (isTop() && other.isTop()) ||
			   (isBottom() && other.isBottom()) ||
			   (isValue() && other.isValue() && (value() == other.value()));
	}

	const T& value() const { return boost::get<const T&>(m_value); }

	std::ostream& printTo(std::ostream& out) const { 
		boost::apply_visitor( print_visitor(out), m_value ); 
		return out;
	}

	bool operator<(const Value<T>& other) const {

		if ( (isBottom() && other.isBottom()) || (!isBottom() && other.isBottom())) { return false; }

		if ( (isTop() && other.isTop()) || (isTop() && !other.isTop())) { 
			return false; 
		}

		return ( isBottom() || other.isTop() ) || value() < other.value();
	}
};

/**
 * Equality operator where the lhs is either a TOP or BOTTOM symbol 
 */
template <class T>
inline bool operator==(const Top& t, const Value<T>& elem) {
	return elem.isTop();
}

template <class T>
inline bool operator==(const Bottom& b, const Value<T>& elem) {
	return elem.isBottom();
}

template <class T>
inline Value<T> value(const T& e) { return Value<T>(e); }


} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace 

namespace std {

	/** 
	 * Define an hashing function for Values, the hashing is done by forwawding the call
	 * to the hashing function of the contained object. In the case of bottom/top symbols 
	 * their hashing is obtained by their address, since they are singletons.
	 */
	template <typename T>
	struct hash<insieme::analysis::dfa::Value<T>> {
		
		size_t operator()(const insieme::analysis::dfa::Value<T>& val) const {
			if (val.isTop()) 	{ return reinterpret_cast<size_t>(&insieme::analysis::dfa::top); }
			if (val.isBottom()) { return reinterpret_cast<size_t>(&insieme::analysis::dfa::bottom); }
			return std::hash<T>()(val.value());
		}

	};

} // end std namespace 

