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

#include <iostream>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/utility/enable_if.hpp>

#include "insieme/utils/pointer.h"
#include "insieme/utils/type_traits_utils.h"
#include "insieme/core/ir_node_traits.h"
#include "insieme/core/ir_node_accessor.h"


namespace insieme {
namespace core {

// Forward declaration of cast functor.
struct StaticPointerCast;
struct DynamicPointerCast;

// a simple type trait to filter IR pointer types
template<typename P> struct is_ir_pointer : public boost::false_type {};
template<typename T> struct is_ir_pointer<Pointer<T>> : public boost::true_type {};

template<typename T>
class Pointer :
	public Ptr<T>,
	public node_type<typename boost::remove_const<T>::type>::ptr_accessor_type {

	/**
	 * The accessor offered to gain convenient access to members of the referenced node
	 */
	typedef typename node_type<typename boost::remove_const<T>::type>::ptr_accessor_type accessor_type;

public:

	typedef StaticPointerCast StaticCast;
	typedef DynamicPointerCast DynamicCast;

	Pointer() : Ptr<T>(NULL) {}

	Pointer(T* ptr) : Ptr<T>(ptr) { }

	/**
	 * A conversion operator to a annotated pointer referencing a super type of the type
	 * pointed to by this instance can be efficiently realized using a reinterpret_cast. This
	 * operator is realizing this efficient conversion.
	 */
	template<typename B, typename boost::enable_if<boost::is_base_of<B,T>,int>::type = 0>
	operator const Pointer<B>() const {
		return Pointer<B>(this->ptr);
	}


	/**
	 * Obtains a accessor instance allowing to access the members of the referenced node.
	 */
	const accessor_type* operator->() const {
		return this;
	}

	/**
	 * Reinterprets this pointer to be referencing the requested element type.
	 */
	template<typename R>
	const Pointer<R>& reinterpret() const {
		return reinterpret_cast<const Pointer<R>&>(*this);
	}

	/**
	 * A short-cut for static pointer casts supporting a reduced syntax.
	 */
	template<typename R>
	typename boost::enable_if<is_ir_pointer<R>, R>::type as() const {
		return static_pointer_cast<R>(*this);
	}
	/**
	 * Returns if a class is an instance of R
	 */
	template<typename R>
	typename boost::enable_if<is_ir_pointer<R>, bool>::type isa() const {
		return dynamic_pointer_cast<R>(*this);
	}
};

template<typename B, typename T>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B,T>,boost::is_base_of<T,B>>, Pointer<B>>::type
dynamic_pointer_cast(const Pointer<T>& src) {
	return Pointer<B>((src)?dynamic_cast<B*>(&(*src)):NULL);
}

template<typename B, typename T, typename E = typename B::element_type>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<E,T>,boost::is_base_of<T,E>>, B>::type
dynamic_pointer_cast(const Pointer<T>& src) {
	return B((src)?dynamic_cast<E*>(&(*src)):NULL);
}

template<typename B, typename T>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B,T>,boost::is_base_of<T,B>>, Pointer<B>>::type
static_pointer_cast(Pointer<T>& src) {
	assert((!src || dynamic_cast<B*>(&(*src))) && "Invalid static cast!");
	return Pointer<B>(static_cast<B*>(src.ptr));
}

template<typename B, typename T>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B,T>,boost::is_base_of<T,B>>, const Pointer<B>>::type
static_pointer_cast(const Pointer<T>& src) {
	assert((!src || dynamic_cast<B*>(&(*src))) && "Invalid static cast!");
	return Pointer<B>(static_cast<B*>(src.ptr));
}

template<typename B, typename T, typename E = typename B::element_type>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<E,T>,boost::is_base_of<T,E>>, B>::type
static_pointer_cast(const Pointer<T>& src) {
	assert((!src || dynamic_cast<E*>(&(*src))) && "Invalid static cast!");
	return B(static_cast<E*>(src.ptr));
}


/**
 * A template version for a functor performing static pointer casts on annotated pointer.
 * The purpose of this struct is to allow the static_pointer_cast function to be defined as
 * a pointer conversion function required as a template parameter of the AST Visitor class.
 */
struct StaticPointerCast {
	template<typename Target, typename Source>
	const Pointer<Target> operator()(const Pointer<Source>& value) const {
		return static_pointer_cast<Target>(value);
	}
};

/**
 * A template version for a functor performing dynamic pointer casts on annotated pointer.
 * The purpose of this struct is to allow the dynamic_pointer_cast function to be defined as
 * a pointer conversion function required as a template parameter of the AST Visitor class.
 */
struct DynamicPointerCast {
	template<typename Target, typename Source>
	const Pointer<Target> operator()(const Pointer<Source>& value) const {
		return dynamic_pointer_cast<Target>(value);
	}
};

} // end namespace core
} // end namespace insieme

namespace std {

	template<typename T>
	std::ostream& operator<<(std::ostream& out, const insieme::core::Pointer<T>& ptr) {
		out << "AP(";
		if (!!ptr) {
			out << *ptr;
		} else {
			out << "NULL";
		}
		out << ")";
		return out;
	}

}

