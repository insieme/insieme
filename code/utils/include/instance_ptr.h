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

#include <cassert>

#include <boost/type_traits/is_base_of.hpp>
#include <boost/type_traits/is_convertible.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/mpl/logical.hpp>


template<typename T>
class InstancePtr {
public:

	/**
	 * Defines a type corresponding to the type this pointer is pointing to.
	 */
	typedef T element_type;

	T* ptr;

	InstancePtr(T* ptr) : ptr(ptr) { }

	template<typename B>
	InstancePtr(const InstancePtr<B>& from, typename boost::enable_if<boost::is_base_of<T,B>,int>::type = 0) : ptr(from.ptr) { }
	
	virtual ~InstancePtr() {};

	operator bool() const {
		return ptr!=NULL;
	}

	bool operator!() const {
		return ptr==NULL;
	}

	T& operator*() const {
		assert(ptr != NULL && "Illegal: dereferencing a NULL pointer!" );
		return *ptr;
	}

	T* operator->() const {
		return ptr;
	}

	/**
	 * Equality is implemented using two different, generic version of the == operator. The first
	 * is considered whenever the two referenced types are pointing to convertible types, the second
	 * version is used if the two pointers are not related.
	 *
	 * This version is used if the given pointers can be converted into each other.
	 *
	 * NOTE: if the pointer are pointing toward the same location (yet are of of non-related types)
     * 		 the result will still be false!!
     *
	 * @tparam A the type the given pointer is pointing to
	 * @param other the other pointer to be compared to
	 * @return true if both point to the same location (regardless of the actual type)
	 */
	template<typename A>
	const typename boost::enable_if< boost::mpl::or_<boost::is_convertible<T*, A*>,boost::is_convertible<A*, T*>> , bool >::type operator==(const InstancePtr<A>& other) const {
		return ptr == other.ptr;
	}

	/**
	 * Equality is implemented using two different, generic version of the == operator. The first
	 * is considered whenever the two referenced types are pointing to convertible types, the second
	 * version is used if the two pointers are not related.
	 *
	 * This version is used if the given pointers are not related.
	 *
	 * NOTE: if the pointer are pointing toward the same location (yet are of of non-related types)
	 * 		 the result will still be false!!
	 *
	 * @tparam A the type the given pointer is pointing to
	 * @param other the other pointer to be compared to
	 * @return true if both point to the same location (regardless of the actual type)
	 */
	template<typename A>
	const typename boost::disable_if<boost::mpl::or_<boost::is_convertible<T*, A*>,boost::is_convertible<A*, T*>>, bool>::type operator==(const InstancePtr<A>& other) const {
		return ptr == NULL && other.ptr == NULL;
	}
	
	template<typename A>
	bool operator!=(const InstancePtr<A>& other) const {
		return !(*this == other);
	}

};

/**
 * Allows to dynamically down-cast between instance pointer of related types.
 *
 * @tparam B the type the resulting pointer should point to
 * @tparam T the type the given pointer is pointing to
 * @param src the pointer to be down-casted
 * @return the down-casted pointer pointing to the same location
 */
template<typename B, typename T>
typename boost::enable_if<boost::is_base_of<T,B>, InstancePtr<B>>::type dynamic_pointer_cast(InstancePtr<T>& src) {
	if (dynamic_cast<B*>(&(*src))) {
		return *(reinterpret_cast<InstancePtr<B>* >(&src));
	}
	return NULL;
}

template<typename T>
std::ostream& operator<<(std::ostream& out, const InstancePtr<T>& ptr) {
	out << "IP(";
	if (!!ptr) {
		out << *ptr;
	} else {
		out << "NULL";
	}
	out << ")";
	return out;
}


