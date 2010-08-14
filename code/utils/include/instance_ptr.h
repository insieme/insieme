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
#include <boost/utility/enable_if.hpp>

#include <boost/mpl/logical.hpp>


using boost::mpl::and_;
using boost::mpl::or_;
using boost::mpl::not_;
using boost::is_base_of;
using boost::is_same;
using boost::enable_if;
using boost::disable_if;


template<typename T>
class InstancePtr {
public:
	T* ptr;

	InstancePtr(T* ptr) : ptr(ptr) { }

	template<typename B>
	InstancePtr(const InstancePtr<B>& from, typename enable_if<is_base_of<T,B>,int>::type = 0) : ptr(from.ptr) { }

	bool isNull() const {
		return ptr == NULL;
	}

	operator bool() const {
		return !isNull();
	}

	const T& operator*() const {
		assert(ptr != NULL);
		return *ptr;
	}

	const T* operator->() const {
		return ptr;
	}

	/**
	 * Equality is implemented using three different, generic version of the == operator. The first
	 * two are considered whenever the two referenced types are related (sub-type of each other, in
	 * the one or the other direction). The last version is used if the two pointers are not related at
	 * all.
	 *
	 * This version is used if the type pointed to by the local pointer is a base type for the given type
	 * or both referenced types are equivalent.
	 *
	 * @tparam A the type the given pointer is pointing to
	 * @param other the other pointer (pointing to this or a sub-type) to be compared to
	 * @return true if both point to the same location (regardless of the actual type)
	 */
	template<typename A>
	const typename enable_if<is_base_of<T, A>, bool>::type operator==(const InstancePtr<A>& other) const {
		return ptr == other.ptr;
	}

	/**
	 * Equality is implemented using three different, generic version of the == operator. The first
	 * two are considered whenever the two referenced types are related (sub-type of each other, in
	 * the one or the other direction). The last version is used if the two pointers are not related at
	 * all.
	 *
	 * This version is used if the type pointed to by the local pointer is a super type for the given type
	 * and both referenced types are not equivalent.
	 *
	 * @tparam A the type the given pointer is pointing to
	 * @param other the other pointer (pointing to this or a pure super-type) to be compared to
	 * @return true if both point to the same location (regardless of the actual type)
	 */
	template<typename A>
	const typename enable_if< and_< is_base_of<A, T>,not_<is_base_of<T, A>>> , bool >::type operator==(const InstancePtr<A>& other) const {
		return ptr == other.ptr;
	}

	/**
	 * Equality is implemented using three different, generic version of the == operator. The first
	 * two are considered whenever the two referenced types are related (sub-type of each other, in
	 * the one or the other direction). The last version is used if the two pointers are not related at
	 * all.
	 *
	 * This version is used if the type pointed to are not related within the sub-type relation.
	 *
	 * NOTE: if the pointer are pointing toward the same location (yet are of of non-related types)
     * 		 the result will still be false!!
	 *
	 * @tparam A the type the given pointer is pointing to
	 * @param other the other pointer (pointing to a non-related type) to be compared to
	 * @return true if both are null, false otherwise
	 */
	template<typename A>
	const typename disable_if<or_<is_base_of<T, A>,is_base_of<A, T>>, bool>::type operator==(const InstancePtr<A>& other) const {
		// NOTE: if pointer are pointing toward the same location (yet are of different types)
		//       the result will still be false!!
		return ptr == NULL && other.ptr == NULL;
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
typename boost::enable_if<boost::is_base_of<T,B>, InstancePtr<B>>::type dynamic_pointer_cast(InstancePtr<T> src) {
	if (dynamic_cast<B*>(&(*src))) {
		return *(reinterpret_cast<InstancePtr<B>* >(&src));
	}
	return NULL;
}

