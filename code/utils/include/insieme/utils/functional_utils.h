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

#include <boost/functional/hash.hpp>
#include <boost/type_traits/is_pointer.hpp>
#include <boost/type_traits/remove_pointer.hpp>
#include <boost/utility/enable_if.hpp>

#include "insieme/utils/hash_utils.h"

struct empty {};

template<typename T>
struct id : public std::unary_function<T, T> {
	T& operator()(const T& element) const { return element; }
};


template<typename PointerType>
struct deref: public std::unary_function<const PointerType&, const typename PointerType::element_type&> {
	const typename PointerType::element_type& operator()(const PointerType& ptr) const {
		return *ptr;
	}
};


/**
 * This utility struct definition defines a predicate comparing two pointers
 * based on the value they are pointing to.
 *
 * @tparam PointerType the type of pointer to be compared
 */
template<typename PointerType>
struct equal_target : public std::binary_function<const PointerType&, const PointerType&, bool> {
	/**
	 * Performs the actual comparison by using the operator== of the generic
	 * pointer type.
	 *
	 * @param x the pointer to the first element to be compared
	 * @param y the pointer to the second element to be compared
	 */
	bool operator()(const PointerType& x, const PointerType& y) const {
		return *x == *y;
	}
};

/**
 * This utility struct definition defines a predicate comparing two pointers
 * based on the value they are pointing to (operator <).
 *
 * @tparam PointerType the type of pointer to be compared
 */
template<typename PointerType>
struct compare_target : public std::binary_function<const PointerType&, const PointerType&, bool> {
	/**
	 * Performs the actual comparison by using the operator< of the generic
	 * pointer type.
	 *
	 * @param x the pointer to the first element to be compared
	 * @param y the pointer to the second element to be compared
	 */
	bool operator()(const PointerType& x, const PointerType& y) const {
		return *x < *y;
	}
};



/**
 * This utility struct defines the function used to compute hash codes for pointers.
 * Thereby, the hash code is not computed using the pointer themselves. Instead, the
 * target they are pointing to is used to compute the value. In case the pointer is null,
 * 0 is returned as a hash value.
 *
 * @tparam PointerType the type of the pointer to be hashed
 */
template<typename PointerType, typename Enabled = void>
struct hash_target; // { /* default is not working */ };

/**
 * This partial template specialization of the hash_target struct is handling
 * real pointers.
 */
template<typename PointerType>
struct hash_target<PointerType, typename boost::enable_if<boost::is_pointer<PointerType>>::type> {

	/**
	 * Derives the element type be removing the pointer extension.
	 */
	typedef typename boost::remove_pointer<PointerType>::type ElementType;

	/**
	 * This function is used to compute the hash of the actual target.
	 */
	boost::hash<ElementType> hasher;

	/**
	 * Explicit Default constructor required by VC.
	 */
	hash_target() : hasher() {}

	/**
	 * Computes the hash value of the given pointer based on the target it is pointing to.
	 */
	std::size_t operator()(const PointerType p) const {
		if (p) {
			return hasher(*p);
		}
		return 0;
	}
};

/**
 * This partial template specialization of the hash_target struct is handling
 * smart pointers.
 */
template<typename PointerType>
struct hash_target<PointerType, typename boost::disable_if<boost::is_pointer<PointerType>>::type> {

	/**
	 * Obtains the element type from the smart pointer.
	 */
	typedef typename PointerType::element_type ElementType;

	/**
	 * This function is used to compute the hash of the actual target.
	 */
	boost::hash<ElementType> hasher;

	/**
	 * Explicit Default constructor required by VC.
	 */
	hash_target() : hasher() {}

	/**
	 * Computes the hash value of the given pointer based on the target it is pointing to.
	 */
	std::size_t operator()(const PointerType p) const {
		if (p) {
			return hasher(*p);
		}
		return 0;
	}
};


// -------------------- Function Traits for Lambdas ----------------------------
//
// see: http://stackoverflow.com/questions/2611357/lambda-traits-inconsistency-across-c0x-compilers
// see: boost function_traits.hpp (which unfortunatelly only work for function pointer, not member function pointer.

namespace detail {

	template<typename Function> struct lambda_traits_helper { };

	// - for member function pointer -
	//
	// TODO: enable in case variadic templates will ever be supported
	//
	//template <typename R, typename C, typename ... A >
	//struct function_traits<R (C::*)( A ... ) const>  { // inherits from this one on VS2010 RC
	//  typedef R result_type;
	//};

	template<typename R, typename C>
	struct lambda_traits_helper<R (C::*)(void) const>
	{
	  BOOST_STATIC_CONSTANT(unsigned, arity = 0);
	  typedef R result_type;
	};

	template<typename R, typename C, typename T1>
	struct lambda_traits_helper<R (C::*)(T1) const>
	{
	  BOOST_STATIC_CONSTANT(unsigned, arity = 1);
	  typedef R result_type;
	  typedef T1 arg1_type;
	  typedef T1 argument_type;
	};

	template<typename R, typename C, typename T1, typename T2>
	struct lambda_traits_helper<R (C::*)(T1, T2) const>
	{
	  BOOST_STATIC_CONSTANT(unsigned, arity = 2);
	  typedef R result_type;
	  typedef T1 arg1_type;
	  typedef T2 arg2_type;
	  typedef T1 first_argument_type;
	  typedef T2 second_argument_type;
	};

	template<typename R, typename C, typename T1, typename T2, typename T3>
	struct lambda_traits_helper<R (C::*)(T1, T2, T3) const>
	{
	  BOOST_STATIC_CONSTANT(unsigned, arity = 3);
	  typedef R result_type;
	  typedef T1 arg1_type;
	  typedef T2 arg2_type;
	  typedef T3 arg3_type;
	};

} // end namespace detail


template <typename Lambda>
struct lambda_traits : public detail::lambda_traits_helper<decltype(&Lambda::operator())> { };

