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

#include <vector>
#include <functional>
#include <unordered_set>

#include <boost/functional/hash.hpp>
#include <boost/type_traits/is_convertible.hpp>
#include <boost/utility/enable_if.hpp>


using std::vector;
using std::function;

using boost::is_convertible;
using boost::enable_if;


/**
 * Creates a vector containing (a copy of) the single element provided as an argument.
 *
 * @tparam the type of element to be contained within the singleton vector
 * @param element the element to be included within the new vector
 *
 * @return a new vector instance containing a single element
 */
template<typename T>
vector<T> toVector(T element) {
	return vector<T> (1, element);
}

/**
 * A small utility function capable of appending all elements of one vector to another.
 *
 * @tparam T the element type of the target vector
 * @tparam B the element type of the source vector, needs to be a sub-type of T
 * @param target the vector to which elements should be appended
 * @param source the vector from which the element should be taken
 */
template<typename T, typename B>
typename enable_if<is_convertible<B,T>, void>::type addAll(vector<T>& target, const vector<B>& source) {
	// add all elements of the source to the end of the target
	copy(source.cbegin(), source.cend(), back_inserter(target));
}


/** Checks whether a condition is true for all elements of the supplied iteration range.
 *
 *  @param first iterator designating the start of the iteration range
 *  @param last iterator designating the end of the iteration range
 *  @param predicate function to be called for each element, returns bool
 */
template<class InputIterator, class Function>
bool all(InputIterator first, InputIterator last, const Function& predicate)
{
	while (first != last) {
		if (!predicate(*first)) {
			return false;
		}
		++first;
	}
	return true;
}

/** Checks whether a condition is true for all elements of the supplied container.
 *
 *  @param list container that is forward iteratable
 *  @param predicate function to be called for each element, returns bool
 */
template<class ContainerType, class Function>
bool all(const ContainerType& list, const Function& predicate) {
	return all(list.cbegin(), list.cend(), predicate);
}

/** Checks whether a condition is true for any element of the supplied iteration range.
 *
 *  @param first iterator designating the start of the iteration range
 *  @param last iterator designating the end of the iteration range
 *  @param predicate function to be called for each element, returns bool
 */
template<class InputIterator, class Function>
bool any(InputIterator first, InputIterator last, const Function& predicate)
{
	while (first != last) {
		if (predicate(*first)) {
			return true;
		}
		++first;
	}
	return false;
}

/** Checks whether a condition is true for any element of the supplied container.
 *
 *  @param list container that is forward iteratable
 *  @param predicate function to be called for each element, returns bool
 */
template<class ContainerType, class Function>
bool any(const ContainerType& list, const Function& predicate) {
	return any(list.cbegin(), list.cend(), predicate);
}

/**
 * Checks whether the given range includes duplicated values.
 * The complexity of this operation is at most O(n). It aborts as soon
 * as two equivalent elements have been discovered.
 *
 * WARNING: The element type of the container has to implement the operator== and
 * has to be boost-hash-able so that they can be used within unordered sets.
 *
 * @param first the iterator pointing to the first element of the range
 * @param last the iterator pointing to the end of the range
 * @return true if duplicates could be found, false otherwise.
 */
template<class InputIterator>
bool hasDuplicates(InputIterator first, InputIterator last) {
	typedef typename std::iterator_traits<InputIterator>::value_type Element;

	// NOTE: uses the boost hasher instead of the default hasher!
	// see: http://stackoverflow.com/questions/647967/how-to-extend-stdtr1hash-for-custom-types
	std::unordered_set<Element, boost::hash<Element>> set(std::distance(first,last));
	return any(first, last, [&set](const Element& cur) {
		// check whether the current element is a new element
		//  - if it is new, insert will return true => fine
		//  - if it is not, insert will return false => duplicate found
		return !set.insert(cur).second;
	});
}


/**
 * Checks whether the given container includes duplicated values.
 * The complexity of this operation is at most O(n). It aborts as soon
 * as two equivalent elements have been discovered.
 *
 * WARNING: The element type of the container has to implement the operator== and
 * has to be boost-hash-able so that they can be used within unordered sets.
 *
 * @param list the list to be checked for duplicates
 * @return true if duplicates could be found, false otherwise.
 */
template<class ContainerType>
bool hasDuplicates(const ContainerType& list) {
	return hasDuplicates(list.cbegin(), list.cend());
}

/**
 * Functor extracting the first element of a std::pair
 *
 * @tparam type of element pair
 */
template<class Pair>
struct extractFirst {
	typedef const typename Pair::first_type & result_type;
	result_type operator()(const Pair& p) const { return p.first; }
};

/**
 * Functor extracting the second element of a std::pair
 *
 * @tparam type of element pair
 */
template<class Pair>
struct extractSecond {
	typedef const typename Pair::second_type & result_type;
	result_type operator()(const Pair& p) const { return p.second; }
};
