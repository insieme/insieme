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

#include <algorithm>
#include <functional>
#include <unordered_set>
#include <vector>
#include <list>
#include <utility>

#include <boost/algorithm/string/join.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/functional/hash.hpp>
#include <boost/type_traits/is_convertible.hpp>
#include <boost/utility/enable_if.hpp>

#include "string_utils.h"
#include "iterator_utils.h"
#include "functional_utils.h"

using std::vector;
using std::function;

using boost::is_convertible;
using boost::enable_if;

/**
 * Convenience function for std::for_each.
 */
template<typename Container, typename Functor>
inline void for_each(Container& c, const Functor& f) {
	std::for_each(c.begin(), c.end(), f);
}

/**
 * Convenience function for std::for_each operating on constant (or temporal) containers
 */
template<typename Container, typename Functor>
inline void for_each(const Container& c, const Functor& f) {
	std::for_each(c.begin(), c.end(), f);
}

/**
 * Convenience function for std::for_each.
 */
template<typename Iterator, typename Functor>
inline void for_range(const std::pair<Iterator, Iterator>& c, const Functor& f) {
	std::for_each(c.first, c.second, f);
}

/**
 * Convenience function for std::copy.
 */
template<typename Container, typename OutputIterator>
inline void copy(const Container& c, OutputIterator out) {
	std::copy(c.begin(), c.end(), out);
}

/**
 * Convenience function for std::transform.
 */
template<template <typename, typename> class Container, typename Functor, 
	typename T, template <typename> class A, typename ResultMember = typename lambda_traits<Functor>::result_type,
	typename Result = Container<ResultMember, A<ResultMember>>>
inline Result transform(const Container<T, A<T>>& c, const Functor& f) {
	Result res;
	std::transform(c.begin(), c.end(), inserter(res, res.end()), f);
	return res;
}

/**
 * Convenience function for std::transform.
 */
template<typename Container, typename OutputIterator, typename Functor>
inline void transform(Container& c, OutputIterator out, const Functor& f) {
	std::transform(c.begin(), c.end(), out, f);
}

/**
 * Convenience function for std::transform.
 */
template<typename Iterator, typename OutputIterator, typename Functor>
inline void transform_range(const std::pair<Iterator,Iterator>& range, OutputIterator out, const Functor& f) {
	std::transform(range.first, range.second, out, f);
}

/**
 * A convenience function for std::reverse.
 */
template<typename Container>
inline Container& reverse(Container& c) {
	std::reverse(c.begin(), c.end());
	return c;
}

namespace {

	/**
	 * The terminal case of a function where a variable number of arguments is written into a vector in proper order.
	 *
	 * @tparam T the element type maintained within the extended vector
	 * @param vector the vector to which nothing is written to
	 */
	template<typename T>
	inline void appendToVector(vector<T>& vector) {}

	/**
	 * A variable-argument function writing elements into a vector in the given order.
	 *
	 * @tparam T the type of element maintained within the modified vector
	 * @tparam Elements the types of the remaining elements (need to be convertible to T)
	 * @param vector the vector to be written to
	 * @param first the next element to be added
	 * @param rest the remaining elements to be added
	 */
	template<typename T, typename ... Elements>
	inline void appendToVector(vector<T>& vector, const T& first, const Elements& ... rest) {
		vector.push_back(first);
		appendToVector<T>(vector, rest...);
	}

}

/**
 * Create an empty vector containing no elements.
 *
 * @tparam T the type of element to be stored in the resulting vector
 * @return the resulting vector
 */
template<typename T>
inline vector<T> toVector() {
	return vector<T> ();
}

/**
 * Creates a vector containing the given elements.
 *
 * @tparam T the type of element to be stored in the resulting vector
 * @tparam Elements the types of the remaining elements (need to be convertible to T)
 * @param first the first element to be within the list
 * @param rest the remaining elements to be stored within the list
 * @return the resulting vector
 */
template<typename T, typename ... Elements>
inline vector<T> toVector(const T& first, const Elements& ... rest) {
	vector<T> res;
	res.reserve(1 + sizeof...(rest));
	appendToVector<T>(res, first, rest...);
	return res;
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
	if (source.empty())  {
		return;
	}
	// add all elements of the source to the end of the target
	target.insert(target.end(), source.begin(), source.end());
}


/**
 * Compares the given list (and their content) and determines whether both represent
 * the an identical sequence of elements.
 *
 * @param a the first list
 * @param b the second list
 * @param comparator the comparator used to compare the elements within the given lists
 * @return true if the list have the same size and contain an equivalent sequence of elements
 * 			(according to the given comparator), false otherwise.
 */
template<typename ListA, typename ListB, typename Comparator>
inline bool equals(const ListA& a, const ListB& b, const Comparator& comparator) {

	// ensure same size
	if (a.size() != b.size()) {
		return false;
	}

	// compare values using std equal ...
	return std::equal(a.begin(), a.end(), b.begin(), comparator);
}

/**
 * Compares the given list (and their content) and determines whether both represent
 * the an identical sequence of elements.
 *
 * @param a the first list
 * @param b the second list
 */
template<typename ListA, typename ListB>
inline bool equals(const ListA& a, const ListB& b) {
	return equals(a, b, std::equal_to<const typename ListA::value_type&>());
}

/**
 * Checks whether the given container contains an element being equal to the given value regarding
 * the given comparison predicate.
 *
 * @tparam Container the type of container to be inspected
 * @tparam Comparator the type of the comparison predicate
 * @param container the container to be tested
 * @param value the value to be searched
 * @param comparator the comparator used to compare the given value with the elements within the container
 * @return true if found, false otherwise
 */
template<class Container, typename Comparator>
inline bool contains(const Container& container, const typename Container::value_type& value, const Comparator& comparator) {
	return any(container, [&value, &comparator](const typename Container::value_type& cur){
		return comparator(cur, value);
	});
}

/**
 * Checks whether the given container contains an element pointing to an equal value
 * as the given pointer.
 *
 * @tparam Container the type of container to be inspected
 * @param container the container to be tested
 * @param ptr the pointer to the element to be searched
 * @return true if found, false otherwise
 */
template<class Container>
inline bool containsPtrToTarget(const Container& container, const typename Container::value_type& value) {
	return contains(container, value, equal_target<typename Container::value_type>());
}

/**
 * Checks whether the given container an element being equal to the given value (using operator==).
 *
 * @tparam Container the type of container to be inspected
 * @param container the container to be tested
 * @param value the value to be searched
 * @return true if found, false otherwise
 */
template<class Container>
inline bool contains(const Container& container, const typename Container::value_type& value) {
	return contains(container, value, std::equal_to<const typename Container::value_type&>());
}

/**
 * Compares the content of the two given containers using lexicographical order.
 *
 * @tparam ContainerA type of first container
 * @tparam ContainerB type of second container
 * @param a the first container
 * @param b the second container
 * @return true if a is lexicographical less than b
 */
template<class ContainerA, class ContainerB>
inline bool lexicographical_compare(const ContainerA& a, const ContainerB& b) {
	return std::lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
}

/**
 * Compares the content of the two given containers using lexicographical order.
 *
 * @tparam ContainerA type of first container
 * @tparam ContainerB type of second container
 * @tparam Compare the comparator to be used for comparing the contained elements
 * @param a the first container
 * @param b the second container
 * @param comp the comparator to be used
 * @return true if a is lexicographical less than b
 */
template<class ContainerA, class ContainerB, class Compare>
inline bool lexicographical_compare(const ContainerA& a, const ContainerB& b, Compare comp) {
	return std::lexicographical_compare(a.begin(), a.end(), b.begin(), b.end(), comp);
}

/**
 * Creates a new list containing the same elements as a concatenation
 * of the given lists.
 *
 * @tparam List the type of list to be handled
 * @param listA the first list
 * @param listB the second list
 * @return the resulting concatenated of listA and listB
 */
template<typename T, typename L1, typename L2>
std::vector<T> concatenate(const L1& listA, const L2& listB) {
	// merging by simply adding all elements to one set ...
	std::vector<T> res;
	std::copy(listA.begin(), listA.end(), std::back_inserter(res));
	std::copy(listB.begin(), listB.end(), std::back_inserter(res));
	return res;
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
	return all(list.begin(), list.end(), predicate);
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
	return any(list.begin(), list.end(), predicate);
}

/** Combines the hash value of each value element in the supplied range of pointers with seed.
 *
 *  @param seed hash value (MODIFIED)
 *  @param first range start
 *  @param last range end
 */
template<class InputIterator>
void hashPtrRange(size_t& seed, InputIterator first, InputIterator last) {
	while (first != last) {
		boost::hash_combine(seed, hash_value(**first));
		++first;
	}
}

/** Combines the hash value of each value element in the supplied list of pointers with seed.
 *
 *  @param seed hash value (MODIFIED)
 *  @param container iteratable container filled with pointers to value elements
 */
template<class ContainerType>
void hashPtrRange(size_t& seed, const ContainerType& container) {
	hashPtrRange(seed, container.begin(), container.end());
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
	return hasDuplicates(list.begin(), list.end());
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

template<typename InputIterator, typename OutputIterator>
void projectToFirst(InputIterator start, InputIterator end, OutputIterator out) {
	typedef typename std::iterator_traits<InputIterator>::value_type Element;
	std::transform(start, end, out, extractFirst<Element>());
}

template<typename InputIterator, typename OutputIterator>
void projectToSecond(InputIterator start, InputIterator end, OutputIterator out) {
	typedef typename std::iterator_traits<InputIterator>::value_type Element;
	std::transform(start, end, out, extractSecond<Element>());
}

template<typename PairContainer, typename ResultContainer>
void projectToFirst(const PairContainer& input, ResultContainer& result) {
	projectToFirst(input.begin(), input.end(), std::back_inserter(result));
}

template<typename PairContainer, typename ResultContainer>
void projectToSecond(const PairContainer& input, ResultContainer& result) {
	projectToSecond(input.begin(), input.end(), std::back_inserter(result));
}

template<typename PairContainer>
vector<typename PairContainer::value_type::first_type> projectToFirst(const PairContainer& input) {
	vector<typename PairContainer::value_type::first_type> res;
	projectToFirst(input, res);
	return res;
}

template<typename PairContainer>
vector<typename PairContainer::value_type::second_type> projectToSecond(const PairContainer& input) {
	vector<typename PairContainer::value_type::second_type> res;
	projectToSecond(input, res);
	return res;
}

namespace std {

/**
 * Allows to print vectors including printable elements.
 *
 * @param out the stream to which the given vector should be printed to
 * @param container the vector to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Element>
std::ostream& operator<<(std::ostream& out, const vector<Element>& container) {

	// convert elements into strings
	vector<string> list;
	std::transform(container.begin(), container.end(), back_inserter(list), &toString<Element>);

	// print and done
	return out << "[" << boost::join(list, ",") << "]";
}

/**
 * Allows to print lists including printable elements.
 *
 * @param out the stream to which the given vector should be printed to
 * @param container the list to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Element>
std::ostream& operator<<(std::ostream& out, const std::list<Element>& container) {

	// convert elements into strings
	vector<string> list;
	std::transform(container.begin(), container.end(), back_inserter(list), &toString<Element>);

	// print and done
	return out << "[" << boost::join(list, ",") << "]";
}

/**
 * Enables user to print pairs whenever the element types are printable.
 *
 * @param out the stream to which the given pair should be printed
 * @param pair the pair to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename First, typename Second>
std::ostream& operator<<(std::ostream& out, const std::pair<First,Second>& pair) {
	out << "(" << pair.first << "," << pair.second << ")";
	return out;
}

/**
 * Enable users to print tuples whenever the element types are printable.
 */
namespace {
template<std::size_t> struct int_{};

template <class Tuple, size_t Pos>
std::ostream& print_tuple(std::ostream& out, const Tuple& t, int_<Pos> ) {
	out << std::get< std::tuple_size<Tuple>::value-Pos >(t) << ',';
	return print_tuple(out, t, int_<Pos-1>());
}

template <class Tuple>
std::ostream& print_tuple(std::ostream& out, const Tuple& t, int_<1> ) {
	return out << std::get<std::tuple_size<Tuple>::value-1>(t);
}
} // end anonymous namespace 

template <class... Args>
ostream& operator<<(ostream& out, const std::tuple<Args...>& t) {
	out << '(';
	print_tuple(out, t, int_<sizeof...(Args)>());
	return out << ')';
} // end std namespace 


}


// Method to remove the head element from a std::tuple
namespace {
template<std::size_t> struct int_{};

template <size_t Pos, class H, class ...T>
void copyTail(const std::tuple<H, T...>& src, std::tuple<T...>& dest, int_<Pos>) {
	std::get<Pos>(dest) = std::get<Pos+1>(src);
	copyTail(src, dest, int_<Pos-1>());
}

template< class H, class ...T>
void copyTail(const std::tuple<H, T...>& src, std::tuple<T...>& dest, int_<1>) {
	std::get<0>(dest) = std::get<1>(src);
}

} // end anonymous namespace 

template <class H, class ...T>
std::tuple<T...> removeFirst(const std::tuple<H,T...>& t) {
	std::tuple<T...> ret;
	copyTail(t, ret, int_<sizeof...(T)-1>());
	return ret;
}


