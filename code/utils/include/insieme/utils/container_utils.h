/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

#include <algorithm>
#include <functional>
#include <unordered_set>
#include <array>
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

// Type trait to check for STL-like container types

template <typename T, typename _ = void>
struct is_container : public std::false_type {};

namespace detail {
	template <typename... Ts>
	struct is_container_helper {};
}

template <typename T>
struct is_container<T, typename std::conditional<false, detail::is_container_helper<typename T::value_type, typename T::size_type, typename T::allocator_type,
                                                                           typename T::iterator, typename T::const_iterator, decltype(std::declval<T>().size()),
                                                                           decltype(std::declval<T>().begin()), decltype(std::declval<T>().end()),
                                                                           decltype(std::declval<T>().cbegin()), decltype(std::declval<T>().cend())>,
                                        void>::type> : public std::true_type {};



template<class, class = void>
struct is_associative_container : std::false_type {};

template<class T>
struct is_associative_container<T, typename std::enable_if<sizeof(typename T::key_type) != 0>::type> : std::true_type {};

/** Checks whether a condition is true for any element of the supplied iteration range.
 *
 *  @param first iterator designating the start of the iteration range
 *  @param last iterator designating the end of the iteration range
 *  @param predicate function to be called for each element, returns bool
 */
template <class InputIterator, class Function>
bool any(InputIterator first, InputIterator last, const Function& predicate) {
	while(first != last) {
		if(predicate(*first)) { return true; }
		++first;
	}
	return false;
}

/** Checks whether a condition is true for any element of the supplied container.
 *
 *  @param list container that is forward iteratable
 *  @param predicate function to be called for each element, returns bool
 */
template <class ContainerType, class Function>
bool any(const ContainerType& list, const Function& predicate) {
	return ::any(list.begin(), list.end(), predicate);
}
/**
 * Convenience function for std::for_each.
 */
template <typename Container, typename Functor>
inline void for_each(Container& c, const Functor& f) {
	std::for_each(c.begin(), c.end(), f);
}

/**
 * Convenience function for std::for_each operating on constant (or temporal) containers
 */
template <typename Container, typename Functor>
inline void for_each(const Container& c, const Functor& f) {
	std::for_each(c.begin(), c.end(), f);
}

/**
 * A for_each function accepting pairs of iterators as an input range.
 */
template <typename Iterator, typename Functor>
inline void for_each(const std::pair<Iterator,Iterator>& c, const Functor& f) {
	std::for_each(c.first, c.second, f);
}

/**
 * Convenience function for std::copy.
 */
template <typename Container, typename OutputIterator>
inline void copy(const Container& c, OutputIterator out) {
	std::copy(c.begin(), c.end(), out);
}

/**
 * Convenience function for std::transform for vector-like types.
 */
template <template <typename, typename> class Container, typename Functor, typename T, template <typename> class A,
          typename ResultMember = typename lambda_traits<Functor>::result_type, typename Result = Container<ResultMember, A<ResultMember>>>
inline Result transform(const Container<T, A<T>>& c, const Functor& f) {
	Result res;
	std::transform(c.begin(), c.end(), inserter(res, res.end()), f);
	return res;
}


/**
 * Convenience function for std::transform for simple map-like types.
 */
template <template <typename, typename> class SimpleMapLikeContainer, typename Functor, typename K, typename V,
          typename ResultMember = typename lambda_traits<Functor>::result_type,
          typename Result = SimpleMapLikeContainer<typename ResultMember::first_type, typename ResultMember::second_type>>
inline Result transform(const SimpleMapLikeContainer<K, V>& m, const Functor& f) {
	Result res;
	std::transform(m.begin(), m.end(), inserter(res, res.end()), f);
	return res;
}

/**
 * Convenience function for std::transform for map-like types.
 */
template <template <typename, typename, typename, typename> class MapLikeContainer, typename Functor, typename T, typename K, typename V, typename C,
          template <typename> class A, typename ResultMember = typename lambda_traits<Functor>::result_type,
          typename Result = MapLikeContainer<typename ResultMember::first_type, typename ResultMember::second_type,
                                             std::less<typename ResultMember::first_type>, A<ResultMember>>>
inline Result transform(const MapLikeContainer<K, V, C, A<T>>& m, const Functor& f) {
	Result res;
	std::transform(m.begin(), m.end(), inserter(res, res.end()), f);
	return res;
}

/**
 * Convenience function for std::transform for unordered_map-like types.
 */
template <template <typename, typename, typename, typename, typename> class HashLikeContainer, typename Functor, typename T, typename K, typename V,
          template <typename> class H, template <typename> class E, template <typename> class A,
          typename ResultMember = typename lambda_traits<Functor>::result_type,
          typename Result = HashLikeContainer<typename ResultMember::first_type, typename ResultMember::second_type, H<typename ResultMember::first_type>,
                                              E<typename ResultMember::first_type>, A<ResultMember>>>
inline Result transform(const HashLikeContainer<K, V, H<K>, E<K>, A<T>>& m, const Functor& f) {
	Result res;
	std::transform(m.begin(), m.end(), inserter(res, res.end()), f);
	return res;
}


/**
 * Convenience function for std::transform for changing vector-like types to other vector-like types.
 */
template <template <typename, typename> class TargetContainer, template <typename, typename> class Container, typename Functor, typename T,
          template <typename> class A, typename ResultMember = typename lambda_traits<Functor>::result_type,
          typename Result = TargetContainer<ResultMember, A<ResultMember>>>
inline Result transform(const Container<T, A<T>>& c, const Functor& f) {
	Result res;
	std::transform(c.begin(), c.end(), inserter(res, res.end()), f);
	return res;
}

/**
 * Convenience function for std::transform for changing vector-like types to map-like types.
 */
template <template <typename, typename, typename, typename> class TargetMapLikeContainer, template <typename, typename> class Container, typename Functor,
          typename T, template <typename> class A, typename ResultMember = typename lambda_traits<Functor>::result_type,
          typename Result = TargetMapLikeContainer<typename ResultMember::first_type, typename ResultMember::second_type,
                                                   std::less<typename ResultMember::first_type>, A<ResultMember>>>
inline Result transform(const Container<T, A<T>>& c, const Functor& f) {
	Result res;
	std::transform(c.begin(), c.end(), inserter(res, res.end()), f);
	return res;
}

/**
 * Convenience function for std::transform for changing vector-like types to set-like types.
 */
template <template <typename, typename, typename> class TargetSetLikeContainer, template <typename, typename> class Container, typename Functor,
          typename T, template <typename> class A, typename ResultMember = typename lambda_traits<Functor>::result_type,
          typename Result = TargetSetLikeContainer<ResultMember, std::less<ResultMember>, A<ResultMember>>>
inline Result transform(const Container<T, A<T>>& c, const Functor& f) {
	Result res;
	std::transform(c.begin(), c.end(), inserter(res, res.end()), f);
	return res;
}

/**
 * Convenience function for std::transform for changing map-like types to vector-like types.
 */
template <template <typename, typename> class TargetContainer, template <typename, typename, typename, typename> class MapLikeContainer, typename Functor,
          typename T, typename K, typename V, typename C, template <typename> class A, typename ResultMember = typename lambda_traits<Functor>::result_type,
          typename Result = TargetContainer<ResultMember, A<ResultMember>>>
inline Result transform(const MapLikeContainer<K, V, C, A<T>>& c, const Functor& f) {
	Result res;
	std::transform(c.begin(), c.end(), inserter(res, res.end()), f);
	return res;
}

/**
 * Convenience function for std::transform.
 */
template <typename Container, typename OutputIterator, typename Functor>
inline void transform(const Container& c, OutputIterator out, const Functor& f) {
	std::transform(c.cbegin(), c.cend(), out, f);
}

/**
 * Convenience function for std::transform.
 */
template <typename Iterator, typename OutputIterator, typename Functor>
inline void transform_range(const std::pair<Iterator, Iterator>& range, OutputIterator out, const Functor& f) {
	std::transform(range.first, range.second, out, f);
}

/**
 * A convenience function for std::reverse.
 */
template <typename Container>
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
	template <typename T>
	inline void appendToVector(vector<T>& /*vector*/) {}

	/**
	 * A variable-argument function writing elements into a vector in the given order.
	 *
	 * @tparam T the type of element maintained within the modified vector
	 * @tparam Elements the types of the remaining elements (need to be convertible to T)
	 * @param vector the vector to be written to
	 * @param first the next element to be added
	 * @param rest the remaining elements to be added
	 */
	template <typename T, typename... Elements>
	inline void appendToVector(vector<T>& vector, const T& first, const Elements&... rest) {
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
template <typename T>
inline vector<T> toVector() {
	return vector<T>();
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
template <typename T, typename... Elements>
inline vector<T> toVector(const T& first, const Elements&... rest) {
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
template <typename T, typename B>
typename enable_if<is_convertible<B, T>, void>::type addAll(vector<T>& target, const vector<B>& source) {
	if(source.empty()) { return; }
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
template <typename ListA, typename ListB, typename Comparator>
inline bool equals(const ListA& a, const ListB& b, const Comparator& comparator) {
	// ensure same size
	if(a.size() != b.size()) { return false; }

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
template <typename ListA, typename ListB>
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
template <class Container, typename Comparator>
inline bool contains(const Container& container, const typename Container::value_type& value, const Comparator& comparator) {
	return ::any(container, [&value, &comparator](const typename Container::value_type& cur) { return comparator(cur, value); });
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
template <class Container>
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
template <class Container>
inline bool contains(const Container& container, const typename Container::value_type& value) {
	return ::contains(container, value, std::equal_to<const typename Container::value_type&>());
}

/**
 * Checks whether the given map contains a key equal to the given value (using find).
 *
 * @tparam Map the type of map to be inspected
 * @param map the map to be tested
 * @param key the key to be searched
 * @return true if found, false otherwise
 */
template <class Map>
inline bool containsKey(const Map& map, const typename Map::key_type& key) {
	return map.find(key) != map.end();
}

/**
* Locates element "toFind" using find. (vector-like variant)
* If found, return it, otherwise returns "elseValue".
*/
template <class Container, class V = typename Container::value_type>
inline typename std::enable_if<!is_associative_container<Container>::value, V>::type
findOrElse(const Container& container, const V& toFind, const V& elseValue) {
	auto found = std::find(container.cbegin(), container.cend(), toFind);
	return found == container.cend() ? elseValue : *found;
}

/**
* Locates element "toFind" using find. (vector-like variant)
* If found, return it, otherwise returns a default constructed value.
*/
template <class Container, class V = typename Container::value_type>
inline typename std::enable_if<!is_associative_container<Container>::value, V>::type
findOrDefaultConstructed(const Container& container, const V& toFind) {
	return findOrElse(container, toFind, V());
}

/**
 * Locates element "toFind" using find. (associative)
 * If found, return it, otherwise returns "elseValue".
 */
template <class Container, class V = typename Container::value_type, class K = typename Container::key_type>
inline typename std::enable_if<is_associative_container<Container>::value, V>::type
		findOrElse(const Container& container, const K& toFind, const V& elseValue) {
	auto found = container.find(toFind);
	return found == container.cend() ? elseValue : *found;
}

/**
* Locates element "toFind" using find. (associative)
* If found, return it, otherwise returns a default constructed value.
*/
template <class Container, class V = typename Container::value_type, class K = typename Container::key_type>
inline typename std::enable_if<is_associative_container<Container>::value, V>::type
		findOrDefaultConstructed(const Container& container, const K& toFind) {
	return findOrElse(container, toFind, V());
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
template <class ContainerA, class ContainerB>
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
template <class ContainerA, class ContainerB, class Compare>
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
template <typename T, typename L1, typename L2>
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
template <class InputIterator, class Function>
bool all(InputIterator first, InputIterator last, const Function& predicate) {
	while(first != last) {
		if(!predicate(*first)) { return false; }
		++first;
	}
	return true;
}

/** Checks whether a condition is true for all elements of the supplied container.
 *
 *  @param list container that is forward iteratable
 *  @param predicate function to be called for each element, returns bool
 */
template <class ContainerType, class Function>
bool all(const ContainerType& list, const Function& predicate) {
	return all(list.begin(), list.end(), predicate);
}


/** Combines the hash value of each value element in the supplied range of pointers with seed.
 *
 *  @param seed hash value (MODIFIED)
 *  @param first range start
 *  @param last range end
 */
template <class InputIterator>
void hashPtrRange(size_t& seed, InputIterator first, InputIterator last) {
	while(first != last) {
		boost::hash_combine(seed, hash_value(**first));
		++first;
	}
}

/** Combines the hash value of each value element in the supplied list of pointers with seed.
 *
 *  @param seed hash value (MODIFIED)
 *  @param container iteratable container filled with pointers to value elements
 */
template <class ContainerType>
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
template <class InputIterator, class Element = typename std::iterator_traits<InputIterator>::value_type>
bool hasDuplicates(InputIterator first, InputIterator last) {
	// NOTE: uses the boost hasher instead of the default hasher!
	// see: http://stackoverflow.com/questions/647967/how-to-extend-stdtr1hash-for-custom-types
	std::unordered_set<Element, boost::hash<Element>> set(std::distance(first, last));
	return ::any(first, last, [&set](const Element& cur) {
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
template <class ContainerType>
bool hasDuplicates(const ContainerType& list) {
	return hasDuplicates(list.begin(), list.end());
}

/**
 * Checks whether the given container includes values with duplicate characteristics.
 * The complexity of this operation is at most O(n). It aborts as soon
 * as two equivalent elements have been discovered.
 *
 * WARNING: The element type of the container has to implement the operator== and
 * has to be boost-hash-able so that they can be used within unordered sets.
 *
 * @param list the list to be checked for duplicates
 * @param extractor a functor which extracts some characteristic to check for duplicates
 * @return true if duplicates could be found, false otherwise.
 */
template <class ContainerType, typename CharacteristicExtractor, typename SFINAE = typename is_container<ContainerType>::type>
bool hasDuplicates(const ContainerType& list, CharacteristicExtractor extractor) {
	std::vector<typename lambda_traits<CharacteristicExtractor>::result_type> characteristics;
	std::transform(list.begin(), list.end(), std::back_inserter(characteristics), extractor);
	return hasDuplicates(characteristics);
}

/**
 * Functor extracting the first element of a std::pair
 *
 * @tparam type of element pair
 */
template <class Pair>
struct extractFirst {
	typedef const typename Pair::first_type& result_type;
	result_type operator()(const Pair& p) const {
		return p.first;
	}
};

/**
 * Functor extracting the second element of a std::pair
 *
 * @tparam type of element pair
 */
template <class Pair>
struct extractSecond {
	typedef const typename Pair::second_type& result_type;
	result_type operator()(const Pair& p) const {
		return p.second;
	}
};

template <typename InputIterator, typename OutputIterator>
void projectToFirst(InputIterator start, InputIterator end, OutputIterator out) {
	typedef typename std::iterator_traits<InputIterator>::value_type Element;
	std::transform(start, end, out, extractFirst<Element>());
}

template <typename InputIterator, typename OutputIterator>
void projectToSecond(InputIterator start, InputIterator end, OutputIterator out) {
	typedef typename std::iterator_traits<InputIterator>::value_type Element;
	std::transform(start, end, out, extractSecond<Element>());
}

template <typename PairContainer, typename ResultContainer>
void projectToFirst(const PairContainer& input, ResultContainer& result) {
	projectToFirst(input.begin(), input.end(), std::back_inserter(result));
}

template <typename PairContainer, typename ResultContainer>
void projectToSecond(const PairContainer& input, ResultContainer& result) {
	projectToSecond(input.begin(), input.end(), std::back_inserter(result));
}

template <typename PairContainer>
vector<typename PairContainer::value_type::first_type> projectToFirst(const PairContainer& input) {
	vector<typename PairContainer::value_type::first_type> res;
	projectToFirst(input, res);
	return res;
}

template <typename PairContainer>
vector<typename PairContainer::value_type::second_type> projectToSecond(const PairContainer& input) {
	vector<typename PairContainer::value_type::second_type> res;
	projectToSecond(input, res);
	return res;
}

namespace std {

	/**
	 * Allows to print arrays including printable elements.
	 *
	 * @param out the stream to which the given vector should be printed to
	 * @param container the array to be printed
	 * @return the handed in ostream to chain operation invocations.
	 */
	template <typename Element, std::size_t size>
	std::ostream& operator<<(std::ostream& out, const std::array<Element, size>& container) {
		// print the joined elements
		return out << "[" << join(",", container) << "]";
	}

	/**
	 * Allows to print vectors including printable elements.
	 *
	 * @param out the stream to which the given vector should be printed to
	 * @param container the vector to be printed
	 * @return the handed in ostream to chain operation invocations.
	 */
	template <typename Element>
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
	template <typename Element>
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
	template <typename First, typename Second>
	std::ostream& operator<<(std::ostream& out, const std::pair<First, Second>& pair) {
		return out << "(" << pair.first << "," << pair.second << ")";
	}

	/**
	 * Enable users to print tuples whenever the element types are printable.
	 */
	namespace {

		template <std::size_t>
		struct int_ {};

		template <class Tuple, size_t Pos>
		std::ostream& print_tuple(std::ostream& out, const Tuple& t, int_<Pos>) {
			out << std::get<std::tuple_size<Tuple>::value - Pos>(t) << ',';
			return print_tuple(out, t, int_<Pos - 1>());
		}

		template <class Tuple>
		std::ostream& print_tuple(std::ostream& out, const Tuple& t, int_<1>) {
			return out << std::get<std::tuple_size<Tuple>::value - 1>(t);
		}

		template <class Tuple>
		std::ostream& print_tuple(std::ostream& out, const Tuple& /*t*/, int_<0>) {
			return out;
		}

	} // end anonymous namespace

	template <class... Args>
	std::ostream& operator<<(std::ostream& out, const std::tuple<Args...>& t) {
		out << '(';
		print_tuple(out, t, int_<sizeof...(Args)>());
		return out << ')';
	} // end std namespace


} // end namespace std


// Method to remove the head element from a std::tuple
namespace {
	template <std::size_t>
	struct int_ {};

	template <size_t Pos, class H, class... T>
	void copyTail(const std::tuple<H, T...>& src, std::tuple<T...>& dest, int_<Pos>) {
		std::get<Pos>(dest) = std::get<Pos + 1>(src);
		copyTail(src, dest, int_<Pos - 1>());
	}

	template <class H, class... T>
	void copyTail(const std::tuple<H, T...>& src, std::tuple<T...>& dest, int_<1>) {
		std::get<0>(dest) = std::get<1>(src);
	}

} // end anonymous namespace

template <class H, class... T>
std::tuple<T...> removeFirst(const std::tuple<H, T...>& t) {
	std::tuple<T...> ret;
	copyTail(t, ret, int_<sizeof...(T)-1>());
	return ret;
}
