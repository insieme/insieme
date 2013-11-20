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
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

#include <boost/algorithm/string/join.hpp>
#include <boost/unordered_set.hpp>

#include "string_utils.h"
#include "container_utils.h"
#include "functional_utils.h"


namespace insieme {
namespace utils {
namespace set {

template<class Ptr>
class PointerSet : public boost::unordered_set<Ptr, hash_target<Ptr>, equal_target<Ptr>> { 
	typedef boost::unordered_set<Ptr, hash_target<Ptr>, equal_target<Ptr>> base;

public:
	PointerSet() : boost::unordered_set<Ptr, hash_target<Ptr>, equal_target<Ptr>>() { }

	template <class IterT>
	PointerSet(const IterT& begin, const IterT& end) : 
		boost::unordered_set<Ptr, hash_target<Ptr>, equal_target<Ptr>>(begin, end) { } 

	/**
	 * Constructor which allows to build a set from an initializer list:
	 * PointerSet<int> a { 01, 20, 30 };
	 */
	PointerSet(std::initializer_list<Ptr> list) : 
		boost::unordered_set<Ptr, hash_target<Ptr>, equal_target<Ptr>>(list.begin(), list.end()) { }

	bool contains(const Ptr& entry) const {
		return base::find(entry) != base::cend();
	}
};

template<class Ptr>
class PointerMultiSet : public boost::unordered_multiset<Ptr, hash_target<Ptr>, equal_target<Ptr>> { };

/**
 * The base case for the toSet template function creating an empty set.
 */
template<typename Set>
inline Set toSet() {
	return Set();
}

/**
 * A function capable of converting an abitrary number of elements into a set containing
 * the given list of arguments.
 */
template<typename Set, typename Element, typename ... Elements>
inline Set toSet(const Element& first, const Elements& ... rest) {
	auto res = toSet<Set>(rest...);
	res.insert(first);
	return res;
}

/**
 * Converts the given container into a set.
 */
template<typename Set, typename Container>
inline Set asSet(const Container& container) {
	Set res;
	std::copy(container.begin(), container.end(), inserter(res, res.end()));
	return res;
}

/**
 * A template providing a more readable utility for the membership test within a set.
 *
 * @tparam Set the type of set to be tested
 * @tparam Element the type of element to be searched
 * @param set the set to be tested
 * @param element the element to be searched
 * @return true if the element is a member of the set, false otherwise
 */
template<typename Set, typename Element>
inline bool contains(const Set& set, const Element& element) {
	return set.find(element) != set.end();
}

/**
 * A template providing a simple utility to determine whether a set A is a subset of set B.
 */
template<typename SetA, typename SetB>
inline bool isSubset(const SetA& setA, const SetB& setB) {
	if (setA.size() > setB.size()) return false;
	if (&setA == &setB) return true;
	for (const auto& cur : setA) {
		if (!insieme::utils::set::contains(setB, cur)) return false;
	}
	return true;
}

/**
 * Computes the set-union (merge) of the given sets.
 *
 * NOTE: this function should not be used to add elements to an existing set.
 *
 * @tparam set the type of set to be handled
 * @param setA the first set
 * @param setB the second set
 * @return the union of setA and setB
 */
template<typename Set>
Set merge(const Set& setA, const Set& setB) {
	// merging by simply adding all elements to one set ...
	Set res;
	res.insert(setA.cbegin(), setA.cend());
	res.insert(setB.cbegin(), setB.cend());
	return res;
}

/**
 * Inserts all the elements from the given container to the given set.
 *
 * @tparam Set the type of set forming the target
 * @tparam Container the type of container to take elements from
 * @param target the set to be extended
 * @param source the collection of elements to be added
 */
template<typename Set, typename Container>
inline void insertAll(Set& target, const Container& source) {
	target.insert(source.begin(), source.end());
}

/**
 * Computes the set-intersection of the given sets.
 *
 * @tparam set the type of set to be handled
 * @param setA the first set
 * @param setB the second set
 * @return the intersection of setA and setB
 */
template<typename Set>
Set intersect(const Set& setA, const Set& setB) {
	typedef typename Set::value_type Element;

	// intersection by iterating through one set and checking membership within the other
	Set res;
	std::for_each(setA.cbegin(), setA.cend(), [&res,&setB](const Element& cur) {
		if (setB.find(cur) != setB.cend()) {
			res.insert(cur);
		}
	});
	return res;
}

/**
 * Computes the set-difference of two sets.
 *
 * NOTE: this function should not be used to remove elements from an existing set.
 *
 * @tparam set the type of set to be handled
 * @param setA the first set
 * @param setB the second set
 * @return a set containing all elements of the first set which are not members of the second set.
 */
template<typename Set>
Set difference(const Set& setA, const Set& setB) {
	typedef typename Set::value_type Element;

	// intersection by iterating through one set and checking membership within the other
	Set res;
	std::for_each(setA.cbegin(), setA.cend(), [&res,&setB](const Element& cur) {
		if (setB.find(cur) == setB.cend()) {
			res.insert(cur);
		}
	});
	return res;
}

template<typename SetA, typename SetB>
bool equal(const SetA& setA, const SetB& setB) {
	typedef typename SetA::value_type Element;

	// check for identity
	if (&setA == &setB) {
		return true;
	}

	// quick check for its size
	if (setA.size() != setB.size()) {
		return false;
	}

	// so - the size is equal - check membership!
	return all(setA.cbegin(), setA.cend(), [&setB](const Element& cur) {
		return setB.find(cur) != setB.cend();
	});
}

/**
 * A utility function to compute a hash value for a set of elements. The hash
 * value is computed by summing up the hash values of all elements within the
 * set. Hence, the order of the elements is not considered. The hash code
 * of an empty set is defined to be zero.
 *
 * @param set the set for which a hash value should be computed
 * @param hasher a function deriving the hash value for each element
 * @return a hash value for the given set
 */
template<typename Set, typename Hasher>
std::size_t computeHash(const Set& set, Hasher hasher) {
	typedef typename Set::value_type Element;

	std::size_t seed = 0;
	std::for_each(set.cbegin(), set.cend(), [&seed, &hasher](const Element& cur) {
		seed += hasher(cur);
	});
	return seed;

}

/**
 * A utility function to compute a hash value for a set of elements. The hash
 * value is computed by summing up the hash values of all elements within the
 * set. Hence, the order of the elements is not considered. The hash code
 * of an empty set is defined to be zero.
 *
 * @param set the set for which a hash value should be computed
 * @return a hash value for the given set
 */
template<typename Set>
std::size_t computeHash(const Set& set) {
	typedef typename Set::value_type Element;
	return computeHash(set, boost::hash<Element>());
}

} // end namespace: set
} // end namespace: utils
} // end namespace: insieme

namespace std {

/**
 * Allows to print sets containing printable elements.
 *
 * @param out the stream to which the given vector should be printed to
 * @param container the set to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Key, typename Compare, typename Allocator>
std::ostream& operator<<(std::ostream& out, const std::set<Key, Compare, Allocator>& container) {

	// convert elements into strings
	std::vector<std::string> list;
	std::transform(container.begin(), container.end(), back_inserter(list), &toString<Key>);

	// print and done
	return out << "{" << boost::join(list, ",") << "}";
}

/**
 * Allows to print unordered sets including printable elements.
 *
 * @param out the stream to which the given vector should be printed to
 * @param container the vector to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Element, typename Hash, typename Pred, typename Alloc>
std::ostream& operator<<(std::ostream& out, const std::unordered_set<Element, Hash, Pred, Alloc>& container) {

	// convert elements into strings
	std::vector<std::string> list;
	std::transform(container.cbegin(), container.cend(), back_inserter(list), &toString<Element>);

	// print and done
	return out << "{" << boost::join(list, ",") << "}";
}

/**
 * Allows to print unordered sets including printable elements.
 *
 * @param out the stream to which the given vector should be printed to
 * @param container the vector to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Element, typename Hash, typename Pred, typename Alloc>
std::ostream& operator<<(std::ostream& out, const boost::unordered_set<Element, Hash, Pred, Alloc>& container) {

	// convert elements into strings
	std::vector<std::string> list;
	std::transform(container.cbegin(), container.cend(), back_inserter(list), &toString<Element>);

	// print and done
	return out << "{" << boost::join(list, ",") << "}";
}

} // end namespace std
