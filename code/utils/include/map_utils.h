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
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/algorithm/string/join.hpp>
#include <boost/unordered_map.hpp>

#include "string_utils.h"
#include "container_utils.h"


namespace insieme {
namespace utils {
namespace map {


/**
 * Compares whether the two given maps are equal, hence they contain
 * the same set of keys and to each key, equivalent values are attacked.
 *
 * @tparam MapA the type of the first map
 * @tparam MapB the type of the second map
 * @param mapA the first map
 * @param mapB the second map
 * @return true if equivalent, false otherwise
 */
template<typename MapA, typename MapB, typename Comparator>
bool equal(const MapA& mapA, const MapB& mapB, const Comparator& comparator) {
	typedef typename MapA::value_type Element;

	// check for identity
	if (&mapA == &mapB) {
		return true;
	}

	// quick check for its size
	if (mapA.size() != mapB.size()) {
		return false;
	}

	// so - the size is equal - check elements ...
	return all(mapA.begin(), mapA.end(), [&mapB,&comparator](const Element& cur)->bool {
		auto pos = mapB.find(cur.first);
		return pos!=mapB.end() && comparator((*pos).second,cur.second);
	});
}

/**
 * Compares whether the two given maps are equal, hence they contain
 * the same set of keys and to each key, equivalent values are attacked.
 *
 * @tparam MapA the type of the first map
 * @tparam MapB the type of the second map
 * @param mapA the first map
 * @param mapB the second map
 * @return true if equivalent, false otherwise
 */
template<typename MapA, typename MapB>
inline bool equal(const MapA& mapA, const MapB& mapB) {
	return insieme::utils::map::equal(mapA, mapB, std::equal_to<typename MapA::mapped_type>());
}

/**
 * A utility function to compute a hash value for a map. The hash
 * value is computed by summing up the hash values of all pairs within the
 * map. Hence, the order of the elements is not considered. The hash code
 * of an empty map is defined to be zero.
 *
 * @param map the map for which a hash value should be computed
 * @param hasher a function deriving the hash value for each value
 * @return a hash value for the given map
 */
template<typename Map, typename Hasher>
std::size_t computeHash(const Map& map, Hasher hasher) {
	typedef typename Map::value_type Element;

	// obtain the hasher used for key-hashing
	typename Map::hasher keyHasher = map.hash_function();

	// compute the hash code for the map
	std::size_t seed = 0;
	std::for_each(map.begin(), map.end(), [&seed, &keyHasher, &hasher](const Element& cur) {
		seed += keyHasher(cur.first) ^ hasher(cur.second);
	});
	return seed;

}

/**
 * A utility function to compute a hash value for a map. The hash
 * value is computed by summing up the hash values of all pairs within the
 * map. Hence, the order of the elements is not considered. The hash code
 * of an empty set is defined to be zero.
 *
 * @param map the map for which a hash value should be computed
 * @return a hash value for the given map
 */
template<typename Map>
std::size_t computeHash(const Map& set) {
	typedef typename Map::mapped_type Element;
	return computeHash(set, boost::hash<Element>());
}

} // end namespace: map
} // end namespace: utils
} // end namespace: insieme

/**
 * Allows to print unordered maps including printable elements.
 *
 * @param out the stream to which the given map should be printed to
 * @param container the map to be printed
 * @return the handed in ostream to chain operation invocations.
 */

template<typename Key, typename Mapped, typename Hash, typename Pred, typename Alloc>
std::ostream& operator<<(std::ostream& out, const std::unordered_map<Key, Mapped, Hash, Pred, Alloc>& container) {
	typedef typename std::unordered_map<Key, Mapped, Hash, Pred, Alloc>::value_type Element;

	return out << "{" <<  join(", ", container, [](std::ostream& out, const Element& cur) {
		out << cur.first << "=" << cur.second;
	}) << "}";
}

/**
 * Allows to print unordered maps including printable elements.
 *
 * @param out the stream to which the given map should be printed to
 * @param container the map to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Key, typename Mapped, typename Hash, typename Pred, typename Alloc>
std::ostream& operator<<(std::ostream& out, const boost::unordered_map<Key,Mapped, Hash, Pred, Alloc>& container) {
	typedef typename boost::unordered_map<Key, Mapped, Hash, Pred, Alloc>::value_type Element;

	return out << "{" <<  join(", ", container, [](const Element& cur) {
		return format("%s=%s", toString(cur.first), toString(cur.second));
	}) << "}";
}
