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

template<class KeyPtr, class ValueType>
struct PointerMap : public std::unordered_map<KeyPtr, ValueType, hash_target<KeyPtr>, equal_target<KeyPtr>> {
	template<typename Container> void insertAll(const Container& c) { insert(c.begin(), c.end()); }
};

template<class KeyPtr, class ValueType>
class PointerMultiMap : public boost::unordered_multimap<KeyPtr, ValueType, hash_target<KeyPtr>, equal_target<KeyPtr>> { };

/**
 * Crates a singleton mapping between the given key/value pair.
 */
template<class Key, class Value>
PointerMap<Key, Value> mapPointerTo(const Key& key, const Value& value) {
	PointerMap<Key, Value> res;
	res[key] = value;
	return res;
}

/**
 * A utility function adding data to a given map. This is the terminal case,
 * where there is no data to be added. The same function is overloaded for
 * differnet parameters, allowing more data to be added.
 *
 * @param map the map to be extended with nothing.
 */
template<class Key, class Value>
inline PointerMap<Key,Value>& addMappings(PointerMap<Key,Value>& map) {
	return map;
}

/**
 * A utility function adding data to a given map. The data after the given map
 * will be interpreted as a sequence of key/value elements.
 *
 * @param map the map to be extended
 * @param key the first key to be added to the given map
 * @param value the value to be assigned to the given key
 * @param rest the remaining key/value pairs being processed recursively
 * @return a reference to the given map
 */
template<class Key, class Value, class ... Data>
inline PointerMap<Key,Value>& addMappings(PointerMap<Key,Value>& map, const Key& key, const Value& value, Data ... rest) {
	addMappings<Key, Value>(map, rest ...);
	map.insert(std::make_pair(key,value));
	return map;
}

/**
 * A generic utility function allowing to compose pointer mappings within
 * a single expression. The function will create a new map and fill it with
 * key / value pairs as they are provided as additional parameters. If the
 * same key occurs multiple times, the latest value will be assigned in the
 * resulting map.
 *
 * @tparam Key the type of key to be used within the resulting map
 * @tparam Value the type of value to be stored within the resulting map
 * @tparam Data the variadic argument list covering the alternating key/value pairs
 * @param key the first key to be inserted (for automatic type deduction)
 * @param value the first value to be assigned to the given key
 * @param rest the remaining data to be inserted into the resulting map
 * @return a map containing all the given data
 */
template<class Key, class Value, class ... Data>
inline PointerMap<Key,Value> toPointerMap(const Key& key, const Value& value, const Data ... rest) {
	PointerMap<Key,Value> res;
	return addMappings<Key, Value>(res, key, value, rest ...);
}

/**
 * Compares whether the two given maps are equal, hence they contain
 * the same set of keys and to each key, equivalent values are attacked.
 *
 * @tparam MapA the type of the first map
 * @tparam MapB the type of the second map
 * @param mapA the first map
 * @param mapB the second map
 * @param comparator the comparator used to compare values within the maps
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
		return pos!=mapB.end() && comparator(pos->second,cur.second);
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
 * @param keyHasher a function deriving a hash value for each key
 * @param valueHasher a function deriving a hash value for each value
 * @return a hash value for the given map
 */
template<typename Map, typename KeyHasher, typename ValueHasher>
std::size_t computeHash(const Map& map, KeyHasher keyHasher, ValueHasher valueHasher) {
	typedef typename Map::value_type Element;

	// compute the hash code for the map
	std::size_t seed = 0;
	std::for_each(map.begin(), map.end(), [&seed, &keyHasher, &valueHasher](const Element& cur) {
		seed += keyHasher(cur.first) ^ valueHasher(cur.second);
	});
	return seed;
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
	// forward request
	typename Map::hasher keyHasher = map.hash_function();
	return computeHash(map, keyHasher, hasher);
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
std::size_t computeHash(const Map& map) {
	typedef typename Map::mapped_type Element;
	return computeHash(map, boost::hash<Element>());
}

} // end namespace: map
} // end namespace: utils
} // end namespace: insieme

/**
 * Integrate standard map hashing into the boost.
 */
template<typename K, typename T, typename C, typename A>
size_t hash_value(const std::map<K,T,C,A>& map) {
	return insieme::utils::map::computeHash(map);
}

/**
 * Integrate unordered map hashing into the boost.
 */
template<typename K, typename T, typename Hash, typename KeyEqual, typename Allocator>
size_t hash_value(const std::unordered_map<K,T,Hash,KeyEqual,Allocator>& map) {
	return insieme::utils::map::computeHash(map);
}

/**
 * Integrate unordered map hashing into the boost.
 */
template<typename K, typename T, typename Hash, typename KeyEqual, typename Allocator>
size_t hash_value(const boost::unordered_map<K,T,Hash,KeyEqual>& map) {
	return insieme::utils::map::computeHash(map);
}

namespace std {

/**
 * Add hash support for unordered maps.
 */
template<typename K, typename T, typename C, typename A>
struct hash<std::map<K,T,C,A>> {
	size_t operator()(const std::map<K,T,C,A>& instance) const {
		return hash_value(instance); // bridge to boost solution
	}
};

/**
 * Add hash support for unordered maps.
 */
template<typename K, typename T, typename H, typename KE, typename A>
struct hash<std::unordered_map<K,T,H,KE,A>> {
	size_t operator()(const std::unordered_map<K,T,H,KE,A>& instance) const {
		return hash_value(instance); // bridge to boost solution
	}
};

/**
 * Add hash support for unordered maps.
 */
template<typename K, typename T, typename H, typename KE, typename A>
struct hash<boost::unordered_map<K,T,H,KE,A>> {
	size_t operator()(const boost::unordered_map<K,T,H,KE,A>& instance) const {
		return hash_value(instance); // bridge to boost solution
	}
};

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

	return out << "{" <<  join(", ", container, [](std::ostream& out, const Element& cur) {
		out << cur.first << "=" << cur.second;
	}) << "}";
}

/**
 * Allows to print maps including printable elements.
 *
 * @param out the stream to which the given map should be printed to
 * @param container the map to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Key, typename Mapped, typename Compare, typename Alloc>
std::ostream& operator<<(std::ostream& out, const std::map<Key, Mapped, Compare, Alloc>& container) {
	typedef typename std::map<Key, Mapped, Compare, Alloc>::value_type Element;

	return out << "{" <<  join(", ", container, [](std::ostream& out, const Element& cur) {
		out << cur.first << "=" << cur.second;
	}) << "}";
}

}

