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

#include <functional>

#include "insieme/utils/map_utils.h"
#include "insieme/utils/functional_utils.h"

namespace insieme {
namespace utils {
namespace cache {

	/**
	 * A template class realizing a simple value cache. The cache is mapping
	 * values from a given key to a given value. Internally, previously computed
	 * results are cached and if requested returned immediately without repeating
	 * the computation again.
	 *
	 * @tparam Key the key used to index the
	 */
	template<
		typename Key,
		typename Value,
		typename Store = std::map<Key, Value>
	>
	class Cache {
	public:

		typedef Key key_type;
		typedef Value value_type;
		typedef std::function<Value(const Key&)> factory_type;
		typedef Store store_type;

	private:

		/**
		 * The internal data cache containing all previously obtained values.
		 */
		Store cache;

		/**
		 * An instance of the factory used to obtain missing values within the cache.
		 */
		factory_type default_factory;

	public:

		/**
		 * Creates a new cache instance without a default factory.
		 */
		Cache() {};

		/**
		 * Creates a new factory based on the given factory.
		 *
		 * @param default_factory the default factory to be used to resolve missing values within this cache
		 */
		Cache(const factory_type& default_factory) : default_factory(default_factory) {}

		/**
		 * Obtains a value from this cache. If the value has been obtained
		 * before for the given key, the previously obtained value will
		 * be returned. Otherwise the internally stored default factory will be used
		 * to obtain a new value and store it within this cache using the given key.
		 *
		 * @param key the key to be looked up
		 * @return the corresponding value cached inside or produced by the factory
		 */
		Value get(const Key& key) {
			return get(key, default_factory);
		}

		/**
		 * Obtains a value from this cache. If the value has been obtained
		 * before for the given key, the previously obtained value will
		 * be returned. Otherwise the given factory will be used
		 * to obtain a new value and store it within this cache using the given key.
		 *
		 * @param key the key to be looked up
		 * @param factory the factory to be used for the computation of the value if it is missing
		 * @return the corresponding value cached inside or produced by the factory
		 */
		Value get(const Key& key, const factory_type& factory) {
			auto pos = cache.find(key);
			if (pos != cache.end()) {
				return pos->second;
			}
			Value res = factory(key);
			cache[key] = res;
			return res;
		}

	};


	/**
	 * A cache utility simplifying the handling of caches where the key is given by a pointer
	 * like type. To handle pointers, the internal map is realized by a pointer map.
	 */
	template<
		typename Key,
		typename Value
	>
	class PointerCache : public Cache<Key, Value, map::PointerMap<Key, Value>> {
	public:

		/**
		 * Define the type of the factory for producing values within this cache.
		 */
		typedef typename Cache<Key, Value, map::PointerMap<Key, Value>>::factory_type factory_type;

		/**
		 * A default constructor for this cache.
		 */
		PointerCache() {}

		/**
		 * A constructor for a cache using the given factory as a default factory.
		 */
		PointerCache(const factory_type& default_factory) : Cache<Key, Value, map::PointerMap<Key, Value>>(default_factory) {}
	};

} // end namespace cache
} // end namespace utils
} // end namespace insieme
