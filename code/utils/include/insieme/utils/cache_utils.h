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
 *
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
	 * @tparam Key the key used to index the cache
	 * @tparam Value the value to be stored within the cache
	 * @tparam Factory the factory used for obtaining missing values
	 * @tparam Store the store used to maintain data instances
	 */
	template <typename Key, typename Value, typename Factory = std::function<Value(const Key&)>, typename Store = std::map<Key, Value>>
	class Cache {
	  public:
		typedef Key key_type;
		typedef Value value_type;
		typedef Factory factory_type;
		typedef Store store_type;

	  private:
		/**
		 * The internal data cache containing all previously obtained values.
		 */
		Store cache;

		/**
		 * An instance of the factory used to obtain missing values within the cache.
		 */
		Factory default_factory;

	  public:
		/**
		 * Creates a new cache instance without a default factory.
		 */
		Cache(){};

		/**
		 * Creates a new factory based on the given factory.
		 *
		 * @param default_factory the default factory to be used to resolve missing values within this cache
		 */
		Cache(const Factory& default_factory) : default_factory(default_factory) {}

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
		Value get(const Key& key, const Factory& factory) {
			auto pos = cache.find(key);
			if(pos != cache.end()) { return pos->second; }
			Value res = factory(key);
			cache[key] = res;
			return res;
		}

		/**
		 * Overrides the current (potentially non-existing) value assigned to the given
		 * key within this cache.
		 *
		 * @param key the addressed key
		 * @param value the new value to be assigned to the given key
		 */
		void set(const Key& key, const Value& value) {
			cache[key] = value;
		}

		/**
		 * Clears this cache. After an invocation, the cache will be empty again.
		 */
		void clear() {
			cache.clear();
		}

		/**
		 * Clears a single entry within this cache.
		 *
		 * @param key the key of the entry to be cleared.
		 */
		void clearEntry(const Key& key) {
			cache.erase(key);
		}
	};


	/**
	 * A cache utility simplifying the handling of caches where the key is given by a pointer
	 * like type. To handle pointers, the internal map is realized by a pointer map.
	 */
	template <typename Key, typename Value, typename Factory = std::function<Value(const Key&)>>
	class PointerCache : public Cache<Key, Value, Factory, map::PointerMap<Key, Value>> {
	  public:
		/**
		 * A default constructor for this cache.
		 */
		PointerCache() {}

		/**
		 * A constructor for a cache using the given factory as a default factory.
		 */
		PointerCache(const Factory& default_factory) : Cache<Key, Value, Factory, map::PointerMap<Key, Value>>(default_factory) {}
	};

} // end namespace cache
} // end namespace utils
} // end namespace insieme
