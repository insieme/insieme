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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_mapper.h"

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/cache_utils.h"

namespace insieme {
namespace core {
namespace transform {

	/**
	 * A common base class for implementing node mappings which benefit from caching results
	 */
	class CachedNodeMapping : public SimpleNodeMapping {
		/**
		 * Defines the type of the internal factory used within the cache.
		 * By specifying the factory type explicitly, the indirection of the std::function
		 * id avoided.
		 */
		typedef member_function_trait<const NodePtr (CachedNodeMapping::*)(const NodePtr&)>::type FactoryType;

		/**
		 * The cache to be used for reusing results.
		 */
		insieme::utils::cache::PointerCache<NodePtr, NodePtr, FactoryType> cache;

	  public:
		/**
		 * A default constructor initializing the factory method of the
		 * internally maintained cache.
		 */
		CachedNodeMapping() : cache(fun(*this, &CachedNodeMapping::resolveElement)){};

		/**
		 * The mapping function which is checking whether the given node has already been
		 * processed. If so, the result of the previous processing is returned. Otherwise,
		 * the method resolveElement(...) is used to obtain a replacement and returned.
		 *
		 * NOTE: should not be overridden by sub-class
		 */
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			// just look up content of cache (will be resolved automatically)
			return cache.get(ptr);
		}

		/**
		 * A pure virtual method to be implemented by sub-classes.
		 */
		virtual const NodePtr resolveElement(const NodePtr& ptr) = 0;

		/**
		 * Clears the internally maintained node resolution cache.
		 */
		void clearCache() {
			cache.clear();
		}

		/**
		 * Clears the cache entry associated to the given node pointer.
		 *
		 * @param ptr the node to be removed from the node cache.
		 */
		void clearCacheEntry(const NodePtr& ptr) {
			cache.clearEntry(ptr);
		}

	  protected:
		/**
		 * Updates the cache for the given key.
		 *
		 * @param key the node to be covered
		 * @param value the node to which the given key should be translated to
		 */
		void setCacheEntry(const NodePtr& key, const NodePtr& value) {
			cache.set(key, value);
		}
	};


	/**
	 * A utility class mapping a child list of a node using some other node mapping. After
	 * mapping all children, it verifies whether any modification has been applied.
	 */
	class ChildListMapping : public SimpleNodeMapping {
		/**
		 * The mapped list of children.
		 */
		vector<NodePtr> children;

		/**
		 * A flag indicating whether there has been any difference between the original
		 * and the mapped list of children.
		 */
		bool different;

	  public:
		/**
		 * Creates a new child list mapping based on the given child list and the given mapping.
		 * The represented list of replaced child nodes will be computed using the given list and mapping.
		 */
		ChildListMapping(const NodeList& list, SimpleNodeMapping& mapping)
		    : SimpleNodeMapping(), children(mapping.mapAll(list)), different(!equals(children, list)) {}

		/**
		 * Create a new child list mapping based on the given list of children. The optional boolean
		 * flag allows to specify whether this list of children is actually different from the original list
		 * of children.
		 *
		 * @param children the children to be offered as a substitute by this mapping
		 * @param different a flag allowing to determine whether the given list of children differs from the original child list
		 */
		ChildListMapping(const NodeList& children, bool different = true) : SimpleNodeMapping(), children(children), different(different) {}

		/**
		 * Determines whether this mapping would cause any modification when being applied
		 * to the child list it has been constructed for.
		 */
		bool isDifferent() const {
			return different;
		}

		/**
		 * Performs the actual mapping of a child node. The resulting node is identical to the
		 * node returned by the mapping passed to the constructor of this class.
		 *
		 * @param index the index of the child node to be mapped
		 * @param ptr the child to be mapped.
		 * @return the mapped child node.
		 */
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			return children[index];
		}
	};

	template <typename Lambda, typename Filter>
	class CachedLambdaNodeMapping : public CachedNodeMapping {
		Lambda lambda;
		Filter filter;
		bool mapTypes;

	  public:
		CachedLambdaNodeMapping(const Lambda& lambda, const Filter& filter, bool mapTypes) : lambda(lambda), filter(filter), mapTypes(mapTypes){};

		virtual const NodePtr resolveElement(const NodePtr& ptr) {
			if(!mapTypes && ptr->getNodeCategory() == NC_Type) { return ptr; }
			if(!filter(ptr)) { return ptr; }
			return lambda(ptr->substitute(ptr->getNodeManager(), *this));
		}
	};

	template <typename Lambda, typename Filter>
	CachedLambdaNodeMapping<Lambda, Filter> makeCachedLambdaMapper(const Lambda& lambda, const Filter& filter, bool mapTypes = false) {
		return CachedLambdaNodeMapping<Lambda, Filter>(lambda, filter, mapTypes);
	}

	template <typename Lambda>
	CachedLambdaNodeMapping<Lambda, AcceptAll<NodePtr>> makeCachedLambdaMapper(const Lambda& lambda, bool mapTypes = false) {
		return makeCachedLambdaMapper(lambda, AcceptAll<NodePtr>(), mapTypes);
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
