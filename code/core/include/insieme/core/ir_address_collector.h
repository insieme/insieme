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

#include <vector>

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"


namespace insieme {
namespace core {

	// ----------------------------------------------------------------------------------------------------------------
	//											interface declarations
	// ----------------------------------------------------------------------------------------------------------------


	/**
	 * A utility to collect all the addresses of child nodes of the given root node
	 * satisfying the given filter property.
	 *
	 * The handled filter is invoked for each node through the interface
	 *
	 *		filter( node , collection )
	 *
	 * where node is a NodePtr referencing the current node, and collection a mutable reference
	 * to the list of resulting addresses. The filter shell return Action::Descent or Action::Prune
	 * depending on whether child nodes shall be further investigated.
	 *
	 * @param root the root node from where to start the search
	 * @param filter the filter predicate to be applied.
	 */
	template<typename Node = Node, typename NodeFilter>
	std::vector<Address<const Node>> collectAllAddresses(const NodeAddress& root, const NodeFilter& filter);



	// ----------------------------------------------------------------------------------------------------------------
	//												definitions
	// ----------------------------------------------------------------------------------------------------------------

	namespace detail {


		// A utility class for the effective storage of collections of addresses.
		class AddressCollection {

			// a marker that this node is included in the set
			bool thisNode;

			// a list of children, compressed
			std::vector<std::pair<std::size_t,const AddressCollection*>> children;

			// a list of children, explicit
			std::vector<NodeAddress> list;

		public:

			// creates a new, empty collection
			AddressCollection() : thisNode(false) {}

			bool empty() const {
				return !thisNode && children.empty() && list.empty();
			}

			// inserts a new address
			void insert(const NodeAddress& cur) {

				// roots are easy
				if (cur.isRoot()) {
					thisNode = true;
					return;
				}

				// in all other cases, we have to add it explicitly
				list.push_back(cur);
			}

			void insert(std::size_t child, const AddressCollection& collection) {
				if (collection.empty()) return;
				children.push_back({child,&collection});
			}

			// runs an operation on each node in the set
			template<typename Operator>
			void forEach(const NodePtr& root, const Operator& op) const {
				forEach(NodeAddress(root),op);
			}

		private:

			template<typename Operator>
			void forEach(const NodeAddress& head, Operator& op) const {
				if (thisNode) op(head);
				for(const auto& cur : children) {
					cur.second->forEach(head.getAddressOfChild(cur.first),op);
				}
				for(const auto& cur : list) {
					op(concat(head,cur));
				}
			}

		};

		template<typename Filter>
		const AddressCollection& collectAllAddressesInternal(const Filter& filter, const NodePtr& cur, std::map<NodePtr,AddressCollection>& cache) {

			// check cache
			auto pos = cache.find(cur);
			if (pos != cache.end()) return pos->second;

			// create result-collection for this node
			auto& result = cache[cur];

			// check the filter
			auto action = filter(cur,result);

			// if the filter decides to stop where => do that
			if (action == Action::Prune) return result;

			// otherwise: descent into child nodes
			std::size_t i = 0;
			for(const auto& child : cur->getChildList()) {
				result.insert(i,collectAllAddressesInternal(filter,child,cache));
				i++;
			}

			// done
			return result;
		}

		/**
		 * Collects a list of addresses pointing to nested records within the given node.
		 */
		template<typename AddressType, typename Filter>
		std::vector<AddressType> collectAllAddresses(const NodePtr& root, const Filter& filter) {
			// run everything through an internal, cached collector
			std::map<NodePtr,AddressCollection> cache;
			auto& collection = collectAllAddressesInternal(filter, root, cache);

			// convert the collection to a list of records
			std::vector<AddressType> res;
			collection.forEach(root,[&](const NodeAddress& cur){
				res.push_back(cur.as<AddressType>());
			});

			// move out the result
			return res;
		}

	}

	template<typename Node, typename NodeFilter>
	std::vector<Address<const Node>> collectAllAddresses(const NodeAddress& root, const NodeFilter& filter) {
		return detail::collectAllAddresses<Address<const Node>>(root,filter);
	}


} // end namespace core
} // end namespace insieme


