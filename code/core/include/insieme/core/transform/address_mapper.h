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

namespace insieme {
namespace core {
namespace transform {


	/**
	 * A mapper which supplies pre-mapping addresses to the user for each node
	 */
	class AddressMapping : public NodeMapping<NodeAddress> {
	  public:
		AddressMapping(){};

		/*
		 * Map the node ptr "ptr", which had the previous address "prevAddr" before mappign started.
		 * Needs to be defined in subclasses.
		 */
		virtual const NodePtr mapAddress(const NodePtr& ptr, NodeAddress& prevAddr) = 0;

		/*
		 * Maps elements while keeping track of addresses.
		 * Internal, should not be changed afterwards.
		 */
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr, NodeAddress& topAddr) final override {
			if(topAddr && topAddr->isValue()) { return ptr->substitute(ptr->getNodeManager(), *this, topAddr); }
			auto curAddr = (topAddr) ? topAddr.getAddressOfChild(index) : NodeAddress(ptr);
			return mapAddress(ptr, curAddr);
		}

		/*
		 * Starts mapping with addresses from a given root node.
		 */
		template <typename T>
		Pointer<T> mapFromRoot(const Pointer<T>& rootPtr) {
			NodeAddress ctxt;
			return NodeMapping<NodeAddress>::map(rootPtr, ctxt);
		}
	};


	template <typename Lambda, typename Filter>
	class LambdaAddressMapping : public AddressMapping {
		Lambda lambda;
		Filter filter;
		bool mapTypes;

	  public:
		LambdaAddressMapping(const Lambda& lambda, const Filter& filter, bool mapTypes) : lambda(lambda), filter(filter), mapTypes(mapTypes){};

		virtual const NodePtr mapAddress(const NodePtr& ptr, NodeAddress& prevAddr) override {
			return lambda(ptr->substitute(ptr->getNodeManager(), *this, prevAddr), prevAddr);
		}
	};

	template <typename Lambda, typename Filter>
	LambdaAddressMapping<Lambda, Filter> makeLambdaAddressMapping(const Lambda& lambda, const Filter& filter, bool mapTypes = false) {
		return LambdaAddressMapping<Lambda, Filter>(lambda, filter, mapTypes);
	}

	template <typename Lambda>
	LambdaAddressMapping<Lambda, AcceptAll<NodePtr>> makeLambdaAddressMapping(const Lambda& lambda, bool mapTypes = false) {
		return makeLambdaAddressMapping(lambda, AcceptAll<NodePtr>(), mapTypes);
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
