/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_mapper.h"

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {
namespace transform {

namespace {
	// Virtual address "on top" of a given address
	// used to resolve address mapping startup, which would otherwise induce statefulness in the mapper
	class VirtualTopAddress : public NodeAddress {
	public:
		VirtualTopAddress(const NodeAddress& root) : NodeAddress(root) {
		}

		virtual NodeAddress getAddressOfChild(unsigned index) const override {
			return NodeAddress(*this);
		}
	};
}

/**
 * A mapper which supplies pre-mapping addresses to the user for each node
 */
class AddressMapping : public NodeMapping<const NodeAddress&> {
public:
	AddressMapping() {};
	
	/*
	 * Map the node ptr "ptr", which had the previous address "prevAddr" before mappign started.
	 * Needs to be defined in subclasses.
	 */
	virtual const NodePtr mapAddress(const NodePtr& ptr, const NodeAddress& prevAddr) = 0;

	/*
	 * Maps elements while keeping track of addresses. 
	 * Internal, should not be changed afterwards.
	 */
	virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr, const NodeAddress& topAddr) final override {
		if(topAddr->isValue()) return ptr->substitute(ptr->getNodeManager(), *this, topAddr);
		auto curAddr = topAddr.getAddressOfChild(index);
		return mapAddress(ptr, curAddr);
	}
	
	/*
	 * Starts mapping with addresses from a given root node.
	 */
	template<typename T>
	Pointer<T> mapFromRoot(const Pointer<T>& rootPtr) {
		auto root = VirtualTopAddress(NodeAddress(rootPtr));
		return NodeMapping<const NodeAddress&>::map(rootPtr, root);
	}
};


template<typename Lambda, typename Filter>
class LambdaAddressMapping : public AddressMapping {
	Lambda lambda;
	Filter filter;
	bool mapTypes;
public:
	LambdaAddressMapping(const Lambda& lambda, const Filter& filter, bool mapTypes)
		: lambda(lambda), filter(filter), mapTypes(mapTypes) { };

	virtual const NodePtr mapAddress(const NodePtr& ptr, const NodeAddress& prevAddr) override {
		return lambda(ptr->substitute(ptr->getNodeManager(), *this, prevAddr), prevAddr);
	}
};

template<typename Lambda, typename Filter>
LambdaAddressMapping<Lambda, Filter> makeLambdaAddressMapping(const Lambda& lambda, const Filter& filter, bool mapTypes = false) {
	return LambdaAddressMapping<Lambda,Filter>(lambda, filter, mapTypes);
}

template<typename Lambda>
LambdaAddressMapping<Lambda, AcceptAll<NodePtr>> makeLambdaAddressMapping(const Lambda& lambda, bool mapTypes = false) {
	return makeLambdaAddressMapping(lambda, AcceptAll<NodePtr>(), mapTypes);
}

} // end namespace transform
} // end namespace core
} // end namespace insieme
