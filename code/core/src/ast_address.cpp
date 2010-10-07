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

#include "ast_address.h"
#include "container_utils.h"

namespace insieme {
namespace core {


NodeAddress::NodeAddress(const NodePtr& root)
	: HashableImmutableData(boost::hash_value(toVector(root))), path(toVector(root)) {}

NodeAddress::NodeAddress(const std::vector<NodePtr>& path) : HashableImmutableData(boost::hash_value(path)), path(path) {}

NodePtr NodeAddress::getRootNode() const {
	assert(!path.empty() && "Invalid node address!");
	return path[0]; // first element of path = root node
}

NodePtr NodeAddress::getParentNode(unsigned level) const {
	assert(path.size() > level && "Invalid parent node level!");
	return path[path.size() - level - 1];
}

NodePtr NodeAddress::getAddressedNode() const {
	assert(!path.empty() && "Invalid node address!");
	return path[path.size() - 1];
}

NodeAddress NodeAddress::getRootNodeAddress() const {
	return NodeAddress(getRootNode());
}

NodeAddress NodeAddress::getParentNodeAddress(unsigned level) const {
	assert(path.size() > level && "Invalid parent node level!");

	// check whether an actual parent is requested
	if (level == 0) {
		return *this;
	}

	// check whether root node is requested
	if (path.size() == level + 1) {
		return getRootNodeAddress();
	}

	// create modified path
	return NodeAddress(std::vector<NodePtr>(path.begin(), path.end()-level));
}

NodeAddress NodeAddress::getChildAddress(const NodePtr& child) const {
	// verify that it is really a child
	assert(::contains(getAddressedNode()->getChildList(), child) && "Given node is not a child!");

	// extend path by child element
	std::vector<NodePtr> newPath(path);
	newPath.push_back(child);
	return NodeAddress(newPath);
}

bool NodeAddress::isValid() const {
	// check whether path is not empty
	if (path.empty()) {
		return false;
	}

	// check parent-child relation
	NodePtr parent = *path.begin();
	return all(path.begin()+1, path.end(), [&parent](const NodePtr& cur) {
		// pointer must not be null and has to be within its parents child list
		bool res = cur && contains(parent->getChildList(), cur);
		parent = cur;
		return res;
	});
}

bool NodeAddress::equals(const NodeAddress& other) const {
	// just compare paths (identity, hash values and others stuff has already been checked by super class)
	return ::equals(path, other.path);
}


} // end namespace core
} // end namespace insieme
