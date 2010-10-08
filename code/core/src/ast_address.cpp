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
#include "string_utils.h"

namespace insieme {
namespace core {

/**
 * Computes a hash code for the given path.
 */
std::size_t hashPath(const NodeAddress::Path& path) {
	std::size_t seed = 0;
	for_each(path, [&seed](const NodeAddress::PathEntry& cur) {
		boost::hash_combine(seed, cur.first);
	});
	return seed;
}

NodeAddress::Path toPath(const NodePtr& root) {
	return toVector(NodeAddress::PathEntry(0,root));
}


NodeAddress::NodeAddress(const NodePtr& root)
	: HashableImmutableData(hashPath(toPath(root))), path(std::make_shared<Path>(toPath(root))) {}

NodeAddress::NodeAddress(const Path& path) : HashableImmutableData(hashPath(path)), path(std::make_shared<Path>(path)) {}

NodePtr NodeAddress::getRootNode() const {
	assert(!path->empty() && "Invalid node address!");
	return (*path)[0].second; // the pointer assigned to the first path element
}

NodePtr NodeAddress::getParentNode(unsigned level) const {
	std::size_t size = path->size();
	assert(size > level && "Invalid parent node level!");
	return (*path)[size - level - 1].second;
}

NodePtr NodeAddress::getAddressedNode() const {
	assert(!path->empty() && "Invalid node address!");
	return (*path)[path->size() - 1].second;
}

NodeAddress NodeAddress::getRootNodeAddress() const {
	return NodeAddress(getRootNode());
}

NodeAddress NodeAddress::getParentNodeAddress(unsigned level) const {
	assert(path->size() > level && "Invalid parent node level!");

	// check whether an actual parent is requested
	if (level == 0) {
		return *this;
	}

	// check whether root node is requested
	if (path->size() == level + 1) {
		return getRootNodeAddress();
	}

	// create modified path
	return NodeAddress(Path(path->begin(), path->end()-level));
}

NodeAddress NodeAddress::getAddressOfChild(unsigned index) const {

	// verify that it is really a child
	assert((getAddressedNode()->getChildList().size() > index) && "Given node index is not a child!");

	NodePtr child = getAddressedNode()->getChildList()[index];

	// extend path by child element
	Path newPath(*path);
	newPath.push_back(std::make_pair(index, child));
	return NodeAddress(newPath);
}

bool NodeAddress::isValid() const {
	// check whether path is not empty
	if (path->empty()) {
		return false;
	}

	// check parent-child relation
	PathEntry parent = *(*path).begin();
	return all((*path).begin()+1, (*path).end(), [&parent](const PathEntry& cur) {

		bool res =
				cur.second && // pointer must be not NULL
				parent.second->getChildList().size() > cur.first && // index must be valid
				parent.second->getChildList()[cur.first] == cur.second; // pointer must be correct

		parent = cur;
		return res;
	});
}

bool NodeAddress::equals(const NodeAddress& other) const {
	// just compare paths (identity, hash values and others stuff has already been checked by super class)
std::cout << "Comparing " << *this << other << std::endl;
	return ::equals(*path, *other.path, [](const PathEntry& a, const PathEntry& b) {
std::cout << "Checking " << a.first << "==" << b.first << std::endl;
		return a.first == b.first;
	});
}

} // end namespace core
} // end namespace insieme


std::ostream& operator<<(std::ostream& out, const insieme::core::NodeAddress& node) {
	return out << join("-", node.getPath(), [](std::ostream& out, const insieme::core::NodeAddress::PathEntry& cur) {
		out << cur.first;
	});
}

