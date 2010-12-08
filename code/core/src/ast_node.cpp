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

#include "insieme/core/ast_node.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/utils/container_utils.h"

// ---------------------------------------------- Utility Functions ------------------------------------

using namespace insieme::core;


namespace insieme {
namespace core {

IntTypeParam NodeMapping::mapParam(const IntTypeParam& param) {
	return param;
}

vector<IntTypeParam> NodeMapping::mapParam(const vector<IntTypeParam>& list) {
	// check whether there are manipulations
	if (!manipulatesIntTypeParameter) {
		return list;
	}

	// apply transformation
	return ::transform(list, [&](const IntTypeParam& cur) {
		return this->mapParam(cur);
	});
}

const Node::ChildList& Node::getChildList() const {
	if (!children) {
		children = getChildNodes();
	}
	return *children;
}

NodePtr Node::substitute(NodeManager& manager, NodeMapping& mapper) const {

	// use child list mapper to check for changes
	transform::ChildListMapping listMapper(getChildList(), mapper);
	if (!listMapper.isDifferent()) {
		return NodePtr(this);
	}

	// create a version having everything substituted
	Node* node = createCopyUsing(listMapper);

	// obtain element within the manager
	NodePtr res = manager.get(node);

	// free temporary instance
	delete node;

	// return instance maintained within manager
	return res;
}

void* Node::operator new(size_t size) {
	return ::operator new(size);
}

void Node::operator delete(void* ptr) {
	return ::operator delete(ptr);
}


bool equalsWithAnnotations(const NodePtr& nodeA, const NodePtr& nodeB) {

	// check identity (under-approximation)
	if (nodeA == nodeB) {
		return true;
	}

	// check structure (over-approximation)
	if (*nodeA!=*nodeB) {
		return false;
	}

	// check annotations of pointer and nodes ...
	if (!hasSameAnnotations(*nodeA, *nodeB)) {
		return false;
	}

	// check annotations of references
	auto listA = nodeA->getChildList();
	auto listB = nodeB->getChildList();
	return all(
			make_paired_iterator(listA.begin(), listB.begin()),
			make_paired_iterator(listA.end(), listB.end()),

			[](const std::pair<NodePtr, NodePtr>& cur) {

		// make a recursive call
		return equalsWithAnnotations(cur.first, cur.second);
	});
}

} // end namespace core
} // end namespace insieme

/**
 * Allows this type to be printed to a stream (especially useful during debugging and
 * within test cases where equals values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const Node& node) {
	return node.printTo(out);
}
