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

#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/ast_builder.h"
#include "insieme/core/ast_address.h"

#include "insieme/utils/container_utils.h"

namespace {

using namespace insieme::core;
using namespace insieme::core::transform;

using namespace insieme::utils::map;

/**
 * Visitor which replace a specific node of the IR starting from a root node.
 */
class NodeReplacer : public NodeMapping {
	NodeManager& manager;
	const PointerMap<NodePtr, NodePtr>& replacements;
	const bool preservePtrAnnotationsWhenModified;
	const bool includesTypes;

public:

	NodeReplacer(NodeManager& manager, const PointerMap<NodePtr, NodePtr>& replacements, bool preservePtrAnnotationsWhenModified)
		: manager(manager), replacements(replacements), preservePtrAnnotationsWhenModified(preservePtrAnnotationsWhenModified),
		  includesTypes(any(replacements, [](const std::pair<NodePtr, NodePtr>& cur) { return cur.first->getNodeCategory() == NC_Type; })) { }

private:

	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
		// check whether the element has been found
		auto pos = replacements.find(ptr);
		if(pos != replacements.end()) {
			return pos->second;
		}

		// if element to be replaced is a not a type but the current node is,
		// the recursion can be pruned (since types only have other types as
		// sub-nodes)
		if (!includesTypes && ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// recursive replacement has to be continued
		NodePtr res = ptr->substitute(manager, *this);

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed
			return ptr;
		}

		// restore annotations if requested
		if (preservePtrAnnotationsWhenModified) {
			res->setAnnotations(ptr->getAnnotations());
		}

		// done
		return res;
	}
};

/**
 * Visitor which replace a specific node of the IR starting from a root node.
 */
class SingleNodeReplacer : public NodeMapping {
	NodeManager& manager;
	const NodePtr& target;
	const NodePtr& replacement;
	const bool preservePtrAnnotationsWhenModified;
	const bool targetIsType;

public:

	SingleNodeReplacer(NodeManager& manager, const NodePtr& target, const NodePtr& replacement, bool preservePtrAnnotationsWhenModified)
		: manager(manager), target(target), replacement(replacement), preservePtrAnnotationsWhenModified(preservePtrAnnotationsWhenModified), targetIsType(target->getNodeCategory()==NC_Type) { }

private:

	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {

		// handle replacement
		if (*ptr == *target) {
			return replacement;
		}

		// prune types if possible
		if (!targetIsType && ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// recursive replacement has to be continued
		NodePtr res = ptr->substitute(manager, *this);

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed
			return ptr;
		}

		// restore annotations if requested
		if (preservePtrAnnotationsWhenModified) {
			res->setAnnotations(ptr->getAnnotations());
		}

		// done
		return res;
	}
};


class NodeAddressReplacer : public NodeMapping {
	const unsigned indexToReplace;
	const NodePtr& replacement;

	public:

		NodeAddressReplacer(unsigned index, const NodePtr& replacement)
			: indexToReplace(index), replacement(replacement) { }

	private:

		/**
		 * Represents an identity-operation except for the one element to be replaced,
		 * which is identified by its index.
		 *
		 * @param index the index of the element to be mapped
		 * @param ptr a pointer to the element to be mapped
		 * @return a pointer to the mapped element
		 */
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			if (indexToReplace == index) {
				return replacement;
			}
			return ptr;
		}

};

class VariableReplacer : public NodeMapping {

	NodeManager& manager;
	const VariablePtr variable;
	const NodePtr replacement;
	const bool preservePtrAnnotationsWhenModified;


public:

	VariableReplacer(NodeManager& manager, const VariablePtr& variable, const NodePtr& replacement, bool preservePtrAnnotationsWhenModified)
		: manager(manager), variable(variable), replacement(replacement), preservePtrAnnotationsWhenModified(preservePtrAnnotationsWhenModified) { }

private:
	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
		// check whether the element has been found
		if (*ptr == *variable) {
			return replacement;
		}

		// shortcut for types => will never be changed
		if (ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// handle scope limiting elements
		switch(ptr->getNodeType()) {
		case NT_JobExpr:
		case NT_LambdaExpr:
			// enters a new scope => variable will no longer occur
			return ptr;
		default: { }
		}

		// recursive replacement has to be continued
		NodePtr res = ptr->substitute(manager, *this);

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed
			return ptr;
		}

		// restore annotations if requested
		if (preservePtrAnnotationsWhenModified) {
			res->setAnnotations(ptr->getAnnotations());
		}

		// done
		return res;
	}
};

}

namespace insieme {
namespace core {
namespace transform {

NodePtr applyReplacer(NodeManager& mgr, const NodePtr& root, NodeMapping& mapper, bool preservePtrAnnotationsWhenModified) {
	if(!root) {
		return NodePtr(NULL);
	}


	NodePtr res = root->substitute(mgr, mapper);

	// check whether something has changed
	if (res == root) {
		// nothing changed => return handed in node
		return root;
	}

	// if annotations should be preserved anyway ...
	if (preservePtrAnnotationsWhenModified) {
		// ... restore annotations.
		res->setAnnotations(root->getAnnotations());
	}

	return res;
}


NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const PointerMap<NodePtr, NodePtr>& replacements, bool preservePtrAnnotationsWhenModified) {

	// shortcut for empty replacements
	if (replacements.empty()) {
		return root;
	}

	// handle single element case
	if (replacements.size() == 1) {
		auto pair = *replacements.begin();
		return replaceAll(mgr, root, pair.first, pair.second, preservePtrAnnotationsWhenModified);
	}

	// handle entire map
	auto mapper = ::NodeReplacer(mgr, replacements, preservePtrAnnotationsWhenModified);
	return applyReplacer(mgr, root, mapper, preservePtrAnnotationsWhenModified);
}

NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const NodePtr& toReplace, const NodePtr& replacement, bool preservePtrAnnotationsWhenModified) {

	if (toReplace->getNodeType() == NT_Variable) {
		return replaceAll(mgr, root, static_pointer_cast<const Variable>(toReplace), replacement, preservePtrAnnotationsWhenModified);
	}

	auto mapper = ::SingleNodeReplacer(mgr, toReplace, replacement, preservePtrAnnotationsWhenModified);
	return applyReplacer(mgr, root, mapper, preservePtrAnnotationsWhenModified);
}

NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const VariablePtr& toReplace, const NodePtr& replacement, bool preservePtrAnnotationsWhenModified) {
	auto mapper = ::VariableReplacer(mgr, toReplace, replacement, preservePtrAnnotationsWhenModified);
	return applyReplacer(mgr, root, mapper, preservePtrAnnotationsWhenModified);
}


NodePtr replaceNode(NodeManager& manager, const NodeAddress& toReplace, const NodePtr& replacement, bool preservePtrAnnotationsWhenModified) {
	assert( toReplace.isValid() && "Invalid node address provided!");

	// create result
	NodePtr res = replacement;

	// process the path bottom up => replace one node after another
	const Path& path = toReplace.getPath();
	unsigned lastPos = path[path.size()-1].first;
	std::for_each(path.rbegin()+1, path.rend(), [&](const PathEntry& cur) {
		// conduct replace operation
		auto mapper = NodeAddressReplacer(lastPos, res);
		res = cur.second->substitute(manager, mapper);

		// restore annotations
		if (preservePtrAnnotationsWhenModified) {
			// copy annotations ...
			// TODO: determine whether annotations should be merged or overridden
			res->setAnnotations(cur.second->getAnnotations());
		}

		// update last-pos
		lastPos = cur.first;
	});

	// done
	return res;
}


} // End transform namespace
} // End core namespace
} // End insieme namespace
