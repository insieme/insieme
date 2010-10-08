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

#include <vector>

#include "ast_node.h"
#include "hash_utils.h"

namespace insieme {
namespace core {

/**
 * This immutable value class can be used to address nodes within the AST. Since nodes within the AST are shared,
 * the same node may be reused at multiple locations within the AST. Hence, a simple pointer would be insufficient for
 * uniquely identifying an element of the syntax tree. To address such an element, the entire path, starting from an
 * arbitrary root node, has to be considered. Only this path allows to address particular sections within an AST.
 *
 * TODO: extend documentation with usage scenarios
 */
class NodeAddress : public utils::HashableImmutableData<NodeAddress> {

public:

	/**
	 * The type used to describe one step among the path addressing a node within the AST.
	 * The pair represents the index of the child to followed within the upper level and a pointer
	 * to the node reached by the this step.
	 *
	 * NOTE: The pointer only represents a 'cached' value and might as well be obtained by traversing
	 * the prefix of the path each time a node on the path has to be resolved.
	 * Due to this pointer, nodes on the path can be obtained with O(1).
	 */
	typedef std::pair<std::size_t, NodePtr> PathEntry;

	/**
	 * The type used to represent the path of a node to be addressed.
	 */
	typedef std::vector<PathEntry> Path;

private:

	/**
	 * The path is maintained via a shared pointer. This way, the rather large vector (which is
	 * also expensive to be copied) can be shared among all node addresses created by copying or
	 * assigning one instance to another. Since this class is immutable, sharing the path does
	 * not result in unexpected side effects.
	 */
	typedef std::shared_ptr<Path> SharedPath;

	/**
	 * The path used to identify the node referenced by this address.
	 * The pointer stored within this vector are describing the sequence of edges to be followed
	 * for reaching the corresponding node.
	 */
	SharedPath path;

public:

	/**
	 * A constructor creating an address for the given root node.
	 */
	NodeAddress(const NodePtr& root);

	/**
	 * A constructor creating a node address based on a path through an AST.
	 *
	 * @param path the path to the node to be addressed.
	 */
	NodeAddress(const Path& path = Path());

	/**
	 * Obtains a pointer to the root node this address is starting from.
	 *
	 * NOTE: nodes are not addressed in a unique way. Since nodes are shared, a single node might be accessible
	 * via multiple addresses.
	 *
	 * @return a pointer to the root node.
	 */
	NodePtr getRootNode() const;

	/**
	 * Obtains a pointer to the parent node of the given address (of an arbitrary higher level).
	 * An assertion error will occur in case the requested level is large or equal the depth of this address.
	 * (Hence, the corresponding parent node does not exist or is unknown).
	 *
	 * @param level the number of levels to go up (0=same node, 1=parent, 2=grandparents ...)
	 * @return the requested address
	 */
	NodePtr getParentNode(unsigned level=1) const;

	/**
	 * Obtains a pointer to the node referenced by this address.
	 *
	 * NOTE: nodes are not addressed in a unique way. Since nodes are shared, a single node might be accessible
	 * via multiple addresses.
	 *
	 * @return a pointer to the addressed node.
	 */
	NodePtr getAddressedNode() const;

	/**
	 * Obtains the address of the root node this address is starting from. Since it will only consist of a single
	 * element its parents will be unknown.
	 *
	 * @return the (relative) address of the root node
	 */
	NodeAddress getRootNodeAddress() const;

	/**
	 * Obtains the address of the parent node of the given address (of an arbitrary higher level).
	 * An assertion error will occur in case the requested level is large or equal the depth of this address.
	 * (Hence, the corresponding parent node does not exist or is unknown).
	 *
	 * @param level the number of levels to go up (0=same node, 1=parent, 2=grandparents ...)
	 * @return the requested address
	 */
	NodeAddress getParentNodeAddress(unsigned level=1) const;

	/**
	 * Obtains the address of a child node. It is extending the path maintained by this address by a single
	 * step, namely the node given by the index.
	 *
	 * @param index the index of the child-node to be addressed within the current nodes child list.
	 * @return the address of the child node
	 */
	NodeAddress getAddressOfChild(unsigned index) const;

	/**
	 * Checks whether this address is constituting a valid path within some AST. The method returns
	 * true in case the represented path can be reconstructed within the AST, hence if the (i+1)-th element
	 * within the path constituting this address is a pointer to a child node referenced by the i-th element.
	 *
	 * @return true if it is a valid path, false otherwise
	 */
	bool isValid() const;

	/**
	 * Obtains the depth / length of this address. The depth corresponds to the number of nodes passed
	 * between the root node and the addressed node.
	 *
	 * @return the depth of the addressed nodes within the AST
	 */
	unsigned getDepth() const {
		return path->size();
	}

	/**
	 * Obtains the entire path constituting this node address.
	 *
	 * @return a constant reference to the internally maintained path.
	 */
	const Path& getPath() const {
		return *path;
	}


	/**
	 * An implicit conversion to boolean. It will be converted to true, if the address is valid.
	 * Otherwise the result will be false.
	 *
	 * @return true if address is valid, false otherwise
	 */
	operator bool() const {
		return isValid();
	}

	/**
	 * Obtains a reference to the node addressed by this address (if valid). Otherwise an
	 * assertion will be violated.
	 *
	 * @return a reference to the addressed node.
	 */
	const Node& operator*() const {
		return *getAddressedNode();
	}

protected:

	/**
	 * Compares this instance with the given instance. Of both addresses represent
	 * the same path within an expression, both addresses will be considered equivalent.
	 *
	 * @param other The node address to be compared to.
	 * @return true if equivalent, false otherwise.
	 */
	virtual bool equals(const NodeAddress& other) const;

};

} // end namespace core
} // end namespace insieme

/**
 * Allows node addresses to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const insieme::core::NodeAddress& node);



