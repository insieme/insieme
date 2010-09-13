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

#include <cassert>

#include "annotated_ptr.h"
#include "hash_utils.h"
#include "instance_manager.h"
#include "string_utils.h"

namespace insieme {
namespace core {

/**
 * Defines a macro to forward declare AST node types. Each node type
 * is defined by a class. Further, for each node type a pointer type
 * is defined, which might be used to reference instances of the
 * those types.
 */
#define DECLARE_NODE_TYPE(NAME) \
	class NAME; \
	typedef AnnotatedPtr<const NAME> NAME ## Ptr;


/**
 * Define root-node type.
 */
DECLARE_NODE_TYPE(Node);


class NodeManager;

/**
 * An enumeration of the fundamental types of nodes
 * to be present within an AST.
 */
enum NodeType {
	SUPPORT,		/* < to represent shared values */
	TYPE, 			/* < to represent a (data) type  */
	EXPRESSION,		/* < to represent expressions */
	STATEMENT, 		/* < to represent statements */
	DEFINITION,		/* < to represent definitions */
	PROGRAM			/* < to represent entire programs */
};

inline bool isNodeType(const NodeType& value) {
	return SUPPORT <= value && value <= PROGRAM;
}

/**
 * This class models an abstract base class for all AST nodes to be used within a program
 * representation. It defines a minimum number of functionality to be supported by all nodes
 * (including to be hash- and comparable, such that instances can be used within unordered
 * sets).
 */
class Node : public insieme::utils::HashableImmutableData<Node>,  public Annotatable {

	/**
	 * Allow the instance manager to access the private clone method.
	 */
	friend class InstanceManager<Node, AnnotatedPtr>;

public:

	/**
	 * The type of instance manager to be used with this type.
	 */
	typedef NodeManager Manager;

	/**
	 * The type used to represent the list of children of a node.
	 */
	typedef vector<NodePtr> ChildList;

	/**
	 * A type used to represent a optionally available child list.
	 */
	typedef std::shared_ptr<ChildList> OptionChildList;

	/**
	 * Allow the test case to access private methods.
	 */
	template<typename PT>
	friend void basicNodeTests(PT, const ChildList& children = ChildList());

private:

	/**
	 * The type of node to be represented by this instance.
	 */
	const NodeType nodeType;

	/**
	 * The list of child nodes referenced by this node.
	 */
	mutable OptionChildList children;

	/**
	 * Retrieves a clone of this node, hence a newly allocated instance representing the same value
	 * as the current instance. The new instance (and all its referenced nodes) should be maintained
	 * by the given manager.
	 *
	 * @param manager the manager which should maintain all referenced nodes
	 * @return a clone of this instance referencing elements maintained exclusively by the given manager
	 */
	virtual Node* clone(NodeManager& manager) const = 0;

protected:

	/**
	 * Construct a new node instance based on the essential features.
	 *
	 * @param nodeType the type of node to be created
	 * @param hashCode the hash code of the new node
	 */
	Node(const NodeType& nodeType, const std::size_t& hashCode) : HashableImmutableData(hashCode), nodeType(nodeType) {
		assert(isNodeType(nodeType) && "Given Node type is not valid!");
	}

	/**
	 * Requests a list of child nodes from the actual node implementation.
	 */
	virtual OptionChildList getChildNodes() const = 0;

public:

	/**
	 * The default virtual destructor enforcing the proper destruction of derived
	 * instances.
	 */
	virtual ~Node() {};

	/**
	 * Retrieves a reference to the internally maintained list of child nodes. The
	 * retrieved list is valid as long as this node exists. Hence, during its destruction,
	 * the returned list will also be eliminated.
	 *
	 * @return a reference to the internally maintained list of child nodes.
	 */
	const ChildList& getChildList() const;

	/**
	 * This pure abstract method is imposing the requirement to every node to
	 * be printable to an output stream.
	 *
	 * @param out the stream to be printed to
	 * @return the output stream to print further information (allows to concatenate print operations)
	 */
	virtual std::ostream& printTo(std::ostream& out) const = 0;

	/**
	 * Provides a string representation of this node, which is by default
	 * the same as would be printed in case the object is written into an
	 * output stream.
	 *
	 * @return a string representation for this node instance
	 */
	virtual string toString() const {
		return ::toString(*this);
	}

	/**
	 * Obtains the fundamental type of this node.
	 *
	 * @return the type of node represented by this instance
	 *
	 * @see NodeType
	 */
	NodeType getNodeType() const {
		return nodeType;
	}

	/**
	 * A default implementation of the equals operator comparing the actual
	 * names of the types.
	 */
	bool operator==(const Node& other) const {
		// test for identity
		if (this == &other) {
			return true;
		}

		// fast hash code test
		if (hashCode != other.hashCode) {
			return false;
		}

		// check node type
		if (nodeType != other.nodeType) {
			return false;
		}

		// use virtual equals method
		return equals(other);
	}

};

/**
 * Implements a node manager to be used for maintaining AST node instances.
 */
class NodeManager : public InstanceManager<Node, AnnotatedPtr> {};
typedef std::shared_ptr<NodeManager> SharedNodeManager;

} // end namespace core
} // end namespace insieme

/**
 * Allows nodes to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const insieme::core::Node& node);

