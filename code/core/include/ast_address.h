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
#include <algorithm>

#include <boost/type_traits/is_base_of.hpp>
#include <boost/utility/enable_if.hpp>

#include "ast_node.h"
#include "hash_utils.h"

namespace insieme {
namespace core {


// --- Forward declarations: ---

// declare address type
template<typename T> class Address;

// Add type definitions for node types (just like for node pointer)
#define NODE(name) \
		class name; \
		typedef Address<const name> name ## Address;
	#include "ast_nodes.def"
#undef NODE

// forward declaration
struct StaticAddressCast;

// TODO: encapsulate path in an actual object

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

/**
 * The path is maintained via a shared pointer. This way, the rather large vector (which is
 * also expensive to be copied) can be shared among all node addresses created by copying or
 * assigning one instance to another. Since this class is immutable, sharing the path does
 * not result in unexpected side effects.
 */
typedef std::shared_ptr<Path> SharedPath;

/**
 * Computes a hash code for the given path.
 *
 * @param path the path to be hashed
 * @return the resulting hash code
 */
std::size_t hashPath(const Path& path);

/**
 * Converts the given single node to a path.
 *
 * @param node the node the new path should consist of
 * @return a path consisting of this node only
 */
Path toPath(const NodePtr& node);

/**
 * This immutable value class can be used to address nodes within the AST. Since nodes within the AST are shared,
 * the same node may be reused at multiple locations within the AST. Hence, a simple pointer would be insufficient for
 * uniquely identifying an element of the syntax tree. To address such an element, the entire path, starting from an
 * arbitrary root node, has to be considered. Only this path allows to address particular sections within an AST.
 *
 * TODO: extend documentation with usage scenarios
 */
template<typename T>
class Address : public utils::HashableImmutableData<Address<T>> {

public:

	/**
	 * Defines a functor representing a static cast operator for this type.
	 */
	typedef StaticAddressCast StaticCast;

	/**
	 * Defines the type of the node this address is pointing to.
	 */
	typedef T element_type;

private:

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
	template<typename B>
	Address(const AnnotatedPtr<B>& root, typename boost::enable_if<boost::is_base_of<T,B>,int>::type = 0)
		: utils::HashableImmutableData<Address<T>>(hashPath(toPath(root))),
		  path(std::make_shared<Path>(toPath(root))) {}

	/**
	 * A constructor creating a node address based on a path through an AST.
	 *
	 * @param path the path to the node to be addressed.
	 */
	Address(const Path& path = Path())
		: utils::HashableImmutableData<Address<T>>(hashPath(path)),
		  path(std::make_shared<Path>(path)) {}

	/**
	 * A extendes version of the copy constructor allowing to copy from addresses pointing
	 * to related types.
	 *
	 * @param from the element to be copied
	 */
	template<typename B>
	Address(const Address<B>& from, typename boost::enable_if<boost::is_base_of<T,B>,int>::type = 0)
		: utils::HashableImmutableData<Address<T>>(from.hash()),
		  path(from.getSharedPath()) {}

	/**
	 * Obtains a pointer to the root node this address is starting from.
	 *
	 * NOTE: nodes are not addressed in a unique way. Since nodes are shared, a single node might be accessible
	 * via multiple addresses.
	 *
	 * @return a pointer to the root node.
	 */
	NodePtr getRootNode() const {
		assert(!path->empty() && "Invalid node address!");
		// root = the pointer assigned to the first path element
		return (*path)[0].second;
	}

	/**
	 * Obtains a pointer to the parent node of the given address (of an arbitrary higher level).
	 * An assertion error will occur in case the requested level is large or equal the depth of this address.
	 * (Hence, the corresponding parent node does not exist or is unknown).
	 *
	 * @param level the number of levels to go up (0=same node, 1=parent, 2=grandparents ...)
	 * @return the requested address
	 */
	NodePtr getParentNode(unsigned level=1) const {
		std::size_t size = path->size();
		assert(size > level && "Invalid parent node level!");
		return (*path)[size - level - 1].second;
	}

	/**
	 * Obtains a pointer to the node referenced by this address.
	 *
	 * NOTE: nodes are not addressed in a unique way. Since nodes are shared, a single node might be accessible
	 * via multiple addresses.
	 *
	 * @return a pointer to the addressed node.
	 */
	AnnotatedPtr<const T> getAddressedNode() const {
		assert(!path->empty() && "Invalid node address!");
		assert(dynamic_pointer_cast<const T>((*path)[path->size() - 1].second) && "Illegal Node-Pointer cast!");
		return static_pointer_cast<const T>((*path)[path->size() - 1].second);
	}

	/**
	 * Obtains the address of the root node this address is starting from. Since it will only consist of a single
	 * element its parents will be unknown.
	 *
	 * @return the (relative) address of the root node
	 */
	NodeAddress getRootNodeAddress() const {
		return NodeAddress(getRootNode());
	}

	/**
	 * Obtains the address of the parent node of the given address (of an arbitrary higher level).
	 * An assertion error will occur in case the requested level is large or equal the depth of this address.
	 * (Hence, the corresponding parent node does not exist or is unknown).
	 *
	 * @param level the number of levels to go up (0=same node, 1=parent, 2=grandparents ...)
	 * @return the requested address
	 */
	NodeAddress getParentNodeAddress(unsigned level=1) const {
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

	/**
	 * Obtains the address of a child node. It is extending the path maintained by this address by a single
	 * step, namely the node given by the index.
	 *
	 * @param index the index of the child-node to be addressed within the current nodes child list.
	 * @return the address of the child node
	 */
	NodeAddress getAddressOfChild(unsigned index) const {

		// verify that it is really a child
		assert((getAddressedNode()->getChildList().size() > index) && "Given node index is not a child!");

		NodePtr child = getAddressedNode()->getChildList()[index];

		// extend path by child element
		Path newPath(*path);
		newPath.push_back(std::make_pair(index, child));
		return NodeAddress(newPath);
	}

	/**
	 * Checks whether this address is constituting a valid path within some AST. The method returns
	 * true in case the represented path can be reconstructed within the AST, hence if the (i+1)-th element
	 * within the path constituting this address is a pointer to a child node referenced by the i-th element.
	 *
	 * @return true if it is a valid path, false otherwise
	 */
	bool isValid() const {
		// check whether path is not empty
		if (path->empty()) {
			return false;
		}

		// check parent-child relation
		PathEntry parent = *(*path).begin();
		return all((*path).begin()+1, (*path).end(), [&parent](const PathEntry& cur) -> bool {

			bool res =
					cur.second && // pointer must be not NULL
					parent.second->getChildList().size() > cur.first && // index must be valid
					parent.second->getChildList()[cur.first] == cur.second; // pointer must be correct

			parent = cur;
			return res;
		});
	}

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
		return *(path);
	}

	/**
	 * Obtains a reference to the internally shared path.
	 */
	const SharedPath& getSharedPath() const {
		return path;
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
	const T& operator*() const {
		return *getAddressedNode();
	}

	const T* operator->() const {
		return &*getAddressedNode();
	}

	/**
	 * Realizes an order on address according to their lexicographical order.
	 *
	 * @param other the address to be compared to
	 * @return true if this address is lexicographical smaller than the given address
	 */
	template<typename S>
	bool operator<(const Address<S>& other) const {
		// get size of paths
		std::size_t sizeA = path->size();
		std::size_t sizeB = other.path->size();

		// compare common prefix
		int length = std::min(sizeA, sizeB);
		Path& pathA = *path;
		Path& pathB = (*other.path);
		for(int i=0; i<length; i++) {
			std::size_t a = pathA[i].first;
			std::size_t b = pathB[i].first;
			if (a != b) {
				return a < b;
			}
		}

		// longer is bigger
		return sizeA < sizeB;
	}

protected:

	/**
	 * Compares this instance with the given instance. Of both addresses represent
	 * the same path within an expression, both addresses will be considered equivalent.
	 *
	 * @param other The node address to be compared to.
	 * @return true if equivalent, false otherwise.
	 */
	virtual bool equals(const Address<T>& other) const {
		// just compare paths (identity, hash values and others stuff has already been checked by super class)
		return ::equals(*path, other.getPath(), [](const PathEntry& a, const PathEntry& b) {
			return a.first == b.first;
		});
	}

};

/**
 * Allows to dynamically down-cast between addresses pointing to related types.
 *
 * @tparam B the type the resulting pointer should point to
 * @tparam T the type the given pointer is pointing to
 * @param src the pointer to be down-casted
 * @return the down-casted address pointing to the same location
 */
template<typename B, typename T>
inline typename boost::enable_if<boost::is_base_of<T,B>, Address<B>>::type dynamic_address_cast(Address<T>& src) {
	if (dynamic_cast<B*>(&(*src))) {
		return *(reinterpret_cast<Address<B>* >(&src));
	}
	// return an invalid address (default constructed)
	return Address<B>();
}

/**
 * Allows to statically down-cast between addresses pointing to related types. Unlike for the dynamic cast, no runtime
 * checks will be conducted.
 *
 * @tparam B the type the resulting pointer should point to
 * @tparam T the type the given pointer is pointing to
 * @param src the pointer to be down-casted
 * @return the down-casted address pointing to the same location
 */
template<typename B, typename T>
inline typename boost::enable_if<boost::is_base_of<T,B>, Address<B>&>::type static_address_cast(Address<T>& src) {
	return reinterpret_cast<Address<B>&>(src);
}

template<typename B, typename T>
inline typename boost::enable_if<boost::is_base_of<T,B>, const Address<B>&>::type static_address_cast(const Address<T>& src) {
	return reinterpret_cast<const Address<B>&>(src);
}

/**
 * A template version for a functor performing static address casts on node addresses.
 * The purpose of this struct is to allow the static_address_cast function to be defined as
 * a pointer conversion function required as a template parameter of the AST Visitor class.
 */
struct StaticAddressCast {
	template<typename Target, typename Source>
	const Address<const Target>& operator()(const Address<const Source>& value) const {
		return static_address_cast<const Target>(value);
	}
};

} // end namespace core
} // end namespace insieme

/**
 * Allows node addresses to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
template<typename T>
std::ostream& operator<<(std::ostream& out, const insieme::core::Address<T>& node) {
	return out << join("-", node.getPath(), [](std::ostream& out, const insieme::core::PathEntry& cur) {
		out << cur.first;
	});
}



