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
#include <boost/type_traits/remove_const.hpp>
#include <boost/utility/enable_if.hpp>

#include "insieme/utils/hash_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {

/**
 * A forward declaration of the node type used for realizing paths.
 */
namespace detail {
	class PathElement;
}

// TODO: merge this with the address

/**
 * The type used to represent the path of a node to be addressed.
 */
class Path {

	/**
	 * The last element of the path represented by this class. The elements
	 * are linked bottom up and the root element is marked by referencing null as its
	 * parent.
	 */
	const detail::PathElement* element;

	/**
	 * A private constructor to create a path based on a path element.
	 *
	 * @param element the element this path is pointing to
	 */
	Path(const detail::PathElement* element);

public:

	/**
	 * A default constructor for this class.
	 */
	Path() : element(NULL) { };

	/**
	 * A constructor creating a path consisting of a single node (the root node).
	 *
	 * @param node the node the new path should consist of
	 */
	Path(const NodePtr& node);

	/**
	 * A copy constructor
	 */
	Path(const Path& other);

	/**
	 * A move constructor for this paths.
	 */
	Path(Path&& other);

	/**
	 * A destructor for paths.
	 */
	~Path();

	/**
	 * Obtains the node addressed by this path.
	 *
	 * @return a reference to the node addressed by this path
	 */
	const NodePtr& getAddressedNode() const;

	/**
	 * Obtains the path element referencing the root node of this path.
	 * @return a pointer to the requested path element
	 */
	const NodePtr& getRootNode() const;

	/**
	 * Obtains a path equivalent to this path, just being rooted
	 * by a different node.
	 *
	 * @param newRoot the alternative root node to be used
	 * @return the same path as this path starting from a different root node.
	 */
	Path switchRoot(const NodePtr& newRoot) const;

	/**
	 * Obtains the parent on the given level of this path.
	 *
	 * @param level if set to 0, it will be this node. If set to 1, it will be
	 * 				the immediate parent, 2 the parents parent and so forth. Must not
	 * 				be larger or equal the depth of this node.
	 * @return a pointer to the requested path element
	 */
	const NodePtr& getParentNode(unsigned level = 1) const;

	/**
	 * Obtains the a path consisting of the root node only.
	 *
	 * @return the requested path
	 */
	Path getPathToRoot() const;

	/**
	 * Obtains a path ending at the given parent node.
	 *
	 * @param level if set to 0, it will be this node. If set to 1, it will be
	 * 				the immediate parent, 2 the parents parent and so forth. Must not
	 * 				be larger or equal the depth of this node.
	 * @return the requested path
	 */
	Path getPathToParent(unsigned level = 1) const;

	/**
	 * Obtains a path extending this path by a node referencing the given child.
	 *
	 * @param index the index of the next step
	 * @return the extended path, ending at the given child node
	 */
	Path extendForChild(unsigned index) const;

	/**
	 * Obtains the index of the addressed node within the parents child list.
	 */
	std::size_t getIndex() const;

	/**
	 * The length of this path.
	 */
	std::size_t getLength() const;

	/**
	 * Verifies whether this path is valid. A path is invalid if either it is empty or
	 * for one of the steps along the path, the referenced element is not corresponding to the
	 * indexed child within the child list.
	 */
	bool isValid() const;

	/**
	 * Computes a hash value for this path.
	 */
	std::size_t hash() const;

	/**
	 * The copy assignment operator.
	 */
	Path& operator=(const Path& source);

	/**
	 * The move assignment operator.
	 */
	Path& operator=(Path&& source);

	/**
	 * The equals operator for paths. To paths are considered equal
	 * if they describe the same sequence of nodes, starting from the root, going
	 * to the addressed leaf.
	 *
	 * @param other the path to be compared with.
	 */
	bool operator==(const Path& other) const;

	/**
	 * The not-equal operator for paths. This is simply the negation of the equals
	 * operator.
	 *
	 * @param other the path to be compared with.
	 */
	bool operator!=(const Path& other) const {
		return !(*this == other);
	}

	/**
	 * A comparison operator for two paths. Paths will be ordered lexicographical.
	 */
	bool operator<(const Path& other) const;

	/**
	 * A pointer-like conversion to a boolean.
	 */
	operator bool() const {
		return element;
	}

	/**
	 * Prints a string-representation of this instance to the given stream.
	 *
	 * @param out the stream to be printed to
	 * @return a reference to the given stream
	 */
	std::ostream& printTo(std::ostream& out) const;

	/**
	 * Computes the hash code for a single-step path only consisting of the given node.
	 */
	static std::size_t hashSingleStepPath(const NodePtr& node);
};


// forward declaration
struct StaticAddressCast;
struct DynamicAddressCast;

// a simple type trait to filter IR address types
template<typename P> struct is_ir_address : public boost::false_type {};
template<typename T> struct is_ir_address<Address<T>> : public boost::true_type {};


// forward declaration for static casts
template<typename B, typename T, typename E = typename B::element_type>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<E,T>,boost::is_base_of<T,E>>, B>::type
static_address_cast(const Address<T>& src);

/**
 * This immutable value class can be used to address nodes within the AST. Since nodes within the AST are shared,
 * the same node may be reused at multiple locations within the AST. Hence, a simple pointer would be insufficient for
 * uniquely identifying an element of the syntax tree. To address such an element, the entire path, starting from an
 * arbitrary root node, has to be considered. Only this path allows to address particular sections within an AST.
 *
 * TODO: extend documentation with usage scenarios
 */
template<typename T>
class Address :
	public node_type<typename boost::remove_const<T>::type>::adr_accessor_type {

public:

	/**
	 * Defines a functor representing a static cast operator for this type.
	 */
	typedef StaticAddressCast StaticCast;
	typedef DynamicAddressCast DynamicCast;

	/**
	 * Defines the type of the node this address is pointing to.
	 */
	typedef T element_type;

private:

	/**
	 * The accessor offered to gain convenient access to members of the referenced node
	 */
	typedef typename node_type<typename boost::remove_const<T>::type>::adr_accessor_type accessor_type;


	/**
	 * The path used to identify the node referenced by this address.
	 */
	Path path;

public:

	/**
	 * A constructor creating an address for the given root node.
	 */
	template<
		typename B,
		typename boost::enable_if<boost::is_base_of<T,B>,int>::type = 0
	>
	explicit Address(const Pointer<B>& root) : path(root) {}

	/**
	 * A constructor creating a node address based on a path through an AST.
	 *
	 * @param path the path to the node to be addressed.
	 */
	Address(const Path& path = Path()) : path(path) {}

	/**
	 * A extended version of the copy constructor allowing to copy from addresses pointing
	 * to related types.
	 *
	 * @param from the element to be copied
	 */
	template<
		typename B,
		typename boost::enable_if<boost::is_base_of<T,B>,int>::type = 0
	>
	Address(const Address<B>& from) : path(from.getPath()) {}

	/**
	 * A extended move constructor for addresses allowing addresses to be moved without
	 * effecting the internally maintained reference counting.
	 */
	template<
		typename B,
		typename boost::enable_if<boost::is_base_of<T,B>,int>::type = 0
	>
	Address(Address<B>&& from) : path(std::move(from.getPath())) {}

	/**
	 * Reinterprets this address to be referencing the requested element type.
	 */
	template<typename R>
	const Address<R>& reinterpret() const {
		return reinterpret_cast<const Address<R>&>(*this);
	}


	/**
	 * A short-cut for static address casts followed by an extraction of
	 * the targeted node.
	 */
	template<typename R>
	typename boost::enable_if<is_ir_pointer<R>, R>::type as() const {
		return getAddressedNode().as<R>();
	}

	/**
	 * A short-cut for static address casts enabling a short syntax.
	 */
	template<typename R>
	typename boost::enable_if<is_ir_address<R>, R>::type as() const {
		return static_address_cast<R>(*this);
	}

	/**
	 * Finds *an* address with the given root and target.
	 *
	 * NOTE: generates the *first* address found that satisfies the criteria. Not necessarily unique (or what you wanted).
	 *
	 * @param target the element that the generated address points to
	 * @param root the root node of the generated address
	 * @param dfs tells whether depth-first-search strategy should be used (when true) or 
	 * breadth-first-search (when false)
	 *
	 * @returns the address found, or the null address if not possible
	 */
	static Address<T> find(const Pointer<T>& target, const NodePtr& root, bool dfs=true) {
		bool visitTypes = (target->getNodeCategory() == NC_Type) || 
						  (target->getNodeCategory() == NC_Support) ||
						  (target->getNodeCategory() == NC_Value);
		Address<T> ret;

		auto search = [&](const Address<T>& addr) -> bool {
			if(*addr.getAddressedNode() == *target) { 
				ret = addr;
				return true;
			}
			return false;
		};

		if (dfs)
			visitDepthFirstOnceInterruptible(Address(root), search, true, visitTypes);
		else 
			visitBreadthFirstInterruptible(Address(root), search, visitTypes);

		return ret;
	}


	/**
	 * Obtains a pointer to the root node this address is starting from.
	 *
	 * NOTE: nodes are not addressed in a unique way. Since nodes are shared, a single node might be accessible
	 * via multiple addresses.
	 *
	 * @return a pointer to the root node.
	 */
	NodePtr getRootNode() const {
		assert(path && "Invalid node address!");
		// root = the pointer assigned to the first path element
		return path.getRootNode();
	}

	/**
	 * Computes a new node address which can be obtained by exchanging the
	 * root of this address by the given root.
	 */
	Address<T> switchRoot(const NodePtr& newRoot) const {
		return Address<T>(getPath().switchRoot(newRoot));
	}

	/**
	 * Obtains a clone of this address within the given node manager.
	 *
	 * @param manager the manager this address should be cloned to
	 * @return a clone of this address referencing nodes within the given node manager
	 */
	Address<T> cloneTo(NodeManager& manager) const {
		if (!*this || &(this->getNodeManager()) == &manager) return *this;	// clone null-pointer or local address
		return switchRoot(manager.get(getRootNode()));
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
		return path.getParentNode(level);
	}

	/**
	 * Obtains a pointer to the node referenced by this address.
	 *
	 * NOTE: nodes are not addressed in a unique way. Since nodes are shared, a single node might be accessible
	 * via multiple addresses.
	 *
	 * @return a pointer to the addressed node.
	 */
	Pointer<const T> getAddressedNode() const {
		assert(path && "Invalid node address!");
		return static_pointer_cast<const T>(path.getParentNode(0));
	}

	/**
	 * Obtains the address of the root node this address is starting from. Since it will only consist of a single
	 * element its parents will be unknown.
	 *
	 * @return the (relative) address of the root node
	 */
	NodeAddress getRootAddress() const {
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
	NodeAddress getParentAddress(unsigned level=1) const {

		// check whether an actual parent is requested
		if (level == 0) {
			return *this;
		}

		// create address based on modified path
		return NodeAddress(path.getPathToParent(level));
	}

	/**
	 * Obtains the address of the first parent node of the given address of type typ
	 *
	 * @param the node type of the parent to find
	 * @return the requested address
	 */
	NodeAddress getFirstParentOfType(NodeType typ) const {

		NodeAddress ret = NodeAddress();
		auto visitor = makeLambdaVisitor([&](const NodeAddress& addr) -> bool { 
			if(addr.getAddressedNode()->getNodeType() == typ) {
				ret = addr;
				return true; 
			}
			return false;
		});
		visitPathBottomUpInterruptible(*this, visitor);
		return ret;

		assert(false && "Requested parent address of this type does not exist");
		return NodeAddress();
	}

	/**
	 * Obtains the address of a child node. It is extending the path maintained by this address by a single
	 * step, namely the node given by the index.
	 *
	 * @param index the index of the child-node to be addressed within the current nodes child list.
	 * @return the address of the child node
	 */
	NodeAddress getAddressOfChild(unsigned index) const {
		// extend path by child element
		return NodeAddress(path.extendForChild(index));
	}

	/**
	 * Obtains the address of a child node. It is extending the path represented by this address by
	 * the given sequence of steps.
	 *
	 * @param index the first child to be resolved
	 * @param rest the remaining steps
	 * @return the address of the requested child node
	 */
	template<typename ... Indices>
	NodeAddress getAddressOfChild(unsigned index, Indices ... rest) const {
		return getAddressOfChild(index).getAddressOfChild(rest...);
	}

	/**
	 * Obtains all child addresses.
	 *
	 * @return a vector containing addresses for all child nodes
	 */
	vector<NodeAddress> getChildAddresses() const {
		vector<NodeAddress> addresses;
		for(size_t i=0; i<getAddressedNode()->getChildList().size(); ++i) {
			addresses.push_back(NodeAddress(path.extendForChild(i)));
		}
		return addresses;
	}

	/**
	 * Checks whether this address is constituting a valid path within some AST. The method returns
	 * true in case the represented path can be reconstructed within the AST, hence if the (i+1)-th element
	 * within the path constituting this address is a pointer to a child node referenced by the i-th element.
	 *
	 * @return true if it is a valid path, false otherwise
	 */
	bool isValid() const {
		// check whether path is not null
		return path.isValid();
	}
	
	/**
	 * Determines whether this address is the address of a root node.
	 */
	bool isRoot() const {
		return getDepth() == 1;
	}

	/**
	 * Obtains the Index of the addressed element within its parent's node list
	 *
	 * @return the index of the addressed node within the parent's node list
	 */
	unsigned getIndex() const {
		 return path.getIndex();
	}

	/**
	 * Obtains the depth / length of this address. The depth corresponds to the number of nodes passed
	 * between the root node and the addressed node.
	 *
	 * @return the depth of the addressed nodes within the IR DAG
	 */
	unsigned getDepth() const {
		return path.getLength();
	}

	/**
	 * Obtains the entire path constituting this node address.
	 *
	 * @return a constant reference to the internally maintained path.
	 */
	const Path& getPath() const {
		return path;
	}

	/**
	 * Obtains the entire path constituting this node address. This function
	 * is required by the move constructor.
	 *
	 * @return a reference to the internally maintained path.
	 */
	Path& getPath() {
		return path;
	}

	/**
	 * An implicit converter to a pointer type.
	 */
	template<
		typename B,
		typename boost::enable_if<boost::is_base_of<B,T>,int>::type = 0
	>
	operator Pointer<const B>() const {
		return getAddressedNode();
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

	/**
	 * Obtains a accessor instance allowing to access the members of the addressed node.
	 */
	const accessor_type* operator->() const {
		return this;
	}

	/**
	 * Realizes an order on address according to their lexicographical order.
	 *
	 * @param other the address to be compared to
	 * @return true if this address is lexicographical smaller than the given address
	 */
	template<typename S>
	bool operator<(const Address<S>& other) const {
		// use the path comparison operation
		return path < other.path;
	}

	/**
	 * Obtains a hash value for this address.
	 */
	std::size_t hash() const {
		// take hash of path
		return path.hash();
	}

	/**
	 * Checks whether this address is referencing the same path as
	 * the given address.
	 */
	template <typename S>
	bool operator==(const Address<S>& other) const {
		// test for identity or equal path
		return this == reinterpret_cast<const Address<T>*>(&other) || path == other.getPath();
	}

	/**
	 * Implementing the not-equal operator addresses.
	 */
	template <typename S>
	bool operator!=(const Address<S>& other) const {
		return !(*this == other);
	}

};

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
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B,T>,boost::is_base_of<T,B>>, Address<B>>::type
static_address_cast(Address<T>& src) {
	assert(src && dynamic_cast<B*>(&(*src)) && "Invalid static cast!");
	return Address<B>(src.getPath());
}

template<typename B, typename T>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B,T>,boost::is_base_of<T,B>>, const Address<B>>::type
static_address_cast(const Address<T>& src) {
	assert(src && dynamic_cast<B*>(&(*src)) && "Invalid static cast!");
	return Address<B>(src.getPath());
}

/**
 * A variant of the static address cast allowing for a address type to be past
 * as a template argument.
 */
template<typename B, typename T, typename E = typename B::element_type>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<E,T>,boost::is_base_of<T,E>>, B>::type
static_address_cast(const Address<T>& src) {
	assert((!src || dynamic_cast<E*>(&(*src))) && "Invalid static cast!");
	return B(src.getPath());
}

/**
 * Allows to dynamically down-cast between addresses pointing to related types.
 *
 * @tparam B the type the resulting pointer should point to
 * @tparam T the type the given pointer is pointing to
 * @param src the pointer to be down-casted
 * @return the down-casted address pointing to the same location
 */
template<typename B, typename T>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B,T>,boost::is_base_of<T,B>>, Address<B>>::type
dynamic_address_cast(const Address<T>& src) {
	if (src && dynamic_cast<B*>(&(*src))) {
		return Address<B>(static_address_cast<B>(src));
	}
	// return an invalid address (default constructed)
	return Address<B>();
}

/**
 * A variant of the dynamic address cast allowing for a address type to be past
 * as a template argument.
 */
template<typename B, typename T, typename E = typename B::element_type>
inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<E,T>,boost::is_base_of<T,E>>, B>::type
dynamic_address_cast(const Address<T>& src) {
	if (src && dynamic_cast<E*>(&(*src))) {
		return static_address_cast<B>(src);
	}
	// return an invalid address (default constructed)
	return B();
}


/**
 * A template version for a functor performing static address casts on node addresses.
 * The purpose of this struct is to allow the static_address_cast function to be defined as
 * a pointer conversion function required as a template parameter of the AST Visitor class.
 */
struct StaticAddressCast {
	template<typename Target, typename Source>
	const Address<const Target> operator()(const Address<const Source>& value) const {
		return static_address_cast<const Target>(value);
	}
};

/**
 * A template version for a functor performing dynamic address casts on node addresses.
 * The purpose of this struct is to allow the dynamic_address_cast function to be defined as
 * a pointer conversion function required as a template parameter of the AST Visitor class.
 */
struct DynamicAddressCast {
	template<typename Target, typename Source>
	const Address<const Target> operator()(const Address<const Source>& value) const {
		return dynamic_address_cast<const Target>(value);
	}
};

template <class T>
Address<const T> concat(const Address<const T>& head, const Address<const T>& tail) {

	// NOTE: this is O(n^2) for the n = length of path
	// TODO: write recursive version which is O(n)

	assert(head.getAddressedNode() == tail.getRootNode() && "Impossible to merge addresses");
	const Path& tailPath = tail.getPath(); 
	// If try to merge a path with another path containing only 1 node we return the head path
	if ( tailPath.getLength() <= 1) { return head; }

	Path newPath = head.getPath();
	for(int i=tailPath.getLength()-2; i>=0; --i) {
		newPath = newPath.extendForChild( tailPath.getPathToParent(i).getIndex() );
	}
	return Address<const T>(newPath);
}

/**
 * Check whether address of node trg is a child of node src
 */
template <class T, class N>
bool isChildOf(const Address<const T>& src, const Address<const N>& trg) {
	// if the root node is not the same, we can already reply to the question
	if (src.getRootNode() != trg.getRootNode()) {
		return false;
	}
	// if it is the same node we are looking at, just return true
	if (src.getPath() == trg.getPath()) { return true; }
	if (src.getDepth() > trg.getDepth()) { return false; }

	return src.getPath() == trg.getParentAddress( trg.getDepth() - src.getDepth() ).getPath();
}

template <class T, class N>
Address<const T> cropRootNode(const Address<const T>& addr, const Address<const N>& newRoot) {
	
	assert(addr.getRootAddress() == newRoot.getRootAddress() && "Root of the two addresses must be the same");

    // Make sure that the newRoot is a child of addr*/
	assert( isChildOf(newRoot, addr) && "addr must be a child of newRoot");

	std::vector<unsigned> newPath;
	auto visitor = [&](const NodeAddress& cur) -> bool { 
		newPath.push_back(cur.getIndex());
		return cur==newRoot; 
	};

	auto lambdaVisitor = makeLambdaVisitor(visitor);
	bool ret = visitPathBottomUpInterruptible(addr, lambdaVisitor);
	if (!ret) assert(ret && "The new root was not find within the src address");
	
	NodeAddress newAddr(newRoot.getAddressedNode());
	for_each(newPath.rbegin()+1, newPath.rend(), [&](const unsigned& cur) { 
			newAddr = newAddr.getAddressOfChild(cur); 
		});

	return static_address_cast<const T>(newAddr);
}

template<typename Visitor, typename T>
void visitPathBottomUp(const Address<const T>& addr, Visitor& visitor) {
	visitor.visit(addr);
	if (addr.getDepth() != 1) {
		visitPathBottomUp(addr.getParentAddress(), visitor);
	}
}

template<typename Visitor, typename T>
void visitPathTopDown(const Address<const T>& addr, Visitor& visitor) {
	if (addr.getDepth() != 1) {
		visitPathTopDown(addr.getParentAddress(), visitor);
	}
	visitor.visit(addr);
}

template<typename Visitor, typename T>
bool visitPathBottomUpInterruptible(const Address<const T>& addr, Visitor& visitor) {
	bool res = visitor.visit(addr);
	if (!res && addr.getDepth() != 1) {
		return visitPathBottomUpInterruptible(addr.getParentAddress(), visitor);
	}
	return res;
}

template<typename Visitor, typename T>
bool visitPathTopDownInterruptible(const Address<const T>& addr, Visitor& visitor) {
	bool res = false;
	if (addr.getDepth() != 1) {
		res = visitPathTopDownInterruptible(addr.getParentAddress(), visitor);
	}
	return res || visitor.visit(addr);
}

template<typename Visitor>
void visitSharedPathTopDown(const NodeAddress& addr1, const NodeAddress& addr2, Visitor& visitor) {
	int d1 = (int)addr1.getDepth() - 1;
	int d2 = (int)addr2.getDepth() - 1;
	for(; d1>0 && d2>0; --d1, --d2) {
		NodeAddress a1 = addr1.getParentAddress(d1);
		NodeAddress a2 = addr2.getParentAddress(d2);
		if(*a1.getAddressedNode() == *a2.getAddressedNode()) {
			visitor.visit(a1);
		}
	}
}

} // end namespace core
} // end namespace insieme


namespace std {

	/**
	 * Allows path elements to be printed to an output stream.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::detail::PathElement& element);

	/**
	 * Allows paths to be printed to a an output stream.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::Path& path);

	/**
	 * Allows node addresses to be printed to a stream (especially useful during debugging and
	 * within test cases where equals expects values to be printable).
	 */
	template<typename T>
	std::ostream& operator<<(std::ostream& out, const insieme::core::Address<T>& node) {
		return (node) ? out << node.getPath() : out << "NULL";
	}

}



