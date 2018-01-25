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

#include <vector>
#include <algorithm>

#include <boost/type_traits/is_base_of.hpp>
#include <boost/type_traits/remove_const.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_polymorphic.hpp>

#include "insieme/utils/assert.h"
#include "insieme/utils/hash_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_path.h"
#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace core {


	// forward declaration
	struct StaticAddressCast;
	struct DynamicAddressCast;

	// a simple type trait to filter IR address types
	template <typename P>
	struct is_ir_address : public boost::false_type {};
	template <typename T>
	struct is_ir_address<Address<T>> : public boost::true_type {};


	// forward declaration for static casts
	template <typename B, typename T, typename E = typename B::element_type>
	inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<E, T>, boost::is_base_of<T, E>>, B>::type static_address_cast(const Address<T>& src);

	// forward declaration for dynamic cast
	template <typename B, typename T, typename E = typename B::element_type>
	inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<E, T>, boost::is_base_of<T, E>>, B>::type dynamic_address_cast(const Address<T>& src);


	namespace detail {

		/**
		 * A specialization of the NodeAccessor template which will be used in cases where
		 * the accessor is inherited by an address (to support access to the same elements
		 * as for pointers and nodes directly).
		 */
		template <typename Node>
		class node_access_helper<Address<const Node>> {
			/**
			 * The lazy-evaluated list of child-addresses. If the pointer is null, the
			 * child list hasn't been evaluated yet. Using the shared pointer will handle
			 * life cycles and it will reduce the amount of work when copying addresses.
			 */
			mutable std::shared_ptr<vector<NodeAddress>> childList;

		  public:
			/**
			 * A simple constructor for this type.
			 */
			node_access_helper() : childList(){};

			/**
			 * The type of the handled node.
			 */
			typedef Node node_type;

			/**
			 * Obtains access to the accessed node.
			 */
			inline const Node& getNode() const {
				return **static_cast<const Address<const Node>*>(this);
			}

			/**
			 * Obtains a reference to the entire list of children stored internally.
			 *
			 * @return a reference to the internally maintained child list
			 */
			const vector<NodeAddress>& getChildList() const & {
				if(!bool(childList)) {
					// produce child list
					const NodeList& children = getNode().getChildNodeList();
					childList = std::make_shared<vector<NodeAddress>>();
					vector<NodeAddress>& list = *childList;
					for(unsigned i = 0; i < children.size(); ++i) {
						list.push_back(static_cast<const Address<const Node>*>(this)->getAddressOfChild(i));
					}
				}
				return *childList;
			}

			/**
			 * Obtains the entire list of children by value (to keep them alive).
			 *
			 * @return a list of child nodes
			 */
			vector<NodeAddress> getChildList() const && {
				return getChildList(); // return by value
			}
		};

	} // end namespace detail


	/**
	 * This immutable value class can be used to address nodes within the AST. Since nodes within the AST are shared,
	 * the same node may be reused at multiple locations within the AST. Hence, a simple pointer would be insufficient for
	 * uniquely identifying an element of the syntax tree. To address such an element, the entire path, starting from an
	 * arbitrary root node, has to be considered. Only this path allows to address particular sections within an AST.
	 *
	 * TODO: extend documentation with usage scenarios
	 */
	template <typename T>
	class Address : public Accessor<typename std::remove_const<T>::type,Address<const typename std::remove_const<T>::type>,insieme::core::Address>  {
	  public:
		/**
		 * The kind of node path utilized by addresses.
		 */
		typedef NodePath<empty> Path;

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
		typedef Accessor<typename std::remove_const<T>::type,Address<const typename std::remove_const<T>::type>,insieme::core::Address> accessor_type;


		/**
		 * The path used to identify the node referenced by this address.
		 */
		Path path;

	  public:
		/**
		 * A constructor creating an address for the given root node.
		 */
		template <typename B, typename boost::enable_if<boost::is_base_of<T, B>, int>::type = 0>
		explicit Address(const Pointer<B>& root)
		    : path(root) {}

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
		template <typename B, typename boost::enable_if<boost::is_base_of<T, B>, int>::type = 0>
		Address(const Address<B>& from)
		    : path(from.getPath()) {}

		/**
		 * A extended move constructor for addresses allowing addresses to be moved without
		 * effecting the internally maintained reference counting.
		 */
		template <typename B, typename boost::enable_if<boost::is_base_of<T, B>, int>::type = 0>
		Address(Address<B>&& from)
		    : path(std::move(from.getPath())) {}

		/**
		 * Reinterprets this address to be referencing the requested element type.
		 */
		template <typename R>
		const Address<R>& reinterpret() const {
			return reinterpret_cast<const Address<R>&>(*this);
		}

		/**
		 * A short-cut for static address casts followed by an extraction of
		 * the targeted node.
		 */
		template <typename R>
		typename boost::enable_if<is_ir_pointer<R>, R>::type as() const {
			return getAddressedNode().template as<R>();
		}

		/**
		 * A short-cut for static address casts enabling a short syntax.
		 */
		template <typename R>
		typename boost::enable_if<is_ir_address<R>, R>::type as() const {
			return static_address_cast<R>(*this);
		}

		/**
		 * A short-cut for dynamic address casts followed by an extraction of
		 * the targeted node.
		 */
		template <typename R>
		typename boost::enable_if<is_ir_pointer<R>, R>::type isa() const {
			return getAddressedNode().template isa<R>();
		}

		/**
		 * Returns if a class is an instance of R
		 */
		template <typename R>
		typename boost::enable_if<is_ir_address<R>, R>::type isa() const {
			return dynamic_address_cast<R>(*this);
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
		static Address<T> find(const Pointer<T>& target, const NodePtr& root, bool dfs = true) {
			bool visitTypes = (target->getNodeCategory() == NC_Type) || (target->getNodeCategory() == NC_Support) || (target->getNodeCategory() == NC_Value);
			Address<T> ret;

			auto search = [&](const Address<T>& addr) -> bool {
				if(*addr.getAddressedNode() == *target) {
					ret = addr;
					return true;
				}
				return false;
			};

			if(dfs) {
				visitDepthFirstOnceInterruptible(Address(root), search, true, visitTypes);
			} else {
				visitBreadthFirstInterruptible(Address(root), search, visitTypes);
			}

			return ret;
		}

		/**
		 * Finds all addresses with an appearance of *target* from the given root.
		 *
		 * @param target the element that the generated address points to
		 * @param root the root node of the generated address
		 * @param dfs tells whether depth-first-search strategy should be used (when true) or
		 * breadth-first-search (when false)
		 *
		 * @returns the address found, or the null address if not possible
		 */
		static std::vector<Address<T>> findAll(const Pointer<T>& target, const NodePtr& root, bool dfs = true) {
			bool visitTypes = (target->getNodeCategory() == NC_Type) || (target->getNodeCategory() == NC_Support) || (target->getNodeCategory() == NC_Value);
			std::vector<Address<T>> ret;

			auto search = [&](const Address<T>& addr){
				if(*addr.getAddressedNode() == *target) ret.push_back(addr);
			};

			if (dfs) visitDepthFirst(Address(root), search, true, visitTypes);
			else	 visitBreadthFirst(Address(root), search, visitTypes);
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
			assert_true(path) << "Invalid node address!";
			// root = the pointer assigned to the first path element
			return path.getRootNode();
		}

		/**
		 * Computes a new node address which can be obtained by exchanging the
		 * root of this address by the given root.
		 */
		Address<T> switchRoot(const NodePtr& newRoot) const {
			return Address<T>(path.switchRoot(newRoot));
		}

		/**
		 * Computes a new node address which can be obtained by exchanging the
		 * root of this address by the given root, with a potentially different type.
		 */
		NodeAddress switchRootAndType(const NodePtr& newRoot) const {
			return NodeAddress(path.switchRoot(newRoot));
		}

		/**
		 * Obtains a clone of this address within the given node manager.
		 *
		 * @param manager the manager this address should be cloned to
		 * @return a clone of this address referencing nodes within the given node manager
		 */
		Address<T> cloneTo(NodeManager& manager) const {
			if(!*this || &(this->getNodeManager()) == &manager) {
				return *this; // clone null-pointer or local address
			}
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
		NodePtr getParentNode(unsigned level = 1) const {
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
			return (!path) ? Pointer<const T>() : static_pointer_cast<const T>(path.getParentNode(0));
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
		NodeAddress getParentAddress(unsigned level = 1) const {
			// check whether an actual parent is requested
			if(level == 0) { return *this; }

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

			assert_fail() << "Requested parent address of this type does not exist";
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
		template <typename... Indices>
		NodeAddress getAddressOfChild(unsigned index, Indices... rest) const {
			return getAddressOfChild(index).getAddressOfChild(rest...);
		}

		/**
		 * Appends the given address at the end of this address.
		 */
		template <typename R>
		Address<R> concat(const Address<R>& other) const {
			// check matching ends
			assert_eq(getAddressedNode(), other.getRootNode()) << "Cannot concatenate paths with un-matching ends: " << getAddressedNode()->getNodeType() << "@"
			                                                   << &*getAddressedNode() << " vs. " << other.getRootNode()->getNodeType() << "@"
			                                                   << &*other.getRootNode();

			// build result
			return Address<R>(Path::concat(path, other.getPath()));
		}

		/**
		 * Obtains all child addresses.
		 *
		 * @return a vector containing addresses for all child nodes
		 */
		vector<NodeAddress> getChildAddresses() const {
			vector<NodeAddress> addresses;
			for(size_t i = 0; i < getAddressedNode()->getChildList().size(); ++i) {
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
			return path;
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
		template <typename B, typename boost::enable_if<boost::is_base_of<B, T>, int>::type = 0>
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
		template <typename S>
		bool operator<(const Address<S>& other) const {
			// use the path comparison operation
			return path < other.getPath();
		}
		template <typename S>
		bool operator<=(const Address<S>& other) const {
			return *this == other || *this < other;
		}
		template <typename S>
		bool operator>=(const Address<S>& other) const {
			return !(*this < other);
		}
		template <typename S>
		bool operator>(const Address<S>& other) const {
			return !(*this <= other);
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
		 * Checks whether this address is referencing the same path as
		 * the given address.
		 */
		template <typename S>
		bool operator!=(const Address<S>& other) const {
			// test for identity or equal path
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
	template <typename B, typename T>
	inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B, T>, boost::is_base_of<T, B>>, Address<B>>::type static_address_cast(Address<T>& src) {
		assert_true(src && dynamic_cast<B*>(&(*src))) << "Invalid static cast!\n"
		                                              << "  source type: " << node_type<T>::getName() << "\n"
		                                              << "  actual type: " << src->getNodeType() << "\n"
		                                              << "  target type: " << node_type<B>::getName() << "\n";
		return Address<B>(src.getPath());
	}

	template <typename B, typename T>
	inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B, T>, boost::is_base_of<T, B>>, const Address<B>>::type
	static_address_cast(const Address<T>& src) {
		assert_true(src && dynamic_cast<B*>(&(*src))) << "Invalid static cast!\n"
		                                              << "  source type: " << node_type<T>::getName() << "\n"
		                                              << "  actual type: " << src->getNodeType() << "\n"
		                                              << "  target type: " << node_type<B>::getName() << "\n";
		return Address<B>(src.getPath());
	}

	/**
	 * A variant of the static address cast allowing for a address type to be past
	 * as a template argument.
	 */
	template <typename B, typename T, typename E>
	inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<E, T>, boost::is_base_of<T, E>>, B>::type static_address_cast(const Address<T>& src) {
		assert_true(!src || dynamic_cast<E*>(&(*src))) << "Invalid static cast!\n"
		                                               << "  source type: " << node_type<T>::getName() << "\n"
		                                               << "  actual type: " << src->getNodeType() << "\n"
		                                               << "  target type: " << node_type<E>::getName() << "\n";
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
	template <typename B, typename T>
	inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B, T>, boost::is_base_of<T, B>>, Address<B>>::type
	dynamic_address_cast(const Address<T>& src) {
		if(src && dynamic_cast<B*>(&(*src))) { return Address<B>(static_address_cast<B>(src)); }
		// return an invalid address (default constructed)
		return Address<B>();
	}

	/**
	 * A variant of the dynamic address cast allowing for a address type to be past
	 * as a template argument.
	 */
	template <typename B, typename T, typename E>
	inline typename boost::enable_if<boost::mpl::or_<boost::is_base_of<E, T>, boost::is_base_of<T, E>>, B>::type dynamic_address_cast(const Address<T>& src) {
		if(src && dynamic_cast<E*>(&(*src))) { return static_address_cast<B>(src); }
		// return an invalid address (default constructed)
		return B();
	}


	/**
	 * A template version for a functor performing static address casts on node addresses.
	 * The purpose of this struct is to allow the static_address_cast function to be defined as
	 * a pointer conversion function required as a template parameter of the AST Visitor class.
	 */
	struct StaticAddressCast {
		template <typename Target, typename Source>
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
		template <typename Target, typename Source>
		const Address<const Target> operator()(const Address<const Source>& value) const {
			return dynamic_address_cast<const Target>(value);
		}
	};

	template <class A, class B>
	Address<const B> concat(const Address<const A>& head, const Address<const B>& tail) {
		return head.concat(tail);
	}

	template <class A, class B>
	Address<const B> operator>>(const Address<const A>& head, const Address<const B>& tail) {
		return concat(head, tail);
	}

	/**
	 * Check whether address of node trg is a child of node src
	 */
	template <class T, class N>
	bool isChildOf(const Address<const T>& src, const Address<const N>& trg) {
		// check the length of the address
		if(src.getDepth() > trg.getDepth()) { return false; }
		// compare the path (that's it)
		return src.getPath() == trg.getParentAddress(trg.getDepth() - src.getDepth()).getPath();
	}

	template <class T, class N>
	Address<const T> cropRootNode(const Address<const T>& addr, const Address<const N>& newRoot) {
		assert(addr.getRootAddress() == newRoot.getRootAddress() && "Root of the two addresses must be the same");

		// Make sure that the newRoot is a child of addr*/
		assert_true(isChildOf(newRoot, addr)) << "addr must be a child of newRoot";

		std::vector<unsigned> newPath;
		auto visitor = [&](const NodeAddress& cur) -> bool {
			newPath.push_back(cur.getIndex());
			return cur == newRoot;
		};

		auto lambdaVisitor = makeLambdaVisitor(visitor);
		bool ret = visitPathBottomUpInterruptible(addr, lambdaVisitor);
		if(!ret) { assert_true(ret) << "The new root was not find within the src address"; }

		NodeAddress newAddr(newRoot.getAddressedNode());
		for_each(newPath.rbegin() + 1, newPath.rend(), [&](const unsigned& cur) { newAddr = newAddr.getAddressOfChild(cur); });

		return static_address_cast<const T>(newAddr);
	}

	template <typename Visitor, typename T>
	void visitPathBottomUp(const Address<const T>& addr, Visitor& visitor) {
		visitor.visit(addr);
		if(addr.getDepth() != 1) { visitPathBottomUp(addr.getParentAddress(), visitor); }
	}
	template <typename T, typename Lambda, typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	void visitPathBottomUp(const Address<const T>& addr, Lambda lam) {
		auto visitor = makeLambdaVisitor(lam);
		visitPathBottomUp(addr, visitor);
	}

	template <typename Visitor, typename T>
	void visitPathTopDown(const Address<const T>& addr, Visitor& visitor) {
		if(addr.getDepth() != 1) { visitPathTopDown(addr.getParentAddress(), visitor); }
		visitor.visit(addr);
	}
	template <typename T, typename Lambda, typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	void visitPathTopDown(const Address<const T>& addr, Lambda lam) {
		auto visitor = makeLambdaVisitor(lam);
		visitPathTopDown(addr, visitor);
	}

	template <typename Visitor, typename T>
	bool visitPathBottomUpInterruptible(const Address<const T>& addr, Visitor& visitor) {
		bool res = visitor.visit(addr);
		if(!res && addr.getDepth() != 1) { return visitPathBottomUpInterruptible(addr.getParentAddress(), visitor); }
		return res;
	}
	template <typename T, typename Lambda, typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	bool visitPathBottomUpInterruptible(const Address<const T>& addr, Lambda lam) {
		auto visitor = makeLambdaVisitor(lam);
		return visitPathBottomUpInterruptible(addr, visitor);
	}

	template <typename Visitor, typename T>
	bool visitPathTopDownInterruptible(const Address<const T>& addr, Visitor& visitor) {
		bool res = false;
		if(addr.getDepth() != 1) { res = visitPathTopDownInterruptible(addr.getParentAddress(), visitor); }
		return res || visitor.visit(addr);
	}
	template <typename T, typename Lambda, typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	bool visitPathTopDownInterruptible(const Address<const T>& addr, Lambda lam) {
		auto visitor = makeLambdaVisitor(lam);
		return visitPathTopDownInterruptible(addr, visitor);
	}

	template <typename Visitor>
	void visitSharedPathTopDown(const NodeAddress& addr1, const NodeAddress& addr2, Visitor& visitor) {
		int d1 = (int)addr1.getDepth() - 1;
		int d2 = (int)addr2.getDepth() - 1;
		for(; d1 > 0 && d2 > 0; --d1, --d2) {
			NodeAddress a1 = addr1.getParentAddress(d1);
			NodeAddress a2 = addr2.getParentAddress(d2);
			if(*a1.getAddressedNode() == *a2.getAddressedNode()) { visitor.visit(a1); }
		}
	}

} // end namespace core
} // end namespace insieme


namespace std {

	/**
	 * Allows node addresses to be printed to a stream (especially useful during debugging and
	 * within test cases where equals expects values to be printable).
	 */
	template <typename T>
	std::ostream& operator<<(std::ostream& out, const insieme::core::Address<T>& node) {
		return (node) ? out << node.getPath() : out << "NULL";
	}

	/**
	 * Integrate addresses into the std::hash framework.
	 */
	template <typename T>
	struct hash<insieme::core::Address<T>> {
		size_t operator()(const insieme::core::Address<T>& addr) const {
			return addr.hash();
		}
	};
}
