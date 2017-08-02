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

#include <type_traits>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node_types.h"
#include "insieme/core/ir_node_traits.h"
#include "insieme/core/ir_pointer.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir.h"

#include "insieme/utils/hash_utils.h"

namespace insieme {
namespace core {

	// ---------------------------------- forward declarations --------------------------------------------

	class IteratorValue;
	template <typename T>
	class Instance;

	/**
	 * Adds forward declarations for all IR node instance types.
	 */
	#define NODE(NAME) typedef Instance<const NAME> NAME##Instance;

	// take all nodes from within the definition file
	#include "insieme/core/ir_nodes.def"

	#undef NODE

	// ----------------------------------------------------------------------------------------------------

	class IteratorValue : public utils::Printable,
	                      public utils::HashableImmutableData<IteratorValue>,
	                      public boost::equality_comparable<IteratorValue>,
	                      public boost::less_than_comparable<IteratorValue> {
		int value;

	  public:
		const static IteratorValue STAR;

		IteratorValue(int value = 0) : utils::HashableImmutableData<IteratorValue>(value), value(value) {}


		bool operator==(const IteratorValue& other) const {
			return value == other.value;
		}

		bool operator<(const IteratorValue& other) const {
			// the order here is 0, 1, 2, ... * ... -2, -1

			// same sign comparison
			if(value >= 0 && other.value >= 0) { return value < other.value; }
			if(value < 0 && other.value < 0) { return value < other.value; }

			// different signs
			return (value >= 0 && other.value < 0);
		}

		std::ostream& printTo(std::ostream& out) const {
			if(*this == STAR) { return out << "*"; }
			return out << value;
		}
	};

	/**
	 * A template specialization for node path elements handling iterator values.
	 */
	template <>
	class NodePathElement<IteratorValue> : public NodePathElementBase<NodePathElement<IteratorValue>> {
		/**
		 * The counter for the referenced iteration.
		 */
		IteratorValue iter;

	  public:
		/**
		 * Creates a new path element using the given values.
		 *
		 * @param ptr a pointer to the node addressed by this element path
		 * @param index the index of the addressed node within its parent node.
		 * @param parent a pointer to the path element referencing the parent node - null if this element is referencing the root node.
		 */
		NodePathElement(const NodePtr& ptr, std::size_t index, const IteratorValue& iter, const NodePathElement<IteratorValue>* parent)
		    : NodePathElementBase<NodePathElement<IteratorValue>>(ptr, index, parent, computeHash(ptr, index, iter, parent)), iter(iter) {
			assert_true(iter == IteratorValue(0) || isIterable()) << "Only loop body may have iteration values != 0";
		}

		/**
		 * Obtains the iterator value attached to this element.
		 */
		const IteratorValue& getValue() const {
			return iter;
		}

		/**
		 * Compares the annotated iterator value.
		 */
		int compareValue(const IteratorValue& value) const {
			return (iter < value) ? -1 : (iter == value ? 0 : 1);
		}

		/**
		 * Implements the printTo function as required by the base type. Note, it is not virtual,
		 * static dispatching is utilized.
		 */
		std::ostream& printToInternal(std::ostream& out) const {
			if(this->parent) { out << *(this->parent) << "-"; }
			out << this->index;
			if(isIterable()) { out << "/" << iter; }
			return out;
		}

		/**
		 * Computes a hash code for the given combination of values.
		 *
		 * @param ptr the node pointer to be pointing to
		 * @param index the index of the node within its parent's child list
		 * @param iter the iteration counter
		 * @param the parent of the given path element.
		 * @return a proper hash value for a node based on the given values
		 */
		static std::size_t computeHash(const NodePtr& ptr, std::size_t index, const IteratorValue& iter, const NodePathElement<IteratorValue>* const parent) {
			std::size_t seed = boost::hash_value(index);
			boost::hash_combine(seed, *ptr);
			boost::hash_combine(seed, ((parent) ? parent->hash : 0));
			boost::hash_combine(seed, iter);
			return seed;
		}

	  private:
		/**
		 * Checks whether the addressed node is iterable.
		 */
		bool isIterable() const {
			// the element must reference the body of a loop
			if(!parent) { return false; }
			if(!this->ptr.isa<CompoundStmtPtr>()) { return false; }
			if(auto stmt = this->parent->ptr.isa<ForStmtPtr>()) { return stmt->getBody() == this->ptr; }
			if(auto stmt = this->parent->ptr.isa<WhileStmtPtr>()) { return stmt->getBody() == this->ptr; }
			return false;
		}
	};

	// ----------------------------------------------------------------------------------------------------

	// forward declaration of cast functors
	struct StaticInstanceCast;
	struct DynamicInstanceCast;

	// a simple type trait to filter IR instance address types
	template <typename P>
	struct is_ir_instance : public boost::false_type {};
	template <typename T>
	struct is_ir_instance<Instance<T>> : public boost::true_type {};

	// forward declaration for static casts
	template <typename B, typename T, typename E = typename B::element_type>
	inline typename std::enable_if<is_ir_instance<B>::value && (std::is_base_of<E, T>::value || std::is_base_of<T, E>::value), B>::type
	static_instance_cast(const Instance<T>& src);

	// forward declaration for dynamic cast
	template <typename B, typename T, typename E = typename B::element_type>
	inline typename std::enable_if<is_ir_instance<B>::value && (std::is_base_of<E, T>::value || std::is_base_of<T, E>::value), B>::type
	dynamic_instance_cast(const Instance<T>& src);

	// ----------------------------------------------------------------------------------------------------

	namespace detail {

		/**
		 * A specialization of the NodeAccessor template which will be used in cases where
		 * the accessor is inherited by an instance addres (to support access to the same elements
		 * as for pointers and nodes directly).
		 */
		template <typename Node>
		class node_access_helper<Instance<const Node>> {
			/**
			 * The lazy-evaluated list of child-addresses. If the pointer is null, the
			 * child list hasn't been evaluated yet. Using the shared pointer will handle
			 * life cycles and it will reduce the amount of work when copying addresses.
			 */
			mutable std::shared_ptr<vector<NodeInstance>> childList;

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
				return **static_cast<const Instance<const Node>*>(this);
			}

			/**
			 * Obtains a reference to the entire list of children stored internally.
			 *
			 * @return a reference to the internally maintained child list
			 */
			const vector<NodeInstance>& getChildList() const {
				if(!bool(childList)) {
					// produce child list
					const NodeList& children = getNode().getChildNodeList();
					childList = std::make_shared<vector<NodeInstance>>();
					vector<NodeInstance>& list = *childList;
					for(unsigned i = 0; i < children.size(); ++i) {
						list.push_back(static_cast<const Instance<const Node>*>(this)->getInstanceOfChild(i));
					}
				}
				return *childList;
			}

		};

	} // end namespace detail


	/**
	 * The generic class implementing the IR-Instances class representing references to instances of
	 * statements and expressions (similar to addresses + loop iterations).
	 */
	template <typename T>
	class Instance : public Accessor<typename std::remove_const<T>::type,Instance<const typename std::remove_const<T>::type>,Instance>,
	                 public utils::Printable,
	                 public boost::equality_comparable<Instance<T>>,
	                 public boost::less_than_comparable<Instance<T>> {
	  public:
		/**
		 * The type of node path utilized by instances of this type.
		 */
		typedef NodePath<IteratorValue> Path;

		/**
		 * Defines a functor representing a static cast operator for this type.
		 */
		typedef StaticInstanceCast StaticCast;
		typedef DynamicInstanceCast DynamicCast;

		/**
		 * Defines the type of the node this address is pointing to.
		 */
		typedef T element_type;

	  private:
		/**
		 * The accessor offered to gain convenient access to members of the referenced node
		 */
		typedef Accessor<typename std::remove_const<T>::type,Instance<const typename std::remove_const<T>::type>,insieme::core::Instance> accessor_type;


		/**
		 * The path used to identify the node referenced by this address.
		 */
		Path path;

	  public:
		/**
		 * A constructor creating an instance address for the given root node.
		 */
		template <typename B, typename std::enable_if<std::is_base_of<T, B>::value, int>::type = 0>
		explicit Instance(const Pointer<B>& root)
		    : path(root) {}

		/**
		 * A constructor converting a address into an instance.
		 */
		template <typename B, typename std::enable_if<std::is_base_of<T, B>::value, int>::type = 0>
		Instance(const Address<B>& addr)
		    : path(convertPath(addr)) {}

		/**
		 * A constructor creating a node address based on a path through an AST.
		 *
		 * @param path the path to the node to be addressed.
		 */
		Instance(const Path& path = Path()) : path(path) {}

		/**
		 * A extended version of the copy constructor allowing to copy of instance addresses pointing
		 * to related types.
		 *
		 * @param from the element to be copied
		 */
		template <typename B, typename std::enable_if<std::is_base_of<T, B>::value, int>::type = 0>
		Instance(const Instance<B>& from)
		    : path(from.getPath()) {}

		/**
		 * A extended move constructor for addresses allowing addresses to be moved without
		 * effecting the internally maintained reference counting.
		 */
		template <typename B, typename boost::enable_if<boost::is_base_of<T, B>, int>::type = 0>
		Instance(Instance<B>&& from)
		    : path(std::move(from.getPath())) {}

		/**
		 * Reinterprets this address to be referencing the requested element type.
		 */
		template <typename R>
		const Instance<R>& reinterpret() const {
			return reinterpret_cast<const Instance<R>&>(*this);
		}


		/**
		 * A short-cut for static address casts followed by an extraction of
		 * the targeted node.
		 */
		template <typename R>
		typename std::enable_if<is_ir_pointer<R>::value, R>::type as() const {
			return getAddressedNode().template as<R>();
		}

		/**
		 * A short-cut for static address casts enabling a short syntax.
		 */
		template <typename R>
		typename std::enable_if<is_ir_address<R>::value, R>::type as() const {
			return getAsAddress().template as<R>();
		}

		/**
		 * A short-cut for static address casts enabling a short syntax.
		 */
		template <typename R>
		typename std::enable_if<is_ir_instance<R>::value, R>::type as() const {
			return static_instance_cast<R>(*this);
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
		 * A short-cut for static address casts enabling a short syntax.
		 */
		template <typename R>
		typename std::enable_if<is_ir_address<R>::value, R>::type isa() const {
			// shortcut
			if(!getAddressedNode().template isa<Pointer<const typename R::element_type>>()) { return R(); }
			return getAsAddress().template isa<R>();
		}

		/**
		 * Returns if a class is an instance of R
		 */
		template <typename R>
		typename boost::enable_if<is_ir_instance<R>, R>::type isa() const {
			return dynamic_instance_cast<R>(*this);
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
			return Address<T>(getPath().switchRoot(newRoot));
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
		 * Converts this instance reference to an address.
		 */
		Address<const T> getAsAddress() const {
			if(!path) { return Address<const T>(); }
			if(isRoot()) { return Address<const T>(getAddressedNode()); }
			return getParentInstance().getAsAddress().getAddressOfChild(getIndex()).template as<Address<const T>>();
		}

		/**
		 * Obtains the instance address of the root node this address is starting from. Since it will only consist of a single
		 * element its parents will be unknown.
		 *
		 * @return the (relative) address of the root node
		 */
		NodeInstance getRootInstance() const {
			return NodeInstance(getRootNode());
		}

		/**
		 * Obtains the instance address of the parent node of the given instance (of an arbitrary higher level).
		 * An assertion error will occur in case the requested level is large or equal the depth of this address.
		 * (Hence, the corresponding parent node does not exist or is unknown).
		 *
		 * @param level the number of levels to go up (0=same node, 1=parent, 2=grandparents ...)
		 * @return the requested address
		 */
		NodeInstance getParentInstance(unsigned level = 1) const {
			// check whether an actual parent is requested
			if(level == 0) { return *this; }

			// create address based on modified path
			return NodeInstance(path.getPathToParent(level));
		}

		/**
		 * Obtains the instance address of a child node. It is extending the path maintained by this instance by a single
		 * step, namely the node given by the index.
		 *
		 * @param index the index of the child-node to be addressed within the current nodes child list.
		 * @return the address of the child node
		 */
		NodeInstance getInstanceOfChild(unsigned index, const IteratorValue& iter = IteratorValue(0)) const {
			// extend path by child element
			return NodeInstance(path.extendForChild(index, iter));
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
		NodeInstance getInstanceOfChild(unsigned index, Indices... rest) const {
			return getInstanceOfChild(index).getInstanceOfChild(rest...);
		}

		/**
		 * Obtains the address of a child node. It is extending the path represented by this address by
		 * the given sequence of steps.
		 *
		 * @param index the first child to be resolved
		 * @param iter the iterator value of the first child
		 * @param rest the remaining steps
		 * @return the address of the requested child node
		 */
		template <typename... Indices>
		NodeInstance getInstanceOfChild(unsigned index, IteratorValue iter, Indices... rest) const {
			return getInstanceOfChild(index, iter).getInstanceOfChild(rest...);
		}

		/**
		 * Obtains all child instance addresses.
		 *
		 * @return a vector containing addresses for all child nodes
		 */
		vector<NodeInstance> getChildInstances() const {
			vector<NodeInstance> addresses;
			for(size_t i = 0; i < getAddressedNode()->getChildList().size(); ++i) {
				addresses.push_back(NodeInstance(path.extendForChild(i)));
			}
			return addresses;
		}

		/**
		 * Appends the given address at the end of this instance address.
		 */
		template <typename R>
		Instance<R> concat(const Address<R>& other) const {
			return concat(Instance<R>(other));
		}

		/**
		 * Appends the given address at the end of this instance address.
		 */
		template <typename R>
		Instance<R> concat(const Instance<R>& other) const {
			// check matching ends
			assert_eq(getAddressedNode(), other.getRootNode()) << "Cannot concatenate paths with un-matching ends: " << getAddressedNode()->getNodeType() << "@"
			                                                   << &*getAddressedNode() << " vs. " << other.getRootNode()->getNodeType() << "@"
			                                                   << &*other.getRootNode();

			// build result
			return Instance<R>(Path::concat(path, other.getPath()));
		}

		/**
		 * Obtains a instance for the same node of a different iteration.
		 */
		Instance<T> getIteration(const IteratorValue& iter) const {
			if(iter == getIteratorValue()) { return *this; }
			assert_false(isRoot()) << "Can not be applied to a root path.";
			return Instance<T>(path.getPathToParent().extendForChild(path.getIndex(), iter));
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
		 * Obtains the iterator value attached to the current addressed element.
		 */
		const IteratorValue& getIteratorValue() const {
			return path.getValue();
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
		 * An implicit converter to a pointer type.
		 */
		template <typename B, typename boost::enable_if<boost::is_base_of<B, T>, int>::type = 0>
		operator Pointer<const B>() const {
			return getAddressedNode();
		}

		/**
		 * An implicit converter to a address type.
		 */
		template <typename B, typename boost::enable_if<boost::is_base_of<B, T>, int>::type = 0>
		operator Address<const B>() const {
			return getAsAddress();
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
		bool operator<(const Instance<S>& other) const {
			// use the path comparison operation
			return path < other.getPath();
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
		bool operator==(const Instance<S>& other) const {
			// test for identity or equal path
			return this == reinterpret_cast<const Instance<T>*>(&other) || path == other.getPath();
		}

		/**
		 * Implementing the not-equal operator addresses.
		 */
		template <typename S>
		bool operator!=(const Instance<S>& other) const {
			return !(*this == other);
		}

		/**
		 * Allow instances to be printed.
		 */
		std::ostream& printTo(std::ostream& out) const {
			return (path) ? (out << path) : (out << "NULL");
		}

	  private:
		/**
		 * Converts a given address into a instance path.
		 */
		static Path convertPath(const NodeAddress& addr) {
			// handle undefined address
			if(!addr) { return Path(); }

			// handle root path
			if(addr.isRoot()) { return Path(addr.getAddressedNode()); }

			// else process recursive
			return convertPath(addr.getParentAddress()).extendForChild(addr.getIndex());
		}
	};

	/**
	 * A template version for a functor performing static address casts on node addresses.
	 * The purpose of this struct is to allow the static_address_cast function to be defined as
	 * a pointer conversion function required as a template parameter of the AST Visitor class.
	 */
	struct StaticInstanceCast {
		template <typename Target, typename Source>
		const Instance<const Target> operator()(const Instance<const Source>& value) const {
			return value.template as<Instance<const Target>>();
		}
	};

	/**
	 * A template version for a functor performing dynamic address casts on node addresses.
	 * The purpose of this struct is to allow the dynamic_address_cast function to be defined as
	 * a pointer conversion function required as a template parameter of the AST Visitor class.
	 */
	struct DynamicInstanceCast {
		template <typename Target, typename Source>
		const Instance<const Target> operator()(const Instance<const Source>& value) const {
			return value.template isa<Instance<const Target>>();
		}
	};


	/**
	 * Allows to statically down-cast between instance addresses pointing to related types. Unlike for the dynamic cast, no runtime
	 * checks will be conducted.
	 *
	 * @tparam B the type the resulting pointer should point to
	 * @tparam T the type the given pointer is pointing to
	 * @param src the pointer to be down-casted
	 * @return the down-casted address pointing to the same location
	 */
	template <typename B, typename T>
	inline typename std::enable_if<!is_ir_instance<B>::value && (std::is_base_of<B, T>::value || std::is_base_of<T, B>::value), Instance<B>>::type
	static_instance_cast(const Instance<T>& src) {
		assert_true(src && dynamic_cast<B*>(&(*src))) << "Invalid static cast!\n"
		                                              << "  source type: " << node_type<T>::getName() << "\n"
		                                              << "  actual type: " << src->getNodeType() << "\n"
		                                              << "  target type: " << node_type<B>::getName() << "\n";
		return Instance<B>(src.getPath());
	}

	/**
	 * A variant of the static instance address cast allowing for a instance type to be past
	 * as a template argument.
	 */
	template <typename B, typename T, typename E>
	inline typename std::enable_if<is_ir_instance<B>::value && (std::is_base_of<E, T>::value || std::is_base_of<T, E>::value), B>::type
	static_instance_cast(const Instance<T>& src) {
		assert_true(!src || dynamic_cast<E*>(&(*src))) << "Invalid static cast!\n"
		                                               << "  source type: " << node_type<T>::getName() << "\n"
		                                               << "  actual type: " << src->getNodeType() << "\n"
		                                               << "  target type: " << node_type<E>::getName() << "\n";
		return B(src.getPath());
	}

	/**
	 * Allows to dynamically down-cast between instance addresses pointing to related types.
	 *
	 * @tparam B the type the resulting pointer should point to
	 * @tparam T the type the given pointer is pointing to
	 * @param src the pointer to be down-casted
	 * @return the down-casted address pointing to the same location
	 */
	template <typename B, typename T>
	inline typename std::enable_if<!is_ir_instance<B>::value && (std::is_base_of<B, T>::value || std::is_base_of<T, B>::value), Instance<B>>::type
	dynamic_instance_cast(const Instance<T>& src) {
		if(src && dynamic_cast<B*>(&(*src))) { return Instance<B>(static_instance_cast<B>(src)); }
		// return an invalid address (default constructed)
		return Instance<B>();
	}

	/**
	 * A variant of the dynamic address cast allowing for a address type to be past
	 * as a template argument.
	 */
	template <typename B, typename T, typename E>
	inline typename std::enable_if<is_ir_instance<B>::value && (std::is_base_of<E, T>::value || std::is_base_of<T, E>::value), B>::type
	dynamic_instance_cast(const Instance<T>& src) {
		if(src && dynamic_cast<E*>(&(*src))) { return static_instance_cast<B>(src); }
		// return an invalid address (default constructed)
		return B();
	}

	/**
	 * Extends the given node instance address by the given tail address.
	 */
	template <class A, class B>
	Instance<const B> concat(const Instance<const A>& head, const Address<const B>& tail) {
		return head.concat(tail);
	}

	/**
	 * Extends the given node instance address by the given tail address.
	 */
	template <class A, class B>
	Instance<const B> concat(const Instance<const A>& head, const Instance<const B>& tail) {
		return head.concat(tail);
	}


} // end namespace core
} // end namespace insieme


namespace std {

	/**
	 * Integrate addresses into the std::hash framework.
	 */
	template <typename T>
	struct hash<insieme::core::Instance<T>> {
		size_t operator()(const insieme::core::Instance<T>& addr) const {
			return addr.hash();
		}
	};
}
