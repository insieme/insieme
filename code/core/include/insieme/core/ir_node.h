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

#include <map>

#include <boost/variant.hpp>

#include "insieme/utils/annotation.h"
#include "insieme/utils/hash_utils.h"
#include "insieme/utils/id_generator.h"
#include "insieme/utils/instance_manager.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_pointer.h"
#include "insieme/core/ir_mapper.h"

namespace insieme {
namespace core {
namespace new_core {


	// **********************************************************************************
	// 									Node Types
	// **********************************************************************************

	/**
	 * Defines an enumeration containing an entry for every node type. This
	 * enumeration can than be used to identify the actual type of AST nodes
	 * in case the exact type cannot be determined statically.
	 */
	#define CONCRETE(name) NT_ ## name,
	enum NodeType {
		// the necessary information is obtained from the node-definition file
		#include "insieme/core/ir_nodes.def"
	};
	#undef CONCRETE

	/**
	 * A constant defining the number of node types.
	 */
	#define CONCRETE(name) +1
	enum { NUM_CONCRETE_NODE_TYPES = 0
		// the necessary information is obtained from the node-definition file
		#include "insieme/core/ir_nodes.def"
	};
	#undef CONCRETE


	// **********************************************************************************
	// 									Node Categories
	// **********************************************************************************

	/**
	 * Defines a set of categories nodes might belong to. Every node has to belong to
	 * exactly one of the enlisted categories.
	 */
	enum NodeCategory {
		NC_Value,			// < a leaf node representing a value
		NC_IntTypeParam,	// < a node representing an int-type-param
		NC_Type,			// < a node representing a type
		NC_Expression,		// < a node representing an expression
		NC_Statement,		// < a node representing a statement
		NC_Program,			// < a node representing a program
		NC_Composed,		// < a utility used to realize a complex data structure
		NC_Support			// < additional supporting nodes
	};




	// **********************************************************************************
	// 									Node Type Traits
	// **********************************************************************************

	namespace detail {

		/**
		 * A helper for defining node traits ...
		 */
		template<
			typename T,
			template<typename D, template <typename P> class P> class Accessor
		>
		struct node_type_helper {
			typedef T type;
			typedef Pointer<const T> ptr_type;
			typedef Address<const T> adr_type;
			typedef Accessor<Pointer<const T>, Pointer> ptr_accessor_type;
			typedef Accessor<Address<const T>, Address> adr_accessor_type;
		};

		template<
			typename T,
			NodeType N,
			template<typename D, template <typename P> class P> class Accessor
		>
		struct concrete_node_type_helper : public node_type_helper<T,Accessor> {
			enum { nt_value = N };
		};

	}

	/**
	 * A type trait linking node types to their properties.
	 */
	template<typename T>
	struct node_type;

	template<typename T>
	struct concrete_node_type;

	/**
	 * A trait struct linking node type values to the represented node's properties.
	 */
	template<NodeType typ>
	struct to_node_type;

	#define CONCRETE(NAME) \
		template<> struct concrete_node_type<NAME> : public detail::concrete_node_type_helper<NAME, NT_ ## NAME, NAME ## Accessor> {}; \
		template<> struct to_node_type<NT_ ## NAME> : public concrete_node_type<NAME> {};
	#include "insieme/core/ir_nodes.def"
	#undef CONCRETE

	#define NODE(NAME) \
		template<> struct node_type<NAME> : public detail::node_type_helper<NAME, NAME ## Accessor> {};
	#include "insieme/core/ir_nodes.def"
	#undef NODE




	// **********************************************************************************
	// 									Node Manager
	// **********************************************************************************

	/**
	 * Instances of the NodeManager class can be used to control the life cycle
	 * of IR nodes. The life cycle of every node is bound to a single manager and
	 * all children of the node have to be bound to the same manager. This constraint
	 * is automatically enforced by the node implementations.
	 */
	class NodeManager: public InstanceManager<Node, Pointer> {

		/**
		 * The data type used to maintain language extensions. Language extensions
		 * are collections of additional types and literals to be integrated within
		 * IR programs.
		 *
		 * Language extensions are handled via pointers. No smart pointer required here
		 * since ownership is never leafing the object.
		 */
		typedef std::map<const char*, lang::Extension*> ExtensionMap;

		/**
		 * An instance of the basic generator offering access to essential INSPIRE
		 * specific language constructs.
		 */
//		const lang::BasicGenerator basic;

		/**
		 * The store maintaining language extensions.
		 */
		ExtensionMap extensions;

	public:

		/**
		 * A default constructor creating a fresh, empty node manager instance.
		 */
		NodeManager() : /* basic(*this), */ extensions() { }

		/**
		 * The destructor cleaning up all language extensions.
		 */
		~NodeManager() {
			// free all extensions
			for_each(extensions, [](const ExtensionMap::value_type& cur) {
				delete cur.second;
			});
		}

//		/**
//		 * Obtains access to the generator of the basic language constructs.
//		 */
//		const lang::BasicGenerator& getBasicGenerator() const {
//			return basic;
//		}

		/**
		 * Obtains access to a generic language extension to be offered by this
		 * node manager. If the extension has not been loaded yet, it will be during
		 * the execution of this method.
		 *
		 * @tparam E the extension to be obtained
		 * @return a reference to the requested language extension
		 */
		template<
			typename E,
			typename boost::enable_if<boost::is_base_of<lang::Extension, E>, int>::type = 0
		>
		const E& getLangExtension() {
			// look up type information within map
			const char* key = typeid(E).name();
			auto pos = extensions.find(key);
			if (pos != extensions.end()) {
				return static_cast<const E&>(*(pos->second));
			}

			// create a new instance
			extensions[key] = new E(*this);
			return getLangExtension<E>();
		}

	};



	// **********************************************************************************
	// 									Node Annotations
	// **********************************************************************************

	// a forward declaration of the node annotation class and a pointer type referencing it
	class NodeAnnotation;
	typedef std::shared_ptr<NodeAnnotation> NodeAnnotationPtr;

	/**
	 * An abstract super type for all annotations being attached to nodes. In addition to the
	 * usual annotation requirements, node annotations have to support the migration between
	 * nodes during transformations.
	 */
	class NodeAnnotation : public utils::Annotation {
	public:

		/**
		 * A method which will be invoked whenever a node with this annotation is
		 * transformed. If the annotation should be preserved, this method has to migrate
		 * itself to the given after node. During this migration, necessary modifications
		 * on the annotations may as well be applied.
		 *
		 * @param ptr the shared annotation pointer referencing this annotation within the before node
		 * @param before the node state before the transformation having this annotation attached to
		 * @param after the node state after the transformation, which might have to be updated
		 * @return true if a migration took place, false otherwise
		 */
		virtual bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const { return false; };
	};




	// **********************************************************************************
	// 							    Abstract Node Base
	// **********************************************************************************

	/**
	 * The class represents the abstract base class of all IR nodes. This calls defines
	 * the basic interfaces and implements all essential methods on nodes. Subclasses of
	 * this class are not allowed to contain any additional value-relevant fields. Hence,
	 * equality, the hash value or any other property if immutable instances of this type
	 * can be solely decided by the values stored within this basic implementation.
	 *
	 * Nodes are forming trees. Trees are formed by the following grammar:
	 *
	 * 		T := Value | NodeType(T*)
	 *
	 * Hence, a tree is either a Value or a node with a type and a list of child nodes.
	 */
	class Node :
			public utils::HashableImmutableData<Node>,			// Nodes are immutable data objects!
			public utils::Annotatable<NodeAnnotation>, 			// Nodes can be annotated
			public utils::Printable {							// Allows instances to be printed

			/**
			 * Allow the instance manager to access private methods to create / destroy nodes.
			 */
			friend class InstanceManager<Node, Pointer>;

		public:

			/**
			 * The type of instance manager to be used with this type.
			 */
			typedef NodeManager Manager;

			/**
			 * The union of all the values which can directly be represented using nodes. If
			 * a node represents a value, it is representing a value of this type.
			 */
			typedef boost::variant<bool,int,unsigned,string> Value;

		private:

			/**
			 * The type of node the concrete instance is representing.
			 */
			NodeType nodeType;

			/**
			 * The list of child nodes of this node (will be empty
			 * if this node is a value node).
			 */
			NodeList children;

			/**
			 * The value represented by this node (only valid if it
			 * is in deed a value node).
			 */
			Value value;


			// --------------------------------------------
			//    Additional, non-data relevant members
			// --------------------------------------------

			/**
			 * The category of node to be represented by this instance.
			 */
			NodeCategory nodeCategory;

			/**
			 * A pointer to the manager this instance is maintained by.
			 */
			NodeManager* manager;

			/**
			 * The type used for representing equality IDs.
			 */
			typedef uint64_t EqualityID;

			/**
			 * A static generator for generating equality class IDs
			 */
			static utils::SimpleIDGenerator<EqualityID> equalityClassIDGenerator;

			/**
			 * The ID of the equality class of this node. This ID is used to significantly
			 * speed up the equality check.
			 */
			mutable EqualityID equalityID;

		protected:

			/**
			 * Construct a new node instance based on the essential features. The resulting
			 * node will be a non-value node according to the second rule of the grammar.
			 *
			 * @param nodeType the type of node to be created
			 * @param nodeCategory the category of the node to be created
			 * @param hashCode the hash code of the new node
			 */
			template<typename ... Nodes>
			Node(const NodeType nodeType, const NodeCategory nodeCategory, const Pointer<const Nodes>& ... children)
				: HashableImmutableData(hashNodes(nodeType, children ...)),
				  nodeType(nodeType), children(toVector<NodePtr>(children...)),
				  nodeCategory(nodeCategory), manager(0), equalityID(0) {

				// ensure that there no non-value node is of the value type
				assert(nodeCategory != NC_Value && "Must not be a value node!");
			}

			/**
			 * Constructs a new node instance based on the given value. The resulting node
			 * represent the given value.
			 *
			 * @param nodeType the type of node to be represented
			 * @param value the value to be represented
			 */
			Node(const NodeType nodeType, const Value& value);

			/**
			 * Constructs a new node instance based on the given type, category and child list.
			 *
			 * @param nodeType the type of the resulting node
			 * @param nodeCategory the category of the resulting node
			 * @param children the list of children to be used when constructing the new node
			 */
			Node(const NodeType nodeType, const NodeCategory nodeCategory, const NodeList& children);

			/**
			 * Defines the new operator to be protected. This prevents instances of AST nodes to be
			 * created on the heap or stack without a NodeManager, thereby enforcing the usage of the
			 * static factory methods and NodeManager.
			 */
			static void* operator new(size_t size){
				return ::operator new(size);
			}

			/**
			 * Defines the delete operator to be protected. This prevents instances of AST nodes to be
			 * created on the heap or stack without a NodeManager, thereby enforcing the usage of the
			 * static factory methods and NodeManager.
			 */
			void operator delete(void* ptr) {
				return ::operator delete(ptr);
			}

			/**
			 * Defines the new operator for arrays to be protected. This prevents instances of AST nodes to be
			 * created on the heap or stack without a NodeManager, thereby enforcing the usage of the
			 * static factory methods and NodeManager.
			 */
			static void* operator new[](size_t);

			/**
			 * Defines the delete operator for arrays to be protected. This prevents instances of AST nodes to be
			 * created on the heap or stack without a NodeManager, thereby enforcing the usage of the
			 * static factory methods and NodeManager.
			 */
			void operator delete[](void*, size_t);

		public:

			/**
			 * Determines the type of this node.
			 *
			 * @return the node type of this instance
			 */
			NodeType getNodeType() const {
				return nodeType;
			}

			/**
			 * Determines the category of this node.
			 *
			 * @return the node category of this instance
			 */
			NodeCategory getNodeCategory() const {
				return nodeCategory;
			}

			/**
			 * Determines whether this node is representing a value or not.
			 *
			 * @return true if it is a value type, false otherwise
			 */
			bool isValue() const {
				return nodeCategory == NC_Value;
			}

			/**
			 * Obtains access to a concrete child of this node.
			 *
			 * @param index the index of the child
			 * @return a pointer to the requested child
			 */
			const NodePtr& getChild(std::size_t index) const {
				assert(!isValue() && "Node represents a value!");
				assert((index < children.size()) && "Index out of bound!");
				return children[index];
			}

			/**
			 * Obtains a reference to the entire list of children stored internally.
			 *
			 * @return a reference to the internally maintained child list
			 */
			const NodeList& getChildList() const {
				assert(!isValue() && "Node represents a value!");
				return children;
			}

			/**
			 * Obtains a reference to the manager maintaining this node instance. In case this
			 * node is not managed by any node manager (by any reason), an assertion will be violated.
			 *
			 * @return a reference to the manager maintaining this node
			 */
			inline NodeManager& getNodeManager() const {
				assert(manager && "NodeManager must not be null - unmanaged node detected!");
				return *manager;
			}

			/**
			 * Obtains a pointer to the manager maintaining this node instance. In case this
			 * node is not managed by any node manager (by any reason), NULL will be returned.
			 *
			 * @return a pointer to the manager maintaining this node
			 */
			inline NodeManager* getNodeManagerPtr() const {
				return manager;
			}

			/**
			 * Creates a new version of this node where every reference to a child node
			 * is replaced by a pointer to the node returned by the given mapper.
			 *
			 * @param manager the manager to be used to create the new node
			 * @param mapper the mapper used to translate child node references
			 * @return a pointer to the modified node.
			 */
			NodePtr substitute(NodeManager& manager, NodeMapping& mapper) const;

			/**
			 * A default implementation of the equals operator comparing the actual
			 * names of the types.
			 */
			bool operator==(const Node& other) const {
				// test for identity (most identifying)
				if (this == &other) {
					return true;
				}

				// quick hash code test (most distinctive)
				if (hash() != other.hash()) {
					return false;
				}

				// check node type (almost most distinctive)
				if (nodeType != other.nodeType) {
					return false;
				}

				// check equality ID (having different IDs does not mean it is different)
				if (equalityID != 0 && other.equalityID != 0 && equalityID == other.equalityID) {
					// has been identified to be equivalent earlier
					return true;
				}

				// check whether values are represented
				bool res = true;
				res = res && isValue() == other.isValue();

				// compare content ..
				if (isValue()) {
					// .. if it is a value
					res = res && getValue() == other.getValue();
				} else {
					// .. if it is a inner node
					res = res && getNodeType() == other.getNodeType();
					res = res && ::equals(getChildList(), other.getChildList(), equal_target<NodePtr>());
				}

				// infect both nodes with a new ID
				if (res) {
					// update equality IDs - both should have the same id
					if (equalityID == 0 && other.equalityID == 0) {
						// non is set yet => pick a new ID and use for both
						equalityID = equalityClassIDGenerator.getNext();
						other.equalityID = equalityID;
					} else if (equalityID == 0) {
						// other.equalityID != 0 ... update local ID with other ID
						equalityID = other.equalityID;
					} else if (other.equalityID == 0){
						// equality ID != 0 ... update other ID
						other.equalityID = equalityID;
					} else {
						// both are != 0
						assert(equalityID != 0 && other.equalityID != 0 && "Equality IDs should be != 0");

						// pick smaller ID for both
						if (equalityID < other.equalityID) {
							other.equalityID = equalityID;
						} else {
							equalityID = other.equalityID;
						}
					}
				}

				// return the comparison result.
				return res;
			}

			/**
			 * Compares this and the given node for inequality. A call to this
			 * is equivalent to !(*this == other).
			 *
			 * @param other the node to be compared with
			 * @return true if not equal, false otherwise
			 */
			bool operator!=(const Node& other) const {
				// just use inverse of == operator
				return !(*this == other);
			}

		protected:

			// ----------------------------------------------------------
			//   Access Members
			//      Those are protected and made public by sub-classes
			// ----------------------------------------------------------

			/**
			 * Obtains a reference to the value represented by this node if
			 * it is representing a value.
			 *
			 * @return a reference to the internally maintained value
			 */
			const Value& getValue() const {
				assert(isValue() && "Node does not represent a value!");
				return value;
			}

			/**
			 * Prints this node to the given stream.
			 *
			 * @param out the stream to be printed to
			 * @return the handed in stream
			 */
			virtual std::ostream& printTo(std::ostream& out) const;

			/**
			 * Compares this node instance with the given instance. Internally the
			 * operator== is used for comparison.
			 *
			 * This function is required by the HashableImmutableData base class.
			 *
			 * @param other the node to be compared with
			 * @return true if equal, false otherwise
			 */
			virtual bool equals(const Node& other) const {
				// use equal operator
				return *this == other;
			}

			/**
			 * This function is required to be implemented by sub-classes by instantiating
			 * a new instance of the corresponding sub-type based on the given list of child
			 * list.
			 *
			 * @param children the children to be used for the construction
			 * @return a pointer to a new, fresh instance of the requested node
			 */
			virtual Node* createInstanceUsing(const NodeList& children) const =0;

		private:

			/**
			 * Retrieves a clone of this node, hence a newly allocated instance representing the same value
			 * as the current instance. The new instance (and all its referenced nodes) should be maintained
			 * by the given manager.
			 *
			 * @param manager the manager which should maintain all referenced nodes
			 * @return a clone of this instance referencing elements maintained exclusively by the given manager
			 */
			const Node* cloneTo(NodeManager& manager) const;

			/**
			 * A static utility function used for hashing a node type and its child nodes
			 * during the construction of a new node.
			 *
			 * @param type the type of the node to be hashed
			 * @param nodes its child nodes
			 * @return a hash value for the resulting node
			 */
			template<typename ... Nodes>
			inline std::size_t static hashNodes(NodeType type, const Nodes& ... nodes) {
				std::size_t seed = 0;
				boost::hash_combine(seed, type);
				utils::appendHash<NodePtr, deref<NodePtr>>(seed, nodes ...);
				return seed;
			}

	};




	// **********************************************************************************
	// 							    Node Utilities
	// **********************************************************************************

	/**
	 * A type trait determining the type of a node child.
	 *
	 * @tparam Node the node type to be queried
	 * @tparam index the index of the child node to be queried
	 */
	template<typename Node, unsigned index>
	struct node_child_type {
		typedef decltype(((Node*)0)->template getElement<index>()) ptr_type;
		typedef typename ptr_type::element_type type;
	};

	/**
	 * A node utility supporting the implementation of nodes having a fixed number
	 * of child nodes. This utility is extended by the actual node (via multiple
	 * inheritance) to inherit type save operations on the child nodes.
	 *
	 * @tparam Derived the derived class, for static polymorthism
	 * @tparam Children the list of child node types
	 */
	template<
		typename Derived,
		typename ... Children
	>
	class FixedSizeNodeHelper {
	public:

		/**
		 * The mayor contribution of this helper - a type save access to the
		 * child nodes.
		 *
		 * @tparam index the index of the child to be accessed
		 * @tparam Res the type of the child, automatically inferred
		 */
		template<
			unsigned index,
			typename Res = typename type_at<index, type_list<Children...>>::type
		>
		const Pointer<const Res> getElement() const {
			// access the child via static polymorthism and cast result to known type
			return static_pointer_cast<const Res>(static_cast<const Derived*>(this)->getChild(index));
		}

	};

	/**
	 * A node utility supporting the implementation of variable sized list like
	 * node types - consisting of a list of nodes of equal types.
	 *
	 * @tparam Derived the derived class, for static polymorthism
	 * @tparam ValueType the type of the nodes to be listed
	 */
	template<
		typename Derived,
		typename ValueType
	>
	class ListNodeHelper {

	public:

		/**
		 * The mayor contribution of this helper - a type save access to the
		 * child nodes.
		 *
		 * @tparam index the index of the child to be accessed
		 */
		template<unsigned index>
		const Pointer<ValueType> getElement() const {
			return static_pointer_cast<ValueType>(static_cast<const Derived*>(this)->getChild(index));
		}

	};


	/**
	 * A default implementation for a node accessor to be used whenever the derived
	 * type is a node itself. Derivations of this class should be used as the base type
	 * for all node accessors.
	 *
	 * @tparam Derived the type which is extended by this accessor (static polymorthism)
	 * @tparam Ptr the type of pointer to be obtained by
	 */
	template<typename Derived,template<typename T> class Ptr = Pointer>
	struct NodeAccessor {

		/**
		 * Obtains access to the accessed node.
		 */
		inline const Derived& getNode() const {
			return *static_cast<const Derived*>(this);
		}
	};

	/**
	 * A specialization of the NodeAccessor template which will be used in cases where
	 * the accessor is inherited by a pointer (to support access to the same elements
	 * as for pointers and nodes directly.
	 */
	template<typename Node, template<typename T> class Ptr>
	struct NodeAccessor<Pointer<const Node>, Ptr> {
		const Node& getNode() const {
			return *(static_cast<const Pointer<const Node>*>(this));
		}
	};

	/**
	 * A specialization of the NodeAccessor template which will be used in cases where
	 * the accessor is inherited by an address (to support access to the same elements
	 * as for pointers and nodes directly.
	 */
	template<typename Node, template<typename T> class Ptr>
	struct NodeAccessor<Address<const Node>, Ptr> {
		const Node& getNode() const {
			return *(static_cast<const Address<const Node>*>(this)->getAddressedNode());
		}
	};



	// **********************************************************************************
	// 							    Node Macros
	// **********************************************************************************

	/**
	 * The following macros should be used to simplify the definition of new node types
	 * within the corresponding header files. They should not be used by the end user.
	 */

	/**
	 * A macro starting a node declaration with the given name and base type.
	 */
	#define IR_NODE(NAME, BASE) \
		class NAME : public BASE, public NAME ## Accessor<NAME> { \
		\
		protected: \
			/* The function required for the clone process. */ \
			virtual NAME* createInstanceUsing(const NodeList& children) const { \
				return new NAME(children); \
			} \
		public: \
			/* A factory method creating instances based on a child list */ \
			static NAME ## Ptr get(NodeManager& manager, const NodeList& children) { \
				return manager.get(NAME(children)); \
			} \
		private: \


	/**
	 * Starts the definition of an accessor struct which is determining how fields of
	 * a node can be accessed by linking names to child node IDs. Properties within
	 * accessors can be defined easiest using the NODE_PROPERTY macro.
	 *
	 * @param NAME the name of the node type this accessor is accociated to
	 * @param ... the types of the child nodes of the accociated node
	 */
	#define IR_NODE_ACCESSOR(NAME, ... ) \
		template<typename Derived,template<typename T> class Ptr = Pointer> \
		struct NAME ## Accessor : public FixedSizeNodeHelper<Derived, __VA_ARGS__>, public NodeAccessor<Derived, Ptr>

	/**
	 * A macro adding new properties to an accessor by linking a name to a
	 * type and an index within the child list.
	 *
	 * @param TYPE the type of the property
	 * @param NAME the name of the property
	 * @param INDEX the index of the property within the child list
	 */
	#define IR_NODE_PROPERTY(TYPE, NAME, INDEX) \
		Ptr<const TYPE> get ## NAME() const { return static_cast<const Derived*>(this)->template getElement<INDEX>(); }


} // end namespace new_core
} // end namespace core
} // end namespace insieme

namespace std {

	/**
	 * Allows node types to be printed using names.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::new_core::NodeType& type);

} // end namespace std
