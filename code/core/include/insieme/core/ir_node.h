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
#include "insieme/core/ir_pointer.h"
#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {


	// **********************************************************************************
	// 									Node Types
	// **********************************************************************************

	enum NodeType {};

	// - node type enumeration
	// - number of node types
	// - node type trait linking enum to type and visa vice



	// **********************************************************************************
	// 									Node Categories
	// **********************************************************************************

	enum NodeCategory {
		NC_Value,
		NC_Type,
		NC_Expression,
		NC_Statement,
		NC_Program,
		NC_Composed
	};




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
		const lang::BasicGenerator basic;

		/**
		 * The store maintaining language extensions.
		 */
		ExtensionMap extensions;

	public:

		/**
		 * A default constructor creating a fresh, empty node manager instance.
		 */
		NodeManager() : basic(*this), extensions() { }

		/**
		 * The destructor cleaning up all language extensions.
		 */
		~NodeManager() {
			// free all extensions
			for_each(extensions, [](const ExtensionMap::value_type& cur) {
				delete cur.second;
			});
		}

		/**
		 * Obtains access to the generator of the basic language constructs.
		 */
		const lang::BasicGenerator& getBasicGenerator() const {
			return basic;
		}

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
			friend class InstanceManager<Node, Pointer> ;

		public:

			/**
			 * The union of all the values which can directly be represented using nodes. If
			 * a node represents a value, it is representing a value of this type.
			 */
			typedef boost::variant<bool,int,string> Value;

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
			Node(const NodeType nodeType, const NodeCategory nodeCategory, const Pointer<Nodes>& ... children)
				: HashableImmutableData(hashNodes(nodeType, children ...)),
				  nodeType(nodeType), children(toVector<NodePtr>(children...)),
				  nodeCategory(nodeCategory), equalityID(0) {

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
			 * Defines the new operator to be protected. This prevents instances of AST nodes to be
			 * created on the heap or stack without a NodeManager, thereby enforcing the usage of the
			 * static factory methods and NodeManager.
			 */
			static void* operator new(size_t);

			/**
			 * Defines the delete operator to be protected. This prevents instances of AST nodes to be
			 * created on the heap or stack without a NodeManager, thereby enforcing the usage of the
			 * static factory methods and NodeManager.
			 */
			void operator delete(void*);

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

		private:

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
	 * A node extension forming a common sub-class of all value nodes.
	 */
	class ValueNode : public Node {

	protected:

		/**
		 * A constructor limiting subclasses to a value-node construction.
		 *
		 * @param type the type of the resulting node
		 * @param value the value to be represented
		 */
		ValueNode(const NodeType type, const Node::Value& value) : Node(type, value) {}

	public:

		/**
		 * Obtains a reference to the value represented by this node if
		 * it is representing a value.
		 *
		 * @return a reference to the internally maintained value
		 */
		const Value& getValue() const {
			// forward call to protected parent method
			return Node::getValue();
		}

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
		 * A constructor helping to ensure the proper list of child nodes.
		 * Although it is not actually carrying out any operation it has to be
		 * invoked with the correct combination of parameters. Thereby the
		 * type of the children is checked by the compiler.
		 *
		 * @param children the children of the created node.
		 */
		FixedSizeNodeHelper(const Pointer<Children>& ... children) {}

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
		const Pointer<Res> get() const {
			// access the child via static polymorthism and cast result to known type
			return static_pointer_cast<Res>(static_cast<const Derived*>(this)->getChild(index));
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
		const Pointer<ValueType> get() const {
			return static_pointer_cast<ValueType>(static_cast<const Derived*>(this)->getChild(index));
		}

	};


} // end namespace core
} // end namespace insieme
