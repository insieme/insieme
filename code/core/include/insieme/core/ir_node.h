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

#include <boost/mpl/or.hpp>

#include "insieme/utils/annotation.h"
#include "insieme/utils/hash_utils.h"
#include "insieme/utils/id_generator.h"
#include "insieme/utils/instance_manager.h"
#include "insieme/utils/range.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_mapper.h"
#include "insieme/core/ir_node_traits.h"
#include "insieme/core/ir_pointer.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/extension.h"

namespace insieme {
namespace core {


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
			public utils::Printable {							// Allows instances to be printed

			/**
			 * Allow the instance manager to access private methods to create / destroy nodes.
			 */
			friend class InstanceManager<Node, Pointer>;

			/**
			 * The Node Accessor may access any internal data element.
			 */
			template<typename Derived,template<typename T> class Ptr>
			friend struct NodeAccessor;

		public:

			/**
			 * The type of instance manager to be used with this type.
			 */
			typedef NodeManager Manager;

			/**
			 * The container used to handle annotations.
			 */
			typedef utils::Annotatable<NodeAnnotation> annotation_container;

			/**
			 * The type of the map within the annotation container.
			 */
			typedef annotation_container::annotation_map_type annotation_map_type;

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
			NodeValue value;


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

			/**
			 * The annotatable part of the node.
			 */
			const annotation_container annotations;

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
			Node(const NodeType nodeType, const NodeValue& value);

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

		protected:

			/**
			 * Determines the type of this node.
			 *
			 * @return the node type of this instance
			 */
			NodeType getNodeTypeInternal() const {
				return nodeType;
			}

			/**
			 * Determines the category of this node.
			 *
			 * @return the node category of this instance
			 */
			NodeCategory getNodeCategoryInternal() const {
				return nodeCategory;
			}

			/**
			 * Determines whether this node is representing a value or not.
			 *
			 * @return true if it is a value type, false otherwise
			 */
			bool isValueInternal() const {
				return nodeCategory == NC_Value;
			}

			/**
			 * Obtains access to a concrete child of this node.
			 *
			 * @param index the index of the child
			 * @return a pointer to the requested child
			 */
			const NodePtr& getChildInternal(std::size_t index) const {
				assert((index < children.size()) && "Index out of bound!");
				return children[index];
			}

			/**
			 * Obtains a reference to the entire list of children stored internally.
			 *
			 * @return a reference to the internally maintained child list
			 */
			const NodeList& getChildListInternal() const {
				return children;
			}

			/**
			 * Obtains a reference to the manager maintaining this node instance. In case this
			 * node is not managed by any node manager (by any reason), an assertion will be violated.
			 *
			 * @return a reference to the manager maintaining this node
			 */
			inline NodeManager& getNodeManagerInternal() const {
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
			NodePtr substituteInternal(NodeManager& manager, NodeMapping& mapper) const;

		public:

			/**
			 * Obtains a reference to the associated annotation container.
			 */
			const annotation_container& getAnnotationContainer() const {
				return annotations;
			}

			/**
			 * Obtains a reference to the entire list of children stored internally.
			 *
			 * @return a reference to the internally maintained child list
			 */
			const NodeList& getChildNodeList() const {
				return children;
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
				res = res && isValueInternal() == other.isValueInternal();

				// compare content ..
				if (isValueInternal()) {
					// .. if it is a value
					res = res && getNodeValue() == other.getNodeValue();
				} else {
					// .. if it is a inner node
					res = res && getNodeTypeInternal() == other.getNodeTypeInternal();
					res = res && ::equals(getChildListInternal(), other.getChildListInternal(), equal_target<NodePtr>());
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

			/**
			 * Compares two node instances. The comparison is based on the hash value
			 * of the nodes and is not following any rules. However, the implementation of this
			 * operator allows nodes to be managed inside a comparison based data structure like
			 * a tree.
			 *
			 * @param other the node to be compared with
			 * @return true if the hash value of this node is smaller than the hash value of the other node
			 */
			bool operator<(const Node& other) const {
				return hash() < other.hash();
			}

		protected:

			/**
			 * Obtains a reference to the value represented by this node if
			 * it is representing a value.
			 *
			 * @return a reference to the internally maintained value
			 */
			const NodeValue& getNodeValue() const {
				assert(isValueInternal() && "Node does not represent a value!");
				return value;
			}

			/**
			 * Prints this node to the given stream. The actual implementation of this
			 * function is left over to the concrete node implementations.
			 *
			 * @param out the stream to be printed to
			 * @return the handed in stream
			 */
			virtual std::ostream& printTo(std::ostream& out) const =0;

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
				utils::appendHash<deref>(seed, nodes ...);
				return seed;
			}

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
		 * A struct summarizing data managed by the root of a node manager hierarchy.
		 */
		struct NodeManagerData {

			/**
			 * A reference to the root manager owning this data object.
			 */
			NodeManager& root;

			/**
			 * An instance of the basic generator offering access to essential INSPIRE
			 * specific language constructs.
			 */
			const lang::BasicGenerator basic;

			/**
			 * The store maintaining language extensions.
			 */
			ExtensionMap extensions;

			/**
			 * A static generator for generating IDs
			 */
			utils::SimpleIDGenerator<unsigned> idGenerator;

			/**
			 * A constructor for this data structure.
			 */
			NodeManagerData(NodeManager& manager)
				: root(manager), basic(manager) {};

			/**
			 * A destructor for this data structure cleaning up extensions.
			 */
			~NodeManagerData() {
				// free all extensions
				for_each(extensions, [](const ExtensionMap::value_type& cur) {
					delete cur.second;
				});
			}
		};

		/**
		 * A pointer to the node manager data struct managed by the root of the hierarchy
		 * this node manager is part of.
		 */
		NodeManagerData* data;

	public:

		/**
		 * A default constructor creating a fresh, empty node manager instance.
		 */
		NodeManager() : data(new NodeManagerData(*this)) { }

		/**
		 * A constructor to be used for manager-chaining, allowing to build hierarchies
		 * of node managers modeling multiple nested scopes.
		 *
		 * @param manager the manager covering the surrounding scope; the life cycle of the
		 * handed in manager has to exceed the life cycle of this manager
		 */
		explicit NodeManager(NodeManager& manager)
			: InstanceManager<Node, Pointer>(manager), data(manager.data) {}

		/**
		 * Creates a new node manager using the given id as its initial fresh id.
		 */
		explicit NodeManager(unsigned initialFreshID) : data(new NodeManagerData(*this)) { setNextFreshID(initialFreshID); }

		/**
		 * The destructor cleaning up all language extensions.
		 */
		~NodeManager() {
			// extensions are only handled by root manager
			if (getBaseManager()) { return; }

			// clear attached data struct
			delete data;
		}

		/**
		 * Obtains a pointer to the base manager this manager is attached to or
		 * Null if this manager is the root of the manager hierarchy.
		 */
		NodeManager* getBaseManager() {
			return static_cast<NodeManager*>(InstanceManager<Node, Pointer>::getBaseManager());
		}

		/**
		 * Obtains access to the generator of the basic language constructs.
		 */
		const lang::BasicGenerator& getLangBasic() const {
			return data->basic;
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
			auto& ext = data->extensions;
			auto pos = ext.find(key);
			if (pos != ext.end()) {
				return static_cast<const E&>(*(pos->second));
			}

			// create a new instance
			ext[key] = new E(data->root);
			return getLangExtension<E>();
		}

		/**
		 * Obtains a fresh ID to be used within a node.
		 */
		unsigned getFreshID() {
			return data->idGenerator.getNext();
		}

		/**
		 * Resets the internal ID generator used for producing fresh IDs to continue
		 * using the given value.
		 */
		void setNextFreshID(unsigned value) {
			data->idGenerator.setNext(value);
		}

	};



	// **********************************************************************************
	// 							    Node Utilities
	// **********************************************************************************

	/**
	 * A utility allowing to convert constant vectors of pointers to derived types to vectors
	 * of pointers to base types.
	 *
	 * @tparam B the target-base type
	 * @tparam D the derived type
	 * @param list the list to be converted
	 */
	template<
		typename B = Node, template<typename T> class Ptr, typename D,
		typename boost::enable_if<boost::mpl::or_<boost::is_base_of<B,D>,boost::is_base_of<D,B>>,int>::type = 0
	>
	const vector<Ptr<const B>>& convertList(const vector<Ptr<const D>>& list) {
		// use a C-like cast since structurally the data is correct
		// if this ever causes troubles, replace it by copying the vector
		return (const vector<Ptr<const B>>&)list;
	}

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
		 * A constructor required for the uniform handling of helpers accepting
		 * the child node list of the constructed node checking the composition
		 * of the child nodes.
		 */
		FixedSizeNodeHelper(const NodeList& children) {
			// verify the proper composition of the child node list
			assert(checkChildList(children) && "Invalid composition of Child-Nodes discovered!");
		}

		/**
		 * Obtains access to the child associated to the given index.
		 * @param index the index of the child node to be accessed
		 */
		Pointer<const Node> getChildNodeReference(std::size_t index) const {
			return static_cast<const Derived*>(this)->getChildList()[index];
		}

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
		Pointer<const Res> getChildNodeReference() const {
			// access the child via static polymorthism and cast result to known type
			return static_pointer_cast<const Res>(static_cast<const Derived*>(this)->getChild(index));
		}


		/**
		 * Checks whether the nodes within the given list are valid to be child nodes of
		 * an instance of this fixed sized node type.
		 */
		static bool checkChildList(const NodeList& list) {
			// the list has to have the correct length and composition of types
			return list.size() == type_list<Children...>::length
					&& checkTypes<NodeList::const_iterator, Children...>(list.begin(), list.end());
		}

	private:

		/**
		 * Checks whether the given range is composed of node pointers of the given types.
		 *
		 * @tparam First the first type to be occuring within the list
		 * @tparam Rest the remaining types to be occuring wihtin the list
		 * @param begin the begin of the range
		 * @param end the end of the range to be checked
		 */
		template<typename Iterator, typename First, typename ... Rest>
		static bool checkTypes(const Iterator& begin, const Iterator& end) {
			return dynamic_pointer_cast<const First>(*begin) && checkTypes<Iterator, Rest...>(begin+1, end);
		}

		/**
		 * The terminal case of the type check, in case there are no more types to be checked.
		 *
		 * @param begin the begin of the range to be checked
		 * @param end the end of the range to be checked
		 */
		template<typename Iterator>
		static bool checkTypes(const Iterator& begin, const Iterator& end) {
			// the iterator has to have reached the end
			return begin == end;
		}

	};

	/**
	 * A node utility supporting the implementation of variable sized list like
	 * node types - consisting of a list of a fixed list of heterogeneous nodes
	 * followed by a variable length list of equal types.
	 *
	 * @tparam Derived the derived class, for static polymorthism
	 * @tparam First the first list of the child list types
	 * @tparam Rest the remaining types of the child list types
	 *
	 * The List [First, Rest...] is forming the child list nodes. The last element
	 * of this list is used as the element type of the list.
	 */
	template<
		typename Derived,
		typename First,
		typename ... Rest
	>
	class ListNodeHelper {

		// extract last type from list => this is the list type
		typedef typename type_at<
				type_list<First, Rest ...>::length - 1,
				type_list<First, Rest ...>
		>::type ListValueType;

		/**
		 * The list of children forming the list part of this node.
		 */
		vector<Pointer<const ListValueType>> listElements;

	public:

		/**
		 * This constructor initializes the internally stored element list.
		 *
		 * @param allChildren all child nodes of the node this helper is assisting.
		 */
		ListNodeHelper(const NodeList& children)
			: listElements(
					convertList<ListValueType>(children).begin() + (type_list<First, Rest ...>::length - 1),
					convertList<ListValueType>(children).end()
			) {

			// verify the proper composition of the child node list
			assert(checkChildList(children) && "Invalid composition of Child-Nodes discovered!");
		}

		/**
		 * Obtains a reference to the internally stored list of elements within the list
		 * section of the child list.
		 */
		const vector<Pointer<const ListValueType>>& getElementList() const {
			return listElements;
		}

		/**
		 * Obtains access to the child associated to the given index.
		 * @param index the index of the child node to be accessed
		 */
		Pointer<const Node> getChildNodeReference(std::size_t index) const {
			return static_cast<const Derived*>(this)->getChildList()[index];
		}

		/**
		 * The mayor contribution of this helper - a type save access to the
		 * child nodes. The implementation is separated into two versions. The
		 * first case handles accesses to nodes within the fixed child-node list
		 * subrange (all but the last node). In this case, the resulting type
		 * corresponds to the type at the corresponding position within the type
		 * list.
		 *
		 * @tparam index the index of the child to be accessed
		 * @return a pointer to the corresponding element of the corresponding type
		 */
		template<
			unsigned index,
			typename boost::enable_if_c<(index+1 < type_list<First, Rest ...>::length),int>::type = 0,
			typename Res = typename type_at<index, type_list<First, Rest ...>>::type
		>
		Pointer<const Res> getChildNodeReference() const {
			// access the child via static polymorthism and cast result to known type
			return static_pointer_cast<const Res>(static_cast<const Derived*>(this)->getChild(index));
		}

		/**
		 * The second case of the getChildNodeReference() implementation accessing
		 * nodes within the list-part of this ListNode.
		 *
		 * @tparam index the index of the element to be accessed
		 */
		template<
			unsigned index,
			typename boost::disable_if_c<(index+1 < type_list<First, Rest ...>::length),int>::type = 0
		>
		Pointer<const ListValueType> getChildNodeReference() const {
			// access the child via static polymorthism and cast result to known type
			return static_pointer_cast<const ListValueType>(static_cast<const Derived*>(this)->getChild(index));
		}

		/**
		 * Checks whether the nodes within the given list are valid to be child nodes of
		 * an instance of this list node type.
		 */
		static bool checkChildList(const NodeList& list) {
			// the list has to have the correct length and composition of types
			return list.size() >= type_list<First, Rest...>::length-1
					&& checkTypes<NodeList::const_iterator, First, Rest...>(list.begin(), list.end());
		}

	private:

		/**
		 * Checks whether the given range is composed of node pointers of the given types.
		 *
		 * @tparam A the first type to be occurring within the list
		 * @tparam B the second type to be occurring within the list
		 * @tparam Cs the remaining types to be occurring within the list
		 * @param begin the begin of the range
		 * @param end the end of the range to be checked
		 */
		template<typename Iterator, typename A, typename B, typename ... Cs>
		static bool checkTypes(const Iterator& begin, const Iterator& end) {
			return dynamic_pointer_cast<const A>(*begin) && checkTypes<Iterator, B, Cs...>(begin+1, end);
		}

		/**
		 * The terminal case of the type check. In this case, only the list-type remains to
		 * be checked. All remaining elements have to be of the list-element type.
		 *
		 * @tparam ElementType the element type to be checked. It is the last type when recursively resolving the list.
		 * @param begin the begin of the range to be checked
		 * @param end the end of the range to be checked
		 */
		template<typename Iterator, typename ElementType>
		static bool checkTypes(const Iterator& begin, const Iterator& end) {
			// check the content of the list - all need to by castable to the given value type
			return all(begin, end, [](const NodePtr& cur) { return (bool)dynamic_pointer_cast<Pointer<const ElementType>>(cur); });
		}

	};


	template<typename Ptr, typename Iter = typename vector<Ptr>::const_iterator>
	struct NodeRange : public utils::range<Iter> {
		NodeRange(const Iter& begin, const Iter& end) : utils::range<Iter>(begin, end) {}
	};

	template<
		typename Derived,
		template<typename T> class Ptr,
		template<typename D,template<typename T> class P> class BaseAccessor,
		typename First,
		typename ... Rest
	>
	struct ListNodeAccessor : public BaseAccessor<Derived,Ptr> {

		// extract last type from list => this is the list type
		typedef typename type_at<
				type_list<First, Rest ...>::length - 1,
				type_list<First, Rest ...>
		>::type ElementType;

		// the number of static values before the first list element
		static const unsigned offset = type_list<First, Rest ...>::length - 1;

		/**
		 * A type definition for the type of iterator offered by this accessor.
		 */
		typedef typename vector<Ptr<const ElementType>>::const_iterator const_iterator;

		/**
		 * Obtains access to an element within this list.
		 */
		const Ptr<const ElementType>& getElement(std::size_t index) const {
			assert(index < size() && "Element index out of bound!");
			return convertList<ElementType>(BaseAccessor<Derived,Ptr>::getChildList())[offset + index];
		}

		/**
		 * Obtains access to a specific element within this list.
		 */
		const Ptr<const ElementType>& operator[](std::size_t index) const {
			return getElement(index);
		}


		/**
		 * Obtains a reference to the list of internally maintained elements.
		 */
		const NodeRange<Ptr<const ElementType>> getElements() const {
			// obtain the sub-range covering the actual elements of this list node
			const auto& all = convertList<ElementType>(BaseAccessor<Derived,Ptr>::getChildList());
			return NodeRange<Ptr<const ElementType>>(all.begin() + offset, all.end());
		}

		/**
		 * Obtains the number of elements within this list.
		 */
		std::size_t size() const {
			return getList().size();
		}

		/**
		 * Checks whether the represented list of elements is empty or not.
		 *
		 * @return true if empty, false otherwise
		 */
		bool empty() const {
			return getList().empty();
		}

		/**
		 * Obtains an iterator pointing to the first element of this list node.
		 */
		const_iterator begin() const {
			return getElements().begin();
		}

		/**
		 * Obtains an iterator pointing to the end of the elements contained
		 * within the list.
		 */
		const_iterator end() const {
			return getElements().end();
		}

	private:

		/**
		 * An internal utility obtaining the vector containing all elements
		 * contained within this list node.
		 *
		 * @return a reference to the contained elements
		 */
		const vector<Pointer<const ElementType>>& getList() const {
			return convertList<ElementType>(BaseAccessor<Derived,Ptr>::getNode().getElementList());
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
		class NAME : public BASE, public NAME ## Accessor<NAME, Pointer>, public NAME ## Accessor<NAME, Pointer>::node_helper { \
			NAME(const NodeList& children) \
				: BASE(NT_ ## NAME, children), NAME ## Accessor<NAME, Pointer>::node_helper(getChildNodeList()) {} \
			template<typename ... Children> \
			NAME(const Pointer<const Children>& ... children) \
				: BASE(NT_ ## NAME, children ...), NAME ## Accessor<NAME, Pointer>::node_helper(getChildNodeList()) {} \
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
	 * @param NAME the name of the node type this accessor is associated to
	 * @param ... the types of the child nodes of the associated node
	 */
	#define IR_NODE_ACCESSOR(NAME, BASE, ... ) \
		template<typename Derived, template<typename T> class Ptr> \
		struct NAME ## Accessor : public BASE ## Accessor<Derived, Ptr> { \
			\
			typedef FixedSizeNodeHelper<Derived, ## __VA_ARGS__> node_helper; \

	#define IR_LIST_NODE_ACCESSOR(NAME, BASE, LIST_NAME, ... ) \
		template<typename Derived, template<typename T> class Ptr> \
		struct NAME ## Accessor : public ListNodeAccessor<Derived,Ptr,BASE ## Accessor, ## __VA_ARGS__> { \
			\
			typedef ListNodeHelper<Derived, ## __VA_ARGS__> node_helper; \
			\
			const NodeRange<Ptr<const typename ListNodeAccessor<Derived,Ptr,BASE ## Accessor, ## __VA_ARGS__>::ElementType>> get ## LIST_NAME () const { \
				return ListNodeAccessor<Derived,Ptr,BASE ## Accessor, ## __VA_ARGS__>::getElements(); \
			} \



	/**
	 * A macro adding new properties to an accessor by linking a name to a
	 * type and an index within the child list.
	 *
	 * @param TYPE the type of the property
	 * @param NAME the name of the property
	 * @param INDEX the index of the property within the child list
	 */
	#define IR_NODE_PROPERTY(TYPE, NAME, INDEX) \
		Ptr<const TYPE> get ## NAME() const { return static_cast<const Derived*>(this)->template getChildNodeReference<INDEX>(); }


	// -------------------------------- An abstract base type for supporting nodes ---------------------------

	/**
	 * The accessor for instances of type expressions.
	 */
	template<typename D,template<typename T> class P>
	struct SupportAccessor : public NodeAccessor<D,P> {};

	/**
	 * The base type for all support nodes. Support nodes do not represent actual language constructs.
	 * Instead, they are used to compose more complex data structures like lists and maps.
	 */
	class Support : public Node {
	protected:

			/**
			 * A constructor for this kind of nodes ensuring that every sub-class is a member of the
			 * Support node category.
			 *
			 * @param nodeType the actual node-type the resulting node will be representing
			 * @param children the child nodes to be contained
			 */
			template<typename ... Nodes>
			Support(const NodeType nodeType, const Pointer<const Nodes>& ... children)
				: Node(nodeType, NC_Support, children ...) { }

			/**
			 * A constructor creating a new instance of this type based on a given child-node list.
			 *
			 * @param nodeType the type of the newly created node
			 * @param children the child nodes to be used to create the new node
			 */
			Support(const NodeType nodeType, const NodeList& children)
				: Node(nodeType, NC_Support, children) { }

	};

	/**
	 * Checks whether the given two node pointer are the root of the same AST tree
	 * containing the same set of annotations on the nodes and the pointer between
	 * nodes.
	 *
	 * @param nodeA the root of the first tree
	 * @param nodeB the root of the second tree
	 * @return true if they are equivalent, false otherwiser
	 */
	bool equalsWithAnnotations(const NodePtr& nodeA, const NodePtr& nodeB);

} // end namespace core
} // end namespace insieme

namespace std {

	/**
	 * Allows node types to be printed using names.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::NodeType& type);

} // end namespace std


// for Debugging:

/**
 * The following list of dump-operations can be used when debugging the insieme
 * compiler within gdb to print IR nodes.
 */

void dumpText(const insieme::core::NodePtr& node);
void dumpPretty(const insieme::core::NodePtr& node);
void dumpPrettyFull(const insieme::core::NodePtr& node);



