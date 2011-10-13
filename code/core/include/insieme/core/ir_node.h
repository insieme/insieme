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

namespace insieme {
namespace core {

	// TODO: move to forward declaration
	using std::string;
	using std::pair;
	using std::vector;


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

	enum NodeCategory {};




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
			 * The type used internally to store the data associated to a node. The data
			 * might either cover a pair aggregating a NodeType and a child list or a value -
			 * depending on which grammar rule has generated the represented node.
			 */
			typedef boost::variant<pair<NodeType, NodeList>, Value> NodeData;

			/**
			 * The data defining this node. This data might either be a value or
			 * a pair aggregating a NodeType an a Child-NodeList.
			 */
			NodeData data;


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
			Node(const NodeType nodeType, const NodeCategory nodeCategory, const Ptr<Nodes>& ... children)
				: HashableImmutableData(hash(nodeType, children ...)),
				  data(std::make_pair(nodeType, toVector<NodePtr>(children...))),
				  nodeCategory(nodeCategory), equalityID(0) {}

			Node(const Value& value);

			Node(const Node& node);

			/**
			 * Defines the new operator to be protected. This prevents instances of AST nodes to be
			 * created on the heap, thereby enforcing the usage of the static factory methods and
			 * NodeManager.
			 */
			static void* operator new(size_t);

			/**
			 * Defines the delete operator to be protected. This prevents instances of AST nodes to be
			 * created on the heap, thereby enforcing the usage of the static factory methods and
			 * NodeManager.
			 */
			void operator delete(void*);

			/**
			 * Defines the new operator for arrays to be protected. This prevents instances of AST nodes to be
			 * created on the heap, thereby enforcing the usage of the static factory methods and
			 * NodeManager.
			 */
			static void* operator new[](size_t);

			/**
			 * Defines the delete operator for arrays to be protected. This prevents instances of AST nodes to be
			 * created on the heap, thereby enforcing the usage of the static factory methods and
			 * NodeManager.
			 */
			void operator delete[](void*, size_t);

		public:

			bool isValue() const;

			NodeType getNodeType() const {
				return boost::get<pair<NodeType, NodeList>>(data).first;
			}

			const NodePtr& getChild(std::size_t index) const {
				return boost::get<pair<NodeType, NodeList>>(data).second[index];
			}

			const NodeList& getChildList() const {
				return boost::get<pair<NodeType, NodeList>>(data).second;
			}

			virtual std::ostream& printTo(std::ostream& out) const;

		protected:

			virtual bool equals(const Node& other) const { return this == &other; }

		private:

			template<typename ... Nodes>
			inline std::size_t static hash(NodeType type, const Nodes& ... nodes) {
				std::size_t seed;
				boost::hash_combine(seed, type);
				utils::appendHash<NodePtr, deref<NodePtr>>(seed, nodes ...);
				return seed;
			}

	};

	// -- Node Utilities -----------------

	// FixedSizeNode
	// ListNode

} // end namespace core
} // end namespace insieme
