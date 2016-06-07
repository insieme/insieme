/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <memory>

#include "insieme/utils/annotation.h"
#include "insieme/utils/string_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/ir_node_traits.h"
#include "insieme/core/ir_node_types.h"

namespace insieme {
namespace core {


	// **********************************************************************************
	// 							      Node Accessor
	// **********************************************************************************

	namespace detail {

		/**
		 * A general implementation of the node accessor helper which will be used in cases where
		 * the accessor is inherited by a node directly.
		 */
		template <typename Derived>
		struct node_access_helper {
			/**
			 * The type of the handled node.
			 */
			typedef Derived node_type;

			/**
			 * Obtains access to the accessed node.
			 */
			inline const Derived& getNode() const {
				return *static_cast<const Derived*>(this);
			}

			/**
			 * Obtains a reference to the entire list of children stored internally.
			 *
			 * @return a reference to the internally maintained child list
			 */
			const NodeList& getChildList() const {
				return getNode().getChildNodeList();
			}
		};
	}

	/**
	 * The definition of all accessors which will be used for adding observer functions
	 * to Nodes, Node Pointers, Node Addresses or other ways of referencing nodes.
	 *
	 * @tparam Type the type for which this node accessor is adding functionality
	 * @tparam Derived the type of node inheriting this accessor
	 * @tparam Ptr the kind of pointer to be used for referencing element types
	 */
	template<typename Type, typename Derived, template <typename X> class Ptr>
	struct Accessor { /* default: no extra features */ };

	/**
	 * A default implementation for a node accessor to be used whenever the derived
	 * type is a node itself. Derivations of this class should be used as the base type
	 * for all node accessors.
	 *
	 * @tparam Derived the type which is extended by this accessor (static polymorthism)
	 * @tparam Ptr the type of pointer to be obtained by
	 */
	template<
		typename Derived,
		template <typename X> class Ptr
	>
	struct Accessor<Node,Derived,Ptr> : public detail::node_access_helper<Derived>,
										public NodeAnnotationAccessHelper<Accessor<Node,Derived,Ptr>> {
		/**
		 * A type definition for the type of the handled node.
		 */
		typedef typename detail::node_access_helper<Derived>::node_type node_type;

		/**
		 * Determines the type of this node.
		 *
		 * @return the node type of this instance
		 */
		NodeType getNodeType() const {
			return getNode().nodeType;
		}

		/**
		 * Determines the category of this node.
		 *
		 * @return the node category of this instance
		 */
		NodeCategory getNodeCategory() const {
			return getNode().nodeCategory;
		}

		/**
		 * Determines whether this node is representing a value or not.
		 *
		 * @return true if it is a value type, false otherwise
		 */
		bool isValue() const {
			return getNode().nodeCategory == NC_Value;
		}

		/**
		 * Obtains access to a concrete child of this node.
		 *
		 * @param index the index of the child
		 * @return a pointer to the requested child
		 */
		const NodePtr& getChild(std::size_t index) const {
			assert_false(isValue()) << "Node represents a value! <<< " << getNode().nodeCategory << " >>>";
			assert_lt(index, getNode().children.size()) << "Index out of bound!";
			return getNode().children[index];
		}

		/**
		 * Obtains a reference to the manager maintaining this node instance. In case this
		 * node is not managed by any node manager (by any reason), an assertion will be violated.
		 *
		 * @return a reference to the manager maintaining this node
		 */
		inline NodeManager& getNodeManager() const {
			assert_true(getNode().manager) << "NodeManager must not be null - unmanaged node detected!";
			return *getNode().manager;
		}

		/**
		 * Obtains a pointer to the manager maintaining this node instance. In case this
		 * node is not managed by any node manager (by any reason), NULL will be returned.
		 *
		 * @return a pointer to the manager maintaining this node
		 */
		inline NodeManager* getNodeManagerPtr() const {
			return getNode().manager;
		}

		/**
		 * Creates a new version of this node where every reference to a child node
		 * is replaced by a pointer to the node returned by the given mapper.
		 *
		 * @param manager the manager to be used to create the new node
		 * @param mapper the mapper used to translate child node references
		 * @param context the mapping context information to be forwarded
		 * @return a pointer to the modified node.
		 */
		template <typename Context>
		Ptr<const node_type> substitute(NodeManager& manager, NodeMapping<Context>& mapper, Context& c) const {
			return getNode().substituteInternal(manager, mapper, c).template as<Ptr<const node_type>>();
		}

		/**
		 * Creates a new version of this node where every reference to a child node
		 * is replaced by a pointer to the node returned by the given mapper.
		 *
		 * @param manager the manager to be used to create the new node
		 * @param mapper the mapper used to translate child node references
		 * @return a pointer to the modified node.
		 */
		Ptr<const node_type> substitute(NodeManager& manager, SimpleNodeMapping& mapper) const {
			int ctxt = 0; // yes, it is ugly, but a future revision of the mapper may get rid of it
			return getNode().substituteInternal(manager, mapper, ctxt).template as<Ptr<const node_type>>();
		}

		/**
		 * Obtains a string-representation of the accessed node.
		 */
		string toString() const {
			return ::toString(getNode());
		}

		/**
		 * Obtains a reference to the value represented by this node if
		 * it is representing a value.
		 *
		 * @return a reference to the internally maintained value
		 */
		const NodeValue& getNodeValue() const {
			assert_true(isValue()) << "Node does not represent a value!";
			return getNode().value;
		}

	  protected:
		/**
		 * Obtains a reference to the node accessed by this accessor.
		 *
		 * @return a reference to the accessed node.
		 */
		inline const node_type& getNode() const {
			return detail::node_access_helper<Derived>::getNode();
		}


	  public:

		/**
		 * Obtains a reference to the associated annotation container.
		 */
		const utils::Annotatable<NodeAnnotation>& getAnnotationContainer() const {
			return getNode().annotations;
		}

	};

} // end namespace core
} // end namespace insieme
