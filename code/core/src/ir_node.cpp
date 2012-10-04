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

#include "insieme/core/ir_node.h"

#include "insieme/core/ir_mapper.h"
#include "insieme/core/ir_node_annotation.h"

#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/dump/text_dump.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/extension.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {


	// **********************************************************************************
	// 							    Abstract Node Base
	// **********************************************************************************

	/**
	 * Defining the equality ID generator.
	 */
	utils::SimpleIDGenerator<Node::EqualityID> Node::equalityClassIDGenerator;

	namespace detail {

		/**
		 * A visitor realizing the hashing for the value type potentially stored
		 * within a node.
		 */
		struct HashVisitor : public boost::static_visitor<std::size_t> {
			template<typename T>
			std::size_t operator()(const T& value) const {
				return boost::hash<T>()(value);
			}

			// hashing of integer values by according to http://www.concentric.net/~ttwang/tech/inthash.htm

			std::size_t operator()(const char value) const {
				return static_cast<std::size_t>(value * 2654435761);
			}

			std::size_t operator()(const int value) const {
				return static_cast<std::size_t>(value * 2654435761);
			}

			std::size_t operator()(const unsigned value) const {
				return static_cast<std::size_t>(value * 2654435761);
			}

		};

		/**
		 * Obtains a hash value for the given value instance.
		 *
		 * @param value the value to be hashed
		 * @return the hash code for the given value object
		 */
		inline std::size_t hash(const NodeType type, const NodeValue& value) {
			std::size_t seed = 0;
			boost::hash_combine(seed, type);
			boost::hash_combine(seed, boost::apply_visitor(HashVisitor(), value));
			return seed;
		}

		/**
		 * A static visitor determining whether an element within a boost::variant
		 * is a value or not.
		 */
		struct IsValueVisitor : public boost::static_visitor<bool> {
			bool operator()(const NodeValue& value) const { return true; }
			template<typename T> bool operator()(const T& other) const { return false; }
		};

	}

	Node::Node(const NodeType nodeType, const NodeValue& value)
		: HashableImmutableData(detail::hash(nodeType, value)),
		  nodeType(nodeType), value(value), nodeCategory(NC_Value),
		  manager(0), equalityID(0) { }


	const Node* Node::cloneTo(NodeManager& manager) const {
		static const NodeList emptyList;

		// NOTE: this method is performing the all-IR-node work, the rest is done by createCloneUsing(...)

		// check whether cloning is necessary
		if (this->manager == &manager) {
			std::cout << "Manager is the same: " << this->manager << " vs. " << &manager << "\n";
			return this;
		}

		// trigger the create a clone using children within the new manager
		Node* res = (isValueInternal())?
				createInstanceUsing(emptyList) :
				createInstanceUsing(manager.getAll(getChildListInternal()));

		// update manager
		res->manager = &manager;

		// update equality ID
		res->equalityID = equalityID;

		// copy annotations
		for_each(annotations.getAnnotations(), [&](const typename annotation_container::annotation_map_type::value_type& cur) {
			cur.second->clone(cur.second, res);
		});

		// done
		return res;
	}

	NodePtr Node::substituteInternal(NodeManager& manager, NodeMapping& mapper) const {

		// skip operation if it is a value node
		if (isValueInternal()) {
			return (&manager != getNodeManagerPtr())?manager.get(*this):NodePtr(this);
		}

		// compute new child node list
		NodeList children = mapper.mapAll(getChildListInternal());
		if (::equals(children, getChildListInternal(), equal_target<NodePtr>())) {
			return (&manager != getNodeManagerPtr())?manager.get(*this):NodePtr(this);
		}

		// create a version having everything substituted
		Node* node = createInstanceUsing(children);

		// obtain element within the manager
		NodePtr res = manager.get(node);

		// free temporary instance
		delete node;

		// migrate annotations
		core::transform::utils::migrateAnnotations(NodePtr(this), res);

		// return instance maintained within manager
		return res;
	}

	bool equalsWithAnnotations(const NodePtr& nodeA, const NodePtr& nodeB) {

		// check identity (under-approximation)
		if (nodeA == nodeB) {
			return true;
		}

		// check structure (over-approximation)
		if (*nodeA!=*nodeB) {
			return false;
		}

		// check annotations of pointer and nodes ...
		if (!hasSameAnnotations((*nodeA).getAnnotationContainer(), (*nodeB).getAnnotationContainer())) {
			return false;
		}

		// check annotations of references
		auto listA = nodeA->getChildList();
		auto listB = nodeB->getChildList();
		return all(
				make_paired_iterator(listA.begin(), listB.begin()),
				make_paired_iterator(listA.end(), listB.end()),

				[](const std::pair<NodePtr, NodePtr>& cur) {

			// make a recursive call
			return equalsWithAnnotations(cur.first, cur.second);
		});
	}


	NodeManager::NodeManager() : data(new NodeManagerData(*this)) { }

	NodeManager::NodeManager(NodeManager& manager)
		: InstanceManager<Node, Pointer>(manager), data(manager.data) {}

	NodeManager::NodeManager(unsigned initialFreshID)
		: data(new NodeManagerData(*this)) { setNextFreshID(initialFreshID); }

	NodeManager::NodeManagerData::NodeManagerData(NodeManager& manager)
		: root(manager), basic(new lang::BasicGenerator(manager)) {};


	NodeManager::NodeManagerData::~NodeManagerData() {
		// free all extensions
		for_each(extensions, [](const ExtensionMap::value_type& cur) {
			delete cur.second;
		});
	}

} // end namespace core
} // end namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::core::NodeType& type) {
		switch(type) {
			#define CONCRETE(NAME) case insieme::core::NT_ ## NAME : return out << #NAME;
				#include "insieme/core/ir_nodes.def"
			#undef CONCRETE
		}

		assert(false && "Unsupported node type encountered!");
		return out << "UnknownType";
	}

} // end namespace std



void dumpText(const void* node) {
	std::cout << insieme::core::dump::text::TextDump(insieme::core::NodePtr((insieme::core::Node*)node));
}

void dumpPretty(const void* node) {
	std::cout << insieme::core::printer::PrettyPrinter(insieme::core::NodePtr((insieme::core::Node*)node));
}

void dumpPrettyFull(const void* node) {
	std::cout << insieme::core::printer::PrettyPrinter(insieme::core::NodePtr((insieme::core::Node*)node),
				insieme::core::printer::PrettyPrinter::OPTIONS_MAX_DETAIL
			);
}

