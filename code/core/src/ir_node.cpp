/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
			template <typename T>
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
			bool operator()(const NodeValue& value) const {
				return true;
			}
			template <typename T>
			bool operator()(const T& other) const {
				return false;
			}
		};
	}

	Node::Node(const NodeType nodeType, const NodeValue& value)
	    : HashableImmutableData(detail::hash(nodeType, value)), nodeType(nodeType), value(value), nodeCategory(NC_Value), manager(0), equalityID(0) {}


	const Node* Node::cloneTo(NodeManager& manager) const {
		static const NodeList emptyList;

		// NOTE: this method is performing the all-IR-node work, the rest is done by createCloneUsing(...)

		// check whether cloning is necessary
		if(this->manager == &manager) {
			std::cout << "Manager is the same: " << this->manager << " vs. " << &manager << "\n";
			return this;
		}

		// create a clone using children within the new manager
		Node* res;
		if(isValueInternal()) {
			res = createInstanceUsing(emptyList);
		} else {
			// clone the child list
			auto clonedChildList = manager.getAll(getChildListInternal());

			// check whether this node was cloned as a side-effect of the child-list cloning
			// (in case this node is somewhere refereed to within an annotation)
			if(auto res = manager.lookupPlain(this)) {
				assert(res != this);
				return res;
			}

			// otherwise: create a new node
			res = createInstanceUsing(clonedChildList);
		}

		// update manager
		res->manager = &manager;

		// update equality ID
		res->equalityID = equalityID;

		// done
		return res;
	}

	void Node::migrateAnnotationsInternal(const NodePtr& target) const {
		core::transform::utils::migrateAnnotations(NodePtr(this), target);
	}

	bool equalsWithAnnotations(const NodePtr& nodeA, const NodePtr& nodeB) {
		// check identity (under-approximation)
		if(nodeA == nodeB) { return true; }

		// check structure (over-approximation)
		if(*nodeA != *nodeB) { return false; }

		// check annotations of pointer and nodes ...
		if(!hasSameAnnotations((*nodeA).getAnnotationContainer(), (*nodeB).getAnnotationContainer())) { return false; }

		// check annotations of references
		auto listA = nodeA->getChildList();
		auto listB = nodeB->getChildList();
		return all(make_paired_iterator(listA.begin(), listB.begin()), make_paired_iterator(listA.end(), listB.end()),

		           [](const std::pair<NodePtr, NodePtr>& cur) {

			           // make a recursive call
			           return equalsWithAnnotations(cur.first, cur.second);
			       });
	}

	void move_annotation_on_clone::operator()(const Node* src, const Node* trg) const {
		// copy annotations
		for_each(src->getAnnotationContainer().getAnnotations(),
		         [&](const typename Node::annotation_container::annotation_map_type::value_type& cur) { cur.second->clone(cur.second, trg); });
	}

	NodeManager::NodeManager() : data(new NodeManagerData(*this)) {}

	NodeManager::NodeManager(NodeManager& manager) : InstanceManager<Node, Pointer, move_annotation_on_clone>(manager), data(manager.data) {}

	NodeManager::NodeManager(unsigned initialFreshID) : data(new NodeManagerData(*this)) {
		setNextFreshID(initialFreshID);
	}

	NodeManager::NodeManagerData::NodeManagerData(NodeManager& manager) : root(manager), basic(new lang::BasicGenerator(manager)){};


	NodeManager::NodeManagerData::~NodeManagerData() {
		// free all extensions
		for_each(extensions, [](const ExtensionMap::value_type& cur) { delete cur.second; });
	}

} // end namespace core
} // end namespace insieme


IRDump dump(const insieme::core::NodePtr& node, std::ostream& out) {
	return dumpPretty(node, out);
}

IRDump dumpText(const insieme::core::NodePtr& node, std::ostream& out) {
	return IRDump([node](std::ostream& out) -> std::ostream& { return out << insieme::core::dump::text::TextDump(node) << std::endl; }, out);
}

IRDump dumpColor(const insieme::core::NodePtr& node, std::ostream& out, bool noLet) {
	return IRDump([node, noLet](std::ostream& out) -> std::ostream& {
		insieme::core::printer::PrettyPrinter print(node);
		print.setOption(insieme::core::printer::PrettyPrinter::USE_COLOR);
		print.setOption(insieme::core::printer::PrettyPrinter::PRINT_DEREFS);
		print.setOption(insieme::core::printer::PrettyPrinter::USE_VARIABLE_NAME_ANNOTATIONS);
		if(noLet) print.setOption(insieme::core::printer::PrettyPrinter::NO_LET_BINDINGS);
		return out << print << std::endl;
	}, out);
}
IRDump dumpOneLine(const insieme::core::NodePtr& node, std::ostream& out) {
	return IRDump([node](std::ostream& out) -> std::ostream& {
		insieme::core::printer::PrettyPrinter print(node);
		print.setOption(insieme::core::printer::PrettyPrinter::PRINT_DEREFS);
		print.setOption(insieme::core::printer::PrettyPrinter::PRINT_CASTS);
		print.setOption(insieme::core::printer::PrettyPrinter::JUST_LOCAL_CONTEXT);
		return out << print;
	}, out);
}

IRDump dumpPretty(const insieme::core::NodePtr& node, std::ostream& out) {
	return IRDump([node](std::ostream& out) -> std::ostream& {
		insieme::core::printer::PrettyPrinter print(node);
		print.setOption(insieme::core::printer::PrettyPrinter::PRINT_DEREFS);
		return out << print << std::endl;
	}, out);
}

IRDump dumpDetail(const insieme::core::NodePtr& node, std::ostream& out) {
	return IRDump([node](std::ostream& out) -> std::ostream& {
		insieme::core::printer::PrettyPrinter print(node, insieme::core::printer::PrettyPrinter::OPTIONS_MAX_DETAIL);
		return out << print << std::endl;
	}, out);
}

IRDump dumpDetailColored(const insieme::core::NodePtr& node, std::ostream& out) {
	return IRDump([node](std::ostream& out) -> std::ostream& {
		insieme::core::printer::PrettyPrinter print(node, insieme::core::printer::PrettyPrinter::OPTIONS_MAX_DETAIL);
		print.setOption(insieme::core::printer::PrettyPrinter::USE_COLOR);
		return out << print << std::endl;
	}, out);
}
