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

#include "insieme/core/checks/ir_checks.h"

#include <algorithm>
#include "insieme/utils/container_utils.h"


namespace insieme {
namespace core {
namespace checks {


Message Message::shiftAddress(const NodeAddress& outer, const utils::AnnotationKeyPtr& key) const {

	// make some assumptions
	assert(outer.hasAnnotation(key));
	assert(any(outer.getAnnotation(key)->getChildNodes(), [&](const NodePtr& cur) { return cur == address.getRootNode(); }));

	// create new path
	annotation_path new_path = annotationPath;
	new_path.insert(new_path.begin(), std::make_pair(key, address));

	// create shifted address
	return Message(outer, new_path, errorCode, message, type);
}

bool Message::operator==(const Message& other) const {
	return type == other.type && address == other.address && annotationPath == other.annotationPath && errorCode == other.errorCode;
}

bool Message::operator<(const Message& other) const {
	if (type != other.type) {
		return type < other.type;
	}

	if (address != other.address) {
		return address < other.address;
	}

	return annotationPath < other.annotationPath;
}

std::ostream& Message::printTo(std::ostream& out) const {

	// start with type ...
	switch(getType()) {
	case insieme::core::checks::Message::Type::WARNING:
		out << "WARNING: "; break;
	case insieme::core::checks::Message::Type::ERROR:
		out << "ERROR:   "; break;
	}

	// add error code
	out << getErrorCode();

	// .. continue with location ..
	out << " @ (";
	const insieme::core::NodeAddress& address = getAddress();
	if (address.isValid()) {
		out << address;
	} else {
		out << "<unknown>";
	}

	const auto& annotationPath = getAnnotationPath();
	if (!annotationPath.empty()) {
		out << " / " << join(" / ", annotationPath, [](std::ostream& out, const std::pair<insieme::utils::AnnotationKeyPtr, insieme::core::NodeAddress>& cur) {
			out << *cur.first << ":" << cur.second;
		});
	}

	// .. and conclude with the message.
	out << ") - MSG: " << getMessage();
	return out;
}

namespace {

	/**
	 * A check combining multiple AST Checks.
	 */
	class CombinedIRCheck : public IRCheck {

		/**
		 * The list of checks to be represented.
		 */
		CheckList checks;

	public:

		/**
		 * Default constructor for this combined check resulting in a check
		 * combining the given checks.
		 *
		 * @param checks the checks to be conducted by the resulting check
		 */
		CombinedIRCheck(const CheckList& checks = CheckList())
			: IRCheck(any(checks, [](const CheckPtr& cur) { return cur->isVisitingTypes(); })), checks(checks) {};

	protected:

		OptionalMessageList visit(const NodeAddress& node) {
			// aggregate list of all error / warning messages
			OptionalMessageList list;
			for_each(checks.begin(), checks.end(), [&list, &node](const CheckPtr& cur) {
				// merge overall list and messages of current visitor
				addAll(list, cur->visit(node));
			});
			return list;
		}
	};


	/**
	 * A check conducting AST Checks recursively throughout an AST. Thereby, each
	 * node might be checked multiple times in case it is shared.
	 */
	class RecursiveIRCheck : public IRCheck {

		/**
		 * The check to be conducted recursively.
		 */
		CheckPtr check;

	public:

		/**
		 * A default constructor for this recursive AST check implementation.
		 *
		 * @param check the check to be conducted recursively on all nodes.
		 */
		RecursiveIRCheck(const CheckPtr& check)
			: IRCheck(check->isVisitingTypes()), check(check) {};

	protected:

		OptionalMessageList visitNode(const NodeAddress& node) {
			// create resulting message list
			OptionalMessageList res;

			// create a node list tracing the decent (through annotations) to prevent cycles
			NodeList trace;

			// conduct recursive check
			visitNodeInternal(node, res, trace);

			// done
			return res;
		}

		void visitNodeInternal(const NodeAddress& node, OptionalMessageList& res, NodeList& trace) {

			// prevent infinite cycles => do not check something that is already on the path
			if (contains(trace, node.as<NodePtr>(), equal_target<NodePtr>())) {
				return;
			}

			// process current node
			addAll(res, this->check->visit(node));

			// add node to trace
			trace.push_back(node.as<NodePtr>());

			// check annotations of current node
			for (const auto& cur : node->getAnnotations()) {
				for(const NodePtr& innerNode : cur.second->getChildNodes()) {
					assert(innerNode && "Nodes must not be null!");

					// create an inner list of issues
					OptionalMessageList innerList;

					// collect messages from current annotation node
					visitNodeInternal(NodeAddress(innerNode), innerList, trace);

					// merge inner message list with outer list
					if (innerList) {
						for(auto msg : innerList->getErrors()) {
							add(res, msg.shiftAddress(node, cur.first));
						}
						for(auto msg : innerList->getWarnings()) {
							add(res, msg.shiftAddress(node, cur.first));
						}
					}
				}
			}

			// visit / check all child nodes
			for (auto cur : node->getChildList()) {
				visitNodeInternal(cur, res, trace);
			}

			// pop from trace
			assert(!trace.empty() && "Invalid stack state!");
			assert(trace.back() == node.as<NodePtr>() && "Invalid stack state!");
			trace.pop_back();
		}
	};

	/**
	 * A check conducting AST Checks recursively throughout an AST. Thereby, each
	 * node is only visited once, even if it is shared multiple times.
	 */
	class VisitOnceIRCheck : public IRCheck {

		/**
		 * The check to be conducted recursively.
		 */
		CheckPtr check;

	public:

		/**
		 * A default constructor for this AST check implementation.
		 *
		 * @param check the check to be conducted recursively on all nodes.
		 */
		VisitOnceIRCheck(const CheckPtr& check) : IRCheck(check->isVisitingTypes()), check(check) {};

	protected:

		OptionalMessageList visitNode(const NodeAddress& node) {

			// create result list
			OptionalMessageList res;

			// create node set to eliminate double-visits
			NodeSet all;

			// use internal implementation for processing
			checkNode(node, res, all);

			// done
			return res;
		}

		void checkNode(const NodeAddress& node, OptionalMessageList& res, NodeSet& all) {

			// add current node to set ...
			bool isNew = all.insert(node.getAddressedNode()).second;
			if (!isNew) {
				// ... and if already checked, we are done!
				return;
			}

			// check the current node and collect results
			addAll(res, this->check->visit(node));

			// check annotations of current node
			auto annotations = node->getAnnotations();		// annotations might mutate while iterating through them
			for (const auto& cur : annotations) {
				for(const NodePtr& innerNode : cur.second->getChildNodes()) {
					assert(innerNode && "Nodes must not be null!");

					// create an inner list of issues
					OptionalMessageList innerList;

					// collect messages from current annotation node
					checkNode(NodeAddress(innerNode), innerList, all);

					// merge inner message list with outer list
					if (innerList) {
						for(auto msg : innerList->getErrors()) {
							add(res, msg.shiftAddress(node, cur.first));
						}
						for(auto msg : innerList->getWarnings()) {
							add(res, msg.shiftAddress(node, cur.first));
						}
					}
				}
			}

			// visit / check all child nodes
			for (auto cur : node->getChildList()) {
				checkNode(cur, res, all);
			}
		}
	};

}

void addAll(OptionalMessageList& target, const OptionalMessageList& list) {
	// if there is no new element => skip
	if (!list) {
		return;
	}

	// check if there has already been a message within the result
	if (!target) {
		target.reset( *list );
		return;
	}
	target->addAll(*list);
}

void add(OptionalMessageList& target, const Message& msg) {
	if (!target) {
		target.reset( MessageList(msg) );
		return;
	}
	target->add(msg);
}

MessageList check(const NodePtr& node, const CheckPtr& check) {

	// check node for null
	if (!node) {
		return MessageList(Message(NodeAddress(node), EC_STRUCTURE_NULL_NODE, "Checking Null node!", Message::WARNING));
	}

	// collect messages ..
	auto res = check->visit(NodeAddress(node));
	if (res) {
		// => list is not empty ... return list
		return *res;
	}
	// return an empty list ...
	return MessageList();
}

CheckPtr makeRecursive(const CheckPtr& check) {
	return make_check<RecursiveIRCheck>(check);
}

CheckPtr makeVisitOnce(const CheckPtr& check) {
	return make_check<VisitOnceIRCheck>(check);
}

CheckPtr combine(const CheckPtr& a) {
	return combine(toVector<CheckPtr>(a));
}

CheckPtr combine(const CheckPtr& a, const CheckPtr& b) {
	return combine(toVector<CheckPtr>(a,b));
}

CheckPtr combine(const CheckPtr& a, const CheckPtr& b, const CheckPtr& c) {
	return combine(toVector<CheckPtr>(a,b,c));
}

CheckPtr combine(const CheckList& list) {
	return make_check<CombinedIRCheck>(list);
}

} // end namespace checks
} // end namespace core
} // end namespace insieme


std::ostream& operator<<(std::ostream& out, const insieme::core::checks::MessageList& messageList) {
	if (messageList.empty()) return out << "[]";
	return out << "[\n\t" << join("\n\t",messageList.getAll()) << "\n]";
}
