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

#include "insieme/core/checks/ir_checks.h"

#include <algorithm>
#include "insieme/utils/container_utils.h"
#include "insieme/core/annotations/source_location.h"

namespace insieme {
namespace core {
namespace checks {

	CodeLocation CodeLocation::shift(const NodeAddress& outer, const utils::AnnotationKeyPtr& key) const {
		// make some assumptions
		assert_true(outer.hasAnnotation(key));
		assert(any(outer.getAnnotation(key)->getChildNodes(), [&](const NodePtr& cur) { return cur == address.getRootNode(); }));

		// create a copy of this instance
		CodeLocation res = *this;

		// update the annotation path
		res.annotationPath.insert(res.annotationPath.begin(), std::make_pair(key, address));

		// update the address
		res.address = outer;

		// done
		return res;
	}

	std::ostream& CodeLocation::printTo(std::ostream& out) const {
		// start with the address ...
		if(address.isValid()) {
			out << address;
		} else {
			out << "<unknown>";
		}

		// ... followed by the annotation path ...
		if(!annotationPath.empty()) {
			out << " / "
				<< join(" / ", annotationPath, [](std::ostream& out, const std::pair<insieme::utils::AnnotationKeyPtr, insieme::core::NodeAddress>& cur) {
					   out << *cur.first << ":" << cur.second;
				   });
		}

		// ... and finish with a source location if present
		auto origin = getOrigin();
		if(origin.isValid()) {
			if(auto loc = annotations::getLocation(origin)) { out << " - " << *loc; }
		}

		return out;
	}

	Message Message::shiftAddress(const NodeAddress& outer, const utils::AnnotationKeyPtr& key) const {
		// just built a new message targeting the shifted location
		return Message(location.shift(outer, key), errorCode, message, type);
	}

	bool Message::operator==(const Message& other) const { return type == other.type && location == other.location && errorCode == other.errorCode; }

	bool Message::operator<(const Message& other) const {
		if(type != other.type) { return type < other.type; }
		return location < other.location;
	}

	std::ostream& Message::printTo(std::ostream& out) const {
		// start with type ...
		switch(getType()) {
		case insieme::core::checks::Message::Type::WARNING: out << "WARNING: "; break;
		case insieme::core::checks::Message::Type::ERROR: out << "ERROR:   "; break;
		}

		// add error code
		out << getErrorCode();

		// .. continue with location ..
		out << " @ (" << getLocation();

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
			/**
			 * Whether this is a full check (which can tag nodes as error-free).
			 */
			bool isFullCheck;

			struct FullyCorrectTag {};

		  public:
			/**
			 * Default constructor for this combined check resulting in a check
			 * combining the given checks.
			 *
			 * @param checks the checks to be conducted by the resulting check
			 */
			CombinedIRCheck(const CheckList& checks = CheckList(), bool isFullCheck = false)
				: IRCheck(any(checks, [](const CheckPtr& cur) { return cur->isVisitingTypes(); })), checks(checks), isFullCheck(isFullCheck) { };

		  protected:
			OptionalMessageList visit(const NodeAddress& node) {
				if(node->hasAttachedValue<FullyCorrectTag>()) { return OptionalMessageList(); }
				// aggregate list of all error / warning messages
				OptionalMessageList list;
				for_each(checks.begin(), checks.end(), [&list, &node](const CheckPtr& cur) {
					// merge overall list and messages of current visitor
					addAll(list, cur->visit(node));
				});
				if(isFullCheck && (!list || list->empty())) { node->attachValue<FullyCorrectTag>(); }
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
			RecursiveIRCheck(const CheckPtr& check) : IRCheck(check->isVisitingTypes()), check(check){};

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
				if(contains(trace, node.as<NodePtr>(), equal_target<NodePtr>())) { return; }

				// process current node
				addAll(res, this->check->visit(node));

				// add node to trace
				trace.push_back(node.as<NodePtr>());

				// check annotations of current node
				for(const auto& cur : node->getAnnotations()) {
					for(const NodePtr& innerNode : cur.second->getChildNodes()) {
						assert_true(innerNode) << "Nodes must not be null!";

						// create an inner list of issues
						OptionalMessageList innerList;

						// collect messages from current annotation node
						visitNodeInternal(NodeAddress(innerNode), innerList, trace);

						// merge inner message list with outer list
						if(innerList) {
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
				for(auto cur : node->getChildList()) {
					visitNodeInternal(cur, res, trace);
				}

				// pop from trace
				assert_false(trace.empty()) << "Invalid stack state!";
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
			VisitOnceIRCheck(const CheckPtr& check) : IRCheck(check->isVisitingTypes()), check(check){};

		  protected:
			OptionalMessageList visitNode(const NodeAddress& node) {
				// create result list
				MessageList res;

				// create node set to eliminate double-visits
				NodeSet all;

				// collect full list of locations to be checked
				vector<CodeLocation> locations;
				collectLocations(node, all, locations);

				// now check all the locations
				for(const auto& loc : locations) {
					auto issues = check->visit(loc.getOrigin());

					// correct locations
					if(issues) {
						for(const Message& cur : issues->getAll()) {
							res.add(Message(loc, cur.getErrorCode(), cur.getMessage(), cur.getType()));
						}
					}
				}

				// -- experimental -- parallel version is disabled due to unsynchronized operations in the core -- experimental --
				//			#pragma omp parallel if(all.size() > 1000)			// i know, it is just a guess
				//			{
				//
				//				// create a thread-local storage for error messages
				//				MessageList local;
				//
				//				#pragma omp for
				//				for (typename vector<CodeLocation>::const_iterator it = locations.begin(); it < locations.end(); it++) {
				//					const auto& loc = *it;
				//
				//					// conduct checks
				//					auto issues = check->visit(loc.getOrigin());
				//
				//					// correct locations
				//					if(issues) {
				//						for(const Message& cur : issues->getAll()) {
				//							local.add(Message(loc, cur.getErrorCode(), cur.getMessage(), cur.getType()));
				//						}
				//					}
				//				}
				//
				//				#pragma omp critical (merge_message_list)
				//				{
				//					// merge the thread-local message list results
				//					res.addAll(local);
				//				}
				//
				//			}

				// done
				return (res.empty()) ? OptionalMessageList() : res;
			}

			void collectLocations(const NodeAddress& cur, NodeSet& all, vector<CodeLocation>& locations) {
				// add node to known list of nodes
				bool isNew = all.insert(cur.getAddressedNode()).second;
				if(!isNew) {
					return; // it has already been known => done
				}

				// add to list of targets (addresses)
				locations.push_back(cur);

				// also add locations within check-able annotations
				auto annotations = cur->getAnnotations(); // annotations might mutate while iterating through them
				for(const auto& entry : annotations) {
					for(const NodePtr& innerNode : entry.second->getChildNodes()) {
						assert_true(innerNode) << "Nodes must not be null!";

						// create a inner list of locations
						vector<CodeLocation> innerList;
						collectLocations(NodeAddress(innerNode), all, innerList);

						// merge inner list of locations with outer list
						for(const auto& loc : innerList) {
							locations.push_back(loc.shift(cur, entry.first));
						}
					}
				}

				// add child nodes
				for(auto c : cur->getChildList()) {
					collectLocations(c, all, locations);
				}
			}
		};
	}

	std::ostream& MessageList::printTo(std::ostream& out) const {
		if(empty()) { return out << "[]"; }
		return out << "[\n\t" << join("\n\t", getAll()) << "\n]"
			                                               // add some advertisements to improve tool usage
			                                               "\n problems to read this error? use dumpErrors in \"insime/core/printer/error_printer.h\"";
	}


	void addAll(OptionalMessageList& target, const OptionalMessageList& list) {
		// if there is no new element => skip
		if(!list) { return; }

		// check if there has already been a message within the result
		if(!target) {
			target.reset(*list);
			return;
		}
		target->addAll(*list);
	}

	void add(OptionalMessageList& target, const Message& msg) {
		if(!target) {
			target.reset(MessageList(msg));
			return;
		}
		target->add(msg);
	}

	MessageList check(const NodePtr& node, const CheckPtr& check) {
		// check node for null
		if(!node) { return MessageList(Message(NodeAddress(), EC_STRUCTURE_NULL_NODE, "Checking Null node!", Message::WARNING)); }

		// collect messages ..
		auto res = check->visit(NodeAddress(node));
		if(res) {
			// => list is not empty ... return list
			return *res;
		}
		// return an empty list ...
		return MessageList();
	}

	CheckPtr makeRecursive(const CheckPtr& check) { return make_check<RecursiveIRCheck>(check); }

	CheckPtr makeVisitOnce(const CheckPtr& check) { return make_check<VisitOnceIRCheck>(check); }

	CheckPtr combine(const CheckPtr& a) { return combine(toVector<CheckPtr>(a)); }

	CheckPtr combine(const CheckPtr& a, const CheckPtr& b) { return combine(toVector<CheckPtr>(a, b)); }

	CheckPtr combine(const CheckPtr& a, const CheckPtr& b, const CheckPtr& c) { return combine(toVector<CheckPtr>(a, b, c)); }

	CheckPtr combine(const CheckList& list, bool isFullCheck) { return make_check<CombinedIRCheck>(list, isFullCheck); }

} // end namespace checks
} // end namespace core
} // end namespace insieme
