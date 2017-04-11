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
 *
 */
#include "insieme/core/analysis/compare.h"

#include <fstream>
#include <iomanip>
#include <iostream>

#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/string_utils.h"

#include "insieme/core/annotations/source_location.h"
#include "insieme/core/dump/json_dump.h"
#include "insieme/core/inspyer/inspyer.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/common/env_vars.h"

namespace insieme {
namespace core {
namespace analysis {

	namespace {

		NodePtr wipeNames(const NodePtr& node) {

			struct Annotation {
				NodePtr cleaned;
				bool operator==(const Annotation& other) const {
					return cleaned == other.cleaned;
				}
			};

			// check the annotation
			if (node->hasAttachedValue<Annotation>()) {
				return node->getAttachedValue<Annotation>().cleaned;
			}

			// compute a cleaned version
			std::map<NodePtr,NodePtr> map;
			std::map<NodeAddress,NodePtr> replacements;
			unsigned id = 0;
			NodeManager& mgr = node->getNodeManager();
			IRBuilder builder(mgr);
			visitDepthFirstPrunable(NodeAddress(node),[&](const NodeAddress& cur)->Action {

				// skip root node
				if (cur.getAddressedNode() == node) return Action::Descent;

				// check some cases
				if (auto ref = cur.isa<LambdaReferencePtr>()) {

					auto& replacement = map[ref];
					if (!replacement) replacement = builder.lambdaReference(ref->getType(), format("l_ref_%d", ++id));
					replacements[cur.as<LambdaReferenceAddress>()->getName()] = replacement.as<LambdaReferencePtr>()->getName();
					return Action::Descent;

				} else if (auto ref = cur.isa<TagTypeReferencePtr>()) {

					auto& replacement = map[ref];
					if (!replacement) replacement = builder.tagTypeReference(format("t_ref_%d", ++id));
					replacements[cur] = replacement;
					return Action::Prune;

				} else if (auto type = cur.isa<TagTypePtr>()) {

					auto clean = wipeNames(type);
					if (*type != *clean) {
						replacements[cur] = clean;
					}
					return Action::Prune;

				}

				// continue searching
				return Action::Descent;
			},true);


			// conduct replacement
			auto cleaned = (replacements.empty()) ? node : core::transform::replaceAll(mgr, replacements);

			// attach and return
			node->attachValue(Annotation{cleaned});
			return cleaned;
		}

	}

	bool equalNameless(const NodePtr& nodeA, const NodePtr& nodeB) {
		return *nodeA == *nodeB || *wipeNames(nodeA) == *wipeNames(nodeB);
	}

	namespace {
		NodePtr safeGetParent(NodeAddress addr, size_t up) {
			up = std::min((size_t)addr.getDepth(), up);
			return addr.getParentNode(up);
		}
	}

	namespace {

		void irDiff_internal(NodeAddress nodeA, NodeAddress nodeB, const string& nameA, const string& nameB, size_t contextSize) {
			const auto& locString = core::annotations::getLocationString(nodeA);

			auto maxLen = std::max(nameA.length(), nameB.length());

			auto reportError = [&](const string& msg, const string& id, const auto& printForA, const auto& printForB) {
				std::cout << "IRDIFF-----\n"
					      << msg << ":" << std::endl
					      << "\t" << id << " " << std::setw(maxLen) << nameA << ": " << printForA << "\n"
					      << "\t" << id << " " << std::setw(maxLen) << nameB << ": " << printForB << "\n"
					      << "\t" << std::setw(maxLen) << nameA << ": " << dumpColor(safeGetParent(nodeA, contextSize)) << "\t" << std::setw(maxLen) << nameB
					      << ": " << dumpColor(safeGetParent(nodeB, contextSize)) << "\tLOC(" << locString << ")\n";

				nodeA.getNodeManager().getMetaGenerator().addBookmark(nodeA);
			};

			if(nodeA->getNodeType() != nodeB->getNodeType()) { reportError("Node types differ", "NT", nodeA->getNodeType(), nodeB->getNodeType()); }

			auto aExp = nodeA.isa<ExpressionPtr>();
			auto bExp = nodeB.isa<ExpressionPtr>();
			if(aExp) {
				if(bExp) {
					if(aExp->getType() != bExp->getType()) { reportError("Expressions differ in type", "type", *aExp->getType(), *bExp->getType()); }
				} else {
					reportError("One is a Expression, the other not", "type", *aExp->getType(), FunctionTypePtr());
				}
			}

			auto aVar = nodeA.isa<VariablePtr>();
			auto bVar = nodeB.isa<VariablePtr>();
			if(aVar) {
				if(bVar) {
					if(aVar->getId() != bVar->getId()) { reportError("Variables differ in id", "id", aVar->getId(), bVar->getId()); }
				} else {
					reportError("One is a Variable, the other not", "variable", aVar->getId(), VariablePtr());
				}
			}

			auto aString = nodeA.isa<StringValuePtr>();
			auto bString = nodeB.isa<StringValuePtr>();
			if(aString) {
				if(aString->getValue() != bString->getValue()) { reportError("StringValues differ", "val", aString->getValue(), bString->getValue()); }
			}

			auto aChildren = nodeA->getChildList();
			auto bChildren = nodeB->getChildList();
			if(aChildren.size() != bChildren.size()) {
				reportError("Nodes differ in number of children (ABORTING)", "child count", aChildren.size(), bChildren.size());
				return;
			}

			for(size_t i = 0; i < aChildren.size(); ++i) {
				irDiff_internal(aChildren[i], bChildren[i], nameA, nameB, contextSize);
			}
		}
	}

	void irDiff(NodeAddress nodeA, NodeAddress nodeB, const string& nameA, const string& nameB, size_t contextSize) {
		auto& meta = nodeA.getNodeManager().getMetaGenerator();
		irDiff_internal(nodeA, nodeB, nameA, nameB, contextSize);
		if(getenv(INSIEME_INSPYER) != nullptr) {
			std::cout << "Dumping INSPYER... " << std::flush;
			std::ofstream nodeA_out(nameA + ".json"), nodeB_out(nameB + ".json"), meta_out(nameA + "_meta.json");
			meta.dump(meta_out);
			dump::json::dumpIR(nodeA_out, nodeA.getRootNode());
			dump::json::dumpIR(nodeB_out, nodeB.getRootNode());
			std::cout << "done" << std::endl;
		}
	}

	void irDiff(NodePtr nodeA, NodePtr nodeB, const string& nameA, const string& nameB, size_t contextSize) {
		irDiff(NodeAddress(nodeA), NodeAddress(nodeB), nameA, nameB, contextSize);
	}

} // namespace analysis
} // namespace core
} // namespace insieme
