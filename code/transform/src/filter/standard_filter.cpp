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
#include "insieme/transform/filter/standard_filter.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/pattern/ir_pattern.h"

#include "insieme/utils/value_wrapper.h"

namespace insieme {
namespace transform {
namespace filter {

	namespace {

		vector<core::NodeAddress> outermostFor(const core::NodePtr& root) {
			vector<core::NodeAddress> res;
			core::visitDepthFirstPrunable(core::NodeAddress(root), [&](const core::ForStmtAddress& cur) {
				res.push_back(cur);
				return true;
			});
			return res;
		}
	}

	TargetFilter outermostLoops() {
		// return pattern(transform::pattern::outermost(transform::pattern::var("x",transform::pattern::irp::forStmt())), "x");
		return TargetFilter("outermostLoops", &outermostFor);
	}


	namespace {

		VALUE_TYPE(ReverseNestingLevel, unsigned);

		unsigned getReverseNestingLevel(const core::ForStmtPtr& node) {
			// skip types - they are never nested
			if(node->getNodeCategory() == core::NC_Type) { return 0; }

			// check annotation
			if(node->hasAttachedValue<ReverseNestingLevel>()) { return node->getAttachedValue<ReverseNestingLevel>(); }

			// compute nesting level recursively from nested loops
			unsigned res = 0;

			core::visitDepthFirstOncePrunable(node->getBody(), [&](const core::ForStmtPtr& cur) -> bool {
				unsigned curLevel = getReverseNestingLevel(cur);
				res = (curLevel > res) ? curLevel : res;
				return true; // always prune
			});

			// add current level
			res++;
			node->attachValue(ReverseNestingLevel(res));
			return res;
		}


		vector<core::NodeAddress> innermostFor(const core::NodePtr& root, unsigned level) {
			vector<core::NodeAddress> res;
			core::visitDepthFirstPrunable(core::NodeAddress(root), [&](const core::ForStmtAddress& cur) -> bool {
				// obtain current nesting level
				unsigned curLevel = getReverseNestingLevel(cur);
				if(curLevel == level) {
					// collect for loops on the requested level
					res.push_back(cur);
				}

				// prune if we are at the required level or below
				return curLevel <= level;
			});
			return res;
		}
	}

	TargetFilter innermostLoops(unsigned level) {
		assert_gt(level, 0) << "Level must be > 0";
		// return allMatches(transform::pattern::irp::innerMostForLoopNest(level));
		std::stringstream name;
		name << "InnermostLevel(" << level << ")";
		return TargetFilter(name.str(), [=](const core::NodePtr& root) { return innermostFor(root, level); });
	}

	namespace {

		typedef vector<unsigned>::const_iterator index_iter;

		core::ForStmtAddress pickFor(const core::NodeAddress& root, index_iter begin, const index_iter& end) {
			assert(begin != end && "Cannot process empty interval!");

			// counter the number of encountered for-stmts on this level
			unsigned counter = 0;

			core::ForStmtAddress res;
			core::visitDepthFirstPrunable(root, [&](const core::NodeAddress& cur) {

				// quick-abort if result has been found
				if(res) { return true; }

				// decent into everything not being a for-stmt
				if(cur->getNodeType() != core::NT_ForStmt) { return cur->getNodeCategory() == core::NC_Type; }

				// convert to for-stmt address
				const core::ForStmtAddress& curFor = cur.as<core::ForStmtAddress>();

				// check whether current index has been reached
				if(counter == *begin) {
					if(begin + 1 == end) {
						res = curFor; // got it (final level)
					} else {
						// resolve rest of the for-index recursively
						res = pickFor(curFor->getBody(), begin + 1, end);
					}
				}
				counter++;
				return true; // do never decent into for-stmt-body
			});

			return res;
		}
	}


	TargetFilter pickLoop(const vector<unsigned>& index) {
		std::stringstream name;
		name << "PickLoop(" << join(",", index) << ")";
		return TargetFilter(name.str(), [=](const core::NodePtr& root) {
			auto res = pickFor(core::NodeAddress(root), index.begin(), index.end());
			return (res) ? toVector<core::NodeAddress>(res) : vector<core::NodeAddress>();
		});
	}


	TargetFilter pickRelative(const core::NodeAddress& relativeAddress) {
		std::stringstream name;
		name << "PickRelative(" << relativeAddress << "::" << relativeAddress->getNodeType() << ")";
		return TargetFilter(name.str(), [=](const core::NodePtr& root) {
			return (root == relativeAddress.getRootNode()) ? toVector(relativeAddress) : vector<core::NodeAddress>();
		});
	}


} // end namespace filter
} // end namespace transform
} // end namespace insieme
