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
 */

#include "insieme/transform/filter/filter.h"
#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace transform {
namespace filter {

	// accept all nodes
	const Filter all("all", AcceptAll<const core::NodePtr&>());

	// don't accept any node
	const Filter none("none", RejectAll<const core::NodePtr&>());

	// produces an empty list
	extern const TargetFilter empty("empty", [](const core::NodePtr& node) { return vector<core::NodeAddress>(); });

	// takes the root node and returns it as a result
	extern const TargetFilter root("root", [](const core::NodePtr& node) { return toVector(core::NodeAddress(node)); });


	TargetFilter pattern(const string& name, const core::pattern::TreePattern& pattern, const string& var) {
		return TargetFilter(name, [=](const core::NodePtr& node) -> vector<core::NodeAddress> {
			auto res = pattern.matchAddress(core::NodeAddress(node));
			if(!res || !res->isVarBound(var)) { return vector<core::NodeAddress>(); }
			auto value = res->getVarBinding(var);
			if(value.getDepth() == 0) { return toVector(value.getValue()); }
			if(value.getDepth() == 1) { return value.getList(); }
			assert_fail() << "Higher dimensions are not supported!";
			return vector<core::NodeAddress>();
		});
	}


	TargetFilter allMatches(const string& name, const core::pattern::TreePattern& pattern, bool ignoreTypes) {
		return TargetFilter(name, [=](const core::NodePtr& node) -> vector<core::NodeAddress> {
			vector<core::NodeAddress> res;

			// search for all matching candidates
			core::visitDepthFirst(core::NodeAddress(node), [&](const core::NodeAddress& candidate) {
				if(pattern.match(candidate.getAddressedNode())) { res.push_back(candidate); }
			}, ignoreTypes);

			return res;
		});
	}

} // end namespace filter
} // end namespace transform
} // end namespace insieme
