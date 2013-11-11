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

#include "insieme/transform/filter/filter.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/analysis/polyhedral/scop.h"

namespace insieme {
namespace transform {
namespace filter {

	// accept all nodes
	const Filter all("all",AcceptAll<const core::NodePtr&>());

	// don't accept any node
	const Filter none("none",RejectAll<const core::NodePtr&>());

	// produces an empty list
	extern const TargetFilter empty("empty", [](const core::NodePtr& node){ return vector<core::NodeAddress>(); });

	// takes the root node and returns it as a result
	extern const TargetFilter root("root", [](const core::NodePtr& node){ return toVector(core::NodeAddress(node)); });


	TargetFilter pattern(const string& name, const core::pattern::TreePatternPtr& pattern, const string& var) {
		return TargetFilter(name,
				[=](const core::NodePtr& node)->vector<core::NodeAddress> {
					auto res = pattern->matchAddress(core::NodeAddress(node));
					if (!res || !res->isVarBound(var)) {
						return vector<core::NodeAddress>();
					}
					auto value = res->getVarBinding(var);
					if (value.getDepth() == 0) {
						return toVector(value.getValue());
					}
					if (value.getDepth() == 1) {
						return value.getList();
					}
					assert(false && "Higher dimensions are not supported!");
					return vector<core::NodeAddress>();
		});
	}


	TargetFilter allMatches(const string& name, const core::pattern::TreePatternPtr& pattern, bool ignoreTypes) {
		return TargetFilter(name,
				[=](const core::NodePtr& node)->vector<core::NodeAddress> {
					vector<core::NodeAddress> res;

					// search for all matching candidates
					core::visitDepthFirst(core::NodeAddress(node), [&](const core::NodeAddress& candidate) {
						if (pattern->match(candidate.getAddressedNode())) {
							res.push_back(candidate);
						}
					}, ignoreTypes);

					return res;
		});
	}

} // end namespace filter
} // end namespace transform
} // end namespace insieme
