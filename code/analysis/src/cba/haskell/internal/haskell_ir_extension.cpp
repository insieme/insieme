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

#include "insieme/analysis/cba/haskell/internal/haskell_ir_extension.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {
namespace internal {

	using namespace core;

	/**
	 * Cleans the given IR code fragment by removing all references
	 * to IR extensions utilized for simplifying code generation on
	 * the haskell side of the analysis.
	 */
	NodePtr clean(const NodePtr& code) {
		auto& mgr = code->getNodeManager();
		IRBuilder builder(mgr);

		auto& ext = mgr.getLangExtension<HaskellExtension>();

		// transform input code bottom up

		return transform::transformBottomUp(code,[&](const NodePtr& node)->NodePtr {

			// only interested in calls
			auto call = node.isa<CallExprPtr>();
			if (!call) return node;

			// replace dummy-ref-member-access calls
			if (ext.isCallOfHaskellRefMemberAccess(node)) {
				assert_true(call->getArgument(0));
				assert_true(call->getArgument(1));
				assert_true(call->getArgument(1).isa<LiteralPtr>());
				return builder.refMember(call->getArgument(0),call->getArgument(1).as<LiteralPtr>()->getStringValue());
			}

			// replace dummy-ref-component-access calls
			if (ext.isCallOfHaskellRefComponentAccess(node)) {
				assert_true(call->getArgument(0));
				assert_true(call->getArgument(1));
				assert_true(call->getArgument(1).isa<LiteralPtr>());
				return builder.refComponent(call->getArgument(0),call->getArgument(1));
			}

			// replace dummy-ref-array-access calls
			if (ext.isCallOfHaskellRefArrayElementAccess(node)) {
				assert_true(call->getArgument(0));
				assert_true(call->getArgument(1));
				assert_true(call->getArgument(1).isa<LiteralPtr>());
				return builder.arrayRefElem(call->getArgument(0),call->getArgument(1));
			}

			// replace dummy-ref-array-access calls
			if (ext.isCallOfHaskellRefStdArrayElementAccess(node)) {
				assert_true(call->getArgument(0));
				assert_true(call->getArgument(1));
				assert_true(call->getArgument(1).isa<LiteralPtr>());
				assert_not_implemented();
				// TODO: use real std::array access operator signature
				return builder.arrayRefElem(call->getArgument(0),call->getArgument(1));
			}

			// re-type all call expressions (types have to be re-deduced!)
			return builder.callExpr(call->getFunctionExpr(),call->getArgumentList());
		});
	}

} // end namespace internal
} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
