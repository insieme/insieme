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
#include "insieme/core/dump/binary_haskell.h"

#include <map>
#include <fstream>

#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/dump/binary_utils.h"
#include "insieme/core/lang/lang.h"

#include "insieme/utils/assert.h"

using namespace insieme::core::dump::binary::utils;

namespace insieme {
namespace core {
namespace dump {
namespace binary {
namespace haskell {

	void dumpIR(const std::string &filename, const NodePtr &ir) {
		std::ofstream out(filename);
		dumpIR(out, ir);
	}

	void dumpIR(std::ostream& out, const NodePtr& root) {
		dumpAddress(out, NodeAddress(root));
	}

	void dumpAddress(std::ostream& out, const NodeAddress& addr) {
		dumpAddresses(out, { addr });
	}

	void dumpAddresses(std::ostream& out, const std::vector<NodeAddress>& addr) {
		assert_false(addr.empty());
		const auto& root = addr[0].getRootNode();

		assert_decl(
			for(const auto& a : addr) {
				assert_eq(a.getRootNode(), root) << "Addresses do not share the same root";
			}
		);

		binary::dumpIR(out, root);

		// find builtins
		NodeSet covered;
		std::map<string, NodeAddress> builtins;
		visitDepthFirstOnce(NodeAddress(root), [&](const ExpressionAddress& expr) {
			if(covered.contains(expr.getAddressedNode())) return;

			covered.insert(expr.getAddressedNode());

			if(core::lang::isBuiltIn(expr)) {
				builtins[core::lang::getConstructName(expr)] = expr;
				if (auto fun = expr.isa<LambdaExprAddress>()) {
					builtins[core::lang::getConstructName(expr)] = fun->getLambda();
				}
			}
		});

		// attach builtins
		write<length_t>(out, builtins.size());
		for(auto& builtin : builtins) {
			utils::dumpString(out, builtin.first);
			utils::dumpString(out, toString(builtin.second));
		}

		// attach addresses
		write<length_t>(out, addr.size());
		for(const auto& a : addr) {
			utils::dumpString(out, toString(a));
		}
	}

} // end namespace haskell
} // end namespace binary
} // end namespace dump
} // end namespace core
} // end namespace insieme
