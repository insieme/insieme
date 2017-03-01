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
#pragma once

#include "insieme/core/forward_decls.h"

#include "insieme/core/ir_pointer.h"

namespace insieme {
namespace core {
namespace transform {


	/**
	 * Iterates through the given code fragment and replaces statically computable constructs with their
	 * simplified equivalents. The following simplifications are applied:
	 *
	 * 	- Direct calls of bind expressions like "bind(..){ ... } ( .. )" are contracted to a single call.
	 * 	- Calls to functions consisting of a single line (e.g. fun('a v1) { return v1; } are inlined.
	 * 	- if with a conditionals evaluating to true / false are substituted by the corresponding body
	 * 	- NoOps are eliminated
	 *
	 * @param manager the manager to be used for constructing the resulting code
	 * @param code the code to be simplified
	 * @param simplifyDerivedOps a flag determining whether derived functions should be simplified as well
	 * @return the simplified program code
	 */
	NodePtr simplify(NodeManager& manager, const NodePtr& code, bool simplifyDerivedOps = false);

	/**
	 * A generic alternative of the simplify function.
	 *
	 * @param manager the manager to be used for constructing the resulting code
	 * @param code the code to be simplified
	 * @param simplifyDerivedOps a flag determining whether derived functions should be simplified as well
	 * @return the simplified program code
	 */
	template <typename T>
	Pointer<const T> simplify(NodeManager& manager, const Pointer<const T>& code, bool simplifyDerivedOps = false) {
		return simplify(manager, NodePtr(code), simplifyDerivedOps).as<Pointer<const T>>();
	}


} // end namespace transform
} // end namespace core
} // end namespace insieme
