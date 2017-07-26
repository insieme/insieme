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

#pragma once

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_pointer.h"
#include "insieme/core/ir_address.h"

/**
 * This header provides an interface for a utility computing normalized versions of IR fragments.
 *
 * Within the IR structurally identical codes may be considered different due to diverging variable
 * names. The normal form of a code fragment computed by the operations offered within this header
 * is eliminating this problem by enforcing a fixed naming schema for variables.
 *
 * A code fragment is in normal form if all bound variables have an increasing ID in the order of
 * their declaration. The scop of the normalization is based on lambdas - hence, nested compound
 * statements do not create a new normalization scope, the IDs are still growing. The IDs of free
 * variables shell thereby be skipped.
 *
 * Justification: the chosen normalization format results in a unique normal form for otherwise
 * structurally identical lambda definitions since those do not contain free variables. Furthermore,
 * it is supporting data flow analysis within lambdas since names are unique.
 */

namespace insieme {
namespace core {
namespace analysis {

	/**
	 * Normalizes the given node to match a fixed schema such that
	 * structural name independent equivalence can be checked with the
	 * equality operator.
	 *
	 * @param node the node to be normalized.
	 * @return the normalized version of the given node.
	 */
	NodePtr normalize(const NodePtr& node);

	/**
	 * A generic wrapper for the normalization operation above.
	 *
	 * @tparam T the type of node to be normalized
	 * @param node the node to be normalized
	 * @param the normalized version of the given node
	 */
	template <typename T>
	Pointer<T> normalize(const Pointer<T>& node) {
		return normalize(NodePtr(node)).as<Pointer<T>>();
	}

	/**
	 * Normalizes the given address to match a fixed schema such that
	 * structural name independent equivalence can be checked with the
	 * equality operator. The normalization is applied to the root node.
	 *
	 * @param node the node to be normalized.
	 * @return the normalized version of the given node.
	 */
	NodeAddress normalize(const NodeAddress& node);

	/**
	 * A generic wrapper for the normalization operation above.
	 *
	 * @tparam T the type of node to be normalized
	 * @param node the node to be normalized
	 * @param the normalized version of the given node
	 */
	template <typename T>
	Address<T> normalize(const Address<T>& node) {
		return normalize(NodeAddress(node)).as<Address<T>>();
	}


	/**
	 *  This function compares the normalize value of two nodes, usefull to identify identic codes
	 *  whith different variables
	 *  @param node to compare
	 *  @param node to compare
	 *  @retun  if both nodes are equivalent
	 */
	inline bool equalNormalize(const NodePtr& a, const NodePtr& b) {
		return *normalize(a) == *normalize(b);
	}


} // end namespace analysis
} // end namespace core
} // end namespace insieme
