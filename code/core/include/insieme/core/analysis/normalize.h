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
	template<typename T> Pointer<T> normalize(const Pointer<T>& node) { return normalize(NodePtr(node)).as<Pointer<T>>(); }

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
	template<typename T> Address<T> normalize(const Address<T>& node) { return normalize(NodeAddress(node)).as<Address<T>>(); }



	/**
	 *  This function compares the normalize value of two nodes, usefull to identify identic codes
	 *  whith different variables
	 *  @param node to compare
	 *  @param node to compare
	 *  @retun  if both nodes are equivalent
	 */
	inline bool equalNormalize ( const NodePtr& a, const NodePtr& b){
		return *normalize(a) == *normalize(b);
	}


} // end namespace analysis
} // end namespace core
} // end namespace insieme
