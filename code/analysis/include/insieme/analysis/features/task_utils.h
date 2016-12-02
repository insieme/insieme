/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <vector>

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_node.h"


namespace insieme {
namespace analysis {
namespace features {

	/// Returns a list of addresses to parallel calls, which spawn new tasks below root
	///
	std::vector<core::NodeAddress> getParallelTasks(const core::NodeAddress& root);

	/// Returns all the recursive task calls found in the passed parallelCalls below root.
	/// All passed parallelCalls will be checked for recursions.
	/// The address of root (which should be the root or a parent of all parallelCalls) needs to be passed to avoid detecting the same recursions multiple times.
	/// Each returned pair represents a recursion between a LambdaExpression call target and a LambdaReference as the call location.
	///
	std::vector<std::pair<core::NodeAddress, core::NodeAddress>> getRecursiveTasks(const core::LambdaExprAddress& root,
	                                                                               const std::vector<core::NodeAddress>& parallelCalls);

} // end namespace features
} // end namespace analysis
} // end namespace insieme
