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

#include "insieme/core/ir_node.h"

namespace insieme {
namespace transform {

// TODO: model cleanup instances just like checks - to compose them

/**
 * This simple method cleaning up the given IR sub-tree. It therefore uses
 * a set of default cleanup operations.
 */
core::NodePtr cleanup(const core::NodePtr& node);

/**
 * This method replaces all arrays in the given sub-tree with scalars if they
 * are never used as arrays (ie. there are no index expression with a index other than 0)
 */
core::NodePtr eliminatePseudoArrays(const core::NodePtr& node);


/**
 * Eliminates branches for which the condition can be statically evaluated and it is found to
 * be false. This applyies to if statements and while loops (which are removed if the condition
 * is false)
 */
core::NodePtr deadBranchElimination(const core::NodePtr& node);

} // end of namespace transform
} // end of namespace insieme
