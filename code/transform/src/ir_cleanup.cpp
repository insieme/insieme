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

#include "insieme/transform/ir_cleanup.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/simplify.h"
#include "insieme/core/pattern/ir_pattern.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/analysis/polyhedral/scopregion.h"

#include "insieme/transform/connectors.h"

#include "insieme/transform/dfabased/const_prop.h"
#include "insieme/transform/dfabased/dead_variables.h"

#include <ostream>
#include <execinfo.h>

namespace insieme { namespace transform {

using namespace insieme::core;


/** Is it just me, or is this function really never called? If I am wrong, please document the behaviour, and the
relationship with the --cleanup/-C command line switch! */
core::NodePtr cleanup(const core::NodePtr& node, bool constantPropagation) {
	core::NodePtr res = node;

	res = deadBranchElimination(res);
	res = eliminateRedundantAssignments(res);
	res = eliminatePseudoArrays(res);

	if(constantPropagation) {
		LOG(INFO) << "Performing Constant Propagation on input program";
		res = doConstProp(res->getNodeManager(), res);
		res = removeDeadVariables(res->getNodeManager(), res);
		res = core::transform::simplify(res->getNodeManager(), res);
	}
	return res;
}

} // end of namespace transform
} // end of namespace insieme
