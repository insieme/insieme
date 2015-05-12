/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <boost/optional.hpp>
#include <cstdlib>
#include <iostream>
#include <isl/aff.h>
#include <isl/ctx.h>
#include <isl/printer.h>
#include <memory>
#include <string>
#include <vector>

#include "insieme/transform/polyhedral/scop.h"
#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::transform::polyhedral::novel;

/// isAffine returns true if the SCoP has affine lower and upper bound, affine stride and all of its subSCoPs are
/// affine as well.
bool NestedSCoP::isAffine() {
	// Do memoization here, as for calculating our affinity we need to calculate affinity of sub-SCoPs.
	// Additionally, we will traverse the SCoPs outside-in, which is why we need memoization here.
	static boost::optional<bool> retval;

	if (!retval && lb && ub && stride) {
		bool subsaffine=true;
		for (auto scop: subscops) subsaffine&=scop.isAffine();
		retval=boost::optional<bool>(isAffine(*lb) &&
		                             isAffine(*ub) &&
		                             isAffine(*stride) &&
		                             subsaffine);
	}
	return retval && *retval; // if in doubt, we say that this SCoP is not affine
}

/// The NestedSCoP constructor takes the lower and upper loop bounds of the originating loop, as well as the stride.
NestedSCoP::NestedSCoP(unsigned int nestlvl, NodeAddress lb, NodeAddress ub, NodeAddress stride):
    nestlvl(nestlvl), lb(boost::optional<NodeAddress>(lb)), ub(boost::optional<NodeAddress>(ub)),
    stride(boost::optional<NodeAddress>(stride)) {
}

/// This NestedSCoP constructor takes the condition of an if statement.
NestedSCoP::NestedSCoP(unsigned int nestlvl, NodeAddress ifcond):
    nestlvl(nestlvl), ifcond(boost::optional<NodeAddress>(ifcond)) {
}

/// isAffine will return true in case the expression given with addr is a linear expression.
bool NestedSCoP::isAffine(insieme::core::NodeAddress addr) {
	// we could use:
	// isl_basic_set_from_constraint_matrices
	// isl_basic_set_(in)equalities_matrix

	return true;
	isl_ctx *ctx=isl_ctx_alloc();
	isl_printer *printer=isl_printer_to_str(ctx);
	isl_aff *aff=isl_aff_read_from_str(ctx, "{ [x, y] -> [(3*x - 4*y - 7)] }");

	printer=isl_printer_print_aff(printer, aff);
	char *out=isl_printer_get_str(printer);
	if (out) {
		//std::cout << "From ISL we got: " << out << std::endl;
		free(out);
	}

	isl_printer_free(printer);
	isl_aff_free(aff);
	isl_ctx_free(ctx);

	return true;
}

/// Recursively print current internal status of polyhedral representation to stdout.
void NestedSCoP::debug() {
	// print the current scop
	if (lb && ub && stride)
		std::cout << std::string(nestlvl, '\t') << "for " << **lb << " .. " << **ub << " : " << **stride << std::endl;
	else if (ifcond)
		std::cout << std::string(nestlvl, '\t') << "if " << **ifcond << std::endl;
	else
		std::cout << std::string(nestlvl, '\t') << "for/if" << std::endl;

	// print all sub-scops
	for (auto scop: subscops) scop.debug();
}
