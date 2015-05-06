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

#include <cstdlib>
#include <boost/optional.hpp>
#include <iostream>
#include <memory>
#include <vector>

#include "insieme/transform/polyhedral/scop.h"
#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::transform::polyhedral::novel;

/// isAffine returns true if the SCoP has affine lower and upper bound, affine stride and all of its subSCoPs are
/// affine as well.
bool NestedSCoP::isAffine() {
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

/// The NestedSCoP constructor initializes the parent of the nested SCoP.
NestedSCoP::NestedSCoP(NestedSCoP *parentscop): parentscop(parentscop) {
}

/// isAffine will return true in case the expression given with addr is a linear expression.
bool NestedSCoP::isAffine(insieme::core::NodeAddress addr) {
	return true;
}
