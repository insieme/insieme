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

#ifndef NEWSCOP_H
#define NEWSCOP_H

#include "boost/optional.hpp"
#include "insieme/core/ir_address.h" // for the typedef insieme::core::NodeAddress
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"

namespace insieme { namespace transform { namespace polyhedral { namespace novel {

class SCoP {
public:
	void iterationDomain() {}
};

class NestedSCoP {
	NestedSCoP *parentscop;
	std::vector<NestedSCoP> subscops;

public:
	NestedSCoP(NestedSCoP *parentscop=0);
	bool isAffine();

private:
	// These vars should not be used outside this class, ever. Instead, give them to a private
	// function generateIterationDomain which then generates the publicly available iteration domain.
	// also possible: boost::variant<empty, loop, conditional>   http://stackoverflow.com/a/19579377
	boost::optional<insieme::core::NodeAddress> lb, ub, stride; ///< for conditionals: lb=0, ub=condition, stride=1

	bool isAffine(insieme::core::NodeAddress addr);
};

}}}}

#endif // NEWSCOP_H
