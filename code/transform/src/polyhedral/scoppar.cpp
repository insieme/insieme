/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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
#include <iostream>
#include <memory>
#include <vector>

#include "insieme/analysis/polyhedral/scopregion.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/transform/polyhedral/scoppar.h"
#include "insieme/utils/logging.h"

using namespace insieme::analysis::polyhedral::scop;
using namespace insieme::core;
using namespace insieme::transform::polyhedral;


// constructor
SCoPPar::SCoPPar(ProgramPtr& program): program(program) {}

/** Return the size of the SCoP, given as n. The number itself is irrelevant, only relative sizes matter. Currently,
this function will return the number of statements but future implementation may return the number of loop instances,
or some similar metric. */
unsigned int SCoPPar::size(NodePtr n) {
	unsigned int count=0;
	// here, we could also use NodePtr with if-statements; however, using a StatementPtr type simplifies the code
	visitDepthFirst(n, [&](const StatementPtr &x) {
		count++;
	});
	return count;
}

/// will transform the sequential program to an OpenCL program
ProgramPtr& SCoPPar::apply() {
	// filter SCoPs and save them for later processing
	// TODO: here, program itself does not have an annotation — instead, we have to visit the nodes
	if (program->hasAnnotation(ScopRegion::KEY)) {
		LOG(WARNING) << "Program is already annotated, doing superfluous work.";
	}
	std::vector<NodeAddress> scops=insieme::analysis::polyhedral::scop::mark(program);

	LOG(WARNING) << "We found " << scops.size() << " SCoPs in the program." << std::endl;
	for (NodeAddress scop: scops) {
		annotations::LocationOpt loc=annotations::getLocation(scop);
		if (loc) std::cout << "Found a SCoP at location " << *loc << "." << std::endl;
		unsigned int s=size(scop.getAddressedNode());
		std::cout << "\tscop # " << scop << " (size " << s << ")" << std::endl
				  << printer::PrettyPrinter(scop) << std::endl;
		if (s<=4) std::cout << "parent: " << printer::PrettyPrinter(scop.getParentAddress()) << std::endl;

	}

	return program;
}
