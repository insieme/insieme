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

#include "insieme/core/ir_address.h"
#include "insieme/transform/polyhedral/scop.h"
#include "insieme/transform/polyhedral/scoplist.h"
#include "insieme/transform/polyhedral/scopvisitor.h"
#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::transform::polyhedral::novel;

// constructor
SCoPList::SCoPList(ProgramPtr& program): program(program) {
	std::cout << "Hello from SCoPList Constructor Nouvelle!" << std::endl;
	findSCoPs(program);
}

// visit all the nodes of a program and find SCoPs, returning a (possibly empty) list of SCoPs
void SCoPList::findSCoPs(ProgramPtr& program) {
	std::cout << "trying to find SCoPs" << std::endl;
	SCoPVisitor scopvisitor;
	scopvisitor.visit(NodeAddress(program));
}

// return the IR of the corresponding polyhedra, if defined
ProgramPtr& SCoPList::IR() {
	// generate code only when all SCoPs are valid; otherwise use original program, since we could not optimize
	// possibly, we could also optimize based on the valid SCoPs, ignoring the other ones (check semantics!)
	int scopsvalid=1;
	for (auto it=begin(); it!=end(); it++) scopsvalid&=it->valid();

	// now, if all SCoPs are valid, generate code
	if (scopsvalid)
		return program; // currently a noop
	else
		return program;
}
