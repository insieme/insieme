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

#include"insieme/analysis/dep_graph.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/analysis/polyhedral/backends/isl_backend.h"

using namespace insieme::analysis::poly;

namespace insieme {
namespace analysis {
namespace dep {

Stmt::Stmt(size_t id, const core::NodeAddress& addr) : m_id(id), m_addr(addr) { }


DependenceGraph extractDependenceGraph( const core::NodePtr& root ) {
	
	assert(root->hasAnnotation(scop::ScopRegion::KEY) && "IR statement must be a SCoP");
	Scop& scop = root->getAnnotation(scop::ScopRegion::KEY)->getScop();

	// create a ISL context
	BackendTraits<POLY_BACKEND>::ctx_type ctx;

	DependenceGraph ret;

	// for each kind of dependence we extract them
	auto&& rawDep = scop.computeDeps(ctx, dep::RAW);

	isl_union_set* dep = isl_union_map_deltas(rawDep->getAsIslMap());

	printIslSet(std::cout, ctx.getRawContext(), dep);

}

} // end dep namespace
} // end analysis namespace
} // end insieme namespace 
