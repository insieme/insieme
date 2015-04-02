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

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_address.h"
#include "insieme/utils/annotation.h"

namespace insieme {
namespace analysis {

class CFG;
typedef std::shared_ptr<CFG> CFGPtr;

namespace mpi {

/**
 * Represent a node of the communication graph. It points to the MPI call expression
 * represented by this node and its id. The pointer to the call expression is kept 
 * by the address (not a pointer) because we want to be able to remember the calling 
 * context.
 */
struct Call { 
	core::CallExprAddress call;
	size_t callID;
};

/** 
 * Represents an edge of the communication graph. 
 */
struct CallDep {
	// TODO
};

typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, Call, CallDep> CommGraph;

CommGraph extractCommGraph( const core::NodePtr& program );

/**
 * Merges the Control Flow Graph with the communication graph which is obtained by reading 
 * annotations (or others) associated to MPI statements. Edges connecting matching statements are 
 * therefore added to the cfg.
 */
void merge(CFGPtr& cfg, const CommGraph& commGraph);

} // end mpi namespace
} // end analysis namespace 
} // end insieme namespace

namespace std {

std::ostream& operator<<(std::ostream& out, const insieme::analysis::mpi::CommGraph& grap);
	
} // end std namespace 

