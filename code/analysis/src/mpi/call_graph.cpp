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

#include <functional>

#include "insieme/analysis/mpi/comm_graph.h"

#include "insieme/core/ir_visitor.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"

#include <boost/bind.hpp>
#include <boost/config.hpp>

using namespace insieme;
using namespace insieme::core;

using namespace insieme::analysis::mpi;

typedef std::vector<CallExprPtr> CallExprList;

namespace insieme {
namespace analysis {
namespace mpi {

const string CallID::NAME = "MPI::CallID";
const utils::StringKey<CallID> CallID::KEY("MPI:CALL::ID");

CommGraph extractCommGraph( const core::NodePtr& program ) {
	
	utils::Timer timer("Insieme.MPI.Communication.Graph");

	CallExprList mpiCalls;
	
	auto&& filter = [&] (const CallExprPtr& callExpr) -> bool { 
		static core::LiteralPtr lit;
		return (lit = dynamic_pointer_cast<const Literal>(callExpr->getFunctionExpr()) ) && 
			    lit->getStringValue().compare(0,4,"MPI_") == 0;
	};

	typedef void (CallExprList::*PushBackPtr)(const CallExprPtr&);

	PushBackPtr push_back = &CallExprList::push_back;
	visitDepthFirstOnce( program, makeLambdaVisitor( filter, fun(mpiCalls, push_back) ) );

	LOG(DEBUG) << "Found " << mpiCalls.size() << " MPI calls";
	
	timer.stop();
	LOG(INFO) << timer;

	// Check wether MPI calls are correctly annotated
	CallExprList toAnnotate;
	auto&& twin = filterIterator(mpiCalls.begin(), mpiCalls.end(), 
			[&] (const CallExprPtr& curr) { return curr->hasAnnotation(CallID::KEY); } );
	
	LOG(INFO) << "Non annotated calls: " << std::distance(twin.first, twin.second);

	//for_each(twin.first, twin.second, [&] (const CallExprPtr& curr) { toAnnotate.push_back(curr); });

	return CommGraph();
}

} // end mpi namespace
} // end analysis namespace
} // end insieme namespace 


