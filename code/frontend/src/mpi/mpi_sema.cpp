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

#include <fstream>

#include "insieme/frontend/mpi/mpi_sema.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/annotations/mpi/mpi_annotations.h"
#include "insieme/annotations/c/location.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/file_rewriter.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::annotations::mpi;

namespace insieme {
namespace frontend {
namespace mpi {

MPICalls extractMPICalls( const core::NodeAddress& program ) {
	using annotations::mpi::CallID;

	MPICalls mpiCalls;
	
	auto&& filter = [&] (const core::CallExprAddress& callExpr) -> bool { 
		static core::LiteralAddress lit;
		return (lit = dynamic_address_cast<const core::Literal>(callExpr->getFunctionExpr()) ) && 
			    lit->getStringValue().compare(0,4,"MPI_") == 0;
	};

	typedef void (MPICalls::*PushBackPtr)(const core::CallExprAddress&);

	PushBackPtr push_back = &MPICalls::push_back;
	visitDepthFirst( program, makeLambdaVisitor( filter, fun(mpiCalls, push_back) ) );

	return mpiCalls;
}

/**
 * Depending on the value of the input command argument (mark-mpi-stmts) either:
 * 	-	Mark MPI statemetns assigning automatically an ID an write back the pragmas into the
 * 		original input program.
 *
 * 	-   It make sure that all the MPI calls present in this program are correctly annotated.
 */
core::ProgramPtr handleMPICalls( const core::ProgramPtr& program, bool tag) {
	
	if (tag) {
		LOG(INFO) << "Tagging MPI statements in the input program";
		MPICalls&& calls = extractMPICalls( core::NodeAddress(program) );

		// Determine the calls which need to be tagged
		auto&& twin = filterIterator(calls.begin(), calls.end(), 
				[&] (const CallExprAddress& curr) { return curr->hasAnnotation(CallID::KEY); } );
	
		LOG(INFO) << "Non annotated calls: " << std::distance(twin.first, twin.second);
		utils::Rewriter::CodeModificationList modifications;

		size_t id=0;
		for_each(twin.first, twin.second, [&] (const CallExprAddress& curr) { 
				assert(curr.getParentNode()->hasAnnotation(annotations::c::CLocAnnotation::KEY));
				const utils::SourceLocation& loc = 
					curr.getParentNode()->getAnnotation(annotations::c::CLocAnnotation::KEY)->getStartLoc();

				utils::SourceLocation annLoc( loc.getFileName(), loc.getLine(), 0);

				std::ostringstream ss;
				ss << "#pragma mpi id(" << ++id << ") dep()";
				modifications.insert( utils::Rewriter::CodeModification( annLoc, ss.str() ) );
			});

		utils::Rewriter::writeBack(modifications);

	}
	return program;
}

} // end mpi namespace
} // end frontend namespace 
} // end insieme namespace
