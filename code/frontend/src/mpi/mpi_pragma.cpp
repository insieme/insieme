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

#include "insieme/frontend/mpi/mpi_pragma.h"
#include "insieme/frontend/mpi/mpi_sema.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/iterator_utils.h"

#include "insieme/annotations/c/location.h"
#include "insieme/annotations/mpi/mpi_annotations.h"

namespace insieme {
namespace frontend {
namespace mpi {

using namespace pragma;

MPIStmtPragma::MPIStmtPragma(const clang::SourceLocation& startLoc, 
				  	         const clang::SourceLocation& endLoc, 
					         const std::string&			type, 	
					         const pragma::MatchMap& 		mmap) 
	
	: pragma::Pragma(startLoc, endLoc, type) 
{ 
	
	auto&& fit = mmap.find("id");
	assert(fit != mmap.end());

	const ValueList& expr = fit->second;
	assert(expr.size() == 1);
	clang::Expr* clangExpr = llvm::dyn_cast<clang::Expr>(expr.front()->get<clang::Stmt*>());
	if(!llvm::isa<clang::IntegerLiteral>(clangExpr)) {
		throw "Id is not an integer value";
	}

	stmtID = *llvm::cast<clang::IntegerLiteral>(clangExpr)->getValue().getRawData();
}

void registerPragmaHandler(clang::Preprocessor& pp) {

	using namespace insieme::frontend;
	using namespace insieme::frontend::pragma::tok;

	// define a PragmaNamespace for insieme::mpi
	clang::PragmaNamespace* insieme = new clang::PragmaNamespace("insieme");
	pp.AddPragmaHandler( insieme );

	insieme->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<MPIStmtPragma>(
		pp.getIdentifierInfo("mpi"), 
			kwd("id")  >> l_paren >> tok::numeric_constant["id"] >> r_paren >>
			!(kwd("dep") >> l_paren >> 
				(tok::numeric_constant >> !( ~comma >> tok::numeric_constant ))["dep"] >> r_paren) >> 
			tok::eod, 
			"insieme"
		)
	);

}

void attachMPIStmtPragma( const core::NodePtr& 				node, 
						  const clang::Stmt* 				clangNode, 
						  conversion::ConversionFactory& 	fact ) 
{

	const PragmaStmtMap::StmtMap& pragmaStmtMap = fact.getPragmaMap().getStatementMap();

	typedef PragmaStmtMap::StmtMap::const_iterator PragmaStmtIter; 
	std::pair<PragmaStmtIter, PragmaStmtIter>&& iter = pragmaStmtMap.equal_range(clangNode);

	std::for_each(iter.first, iter.second,
		[&] (const PragmaStmtMap::StmtMap::value_type& curr) {
			if(const MPIStmtPragma* mpiPragma = dynamic_cast<const MPIStmtPragma*>( &*(curr.second) )) {
				LOG(DEBUG) << "@ Statement has an MPI pragma attached" << std::endl << *node << std::endl; 
				LOG(DEBUG) << mpiPragma->getID();
				
				MPICalls&& mpiCalls = extractMPICalls(node);
				assert(!mpiCalls.empty());
				// we attach this pragma to the fist MPI call in the list of MPI calls which are
				// contained within the body 
				const core::CallExprPtr& mpiCall = mpiCalls.front();

				if (mpiCall->hasAnnotation(annotations::mpi::CallID::KEY) ) {
					utils::compilerMessage(
						utils::DiagnosticLevel::Error, 
						mpiPragma->getStartLocation(), 
						"Failed to identify associated MPI statement due to ambiguity",
						fact.getCurrentCompiler()
					);
					throw MPIFrontendError();
				}
				assert (mpiCall->hasAnnotation(annotations::c::CLocAnnotation::KEY) && 
					"MPI stmt not carrying location annotation"
				);
				mpiCall->addAnnotation ( std::make_shared<annotations::mpi::CallID>( mpiPragma->getID() ) );

			}
	});
}

} // end mpi namespace
} // end frontend namespace
} // end insieme namespace
