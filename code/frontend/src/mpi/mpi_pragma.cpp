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
#include "insieme/core/ir_address.h"

#include "insieme/utils/iterator_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/unused.h"

#include "insieme/annotations/c/location.h"
#include "insieme/annotations/mpi/mpi_annotations.h"

namespace insieme {
namespace frontend {
namespace mpi {

using namespace pragma;

MPIStmtPragma::MPIStmtPragma(const clang::SourceLocation& startLoc, 
				  	         const clang::SourceLocation& endLoc, 
					         const std::string&			  type, 	
					         const pragma::MatchMap& 	  mmap) 
	
	: pragma::Pragma(startLoc, endLoc, type) 
{ 
	
	auto&& fit = mmap.find("id");
	assert(fit != mmap.end());

	auto extract_int_val = [](const pragma::ValueUnionPtr& val) {
		std::string intLit = *val->get<std::string*>();
		return insieme::utils::numeric_cast<unsigned>( intLit.c_str() );
	};

	// Extract the ID value 
	assert(fit->second.size() == 1);
	m_id = extract_int_val(fit->second.front());
	
	// Extract the list of dependencies 
	fit = mmap.find("dep");
	assert(fit != mmap.end());

	for_each(fit->second, [&](const pragma::ValueUnionPtr& cur) {
		__unused auto ret = this->m_deps.insert( extract_int_val(cur) );
		assert(ret.second && "Dependence list for MPI statement contains the same element twice");
	});
}

void registerPragmaHandler(clang::Preprocessor& pp) {

	using namespace insieme::frontend;
	using namespace insieme::frontend::pragma::tok;

	// define a PragmaNamespace for insieme::mpi
	clang::PragmaNamespace* mpi = new clang::PragmaNamespace("mpi");
	pp.AddPragmaHandler( mpi );

	mpi->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<MPIStmtPragma>(
		pp.getIdentifierInfo("id"), 
			l_paren >> tok::numeric_constant["id"] >> r_paren >>
			kwd("dep") >> l_paren >> 
				!(tok::numeric_constant >> !( ~comma >> tok::numeric_constant ))["dep"] >> r_paren >> 
			tok::eod, 
			"mpi"
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

	std::for_each(iter.first, iter.second, [&] (const PragmaStmtMap::StmtMap::value_type& curr) {
		if(const MPIStmtPragma* mpiPragma = dynamic_cast<const MPIStmtPragma*>( &*(curr.second) )) {
			// LOG(DEBUG) << *node;
			MPICalls&& mpiCalls = extractMPICalls( core::NodeAddress(node) );
			assert(!mpiCalls.empty());
			// we attach this pragma to the fist MPI call in the list of MPI calls which are
			// contained within the body 
			const core::CallExprAddress& mpiCall = mpiCalls.front();

			if (mpiCall->hasAnnotation(annotations::mpi::CallID::KEY) ) {
				utils::compilerMessage(
					utils::DiagnosticLevel::Error, 
					mpiPragma->getStartLocation(), 
					"Failed to identify associated MPI statement due to ambiguity",
					fact.getCurrentCompiler()
				);
				throw MPIFrontendError();
			}
			assert (mpiCall.getParentNode()->hasAnnotation(annotations::c::CLocAnnotation::KEY) && 
				"MPI stmt not carrying location annotation"
			);
			
			LOG(DEBUG) << "@ Statement at location [" 
					   << *mpiCall.getParentNode()->getAnnotation(annotations::c::CLocAnnotation::KEY)
					   << "] has an MPI pragma attached: id = " << mpiPragma->id();

			mpiCall.getParentNode()->addAnnotation( 
					std::make_shared<annotations::mpi::CallID>( mpiPragma->id(), mpiPragma->deps() ) 
				);

		}
	});
}

} // end mpi namespace
} // end frontend namespace
} // end insieme namespace
