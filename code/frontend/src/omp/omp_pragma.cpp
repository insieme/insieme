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

#include "omp/omp_pragma.h"

#include "pragma_handler.h"
#include "pragma_matcher.h"

#include <iostream>

using namespace std;

namespace insieme {
namespace frontend {
namespace omp {

OmpPragma::OmpPragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const string& name, const MatchMap& mmap):
	Pragma(startLoc, endLoc, name, mmap) {

	std::cout << "~~~PRAGMA~~~" << std::endl;
	for(MatchMap::const_iterator i = mmap.begin(), e = mmap.end(); i!=e; ++i) {
		std::cout << "KEYWORD: " << i->first << ":" << std::endl;
		for(ValueList::const_iterator i2=i->second.begin(), e2=i->second.end(); i2!=e2; ++i2) {
			(*i2)->dump();
			llvm::outs() << ", ";
		}
		std::cout << std::endl;
	}
}

void OmpPragma::RegisterPragmaHandlers(clang::Preprocessor& pp) {
	using namespace insieme::frontend;
	using namespace insieme::frontend::tok;

	auto identifier_list   = ( identifier >> *(~comma >> identifier) );

	auto private_clause    =  kwd("private") >> l_paren >> identifier_list("private") >> r_paren;

	auto first_private_clause = kwd("firstprivate") >> l_paren >> identifier_list("firstprivate") >> r_paren;

	auto op 			  = tok::plus | tok::minus;
	auto reduction_clause = kwd("reduction") >> l_paren >> op >> colon >> identifier_list >> r_paren;

	auto parallel_clause =  ( 	(kwd("if") >> l_paren >> tok::expr("if") >> r_paren)
							| 	(kwd("num_threads") >> l_paren >> expr("num_threads") >> r_paren)
							|   (t<clang::tok::kw_default>() >> l_paren >> ( kwd("shared") | kwd("none") )("default") >> r_paren)
							|	private_clause
							|	first_private_clause
							|	(kwd("shared") >> l_paren >> identifier_list("shared") >> r_paren)
							|	(kwd("copyin") >> l_paren >> identifier_list("copyin") >> r_paren)
							| 	reduction_clause
							);

	auto kind 			=   ( t<clang::tok::kw_static>() | kwd("dynamic") | kwd("guided") | kwd("auto") | kwd("runtime") );

	auto chunk_size		=	expr;

	auto for_clause 	=	(	private_clause
							|	first_private_clause
							|	(kwd("lastprivate") >> l_paren >> identifier_list("lastprivate") >> r_paren)
							|	reduction_clause
							|	(kwd("schedule") >> l_paren >> kind("schedule") >> !( comma >> chunk_size("chunk_size") ) >> r_paren)
							|	(kwd("collapse") >> l_paren >> expr("collapse") >> r_paren)
							|	kwd("nowait")
							);

	auto for_clause_list = !(for_clause >> *( !comma >> for_clause ));

	auto parallel_for_clause_list = (parallel_clause | for_clause) >> *( !comma >> (parallel_clause | for_clause) );

	auto parallel_clause_list = !( 	(t<clang::tok::kw_for>() >> !parallel_for_clause_list)
								 | 	(parallel_clause >> *(!comma >> parallel_clause))
								 );

	pp.AddPragmaHandler("omp", PragmaHandlerFactory::CreatePragmaHandler<OmpPragma>("omp", pp.getIdentifierInfo("parallel"), parallel_clause_list >> tok::eom));
	pp.AddPragmaHandler("omp", PragmaHandlerFactory::CreatePragmaHandler<OmpPragma>("omp", pp.getIdentifierInfo("for"), for_clause_list >> tok::eom));


}

} // End omp namespace
} // End frontend namespace
} // End insieme namespace
