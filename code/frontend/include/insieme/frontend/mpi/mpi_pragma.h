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

#include <set>

#include "insieme/core/forward_decls.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/compiler.h"

namespace clang {
class Preprocessor;
}

namespace insieme {
namespace frontend {

namespace conversion {
class Converter;
}

namespace mpi {

struct MPIFrontendError : public ClangParsingError {
	MPIFrontendError() : 
		ClangParsingError("MPI Frontend failed to handle the input program.") { }
};

/**
 */
struct MPIStmtPragma: public pragma::Pragma {

	typedef std::set<size_t> 				DependenceSet;
	typedef DependenceSet::const_iterator 	const_iterator;

	MPIStmtPragma(const clang::SourceLocation& startLoc, 
			      const clang::SourceLocation& endLoc, 
			      const std::string&			type, 	
			      const pragma::MatchMap& 		mmap);


	size_t id() const { return m_id; }

	const_iterator deps_begin() const { return m_deps.begin(); }
	const_iterator deps_end() const { return m_deps.end(); }

	const DependenceSet& deps() const { return m_deps; }

private:
	size_t 			m_id;
	DependenceSet 	m_deps;
};

void registerPragmaHandler(clang::Preprocessor& pp);

void attachMPIStmtPragma( 
		const core::NodePtr& node, 
		const clang::Stmt* clangNode, 
		conversion::Converter& fact 
	);

} // end mpi namespace
} // end frontend namespace 
} // end insieme namespace 
