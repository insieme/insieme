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

#include "insieme/frontend/pragma/handler.h"
#include "insieme/core/ir_node.h"

#include <set>
#include <memory>

namespace clang {
class VarDecl;
} // end clang namespace 

namespace insieme {
namespace frontend {

namespace conversion {
class Converter;
}

namespace omp {

// forward declaration
class Annotation;

typedef std::shared_ptr<Annotation> AnnotationPtr;

/**
 * Base class for OpenMP pragmas
 */
class OmpPragma: public pragma::Pragma {
	pragma::MatchMap mMap;
public:
	OmpPragma(const clang::SourceLocation&  startLoc, 
			  const clang::SourceLocation&  endLoc, 
			  const std::string& 			name, 
			  const pragma::MatchMap& 		mmap);
	/**
	 * Converts the pragma into an annotation which will be attached to the IR.
	 */
	virtual omp::AnnotationPtr toAnnotation(conversion::Converter& fact) const = 0;

	const pragma::MatchMap& getMap() const { return mMap; }
};

/**
 * Registers the handlers for OpenMP pragmas
 */
void registerPragmaHandlers(clang::Preprocessor& pp);

core::ExpressionPtr attachOmpAnnotation(const core::ExpressionPtr& 		irNode, 
									    const clang::Stmt* 				clangNode, 
										conversion::Converter& 	fact);

core::StatementPtr attachOmpAnnotation(const core::StatementPtr& 		irNode, 
									   const clang::Stmt* 				clangNode, 
									   conversion::Converter&	fact);

core::ExpressionPtr attachOmpAnnotation(const core::ExpressionPtr& 		irNode, 
									    const clang::Decl* 				clangDecl, 
									    conversion::Converter&	fact);

void collectThreadPrivate(const pragma::PragmaStmtMap& map, std::set<const clang::VarDecl*>& vars);

void collectVolatile(const pragma::PragmaStmtMap& map, std::set<const clang::VarDecl*>& vars);

void addThreadPrivateAnnotation(const core::NodePtr& var);

} // End omp namespace
} // End frontend namespace
} // End insieme namespace
