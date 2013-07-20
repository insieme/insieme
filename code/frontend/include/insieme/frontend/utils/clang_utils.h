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

#include <clang/AST/Expr.h>
#include <llvm/Support/Casting.h>

namespace insieme {
namespace frontend {
namespace utils {

using namespace llvm;

/**
 * Utility function which removes eventual cast/implicit_cast/parenthesis and returns
 * the expected statement if found
 * @return
 */
template <class ExpectedTy>
const ExpectedTy* skipSugar(const clang::Expr* expr) {

	// remove eventual parenthesis
	if(const clang::ParenExpr* parenExpr = dyn_cast<clang::ParenExpr>(expr)) {
		return skipSugar<ExpectedTy>(parenExpr->getSubExpr());
	}

	// remove eventual casts
	if(const clang::CastExpr* castExpr = dyn_cast<clang::CastExpr>(expr)) {
		return skipSugar<ExpectedTy>(castExpr->getSubExpr());
	}

	if(const clang::UnaryOperator* unOp = dyn_cast<clang::UnaryOperator>(expr)) {
		if(unOp->getOpcode() == clang::UO_Minus)
			return skipSugar<ExpectedTy>(unOp->getSubExpr());
	}

	return dyn_cast<const ExpectedTy>(expr);
}

} // End utils namespace
} // End frontend namespace
} // End insieme namespace
