/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/forward_decls.h"
#include "insieme/frontend/clang.h"

namespace insieme {
namespace frontend {

namespace conversion {
	class Converter;
}

namespace utils {

	/// Replace RefTemps in initialization with memLoc
	///
	core::ExpressionPtr fixTempMemoryInInitExpression(const core::ExpressionPtr& memLoc, const core::ExpressionPtr& initExp);

	/// Build a Cxx method call from its components
	///
	core::CallExprPtr buildCxxMethodCall(conversion::Converter& converter, const core::TypePtr& retType, const core::ExpressionPtr& callee,
		                                 const core::ExpressionPtr& thisArgument, clang::CallExpr::arg_const_range argumentRange);

	/// Creates an expression corresponding to a clang EnumConstantDecl
	///
	core::ExpressionPtr buildEnumConstantExpression(conversion::Converter& converter, const clang::EnumConstantDecl* decl);

	/// Convert a CXXConstructExpr to construct the object at a given ir memory location
	///
	core::ExpressionPtr convertConstructExpr(conversion::Converter& converter, const clang::CXXConstructExpr* constructExpr, const core::ExpressionPtr& memLoc);

	/// Materialize "retIr" if possible
	///
	core::ExpressionPtr convertMaterializingExpr(conversion::Converter& converter, core::ExpressionPtr retIr);

	/// Modify the body of a for loop to contain the given update expression before every possible exit point
	///
	core::StatementPtr addIncrementExprBeforeAllExitPoints(const core::StatementPtr& body, const core::StatementPtr& incrementExpression);

} // end namespace utils
} // end namespace frontend
} // end namespace insieme
