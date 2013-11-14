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

// forwared declaration
namespace clang { class CastExpr; }

namespace insieme {
namespace frontend {


//FORWARD DECLARATION
namespace conversion {
	class Converter;
}

namespace utils {

	std::size_t getPrecission(const core::TypePtr& type, const core::lang::BasicGenerator& gen);

	/**
	 * casts to bool an expression
	 */
	core::ExpressionPtr castToBool (const core::ExpressionPtr& expr);

	/**
	 * cast between 2 scalar types an IR expression
	 */
	core::ExpressionPtr castScalar(const core::TypePtr& targetTy, 
								   core::ExpressionPtr expr);

	/**
	 * Takes a clang::CastExpr, converts its subExpr into IR and wraps it with the necessary IR casts
	 *
	 * @param convFact, conversionFactor holding all converters and helpers
	 * @param castExpr the clang cast expression
	 * return right typed expression
	 */
	core::ExpressionPtr performClangCastOnIR (insieme::frontend::conversion::Converter& convFact,
											  const clang::CastExpr* castExpr);

} // end utils namespace 
} // end frontend namespace
} // end insisme namespace
