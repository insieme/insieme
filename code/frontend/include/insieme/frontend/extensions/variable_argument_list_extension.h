/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include "insieme/frontend/extensions/frontend_extension.h"

namespace insieme {
namespace frontend {
namespace extensions {

	/**
	 * An extension for handling everything related to C variable argument lists
	 * Responsibilities:
	 * - adjust lambda types of variadic functions, adding a var_list argument
	 * - adjust calls to such functions, packing excess expressions into a var_list
	 * TODO:
	 * - support custom variable argument list implementations, by supporting va_start(), va_end(), va_arg() macros 
	 */
	class VariableArgumentListExtension : public insieme::frontend::extensions::FrontendExtension {
	  public:
		virtual insieme::core::TypePtr PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
		                                         insieme::frontend::conversion::Converter& converter);

		virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
			                                           insieme::frontend::conversion::Converter& converter);
	};

} // extensions
} // frontend
} // insieme
