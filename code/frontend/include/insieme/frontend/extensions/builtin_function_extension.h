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
 */

#pragma once

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/annotations/c/include.h"


namespace insieme {
namespace frontend {
namespace extensions {

	using namespace insieme;

	class BuiltinFunctionExtension : public insieme::frontend::extensions::FrontendExtension {
	  public:
		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
			// check if it is a decl ref expr
			if(const clang::DeclRefExpr* declRefExpr = llvm::dyn_cast<clang::DeclRefExpr>(expr)) {
				// check if it we can get the function decl out of the decl ref
				// if it is a builtin_constant_p we have to form it with the correct type
				if(const clang::FunctionDecl* funcDecl = llvm::dyn_cast<clang::FunctionDecl>(declRefExpr->getDecl())) {
					if(funcDecl->getNameAsString() == "__builtin_constant_p") {
						auto builder = converter.getIRBuilder();
						auto tyList = core::TypeList();
						tyList.push_back(builder.typeVariable("a"));
						auto type = builder.functionType(tyList, builder.getLangBasic().getInt4());
						auto ret = builder.literal(type, insieme::frontend::utils::buildNameForFunction(funcDecl, converter));
						// tell the compiler that this builtin is included somewhere else, even if it is not true
						annotations::c::attachInclude(ret, "stdint.h");
						return ret.as<core::ExpressionPtr>();
					}
				}
			}
			return nullptr;
		}
	};

} // extensions
} // frontend
} // insieme
