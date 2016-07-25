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
