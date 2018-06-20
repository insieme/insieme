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

#include "insieme/frontend/extensions/std_move_extension.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"

#include "insieme/core/lang/reference.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace frontend {
namespace extensions {

	insieme::core::ExpressionPtr StdMoveExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		if(auto clangCall = llvm::dyn_cast<clang::CallExpr>(expr)) {
			if(auto namedDecl = llvm::dyn_cast_or_null<clang::NamedDecl>(clangCall->getCalleeDecl())) {
				if(namedDecl->getQualifiedNameAsString() == "std::move") {
					const auto& refExt = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
					auto arg = converter.convertExpr(clangCall->getArg(0));

					// special handling for value parameters, which are de-refed here
					if(!core::lang::isReference(arg) && refExt.isCallOfRefDeref(arg)) {
						arg = core::analysis::getArgument(arg, 0);
					}
					assert_true(core::lang::isReference(arg)) << "Argument " << arg << " to std::move is not a reference type";

					core::ExpressionPtr callee;
					if(core::lang::isPlainReference(arg)) callee = refExt.getRefMovePlain();
					else if(core::lang::isCppReference(arg)) callee = refExt.getRefMoveReference();
					else callee = refExt.getRefMoveRValueReference();
					return converter.getIRBuilder().callExpr(callee, arg);
				}
			}
		}
		return nullptr;
	}

} // end namespace extensions
} // end namespace frontend
} // end namespace insieme
