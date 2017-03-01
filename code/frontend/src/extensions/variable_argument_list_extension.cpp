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
#include "insieme/frontend/extensions/variable_argument_list_extension.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/lang/varargs_extension.h"

namespace insieme {
namespace frontend {
namespace extensions {

	using namespace core;

	TypePtr VariableArgumentListExtension::PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
		                                             insieme::frontend::conversion::Converter& converter) {
		if(type->isFunctionProtoType()) {
			const clang::FunctionProtoType* funcTy = type->getAs<clang::FunctionProtoType>();
			if(funcTy->isVariadic()) {
				auto irFt = irType.as<FunctionTypePtr>();
				TypeList paramTypes = irFt->getParameterTypeList();
				paramTypes.push_back(converter.getNodeManager().getLangExtension<lang::VarArgsExtension>().getVarList());
				return converter.getIRBuilder().functionType(paramTypes, irFt->getReturnType(), irFt->getKind());
			}
		}
		return irType;
	}

	ExpressionPtr VariableArgumentListExtension::PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
		                                                   insieme::frontend::conversion::Converter& converter) {
		if(auto clangCallExpr = llvm::dyn_cast<clang::CallExpr>(expr)) {
			auto clangCallee = clangCallExpr->getDirectCallee();
			if(clangCallee) {
				auto clangFunTy = clangCallee->getType()->getAs<clang::FunctionProtoType>();
				if(clangFunTy && clangFunTy->isVariadic()) { 
					// pack extraneous parameters
					auto irCall = irExpr.as<CallExprPtr>();
					auto irFunTy = irCall->getFunctionExpr()->getType().as<FunctionTypePtr>();
					auto irFunArgs = irCall->getArgumentList();
					auto numFixedParams = irFunTy->getParameterTypeList().size()-1;
					// build packed arguments
					ExpressionList exprsToPack;
					std::copy(irFunArgs.begin()+numFixedParams, irFunArgs.end(), std::back_inserter(exprsToPack));
					auto packedArgList = converter.getIRBuilder().pack(exprsToPack);
					// build new call with packed args
					ExpressionList newArgs;
					std::copy(irFunArgs.begin(), irFunArgs.begin()+numFixedParams, std::back_inserter(newArgs));
					newArgs.push_back(packedArgList);
					return converter.getIRBuilder().callExpr(irExpr->getType(), irCall->getFunctionExpr(), newArgs);
				}
			}
		}
		return irExpr;
	}

} // extensions
} // frontend
} // insieme
