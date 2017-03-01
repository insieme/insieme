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
#include "insieme/frontend/utils/expr_to_bool.h"

#include "insieme/frontend/utils/frontend_inspire_module.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/enum.h"

namespace insieme {
namespace frontend {
namespace utils {
	using namespace core;

	ExpressionPtr exprToBool(const ExpressionPtr& expr) {
		auto& mgr = expr->getNodeManager();
		auto& basic = mgr.getLangBasic();
		auto& fMod = mgr.getLangExtension<FrontendInspireModule>();
		IRBuilder builder(mgr);

		auto t = expr->getType();

		// no need to do anything if already bool
		if(basic.isBool(t)) return expr;

		// if it is a bool to int conversion, strip it off
		if(core::analysis::isCallOf(expr, fMod.getBoolToInt())) return expr.as<CallExprPtr>()->getArgument(0);

		// if numeric or char, check against 0
		if(basic.isInt(t) || basic.isReal(t) || basic.isChar(t)) return builder.ne(expr, builder.getZero(t));

		// if pointer, check against equality with PtrNull
		auto& pExt = builder.getExtension<core::lang::PointerExtension>();
		if(lang::isPointer(t)) return builder.callExpr(basic.getBool(), pExt.getPtrNotEqual(), expr, lang::buildPtrNull(t));

		//if enum type, cast to boolean
		if(lang::isEnum(t)) return exprToBool(core::lang::buildEnumToInt(expr));

		assert_not_implemented() << "Trying to build bool expression from unsupported type " << *t;
		return ExpressionPtr();
	}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme

