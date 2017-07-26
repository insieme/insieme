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

#include "insieme/backend/addons/comma_operator.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/operator_converter.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/manipulation.h"

namespace insieme {
namespace backend {
namespace addons {

	void CommaOperator::installOn(Converter& converter) const {
		auto& mgr = converter.getNodeManager();
		core::IRBuilder builder(mgr);
		auto op = builder.normalize(builder.parseExpr("def comma_operator = (lhs : () => 'a, rhs : () => 'b) -> 'b { lhs(); return rhs(); }; comma_operator"));

		// register operator
		#include "insieme/backend/operator_converter_begin.inc"
			converter.getFunctionManager().getOperatorConverterTable()[op] = OP_CONVERTER {
				return c_ast::parentheses(
				    c_ast::comma(CONVERT_EXPR(core::transform::evalLazy(NODE_MANAGER, ARG(0))), CONVERT_EXPR(core::transform::evalLazy(NODE_MANAGER, ARG(1)))));
			};
        #include "insieme/backend/operator_converter_end.inc"
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme
