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
