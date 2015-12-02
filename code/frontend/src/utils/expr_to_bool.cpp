/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/utils/expr_to_bool.h"

#include "insieme/frontend/utils/frontend_inspire_module.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/pointer.h"

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

		assert_not_implemented() << "Trying to build bool expression from unsupported type " << *t;
		return ExpressionPtr();
	}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme

