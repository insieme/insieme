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

#include "insieme/analysis/haskell/dataflow.h"

#include "insieme/analysis/haskell/adapter.h"

#include "insieme/utils/assert.h"

using namespace insieme::core;

namespace insieme {
namespace analysis {
namespace haskell {

	VariableAddress getDefinitionPoint(const VariableAddress& var) {
		Context ctx(var.getRootNode());
		return ctx.getDefinitionPoint(var);
	}

	namespace {
		BooleanAnalysisResult checkBoolean(const ExpressionAddress& expr) {
			Context ctx(expr.getRootNode());
			return ctx.checkBoolean(expr);
		}
	}

	bool isTrue(const core::ExpressionAddress& expr) {
		return checkBoolean(expr) == BooleanAnalysisResult_AlwaysTrue;
	}

	bool isFalse(const core::ExpressionAddress& expr) {
		return checkBoolean(expr) == BooleanAnalysisResult_AlwaysFalse;
	}

	bool mayBeTrue(const core::ExpressionAddress& expr) {
		auto res = checkBoolean(expr);
		return res == BooleanAnalysisResult_AlwaysTrue || res == BooleanAnalysisResult_Both;
	}

	bool mayBeFalse(const core::ExpressionAddress& expr) {
		auto res = checkBoolean(expr);
		return res == BooleanAnalysisResult_AlwaysFalse || res == BooleanAnalysisResult_Both;
	}

	namespace {
		AliasAnalysisResult checkAlias(const ExpressionAddress& x, const ExpressionAddress& y) {
			assert_eq(x.getRootNode(), y.getRootNode());
			Context ctx(x.getRootNode());
			return ctx.checkAlias(x, y);
		}
	}

	bool areAlias(const ExpressionAddress& x, const ExpressionAddress& y) {
		return checkAlias(x, y) == AliasAnalysisResult_AreAlias;
	}

	bool mayAlias(const ExpressionAddress& x, const ExpressionAddress& y) {
		return checkAlias(x, y) != AliasAnalysisResult_NotAlias;
	}

	bool notAlias(const ExpressionAddress& x, const ExpressionAddress& y) {
		return checkAlias(x, y) == AliasAnalysisResult_NotAlias;
	}

	ArithmeticSet getArithmeticValue(const core::ExpressionAddress& expr) {
		Context ctx(expr.getRootNode());
		return ctx.arithmeticValue(expr);
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme
