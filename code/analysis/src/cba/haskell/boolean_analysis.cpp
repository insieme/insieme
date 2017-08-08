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

#include "insieme/analysis/cba/haskell/boolean_analysis.h"
#include "insieme/analysis/cba/common/failure.h"

extern "C" {

	namespace hat = insieme::analysis::cba::haskell;

	// Analysis
	hat::AnalysisResult<int>* hat_check_boolean(hat::StablePtr ctxt, const hat::HaskellNodeAddress expr_hs);

}

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	enum class BooleanAnalysisResult : int {
		AlwaysTrue = 0,
		AlwaysFalse = 1,
		Both = 2,
		Neither = 3,
	};

	BooleanAnalysisResult checkBoolean(Context& ctxt, const core::ExpressionAddress& expr) {
		auto expr_hs = ctxt.resolveNodeAddress(expr);
		auto result = hat_check_boolean(ctxt.getHaskellContext(), expr_hs);
		auto value = static_cast<BooleanAnalysisResult>(ctxt.unwrapResult(result));
		if(value == BooleanAnalysisResult::Neither) {
			std::vector<std::string> msgs{"Boolean Analysis Error"};
			throw AnalysisFailure(msgs);
		}
		return value;
	}

	bool isTrue(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkBoolean(ctxt, expr) == BooleanAnalysisResult::AlwaysTrue;
	}

	bool isFalse(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkBoolean(ctxt, expr) == BooleanAnalysisResult::AlwaysFalse;
	}

	bool mayBeTrue(Context& ctxt, const core::ExpressionAddress& expr) {
		auto res = checkBoolean(ctxt, expr);
		return res == BooleanAnalysisResult::AlwaysTrue || res == BooleanAnalysisResult::Both;
	}

	bool mayBeFalse(Context& ctxt, const core::ExpressionAddress& expr) {
		auto res = checkBoolean(ctxt, expr);
		return res == BooleanAnalysisResult::AlwaysFalse || res == BooleanAnalysisResult::Both;
	}

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
