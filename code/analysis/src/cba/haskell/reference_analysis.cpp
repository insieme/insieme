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

#include "insieme/analysis/cba/common/failure.h"
#include "insieme/analysis/cba/haskell/reference_analysis.h"
#include "insieme/core/lang/reference.h"

enum class Result : int {
	Yes=0,
	Maybe=1,
	No=2,
};

extern "C" {

	namespace ia = insieme::analysis::cba;
	namespace hat = ia::haskell;

	// Analysis
	hat::AnalysisResult<Result>* hat_check_null(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);
	hat::AnalysisResult<Result>* hat_check_extern(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);
	hat::AnalysisResult<ia::MemoryLocationSet*>* hat_memory_locations(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);

}

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	namespace {

		Result checkNull(Context& ctxt, const core::ExpressionAddress& expr) {
			auto expr_hs = ctxt.resolveNodeAddress(expr);
			auto result = ctxt.runAnalysis<Result>(hat_check_null, expr_hs);
			if(!result) throw AnalysisFailure("Timeout in Reference Analysis");
			return *result;
		}

	}

	bool isNull(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkNull(ctxt, expr) == Result::Yes;
	}

	bool mayBeNull(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkNull(ctxt, expr) == Result::Maybe;
	}

	bool notNull(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkNull(ctxt, expr) == Result::No;
	}


	namespace {

		Result checkExtern(Context& ctxt, const core::ExpressionAddress& expr) {
			auto expr_hs = ctxt.resolveNodeAddress(expr);
			auto result = ctxt.runAnalysis<Result>(hat_check_extern, expr_hs);
			if(!result) throw AnalysisFailure("Timeout in Reference Analysis");
			return *result;
		}
	}

	bool isExtern(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkExtern(ctxt, expr) == Result::Yes;
	}

	bool mayBeExtern(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkExtern(ctxt, expr) == Result::Maybe;
	}

	bool notExtern(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkExtern(ctxt, expr) == Result::No;
	}

	MemoryLocationSet getReferencedMemoryLocations(Context& ctxt, const core::ExpressionAddress& expr) {
		auto expr_hs = ctxt.resolveNodeAddress(expr);
		auto result = ctxt.runAnalysis<ia::MemoryLocationSet*>(hat_memory_locations, expr_hs);
		if(!result) throw AnalysisFailure("TImeout in Reference Analysis");
		return *result;
	}

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
