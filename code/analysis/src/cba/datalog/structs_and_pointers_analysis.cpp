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

#include "insieme/analysis/cba/datalog/structs_and_pointers_analysis.h"

#include <iostream>

#include "insieme/analysis/cba/datalog/framework/souffle_extractor.h"

#include "souffle/gen/pointers.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	#ifdef ResultValueIsInteger
		#define convertValue std::to_string
	#else
		#define convertValue analysis.getSymbolTable().resolve
	#endif


	PointerResults runPointerAnalysis(Context& context, const std::set<core::ExpressionAddress>& targets) {
		PointerResults results;

		for (const auto &target : targets) {
			auto res = runPointerAnalysis(context, target);
			results[target] = res;
		}

		return results;

	}

	PointerResult runPointerAnalysis(Context &context, const core::ExpressionAddress &target) {
		const bool debug = false;

		// Instantiate analysis
		auto &analysis = context.getAnalysis<souffle::Sf_pointers>(target.getRootNode(), debug);

		// Get ID of target
		int targetExprID = context.getNodeID(target, debug);

		// Read result
		auto &resultRel = analysis.rel_Result;
		auto filtered = resultRel.template equalRange<0>({{targetExprID,0,0}});

		const auto &res = *(filtered.begin());
		const bool &defined = res[1];
		const auto &value = convertValue(res[2]);

		return std::make_pair(defined, value);
	}

	#undef convertValue

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
