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
#include "insieme/analysis/cba/datalog/integer_analysis.h"

#include "insieme/analysis/cba/datalog/framework/souffle_extractor.h"
#include "insieme/analysis/cba/datalog/framework/toolbox.h"

#include "insieme/core/ir_address.h"

#include "souffle/gen/integer_analysis.h"


namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	IntegerSet getIntegerValues(Context& context, const core::ExpressionAddress& expr) {
		const bool debug = false;

		// instantiate the analysis
		auto &analysis = context.getAnalysis<souffle::Sf_integer_analysis>(expr.getRootNode(), debug);

		// Get ID for variable we are interested in (if it's there)
		int targetExprID = context.getNodeID(expr, debug);

		assert_ne(targetExprID, -1) << "Integer analysis incomplete (target expr not found)!";

		// Get result
		auto &resultRel = analysis.rel_Result;

		auto filtered = resultRel.template equalRange<0>({{targetExprID,0,0}});

		assert_false(filtered.empty()) << "Integer analysis incomplete (target expr not found)!";

		// Read values. Return the universal integer set if an undefined value is encountered
		IntegerSet res;
		for (auto it = filtered.begin(); it != filtered.end(); ++it) {
			const auto &defined = (*it)[1];
			const auto &value = (*it)[2];

			if (!defined)
				return IntegerSet::getUniversal();

			res.insert(value);
		}

		return res;
	}

	bool isIntegerConstant(Context& context, const core::ExpressionAddress& expr) {
		return getIntegerValues(context, expr).size() == 1;
	}



	namespace integer {

		bool areEqual(Context& context, const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
			auto resA = getIntegerValues(context, a);
			assert_ne(0, resA.size());

			/* Return early to save computation time */
			if (resA.size() != 1)
				return false;

			auto resB = getIntegerValues(context, b);
			assert_ne(0, resB.size());

			/* Both sides are constants and have equal value */
			return resA == resB;
		}

		bool mayEqual(Context& context, const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
			auto resA = getIntegerValues(context, a);
			auto resB = getIntegerValues(context, b);

			assert_ne(0, resA.size());
			assert_ne(0, resB.size());

			/* True if set intersection size > 0 */
			for (const auto &valA : resA)
				for (const auto &valB : resB)
					if (valA == valB)
						return true;
			return false;
		}

		bool areNotEqual(Context& context, const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
			auto resA = getIntegerValues(context, a);
			auto resB = getIntegerValues(context, b);

			assert_ne(0, resA.size());
			assert_ne(0, resB.size());

			/* True if sets are disjoint */
			for (const auto &valA : resA)
				for (const auto &valB : resB)
					if (valA == valB)
						return false;
			return true;
		}

		bool mayNotEqual(Context& context, const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
			auto resA = getIntegerValues(context, a);
			auto resB = getIntegerValues(context, b);

			assert_ne(0, resA.size());
			assert_ne(0, resB.size());

			/* True if set size > 1 or elements not equal */
			if (resA.size() > 1 || resB.size() > 1)
				return true;
			return resA != resB;
		}

	}


} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
