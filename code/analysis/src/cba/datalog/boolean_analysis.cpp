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
#include "insieme/analysis/cba/datalog/boolean_analysis.h"

#include "souffle/gen/boolean_analysis.h"

#include "insieme/analysis/cba/datalog/framework/souffle_extractor.h"
#include "insieme/analysis/cba/datalog/framework/toolbox.h"


namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	enum Result {
		TRUE,
		FALSE,
		TRUE_OR_FALSE
	};

	Result getBoolValue(Context& context, const core::ExpressionAddress& expr) {
		const bool debug = false;

		// Instantiate the analysis
		auto& analysis = context.getAnalysis<souffle::Sf_boolean_analysis>(expr.getRootNode(), debug);

		// Get ID of target expression
		int targetID = context.getNodeID(expr);

		// Read result
		auto& resultRelation = analysis.rel_result;
		auto resultRelationContents = resultRelation.template equalRange<0>({{targetID,0}});

		if (debug) {

			std::cout << std::endl << "-------- Results: --------" << std::endl;
			for(const auto& cur : resultRelationContents)
				std::cout << cur << std::endl;

			if (resultRelationContents.empty())
				std::cout << "INCOMPLETE ANALYSIS!!!!" << std::endl;

		} else {
			assert_false(resultRelationContents.empty()) << "Incomplete analysis!";
			if (resultRelationContents.empty())
				std::cout << "INCOMPLETE ANALYSIS!!!!" << std::endl;
		}

		// Get possible result value
		auto possibleResult = (*resultRelationContents.begin())[1];

		// Check if there's more than one value ( = can't tell result)
		if (++resultRelationContents.begin() != resultRelationContents.end())
			return TRUE_OR_FALSE;

		// Only one value: Return result
		return (possibleResult) ? TRUE : FALSE;
	}


	bool isTrue(Context& c, const core::ExpressionAddress& expr) {
		return getBoolValue(c,expr) == TRUE;
	}

	bool isFalse(Context& c, const core::ExpressionAddress& expr) {
		return getBoolValue(c,expr) == FALSE;
	}

	bool mayBeTrue(Context& c, const core::ExpressionAddress& expr) {
		return getBoolValue(c,expr) != FALSE;
	}

	bool mayBeFalse(Context& c, const core::ExpressionAddress& expr) {
		return getBoolValue(c,expr) != TRUE;
	}

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
