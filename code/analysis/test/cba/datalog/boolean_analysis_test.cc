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
#include <gtest/gtest.h>

#include "insieme/analysis/cba/datalog/interface.h"

#include "../common/boolean_analysis_test.inc"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * Run the boolean value tests using the datalog backend.
	 */
	// NOTE: DISABLED BECAUSE VALGRIND TEST FAILS
	INSTANTIATE_TYPED_TEST_CASE_P(DISABLED_Datalog, BooleanValue, DatalogEngine);


	// NOTE: DISABLED BECAUSE VALGRIND TEST FAILS
	TEST(DataflowAnalysis, DISABLED_FailureDetection) {
		using namespace core;

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto analyse = [&](const std::string& code) {
			auto expr = builder.parseExpr(code);
			datalog::Context ctxt;
			datalog::isFalse(ctxt, ExpressionAddress(expr));
		};

		{
			auto input = "ref_assign";
			ASSERT_THROW(analyse(input), AnalysisFailure);

			try {
				analyse(input);
			} catch (const AnalysisFailure& af) {
				// TODO: better checking of error string
				//EXPECT_STREQ("Encountered 1 failures during analysis:\n\tError: Found a ref_assign whose parent is not a CallExpr!", af.what());
			}
		}

		{
			auto input = "(x : 'a)-> 'a{ return x; }(ref_assign)";
			ASSERT_THROW(analyse(input), AnalysisFailure);
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

