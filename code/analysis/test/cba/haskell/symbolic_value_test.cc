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

#include "insieme/analysis/cba/haskell/symbolic_value_analysis.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/dump/json_dump.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	using namespace insieme::core;

	namespace {

		using symbolic_value_list = std::vector<std::string>;


		symbolic_value_list toList(const SymbolicValueSet& values) {
			symbolic_value_list res;

			// handle universal sets
			if (values.isUniversal()) {
				res.push_back("-all-");
				return res;
			}

			// convert values
			for(const auto& cur : values) {
				res.push_back(toString(*cur));
			}

			// sort values
			std::sort(res.begin(),res.end());

			// done
			return res;
		}


		symbolic_value_list getValues(NodeManager& mgr, const std::string& code, bool debug = false) {
			IRBuilder builder(mgr);

			// parse the value set
			auto expr = builder.parseExpr(code);
			assert_correct_ir(expr);

			// compute the symbolic values
			Context ctxt;
			auto res = getSymbolicValue(ctxt,core::ExpressionAddress(expr));

			if (debug) {
				insieme::core::dump::json::dumpIR("ir.json",expr);
				ctxt.dumpStatistics();
				ctxt.dumpSolution();
			}

			// convert set to list of values
			return toList(res);

		}

	}

	TEST(SymbolicValues, Literals) {
		NodeManager mgr;

		// test literals of differnt types
		EXPECT_EQ("[12]",toString(getValues(mgr,"12")));
		EXPECT_EQ("['c']",toString(getValues(mgr,"'c'")));
		EXPECT_EQ("[true]",toString(getValues(mgr,"true")));
		EXPECT_EQ("[fun]",toString(getValues(mgr,"lit(\"fun\":(int<4>)->int<4>)")));

	}

	TEST(DISABLED_SymbolicValues, Operators) {
		NodeManager mgr;

		// test composed values
		EXPECT_EQ("[1+2]",toString(getValues(mgr,"1 + 2")));

		// more complex values
		EXPECT_EQ("[1+2*3]",toString(getValues(mgr,"1 + 2 * 3")));

	}

	TEST(SymbolicValues, Forwarding) {
		NodeManager mgr;

		// test composed values
		EXPECT_EQ(
			"[12]",
			toString(getValues(mgr,
				"def id = ( x : 'a ) -> 'a {"
				"	return x;"
				"};"
				""
				"id(12)"
			))
		);

	}

	TEST(DISABLED_SymbolicValues, Increment) {
		NodeManager mgr;

		// test composed values
		EXPECT_EQ(
			"[3+1]",
			toString(getValues(mgr,
				R"(
					def id = ( x : int<4> ) -> int<4> {
						return x + 1;
					};
					
					id(3)
				)"
			))
		);

	}

	TEST(DISABLED_SymbolicValues, Computation) {
		NodeManager mgr;

		// test composed values
		EXPECT_EQ(
			"[3%((3*2)+1)]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : int<4> ) -> int<4> {
						var ref<int<4>> tmp;
						tmp = x;
						tmp = *tmp * 2;
						tmp = *tmp + 1;
						tmp = 3 % (*tmp);
						return *tmp;
					};
					
					fun(3)
				)"
			))
		);

	}


} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

