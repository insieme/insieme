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

#include <gtest/gtest.h>

#include <string>

#include "insieme/transform/progress_estimation.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"


namespace insieme {
namespace transform {

	core::NodeManager mgr;
	core::IRBuilder builder(mgr);

	TEST(IrExtension, Basic) {
		const auto& ext = mgr.getLangExtension<ProgressEstomationExtension>();

		const auto& lit = ext.getProgressReportingLiteral();
		ASSERT_TRUE(lit);

		const auto& funType = lit->getType().isa<core::FunctionTypePtr>();
		ASSERT_TRUE(funType);

		const auto desiredType = builder.parseType("(uint<16>) -> unit");
		EXPECT_EQ(desiredType, funType);
	}

	TEST(IrExtension, Builder) {
		const auto& ext = mgr.getLangExtension<ProgressEstomationExtension>();

		const auto call = buildProgressReportingCall(mgr, 0);
		assert_correct_ir(call);

		ASSERT_TRUE(ext.isCallOfProgressReportingLiteral(call));
		const auto& funType = call->getFunctionExpr()->getType().isa<core::FunctionTypePtr>();
		const auto desiredType = builder.parseType("(uint<16>) -> unit");
		ASSERT_EQ(desiredType, funType);

		const auto& arg = core::analysis::getArgument(call, 0);
		EXPECT_EQ(builder.getLangBasic().getUInt16(), arg->getType());
	}

	TEST(IrExtension, Extractor) {
		const auto call0 = buildProgressReportingCall(mgr, 0);
		EXPECT_EQ(0, getReportedProgress(call0));

		const auto call1 = buildProgressReportingCall(mgr, 42);
		EXPECT_EQ(42, getReportedProgress(call1));

		const auto call2 = buildProgressReportingCall(mgr, 9876543210ull);
		EXPECT_EQ(9876543210ull, getReportedProgress(call2));
	}


	#define TEST_PROGRESS(REPORTING_LIMIT, INPUT_IR, DESIRED_IR)                                           \
	{                                                                                                      \
		const auto _input = builder.normalize(builder.parseStmt(INPUT_IR));                                  \
		assert_correct_ir(_input);                                                                           \
		const auto _finalDesiredIr = std::string("decl report_progress : (uint<16>) -> unit;") + DESIRED_IR; \
		const auto _desiredOutput = builder.normalize(builder.parseStmt(_finalDesiredIr));                   \
		assert_correct_ir(_desiredOutput);                                                                   \
		const auto _withProgressEstimation = applyProgressEstimation(_input, REPORTING_LIMIT);               \
		EXPECT_TRUE(_desiredOutput == _withProgressEstimation)                                               \
		            << "\tActual Pretty: \n" << dumpReadable(_withProgressEstimation) << "\n\n"              \
		            << "\tExpect Pretty: \n" << dumpReadable(_desiredOutput) << "\n";                        \
	}


	TEST(ProgressEstimation, SimpleStmts) {
		const auto input = R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;      // effort 1
				var ref<real<8>,f,f,plain> v2 = 7.0;      // effort 1
				var ref<real<8>,f,f,plain> v3 = 8.0;      // effort 1
				var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1; // effort 5
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1; // effort 5
			})";

		TEST_PROGRESS(1, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				report_progress(1ull);
				var ref<real<8>,f,f,plain> v2 = 7.0;
				report_progress(1ull);
				var ref<real<8>,f,f,plain> v3 = 8.0;
				report_progress(1ull);
				var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
				report_progress(5ull);
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(5ull);
			})");

		TEST_PROGRESS(2, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				var ref<real<8>,f,f,plain> v2 = 7.0;
				report_progress(2ull);
				var ref<real<8>,f,f,plain> v3 = 8.0;
				report_progress(1ull);
				var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
				report_progress(5ull);
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(5ull);
			})");

		TEST_PROGRESS(5, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				var ref<real<8>,f,f,plain> v2 = 7.0;
				var ref<real<8>,f,f,plain> v3 = 8.0;
				report_progress(3ull);
				var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
				report_progress(5ull);
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(5ull);
			})");

		TEST_PROGRESS(10, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				var ref<real<8>,f,f,plain> v2 = 7.0;
				var ref<real<8>,f,f,plain> v3 = 8.0;
				var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
				report_progress(8ull);
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(5ull);
			})");

		TEST_PROGRESS(15, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				var ref<real<8>,f,f,plain> v2 = 7.0;
				var ref<real<8>,f,f,plain> v3 = 8.0;
				var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(13ull);
			})");
	}

	TEST(ProgressEstimation, NestedCompounds) {
		const auto input = R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;          // effort 1
				{
					var ref<real<8>,f,f,plain> v2 = 7.0;        // effort 1
					var ref<real<8>,f,f,plain> v3 = 8.0;        // effort 1
					{
						var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1; // effort 5
					}
				}
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;     // effort 5
			})";

		TEST_PROGRESS(1, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				{
					report_progress(1ull);
					var ref<real<8>,f,f,plain> v2 = 7.0;
					report_progress(1ull);
					var ref<real<8>,f,f,plain> v3 = 8.0;
					{
						report_progress(1ull);
						var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
					}
				}
				report_progress(5ull);
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(5ull);
			})");

		TEST_PROGRESS(2, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				{
					var ref<real<8>,f,f,plain> v2 = 7.0;
					report_progress(2ull);
					var ref<real<8>,f,f,plain> v3 = 8.0;
					{
						report_progress(1ull);
						var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
					}
				}
				report_progress(5ull);
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(5ull);
			})");

		TEST_PROGRESS(5, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				{
					var ref<real<8>,f,f,plain> v2 = 7.0;
					var ref<real<8>,f,f,plain> v3 = 8.0;
					{
						report_progress(3ull);
						var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
					}
				}
				report_progress(5ull);
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(5ull);
			})");

		TEST_PROGRESS(10, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				{
					var ref<real<8>,f,f,plain> v2 = 7.0;
					var ref<real<8>,f,f,plain> v3 = 8.0;
					{
						var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
					}
				}
				report_progress(8ull);
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(5ull);
			})");

		TEST_PROGRESS(15, input, R"(
			{
				var ref<real<8>,f,f,plain> v1 = 6.0;
				{
					var ref<real<8>,f,f,plain> v2 = 7.0;
					var ref<real<8>,f,f,plain> v3 = 8.0;
					{
						var ref<int<4>,f,f,plain> v4 = 5+4+3+2+1;
					}
				}
				var ref<int<4>,f,f,plain> v5 = 5+4+3+2+1;
				report_progress(13ull);
			})");
	}

	TEST(ProgressEstimation, SimpleCalls) {
		const auto input = R"(
			def a = (p : int<4>) -> int<4> {
				return p;                                 // effort 1 + 1 implicit deref
			};
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;         // effort 1
				return p;                                 // effort 1 + 1 implicit deref
			};
			def c = (p : int<4>) -> int<4> {
				a(p);
				b(p);
				return p;                                 // effort 1 + 1 implicit deref
			};
			{
				a(0);
				b(1);
				a(0) + b(1);
				c(2);
				b(a(b(a(3))));
			})";

		TEST_PROGRESS(1, input, R"(
			def a = (p : int<4>) -> int<4> {
				return p;                                 // unreported effort: 2
			};
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				report_progress(1ull);
				return p;                                 // unreported effort: 2
			};
			def c = (p : int<4>) -> int<4> {
				a(p);
				report_progress(8ull);                    // 2 unreported + 5 call overhead + 1 implicit deref
				b(p);
				report_progress(8ull);                    // 2 unreported + 5 call overhead + 1 implicit deref
				return p;                                 // unreported effort: 2
			};
			{
				a(0);
				report_progress(7ull);                    // 2 unreported + 5 call overhead
				b(1);
				report_progress(7ull);                    // 2 unreported + 5 call overhead
				a(0) + b(1);
				report_progress(15ull);                   // 2 unreported + 2 unreported + (5 call overhead) * 2 + builtin overhead 1
				c(2);
				report_progress(7ull);                    // 2 unreported + 5 call overhead
				b(a(b(a(3))));
				report_progress(28ull);                   // (2 unreported + 5 call overhead) * 4
			})");

		TEST_PROGRESS(10, input, R"(
			def a = (p : int<4>) -> int<4> {
				return p;                                 // unreported effort: 2
			};
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				return p;                                 // unreported effort: 3
			};
			def c = (p : int<4>) -> int<4> {
				a(p);
				report_progress(8ull);                    // 2 unreported + 5 call overhead + 1 implicit deref
				b(p);
				report_progress(9ull);                    // 3 unreported + 5 call overhead + 1 implicit deref
				return p;                                 // unreported effort: 2
			};
			{
				a(0);
				report_progress(7ull);                    // 2 unreported + 5 call overhead
				b(1);
				report_progress(8ull);                    // 3 unreported + 5 call overhead
				a(0) + b(1);
				report_progress(16ull);                   // 2 unreported + 3 unreported + (5 call overhead) * 2 + builtin overhead 1
				c(2);
				report_progress(7ull);                    // 2 unreported + 5 call overhead
				b(a(b(a(3))));
				report_progress(30ull);                   // (2 unreported + 3 unreported) * 2 + (5 call overhead) * 4
			})");

		TEST_PROGRESS(15, input, R"(
			def a = (p : int<4>) -> int<4> {
				return p;                                 // unreported effort: 2
			};
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				return p;                                 // unreported effort: 3
			};
			def c = (p : int<4>) -> int<4> {
				a(p);
				report_progress(8ull);                    // 2 unreported + 5 call overhead + 1 implicit deref
				b(p);
				return p;                                 // unreported effort: 11
			};
			{
				a(0);
				b(1);
				report_progress(15ull);                   // 2 unreported + 3 unreported + (5 call overhead) * 2
				a(0) + b(1);
				report_progress(16ull);                   // 2 unreported + 3 unreported + (5 call overhead) * 2 + builtin overhead 1
				c(2);
				report_progress(16ull);                   // 11 unreported + 5 call overhead
				b(a(b(a(3))));
				report_progress(30ull);                   // (2 unreported + 3 unreported) * 2 + (5 call overhead) * 4
			})");
	}

} // end namespace transform
} // end namespace insieme
