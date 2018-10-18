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

	TEST(IrExtension, Basic) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<ProgressEstomationExtension>();

		auto checkType = [&](const core::LiteralPtr& lit) {
			ASSERT_TRUE(lit);

			const auto& funType = lit->getType().isa<core::FunctionTypePtr>();
			ASSERT_TRUE(funType);

			const auto desiredType = builder.parseType("(uint<16>) -> unit");
			EXPECT_EQ(desiredType, funType);
		};

		checkType(ext.getProgressReportingLiteral());
		checkType(ext.getProgressReportingThreadLiteral());
	}

	TEST(IrExtension, Builder) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<ProgressEstomationExtension>();

		{
			const auto call = buildProgressReportingCall(mgr, 0);
			assert_correct_ir(call);

			ASSERT_TRUE(ext.isCallOfProgressReportingLiteral(call));
			const auto& funType = call->getFunctionExpr()->getType().isa<core::FunctionTypePtr>();
			const auto desiredType = builder.parseType("(uint<16>) -> unit");
			ASSERT_EQ(desiredType, funType);

			const auto& arg = core::analysis::getArgument(call, 0);
			EXPECT_EQ(builder.getLangBasic().getUInt16(), arg->getType());
		}

		{
			const auto call = buildProgressReportingThreadCall(mgr, 0);
			assert_correct_ir(call);

			ASSERT_TRUE(ext.isCallOfProgressReportingThreadLiteral(call));
			const auto& funType = call->getFunctionExpr()->getType().isa<core::FunctionTypePtr>();
			const auto desiredType = builder.parseType("(uint<16>) -> unit");
			ASSERT_EQ(desiredType, funType);

			const auto& arg = core::analysis::getArgument(call, 0);
			EXPECT_EQ(builder.getLangBasic().getUInt16(), arg->getType());
		}
	}

	TEST(IrExtension, Extractor) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		const auto negativeCall = builder.negateExpr(builder.getLangBasic().getTrue());
		EXPECT_EQ(0, getReportedProgress(negativeCall));

		{
			const auto call0 = buildProgressReportingCall(mgr, 0);
			EXPECT_EQ(0, getReportedProgress(call0));

			const auto call1 = buildProgressReportingCall(mgr, 42);
			EXPECT_EQ(42, getReportedProgress(call1));

			const auto call2 = buildProgressReportingCall(mgr, 9876543210ull);
			EXPECT_EQ(9876543210ull, getReportedProgress(call2));
		}

		{
			const auto call0 = buildProgressReportingThreadCall(mgr, 0);
			EXPECT_EQ(0, getReportedProgress(call0));

			const auto call1 = buildProgressReportingThreadCall(mgr, 42);
			EXPECT_EQ(42, getReportedProgress(call1));

			const auto call2 = buildProgressReportingThreadCall(mgr, 9876543210ull);
			EXPECT_EQ(9876543210ull, getReportedProgress(call2));
		}
	}


	#define TEST_PROGRESS(REPORTING_LIMIT, INPUT_IR, DESIRED_IR)                                           \
	{                                                                                                      \
		core::NodeManager mgr;                                                                               \
		core::IRBuilder builder(mgr);                                                                        \
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


	TEST(SimpleTests, Basic) {
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

	TEST(SimpleTests, NestedCompounds) {
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

	TEST(SimpleTests, SimpleCalls) {
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
			def d = () -> unit {
				var ref<int<4>,f,f,plain> v1 = 0;         // effort 1
				var ref<int<4>,f,f,plain> v2 = 1;         // effort 1
			};
			{
				a(0);
				b(1);
				a(0) + b(1);
				c(2);
				b(a(b(a(3))));
				d();
			})";

		TEST_PROGRESS(1, input, R"(
			def a = (p : int<4>) -> int<4> {
				report_progress(2ull);                    // 1 from return below, 1 from implicit deref below
				return p;                                 // unreported effort: 0
			};
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				report_progress(3ull);                    // 1 from stmt above, 1 from return below, 1 from implicit deref below
				return p;                                 // unreported effort: 0
			};
			def c = (p : int<4>) -> int<4> {
				a(p);
				report_progress(6ull);                    // 5 call overhead + 1 implicit deref
				b(p);
				report_progress(8ull);                    // 5 call overhead + 1 implicit deref, 1 from return below, 1 from implicit deref below
				return p;                                 // unreported effort: 0
			};
			def d = () -> unit {
				var ref<int<4>,f,f,plain> v1 = 0;
				report_progress(1ull);                    // 1 from stmt above
				var ref<int<4>,f,f,plain> v2 = 1;         // unreported effort: 1
			};
			{
				a(0);
				report_progress(5ull);                    // 5 call overhead
				b(1);
				report_progress(5ull);                    // 5 call overhead
				a(0) + b(1);
				report_progress(11ull);                   // (5 call overhead) * 2 + builtin overhead 1
				c(2);
				report_progress(5ull);                    // 5 call overhead
				b(a(b(a(3))));
				report_progress(20ull);                   // (5 call overhead) * 4
				d();
				report_progress(6ull);                    // 1 unreported + 5 call overhead
			})");

		TEST_PROGRESS(10, input, R"(
			def a = (p : int<4>) -> int<4> {
				return p;                                 // unreported effort: 2 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				return p;                                 // unreported effort: 3 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			def c = (p : int<4>) -> int<4> {
				a(p);
				report_progress(8ull);                    // 2 unreported + 5 call overhead + 1 implicit deref
				b(p);
				report_progress(11ull);                   // 3 unreported + 5 call overhead + 1 implicit deref, 1 from return below, 1 from implicit deref below
				return p;                                 // unreported effort: 0
			};
			def d = () -> unit {
				var ref<int<4>,f,f,plain> v1 = 0;
				var ref<int<4>,f,f,plain> v2 = 1;         // unreported effort: 2
			};
			{
				a(0);
				report_progress(7ull);                    // 2 unreported + 5 call overhead
				b(1);
				report_progress(8ull);                    // 3 unreported + 5 call overhead
				a(0) + b(1);
				report_progress(16ull);                   // 2 unreported + 3 unreported + (5 call overhead) * 2 + builtin overhead 1
				c(2);
				report_progress(5ull);                    // 5 call overhead
				b(a(b(a(3))));
				report_progress(30ull);                   // (2 unreported + 3 unreported) * 2 + (5 call overhead) * 4
				d();
				report_progress(7ull);                    // 2 unreported + 5 call overhead
			})");

		TEST_PROGRESS(15, input, R"(
			def a = (p : int<4>) -> int<4> {
				return p;                                 // unreported effort: 2 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				return p;                                 // unreported effort: 3 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			def c = (p : int<4>) -> int<4> {
				a(p);
				report_progress(8ull);                    // 2 unreported + 5 call overhead + 1 implicit deref
				b(p);
				report_progress(11ull);                   // 3 unreported + 5 call overhead + 1 implicit deref, 1 from return below, 1 from implicit deref below
				return p;                                 // unreported effort: 0
			};
			def d = () -> unit {
				var ref<int<4>,f,f,plain> v1 = 0;
				var ref<int<4>,f,f,plain> v2 = 1;         // unreported effort: 2
			};
			{
				a(0);
				b(1);
				report_progress(15ull);                   // 2 unreported + 3 unreported + (5 call overhead) * 2
				a(0) + b(1);
				report_progress(16ull);                   // 2 unreported + 3 unreported + (5 call overhead) * 2 + builtin overhead 1
				c(2);
				report_progress(5ull);                    // 5 call overhead
				b(a(b(a(3))));
				report_progress(30ull);                   // (2 unreported + 3 unreported) * 2 + (5 call overhead) * 4
				d();
				report_progress(7ull);                    // 2 unreported + 5 call overhead
			})");
	}

	TEST(ForLoops, Simple) {
		const auto input = R"(
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				for(int<4> v1 = 0 .. 10 : 1) {
					v0 = v0 + v1;
				};
				var ref<int<4>,f,f,plain> v2 = 2;
			})";

		TEST_PROGRESS(1, input, R"(
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				report_progress(2ull);                    // 1 from stmt above, 1 for iterator variable overhead
				for(int<4> v1 = 0 .. 10 : 1) {
					report_progress(4ull);                  // 2 branch overhead, 1 condition comparison, 1 iterator update
					v0 = v0 + v1;
					report_progress(3ull);                  // 1 assignment, 1 implicit deref, 1 builtin. Iterator variable access is free
				};
				var ref<int<4>,f,f,plain> v2 = 2;
				report_progress(1ull);                    // 1 from stmt above
			})");

		TEST_PROGRESS(15, input, R"(
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				report_progress(2ull);                    // 1 from stmt above, 1 for iterator variable overhead
				for(int<4> v1 = 0 .. 10 : 1) {
					v0 = v0 + v1;
					report_progress(7ull);                  // 2 branch overhead, 1 condition comparison, 1 iterator update, 1 assignment, 1 implicit deref, 1 builtin. Iterator variable access is free
				};
				var ref<int<4>,f,f,plain> v2 = 2;
				report_progress(1ull);                    // 1 from stmt above
			})");
	}

	TEST(ForLoops, FunctionCalls) {
		const auto input = R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;         // effort 1
				return p;                                 // effort 1 + 1 implicit deref
			};
			{
				for(int<4> v1 = 0 .. 10 : 1) {
					b(v1);
				};
			})";

		TEST_PROGRESS(1, input, R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				report_progress(3ull);                    // 1 from stmt above, 1 from return below, 1 from implicit deref below
				return p;                                 // unreported effort: 0
			};
			{
				report_progress(1ull);                    // 1 for iterator variable overhead
				for(int<4> v1 = 0 .. 10 : 1) {
					report_progress(4ull);                  // 2 branch overhead, 1 condition comparison, 1 iterator update
					b(v1);
					report_progress(5ull);                  // 5 call overhead
				};
			})");

		TEST_PROGRESS(15, input, R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				return p;                                 // unreported effort: 3 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			{
				report_progress(1ull);                    // 1 for iterator variable overhead
				for(int<4> v1 = 0 .. 10 : 1) {
					b(v1);
					report_progress(12ull);                 // 2 branch overhead, 1 condition comparison, 1 iterator update, 3 unreported + 5 call overhead
				};
			})");
	}

	TEST(WhileLoops, Simple) {
		const auto input = R"(
			{
				var ref<int<4>,f,f,plain> v0 = 0;         // unrelated code
				var ref<int<4>,f,f,plain> v1 = 1;         // iterator variable
				while(*v1<5) {
					gen_post_inc(v1);
				}
				var ref<int<4>,f,f,plain> v2 = 2;         // unrelated code
			})";

		TEST_PROGRESS(1, input, R"(
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				report_progress(1ull);
				var ref<int<4>,f,f,plain> v1 = 1;
				report_progress(1ull);
				while(*v1<5) {
					report_progress(4ull);                  // 2 branch overhead, 1 builtin from condition, 1 implicit deref from condition
					gen_post_inc(v1);
					report_progress(2ull);                  // 1 builtin, 1 implicit deref
				}
				var ref<int<4>,f,f,plain> v2 = 2;
				report_progress(1ull);
			})");

		TEST_PROGRESS(15, input, R"(
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				var ref<int<4>,f,f,plain> v1 = 1;
				report_progress(2ull);
				while(*v1<5) {
					gen_post_inc(v1);
					report_progress(6ull);                  // 2 branch overhead, 1 builtin from condition, 1 implicit deref from condition, 1 builtin , 1 implicit deref
				}
				var ref<int<4>,f,f,plain> v2 = 2;
				report_progress(1ull);
			})");
	}

	TEST(WhileLoops, FunctionCalls) {
		const auto input = R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;         // effort 1
				return p;                                 // effort 1 + 1 implicit deref
			};
			def c = (p : int<4>) -> bool {
				b(p);
				return true;                              // effort 1
			};
			{
				var ref<int<4>,f,f,plain> v1 = 1;         // iterator variable
				while(c(*v1)) {
					b(*v1);
					gen_post_inc(v1);
				}
			})";

		TEST_PROGRESS(1, input, R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				report_progress(3ull);                     // 1 from stmt above, 1 from return below, 1 from implicit deref below
				return p;                                  // unreported effort: 0
			};
			def c = (p : int<4>) -> bool {
				b(p);
				report_progress(7ull);                     // 5 call overhead + 1 implicit deref, 1 from return below
				return true;                               // unreported effort: 0
			};
			{
				var ref<int<4>,f,f,plain> v1 = 1;
				report_progress(1ull);
				while(c(*v1)) {
					report_progress(8ull);                   // 2 branch overhead, 1 implicit deref from condition, 5 call overhead
					b(*v1);
					report_progress(6ull);                   // 5 call overhead + 1 implicit deref
					gen_post_inc(v1);
					report_progress(2ull);                   // 1 builtin, 1 implicit deref
				}
			})");

		TEST_PROGRESS(15, input, R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				return p;                                  // unreported effort: 3 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			def c = (p : int<4>) -> bool {
				b(p);
				return true;                               // unreported effort: 10 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			{
				var ref<int<4>,f,f,plain> v1 = 1;
				report_progress(1ull);
				while(c(*v1)) {
					report_progress(18ull);                  // 2 branch overhead, 1 implicit deref from condition, 5 call overhead, 10 unreported
					b(*v1);
					gen_post_inc(v1);
					report_progress(11ull);                  // 3 unreported + 5 call overhead + 1 implicit deref + 1 builtin + 1 implicit deref
				}
			})");
	}

	TEST(IfStmt, Simple) {
		const auto input = R"(
			{
				var ref<int<4>,f,f,plain> v0 = 0;         // unrelated code
				if(true) {
					var ref<int<4>,f,f,plain> v1 = 1;
				}
				var ref<int<4>,f,f,plain> v2 = 2;         // unrelated code
				if(*v0 == *v2) {
					var ref<int<4>,f,f,plain> v3 = 5+4+3+2+1;
				} else {
					var ref<int<4>,f,f,plain> v4 = 4;
				}
				var ref<int<4>,f,f,plain> v5 = 5;         // unrelated code
			})";

		TEST_PROGRESS(1, input, R"(
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				if(true) {
					report_progress(3ull);                  // 1 from stmt above, 2 branch overhead
					var ref<int<4>,f,f,plain> v1 = 1;
					report_progress(1ull);                  // 1 from stmt above
				} else {
					report_progress(3ull);                  // 1 from stmt above if, 2 branch overhead (note that every if will end up with an else branch even if it had none before)
				}
				var ref<int<4>,f,f,plain> v2 = 2;
				if(*v0 == *v2) {
					report_progress(6ull);                  // 1 from stmt above if, 2 branch overhead, 1 builtin call, 2 from deref
					var ref<int<4>,f,f,plain> v3 = 5+4+3+2+1;
					report_progress(5ull);
				} else {
					report_progress(6ull);                  // 1 from stmt above if, 2 branch overhead, 1 builtin call, 2 from deref
					var ref<int<4>,f,f,plain> v4 = 4;
					report_progress(1ull);
				}
				var ref<int<4>,f,f,plain> v5 = 5;
				report_progress(1ull);                    // 1 from stmt above
			})");

		TEST_PROGRESS(15, input, R"(
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				if(true) {
					var ref<int<4>,f,f,plain> v1 = 1;
					report_progress(4ull);                  // 2 from stmts above, 2 branch overhead
				} else {
					report_progress(3ull);                  // 1 from stmt above if, 2 branch overhead (note that every if will end up with an else branch even if it had none before)
				}
				var ref<int<4>,f,f,plain> v2 = 2;
				if(*v0 == *v2) {
					var ref<int<4>,f,f,plain> v3 = 5+4+3+2+1;
					report_progress(11ull);                 // 1 from stmt above if, 5 from stmt above, 2 branch overhead, 1 builtin call, 2 from deref
				} else {
					var ref<int<4>,f,f,plain> v4 = 4;
					report_progress(7ull);                  // 1 from stmt above if, 1 from stmt above, 2 branch overhead, 1 builtin call, 2 from deref
				}
				var ref<int<4>,f,f,plain> v5 = 5;
				report_progress(1ull);                    // 1 from stmt above
			})");
	}

	TEST(IfStmt, FunctionCalls) {
		const auto input = R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;         // effort 1
				return p;                                 // effort 1 + 1 implicit deref
			};
			def c = (p : int<4>) -> bool {
				b(p);
				return true;                              // effort 1
			};
			{
				b(0);                                     // unrelated code
				var ref<int<4>,f,f,plain> v0 = 2;         // unrelated code
				if(*v0 == b(1)) {
					var ref<int<4>,f,f,plain> v4 = 4;
				} else {
					c(b(*v0));
				}
			})";

		TEST_PROGRESS(1, input, R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				report_progress(3ull);                     // 1 from stmt above, 1 from return below, 1 from implicit deref below
				return p;                                  // unreported effort: 0
			};
			def c = (p : int<4>) -> bool {
				b(p);
				report_progress(7ull);                     // 5 call overhead + 1 implicit deref, 1 from return below
				return true;                               // unreported effort: 0
			};
			{
				b(0);
				report_progress(5ull);                     // 5 call overhead
				var ref<int<4>,f,f,plain> v0 = 2;
				if(*v0 == b(1)) {
					report_progress(10ull);                  // 1 from stmt above if, 2 branch overhead, 1 from deref, 5 call overhead, 1 from builtin
					var ref<int<4>,f,f,plain> v4 = 4;
					report_progress(1ull);
				} else {
					report_progress(10ull);                  // 1 from stmt above if, 2 branch overhead, 1 from deref, 5 call overhead, 1 from builtin
					c(b(*v0));
					report_progress(11ull);                  // 5 call overhead * 2, 1 from deref
				}
			})");

		TEST_PROGRESS(15, input, R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				return p;                                  // unreported effort: 3 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			def c = (p : int<4>) -> bool {
				b(p);
				return true;                               // unreported effort: 10 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			{
				b(0);
				var ref<int<4>,f,f,plain> v0 = 2;
				if(*v0 == b(1)) {
					report_progress(21ull);                  // 9 from stmts above if, 2 branch overhead, 1 from deref, 3 unreported, 5 call overhead, 1 from builtin
					var ref<int<4>,f,f,plain> v4 = 4;
					report_progress(1ull);
				} else {
					report_progress(21ull);                  // 9 from stmts above if, 2 branch overhead, 1 from deref, 3 unreported, 5 call overhead, 1 from builtin
					c(b(*v0));
					report_progress(24ull);                  // 3 unreported, 10 unteported, 5 call overhead * 2, 1 from deref
				}
			})");

		TEST_PROGRESS(50, input, R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				return p;                                  // unreported effort: 3
			};
			def c = (p : int<4>) -> bool {
				b(p);
				return true;                               // unreported effort: 10
			};
			{
				b(0);
				var ref<int<4>,f,f,plain> v0 = 2;
				if(*v0 == b(1)) {
					var ref<int<4>,f,f,plain> v4 = 4;
					report_progress(22ull);                  // 9 from stmts above if, 1 from stmt above, 2 branch overhead, 1 from deref, 3 unreported, 5 call overhead, 1 from builtin
				} else {
					c(b(*v0));
					report_progress(45ull);                  // 9 from stmts above if, 2 branch overhead, 1 from deref, 3 unreported, 5 call overhead, 1 from builtin, 3 unreported, 10 unteported, 5 call overhead * 2, 1 from deref
				}
			})");
	}

	TEST(ExitPoints, Simple) {
		const auto input = R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;         // effort 1
				if(p == 0) {                              // effort 1 builtin + 1 implicit deref
					return 0;
				}
				return p;                                 // effort 1 + 1 implicit deref
			};
			def c = (p : int<4>) -> bool {
				b(p);
				return true;                              // effort 1
			};
			{
				var ref<int<4>,f,f,plain> v1 = 1;         // iterator variable
				while(c(*v1)) {
					b(*v1);
					if(*v1 == 1) {
						continue;
					}
					if(*v1 == 2) {
						break;
					}
					if(*v1 == 3) {
						return;
					}
					gen_post_inc(v1);
				}
			})";

		TEST_PROGRESS(50, input, R"(
			def b = (p : int<4>) -> int<4> {
				var ref<int<4>,f,f,plain> v1 = 0;
				if(p==0) {
					report_progress(6ull);                  // 1 from stmt above if, 2 branch overhead, 1 from implicit deref, 1 from builtin, 1 from return below
					return 0;                               // unreported effort: 0
				} else {
					report_progress(5ull);                  // 1 from stmt above if, 2 branch overhead, 1 from implicit deref, 1 from builtin
				}
				report_progress(2ull);                    // 1 from implicit deref + 1 from return below
				return p;                                 // unreported effort: 0
			};
			def c = (p : int<4>) -> bool {
				b(p);
				return true;                              // unreported effort: 7 (NOTE: originally there was a reporting call above, but that has been inlined)
			};
			{
				var ref<int<4>,f,f,plain> p = 1;
				report_progress(1ull);
				while(c(*p)) {
					b(*p);
					if(*p==1) {
						report_progress(26ull);               // 7 unreported, 5 call overhead, 1 from deref, 2 branch overhead, 5 call overhead, 1 from deref, 2 branch overhead, 1 from builtin, 1 from deref, 1 from continue below
						continue;
					} else {
						report_progress(25ull);               // 7 unreported, 5 call overhead, 1 from deref, 2 branch overhead, 5 call overhead, 1 from deref, 2 branch overhead, 1 from builtin, 1 from deref
					}
					if(*p==2) {
						report_progress(5ull);                // 2 branch overhead, 1 from builtin, 1 from deref, 1 from break below
						break;
					} else {
						report_progress(4ull);                // 2 branch overhead, 1 from builtin, 1 from deref
					}
					if(*p==3) {
						report_progress(5ull);                // 2 branch overhead, 1 from builtin, 1 from deref, 1 from return below
						return unit;
					} else {
						report_progress(4ull);                // 2 branch overhead, 1 from builtin, 1 from deref
					}
					gen_post_inc(p);
					report_progress(2ull);                  // 1 builtin, 1 implicit deref
				}
			})");
	}

} // end namespace transform
} // end namespace insieme
