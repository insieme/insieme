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

} // end namespace transform
} // end namespace insieme
