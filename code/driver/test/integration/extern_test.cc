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
#include <sstream>

#include "insieme/frontend/frontend.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/annotations/c/extern.h"
#include "insieme/backend/sequential/sequential_backend.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/config.h"

#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_step.h"

namespace insieme {

	using namespace driver::integration;

	TEST(ExternTest, PreserveStorageClass) {
		core::NodeManager manager;

		// obtain test case & check that it's available
		auto testCaseOpt = getCase("seq/c/extern_global");
		ASSERT_FALSE(!testCaseOpt);
		driver::integration::IntegrationTestCase testCase = *testCaseOpt;

		// load the code using the frontend
		core::ProgramPtr code = testCase.load(manager);
		ASSERT_TRUE(code);

		// check that extern_global is annotated as extern
		bool externAnnotation = false;
		core::visitDepthFirstOnce(code, [&](const core::LiteralPtr& lit) {
			if(lit->getValue()->getValue() == "extern_global") {
				externAnnotation = annotations::c::isExtern(lit);
			}
		});
		EXPECT_TRUE(externAnnotation) << "ExternTag missing on global literal generated for extern global";

		// compile to C and check for extern declaration
		auto target = toString(*backend::sequential::SequentialBackend::getDefault()->convert(code));
		
		EXPECT_NE(target.find("extern int32_t extern_global"), string::npos) << "Extern storage class specifier not found in output code:\n" << target;
	}
	
	TEST(ExternTest, MultiTUOverrideStorageClass) {
		core::NodeManager manager;

		// obtain test case & check that it's available
		auto testCaseOpt = getCase("seq/c/ext_multi");
		ASSERT_FALSE(!testCaseOpt);
		driver::integration::IntegrationTestCase testCase = *testCaseOpt;

		// load the code using the frontend
		core::ProgramPtr code = testCase.load(manager);
		ASSERT_TRUE(code);

		// check that flag_a and flag_b are not annotated as extern
		bool externAnnotation = true;
		core::visitDepthFirstOnce(code, [&](const core::LiteralPtr& lit) {
			if(lit->getValue()->getValue() == "flag_a" || lit->getValue()->getValue() == "flag_b") {
				externAnnotation = annotations::c::isExtern(lit);
			}
		});
		EXPECT_FALSE(externAnnotation) << "ExternTag on global which is actually defined";

		// compile to C and check for absence of extern declaration
		auto target = toString(*backend::sequential::SequentialBackend::getDefault()->convert(code));
		
		EXPECT_EQ(target.find("extern"), string::npos) << "Extern storage class specifier found in output code:\n" << target;
	}
}
