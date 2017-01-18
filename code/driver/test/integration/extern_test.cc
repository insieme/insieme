/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
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
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
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
