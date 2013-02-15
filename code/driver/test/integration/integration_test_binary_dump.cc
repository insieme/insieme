/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/dump/text_dump.h"

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/test/integration_tests.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/timer.h"

#include "insieme/driver/driver_config.h"

#include "integration_tests.inc"

namespace insieme {

	// ---------------------------------- Check the binary dump -------------------------------------

	// the type definition (specifying the parameter type)
	class BinaryDumpIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };

	// define the test case pattern
	TEST_P(BinaryDumpIntegrationTest, WriteReadTest) {
		core::NodeManager managerA;

		// obtain test case
		utils::test::IntegrationTestCase testCase = GetParam();

		SCOPED_TRACE("Testing Case: " + testCase.getName());
		LOG(INFO) << "Testing Case: " + testCase.getName();
	
		// load the code using the frontend
		core::ProgramPtr code = load(managerA, testCase);

		// create a in-memory stream
		std::stringstream buffer(std::ios_base::out | std::ios_base::in | std::ios_base::binary);

		// dump IR using a binary format
		core::dump::binary::dumpIR(buffer, code);

		// reload IR using a different node manager
		core::NodeManager managerB;
		core::NodePtr restored = core::dump::binary::loadIR(buffer, managerB);

		EXPECT_NE(code, restored);
		EXPECT_EQ(*code, *restored);

		buffer.seekg(0); // reset stream

		core::NodePtr restored2 = core::dump::binary::loadIR(buffer, managerA);
		EXPECT_EQ(code, restored2);

	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(BinaryDumpIntegrationCheck, BinaryDumpIntegrationTest, ::testing::ValuesIn(getAllCases()));

}
