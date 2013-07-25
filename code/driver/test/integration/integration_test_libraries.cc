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

#include <boost/filesystem/operations.hpp>

#include "insieme/frontend/frontend.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/dump/text_dump.h"

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/timer.h"

#include "insieme/driver/driver_config.h"
#include "insieme/driver/integration/tests.h"

#include "insieme/driver/object_file_utils.h"

namespace insieme {

	using namespace driver;
	using namespace driver::integration;
	using namespace boost::filesystem;

	// ---------------------------------- Check the binary dump -------------------------------------

	// the type definition (specifying the parameter type)
	class LibraryDumpIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };

	// define the test case pattern
	TEST_P(LibraryDumpIntegrationTest, WriteReadTest) {
		core::NodeManager managerA;

		// obtain test case
		IntegrationTestCase testCase = GetParam();

		SCOPED_TRACE("Testing Case: " + testCase.getName());
		LOG(INFO) << "Testing Case: " + testCase.getName();
	
		// load the code using the frontend
		auto unit = testCase.loadTU(managerA);

		// save tu to temporary file
		auto file = unique_path(temp_directory_path() / "tmp%%%%%%%%.o");

		// save translation unit
		saveLib(unit, file);

		// check validity
		EXPECT_TRUE(exists(file));
		EXPECT_TRUE(isInsiemeLib(file));

		// reload translation unit
		auto tu = loadLib(managerA, file);

		EXPECT_EQ(unit, tu);

		// cleanup
		if (exists(file)) {
			remove(file);
		}
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(LibraryDumpIntegrationTestCheck, LibraryDumpIntegrationTest, ::testing::ValuesIn(getAllCases()));

}
