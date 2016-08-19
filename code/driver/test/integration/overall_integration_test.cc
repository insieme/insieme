/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
#include <iostream>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_step.h"
#include "insieme/driver/object_file_utils.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/dump/text_dump.h"
#include "insieme/core/types/type_variable_deduction.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/backend/runtime/runtime_backend.h"


using namespace insieme::utils;

namespace insieme {
namespace driver {
namespace integration {

	using namespace driver::integration;
	using namespace boost::filesystem;

	// the type definition (specifying the parameter type)
	class IntegrationTests : public ::testing::TestWithParam<IntegrationTestCase> {};

	// define the test case pattern
	TEST_P(IntegrationTests, OverallTest) {
		core::NodeManager manager;

		// obtain test case
		driver::integration::IntegrationTestCase testCase = GetParam();
		SCOPED_TRACE("Testing Case: " + testCase.getName());
		LOG(INFO) << "Testing Case: " + testCase.getName();

		core::ProgramPtr code = testCase.load(manager);

		//TEST THE FRONTEND
		{
			// load the code using the frontend
			EXPECT_TRUE(code) << "frontend failure in test case " << testCase.getName();

			// run semantic checks on loaded program
			auto errors = core::checks::check(code).getErrors();

			EXPECT_EQ(static_cast<std::size_t>(0), errors.size()) << "found semantic errors in test case " << testCase.getName();
			if (!errors.empty()) {
				for_each(errors, [](const core::checks::Message& cur) {
					LOG(INFO) << cur;
					core::NodeAddress address = cur.getOrigin();
					std::stringstream ss;
					unsigned contextSize = 1;
					do {
						ss.str("");
						ss.clear();

						unsigned up = contextSize;
						if (contextSize > address.getDepth()) { up = address.getDepth(); }
						core::NodePtr context = address.getParentNode(up);
						ss << insieme::core::printer::PrettyPrinter(context, insieme::core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE, 1 + 2 * contextSize);
					} while (ss.str().length() < 50 && contextSize++ < 5);
					LOG(INFO) << "\t Context: " << ss.str() << std::endl;
				});
				// assert_fail();
			}
		}

		//TEST BINARY DUMP
		{
			// create a in-memory stream
			std::stringstream buffer(std::ios_base::out | std::ios_base::in | std::ios_base::binary);

			// dump IR using a binary format
			core::dump::binary::dumpIR(buffer, code);

			// reload IR using a different node manager
			core::NodeManager managerB;
			core::NodePtr restored = core::dump::binary::loadIR(buffer, managerB);

			EXPECT_NE(code, restored) << "binary load/dump failed. test case: " << testCase.getName();
			EXPECT_EQ(*code, *restored) << "binary load/dump failed. test case: " << testCase.getName();

			buffer.seekg(0); // reset stream

			core::NodePtr restored2 = core::dump::binary::loadIR(buffer, manager);
			EXPECT_EQ(code, restored2);
		}

		//TEST TYPE DEDUCTION
		{
			// and now, apply the check and see whether a solution could be found
			core::visitDepthFirstOnce(code, [&](const core::CallExprPtr& call) {
				EXPECT_TRUE(core::types::getTypeVariableInstantiation(manager, call))
					<< "Type deduction failed for test case: " << testCase.getName() << "\n"
					<< "FunctionType:   " << *(call->getFunctionExpr()->getType()) << "\n"
					<< "Argument Types: " << extractTypes(core::transform::extractArgExprsFromCall(call));
			});
		}

		// skip OpenCL tests as those are handeled via driver/integration_tests
		if (testCase.isEnableOpenCL() && !utils::compiler::isOpenCLAvailable()) {
			LOG(INFO) << "Skipping OpenCL test: " + testCase.getName();
			return;
		}

		// each test might require some prerequisites which are checked here
		if (!driver::integration::checkPrerequisites(testCase)) {
			ASSERT_TRUE(false) << "Prerequisites for test case " << testCase.getName() << " are not satisfied";
			return;
		}

		//TEST RUNTIME BACKEND
		{
			// create target code using the runtime backend
			auto target = backend::runtime::RuntimeBackend::getDefault()->convert(code);

			// see whether target code can be compiled
			utils::compiler::Compiler compiler = utils::compiler::Compiler::getRuntimeCompiler();

			std::string step = TEST_STEP_INSIEMECC_RUN_C_COMPILE;
			// switch to C++ compiler if necessary
			if (any(testCase.getFiles(), [](const frontend::path& cur) { return *cur.string().rbegin() == 'p'; })) {
				compiler = utils::compiler::Compiler::getRuntimeCompiler(utils::compiler::Compiler::getDefaultCppCompiler());
				step = TEST_STEP_INSIEMECC_RUN_CPP_COMPILE;
			}

			// add extra compiler flags from test case
			for (const auto& flag : testCase.getCompilerArguments(step, true, step == TEST_STEP_INSIEMECC_RUN_CPP_COMPILE)) {
				compiler.addFlag(flag);
			}

			// add includes
			for (const auto& cur : testCase.getIncludeDirs()) {
				compiler.addIncludeDir(cur.string());
			}

			// add library directories
			for (const auto& cur : testCase.getLibDirs()) {
				compiler.addFlag("-L" + cur.string());
			}

			// add libraries
			for (const auto& cur : testCase.getLibNames()) {
				compiler.addFlag("-l" + cur);
			}

			EXPECT_TRUE(utils::compiler::compile(*target, compiler)) << "runtime backend test failed for test case: " << testCase.getName();
		}

		//LIBRARIES TEST
		{
			core::NodeManager tmpManager;
			core::tu::IRTranslationUnit codeTU = testCase.loadTU(tmpManager);

			// save tu to temporary file
			auto file = unique_path(temp_directory_path() / "tmp%%%%%%%%.o");

			// save translation unit
			insieme::driver::saveLib(codeTU, file);

			// check validity
			EXPECT_TRUE(exists(file)) << "save library test failed for test case: " << testCase.getName();
			EXPECT_TRUE(insieme::driver::isInsiemeLib(file)) << "save library test failed for test case: " << testCase.getName();

			// reload translation unit
			auto tu = insieme::driver::loadLib(tmpManager, file);

			EXPECT_EQ(codeTU, tu) << "load library test failed for test case: " << testCase.getName();;

			// cleanup
			if (exists(file)) { remove(file); }
		}

	}

	INSTANTIATE_TEST_CASE_P(OverallTest, IntegrationTests, ::testing::ValuesIn(getAllCases()));

} // end namespace integration
} // end namespace driver
} // end namespace insieme
