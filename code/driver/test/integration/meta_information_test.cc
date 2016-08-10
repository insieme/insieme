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
#include <sstream>

#include "insieme/frontend/frontend.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/annotations/meta_info/meta_infos.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/timer.h"

#include "insieme/utils/config.h"
#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_step.h"

// Execute & get result string
#include <stdio.h>
std::string exec(const char* cmd) {
	FILE* pipe = popen(cmd, "r");
	if(!pipe) { return "ERROR"; }
	char buffer[128];
	std::string result = "";
	while(!feof(pipe)) {
		if(fgets(buffer, 128, pipe) != NULL) { result += buffer; }
	}
	pclose(pipe);
	return result;
}

namespace insieme {

	using namespace driver::integration;

	// ---------------------------------- Check forwarding of meta-information to the runtime -------------------------------------

	TEST(MetaInformationTest, Integration) {
		core::NodeManager manager;
		auto& parExt = manager.getLangExtension<core::lang::ParallelExtension>();

		// obtain test case & check that it's available
		driver::integration::IntegrationTestCaseOpt testCaseOpt = getCase("omp/meta_info_test");
		EXPECT_FALSE(!testCaseOpt);
		driver::integration::IntegrationTestCase testCase = *testCaseOpt;

		// load the code using the frontend
		core::ProgramPtr code = testCase.load(manager);

		dumpPretty(code);

		// find parallel
		core::CallExprPtr parallel;
		core::visitDepthFirstOnceInterruptible(code, [&](const core::CallExprPtr& call) {
			if(parExt.isCallOfParallel(call)) {
				parallel = call;
				return true;
			}
			return false;
		});
		EXPECT_NE(parallel, core::CallExprPtr());

		// add meta information
		annotations::effort_estimation_info eff;
		eff.fallback_estimate = 42;
		parallel->attachValue(eff);

		// create target code using the runtime backend
		auto target = backend::runtime::RuntimeBackend::getDefault()->convert(code);

		// see whether target code can be compiled
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getRuntimeCompiler();

		// add extra compiler flags from test case
		for(const auto& flag : testCase.getCompilerArguments(TEST_STEP_INSIEMECC_RUN_C_COMPILE, false)) {
			compiler.addFlag(flag);
		}

		// add includes
		for(const auto& cur : testCase.getIncludeDirs()) {
			compiler.addIncludeDir(cur.string());
		}

		// add library directories
		for(const auto& cur : testCase.getLibDirs()) {
			compiler.addFlag("-L" + cur.string());
		}

		// add libraries
		for(const auto& cur : testCase.getLibNames()) {
			compiler.addFlag("-l" + cur);
		}

		// build binary
		auto fn = utils::compiler::compileToBinary(*target, compiler);
		EXPECT_FALSE(fn.empty());

		// execute
		string command = string("IRT_REPORT=1 ") + fn;
		auto res = exec(command.c_str());

		// search for our meta information
		EXPECT_NE(res.find("fallback_estimate = 42"), res.npos);

		// delete binary
		if(boost::filesystem::exists(fn)) { boost::filesystem::remove(fn); }
	}

	TEST(MetaInformationTest, Migrate) {
		core::NodeManager manager;

		// obtain test case & check that it's available
		driver::integration::IntegrationTestCaseOpt testCaseOpt = getCase("omp/meta_info_test");
		EXPECT_FALSE(!testCaseOpt);
		driver::integration::IntegrationTestCase testCase = *testCaseOpt;

		// load the code using the frontend
		core::ProgramPtr code = testCase.load(manager);

		dumpPretty(code);

		// find parallel
		core::ExpressionPtr postinc;
		core::visitDepthFirstOnceInterruptible(code, [&](const core::ExpressionPtr& expr) {
			if(expr == manager.getLangExtension<core::lang::ReferenceExtension>().getGenPostInc()) {
				postinc = expr;
				return true;
			}
			return false;
		});
		EXPECT_NE(postinc, core::ExpressionPtr());

		// add meta information
		annotations::effort_estimation_info eff;
		eff.fallback_estimate = 42;
		postinc->attachValue(eff);

		// create target code using the runtime backend
		auto target = backend::runtime::RuntimeBackend::getDefault()->convert(code);

		// see whether target code can be compiled
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getRuntimeCompiler();

		// add extra compiler flags from test case
		for(const auto& flag : testCase.getCompilerArguments(TEST_STEP_INSIEMECC_RUN_C_COMPILE, false)) {
			compiler.addFlag(flag);
		}

		// add includes
		for(const auto& cur : testCase.getIncludeDirs()) {
			compiler.addIncludeDir(cur.string());
		}

		// add library directories
		for(const auto& cur : testCase.getLibDirs()) {
			compiler.addFlag("-L" + cur.string());
		}

		// add libraries
		for(const auto& cur : testCase.getLibNames()) {
			compiler.addFlag("-l" + cur);
		}

		// build binary
		auto fn = utils::compiler::compileToBinary(*target, compiler);
		EXPECT_FALSE(fn.empty());

		// execute
		string command = string("IRT_REPORT=1 ") + fn;
		auto res = exec(command.c_str());

		std::cout << res << "\n";

		// search for our meta information
		EXPECT_NE(res.find("fallback_estimate = 42"), res.npos);

		// delete binary
		if(boost::filesystem::exists(fn)) { boost::filesystem::remove(fn); }
	}
}
