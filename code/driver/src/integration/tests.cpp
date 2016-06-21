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

#include "insieme/driver/integration/tests.h"

#include <iostream>
#include <vector>
#include <set>
#include <string>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/tokenizer.hpp>

#include <boost/optional.hpp>
#include <boost/algorithm/string.hpp>

#include "insieme/core/tu/ir_translation_unit.h"

#include "insieme/utils/config.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/string_utils.h"

#include "insieme/frontend/frontend.h"

#include "insieme/driver/cmd/insiemecc_options.h"
#include "insieme/driver/integration/test_step.h"

namespace insieme {
namespace driver {
namespace integration {

	namespace {

		frontend::ConversionJob toJob(const IntegrationTestCase& testCase) {
			driver::cmd::Options options = testCase.getOptions();
			for(auto file : testCase.getFiles()) {
				options.job.addFile(file.string());
			}

			std::string step = TEST_STEP_INSIEMECC_RUN_C_CONVERT;

			// add pre-processor definitions
			for_each(testCase.getDefinitions(step), [&](const std::pair<string, string>& def) { options.job.setDefinition(def.first, def.second); });

			// add include directories
			for_each(testCase.getIncludeDirs(), [&](const frontend::path& dir) { options.job.addIncludeDirectory(dir); });

			// add interceptor configuration
			options.job.setInterceptedHeaderDirs(testCase.getInterceptedHeaderFileDirectories());

			// done
			return options.job;
		}
	}


	driver::cmd::Options IntegrationTestCase::getOptions() const {
		// load code using frontend
		std::vector<std::string> args = {"compiler"};
		for(auto includeDir : getIncludeDirs()) {
			std::string include = "-I" + includeDir.string();
			args.push_back(include);
		}
		if(isEnableOpenMP()) { args.push_back("-fopenmp"); }
		if(isEnableOpenCL()) { args.push_back("-fopencl=1"); args.push_back("-lOpenCL"); }

		driver::cmd::Options options = driver::cmd::Options::parse(args);
		return options;
	}


	core::ProgramPtr IntegrationTestCase::load(core::NodeManager& manager) const {
		return toJob(*this).execute(manager);
	}

	core::tu::IRTranslationUnit IntegrationTestCase::loadTU(core::NodeManager& manager) const {
		return toJob(*this).toIRTranslationUnit(manager);
	}

	namespace fs = boost::filesystem;

	namespace {

		Properties loadProperties(const fs::path& dir, const string& configFileName = "config") {
			Properties res;

			// if it is the root we are done
			if(dir.empty()) { return res; }

			// the directory should be absolute
			assert_eq(dir, fs::absolute(dir)) << "Expecting an absolute directory - got " << dir << "\n";

			// load configuration of parent directory
			res = loadProperties(dir.parent_path(), configFileName);

			// check whether there is a config file
			auto file = dir / configFileName;

			if(fs::exists(file)) {
				// try loading file
				fs::ifstream in(file);
				if(in.is_open()) {
					// load local configuration
					auto p = Properties::load(in);
					res.set("CUR_CONFIG_PATH", dir.string());
					res <<= p;
				} else {
					LOG(WARNING) << "Unable to open test-configuration file " << file << "\n";
				}
			}

			// done
			return res;
		}


		boost::optional<IntegrationTestCase> loadTestCase(const std::string& testName) {
			static const boost::optional<IntegrationTestCase> fail;

			bool enableOpenMP = false;
			bool enableOpenCL = false;

			// get the test directory -- if the testName is a existingPath skip appending rootDir
			auto testDir = (fs::exists(fs::path(testName))) ? fs::path(testName) : fs::path(utils::getInsiemeSourceRootDir() + "../test") / testName;

			// check test case directory
			const fs::path testCaseDir = fs::canonical(fs::absolute(testDir));
			if(!fs::exists(testCaseDir)) {
				LOG(WARNING) << "Directory for test case " + testDir.string() + " not found!";
				return fail;
			}

			// assemble properties
			Properties prop;

			// define environment variables for the current test case
			prop.set("PATH", testCaseDir.string());

			// load global properties
			Properties global = loadProperties(fs::current_path(), "integration_test_config");

			// combine the various parts of the configuration (in the proper order)
			prop = prop << global << loadProperties(testCaseDir);

			// get files
			vector<frontend::path> files;

			for(const auto& file : prop.get<vector<string>>("files")) {
				if(fs::path(file).is_absolute()) {
					files.push_back(file);
				} else {
					files.push_back((testCaseDir / file).string());
				}
			}

			// no files specified, use default names
			if(files.size() == 0) {
				// extract the case name from the test directory
				string caseName = boost::filesystem::path(testCaseDir).filename().string();

				// add default file name
				if(fs::exists(testCaseDir / (caseName + ".c"))) {
					// This is a C test case
					files.push_back((testCaseDir / (caseName + ".c")).string());

					// this is a c++ test case
				} else if (fs::exists(testCaseDir / (caseName + ".cpp"))) {
					files.push_back((testCaseDir / (caseName + ".cpp")).string());

					//otherwise we don't know how to handle this test case
				} else {
					LOG(WARNING) << "Directory " << testCaseDir << " doesn't contain a matching .c or .cpp file - Skipping";
					return fail;
				}
			}

			// get includes
			vector<frontend::path> includeDirs;

			for(const auto& path : prop.get<vector<string>>("includes")) {
				if(path.at(0) == '/') {
					includeDirs.push_back(path);
				} else {
					includeDirs.push_back((testCaseDir / path).string());
				}
			}

			// get libs paths
			vector<frontend::path> libPaths;

			for(const auto& path : prop.get<vector<string>>("libPaths")) {
				if(path.at(0) == '/') {
					libPaths.push_back(path);
				} else {
					libPaths.push_back((testCaseDir / path).string());
				}
			}

			// get lib names
			vector<std::string> libNames;

			for(const auto& name : prop.get<vector<string>>("libNames")) {
				libNames.push_back(name);
			}

			enableOpenMP = prop.get<bool>("use_omp");
			enableOpenCL = prop.get<bool>("use_opencl");

			// extract interception configuration
			vector<frontend::path> interceptedHeaderFileDirectories;

			for(const auto& path : prop.get<vector<string>>("intercepted_header_file_dirs")) {
				if(path.at(0) == '/') {
					interceptedHeaderFileDirectories.push_back(path);
				} else {
					interceptedHeaderFileDirectories.push_back((testCaseDir / path).string());
				}
			}

			// find "canonical" test name regardless of setup
			string canonRoot = fs::canonical(fs::path(utils::getInsiemeSourceRootDir() + "../test")).string();
			string prefix = commonPrefix(testCaseDir.string(), canonRoot);
			string name = testCaseDir.string().substr(prefix.size());
			// don't just replace "test/" here, as unintended replacements might occur deeper in the directory structure
			boost::algorithm::replace_first(name, "ext/test/", "ext/");
			boost::algorithm::replace_first(name, "base/test/", "base/");
			if(boost::algorithm::starts_with(name, "/")) { name = name.substr(1); }

			// add test case
			return IntegrationTestCase(name, testCaseDir, files, includeDirs, libPaths, libNames,
			                           interceptedHeaderFileDirectories, enableOpenMP, enableOpenCL, prop);
		}


		vector<IntegrationTestCase> loadAllCases(const std::string& testDirStr, const LoadTestCaseMode loadMode = ENABLED_TESTS) {
			// create a new result vector
			vector<IntegrationTestCase> res;

			// obtain access to the test directory
			const fs::path testDir(testDirStr);

			// check whether the directory is correct
			if(!fs::exists(testDir) || !fs::is_directory(testDir)) {
				LOG(WARNING) << "Test-Directory path not properly set!";
				return res;
			}

			// read the blacklisted_tests file
			const fs::path blacklistedTestsPath = testDir / "blacklisted_tests";
			if(!fs::exists(blacklistedTestsPath)) {
				LOG(WARNING) << "No blacklisted_tests file found!";
				return res;
			}

			// get all blacklisted test cases
			fs::ifstream blacklistedTestsFile;
			set<string> blacklistedTestCases;
			set<string> longTestCases;
			blacklistedTestsFile.open(blacklistedTestsPath);
			if(!blacklistedTestsFile.is_open()) {
				LOG(WARNING) << "Unable to open blacklisted_tests file!";
				return res;
			}

			string testCase;
			while(getline(blacklistedTestsFile, testCase)) {
				//strip comments
				if (testCase.find('#') != string::npos) {
					testCase = testCase.substr(0, testCase.find("#"));
				}

				//and trim
				testCase = boost::algorithm::trim_copy(testCase);

				if(!testCase.empty()) {
					//strip leading '+' for long test cases and store this as a flag
					bool isLongTest = false;
					if (testCase[0] == '+') {
						testCase = testCase.substr(1);
						isLongTest = true;
					}

					if(!fs::is_directory(testDir / testCase)) {
						LOG(WARNING) << "Blacklisted test case \"" << testCase << "\" does not exist.";

						//insert all existing directory names
					} else {
						//if it is a long running test case
						if (isLongTest) {
							longTestCases.insert(testCase);

							// otherwise it is a blacklisted one
						} else {
							blacklistedTestCases.insert(testCase);
						}
					}
				}
			}
			blacklistedTestsFile.close();

			// Add all sub directories to the list of test cases to load - consider blacklisted test cases
			for(fs::directory_iterator it(testDir); it != fs::directory_iterator(); ++it) {
				const fs::path testCaseDir = *it;
				if(fs::is_directory(testCaseDir)) {
					string testCaseName = testCaseDir.filename().string();
					bool testIsBlacklisted = blacklistedTestCases.find(testCaseName) != blacklistedTestCases.end();
					bool testIsLong = longTestCases.find(testCaseName) != longTestCases.end();

					//if this test is blacklisted and we should not run them now, we don't add it
					if (testIsBlacklisted && loadMode != BLACKLISTED_TESTS && loadMode != ALL_TESTS) {
						continue;
					}
					//if this test is a long one and we should not run them now
					if (testIsLong && loadMode != LONG_TESTS && loadMode != ENABLED_AND_LONG_TESTS && loadMode != ALL_TESTS) {
						continue;
					}

					// check whether it is a test suite
					if(fs::exists(testCaseDir / "blacklisted_tests")) {
						LOG(DEBUG) << "Descending into sub-test-directory " << (testCaseDir).string();

						//if the test suite is blacklisted and we should run blacklisted tests, we descend and schedule all tests
						auto childLoadMode = (testIsBlacklisted && loadMode == BLACKLISTED_TESTS) ? ALL_TESTS : loadMode;
						vector<IntegrationTestCase>&& subCases = loadAllCases((testCaseDir).string(), childLoadMode);
						std::copy(subCases.begin(), subCases.end(), std::back_inserter(res));
						continue;
					}

					//load the current test case according to the load mode we are supposed to apply
					if (loadMode == ALL_TESTS
							|| (loadMode == BLACKLISTED_TESTS && testIsBlacklisted)
							|| (loadMode == ENABLED_AND_LONG_TESTS && !testIsBlacklisted)
							|| (loadMode == LONG_TESTS && testIsLong)
							|| (loadMode == ENABLED_TESTS && !testIsBlacklisted && !testIsLong)) {
						// load individual test case
						auto testCase = loadTestCase(fs::canonical(fs::absolute(testDir / testCaseName)).string());
						if(testCase) { res.push_back(*testCase); }
					}
				}
			}

			return res;
		}
	}

	// a global variable containing the list of test cases after they have been loaded the first time
	boost::optional<vector<IntegrationTestCase>> TEST_CASES = boost::none;

	const vector<IntegrationTestCase>& getAllCases(const LoadTestCaseMode loadTestCaseMode) {
		// check whether cases have been loaded before
		if(!TEST_CASES) {
			TEST_CASES = boost::optional<vector<IntegrationTestCase>>(loadAllCases(utils::getInsiemeSourceRootDir() + "../test", loadTestCaseMode));
			std::sort(TEST_CASES->begin(), TEST_CASES->end());
		}
		return *TEST_CASES;
	}

	const IntegrationTestCaseOpt getCase(const string& name) {

		// in case all test case have already been loaded ...
		if (TEST_CASES) {
			// load list of test cases
			const vector<IntegrationTestCase>& cases = getAllCases();

			// search for case with given name
			for(auto it = cases.begin(); it != cases.end(); ++it) {
				// for base-only setups
				if(it->getName() == name) {
					return *it;
					// for ext + base setups, check base and ext but prefer base
				} else if(it->getName() == "base/" + name) {
					return *it;
				} else if(it->getName() == "ext/" + name) {
					return *it;
				}
			}
		}

		// try loading test case directly (e.g if blacklisted)
		return loadTestCase(name);
	}

	vector<IntegrationTestCase> getTestSuite(const string& path) {
		// create a dummy error code object to ignore if the path can't be resolved
		boost::system::error_code errorCode;
		// convert the path into an absolute path
		frontend::path absolute_path = fs::canonical(fs::absolute(path), errorCode);

		// if the given path doesn't exist, we try to interpret the argument as relative to the base test directory
		if (!fs::exists(absolute_path)) {
			absolute_path = fs::canonical(fs::absolute(utils::getInsiemeSourceRootDir() + "../test/" + path));
		}

		// first check if it's an individual test case
		const fs::path blacklistFile = absolute_path / "blacklisted_tests";
		if(!fs::exists(blacklistFile)) {
			// individual test cases have no "blacklisted_tests" file in their folder
			auto testCase = loadTestCase(path);
			if(testCase) { return toVector(*testCase); }
		}

		return loadAllCases(absolute_path.string());
	}


	core::ProgramPtr loadIntegrationTest(core::NodeManager& manager, const std::string& name, bool enableOpenMP, const std::map<string, string>& definitions) {
		// load case information
		auto curCase = getCase(name);
		if(!curCase) { return core::ProgramPtr(); }

		// load code using frontend - using given options
		std::vector<std::string> args = {"compiler"};
		for(auto file : curCase->getFiles()) {
			args.push_back(file.string());
		}
		for(auto incl : curCase->getIncludeDirs()) {
			std::string inc = "-I" + incl.string();
			args.push_back(inc);
		}
		if(enableOpenMP) { args.push_back("-fopenmp"); }
		insieme::driver::cmd::Options options = insieme::driver::cmd::Options::parse(args);

		// add pre-processor definitions
		for(const auto& cur : definitions) {
			options.job.setDefinition(cur.first, cur.second);
		}

		return options.job.execute(manager);
	}


} // end namespace integration
} // end namespace driver
} // end namespace insieme
