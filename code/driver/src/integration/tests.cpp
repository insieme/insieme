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

#include "insieme/driver/integration/tests.h"

#include <iostream>
#include <vector>
#include <set>
#include <string>
#include <regex>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/tokenizer.hpp>

#include <boost/optional.hpp>
#include <boost/algorithm/string.hpp>

#include "insieme/core/tu/ir_translation_unit.h"

#include "insieme/common/env_vars.h"

#include "insieme/utils/config.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/frontend/frontend.h"

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/integration/test_step.h"

namespace insieme {
namespace driver {
namespace integration {

	namespace fs = boost::filesystem;

	namespace {

		boost::optional<Properties> loadProperties(const fs::path& configFile) {
			// the directory should be absolute
			assert_eq(configFile, fs::absolute(configFile)) << "Expecting an absolute path - got " << configFile << "\n";

			if(fs::exists(configFile)) {
				// try loading file
				fs::ifstream in(configFile);
				if(in.is_open()) {
					return Properties::load(in);
				} else {
					LOG(WARNING) << "Unable to open test-configuration file " << configFile << "\n";
				}
			}
			return {};
		}

		Properties getGlobalConfiguration(const IntegrationTestPaths testPaths) {
			// first we look in the current working directory and all directories above
			fs::path dir = fs::absolute(fs::current_path());
			do {
				fs::path file = dir / testPaths.globalConfigFileName;
				if(fs::exists(file)) {
					return *loadProperties(file);
				}
				dir = dir.parent_path();
			}	while (!dir.empty());

			// after that we try to look in the build folder
			dir = fs::path(testPaths.buildDir);
			fs::path file = dir / testPaths.globalConfigFileName;
			if(fs::exists(file)) {
				return *loadProperties(file);
			}

			// otherwise we assert here. We need to have a global config file
			assert_fail() << "Could not find the global configuration file \"" << testPaths.globalConfigFileName
					<< "\" anywhere in the current working directory, any of it's parents or the build folder " << testPaths.buildDir;
			return {};
		}

		// a simple cache for configurations to speed up the lookup
		std::map<fs::path, Properties> CONFIG_CACHE;

		Properties getConfiguration(const fs::path& dir) {
			// if it is the root we are done
			if(dir.empty()) { return {}; }

			// the directory should be absolute
			assert_eq(dir, fs::absolute(dir)) << "Expecting an absolute directory - got " << dir << "\n";

			// perform cache lookup
			auto it = CONFIG_CACHE.find(dir);
			if(it != CONFIG_CACHE.end()) {
				return it->second;
			}

			Properties res;

			// load configuration of parent directory
			res = getConfiguration(dir.parent_path());

			// check whether there is a config file
			auto file = dir / "config";

			// load the config file
			auto currentConfig = loadProperties(file);
			if(currentConfig) {
				// and merge the config with it's parent
				res.set("CUR_CONFIG_PATH", dir.string());
				res <<= (*currentConfig);
			}

			// cache result
			CONFIG_CACHE.insert({ dir, res });

			// done
			return res;
		}

		// a simple cache for compiler configurations to speed up the lookup
		std::map<std::string, Properties> COMPILER_CONFIG_CACHE;

		Properties getCompilerArgumentsFromConfiguration(const string& compilerString, const Properties& properties) {
			// get the filename from the compilerString
			auto compilerName = fs::path(compilerString).filename().string();

			// perform cache lookup
			auto it = COMPILER_CONFIG_CACHE.find(compilerName);
			if(it != COMPILER_CONFIG_CACHE.end()) {
				return it->second;
			}

			// get the filename of the compiler configuration
			auto configurationFilename = properties.get("compilerConfigurationFile", compilerName);
			if(configurationFilename.empty()) {
				assert_fail() << "Could not find property \"compilerConfigurationFile[" << compilerName << "]\" anywhere in configuration.";
			}

			auto loadedProperties = loadProperties(fs::path(configurationFilename));
			if(!loadedProperties) {
				assert_fail() << "Unable to load compiler configuration file " << configurationFilename;
			}

			Properties res = *loadedProperties;

			// cache result
			COMPILER_CONFIG_CACHE.insert({ compilerName, res });

			return res;
		}

		fs::path canonicalizeTestName(const fs::path& absTestDir, const IntegrationTestPaths testPaths) {
			const fs::path absTestRoot = testPaths.testsRootDir;

			const fs::path cnTestDir  = fs::canonical(absTestDir).remove_trailing_separator();
			const fs::path cnTestRoot = fs::canonical(absTestRoot).remove_trailing_separator();

			auto  a  = cnTestRoot.begin(), b  = cnTestDir.begin();
			while(a != cnTestRoot.end() && b != cnTestDir.end()) {
				if(*a == *b) {
					a++;
					b++;
				} else {
					break;
				}
			}

			if(a == cnTestRoot.end()) {
				fs::path p;

				while(b != cnTestDir.end()) {
					p = p / *b;
					b++;
				}
				return p;
			} else {
				return absTestDir;
			}
		}

		fs::path absolutizeTestDir(const fs::path& testDir, const IntegrationTestPaths testPaths) {
			fs::path absTestDir;
			if(testDir.is_relative() && fs::exists(testDir)) {
				absTestDir = fs::current_path() / testDir;
			} else if(testDir.is_relative()) {
				absTestDir = fs::path(testPaths.testsRootDir) / testDir;
			} else { // testDir is absolute
				absTestDir = testDir;
			}

			return absTestDir;
		}

		boost::optional<IntegrationTestCase> loadSingleTestCase(const fs::path& testDir, const IntegrationTestPaths testPaths) {
			const fs::path absTestSourceDir = absolutizeTestDir(testDir, testPaths);

			if(!fs::exists(absTestSourceDir)) {
				LOG(WARNING) << "Directory for test case " + absTestSourceDir.string() + " not found!";
				return {};
			}

			const fs::path cnTestSourceDir = canonicalizeTestName(absTestSourceDir, testPaths);

			const fs::path absTestOutputDir = testPaths.outputDir == testPaths.testsRootDir
				? absTestSourceDir
				: testPaths.outputDir / cnTestSourceDir;

			// assemble properties
			Properties prop;

			// define environment variables for the current test case
			prop.set("PATH", absTestSourceDir.string());
			prop.set("TEST_SOURCE_DIR", absTestSourceDir.string());
			prop.set("TEST_OUTPUT_DIR", absTestOutputDir.string());
			prop.set("TESTS_ROOT_DIR", testPaths.testsRootDir.string());
			prop.set("BUILD_DIR", testPaths.buildDir.string());

			// load global properties
			Properties global = getGlobalConfiguration(testPaths);

			// combine the various parts of the configuration (in the proper order)
			prop = prop << global << getConfiguration(absTestSourceDir);

			// get files
			vector<fs::path> files;

			auto addPath = [&absTestSourceDir](std::vector<fs::path>& paths, const fs::path& dir, const fs::path& path) {
				if(path.is_absolute())
					paths.push_back(path);
				else
					paths.push_back(dir / path);
			};

			// use the files specified in the configuration file, if present
			for(const auto& file : prop.get<vector<string>>("files")) {
				addPath(files, absTestSourceDir, fs::path(file));
			}

			// no files specified, use default names
			if(files.size() == 0) {
				// extract the case name from the test directory
				fs::path absTestSourceDir_ = absTestSourceDir;
				absTestSourceDir_.remove_trailing_separator();
				string caseName = absTestSourceDir_.filename().string();

				// add default file name
				if(fs::exists(absTestSourceDir / (caseName + ".c"))) {
					// This is a C test case
					files.push_back((absTestSourceDir / (caseName + ".c")).string());
				} else if (fs::exists(absTestSourceDir / (caseName + ".cpp"))) {
					// this is a c++ test case
					files.push_back((absTestSourceDir / (caseName + ".cpp")).string());
				} else {
					//otherwise we don't know how to handle this test case
					LOG(WARNING) << "Directory " << absTestSourceDir << " doesn't contain a matching .c or .cpp file - Skipping";
					return {};
				}
			}

			// get includes
			vector<fs::path> includeDirs;
			for(const auto& path : prop.get<vector<string>>("includes")) {
				addPath(includeDirs, absTestSourceDir, fs::path(path));
			}

			// get libs paths
			vector<fs::path> libPaths;
			for(const auto& path : prop.get<vector<string>>("libPaths")) {
				addPath(libPaths, absTestOutputDir, fs::path(path));
			}

			// get lib names
			vector<std::string> libNames(prop.get<vector<string>>("libNames"));

			bool enableOpenMP = prop.get<bool>("use_omp");
			bool enableOpenCL = prop.get<bool>("use_opencl");

			// extract interception configuration
			vector<fs::path> interceptedHeaderFileDirectories;
			for(const auto& path : prop.get<vector<string>>("intercepted_header_file_dirs")) {
				addPath(interceptedHeaderFileDirectories, absTestSourceDir, fs::path(path));
			}

			// add test case
			return IntegrationTestCase(cnTestSourceDir,
			                           absTestSourceDir,
			                           absTestOutputDir,
			                           files,
			                           includeDirs,
			                           libPaths,
			                           libNames,
			                           interceptedHeaderFileDirectories,
			                           enableOpenMP,
			                           enableOpenCL,
			                           prop);
		}

		vector<IntegrationTestCase> loadAllCasesInDirectory(const fs::path& absTestDir, const IntegrationTestPaths testPaths,
		                                                    const LoadTestCaseMode loadMode = ENABLED_TESTS) {
			// create a new result vector
			vector<IntegrationTestCase> res;

			// check whether the directory is correct
			if(!fs::exists(absTestDir) || !fs::is_directory(absTestDir)) {
				LOG(WARNING) << "Test-Directory '"+ absTestDir.string() +"' does not exist or is not a directory!";
				return res;
			}

			// read the blacklisted_tests file
			const fs::path blacklistedTestsPath = absTestDir / "blacklisted_tests";
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
				LOG(WARNING) << "Unable to open file: "+ blacklistedTestsPath.string() +"!";
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

					if(!fs::is_directory(absTestDir / testCase)) {
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
			for(fs::directory_iterator it(absTestDir); it != fs::directory_iterator(); ++it) {
				const fs::path absTestCaseDir = *it;
				if(!fs::is_directory(absTestCaseDir))
					continue;

				fs::path absTestCaseDir_ = absTestCaseDir;
				absTestCaseDir_.remove_trailing_separator();

				string testCaseName = absTestCaseDir_.filename().string();
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
				if(fs::exists(absTestCaseDir / "blacklisted_tests")) {
					LOG(DEBUG) << "Descending into sub-test-directory " << absTestCaseDir.string();

					//if the test suite is blacklisted and we should run blacklisted tests, we descend and schedule all tests
					auto childLoadMode = (testIsBlacklisted && loadMode == BLACKLISTED_TESTS) ? ALL_TESTS : loadMode;
					vector<IntegrationTestCase>&& subCases =
						loadAllCasesInDirectory(absTestCaseDir, testPaths, childLoadMode);
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
					auto testCase = loadSingleTestCase(absTestCaseDir, testPaths);
					if(testCase)
						res.push_back(*testCase);
				}
			}

			return res;
		}

		vector<IntegrationTestCase> getAllTestCasesInternal(const LoadTestCaseMode loadTestCaseMode, const IntegrationTestPaths testPaths) {
			auto testCases = loadAllCasesInDirectory(testPaths.testsRootDir, testPaths, loadTestCaseMode);
			std::sort(testCases.begin(), testCases.end());
			return testCases;
		}

	}

	IntegrationTestCase::IntegrationTestCase(const boost::filesystem::path& cnTestSourceDir,
	                                         const boost::filesystem::path& absTestSourceDir,
	                                         const boost::filesystem::path& absTestOutputDir,
	                                         const vector<boost::filesystem::path>& files,
	                                         const vector<boost::filesystem::path>& includeDirs,
	                                         const vector<boost::filesystem::path>& libDirs,
	                                         const vector<string>& libNames,
	                                         const vector<boost::filesystem::path>& interceptedHeaderFileDirectories,
	                                         bool enableOpenMP,
	                                         bool enableOpenCL,
	                                         const Properties& properties)
				: cnTestSourceDir(cnTestSourceDir),
				  absTestSourceDir(absTestSourceDir),
				  absTestOutputDir(absTestOutputDir),
				  files(files),
				  includeDirs(includeDirs),
				  libDirs(libDirs),
				  libNames(libNames),
				  interceptedHeaderFileDirectories(interceptedHeaderFileDirectories),
				  enableOpenMP(enableOpenMP),
				  enableOpenCL(enableOpenCL),
				  properties(properties)
	{
		if(enableOpenCL && !utils::getOpenCLRootDir().empty() && !utils::getOpenCLRootDir().empty()) {
			// add the OpenCL specific directories
			this->includeDirs.push_back(utils::getOpenCLRootDir() + "include/");
			this->libDirs.push_back(utils::getOpenCLRootDir() + "lib64/");
		}
	}

	frontend::ConversionJob IntegrationTestCase::toConversionJob() const {
		// prepare arguments
		std::vector<std::string> args = {"dummy_compiler_name_as_first_argument"};
		auto compilerArgs = getCompilerArguments(TEST_STEP_INSIEMECC_RUN_C_CONVERT, false, false);
		auto insiemeArgs = getInsiemeCompilerArguments(TEST_STEP_INSIEMECC_RUN_C_CONVERT, false);
		args.insert(args.end(), compilerArgs.begin(), compilerArgs.end());
		args.insert(args.end(), insiemeArgs.begin(), insiemeArgs.end());

		// append files to compile
		for(auto file : files) {
			args.push_back(file.string());
		}

		// parse using our standard command line parser
		driver::cmd::Options options = driver::cmd::Options::parse(args);

		return options.job;
	}

	const map<string, string> IntegrationTestCase::getDefinitions(std::string step) const {
		std::map<string, string> defs;
		boost::char_separator<char> sep("\",");
		std::string defStr = properties.get("definitions", step);

		boost::tokenizer<boost::char_separator<char>> tokens = boost::tokenizer<boost::char_separator<char>>(defStr, sep);
		for(const auto& t : tokens) {
			std::string name = t;
			if(!name.empty()) {
				if(name.find("=") != std::string::npos) {
					defs[name.substr(0, name.find("="))] = name.substr(name.find("=") + 1);
				} else {
					defs[name] = "1";
				}
			}
		}
		return defs;
	}

	core::ProgramPtr IntegrationTestCase::load(core::NodeManager& manager) const {
		return toConversionJob().execute(manager);
	}

	core::tu::IRTranslationUnit IntegrationTestCase::loadTU(core::NodeManager& manager) const {
		return toConversionJob().toIRTranslationUnit(manager);
	}

	std::string IntegrationTestCase::getCompilerString(std::string step, bool isCpp, bool isBackendCompileStep) const {
		if(isBackendCompileStep) {
			if(isCpp) {
				const char* envVar = std::getenv(INSIEME_CXX_BACKEND_COMPILER);
				if(envVar != nullptr) return envVar;
			} else {
				const char* envVar = std::getenv(INSIEME_C_BACKEND_COMPILER);
				if(envVar != nullptr) return envVar;
			}
		}

		// integration test config
		auto compilerProperty = properties.get("compiler", step);
		if(!compilerProperty.empty()) return compilerProperty;

		// fallback
		if(isCpp)
			return utils::compiler::getDefaultCxxCompilerExecutable();
		else
			return utils::compiler::getDefaultCCompilerExecutable();
	}

	const vector<string> IntegrationTestCase::getCompilerArguments(std::string step, bool isCpp, bool addLibs, bool isBackendCompileStep) const {
		vector<string> compArgs;

		// add include directories
		for(const auto& cur : includeDirs) {
			compArgs.push_back(std::string("-I") + cur.string());
		}

		if(addLibs) {
			// add external lib dirs
			for(const auto& cur : libDirs) {
				compArgs.push_back(std::string("-L") + cur.string());
			}

			// add external libs
			for(const auto& cur : libNames) {
				compArgs.push_back(std::string("-l") + cur);
			}
		}

		// add pre-processor definitions
		for_each(getDefinitions(step), [&](const std::pair<string, string>& def) {
			std::string definition = "-D" + def.first + "=" + def.second;
			compArgs.push_back(definition);
		});

		// add compiler flags according to the compiler configuration file
		auto compilerString = getCompilerString(step, isCpp, isBackendCompileStep);
		auto compilerArgsFromConfig = getCompilerArgumentsFromConfiguration(compilerString, properties);

		for(const auto& key : properties.getKeys()) {
			auto isBoolFlagSet = properties.get<bool>(key, step);
			// check if property is switched on
			if(isBoolFlagSet) {
				// check if property is supported
				if(compilerArgsFromConfig.getKeys().count(key) != 0) {
					// there might be multiple values per key here
					auto args = compilerArgsFromConfig.get<std::vector<std::string>>(key);
					compArgs.insert(compArgs.end(), args.begin(), args.end());
				} else {
					std::cout << "WARNING: Property " << key << " not supported by the current compiler configuration!" << std::endl;
				}
			}
		}

		// add standard flags specified in configuration file
		auto standardFlags = compilerArgsFromConfig.get<std::vector<std::string>>("standardFlags");
		compArgs.insert(compArgs.end(), standardFlags.begin(), standardFlags.end());

		// add remaining flags
		compArgs.push_back(properties.get("compFlags", step));

		// remove all empty arguments
		compArgs.erase(std::remove_if(compArgs.begin(), compArgs.end(), [](const auto& arg) { return arg.empty(); }), compArgs.end());

		return compArgs;
	}

	const vector<string> IntegrationTestCase::getInsiemeCompilerArguments(std::string step, bool isCpp) const {
		vector<string> compArgs;

		// add intercepted include directories
		for(const auto& cur : interceptedHeaderFileDirectories) {
			compArgs.push_back(std::string("--intercept-include=") + cur.string());
		}

		return compArgs;
	}

	IntegrationTestPaths getDefaultIntegrationTestPaths() {
		return { fs::path(utils::getInsiemeTestRootDir()), fs::path(utils::getInsiemeBuildRootDir()), "integration_test_config", fs::path(utils::getInsiemeBuildRootDir()) / "integration-testdirs" };
	}

	const vector<IntegrationTestCase>& getAllCases(const LoadTestCaseMode loadTestCaseMode, const IntegrationTestPaths testPaths) {
		static vector<IntegrationTestCase> TEST_CASES = getAllTestCasesInternal(loadTestCaseMode, testPaths);
		return TEST_CASES;
	}

	const boost::optional<IntegrationTestCase> getCase(const fs::path& cnTestDir, const IntegrationTestPaths testPaths) {
		// get all cases which are loaded by default
		auto allCases = getAllCases(ENABLED_TESTS, testPaths);

		// search for case with given canonical path
		for(const auto& testCase : allCases) {
			if(cnTestDir == testCase.getCanonicalPath()) {
				return testCase;
			}
		}

		// try loading test case directly (e.g if blacklisted)
		return loadSingleTestCase(fs::path(cnTestDir), testPaths);
	}

	vector<IntegrationTestCase> getTestSuite(const fs::path& testDir, const IntegrationTestPaths testPaths) {
		fs::path absTestDir = absolutizeTestDir(testDir, testPaths);

		// first check if it's an individual test case. Individual test cases have no "blacklisted_tests" file in their folder
		if(!fs::exists(absTestDir / "blacklisted_tests")) {
			auto testCase = loadSingleTestCase(absTestDir, testPaths);
			if(testCase) {
				return toVector(*testCase);
			} else {
				return {};
			}
		}

		// otherwise it is a whole directory structure. Load all the test cases in there
		return loadAllCasesInDirectory(absTestDir, testPaths);
	}

	vector<IntegrationTestCase> loadCasesForOptions(IntegrationTestPaths testPaths, const Options& options) {
		// if no test is specified explicitly load all of them
		LoadTestCaseMode loadMode = ENABLED_TESTS;
		if(options.blacklistedOnly) loadMode = BLACKLISTED_TESTS;
		else if(options.longTestsOnly) loadMode = LONG_TESTS;
		else if(options.longTestsAlso) loadMode = ENABLED_AND_LONG_TESTS;

		if(options.inplace)
			testPaths.outputDir = testPaths.testsRootDir;

		if(options.cases.empty()) {
			return getAllCases(loadMode, testPaths);
		}

		// load selected test cases
		vector<IntegrationTestCase> cases;
		for(const auto& cur : options.cases) {
			// load test case based on the location
			auto curSuite = getTestSuite(boost::filesystem::path(cur), testPaths);
			for(const auto& cur : curSuite) {
				if(!contains(cases, cur)) { // make sure every test is only present once
					cases.push_back(cur);
				}
			}
		}
		return cases;
	}

	namespace {
		bool isExcluded(const IntegrationTestCase& testCase, const TestStep& testStep) {
			std::string excludes = testCase.getProperties()["excludeSteps"];
			boost::char_separator<char> sep(",\"");
			boost::tokenizer<boost::char_separator<char>> tokens(excludes, sep);

			for(const string& it : tokens) {
				string tmp(it);
				boost::replace_all(tmp, "*", ".*");
				std::regex reg(tmp);
				if(std::regex_match(testStep.getName(), reg)) { return true; }
			}
			return false;
		}

		bool addStepDependencies(std::set<TestStep>& steps, const TestStep& testStep, const IntegrationTestCase& testCase, bool ignoreExcludes, bool excludeIsWarning) {
			if(!ignoreExcludes && isExcluded(testCase, testStep)) {
				return false;
			}

			// insert the step itself
			steps.insert(testStep);

			// and insert all dependencies recursively
			for(const auto& dependencyName : testStep.getDependencies()) {
				auto dependency = getStepByName(dependencyName);
				assert_true(dependency) << "TestStep \"" << testStep.getName() << "\" depends on non-existant TestStep \"" << dependencyName << "\"";
				// add child dependencies. If one of them is excluded, return false
				if(!addStepDependencies(steps, dependency.get(), testCase, ignoreExcludes, excludeIsWarning)) {
					if(excludeIsWarning) {
						std::cerr << "INFO: TestStep \"" << testStep.getName() << "\" can not be scheduled because it's dependency \"" << dependencyName << "\" has been excluded" << std::endl;
					}
					return false;
				}
			}
			return true;
		}

		std::set<TestStep> scheduleStep(const TestStep& testStep, const IntegrationTestCase& testCase, bool ignoreExcludes, bool excludeIsWarning) {
			std::set<TestStep> res;
			// recursively add the TestStep itself, as well as any dependencies.
			if(addStepDependencies(res, testStep, testCase, ignoreExcludes, excludeIsWarning)) {
				return res;
			}

			// return an empty set of steps, if one of the steps or it's dependencies has been excluded
			return {};
		}
	}

	std::set<TestStep> getTestStepsForTestCaseAndOptions(const IntegrationTestCase& testCase, const Options& options) {
		// in case the test requires OpenCL but we do lack of e.g. headers, simply disable it
		if(testCase.isEnableOpenCL() && !utils::compiler::isOpenCLAvailable()) return {};

		// if we should run pre- or postprocessing only, return the respective steps
		if(options.preprocessingOnly) {
			return { getStepByName(TEST_STEP_PREPROCESSING).get() };
		} else if(options.postprocessingOnly) {
			return { getStepByName(TEST_STEP_POSTPROCESSING).get() };
		}

		std::set<TestStep> res;

		// if the user requested to execute only certain steps, we add them and all their dependencies, if neither is blacklisted
		if(!options.steps.empty()) {
			for(const auto& stepToExecute : options.steps) {
				auto step = getStepByName(stepToExecute);
				if(!step) {
					std::cerr << "WARNING: No such step to run: " << stepToExecute << std::endl;
				} else {
					// add all required steps to the final schedule
					auto stepsToSchedule = scheduleStep(step.get(), testCase, options.ignoreExcludes, true);
					res.insert(stepsToSchedule.begin(), stepsToSchedule.end());
					if(stepsToSchedule.empty()) {
						std::cerr << "WARNING: No steps scheduled for step: " << stepToExecute << std::endl;
					}
				}
			}

			// if the user didn't specify steps, we schedule all steps which haven't been excluded (or depend on excluded steps respectively)
		} else {
			for(const auto& step : getFullStepList()) {
				auto stepsToSchedule = scheduleStep(step, testCase, false, false);
				res.insert(stepsToSchedule.begin(), stepsToSchedule.end());
			}
		}

		// remove the pre- and postprocessing steps from this list. Those steps are special and shouldn't be executed normally
		res.erase(getStepByName(TEST_STEP_PREPROCESSING).get());
		res.erase(getStepByName(TEST_STEP_POSTPROCESSING).get());

		// ensure the prerequisite check is there (it will be scheduled as the first step)
		res.insert(getStepByName(TEST_STEP_CHECK_PREREQUISITES).get());

		return res;
	}

} // end namespace integration
} // end namespace driver
} // end namespace insieme
