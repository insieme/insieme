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

#include "insieme/utils/config.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/string_utils.h"

#include "insieme/frontend/frontend.h"

#include "insieme/driver/cmd/insiemecc_options.h"
#include "insieme/driver/integration/test_step.h"
#include "insieme/frontend/translation_unit.h"

namespace insieme {
namespace driver {
namespace integration {

	namespace {

		frontend::ConversionJob toJob(const IntegrationTestCase& testCase) {

			// load code using frontend
			std::vector<std::string> args = {"compiler"};
			for(auto file : testCase.getFiles()) {
				args.push_back(file.string());
			}
			for(auto includeDir : testCase.getIncludeDirs()) {
				std::string include = "-I"+includeDir.string();
				args.push_back(include);
			}
			if(testCase.isEnableOpenMP()) {
				args.push_back("-fopenmp");
			}
			if(testCase.isEnableOpenCL()) {
				args.push_back("--flib-icl -lOpenCL");
			}

			driver::cmd::Options options = driver::cmd::Options::parse(args);
			options.job.setOption(insieme::frontend::ConversionSetup::ProgressBar);

			std::string step=TEST_STEP_INSIEMECC_RUN_C_CONVERT;
			if (testCase.isCXX11()){
				options.job.setStandard(frontend::ConversionSetup::Cxx11);
				step=TEST_STEP_INSIEMECC_RUN_CPP_CONVERT;
			}

			// add pre-processor definitions
			for_each(testCase.getDefinitions(step), [&](const std::pair<string,string>& def) {
				options.job.setDefinition(def.first, def.second);
			});

			// add interceptor configuration
			options.job.addInterceptedNameSpacePatterns(testCase.getInterceptedNameSpaces());
			options.job.setInterceptedHeaderDirs(testCase.getInterceptedHeaderFileDirectories());

			// done
			return options.job;
		}

	}


	core::ProgramPtr IntegrationTestCase::load(core::NodeManager& manager) const {
		return toJob(*this).execute(manager);
	}

	frontend::tu::IRTranslationUnit IntegrationTestCase::loadTU(core::NodeManager& manager) const {
		return toJob(*this).toIRTranslationUnit(manager);
	}

	namespace fs = boost::filesystem;

	namespace {

		Properties loadProperties(const fs::path& dir, const string& configFileName = "config") {
			Properties res;

			// if it is the root we are done
			if (dir.empty()) return res;

			// the directory should be absolute
			assert_eq(dir, fs::absolute(dir)) << "Expecting an absolute directory - got " << dir << "\n";

			// load configuration of parent directory
			res = loadProperties(dir.parent_path(), configFileName);

			// check whether there is a config file
			auto file = dir / configFileName;

			if (fs::exists(file)) {
				// try loading file
				fs::ifstream in(file);
				if (in.is_open()) {
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
			bool enableCXX11 = false;

			// get the test directory -- if the testName is a existingPath skip appending rootDir
			auto testDir = (fs::exists(fs::path(testName))) ? fs::path(testName) : fs::path(TEST_ROOT_DIR) / testName;

			// check test case directory
			const fs::path testCaseDir = fs::canonical(fs::absolute(testDir));
			if (!fs::exists(testCaseDir)) {
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

			//get files
			vector<frontend::path> files;

			for (const auto& file : prop.get<vector<string>>("files")) {
				if(fs::path(file).is_absolute())
					files.push_back(file);
				else
					files.push_back((testCaseDir / file).string());
			}

			//no files specified, use default names
			if(files.size()==0){

				// extract the case name from the test directory
				string caseName = boost::filesystem::path(testCaseDir).filename().string();

				// add default file name
				if (fs::exists(testCaseDir / (caseName + ".c")))
					// This is a C test case
					files.push_back((testCaseDir / (caseName + ".c")).string());
				else {
					// this must be a c++ test case
					assert_true(fs::exists(testCaseDir / (caseName + ".cpp"))) << "file doesn't exist: " << testCaseDir << "/" << caseName << ".cpp";
					files.push_back((testCaseDir / (caseName + ".cpp")).string());
				}
			}

			//get includes
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

			for (const auto& name : prop.get<vector<string>>("libNames")) {
				libNames.push_back(name);
			}

			enableCXX11  = prop.get<bool>("use_cpp11");
			enableOpenMP = prop.get<bool>("use_omp");
			enableOpenCL = prop.get<bool>("use_opencl");

			// extract interception configuration
			auto interceptionNameSpacePatterns = prop.get<vector<string>>("intercepted_name_spaces");
			vector<frontend::path> interceptedHeaderFileDirectories;

			for(const auto& path : prop.get<vector<string>>("intercepted_header_file_dirs")) {
				if(path.at(0) == '/') {
					interceptedHeaderFileDirectories.push_back(path);
				} else {
					interceptedHeaderFileDirectories.push_back((testCaseDir / path).string());
				}
			}

			// find "canonical" test name regardless of setup
			string canonRoot = fs::canonical(fs::path(TEST_ROOT_DIR)).string();
			string prefix = commonPrefix(testCaseDir.string(), canonRoot);
			string name = testCaseDir.string().substr(prefix.size());
			// don't just replace "test/" here, as unintended replacements might occur deeper in the directory structure
			boost::algorithm::replace_first(name, "ext/test/", "ext/");
			boost::algorithm::replace_first(name, "base/test/", "base/");
			if(boost::algorithm::starts_with(name, "/")) name = name.substr(1);

			// add test case
			return IntegrationTestCase(name, testCaseDir, files, includeDirs, libPaths, libNames,
					interceptionNameSpacePatterns, interceptedHeaderFileDirectories,
					enableOpenMP, enableOpenCL, enableCXX11, prop);
		}


		vector<IntegrationTestCase> loadAllCases(const std::string& testDirStr, bool forceCommented = false) {

			// create a new result vector
			vector<IntegrationTestCase> res;

			// obtain access to the test directory
			const fs::path testDir(testDirStr);

			// check whether the directory is correct
			if (!fs::exists(testDir) || !fs::is_directory(testDir)) {
				LOG(WARNING) << "Test-Directory path not properly set!";
				return res;
			}

			// read the blacklisted_tests file
			const fs::path blacklistedTestsPath = testDir / "blacklisted_tests";
			if (!fs::exists(blacklistedTestsPath)) {
				LOG(WARNING) << "No blacklisted_tests file found!";
				return res;
			}

			//Add all sub directories to the list of test cases to load
			set<string> testCases;
			for(fs::directory_iterator it(testDir); it != fs::directory_iterator(); ++it) {
				const fs::path file = *it;
				if (fs::is_directory(file)) {
					testCases.insert(file.filename().string());
				}
			}

			//now exclude all the blacklisted test cases
			fs::ifstream blacklistedTestsFile;
			blacklistedTestsFile.open(blacklistedTestsPath);
			if (!blacklistedTestsFile.is_open()) {
				LOG(WARNING) << "Unable to open blacklisted_tests file!";
				return res;
			}

			string testCase;
			while (getline(blacklistedTestsFile, testCase)) {
				// remove any comments, except if commented tests should be included
				if(!forceCommented || !boost::starts_with(boost::algorithm::trim_copy(testCase),"#"))
					testCase = testCase.substr(0,testCase.find("#",0));
				else		//only remove first "#"
					testCase=testCase.substr(testCase.find("#")+1);

				// trim
				testCase.erase(0, testCase.find_first_not_of(" "));
				testCase.erase(testCase.find_last_not_of(" ")+1);

				if (!testCase.empty()) {
					if (!fs::is_directory(testDir / testCase)) {
						LOG(WARNING) << "Blacklisted test case \"" << testCase << "\" does not exist.";
					} else {
						testCases.erase(testCase);
					}
				}
			}
			blacklistedTestsFile.close();

			// load individual test cases
			for(const string& cur : testCases) {

				// check test case directory
				const fs::path testCaseDir = fs::canonical(fs::absolute(testDir / cur));
				if (!fs::exists(testCaseDir)) {
					LOG(WARNING) << "Directory for test case " + cur + " not found!";
					continue;
				}

				// check whether it is a test suite
				const fs::path subTestBlacklist = testCaseDir / "blacklisted_tests";
				if (fs::exists(subTestBlacklist)) {
					LOG(DEBUG) << "Descending into sub-test-directory " << (testCaseDir).string();
					vector<IntegrationTestCase>&& subCases = loadAllCases((testCaseDir).string(), forceCommented);
					std::copy(subCases.begin(), subCases.end(), std::back_inserter(res));
					continue;
				}

				// load individual test case
				auto testCase = loadTestCase(testCaseDir.string());
				if (testCase) res.push_back(*testCase);
			}

			return res;
		}
	}

	// a global variable containing the list of test cases after they have been loaded the first time
	boost::optional<vector<IntegrationTestCase>> TEST_CASES = 0;

	const vector<IntegrationTestCase>& getAllCases(bool forceCommented) {
		// check whether cases have been loaded before
		if (!TEST_CASES) {
			TEST_CASES = boost::optional<vector<IntegrationTestCase>>(loadAllCases(TEST_ROOT_DIR, forceCommented));
			std::sort(TEST_CASES->begin(), TEST_CASES->end());
		}
		return *TEST_CASES;
	}

	const IntegrationTestCaseOpt getCase(const string& name) {

		// load list of test cases
		const vector<IntegrationTestCase>& cases = getAllCases();

		// search for case with given name
		for(auto it = cases.begin(); it != cases.end(); ++it) {
			// for base-only setups
			if (it->getName() == name) {
				return *it;
			// for ext + base setups, check base and ext but prefer base
			} else if(it->getName() == "base/" + name) {
				return *it;
			} else if(it->getName() == "ext/" + name) {
				return *it;
			}
		}
		// no such test case present
		return IntegrationTestCaseOpt();
	}

	namespace {

		bool isParentOf(const fs::path& parent, const fs::path& child) {
			assert_true(parent.is_absolute());
			//assertion fails if child is empty
			//assert_true(child.is_absolute());

			// if it is the same => done
			if (parent == child) return true;

			// if child is empty => terminate
			if (child.empty()) return false;

			// go one more step
			return isParentOf(parent, child.parent_path());
		}

	}

	vector<IntegrationTestCase> getTestSuite(const string& path, bool forceCommented) {

		// convert the path into an absolute path
		frontend::path absolute_path = fs::canonical(fs::absolute(path));

		// first check if it's an individual test case
		const fs::path blacklistFile = absolute_path / "blacklisted_tests";
		if (!fs::exists(blacklistFile)) {
			//individual test cases have no "blacklisted_tests" file in their folder
			auto testCase = loadTestCase(path);
			if (testCase) {
				return toVector(*testCase);
			}
		}

		return loadAllCases(absolute_path.string(), forceCommented);
	}


	core::ProgramPtr loadIntegrationTest(core::NodeManager& manager, const std::string& name, bool enableOpenMP, const std::map<string,string>& definitions) {

		// load case information
		auto curCase = getCase(name);
		if (!curCase) {
			return core::ProgramPtr();
		}

		// load code using frontend - using given options
		std::vector<std::string> args = {"compiler"};
		for(auto file : curCase->getFiles()) {
			args.push_back(file.string());
		}
		for(auto incl : curCase->getIncludeDirs()) {
			std::string inc = "-I"+incl.string();
			args.push_back(inc);
		}
		if(enableOpenMP) args.push_back("-fopenmp");
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
