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

#include "insieme/driver/integration/tests.h"

#include <iostream>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/tokenizer.hpp>

#include <boost/optional.hpp>
#include <boost/algorithm/string.hpp>

#include "insieme/utils/config.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/string_utils.h"

#include "insieme/frontend/frontend.h"

namespace insieme {
namespace driver {
namespace integration {

	namespace {

		frontend::ConversionJob toJob(const IntegrationTestCase& testCase) {

			// load code using frontend
			auto job = frontend::ConversionJob(testCase.getFiles(), testCase.getIncludeDirs());
			job.setOption(frontend::ConversionJob::OpenMP, testCase.isEnableOpenMP());
			job.setOption(frontend::ConversionJob::OpenCL, testCase.isEnableOpenCL());
			job.setOption(insieme::frontend::ConversionSetup::ProgressBar);

			std::string step="main_run_convert";
			if (testCase.isCXX11()){
				job.setStandard(frontend::ConversionSetup::Cxx11);
				step="main_run_c++_convert";
			}

			// add pre-processor definitions
			for_each(testCase.getDefinitions(step), [&](const std::pair<string,string>& def) {
				job.setDefinition(def.first, def.second);
			});

			// add interceptor configuration
			job.addInterceptedNameSpacePatterns(testCase.getInterceptedNameSpaces());
			job.setInterceptedHeaderDirs(testCase.getInterceptedHeaderFileDirectories());

			// done
			return job;
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
					assert_true(fs::exists(testCaseDir / (caseName + ".cpp"))) << "file dosen't exist: " << testCaseDir << "/" << caseName << ".cpp";
					files.push_back((testCaseDir / (caseName + ".cpp")).string());

					// if test is located in apropiate folder, activate CXX11 standard
					//std::size_t found = testCaseDir.string().find("cpp11");
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

			// add test case
			return IntegrationTestCase(testName, testCaseDir, files, includeDirs, libPaths, libNames,
					interceptionNameSpacePatterns, interceptedHeaderFileDirectories,
					enableOpenMP, enableOpenCL, enableCXX11, prop);
		}


		vector<IntegrationTestCase> loadAllCases(const std::string& testDirStr, const std::string& prefix = "", bool forceCommented = false) {

			// create a new result vector
			vector<IntegrationTestCase> res;

			// obtain access to the test directory
			const fs::path testDir(testDirStr /*TEST_ROOT_DIR*/);

			// check whether the directory is correct
			if (!fs::exists(testDir)) {
				LOG(WARNING) << "Test-Directory path not properly set!";
				return res;
			}

			// read the test.cfg file
			const fs::path testConfig = testDir / "test.cfg";
			if (!fs::exists(testConfig)) {
				LOG(WARNING) << "No test-configuration file found!";
				return res;
			}

			fs::ifstream configFile;
			configFile.open(testConfig);
			if (!configFile.is_open()) {
				LOG(WARNING) << "Unable to open test-configuration file!";
				return res;
			}
			vector<string> testCases;

			string testCase ;
			while ( getline(configFile, testCase) ) {

				// remove any comments, except if commented tests should be included
				if(!forceCommented || !boost::starts_with(boost::algorithm::trim_copy(testCase),"#"))
					testCase = testCase.substr(0,testCase.find("#",0));
				else		//only remove first "#"
					testCase=testCase.substr(testCase.find("#")+1);
				
				// trim
				testCase.erase(0, testCase.find_first_not_of(" "));
				testCase.erase(testCase.find_last_not_of(" ")+1);

				if (!testCase.empty() && fs::is_directory(testDir / testCase)) {
					testCases.push_back(testCase);
				}
			}
			configFile.close();

			// load individual test cases
			for(const string& cur : testCases) {

				// check test case directory
				const fs::path testCaseDir = fs::canonical(fs::absolute(testDir / cur));
				if (!fs::exists(testCaseDir)) {
					LOG(WARNING) << "Directory for test case " + cur + " not found!";
					continue;
				}

				// check whether it is a test suite
				const fs::path subTestConfig = testCaseDir / "test.cfg";
				if (fs::exists(subTestConfig)) {
					LOG(DEBUG) << "Descending into sub-test-directory " << (testCaseDir).string();
					vector<IntegrationTestCase>&& subCases = loadAllCases((testCaseDir).string(), prefix + cur + "/",forceCommented);
					std::copy(subCases.begin(), subCases.end(), std::back_inserter(res));
					continue;
				}

				// load individual test case
				auto testCase = loadTestCase(prefix + cur);
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
			TEST_CASES = boost::optional<vector<IntegrationTestCase>>(loadAllCases(TEST_ROOT_DIR,"",forceCommented));
			std::sort(TEST_CASES->begin(), TEST_CASES->end());
		}
		return *TEST_CASES;
	}

	const vector<IntegrationTestCase> getAllCasesAt(const string& path){
		const fs::path testDir(path);
		const fs::path testConfig = testDir / "test.cfg";
		vector<IntegrationTestCase> ret;
		IntegrationTestCaseOpt testCase;

		// parse subfolders if test.cfg is present
		if (fs::exists(testConfig))
			return vector<IntegrationTestCase>(loadAllCases(path));
		else
			testCase=getCase(path);
			if(testCase)
				ret.push_back(*testCase);
			return ret;

		return vector<IntegrationTestCase>();
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
			assert(parent.is_absolute());
			//assertion fails if child is empty
			//assert(child.is_absolute());

			// if it is the same => done
			if (parent == child) return true;

			// if child is empty => terminate
			if (child.empty()) return false;

			// go one more step
			return isParentOf(parent, child.parent_path());
		}

	}

	vector<IntegrationTestCase> getTestSuite(const string& path, bool forceCommented) {

		// load list of test cases
		const vector<IntegrationTestCase>& cases = getAllCases(forceCommented);

		// convert the path into an absolute path
		frontend::path absolute_path = fs::canonical(fs::absolute(path));

		// search for case with given name
		vector<IntegrationTestCase> res;
		for(const auto& cur : cases) {
			// check the directory
			if (isParentOf(absolute_path, cur.getDirectory())) {
				res.push_back(cur);
			}
		}

		// if not included in ALL test cases since not covered by the configuration => load rest
		if (res.empty()) {
            string prefix;
            if(absolute_path.string().size() > fs::canonical(fs::absolute(TEST_ROOT_DIR)).string().size()) {
                prefix = absolute_path.string().substr(fs::canonical(fs::absolute(TEST_ROOT_DIR)).string().size());
                if(prefix.back() != '/') prefix.push_back('/');
            }
			res = loadAllCases(path, prefix );
		}

		// if still not found => it is a individual test case
		if (res.empty()) {

			const fs::path testConfig = absolute_path / "test.cfg";
			if (!fs::exists(testConfig)) { 
				//individual test cases have no "test.cfg"
				auto testCase = loadTestCase(path);
				if (testCase) {
					return toVector(*testCase);
				}
			}
		}

		// return list of results
		return res;
	}


	core::ProgramPtr loadIntegrationTest(core::NodeManager& manager, const std::string& name, bool enableOpenMP, const std::map<string,string>& definitions) {

		// load case information
		auto curCase = getCase(name);
		if (!curCase) {
			return core::ProgramPtr();
		}

		// load code using frontend - using given options
		auto job = frontend::ConversionJob(curCase->getFiles(), curCase->getIncludeDirs());
		job.setOption(frontend::ConversionJob::OpenMP, enableOpenMP);

		// add pre-processor definitions
		for(const auto& cur : definitions) {
			job.setDefinition(cur.first, cur.second);
		}

		return job.execute(manager);

	}


} // end namespace integration
} // end namespace driver
} // end namespace insieme
