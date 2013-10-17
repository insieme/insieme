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

#include <boost/optional.hpp>

#include "insieme/utils/test/test_config.h"
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

			// add pre-processor definitions
			for_each(testCase.getDefinitions(), [&](const std::pair<string,string>& def) {
				job.setDefinition(def.first, def.second);
			});

			return job;
		}

	}


	core::ProgramPtr IntegrationTestCase::load(core::NodeManager& manager) const {
		return toJob(*this).execute(manager);
	}

	frontend::tu::IRTranslationUnit IntegrationTestCase::loadTU(core::NodeManager& manager) const {
		return toJob(*this).toTranslationUnit(manager);
	}

	namespace fs = boost::filesystem;

	namespace {

		vector<IntegrationTestCase> loadAllCases(const std::string& testDirStr, const std::string& prefix = "") {
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
				LOG(WARNING) << "Not test-configuration file found!";
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
				std::remove(testCase.begin(), testCase.end(), ' ');
				if (!testCase.empty() && testCase[0] != '#' && fs::is_directory(testDir / testCase)) {

					testCases.push_back(testCase);
				}
			}
			configFile.close();


			// load individual test cases
			for(auto it=testCases.begin(); it != testCases.end(); ++it) {
				const string& cur = *it;

				// check test case directory
				const fs::path testCaseDir = testDir / cur;
				if (!fs::exists(testCaseDir)) {
					LOG(WARNING) << "Directory for test case " + cur + " not found!";
					continue;
				}

				const fs::path subTestConfig = testCaseDir / "test.cfg";
				if (fs::exists(subTestConfig)) {
					LOG(INFO) << "Descending into sub-test-directory " << (testCaseDir).string();
					vector<IntegrationTestCase>&& subCases = loadAllCases((testCaseDir).string(), prefix + cur + "/");
					std::copy(subCases.begin(), subCases.end(), std::back_inserter(res));	
					continue;
				}


				// read inputs.data (if present)
				vector<frontend::path> files;
				vector<frontend::path> includeDirs;
				auto inputFile = testCaseDir / "inputs.data";
				if (fs::exists(inputFile)) {
					// read file list from inputs.data
					fs::ifstream inputs;
					inputs.open(inputFile);
					if (!inputs.is_open()) {
						LOG(WARNING) << "Unable to open input file " << inputFile.string();
						continue;
					}

					// read entry by entry
					while (!inputs.eof()) {
						string file;
						inputs >> file;
						std::remove(file.begin(), file.end(), ' ');
						if (!file.empty()) {
							if (file[0] != '-') {
								// it's an input file
								files.push_back((testCaseDir / file).string());
							} else if (file.length() > 2){
								// it's an include directory
								string path = file.substr(2);
								// if it starts with /, assume absolute path
								if(path.at(0) == '/')
									includeDirs.push_back(path);
								// if not, assume path to be relative to testCaseDir
								else
									includeDirs.push_back((testCaseDir / path).string());
							}
						}
					}
					inputs.close();

				} else {
					// use default file name
					if (fs::exists(testCaseDir / (cur + ".c")))
						// This is a C test case 
						files.push_back((testCaseDir / (cur + ".c")).string());
					else {
						// this must be a c++ test case
						assert(fs::exists(testCaseDir / (cur + ".cpp")));
						files.push_back((testCaseDir / (cur + ".cpp")).string());
					}
				}

				// collect flags
				bool enableOpenMP = false;
				bool enableOpenCL = false;
				map<string,string> definitions;
				auto flagsFile = testCaseDir / "insieme.flags";
				if (fs::exists(flagsFile)) {
					// just check for special flags
					fs::ifstream inputs;
					inputs.open(flagsFile);
					if (!inputs.is_open()) {
						LOG(WARNING) << "Unable to open flag file " << flagsFile.string();
						continue;
					}

					// read entry by entry
					while (!inputs.eof()) {
						string flag;
						inputs >> flag;
						std::remove(flag.begin(), flag.end(), ' ');
						if (flag.empty()) continue;

						// process flags
						if (flag == "--omp-sema") {
							// enable the OpenMP frontend
							enableOpenMP = true;
						}
						if (flag == "--opencl") {
							// enable the OpenCL frontend
							enableOpenCL = true;
						}

						if (flag.size() > 2 && flag[0] == '-' && flag[1] == 'D') {
							string def = flag.substr(2);
							std::size_t eq_pos = def.find('=');
							if (eq_pos == std::string::npos) {
								// there is no equal sign => just define string
								definitions[def] = "";
							} else {
								definitions[def.substr(0,eq_pos)] = def.substr(eq_pos+1);
							}
						}
					}
					inputs.close();
				}

				// collect compiler arguments
				vector<string> compilerFlags;
				auto compilerFlagsFile = testCaseDir / "test-gcc.flags";
				if (fs::exists(compilerFlagsFile)) {
					// just check for special flags
					fs::ifstream inputs;
					inputs.open(compilerFlagsFile);
					if (!inputs.is_open()) {
						LOG(WARNING) << "Unable to open flag file " << compilerFlagsFile.string();
						continue;
					}

					// read entry by entry
					while (!inputs.eof()) {
						string flag;
						inputs >> flag;
						std::remove(flag.begin(), flag.end(), ' ');
						if (flag.empty()) continue;

						// process flag
						if (fs::exists(testCaseDir / flag)) {
							// it is an extra file to be compiled => use absolute path
							compilerFlags.push_back((testCaseDir / flag).string());
						} else {
							// accept as ordinary flag
							compilerFlags.push_back(flag);
						}
					}
					inputs.close();
				}


				// add test case
				res.push_back(IntegrationTestCase(prefix + cur, files, includeDirs, enableOpenMP, enableOpenCL, definitions, compilerFlags));
			}

			return res;
		}
	}

	// a global variable containing the list of test cases after they have been loaded the first time
	boost::optional<vector<IntegrationTestCase>> TEST_CASES = 0;

	const vector<IntegrationTestCase>& getAllCases() {
		// check whether cases have been loaded before
		if (!TEST_CASES) {
			TEST_CASES = boost::optional<vector<IntegrationTestCase>>(loadAllCases(TEST_ROOT_DIR));
			std::sort(TEST_CASES->begin(), TEST_CASES->end());
		}
		return *TEST_CASES;
	}

	const IntegrationTestCaseOpt getCase(const string& name) {

		// load list of test cases
		const vector<IntegrationTestCase>& cases = getAllCases();

		// search for case with given name
		for(auto it = cases.begin(); it != cases.end(); ++it) {
			if (it->getName() == name) {
				return *it;
			}
		}
		// no such test case present
		return IntegrationTestCaseOpt();
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
		for_each(definitions, [&](const std::pair<string,string>& def) {
			job.setDefinition(def.first, def.second);
		});

		return job.execute(manager);

	}


} // end namespace integration
} // end namespace driver
} // end namespace insieme
