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

#include <sys/stat.h>
#include <sys/fcntl.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <csignal>
#include <cerrno>
#include <sstream>

#include "insieme/driver/integration/test_step.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/config.h"
#include "insieme/utils/compiler/compiler.h"

#include <boost/foreach.hpp>
#include <boost/algorithm/string.hpp>
#include <regex>
#include <boost/tokenizer.hpp>

namespace insieme {
namespace driver {
namespace integration {

	using namespace std;

	namespace {

		namespace fs = boost::filesystem;

		typedef std::set<std::string> Dependencies;

		enum Backend { Sequential, Runtime, Opencl };

		enum Language { C, CPP };

		string getExtension(Language ext) {
			switch(ext) {
			case C: return "c";
			case CPP: return "cpp";
			}
			return "xxx";
		}

		string getBackendKey(Backend be) {
			switch(be) {
			case Sequential: return "seq";
			case Runtime: return "run";
			case Opencl: return "ocl";
			}
			return "xxx";
		}

		TestStep createRefCompStep(const string& name, Language l) {
			return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				TestSetup set = setup;
				auto props = test.getPropertiesFor(name);

				// start with executable
				cmd << test.getCompilerString(name, l == Language::CPP, true);

				// add input files
				for(const auto& cur : test.getFiles()) {
					cmd << " " << cur.string();
				}

				// append all the arguments
				cmd << " " << join(" ", test.getCompilerArguments(name, l == Language::CPP, true, true));

				// disable multithreading
				set.numThreads = 0;

				// get execution directory
				string executionDirectory = test.getDirectory().string();
				if(!set.executionDir.empty()) executionDirectory = set.executionDir;

				// set output file, stdOutFile and stdErrFile
				set.outputFile = executionDirectory + "/" + test.getBaseName() + ".ref";
				set.stdOutFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".out";
				set.stdErrFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, set, props, cmd.str());
			}, std::set<std::string>(), COMPILE);
		}

		TestStep createRefRunStep(const string& name, const Dependencies& deps = Dependencies(), int numThreads = 0) {
			return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				TestSetup set = setup;
				auto props = test.getPropertiesFor(name);

				// get execution directory
				string executionDirectory = test.getDirectory().string();
				if(!set.executionDir.empty()) executionDirectory = set.executionDir;

				// start with executable
				cmd << executionDirectory << "/" << test.getBaseName() << ".ref";

				// add arguments
				cmd << " " << props["executionFlags"];

				// set number of threads
				set.numThreads = numThreads;

				// set output files
				set.stdOutFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".out";
				set.stdErrFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, set, props, cmd.str(), "", executionDirectory);
			}, deps, RUN);
		}

		TestStep createBashCommandStep(const string& name, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				TestSetup set = setup;
				auto props = test.getPropertiesFor(name);

				if(props[name].empty()) return TestResult::stepOmitted(name);

				// start with executable
				cmd << props[name];

				// set number of threads
				set.numThreads = 0;

				// get execution directory
				string executionDirectory = test.getDirectory().string();
				if(!set.executionDir.empty()) executionDirectory = set.executionDir;

				// set output files
				set.stdOutFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".out";
				set.stdErrFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, set, props, cmd.str(), "", executionDirectory);
			}, deps);
		}

		TestStep createInsiemeccSemaStep(const string& name, Language l, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				TestSetup set = setup;
				auto props = test.getPropertiesFor(name);

				// start with executable
				cmd << test.getCompilerString(name, l == Language::CPP);

				// add input files
				for(const auto& cur : test.getFiles()) {
					cmd << " " << cur.string();
				}

				// append all the arguments
				cmd << " " << join(" ", test.getCompilerArguments(name, l == Language::CPP, false));

				// append the Insieme specific arguments
				cmd << " " << join(" ", test.getInsiemeCompilerArguments(name, l == Language::CPP));

				// enable semantic tests
				cmd << " --check-sema-only";

				// get execution dir
				string executionDirectory = test.getDirectory().string();
				if(!set.executionDir.empty()) executionDirectory = set.executionDir;

				// also dump IR
				std::string irFile = executionDirectory + "/" + test.getBaseName() + ".ir";
				cmd << " --dump-ir " << irFile;

				// disable multithreading
				set.numThreads = 0;

				set.stdOutFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".out";
				set.stdErrFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, set, props, cmd.str(), irFile);
			}, deps, COMPILE);
		}

		TestStep createInsiemeccConversionStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				TestSetup set = setup;
				auto props = test.getPropertiesFor(name);

				// start with executable
				cmd << test.getCompilerString(name, l == Language::CPP);

				// add input files
				for(const auto& cur : test.getFiles()) {
					cmd << " " << cur.string();
				}

				// append all the arguments
				cmd << " " << join(" ", test.getCompilerArguments(name, l == Language::CPP, false));

				// append the Insieme specific arguments
				cmd << " " << join(" ", test.getInsiemeCompilerArguments(name, l == Language::CPP));

				// get execution dir
				string executionDirectory = test.getDirectory().string();
				if(!set.executionDir.empty()) executionDirectory = set.executionDir;

				// determine backend
				string be = getBackendKey(backend);
				cmd << " --backend " << be;

				// source-to-source compilation only
				set.outputFile = executionDirectory + "/" + test.getBaseName() + ".insieme." + be + "." + getExtension(l);
				cmd << " --dump-trg-only " << set.outputFile;

				// disable multithreading
				set.numThreads = 0;

				// set stdOut file and stdErr file
				set.stdOutFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".out";
				set.stdErrFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, set, props, cmd.str());
			}, deps, COMPILE);
		}

		TestStep createInsiemeccCompilationStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				TestSetup set = setup;
				auto props = test.getPropertiesFor(name);

				// start with executable
				cmd << test.getCompilerString(name, l == Language::CPP, true);

				// determine backend
				string be = getBackendKey(backend);

				// get execution dir
				string executionDirectory = test.getDirectory().string();
				if(!set.executionDir.empty()) executionDirectory = set.executionDir;

				// add input file
				cmd << " " << executionDirectory << "/" << test.getBaseName() << ".insieme." << be << "." << getExtension(l);

				// append all the arguments
				cmd << " " << join(" ", test.getCompilerArguments(name, l == Language::CPP, true, true));

				// add runtime include directories
				if(backend != Sequential) { // TODO: make this non-hardcoded -- it is ugly, but I don't have the time ...
					cmd << " -I " << utils::getInsiemeSourceRootDir() << "runtime/include";
					cmd << " -I " << utils::getInsiemeSourceRootDir() << "common/include";
				}

				// disable multithreading
				set.numThreads = 0;

				// set output file, stdOut file and stdErr file
				set.outputFile = executionDirectory + "/" + test.getBaseName() + ".insieme." + be;
				set.stdOutFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".out";
				set.stdErrFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, set, props, cmd.str());
			}, deps, COMPILE);
		}

		TestStep createInsiemeccExecuteStep(const string& name, Backend backend, const Dependencies& deps = Dependencies(), int numThreads = 0,
		                                    SchedulingPolicy sched = SCHED_UNDEFINED) {
			return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				TestSetup set = setup;
				auto props = test.getPropertiesFor(name);

				// get execution dir
				string executionDirectory = test.getDirectory().string();
				if(!set.executionDir.empty()) executionDirectory = set.executionDir;

				// the log file to delete afterwards
				std::string logFile = "";
				if (backend == Runtime) {
					logFile = executionDirectory + "/insieme_runtime.log";
				}

				// determine backend
				string be = getBackendKey(backend);

				// start with executable
				cmd << executionDirectory << "/" << test.getBaseName() << ".insieme." << be;

				// add arguments
				cmd << " " << props["executionFlags"];

				// set number of threads
				set.numThreads = numThreads;

				// set scheduling variant
				set.sched = sched;

				set.stdOutFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".out";
				set.stdErrFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, set, props, cmd.str(), logFile, executionDirectory);
			}, deps, RUN);
		}

		TestStep createInsiemeccCheckStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies(), int numThreads = 0,
		                                  SchedulingPolicy sched = SCHED_UNDEFINED) {
			return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				TestSetup set = setup;
				auto props = test.getPropertiesFor(name);

				std::string langstr("_c_");
				if(l == CPP) langstr = string("_cpp_");

				std::string schedString("");
				if(sched == STATIC)
					schedString = "stat_";
				else if(sched == DYNAMIC)
					schedString = "dyn_";
				else if(sched == GUIDED)
					schedString = "guid_";

				// define comparison script
				cmd << props["compareOutputScript"];

				// disable multithreading
				set.numThreads = 0;

				// get execution dir
				string executionDirectory = test.getDirectory().string();
				if(!set.executionDir.empty()) executionDirectory = set.executionDir;

				// start with executable
				cmd << " " << executionDirectory << "/" << test.getBaseName() << ".ref" + langstr + "execute.out";

				// determine backend
				string be = getBackendKey(backend);

				// pipe result to output file
				if(numThreads)
					cmd << " " << executionDirectory << "/" << test.getBaseName()
					    << ".insiemecc_" + be + langstr + "execute_" + schedString + std::to_string(numThreads) + ".out";
				else {
					cmd << " " << executionDirectory << "/" << test.getBaseName() << ".insiemecc_" + be + langstr + "execute.out";
				}

				// add awk pattern
				// TODO: generally remove outer quotation marks in properties if present - I don't have the time now but it needs to be done at some point
				string outputAwk = props["outputAwk"]; //.substr(props["outputAwk"].find("\"")+1, props["outputAwk"].rfind("\"")-1);
				cmd << " \"" << outputAwk << "\"";

				set.stdOutFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".out";
				set.stdErrFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, set, props, cmd.str());
			}, deps, CHECK);
		}

		TestStep createRefCheckStep(const string& name, Language l, const Dependencies& deps = Dependencies(), int numThreads = 0) {
			return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				auto props = test.getPropertiesFor(name);

				std::string langstr("c");
				if(l == CPP) langstr = string("cpp");

				std::stringstream cmd;
				TestSetup set = setup;

				// define comparison script
				cmd << props["compareOutputScript"];

				// get execution dir
				string executionDirectory = test.getDirectory().string();
				if(!set.executionDir.empty()) executionDirectory = set.executionDir;

				// start with executable
				cmd << " " << executionDirectory << "/" << test.getBaseName() << ".ref_" << langstr << "_execute.out";

				// pipe result to output file
				cmd << " " << executionDirectory << "/" << test.getBaseName() << ".ref_" << langstr << "_execute_" << std::to_string(numThreads) << ".out";

				// add awk pattern
				cmd << " \"" << props["outputAwk"] << "\"";

				// disable multithreading
				set.numThreads = 0;

				set.stdOutFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".out";
				set.stdErrFile = executionDirectory + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, set, props, cmd.str());
			}, deps, CHECK);
		}

		// create steps for statistics mode
		std::map<std::string, TestStep> createFullStepList(int statThreads, bool schedule) {
			std::map<std::string, TestStep> list;

			vector<int> threadList;
			for(int i = 1; i < statThreads; i *= 2) {
				threadList.push_back(i);
			}
			threadList.push_back(statThreads);

			auto add = [&](const TestStep& step) { list.insert({step.getName(), step}); };

			// --- real steps ----

			// ref compile steps
			add(createRefCompStep(TEST_STEP_REF_C_COMPILE, C));
			add(createRefCompStep(TEST_STEP_REF_CPP_COMPILE, CPP));

			// ref run steps
			add(createRefRunStep(TEST_STEP_REF_C_EXECUTE, {TEST_STEP_REF_C_COMPILE}, 1));
			add(createRefRunStep(TEST_STEP_REF_CPP_EXECUTE, {TEST_STEP_REF_CPP_COMPILE}, 1));
			// iterate over whole vector starting with second element (> 1 thread)
			for(int i : vector<int>(++threadList.begin(), threadList.end())) {
				std::string suffix = "_" + std::to_string(i);
				add(createRefRunStep(TEST_STEP_REF_C_EXECUTE + suffix, {TEST_STEP_REF_C_COMPILE}, i));
				add(createRefRunStep(TEST_STEP_REF_CPP_EXECUTE + suffix, {TEST_STEP_REF_CPP_COMPILE}, i));
			}

			// insiemecc sema steps
			add(createInsiemeccSemaStep(TEST_STEP_INSIEMECC_C_SEMA, C));
			add(createInsiemeccSemaStep(TEST_STEP_INSIEMECC_CPP_SEMA, CPP));

			// insiemecc conversion steps
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_SEQ_C_CONVERT, Sequential, C));
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_RUN_C_CONVERT, Runtime, C));
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_OCL_C_CONVERT, Opencl, C));
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_SEQ_CPP_CONVERT, Sequential, CPP));
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_RUN_CPP_CONVERT, Runtime, CPP));

			// insiemecc compilation steps
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_SEQ_C_COMPILE, Sequential, C, {TEST_STEP_INSIEMECC_SEQ_C_CONVERT}));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_RUN_C_COMPILE, Runtime, C, {TEST_STEP_INSIEMECC_RUN_C_CONVERT}));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_OCL_C_COMPILE, Opencl, C, {TEST_STEP_INSIEMECC_OCL_C_CONVERT}));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_SEQ_CPP_COMPILE, Sequential, CPP, {TEST_STEP_INSIEMECC_SEQ_CPP_CONVERT}));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_RUN_CPP_COMPILE, Runtime, CPP, {TEST_STEP_INSIEMECC_RUN_CPP_CONVERT}));

			// insiemecc execute steps
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_SEQ_C_EXECUTE, Sequential, {TEST_STEP_INSIEMECC_SEQ_C_COMPILE}));
			for(int i : threadList) {
				std::string suffix = "_" + std::to_string(i);
				add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_C_EXECUTE + suffix, Runtime, {TEST_STEP_INSIEMECC_RUN_C_COMPILE}, i));
			}
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_OCL_C_EXECUTE, Opencl, {TEST_STEP_INSIEMECC_OCL_C_COMPILE}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_SEQ_CPP_EXECUTE, Sequential, {TEST_STEP_INSIEMECC_SEQ_CPP_COMPILE}));
			for(int i : threadList) {
				std::string suffix = "_" + std::to_string(i);
				add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE + suffix, Runtime, {TEST_STEP_INSIEMECC_RUN_CPP_COMPILE}, i));
			}

			// insiemecc check steps
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_SEQ_C_CHECK, Sequential, C, {TEST_STEP_INSIEMECC_SEQ_C_EXECUTE, TEST_STEP_REF_C_EXECUTE}));
			for(int i : threadList) {
				std::string suffix = "_" + std::to_string(i);
				add(createInsiemeccCheckStep(std::string(TEST_STEP_INSIEMECC_RUN_C_CHECK) + suffix, Runtime, C,
				                             {TEST_STEP_INSIEMECC_RUN_C_EXECUTE + suffix, TEST_STEP_REF_C_EXECUTE}, i));
			}
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_OCL_C_CHECK, Opencl, C, {TEST_STEP_INSIEMECC_OCL_C_EXECUTE, TEST_STEP_REF_C_EXECUTE}));
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_SEQ_CPP_CHECK, Sequential, CPP, {TEST_STEP_INSIEMECC_SEQ_CPP_EXECUTE, TEST_STEP_REF_CPP_EXECUTE}));
			for(int i : threadList) {
				std::string suffix = "_" + std::to_string(i);
				add(createInsiemeccCheckStep(std::string(TEST_STEP_INSIEMECC_RUN_CPP_CHECK) + suffix, Runtime, CPP,
				                             {TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE + suffix, TEST_STEP_REF_CPP_EXECUTE}, i));
			}

			// additional ref steps
			for(int i : threadList) {
				std::string suffix = "_" + std::to_string(i);
				// ref check
				if(i != 1) {
					add(createRefCheckStep(TEST_STEP_REF_C_CHECK + suffix, C, {TEST_STEP_REF_C_EXECUTE, TEST_STEP_REF_C_EXECUTE + suffix}, i));
					add(createRefCheckStep(TEST_STEP_REF_CPP_CHECK + suffix, CPP, {TEST_STEP_REF_CPP_EXECUTE, TEST_STEP_REF_C_EXECUTE + suffix}, i));
				}
			}

			// clone insiemecc steps using different scheduling policies
			if(schedule) {
				std::string suffix = "_" + std::to_string(statThreads);

				std::string statSuffix = "_stat" + suffix;
				// execute STATIC
				add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_C_EXECUTE + statSuffix, Runtime, {TEST_STEP_INSIEMECC_RUN_C_COMPILE}, statThreads,
				                               STATIC));
				add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE + statSuffix, Runtime, {TEST_STEP_INSIEMECC_RUN_CPP_COMPILE}, statThreads,
				                               STATIC));
				// check STATIC
				add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_C_CHECK + statSuffix, Runtime, C,
				                             {TEST_STEP_INSIEMECC_RUN_C_EXECUTE + statSuffix, TEST_STEP_REF_C_EXECUTE}, statThreads, STATIC));
				add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_CPP_CHECK + statSuffix, Runtime, CPP,
				                             {TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE + statSuffix, TEST_STEP_REF_CPP_EXECUTE}, statThreads, STATIC));

				std::string dynSuffix = "_dyn" + suffix;
				// execute DYNAMIC
				add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_C_EXECUTE + dynSuffix, Runtime, {TEST_STEP_INSIEMECC_RUN_C_COMPILE}, statThreads,
				                               DYNAMIC));
				add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE + dynSuffix, Runtime, {TEST_STEP_INSIEMECC_RUN_CPP_COMPILE}, statThreads,
				                               DYNAMIC));
				// check DYNAMIC
				add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_C_CHECK + dynSuffix, Runtime, C,
				                             {TEST_STEP_INSIEMECC_RUN_C_EXECUTE + dynSuffix, TEST_STEP_REF_C_EXECUTE}, statThreads, DYNAMIC));
				add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_CPP_CHECK + dynSuffix, Runtime, CPP,
				                             {TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE + dynSuffix, TEST_STEP_REF_CPP_EXECUTE}, statThreads, DYNAMIC));

				std::string guidSuffix = "_guid" + suffix;
				// execute GUIDED
				add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_C_EXECUTE + guidSuffix, Runtime, {TEST_STEP_INSIEMECC_RUN_C_COMPILE}, statThreads,
				                               GUIDED));
				add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE + guidSuffix, Runtime, {TEST_STEP_INSIEMECC_RUN_CPP_COMPILE}, statThreads,
				                               GUIDED));
				// check GUIDED
				add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_C_CHECK + guidSuffix, Runtime, C,
				                             {TEST_STEP_INSIEMECC_RUN_C_EXECUTE + guidSuffix, TEST_STEP_REF_C_EXECUTE}, statThreads, GUIDED));
				add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_CPP_CHECK + guidSuffix, Runtime, CPP,
				                             {TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE + guidSuffix, TEST_STEP_REF_CPP_EXECUTE}, statThreads, GUIDED));
			}

			// preprocessing steps are executed by a special run of the integration test driver - as are postprocessing steps
			// these steps are excluded from normal runs
			// see "scheduleSteps" function
			add(createBashCommandStep(TEST_STEP_PREPROCESSING));
			add(createBashCommandStep(TEST_STEP_POSTPROCESSING));

			// the check for the prerequisites is executed before all other steps
			add(createBashCommandStep(TEST_STEP_CHECK_PREREQUISITES));

			return list;
		}


		std::map<std::string, TestStep> createFullStepList() {
			std::map<std::string, TestStep> list;

			auto add = [&](const TestStep& step) { list.insert({step.getName(), step}); };

			// ref compile steps
			add(createRefCompStep(TEST_STEP_REF_C_COMPILE, C));
			add(createRefCompStep(TEST_STEP_REF_CPP_COMPILE, CPP));

			// ref run steps
			add(createRefRunStep(TEST_STEP_REF_C_EXECUTE, {TEST_STEP_REF_C_COMPILE}));
			add(createRefRunStep(TEST_STEP_REF_CPP_EXECUTE, {TEST_STEP_REF_CPP_COMPILE}));

			// insiemecc sema steps
			add(createInsiemeccSemaStep(TEST_STEP_INSIEMECC_C_SEMA, C));
			add(createInsiemeccSemaStep(TEST_STEP_INSIEMECC_CPP_SEMA, CPP));

			// insiemecc conversion steps
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_SEQ_C_CONVERT, Sequential, C));
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_RUN_C_CONVERT, Runtime, C));
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_OCL_C_CONVERT, Opencl, C));
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_SEQ_CPP_CONVERT, Sequential, CPP));
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_RUN_CPP_CONVERT, Runtime, CPP));

			// insiemecc compilation steps
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_SEQ_C_COMPILE, Sequential, C, {TEST_STEP_INSIEMECC_SEQ_C_CONVERT}));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_RUN_C_COMPILE, Runtime, C, {TEST_STEP_INSIEMECC_RUN_C_CONVERT}));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_OCL_C_COMPILE, Opencl, C, {TEST_STEP_INSIEMECC_OCL_C_CONVERT}));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_SEQ_CPP_COMPILE, Sequential, CPP, {TEST_STEP_INSIEMECC_SEQ_CPP_CONVERT}));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_RUN_CPP_COMPILE, Runtime, CPP, {TEST_STEP_INSIEMECC_RUN_CPP_CONVERT}));

			// insiemecc execution steps
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_SEQ_C_EXECUTE, Sequential, {TEST_STEP_INSIEMECC_SEQ_C_COMPILE}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_C_EXECUTE, Runtime, {TEST_STEP_INSIEMECC_RUN_C_COMPILE}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_OCL_C_EXECUTE, Opencl, {TEST_STEP_INSIEMECC_OCL_C_COMPILE}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_SEQ_CPP_EXECUTE, Sequential, {TEST_STEP_INSIEMECC_SEQ_CPP_COMPILE}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE, Runtime, {TEST_STEP_INSIEMECC_RUN_CPP_COMPILE}));

			// insiemecc check steps
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_SEQ_C_CHECK, Sequential, C, {TEST_STEP_INSIEMECC_SEQ_C_EXECUTE, TEST_STEP_REF_C_EXECUTE}));
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_C_CHECK, Runtime, C, {TEST_STEP_INSIEMECC_RUN_C_EXECUTE, TEST_STEP_REF_C_EXECUTE}));
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_OCL_C_CHECK, Opencl, C, {TEST_STEP_INSIEMECC_OCL_C_EXECUTE, TEST_STEP_REF_C_EXECUTE}));
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_SEQ_CPP_CHECK, Sequential, CPP, {TEST_STEP_INSIEMECC_SEQ_CPP_EXECUTE, TEST_STEP_REF_CPP_EXECUTE}));
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_CPP_CHECK, Runtime, CPP, {TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE, TEST_STEP_REF_CPP_EXECUTE}));

			// preprocessing steps are executed by a special run of the integration test driver - as are postprocessing steps
			// these steps are excluded from normal runs
			// see "scheduleSteps" function
			add(createBashCommandStep(TEST_STEP_PREPROCESSING));
			add(createBashCommandStep(TEST_STEP_POSTPROCESSING));

			// the check for the prerequisites is executed before all other steps
			add(createBashCommandStep(TEST_STEP_CHECK_PREREQUISITES));

			return list;
		}
	}


	// a function obtaining an index of available steps
	const std::map<std::string, TestStep>& getFullStepList(int statThreads, bool scheduling) {
		const static std::map<std::string, TestStep> list = createFullStepList(statThreads, scheduling);
		return list;
	}

	const std::map<std::string, TestStep>& getFullStepList() {
		const static std::map<std::string, TestStep> list = createFullStepList();
		return list;
	}

	const TestStep& getStepByName(const std::string& name, int numThreads, bool scheduling) {
		static const TestStep fail;

		if(numThreads) {
			auto& list = getFullStepList(numThreads, scheduling);
			auto pos = list.find(name);
			if(pos != list.end()) { return pos->second; }
		} else {
			auto& list = getFullStepList();
			auto pos = list.find(name);
			if(pos != list.end()) { return pos->second; }
		}
		assert_fail() << "Requested unknown step: " << name;
		return fail;
	}

	bool isExcluded(string excludes, TestStep step) {
		boost::char_separator<char> sep(",\"");
		boost::tokenizer<boost::char_separator<char>> tokens(excludes, sep);

		for(const string& it : tokens) {
			string tmp(it);
			boost::replace_all(tmp, "*", ".*");
			std::regex reg(tmp);
			if(std::regex_match(step.getName(), reg)) { return true; }
		}
		return false;
	}

	// filter steps based on some conflicting steps
	vector<TestStep> filterSteps(const vector<TestStep>& steps, const IntegrationTestCase& test, const map<string, string>& conflicting) {
		auto props = test.getProperties();
		vector<TestStep> stepsToExecute;

		for(const TestStep step : steps) {
			bool conflicts = false;
			string conflictingStep = "";

			if(!conflicting.empty()) {
				for(auto confl = conflicting.begin(); confl != conflicting.end(); confl++) {
					if(step.getName().find(confl->first) != std::string::npos) {
						conflictingStep = confl->second;
						break;
					}
				}

				if(!conflictingStep.empty()) {
					for(const TestStep stepConfl : stepsToExecute) {
						if(stepConfl.getName().find(conflictingStep) != std::string::npos) {
							conflicts = true;
							break;
						}
					}
				}
			}

			if(!isExcluded(props["excludeSteps"], step) && !conflicts) { stepsToExecute.push_back(step); }

			// in case the test requires OpenCL but we do lack of e.g. headers, simply disable it
			if(test.isEnableOpenCL() && !utils::compiler::isOpenCLAvailable()) return vector<TestStep>();
			// in case the test is not an OpenCL one automatically remove all such devoted steps
			if(!test.isEnableOpenCL()) {
				// filter out all steps which are devoted to OpenCL
				for(TestStep& step : stepsToExecute)
					if(step.getName().find("_ocl_") != std::string::npos) { return vector<TestStep>(); }
			}
		}
		return stepsToExecute;
	}


	namespace {

		void scheduleStep(const TestStep& step, vector<TestStep>& res, const IntegrationTestCase& test, int numThreads = 0, bool scheduling = false) {
			// check whether test is already present
			if(::contains(res, step)) { return; }
			auto props = test.getProperties();

			if(isExcluded(props["excludeSteps"], step)) {
				LOG(WARNING) << test.getName() << " has a step with a dependency on an excluded step (" << step.getName() << ") -- please fix the test config!"
				             << std::endl;
			}

			// check that all dependencies are present
			for(const auto& cur : step.getDependencies()) {
				scheduleStep(getStepByName(cur, numThreads, scheduling), res, test);
			}

			// append step to schedule
			res.push_back(step);
		}
	}


	vector<TestStep> scheduleSteps(const vector<TestStep>& steps, const IntegrationTestCase& test, int numThreads, bool scheduling) {
		vector<TestStep> res;
		for(const auto& cur : steps) {
			scheduleStep(cur, res, test, numThreads, scheduling);
		}

		// Handling the preprocessing & postprocessing case as well as the prerequisite check
		vector<TestStep> final;
		TestStep prerequisiteCheck;
		for(const auto& cur : res) {
			// store the prerequisite step
			if(cur.getName() == TEST_STEP_CHECK_PREREQUISITES) {
				prerequisiteCheck = cur;

				// add all the others except pre- and post-processing which we'll simply drop
			} else if(cur.getName() != TEST_STEP_PREPROCESSING && cur.getName() != TEST_STEP_POSTPROCESSING) {
				final.push_back(cur);
			}
		}

		if(!prerequisiteCheck.getName().empty()) { final.insert(final.begin(), prerequisiteCheck); }
		return final;
	}

	string readFile(string fileName) {
		FILE* file = fopen(fileName.c_str(), "r");

		if(file == NULL) { return string(""); }

		char buffer[1024];
		string output;

		while(!feof(file)) {
			if(fgets(buffer, 1024, file) != NULL) { output += string(buffer); }
		}
		fclose(file);
		return output;
	}


	namespace {
		bool runSingleTestStep(const IntegrationTestCase& test, const std::string stepName) {
			TestStep step = getStepByName(stepName);

			// prepare the setup
			TestSetup setup;
			setup.mockRun = false;
			setup.sched = SCHED_UNDEFINED;
			setup.clean = true;
			setup.executionDir = "";
			setup.perf = false;

			// now execute the step
			auto result = step.run(setup, test, TestRunner::getInstance());

			// and don't forget to clean up the produced files here
			result.clean();

			// also return true if the step was omitted
			return result.wasSuccessful() || result.wasOmitted();
		}
	}

	bool checkPrerequisites(const IntegrationTestCase& test) {
		return runSingleTestStep(test, TEST_STEP_CHECK_PREREQUISITES);
	}

	/*
	 *  Test Runner member functions
	 */
	int TestRunner::executeWithTimeout(const string& executableParam, const string& argumentsParam, const string& environmentParam, const string& outFilePath,
	                                   const string& errFilePath, unsigned cpuTimeLimit, const string& execDir) const {
		/*
		 * Setup arguments
		 */

		// quick and dirty: have boost split everything and then reassemble tokens that were quoted
		vector<string> argumentsVecTemp;
		vector<string> argumentsVec;
		boost::split(argumentsVecTemp, argumentsParam, boost::is_any_of(" "));

		bool insideQuote = false;

		for(const auto& e : argumentsVecTemp) {
			if(e.empty()) { continue; }
			string temp = boost::replace_all_copy(e, "\"", "");
			if(insideQuote) {
				argumentsVec.back().append(" " + temp);
			} else {
				argumentsVec.push_back(temp);
			}
			size_t pos = string::npos;
			if((pos = e.find_first_of("\"\'")) != string::npos) {
				// in case a single word was quoted
				if(e.find_first_of("\"\'", pos + 1) != string::npos) {
					continue;
				} else {
					insideQuote = !insideQuote;
				}
			}
		}

		// convert arguments to char**
		vector<char*> argumentsForExec;
		// argv[0] needs to be the executable itself
		argumentsForExec.push_back(const_cast<char*>(executableParam.c_str()));
		for(const auto& s : argumentsVec) {
			if(!s.empty()) { argumentsForExec.push_back(const_cast<char*>(s.c_str())); }
		}
		// terminate
		argumentsForExec.push_back(nullptr);

		/*
		 * Setup environment
		 */

		vector<string> environmentVec;
		boost::split(environmentVec, environmentParam, boost::is_any_of(" "));
		std::map<string, string> environmentMap;

		// convert environment variables to char**
		// add existing environment variables of the current shell session we are running in
		unsigned i = 0;
		while(environ[i] != nullptr) {
			string current(environ[i]);
			string varName = current.substr(0, current.find("="));
			string varValue = string(getenv(varName.c_str()));
			environmentMap[varName] = varValue;
			i++;
		}

		// match the name of each environment variable with the syntax ${NAME}
		std::regex reg("\\$\\{([^\\}]*)");
		std::regex_constants::match_flag_type flags = std::regex_constants::match_default;

		// iterate through Insieme environment setup, expand variables and merge everything with the environment map
		for(auto s : environmentVec) {
			if(!s.empty()) {
				string varName = s.substr(0, s.find("="));
				string varValue = s.substr(s.find("=") + 1, string::npos);
				string expandedVarValue;
				std::match_results<std::string::const_iterator> what;
				string::const_iterator begin = varValue.begin();
				string::const_iterator end = varValue.end();
				while(std::regex_search(begin, end, what, reg, flags)) {
					boost::replace_all(varValue, string("${" + what[1].str() + "}"), environmentMap[what[1]]);
					begin = what[0].second;
				}
				// replace if already present, i.e. normal shell behavior
				environmentMap[varName] = varValue;
			}
		}

		// convert environment to char**
		// temp vector to be able to use c_str() later
		vector<string> environmentTemp;
		vector<char*> environmentForExec;
		for(auto e : environmentMap) {
			environmentTemp.push_back(string(e.first + "=" + e.second));
			environmentForExec.push_back(const_cast<char*>(environmentTemp.back().c_str()));
		}
		// terminate
		environmentForExec.push_back(nullptr);

		/*
		 * Fork, setup timeout, stdout and sterr redirection, execute and wait
		 */

		int retVal = 0;
		// create child to execute current step within CPU time limit, have parent wait for its exit/termination
		pid_t pid = fork();
		if(pid == -1) {
			std::cerr << "Unable to fork, reason: " << strerror(errno) << "\n";
		} else if(pid == 0) {
			// soft and hard limit in seconds, will raise SIGXCPU and SIGKILL respectively afterwards, or only SIGKILL if they are equal
			struct rlimit cpuLimit = { 0, 0 };
			getrlimit(RLIMIT_CPU, &cpuLimit);
			// set cpu time limit only if current setting is unlimited or at least larger than our own limit
			if((cpuLimit.rlim_cur > cpuTimeLimit || cpuLimit.rlim_cur == RLIM_INFINITY) && (cpuLimit.rlim_max > cpuTimeLimit + 5 || cpuLimit.rlim_max == RLIM_INFINITY)) {
				cpuLimit.rlim_cur = cpuTimeLimit;
				cpuLimit.rlim_max = cpuTimeLimit + 5;
				if(setrlimit(RLIMIT_CPU, &cpuLimit) != 0) { std::cerr << strerror(errno); }
			}
			// stdout and stderr redirection
			int fdOut, fdErr;
			if((fdOut = open(outFilePath.c_str(), O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR)) == -1) {
				std::cerr << "Unable to create stdout file " << outFilePath << ", reason: " << strerror(errno) << "\n";
			}
			if((fdErr = open(errFilePath.c_str(), O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR)) == -1) {
				std::cerr << "Unable to create stderr file " << errFilePath << ", reason: " << strerror(errno) << "\n";
			}
			if(dup2(fdOut, STDOUT_FILENO) == -1) { std::cerr << "Unable to redirect stdout, reason: " << strerror(errno) << "\n"; }
			if(dup2(fdErr, STDERR_FILENO) == -1) { std::cerr << "Unable to redirect stderr, reason: " << strerror(errno) << "\n"; }
			if(close(fdOut) == -1) { std::cerr << "Unable to close stdout file descriptor, reason: " << strerror(errno) << "\n"; }
			if(close(fdErr) == -1) { std::cerr << "Unable to close stderr file descriptor, reason: " << strerror(errno) << "\n"; }

			// navigate to execution directory if one is specified
			if(!execDir.empty()) { boost::filesystem::current_path(execDir); }

			if(execve(executableParam.c_str(), argumentsForExec.data(), environmentForExec.data()) == -1) {
				assert_fail() << "Unable to run executable " << executableParam << ", reason: " << strerror(errno) << "\n";
			}
		} else {
		#pragma omp critical(pids)
			TestRunner::getInstance().pids.push_back(pid);
			if(waitpid(pid, &retVal, 0) == -1) { std::cerr << "Unable to wait for child process " << pid << ", reason: " << strerror(errno) << "\n"; }
		}
		return retVal;
	}

	TestResult TestRunner::runCommand(const string& stepName, const TestSetup& setup, const PropertyView& testConfig, const string& cmd,
	                                  const string& producedFile, const string& execDir) const {
		vector<string> producedFiles;
		producedFiles.push_back(setup.stdOutFile);
		producedFiles.push_back(setup.stdErrFile);

		map<string, float> metricResults;
		// insert dummy vals
		metricResults["walltime"] = 0;
		metricResults["cputime"] = 0;
		metricResults["mem"] = 0;

		if(!producedFile.empty()) { producedFiles.push_back(producedFile); }

		string outfile = "";
		if(!setup.outputFile.empty()) {
			producedFiles.push_back(setup.outputFile);
			outfile = " -o " + setup.outputFile;
		}

		// setup possible environment vars
		std::stringstream env;
		{
			if(!testConfig.get<vector<string>>("libPaths").empty()) {
				// set LD_LIBRARY_PATH
				env << "LD_LIBRARY_PATH=";
				for(const auto& ldPath : testConfig.get<vector<string>>("libPaths")) {
					env << ldPath << ":";
				}
				env << "${LD_LIBRARY_PATH} ";
			}

			// set number of threads
			if(setup.numThreads) {
				env << "OMP_NUM_THREADS=" << setup.numThreads << " ";
				env << "IRT_NUM_WORKERS=" << setup.numThreads << " ";
			}

			// set scheduling policy
			if(setup.sched == STATIC) {
				env << "IRT_SCHED_POLICY=IRT_SCHED_POLICY_STATIC ";
				env << "IRT_LOOP_SCHED_POLICY=IRT_STATIC ";
				env << "OMP_SCHEDULE=STATIC ";
			} else if(setup.sched == DYNAMIC) {
				env << "IRT_SCHED_POLICY=IRT_SCHED_POLICY_STATIC ";
				env << "IRT_LOOP_SCHED_POLICY=IRT_DYNAMIC ";
				env << "OMP_SCHEDULE=DYNAMIC ";
			} else if(setup.sched == GUIDED) {
				env << "IRT_SCHED_POLICY=IRT_SCHED_POLICY_STATIC ";
				env << "IRT_LOOP_SCHED_POLICY=IRT_GUIDED ";
				env << "OMP_SCHEDULE=GUIDED ";
			}

			// append user defined custom environment variables
			if(!testConfig.get("env").empty()) {
				env << testConfig.get("env") << " ";
			}
		}

		// if it is a mock-run do nothing
		if(setup.mockRun) {
			return TestResult(TestResult::ResultType::SUCCESS, stepName, 0, metricResults, "", "",
			                  (execDir.empty() ? "" : "cd " + execDir + " && ") + env.str() + cmd + outfile);
		}

		string perfString("");
		vector<string> perfCodes;
		if(setup.perf) {
			// cache load misses
			perfCodes.push_back(setup.load_miss);

			// cache write misses
			perfCodes.push_back(setup.store_miss);

			// flops
			perfCodes.push_back(setup.flops);

			// additional requested metrics
			BOOST_FOREACH(string s, setup.perf_metrics) { perfCodes.push_back(s); }

			// build perf command
			perfString = "perf stat -x , ";
			BOOST_FOREACH(string s, perfCodes) { perfString = perfString + "-e " + s + " "; }
		}

		string executable = string(testConfig["time_executable"]);
		string envString = env.str();
		string argumentString = string(" -f WALLTIME%e\nCPUTIME%U\nMEM%M\n ") + perfString + cmd + outfile;

		// cpu time limit in seconds
		const unsigned cpuTimeLimit = 1200;

		int retVal = executeWithTimeout(executable, argumentString, envString, setup.stdOutFile, setup.stdErrFile, cpuTimeLimit, execDir);

		/*
		 * NOTE: Ordinarily, one would use WIFSIGNALED(int exitCode) to check whether a child process was terminated by a signal.
		 *
		 * However, since our child process executes /usr/bin/time, the information that a signal was received is hidden and the
		 * return/exit code of the client application + 128 is returned instead. As a result, we need to manually check for the
		 * signal received. Note that this can cause problems for applications that return higher exit codes (i.e. exit(9) and SIGKILL
		 * cannot be distinguished).
		 */

		int actualReturnCode = WEXITSTATUS(retVal);

		if(actualReturnCode > 128) {
			actualReturnCode -= 128;
			if(actualReturnCode > 0) { std::cerr << "Killed by signal " << actualReturnCode << "\n"; }
		}

		string output = readFile(setup.stdOutFile);
		string error = readFile(setup.stdErrFile);

		// get time, memory and perf values and remove them from stdError
		string stdErr;
		boost::char_separator<char> sep("\n");
		boost::tokenizer<boost::char_separator<char>> tok(error, sep);
		for(boost::tokenizer<boost::char_separator<char>>::iterator beg = tok.begin(); beg != tok.end(); ++beg) {
			string token(*beg);
			if(token.find("WALLTIME") != token.npos) {
				metricResults["walltime"] = atof(token.substr(token.find("WALLTIME") + 8).c_str());
			} else if(token.find("CPUTIME") != token.npos) {
				metricResults["cputime"] = atof(token.substr(token.find("CPUTIME") + 7).c_str());
				// check if we approached the cpu time limit. If so, print a warning
				if(((metricResults["cputime"])) / cpuTimeLimit > 0.95) {
					std::cerr << "Killed by timeout, CPU time was " << metricResults["cputime"] << ", limit was " << cpuTimeLimit << " seconds\n";
					metricResults["timeout"] = 1;
				}
			} else if(token.find("MEM") != token.npos) {
				metricResults["mem"] = atof(token.substr(token.find("MEM") + 3).c_str());
			} else {
				// check perf metrics, otherwise append to stderr
				bool found = false;
				for(auto code : perfCodes) {
					if(token.find(code) != token.npos) {
						string value = token.substr(0, token.find(","));
						float intVal;
						// try cast to int
						try {
							intVal = boost::lexical_cast<float>(value);
						} catch(const boost::bad_lexical_cast&) {
							// not counted or error
							intVal = -1;
						}

						// mark special perf metrics
						if(code.compare(setup.load_miss) == 0) {
							metricResults["load_miss"] = intVal;
						} else if(code.compare(setup.store_miss) == 0) {
							metricResults["store_miss"] = intVal;
						} else if(code.compare(setup.flops) == 0) {
							metricResults["flops"] = intVal;
						} else {
							metricResults[code] = intVal;
						}

						found = true;
						break;
					}
				}
				// no metric -> it is stdErr
				if(!found) { stdErr += token + "\n"; }
			}
		}

		// check whether execution has been aborted by the user
		if(actualReturnCode == SIGINT || actualReturnCode == SIGQUIT) { return TestResult::userAborted(stepName); }
		// produce regular result
		return TestResult(retVal == 0 ? TestResult::ResultType::SUCCESS : TestResult::ResultType::FAILURE, stepName, actualReturnCode, metricResults, output,
		                  stdErr, cmd, producedFiles, setup.numThreads, setup.sched);
	}


} // end namespace integration
} // end namespace driver
} // end namespace insieme
