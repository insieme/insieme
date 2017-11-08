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

		TestStep createRefCompilationStep(const string& name, Language l) {
			return TestStep(name, [=](TestSetup setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				auto props = test.getPropertiesFor(name);

				// start with executable
				cmd << test.getCompilerString(name, l == Language::CPP, true);

				// add input files
				for(const auto& cur : test.getFiles()) {
					cmd << " " << cur.string();
				}

				// append all the arguments
				cmd << " " << join(" ", test.getCompilerArguments(name, l == Language::CPP, true, true));

				string outputDir = test.getOutputDirectory().string();
				setup.outputFile = outputDir + "/" + test.getBaseName() + ".ref";
				setup.stdOutFile = outputDir + "/" + test.getBaseName() + "." + name + ".out";
				setup.stdErrFile = outputDir + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, setup, props, cmd.str(), test.getOutputDirectory());
			}, std::set<std::string>());
		}

		TestStep createRefExecuteStep(const string& name, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](TestSetup setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				auto props = test.getPropertiesFor(name);

				string outputDir = test.getOutputDirectory().string();

				// start with executable
				cmd << outputDir << "/" << test.getBaseName() << ".ref";

				// add arguments
				cmd << " " << props["executionFlags"];

				setup.stdOutFile = outputDir + "/" + test.getBaseName() + "." + name + ".out";
				setup.stdErrFile = outputDir + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, setup, props, cmd.str(), test.getOutputDirectory());
			}, deps);
		}

		TestStep createBashCommandStep(const string& name, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](TestSetup setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				auto props = test.getPropertiesFor(name);

				if(props[name].empty()) return TestResult::stepOmitted(name);

				// start with executable
				cmd << props[name];

				string outputDir = test.getOutputDirectory().string();
				setup.stdOutFile = outputDir + "/" + test.getBaseName() + "." + name + ".out";
				setup.stdErrFile = outputDir + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, setup, props, cmd.str(), test.getOutputDirectory());
			}, deps);
		}

		TestStep createInsiemeccSemaStep(const string& name, Language l, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](TestSetup setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
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

				string outputDir = test.getOutputDirectory().string();

				// also dump IR
				std::string irFile = outputDir + "/" + test.getBaseName() + ".ir";
				cmd << " --dump-ir " << irFile;

				setup.stdOutFile = outputDir + "/" + test.getBaseName() + "." + name + ".out";
				setup.stdErrFile = outputDir + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, setup, props, cmd.str(), test.getOutputDirectory(), irFile);
			}, deps);
		}

		TestStep createInsiemeccConversionStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](TestSetup setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
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

				// determine backend
				string be = getBackendKey(backend);
				cmd << " --backend " << be;

				string outputDir = test.getOutputDirectory().string();

				// source-to-source compilation only
				setup.outputFile = outputDir + "/" + test.getBaseName() + ".insieme." + be + "." + getExtension(l);
				cmd << " --dump-trg-only " << setup.outputFile;

				setup.stdOutFile = outputDir + "/" + test.getBaseName() + "." + name + ".out";
				setup.stdErrFile = outputDir + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, setup, props, cmd.str(), test.getOutputDirectory());
			}, deps);
		}

		TestStep createInsiemeccCompilationStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](TestSetup setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				auto props = test.getPropertiesFor(name);

				// start with executable
				cmd << test.getCompilerString(name, l == Language::CPP, true);

				// determine backend
				string be = getBackendKey(backend);

				string outputDir = test.getOutputDirectory().string();

				// add input file
				cmd << " " << outputDir << "/" << test.getBaseName() << ".insieme." << be << "." << getExtension(l);

				// append all the arguments
				cmd << " " << join(" ", test.getCompilerArguments(name, l == Language::CPP, true, true));

				// add runtime include directories
				if(backend != Sequential) { // TODO: make this non-hardcoded -- it is ugly, but I don't have the time ...
					cmd << " -I " << utils::getInsiemeSourceRootDir() << "runtime/include";
					cmd << " -I " << utils::getInsiemeSourceRootDir() << "common/include";
				}

				setup.outputFile = outputDir + "/" + test.getBaseName() + ".insieme." + be;
				setup.stdOutFile = outputDir + "/" + test.getBaseName() + "." + name + ".out";
				setup.stdErrFile = outputDir + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, setup, props, cmd.str(), test.getOutputDirectory());
			}, deps);
		}

		TestStep createInsiemeccExecuteStep(const string& name, Backend backend, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](TestSetup setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				auto props = test.getPropertiesFor(name);

				string outputDir = test.getOutputDirectory().string();

				// the log file to delete afterwards
				std::string logFile = "";
				if (backend == Runtime) {
					logFile = outputDir + "/insieme_runtime.log";
				}

				// determine backend
				string be = getBackendKey(backend);

				// start with executable
				cmd << outputDir << "/" << test.getBaseName() << ".insieme." << be;

				// add arguments
				cmd << " " << props["executionFlags"];

				setup.stdOutFile = outputDir + "/" + test.getBaseName() + "." + name + ".out";
				setup.stdErrFile = outputDir + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, setup, props, cmd.str(), test.getOutputDirectory(), logFile);
			}, deps);
		}

		TestStep createInsiemeccCheckStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
			return TestStep(name, [=](TestSetup setup, const IntegrationTestCase& test, const TestRunner& runner) -> TestResult {
				std::stringstream cmd;
				auto props = test.getPropertiesFor(name);

				std::string langstr("_c_");
				if(l == CPP) langstr = string("_cpp_");

				// define comparison script
				cmd << props["compareOutputScript"];

				string outputDir = test.getOutputDirectory().string();

				// start with executable
				cmd << " " << outputDir << "/" << test.getBaseName() << ".ref" + langstr + "execute.out";

				// determine backend
				string be = getBackendKey(backend);

				// pipe result to output file
				cmd << " " << outputDir << "/" << test.getBaseName() << ".insiemecc_" + be + langstr + "execute.out";

				// add awk pattern
				// TODO: generally remove outer quotation marks in properties if present - I don't have the time now but it needs to be done at some point
				string outputAwk = props["outputAwk"]; //.substr(props["outputAwk"].find("\"")+1, props["outputAwk"].rfind("\"")-1);
				cmd << " \"" << outputAwk << "\"";

				setup.stdOutFile = outputDir + "/" + test.getBaseName() + "." + name + ".out";
				setup.stdErrFile = outputDir + "/" + test.getBaseName() + "." + name + ".err.out";

				// run it
				return runner.runCommand(name, setup, props, cmd.str(), test.getOutputDirectory());
			}, deps);
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

		std::set<TestStep> createFullStepList() {
			std::set<TestStep> list;

			auto add = [&](const TestStep& step) { list.insert(step); };

			// Note that we have to create the steps here in the order they should (and can) be executed. The creation order will set their id which will be used for comparison

			// the check for the prerequisites is executed before all other steps
			add(createBashCommandStep(TEST_STEP_CHECK_PREREQUISITES));

			// C steps --------------
			// sema
			add(createInsiemeccSemaStep(TEST_STEP_INSIEMECC_C_SEMA, C));
			// seq
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_SEQ_C_CONVERT, Sequential, C));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_SEQ_C_COMPILE, Sequential, C, {TEST_STEP_INSIEMECC_SEQ_C_CONVERT}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_SEQ_C_EXECUTE, Sequential, {TEST_STEP_INSIEMECC_SEQ_C_COMPILE}));
			// run
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_RUN_C_CONVERT, Runtime, C));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_RUN_C_COMPILE, Runtime, C, {TEST_STEP_INSIEMECC_RUN_C_CONVERT}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_C_EXECUTE, Runtime, {TEST_STEP_INSIEMECC_RUN_C_COMPILE}));
			// ocl
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_OCL_C_CONVERT, Opencl, C));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_OCL_C_COMPILE, Opencl, C, {TEST_STEP_INSIEMECC_OCL_C_CONVERT}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_OCL_C_EXECUTE, Opencl, {TEST_STEP_INSIEMECC_OCL_C_COMPILE}));
			// ref
			add(createRefCompilationStep(TEST_STEP_REF_C_COMPILE, C));
			add(createRefExecuteStep(TEST_STEP_REF_C_EXECUTE, {TEST_STEP_REF_C_COMPILE}));
			// check
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_SEQ_C_CHECK, Sequential, C, {TEST_STEP_INSIEMECC_SEQ_C_EXECUTE, TEST_STEP_REF_C_EXECUTE}));
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_C_CHECK, Runtime, C, {TEST_STEP_INSIEMECC_RUN_C_EXECUTE, TEST_STEP_REF_C_EXECUTE}));
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_OCL_C_CHECK, Opencl, C, {TEST_STEP_INSIEMECC_OCL_C_EXECUTE, TEST_STEP_REF_C_EXECUTE}));

			// CPP steps --------------
			// sema
			add(createInsiemeccSemaStep(TEST_STEP_INSIEMECC_CPP_SEMA, CPP));
			// seq
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_SEQ_CPP_CONVERT, Sequential, CPP));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_SEQ_CPP_COMPILE, Sequential, CPP, {TEST_STEP_INSIEMECC_SEQ_CPP_CONVERT}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_SEQ_CPP_EXECUTE, Sequential, {TEST_STEP_INSIEMECC_SEQ_CPP_COMPILE}));
			// run
			add(createInsiemeccConversionStep(TEST_STEP_INSIEMECC_RUN_CPP_CONVERT, Runtime, CPP));
			add(createInsiemeccCompilationStep(TEST_STEP_INSIEMECC_RUN_CPP_COMPILE, Runtime, CPP, {TEST_STEP_INSIEMECC_RUN_CPP_CONVERT}));
			add(createInsiemeccExecuteStep(TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE, Runtime, {TEST_STEP_INSIEMECC_RUN_CPP_COMPILE}));
			// ref
			add(createRefCompilationStep(TEST_STEP_REF_CPP_COMPILE, CPP));
			add(createRefExecuteStep(TEST_STEP_REF_CPP_EXECUTE, {TEST_STEP_REF_CPP_COMPILE}));
			// check
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_SEQ_CPP_CHECK, Sequential, CPP, {TEST_STEP_INSIEMECC_SEQ_CPP_EXECUTE, TEST_STEP_REF_CPP_EXECUTE}));
			add(createInsiemeccCheckStep(TEST_STEP_INSIEMECC_RUN_CPP_CHECK, Runtime, CPP, {TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE, TEST_STEP_REF_CPP_EXECUTE}));

			// preprocessing steps are executed by a special run of the integration test driver - as are postprocessing steps
			// these steps are excluded from normal runs
			// see "scheduleSteps" function
			add(createBashCommandStep(TEST_STEP_PREPROCESSING));
			add(createBashCommandStep(TEST_STEP_POSTPROCESSING));

			return list;
		}
	}

	const std::set<TestStep>& getFullStepList() {
		const static auto list = createFullStepList();
		return list;
	}

	boost::optional<TestStep> getStepByName(const std::string& name) {
		for(const auto& step : getFullStepList()) {
			if(step.getName() == name) {
				return step;
			}
		}
		return {};
	}

	/*
	 *  Test Runner member functions
	 */
	int TestRunner::executeWithTimeout(const string& executableParam, const string& argumentsParam, const string& environmentParam, const string& outFilePath,
	                                   const string& errFilePath, unsigned cpuTimeLimit, const fs::path& execDir) const {
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

			boost::filesystem::current_path(execDir);

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
	                                  const fs::path& execDir, const string& producedFile) const {
		vector<string> producedFiles;
		producedFiles.push_back(setup.stdOutFile);
		producedFiles.push_back(setup.stdErrFile);

		// insert dummy vals
		float walltime = 0;
		float mem = 0;

		if(!producedFile.empty()) { producedFiles.push_back(producedFile); }

		string outfile = "";
		if(!setup.outputFile.empty()) {
			producedFiles.push_back(setup.outputFile);
			outfile = " -o " + setup.outputFile;
		}

		if(!execDir.empty())
			fs::create_directories(execDir);

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

			// append user defined custom environment variables
			if(!testConfig.get("env").empty()) {
				env << testConfig.get("env") << " ";
			}
		}

		// if it is a mock-run do nothing
		if(setup.mockRun) {
			return TestResult(TestResult::ResultType::SUCCESS, stepName, 0, 0, 0, "", "",
			                  (execDir.empty() ? "" : "cd " + execDir.string() + " && ") + env.str() + cmd + outfile);
		}

		string executable = string(testConfig["time_executable"]);
		string envString = env.str();
		string argumentString = string(" -f WALLTIME%e\nCPUTIME%U\nMEM%M\n ") + cmd + outfile;

		// cpu time limit in seconds
		unsigned cpuTimeLimit = 1200;
		if(!testConfig.get("cpuTimeLimit").empty()) {
			cpuTimeLimit = atoi(testConfig.get("cpuTimeLimit").c_str());
		}

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
				walltime = atof(token.substr(token.find("WALLTIME") + 8).c_str());
			} else if(token.find("CPUTIME") != token.npos) {
				float cputime = atof(token.substr(token.find("CPUTIME") + 7).c_str());
				// check if we approached the cpu time limit. If so, print a warning
				if(cputime / cpuTimeLimit > 0.95) {
					std::cerr << "Killed by timeout, CPU time was " << cputime << ", limit was " << cpuTimeLimit << " seconds\n";
				}
			} else if(token.find("MEM") != token.npos) {
				mem = atof(token.substr(token.find("MEM") + 3).c_str());
			} else {
				// no metric -> it is stdErr
				stdErr += token + "\n";
			}
		}

		// check whether execution has been aborted by the user
		if(actualReturnCode == SIGINT || actualReturnCode == SIGQUIT) { return TestResult::userAborted(stepName); }
		// produce regular result
		return TestResult(retVal == 0 ? TestResult::ResultType::SUCCESS : TestResult::ResultType::FAILURE, stepName, actualReturnCode, walltime, mem, output,
		                  stdErr, cmd, producedFiles);
	}


} // end namespace integration
} // end namespace driver
} // end namespace insieme
