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

#pragma once

#include <vector>
#include <set>
#include <string>
#include <functional>
#include <signal.h>

#include <boost/filesystem.hpp>
#include <boost/noncopyable.hpp>
#include <boost/operators.hpp>
#include <boost/optional.hpp>

#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_result.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace driver {
namespace integration {

	// a forward declaration of the step class
	struct TestStep;
	class TestRunner;

	// a function obtaining a list of available steps
	const std::set<TestStep>& getFullStepList();

	// gets a specific test step by name
	boost::optional<TestStep> getStepByName(const std::string& name);

	// ------------------------------------------------------------------------

	static const std::string TEST_STEP_REF_C_COMPILE = "ref_c_compile";
	static const std::string TEST_STEP_REF_CPP_COMPILE = "ref_cpp_compile";

	static const std::string TEST_STEP_REF_C_EXECUTE = "ref_c_execute";
	static const std::string TEST_STEP_REF_CPP_EXECUTE = "ref_cpp_execute";

	static const std::string TEST_STEP_REF_C_CHECK = "ref_c_check";
	static const std::string TEST_STEP_REF_CPP_CHECK = "ref_cpp_check";

	static const std::string TEST_STEP_INSIEMECC_C_SEMA = "insiemecc_c_sema";
	static const std::string TEST_STEP_INSIEMECC_CPP_SEMA = "insiemecc_cpp_sema";

	static const std::string TEST_STEP_INSIEMECC_SEQ_C_CONVERT = "insiemecc_seq_c_convert";
	static const std::string TEST_STEP_INSIEMECC_RUN_C_CONVERT = "insiemecc_run_c_convert";
	static const std::string TEST_STEP_INSIEMECC_OCL_C_CONVERT = "insiemecc_ocl_c_convert";
	static const std::string TEST_STEP_INSIEMECC_SEQ_CPP_CONVERT = "insiemecc_seq_cpp_convert";
	static const std::string TEST_STEP_INSIEMECC_RUN_CPP_CONVERT = "insiemecc_run_cpp_convert";

	static const std::string TEST_STEP_INSIEMECC_SEQ_C_COMPILE = "insiemecc_seq_c_compile";
	static const std::string TEST_STEP_INSIEMECC_RUN_C_COMPILE = "insiemecc_run_c_compile";
	static const std::string TEST_STEP_INSIEMECC_OCL_C_COMPILE = "insiemecc_ocl_c_compile";
	static const std::string TEST_STEP_INSIEMECC_SEQ_CPP_COMPILE = "insiemecc_seq_cpp_compile";
	static const std::string TEST_STEP_INSIEMECC_RUN_CPP_COMPILE = "insiemecc_run_cpp_compile";

	static const std::string TEST_STEP_INSIEMECC_SEQ_C_EXECUTE = "insiemecc_seq_c_execute";
	static const std::string TEST_STEP_INSIEMECC_RUN_C_EXECUTE = "insiemecc_run_c_execute";
	static const std::string TEST_STEP_INSIEMECC_OCL_C_EXECUTE = "insiemecc_ocl_c_execute";
	static const std::string TEST_STEP_INSIEMECC_SEQ_CPP_EXECUTE = "insiemecc_seq_cpp_execute";
	static const std::string TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE = "insiemecc_run_cpp_execute";

	static const std::string TEST_STEP_INSIEMECC_SEQ_C_CHECK = "insiemecc_seq_c_check";
	static const std::string TEST_STEP_INSIEMECC_RUN_C_CHECK = "insiemecc_run_c_check";
	static const std::string TEST_STEP_INSIEMECC_OCL_C_CHECK = "insiemecc_ocl_c_check";
	static const std::string TEST_STEP_INSIEMECC_SEQ_CPP_CHECK = "insiemecc_seq_cpp_check";
	static const std::string TEST_STEP_INSIEMECC_RUN_CPP_CHECK = "insiemecc_run_cpp_check";

	static const std::string TEST_STEP_PREPROCESSING = "preprocessing";
	static const std::string TEST_STEP_POSTPROCESSING = "postprocessing";

	static const std::string TEST_STEP_CHECK_PREREQUISITES = "check_prerequisites";

	// ------------------------------------------------------------------------


	struct TestSetup {
		bool mockRun;
		bool clean;
		int numThreads;
		std::string stdOutFile;
		std::string stdErrFile;
		std::string outputFile;
	};


	struct TestStep : public boost::less_than_comparable<TestStep>, public boost::equality_comparable<TestStep>, public insieme::utils::Printable {

		using StepOp = std::function<TestResult(TestSetup, const IntegrationTestCase& test, const TestRunner& runner)>;

	  private:
		unsigned id;

		std::string name;

		StepOp step;

		std::set<std::string> dependencies;

	  public:
		TestStep(const std::string& name, const StepOp& op, const std::set<std::string>& dependencies = std::set<std::string>())
		    : name(name), step(op), dependencies(dependencies) {
			static unsigned idCounter = 0;
			id = idCounter++;
		}

		const std::string& getName() const {
			return name;
		}

		TestResult run(const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner) const {
			return step(setup, test, runner);
		}

		const std::set<std::string>& getDependencies() const {
			return dependencies;
		}

		bool operator==(const TestStep& other) const {
			return name == other.name;
		}

		bool operator<(const TestStep& other) const {
			return id < other.id;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << name;
		}
	};


	// define the TestRunner here
	class TestRunner {
	  private:
		mutable std::vector<pid_t> pids;
		std::function<void()> func;
		std::function<void()> pre;
		TestRunner() {
			// register signal handler
			signal(SIGINT, TestRunner::signalHandler);
		}
		TestRunner(const TestRunner&);
		TestRunner& operator=(const TestRunner&);

	  public:
		static TestRunner& getInstance() {
			static TestRunner r = TestRunner();
			return r;
		}
		int executeWithTimeout(const string& executableParam, const string& argumentsParam, const string& environmentParam, const string& outFilePath,
		                       const string& errFilePath, unsigned cpuTimeLimit, const boost::filesystem::path& execDir = "") const;
		TestResult runCommand(const string& stepName, const TestSetup& setup, const PropertyView& testConfig, const string& cmd,
		                      const boost::filesystem::path& execDir = "", const string& producedFile = "") const;
		void attachExecuteOnKill(std::function<void()> f) {
			func = f;
		}
		void attachExecuteBeforeKill(std::function<void()> f) {
			pre = f;
		}

		static void signalHandler(int signum) {
		#pragma omp single
			{
				TestRunner::getInstance().pre();
				sleep(1);
				#pragma omp critical(pids)
				{
					for(auto pid : TestRunner::getInstance().pids) {
						kill(pid, SIGINT);
					}
					sleep(3);
					TestRunner::getInstance().func();
				}
			}
			exit(0);
		}
	};

} // end namespace integration
} // end namespace driver
} // end namespace insieme
