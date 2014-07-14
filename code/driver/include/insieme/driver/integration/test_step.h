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

#pragma once

#include <map>
#include <set>
#include <string>
#include <functional>

#include <boost/noncopyable.hpp>
#include <boost/operators.hpp>

#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_result.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace driver {
namespace integration {

	// a forward declaration of the step class
	struct TestStep;
	class TestRunner;

	// a function obtaining an index of available steps
	const std::map<std::string,TestStep>& getFullStepList(int statThreads,bool scheduling);
	const std::map<std::string,TestStep>& getFullStepList();

	// filters out test steps that are not suitable for the given tests
	vector<TestStep> filterSteps(const vector<TestStep>& steps, const IntegrationTestCase& test);

	// schedules the list of test steps by adding dependent steps and fixing the order properly
	vector<TestStep> scheduleSteps(const vector<TestStep>& steps, const IntegrationTestCase& test, int numThreads=0, bool scheduling=false);

	//reads out a given file and returns the contents
	std::string readFile(std::string filename);

	// ------------------------------------------------------------------------



	struct TestSetup {
		bool mockRun;
		SchedulingPolicy sched;
		bool clean;
		int numThreads;
		std::string stdOutFile;
		std::string stdErrFile;
		std::string outputFile;

		//perf metrics
		bool perf;
		string load_miss;
		string store_miss;
		string flops;
		vector<string> perf_metrics;
	};

	enum StepType {COMPILE,RUN,CHECK,STATIC_METRIC,UNDEFINED};

	struct TestStep :
			public boost::less_than_comparable<TestStep>,
			public boost::equality_comparable<TestStep>,
			public insieme::utils::Printable
	{

		typedef std::function<TestResult(const TestSetup&, const IntegrationTestCase& test, const TestRunner& runner)> StepOp;

	protected:
		friend class boost::serialization::access;
		template<class Archive>
		void serialize(Archive & ar, const unsigned int version) {
			ar & name;
			//ar & step;
			ar & dependencies;
			ar & type;
		}

	public:

		std::string name;

		StepOp step;

		std::set<std::string> dependencies;

		StepType type;
	public:

		TestStep() {};

		TestStep(const std::string& name, const StepOp& op, const std::set<std::string>& dependencies = std::set<std::string>(),
				StepType type=UNDEFINED)
			: name(name), step(op), dependencies(dependencies),type(type) {}

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
			return name < other.name;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << name;
		}

		const StepType getStepType(){
			return type;
		}
	};

	//special test step only to contain results of static metrics
	struct StaticMetricsStep : public TestStep{
		public:
		StaticMetricsStep(){type=STATIC_METRIC;};
	};



    //define the TestRunner here
    class TestRunner {
    private:
        typedef insieme::driver::integration::IntegrationTestCase TestCase;
        mutable std::vector<pid_t> pids;
        std::function<void()> func;
        std::function<void()> pre;
        TestRunner() {
            //register signal handler
            signal(SIGINT, TestRunner::signalHandler);
        }
        TestRunner(const TestRunner&);
        TestRunner& operator =(const TestRunner&);
    public:
        static TestRunner& getInstance() {
            static TestRunner r = TestRunner();
            return r;
        }
        int executeWithTimeout(const string& executableParam, const string& argumentsParam,
                               const string& environmentParam, const string& outFilePath,
                               const string& errFilePath, unsigned cpuTimeLimit) const;
        TestResult runCommand(const string& stepName, const TestSetup& setup, const PropertyView& testConfig,
                              const string& cmd, const string& producedFile="") const;
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
                #pragma omp critical (pids)
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
