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

#include "insieme/driver/integration/test_step.h"

#include <sstream>

#include "insieme/utils/assert.h"
#include "insieme/utils/config.h"

namespace insieme {
namespace driver {
namespace integration {

	namespace {

		namespace {

			TestResult runCommand(const TestSetup& setup, const string& cmd) {
				// if it is a mock-run do nothing
				if (setup.mockRun) {
					std::cout << cmd << "\n";
					return TestResult();
				}

				// run command - //TODO: measure time
				auto res = system(cmd.c_str());		// TODO: record output of executable and store it in the result
				return TestResult(res == 0);
			}

			typedef std::set<std::string> Dependencies;

			enum Backend {
				Sequential, Runtime
			};

			enum Language {
				C, CPP
			};

			string getExtension(Language ext) {
				switch(ext) {
				case C:   return "c";
				case CPP: return "cpp";
				}
				return "xxx";
			}

			string getBackendKey(Backend be) {
				switch(be) {
				case Sequential:   	return "seq";
				case Runtime: 		return "run";
				}
				return "xxx";
			}


			TestStep createRefCompStep(const string& name, Language l) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;

					// start with executable
					switch(l) {
					case C:   cmd << props["global.ref.cc"];  break;
					case CPP: cmd << props["global.ref.cxx"]; break;
					}

					// standard flags
					cmd << " -O3";
					cmd << " -fshow-column";
					cmd << " -Wall";
					cmd << " -Wl,--no-as-needed";
					cmd << " -pipe";

					// add language flag
					switch(l) {
					case C:   cmd << " --std=c99"; break;
					case CPP: cmd << " --std=c++03"; break;
					}


					// add include directories
					if (!test.getIncludeDirs().empty()) cmd << " -I " << join(":",test.getIncludeDirs());

					// add input files
					for(const auto& cur : test.getFiles()) {
						cmd << " " << cur.string();
					}

					// add output file
					cmd << " -o " << test.getDirectory().string() << "/" << test.getName() << ".ref";

					// run it
					return runCommand(setup, cmd.str());
				});
			}

			TestStep createRefRunStep(const string& name, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					std::stringstream cmd;

					// start with executable
					cmd << test.getDirectory().string() << "/" << test.getName() << ".ref";

					// TODO: add arguments

					// pipe result to output file
					cmd << " > " << test.getDirectory().string() << "/" << test.getName() << ".ref.out";

					// run it
					return runCommand(setup, cmd.str());
				}, deps);
			}


			TestStep createMainSemaStep(const string& name, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;

					// start with executable
					cmd << props["global.insieme.main"];

					// add language flag
					switch(l) {
					case C:   cmd << " --std=c99"; break;
					case CPP: cmd << " --std=c++03"; break;
					}

					// add always-on flags
					cmd << " --col-wrap=120";
					cmd << " --show-line-no";
					cmd << " --log-level=INFO";		// TODO: this could be a config issue

					// enable semantic tests
					cmd << " -S";

					// also dump IR
					cmd << " --dump-ir " << test.getDirectory().string() << "/" << test.getName() << ".ir";

					// add include directories
					if (!test.getIncludeDirs().empty()) cmd << " -I " << join(":",test.getIncludeDirs());

					// add input files
					for(const auto& cur : test.getFiles()) {
						cmd << " " << cur.string();
					}

					// run it
					return runCommand(setup, cmd.str());
				}, deps);
			}

			TestStep createMainConversionStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;

					// start with executable
					cmd << props["global.insieme.main"];

					// add language flag
					switch(l) {
					case C:   cmd << " --std=c99"; break;
					case CPP: cmd << " --std=c++03"; break;
					}

					// add always-on flags
					cmd << " --col-wrap=120";
					cmd << " --show-line-no";
					cmd << " --log-level=INFO";		// TODO: this could be a config issue

					// determine backend
					string be = getBackendKey(backend);
					cmd << " -b " << be;

					// add include directories
					if (!test.getIncludeDirs().empty()) cmd << " -I " << join(":",test.getIncludeDirs());

					// add input files
					for(const auto& cur : test.getFiles()) {
						cmd << " " << cur.string();
					}

					// add output file
					cmd << " -o " << test.getDirectory().string() << "/" << test.getName() << ".insieme." << be << "." << getExtension(l);

					// run it
					return runCommand(setup, cmd.str());
				}, deps);
			}

			TestStep createMainCompilationStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;

					// start with executable
					switch(l) {
					case C:   cmd << props["global.ref.cc"];  break;
					case CPP: cmd << props["global.ref.cxx"]; break;
					}

					// standard flags
					cmd << " -O3";
					cmd << " -fshow-column";
					cmd << " -Wall";
					cmd << " -Wl,--no-as-needed";
					cmd << " -pipe";

					// add language flag
					switch(l) {
					case C:   cmd << " --std=c99"; break;
					case CPP: cmd << " --std=c++03"; break;
					}

					// determine backend
					string be = getBackendKey(backend);

					// add flags for the runtime code
					if (backend == Runtime) {
						cmd << " " << props["global.run.comp.flags"] << " ";
					}

					// add include directories
					if (!test.getIncludeDirs().empty()) cmd << " -I " << join(":",test.getIncludeDirs());

					// add input file
					cmd << " " << test.getDirectory().string() << "/" << test.getName() << ".insieme." << be << "." << getExtension(l);

					// add output file
					cmd << " -o " << test.getDirectory().string() << "/" << test.getName() << ".insieme." << be << ".test";

					// run it
					return runCommand(setup, cmd.str());
				}, deps);
			}

			TestStep createMainExecuteStep(const string& name, Backend backend, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					std::stringstream cmd;

					// determine backend
					string be = getBackendKey(backend);

					// start with executable
					cmd << test.getDirectory().string() << "/" << test.getName() << ".insieme." << be << ".test";

					// TODO: add arguments

					// pipe result to output file
					cmd << " > " << test.getDirectory().string() << "/" << test.getName() << ".insieme." << be << ".test.out";

					// run it
					return runCommand(setup, cmd.str());
				}, deps);
			}

			TestStep createMainCheckStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;

					// define comparison script
					cmd << props["global.sortdiff"];

					// determine backend
					string be = getBackendKey(backend);

					// start with executable
					cmd << " " << test.getDirectory().string() << "/" << test.getName() << ".ref.out";

					// pipe result to output file
					cmd << " " << test.getDirectory().string() << "/" << test.getName() << ".insieme." << be << ".test.out";

					// run it
					return runCommand(setup, cmd.str());
				}, deps);
			}


		}


		std::map<std::string,TestStep> createFullStepList() {

			std::map<std::string,TestStep> list;

			auto add = [&](const TestStep& step) {
				list.insert({step.getName(), step});
			};

			// --- real steps ----

			add(createRefCompStep("ref_c_comp", C));
			add(createRefCompStep("ref_c++_comp", CPP));

			add(createRefRunStep("ref_c_exec", { "ref_c_comp" }));
			add(createRefRunStep("ref_c++_exec", { "ref_c++_comp" }));

			add(createMainSemaStep("main_c_sema", C));
			add(createMainSemaStep("main_cxx_sema", C));

			add(createMainConversionStep("main_seq_conv", Sequential, C));
			add(createMainConversionStep("main_run_conv", Runtime, C));

			add(createMainCompilationStep("main_seq_comp", Sequential, C, { "main_seq_conv" }));
			add(createMainCompilationStep("main_run_comp", Runtime, C, { "main_run_conv" }));

			add(createMainExecuteStep("main_seq_exec", Sequential, { "main_seq_comp" }));
			add(createMainExecuteStep("main_run_exec", Runtime, { "main_run_comp" }));

			add(createMainCheckStep("main_seq_check", Sequential, C, { "main_seq_exec", "ref_c_exec" }));
			add(createMainCheckStep("main_run_check", Runtime, C, { "main_run_exec", "ref_c_exec" }));

			return list;
		}

	}


	// a function obtaining an index of available steps
	const std::map<std::string,TestStep>& getFullStepList() {
		const static std::map<std::string,TestStep> list = createFullStepList();
		return list;
	}

	const TestStep& getStepByName(const std::string& name) {
		static const TestStep fail;

		const auto& list = getFullStepList();
		auto pos = list.find(name);
		if (pos != list.end()) {
			return pos->second;
		}
		assert_fail() << "Requested unknown step: " << name;
		return fail;
	}

	vector<TestStep> filterSteps(const vector<TestStep>& steps, const IntegrationTestCase& test) {
		return steps;
	}

	namespace {

		void scheduleStep(const TestStep& step, vector<TestStep>& res) {

			// check whether test is already present
			if (contains(res, step)) return;

			// check that all dependencies are present
			for(const auto& cur : step.getDependencies()) {
				scheduleStep(getStepByName(cur), res);
			}

			// append step to schedule
			res.push_back(step);
		}

	}


	vector<TestStep> scheduleSteps(const vector<TestStep>& steps) {
		vector<TestStep> res;
		for(const auto& cur : steps) {
			scheduleStep(cur, res);
		}
		return res;
	}


} // end namespace integration
} // end namespace driver
} // end namespace insieme
