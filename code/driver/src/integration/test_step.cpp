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

#include <boost/foreach.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
#include <boost/tokenizer.hpp>

namespace insieme {
namespace driver {
namespace integration {

	namespace {

		namespace {

			namespace fs = boost::filesystem;

			typedef std::set<std::string> Dependencies;

			enum Backend {
				Sequential, Runtime, Opencl
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
				case Opencl: 		return "ocl";
				}
				return "xxx";
			}


			//TODO MAKE FLAGS STEP SPECIFIC

			TestStep createRefCompStep(const string& name, Language l) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;
					TestSetup set=setup;

					// start with executable
					cmd<<props["compiler"];

					// add include directories
					for(const auto& cur : test.getIncludeDirs()) {
						cmd << " -I" << cur.string();
					}

					// add external lib dirs
					for(const auto& cur : test.getLibDirs()) {
						cmd <<" -L" << cur.string();
					}

					// add external libs
					for(const auto& cur : test.getLibNames()) {
						cmd <<" -l" << cur;
					}

					// disable multithreading
					set.numThreads=0;

					// add input files
					for(const auto& cur : test.getFiles()) {
						cmd << " " << cur.string();
					}

					std::vector<string> flags=test.getCompilerArguments(name);
					// get all flags defined by properties
					for (string s: flags){
						cmd << " " << s;
					}

					//get definitions
					for_each(test.getDefinitions(name), [&](const std::pair<string,string>& def) {
						cmd << " -D" << def.first << "=" << def.second;
					});
					cmd << " ";

					string executionDirectory=test.getDirectory().string();
					if(!set.executionDir.empty())
						executionDirectory=set.executionDir;
					
					// set output file, stdOutFile and stdErrFile
					set.outputFile=executionDirectory+"/"+test.getBaseName()+".ref";
					set.stdOutFile=executionDirectory+"/"+test.getBaseName()+"."+name+".out";
					set.stdErrFile=executionDirectory+"/"+test.getBaseName()+"."+name+".err.out";

					// run it
					return runner.runCommand(name, set, props, cmd.str());
				},std::set<std::string>(),COMPILE);
			}

			TestStep createRefRunStep(const string& name, const Dependencies& deps = Dependencies(), int numThreads=0) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner)->TestResult {
					std::stringstream cmd;
					TestSetup set=setup;
					auto props = test.getPropertiesFor(name);
					
					// get execution directory	
					string executionDirectory=test.getDirectory().string();
					if(!set.executionDir.empty())
						executionDirectory=set.executionDir;
					

					// start with executable
					cmd << executionDirectory << "/" << test.getBaseName() << ".ref";

					// add arguments
					cmd << " " << props["executionFlags"];
				
					// set output files
					set.stdOutFile=executionDirectory+"/"+test.getBaseName()+"."+name+".out";
					set.stdErrFile=executionDirectory+"/"+test.getBaseName()+"."+name+".err.out";

					// set number of threads
					set.numThreads=numThreads;

					// run it
					return runner.runCommand(name, set, props, cmd.str(), "", executionDirectory);
				}, deps,RUN);
			}

			TestStep createBashCommandStep(const string& name, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner)->TestResult {
					std::stringstream cmd;
					TestSetup set=setup;
					auto props = test.getPropertiesFor(name);
					string executionDirectory;

					if (props[name].empty())
						return TestResult::stepOmitted(name);

					// get execution directory
					executionDirectory=test.getDirectory().string();
					if(!set.executionDir.empty())
						executionDirectory = set.executionDir;

					// start with executable
					cmd << props[name];

					// set output files
					set.stdOutFile=executionDirectory+"/"+test.getBaseName()+"."+name+".out";
					set.stdErrFile=executionDirectory+"/"+test.getBaseName()+"."+name+".err.out";

					// set number of threads
					set.numThreads=0;

					// run it
					return runner.runCommand(name, set, props, cmd.str(), "", executionDirectory);
				}, deps);
			}

			TestStep createInsiemeccSemaStep(const string& name, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;
					TestSetup set=setup;

					// get execution dir
					string executionDirectory=test.getDirectory().string();
					if(!set.executionDir.empty())
						executionDirectory=set.executionDir;

					// start with executable
					cmd << props["compiler"];

					// enable semantic tests
					cmd << " --check-sema-only";

					// also dump IR
					std::string irFile = executionDirectory + "/" + test.getBaseName() + ".ir";
					cmd << " --dump-ir " << irFile;

					// add include directories
					for(const auto& cur : test.getIncludeDirs()) {
						cmd << " -I" << cur.string();
					}

					// add input files
					for(const auto& cur : test.getFiles()) {
						cmd << " " << cur.string();
					}

					// disable multithreading
					set.numThreads=0;

					std::vector<string> flags=test.getCompilerArguments(name);
					// get all flags defined by properties
					for (string s: flags){
						cmd << " " <<s;
					}

					//get definitions
					for_each(test.getDefinitions(name), [&](const std::pair<string,string>& def) {
						cmd << " -D" << def.first << "=" << def.second;
					});

					//append intercept patterns
					for(const auto& cur : test.getInterceptedNameSpaces()) {
						cmd << " --intercept " << cur;
					}
					//append intercepted header file dirs
					for(const auto& cur : test.getInterceptedHeaderFileDirectories()) {
						cmd << " --intercept-include " << cur.string();
					}
					cmd<<" ";

					set.stdOutFile=executionDirectory+"/"+test.getBaseName()+"."+name+".out";
					set.stdErrFile=executionDirectory+"/"+test.getBaseName()+"."+name+".err.out";

					// run it
					return runner.runCommand(name, set, props, cmd.str(),irFile);
//					return runner.runCommand(name, set, props, cmd.str(),set.stdOutFile);
				}, deps,COMPILE);
			}

			TestStep createInsiemeccConversionStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;
					TestSetup set=setup;

					// start with executable
					cmd << props["compiler"];

					string executionDirectory=test.getDirectory().string();
					if(!set.executionDir.empty())
						executionDirectory=set.executionDir;

					// determine backend
					string be = getBackendKey(backend);

					// source-to-source compilation only
					set.outputFile=executionDirectory+"/"+test.getBaseName()+".insieme."+be+"."+getExtension(l);
					cmd << " --dump-trg-only " << set.outputFile;

					cmd << " --backend " << be;

					// add include directories
					for(const auto& cur : test.getIncludeDirs()) {
						cmd << " -I" << cur.string();
					}

					// add input files
					for(const auto& cur : test.getFiles()) {
						cmd << " " << cur.string();
					}

					std::vector<string> flags=test.getCompilerArguments(name);
					// get all flags defined by properties
					for (string s: flags){
						cmd << " " << s;
					}

					// disable multithreading
					set.numThreads=0;

					//get definitions
					for_each(test.getDefinitions(name), [&](const std::pair<string,string>& def) {
						cmd << " -D" << def.first << "=" << def.second;
					});

					//append intercept patterns
					for(const auto& cur : test.getInterceptedNameSpaces()) {
						cmd << " --intercept " << cur;
					}
					//append intercepted header file dirs
					for(const auto& cur : test.getInterceptedHeaderFileDirectories()) {
						cmd << " --intercept-include " << cur.string();
					}
					cmd << " ";

	
					// set stdOut file and stdErr file
					set.stdOutFile=executionDirectory+"/"+test.getBaseName()+"."+name+".out";
					set.stdErrFile=executionDirectory+"/"+test.getBaseName()+"."+name+".err.out";

					// run it
					return runner.runCommand(name, set, props, cmd.str());
				}, deps,COMPILE);
			}

			TestStep createInsiemeccCompilationStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;
					TestSetup set=setup;

					// get execution dir
					string executionDirectory=test.getDirectory().string();
					if(!set.executionDir.empty())
						executionDirectory=set.executionDir;

					// start with executable
					cmd << props["compiler"];

					// determine backend
					string be = getBackendKey(backend);

					// add intercepted include directories
					for(const auto& cur : test.getInterceptedHeaderFileDirectories()) {
						cmd << " -I" << cur.string();
					}

					// add runtime include directories
					if (backend != Sequential) {			// TODO: make this non-hardcoded -- it is ugly, but I don't have the time ...
						cmd << " -I "<< SRC_ROOT_DIR << "runtime/include";
						cmd << " -I "<< SRC_ROOT_DIR << "common/include";
					}

					// add include directories
					for(const auto& cur : test.getIncludeDirs()) {
						cmd << " -I" << cur.string();
					}

					// add external lib dirs
					for(const auto& cur : test.getLibDirs()) {
						cmd << " -L" <<cur.string();
					}

					// add external libs
					for(const auto& cur : test.getLibNames()) {
						cmd << " -l" <<cur;
					}

					// disable multithreading
					set.numThreads=0;

					// add input file
					cmd << " " << executionDirectory << "/" << test.getBaseName() << ".insieme." << be << "." << getExtension(l);

					std::vector<string> flags=test.getCompilerArguments(name);
					// get all flags defined by properties
					for (string s: flags){
						cmd << " " <<s;
					};

					//get definitions
					for_each(test.getDefinitions(name), [&](const std::pair<string,string>& def) {
						cmd << " -D" << def.first << "=" <<def.second;
					});
					cmd << " ";

					// set output file, stdOut file and stdErr file
					set.outputFile=executionDirectory+"/"+test.getBaseName()+".insieme."+be;
					set.stdOutFile=executionDirectory+"/"+test.getBaseName()+"."+name+".out";
					set.stdErrFile=executionDirectory+"/"+test.getBaseName()+"."+name+".err.out";

					// run it
					return runner.runCommand(name, set, props, cmd.str());
				}, deps,COMPILE);
			}

			TestStep createInsiemeccExecuteStep(const string& name, Backend backend, const Dependencies& deps = Dependencies(), int numThreads=0, SchedulingPolicy sched=SCHED_UNDEFINED) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner)->TestResult {
					std::stringstream cmd;
					TestSetup set=setup;
					auto props = test.getPropertiesFor(name);

					// determine backend
					string be = getBackendKey(backend);

					// get execution dir	
					string executionDirectory=test.getDirectory().string();
					if(!set.executionDir.empty())
						executionDirectory=set.executionDir;
						
					// start with executable
					cmd << executionDirectory << "/" << test.getBaseName() << ".insieme." << be;

					// set number of threads
					set.numThreads=numThreads;

					// set scheduling variant
					set.sched=sched;

					// add arguments
					cmd << " " << props["executionFlags"];

					set.stdOutFile=executionDirectory+"/"+test.getBaseName()+"."+name+".out";
					set.stdErrFile=executionDirectory+"/"+test.getBaseName()+"."+name+".err.out";

					// run it
					return runner.runCommand(name, set, props, cmd.str(), "", executionDirectory);
				}, deps,RUN);
			}

			TestStep createInsiemeccCheckStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies(), int numThreads=0, SchedulingPolicy sched=SCHED_UNDEFINED) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::string langstr("_c_");
					if(l==CPP)
						langstr=string("_c++_");

					std::string schedString("");
					if(sched==STATIC)
						schedString="stat_";
					else if(sched==DYNAMIC)
						schedString="dyn_";
					else if(sched==GUIDED)
						schedString="guid_";


					std::stringstream cmd;
					TestSetup set=setup;

					// define comparison script
					cmd << props["sortdiff"];

					// determine backend
					string be = getBackendKey(backend);

					// disable multithreading
					set.numThreads=0;

					// get execution dir
					string executionDirectory=test.getDirectory().string();
					if(!set.executionDir.empty())
						executionDirectory=set.executionDir;
	
					// start with executable
					cmd << " " << executionDirectory << "/" << test.getBaseName() << ".ref" + langstr + "execute.out";

					// pipe result to output file
					if(numThreads)
						cmd << " " << executionDirectory << "/" << test.getBaseName() << ".insiemecc_"+be+langstr+"execute_"+schedString+std::to_string(numThreads)+".out";
					else
						cmd << " " << executionDirectory << "/" << test.getBaseName() << ".insiemecc_"+be+langstr+"execute.out";

					// add awk pattern
					// TODO: generally remove outer quotation marks in properties if present - I don't have the time now but it needs to be done at some point
					string outputAwk = props["outputAwk"]; //.substr(props["outputAwk"].find("\"")+1, props["outputAwk"].rfind("\"")-1);
					cmd << " " << outputAwk;

					set.stdOutFile=executionDirectory+"/"+test.getBaseName()+"."+name+".out";
					set.stdErrFile=executionDirectory+"/"+test.getBaseName()+"."+name+".err.out";

					// run it
					return runner.runCommand(name, set, props, cmd.str());
				}, deps,CHECK);
			}

			TestStep createRefCheckStep(const string& name, Language l, const Dependencies& deps = Dependencies(), int numThreads=0) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test, const TestRunner& runner)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::string langstr("c");
					if(l==CPP)
						langstr=string("c++");

					std::stringstream cmd;
					TestSetup set=setup;

					// define comparison script
					cmd << props["sortdiff"];

					// get execution dir
					string executionDirectory=test.getDirectory().string();
					if(!set.executionDir.empty())
						executionDirectory=set.executionDir;

					// start with executable
					cmd << " " << executionDirectory << "/" << test.getBaseName() << ".ref_"<<langstr<<"_execute.out";

					// pipe result to output file
					cmd << " " << executionDirectory << "/" << test.getBaseName() << ".ref_"<<langstr<<"_execute_"<<std::to_string(numThreads)<<".out";

					// add awk pattern
					cmd << " "<< props["outputAwk"];

					// disable multithreading
					set.numThreads=0;

					set.stdOutFile=executionDirectory+"/"+test.getBaseName()+"."+name+".out";
					set.stdErrFile=executionDirectory+"/"+test.getBaseName()+"."+name+".err.out";

					// run it
					return runner.runCommand(name, set, props, cmd.str());
				}, deps,CHECK);
			}


		}


		//create steps for statistics mode
		std::map<std::string,TestStep> createFullStepList(int statThreads,bool schedule) {
			std::map<std::string,TestStep> list;

			vector<int> threadList;
			for(int i=1;i<statThreads;i*=2)
				threadList.push_back(i);
			threadList.push_back(statThreads);

			auto add = [&](const TestStep& step) {
				list.insert({step.getName(), step});
			};

			// --- real steps ----

			add(createRefCompStep("ref_c_compile", C));
			add(createRefCompStep("ref_c++_compile", CPP));

			//add steps for each number of threads

			add(createRefRunStep("ref_c_execute", { "ref_c_compile" },1));
			add(createRefRunStep("ref_c++_execute", { "ref_c++_compile" },1));

			//iterate over whole vector starting with second element (> 1 thread)
			for(int i:vector<int>(++threadList.begin(),threadList.end())){
				add(createRefRunStep(std::string("ref_c_execute_")+std::to_string(i), { "ref_c_compile" },i));
				add(createRefRunStep(std::string("ref_c++_execute_")+std::to_string(i), { "ref_c++_compile" },i));
			}

			add(createInsiemeccSemaStep("insiemecc_c_sema", C));
			add(createInsiemeccSemaStep("insiemecc_c++_sema", CPP));

			add(createInsiemeccConversionStep("insiemecc_seq_c_convert", Sequential, C));
			add(createInsiemeccConversionStep("insiemecc_run_c_convert", Runtime, C));
			add(createInsiemeccConversionStep("insiemecc_ocl_c_convert", Opencl, C));

			add(createInsiemeccConversionStep("insiemecc_seq_c++_convert", Sequential, CPP));
			add(createInsiemeccConversionStep("insiemecc_run_c++_convert", Runtime, CPP));

			add(createInsiemeccCompilationStep("insiemecc_seq_c_compile", Sequential, C, { "insiemecc_seq_c_convert" }));
			add(createInsiemeccCompilationStep("insiemecc_run_c_compile", Runtime, C, { "insiemecc_run_c_convert" }));
			add(createInsiemeccCompilationStep("insiemecc_ocl_c_compile", Opencl, C, { "insiemecc_ocl_c_convert" }));

			add(createInsiemeccCompilationStep("insiemecc_seq_c++_compile", Sequential, CPP, { "insiemecc_seq_c++_convert" }));
			add(createInsiemeccCompilationStep("insiemecc_run_c++_compile", Runtime, CPP, { "insiemecc_run_c++_convert" }));

			// main seq execute
			add(createInsiemeccExecuteStep("insiemecc_seq_c_execute", Sequential, { "insiemecc_seq_c_compile" }));
			add(createInsiemeccExecuteStep("insiemecc_seq_c++_execute", Sequential, { "insiemecc_seq_c++_compile" }));

			// main seq check
			add(createInsiemeccCheckStep("insiemecc_seq_c_check", Sequential, C, { "insiemecc_seq_c_execute", "ref_c_execute" }));
			add(createInsiemeccCheckStep("insiemecc_seq_c++_check", Sequential, CPP, { "insiemecc_seq_c++_execute", "ref_c++_execute" }));

			// ocl execute & check
			add(createInsiemeccExecuteStep("insiemecc_ocl_c_execute", Opencl, { "insiemecc_ocl_c_compile" }));
			add(createInsiemeccCheckStep("insiemecc_ocl_c_check", Opencl, C, { "insiemecc_ocl_c_execute", "ref_c_execute" },1,STATIC));

			for(int i:threadList){
				// insiemecc_run execute
				add(createInsiemeccExecuteStep(std::string("insiemecc_run_c_execute_")+std::to_string(i), Runtime, { "insiemecc_run_c_compile" },i));
				add(createInsiemeccExecuteStep(std::string("insiemecc_run_c++_execute_")+std::to_string(i), Runtime, { "insiemecc_run_c++_compile" },i));

				// ref check
				if(i!=1){
					add(createRefCheckStep(std::string("ref_c_check_")+std::to_string(i),C,{ "ref_c_execute",std::string("ref_c_execute_")+std::to_string(i) },i));
					add(createRefCheckStep(std::string("ref_c++_check_")+std::to_string(i),CPP,{"ref_c++_execute",std::string("ref_c++_execute_")+std::to_string(i)},i));
				}

				// insiemecc_run check
				add(createInsiemeccCheckStep(std::string("insiemecc_run_c_check_")+std::to_string(i), Runtime, C, { std::string("insiemecc_run_c_execute_")+std::to_string(i), "ref_c_execute" },i));
				add(createInsiemeccCheckStep(std::string("insiemecc_run_c++_check_")+std::to_string(i), Runtime, CPP, { std::string("insiemecc_run_c++_execute_")+std::to_string(i), "ref_c++_execute"},i));

			}

			// clone insieme runs using different scheduling policies
			if(schedule){
				// main run execute STATIC
				add(createInsiemeccExecuteStep(std::string("insiemecc_run_c_execute_stat_")+std::to_string(statThreads), Runtime, { "insiemecc_run_c_compile" },statThreads,STATIC));
				add(createInsiemeccExecuteStep(std::string("insiemecc_run_c++_execute_stat_")+std::to_string(statThreads), Runtime, { "insiemecc_run_c++_compile" },statThreads,STATIC));

				// insiemecc_run check STATIC
				add(createInsiemeccCheckStep(std::string("insiemecc_run_c_check_stat_")+std::to_string(statThreads), Runtime, C, { std::string("insiemecc_run_c_execute_stat_")+std::to_string(statThreads),"ref_c_execute" },statThreads,STATIC));
				add(createInsiemeccCheckStep(std::string("insiemecc_run_c++_check_stat_")+std::to_string(statThreads), Runtime, CPP, { std::string("insiemecc_run_c++_execute_stat_")+std::to_string(statThreads), "ref_c++_execute"},statThreads,STATIC));

				// main run execute DYNAMIC
				add(createInsiemeccExecuteStep(std::string("insiemecc_run_c_execute_dyn_")+std::to_string(statThreads), Runtime, { "insiemecc_run_c_compile" },statThreads,DYNAMIC));
				add(createInsiemeccExecuteStep(std::string("insiemecc_run_c++_execute_dyn_")+std::to_string(statThreads), Runtime, { "insiemecc_run_c++_compile" },statThreads,DYNAMIC));

				// insiemecc_run check DYNAMIC
				add(createInsiemeccCheckStep(std::string("insiemecc_run_c_check_dyn_")+std::to_string(statThreads), Runtime, C, { std::string("insiemecc_run_c_execute_dyn_")+std::to_string(statThreads), "ref_c_execute"},statThreads,DYNAMIC));
				add(createInsiemeccCheckStep(std::string("insiemecc_run_c++_check_dyn_")+std::to_string(statThreads), Runtime, CPP, { std::string("insiemecc_run_c++_execute_dyn_")+std::to_string(statThreads), "ref_c++_execute"},statThreads,DYNAMIC));

				// main run execute GUIDED
				add(createInsiemeccExecuteStep(std::string("insiemecc_run_c_execute_guid_")+std::to_string(statThreads), Runtime, { "insiemecc_run_c_compile" },statThreads,GUIDED));
				add(createInsiemeccExecuteStep(std::string("insiemecc_run_c++_execute_guid_")+std::to_string(statThreads), Runtime, { "insiemecc_run_c++_compile" },statThreads,GUIDED));

				// insiemecc_run check GUIDED
				add(createInsiemeccCheckStep(std::string("insiemecc_run_c_check_guid_")+std::to_string(statThreads), Runtime, C, { std::string("insiemecc_run_c_execute_guid_")+std::to_string(statThreads), "ref_c_execute"},statThreads,GUIDED));
				add(createInsiemeccCheckStep(std::string("insiemecc_run_c++_check_guid_")+std::to_string(statThreads), Runtime, CPP, { std::string("insiemecc_run_c++_execute_guid_")+std::to_string(statThreads), "ref_c++_execute"},statThreads,GUIDED));
			}

			// PreCommand and PostCommand are executed before and after all the other steps
			// see "scheduleSteps" function
			add(createBashCommandStep("preprocessing"));
			add(createBashCommandStep("postprocessing"));

			return list;

		}


		std::map<std::string,TestStep> createFullStepList() {

			std::map<std::string,TestStep> list;

			auto add = [&](const TestStep& step) {
				list.insert({step.getName(), step});
			};

			// --- real steps ----

			add(createRefCompStep("ref_c_compile", C));
			add(createRefCompStep("ref_c++_compile", CPP));

			//add steps for each number of threads
			add(createRefRunStep("ref_c_execute", { "ref_c_compile" }));
			add(createRefRunStep("ref_c++_execute", { "ref_c++_compile" }));

			add(createInsiemeccSemaStep("insiemecc_c_sema", C));
			add(createInsiemeccSemaStep("insiemecc_c++_sema", CPP));

			add(createInsiemeccConversionStep("insiemecc_seq_c_convert", Sequential, C));
			add(createInsiemeccConversionStep("insiemecc_run_c_convert", Runtime, C));
			add(createInsiemeccConversionStep("insiemecc_ocl_c_convert", Opencl, C));

			add(createInsiemeccConversionStep("insiemecc_seq_c++_convert", Sequential, CPP));
			add(createInsiemeccConversionStep("insiemecc_run_c++_convert", Runtime, CPP));

			add(createInsiemeccCompilationStep("insiemecc_seq_c_compile", Sequential, C, { "insiemecc_seq_c_convert" }));
			add(createInsiemeccCompilationStep("insiemecc_run_c_compile", Runtime, C, { "insiemecc_run_c_convert" }));
			add(createInsiemeccCompilationStep("insiemecc_ocl_c_compile", Opencl, C, { "insiemecc_ocl_c_convert" }));

			add(createInsiemeccCompilationStep("insiemecc_seq_c++_compile", Sequential, CPP, { "insiemecc_seq_c++_convert" }));
			add(createInsiemeccCompilationStep("insiemecc_run_c++_compile", Runtime, CPP, { "insiemecc_run_c++_convert" }));

			add(createInsiemeccExecuteStep("insiemecc_seq_c_execute", Sequential, { "insiemecc_seq_c_compile" }));
			add(createInsiemeccExecuteStep("insiemecc_run_c_execute", Runtime, { "insiemecc_run_c_compile" }));
			add(createInsiemeccExecuteStep("insiemecc_ocl_c_execute", Opencl, { "insiemecc_ocl_c_compile" }));

			add(createInsiemeccExecuteStep("insiemecc_seq_c++_execute", Sequential, { "insiemecc_seq_c++_compile" }));
			add(createInsiemeccExecuteStep("insiemecc_run_c++_execute", Runtime, { "insiemecc_run_c++_compile" }));

			add(createInsiemeccCheckStep("insiemecc_seq_c_check", Sequential, C, { "insiemecc_seq_c_execute", "ref_c_execute" }));
			add(createInsiemeccCheckStep("insiemecc_run_c_check", Runtime, C, { "insiemecc_run_c_execute", "ref_c_execute" }));
			add(createInsiemeccCheckStep("insiemecc_ocl_c_check", Opencl, C, { "insiemecc_ocl_c_execute", "ref_c_execute" }));

			add(createInsiemeccCheckStep("insiemecc_seq_c++_check", Sequential, CPP, { "insiemecc_seq_c++_execute", "ref_c++_execute" }));
			add(createInsiemeccCheckStep("insiemecc_run_c++_check", Runtime, CPP, { "insiemecc_run_c++_execute", "ref_c++_execute" }));

			// preprocessing and postprocessing steps are executed before and after all the other steps
			// see "scheduleSteps" function
			add(createBashCommandStep("preprocessing"));
			add(createBashCommandStep("postprocessing"));

			return list;
		}

	}


	// a function obtaining an index of available steps
	const std::map<std::string,TestStep>& getFullStepList(int statThreads,bool scheduling) {
		const static std::map<std::string,TestStep> list = createFullStepList(statThreads,scheduling);
		return list;
	}

	const std::map<std::string,TestStep>& getFullStepList() {
		const static std::map<std::string,TestStep> list = createFullStepList();
		return list;
	}

	const TestStep& getStepByName(const std::string& name, int numThreads=0, bool scheduling=false) {
		static const TestStep fail;

		if(numThreads){
			auto& list =getFullStepList(numThreads,scheduling);
			auto pos = list.find(name);
			if (pos != list.end()) {
				return pos->second;
			}
		}
		else{
			auto& list =getFullStepList();
			auto pos = list.find(name);
			if (pos != list.end()) {
				return pos->second;
			}
		}
		assert_fail() << "Requested unknown step: " << name;
		return fail;
	}

	bool isExcluded(string excludes, TestStep step){
 		boost::char_separator<char> sep(",\"");
		boost::tokenizer<boost::char_separator<char>> tokens(excludes, sep);

		for (const string& it : tokens) {
			assert_true(it.find("\\E"));
			string tmp("\\Q" + it + "\\E");
			boost::replace_all(tmp, "*", "\\E.*\\Q");
			boost::regex reg(tmp, boost::regex::perl);
			if (boost::regex_match(step.getName(), reg))
				return true;
		}
		return false;
	}

	//filter steps based on some conflicting steps
	vector<TestStep> filterSteps(const vector<TestStep>& steps, const IntegrationTestCase& test, const map<string, string>& conflicting) {
		auto props = test.getProperties();
		vector<TestStep> stepsToExecute;

		for(const TestStep step:steps) {
			bool conflicts = false;
			string conflictingStep = "";

			if (!conflicting.empty()) {
				for (auto confl = conflicting.begin(); confl != conflicting.end(); confl++) {
					if(step.getName().find(confl->first) != std::string::npos){
						conflictingStep = confl->second;
						break;
					}
				}

				if(!conflictingStep.empty()) {
					for (const TestStep stepConfl:stepsToExecute) {
						if(stepConfl.getName().find(conflictingStep) != std::string::npos){
							conflicts = true;
							break;
						}
					}
				}
			}
			
			if(!isExcluded(props["excludeSteps"],step) && !conflicts)
				stepsToExecute.push_back(step);

			#ifndef USE_OPENCL
			for (TestStep& step : stepsToExecute)
				if (step.getName().find("_ocl_") != std::string::npos)
					return vector<TestStep>();
			#endif
		}
		return stepsToExecute;
	}



	namespace {

		void scheduleStep(const TestStep& step, vector<TestStep>& res, const IntegrationTestCase& test, int numThreads=0, bool scheduling=false) {
			// check whether test is already present
			if (::contains(res, step)) return;
			auto props = test.getProperties();

			if(isExcluded(props["excludeSteps"], step)) {
				LOG(WARNING) << test.getName() << " has a step with a dependency on an excluded step (" << step.getName() << ") -- please fix the test config!" << std::endl;
			}
			
			// check that all dependencies are present
			for(const auto& cur : step.getDependencies())
				scheduleStep(getStepByName(cur, numThreads, scheduling), res, test);
			
			// append step to schedule
			res.push_back(step);
		}

	}


	vector<TestStep> scheduleSteps(const vector<TestStep>& steps, const IntegrationTestCase& test, int numThreads, bool scheduling) {
		vector<TestStep> res;
		for(const auto& cur : steps) {
			scheduleStep(cur, res, test, numThreads, scheduling);
		}

		// Handling the preprocessing & postprocessing case
		vector<TestStep> final;
		TestStep pre, post;
		for(const auto& cur : res) {
			if (cur.getName() == "preprocessing")	pre = cur;
			else if (cur.getName() == "postprocessing") post = cur;
			else final.push_back(cur);
		}

		if(!pre.getName().empty()) final.insert(final.begin(), pre);
		if(!post.getName().empty()) final.push_back(post);
		return final;
	}

	string readFile(string fileName){
		FILE* file=fopen(fileName.c_str(),"r");

		if(file==NULL)
			return string("");

		char buffer[1024];
		string output;

		while(!feof(file)){
			if(fgets(buffer,1024,file)!=NULL){
				output+=string(buffer);
			}
		}
		fclose(file);
		return output;
	}

    /*
     *  Test Runner member functions
     */
    int TestRunner::executeWithTimeout(const string& executableParam, const string& argumentsParam,
							const string& environmentParam, const string& outFilePath,
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
			if(e.empty())
				continue;
			string temp = boost::replace_all_copy(e, "\"", "");
			if(insideQuote)
				argumentsVec.back().append(" " + temp);
			else
				argumentsVec.push_back(temp);
			size_t pos = string::npos;
			if((pos = e.find_first_of("\"\'")) != string::npos) {
				// in case a single word was quoted
				if(e.find_first_of("\"\'", pos + 1) != string::npos)
					continue;
				else
					insideQuote = !insideQuote;
			}
		}

		// convert arguments to char**
		vector<char*> argumentsForExec;
		// argv[0] needs to be the executable itself
		argumentsForExec.push_back(const_cast<char*>(executableParam.c_str()));
		for(const auto& s : argumentsVec)
			if(!s.empty())
				argumentsForExec.push_back(const_cast<char*>(s.c_str()));
		// terminate
		argumentsForExec.push_back('\0');

		/*
		 * Setup environment
		 */

		vector<string> environmentVec;
		boost::split(environmentVec, environmentParam, boost::is_any_of(" "));
		std::map<string,string> environmentMap;

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
		boost::regex reg("\\$\\{([^\\}]*)");
		boost::match_flag_type flags = boost::match_default;

		// iterate through Insieme environment setup, expand variables and merge everything with the environment map
		for(auto s : environmentVec) {
			if(!s.empty()) {
				string varName = s.substr(0, s.find("="));
				string varValue = s.substr(s.find("=")+1, string::npos);
				string expandedVarValue;
				boost::match_results<std::string::const_iterator> what;
				string::const_iterator begin = varValue.begin();
				string::const_iterator end = varValue.end();
				while (boost::regex_search(begin, end, what, reg, flags)) {
					boost::replace_all(varValue, string("${" + what[1] + "}"), environmentMap[what[1]]);
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
		environmentForExec.push_back('\0');

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
			const struct rlimit cpuLimit = { cpuTimeLimit, cpuTimeLimit + 5};
			if(setrlimit(RLIMIT_CPU, &cpuLimit) != 0)
				std::cerr << strerror(errno);
			// stdout and stderr redirection
			int fdOut, fdErr;
			if((fdOut = open(outFilePath.c_str(), O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR )) == -1)
				std::cerr << "Unable to create stdout file " << outFilePath << ", reason: " << strerror(errno) << "\n";
			if((fdErr = open(errFilePath.c_str(), O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR )) == -1)
				std::cerr << "Unable to create stderr file " << errFilePath << ", reason: " << strerror(errno) << "\n";
			if(dup2(fdOut, STDOUT_FILENO) == -1)
				std::cerr << "Unable to redirect stdout, reason: " << strerror(errno) << "\n";
			if(dup2(fdErr, STDERR_FILENO) == -1)
				std::cerr << "Unable to redirect stderr, reason: " << strerror(errno) << "\n";
			if(close(fdOut) == -1)
				std::cerr << "Unable to close stdout file descriptor, reason: " << strerror(errno) << "\n";
			if(close(fdErr) == -1)
				std::cerr << "Unable to close stderr file descriptor, reason: " << strerror(errno) << "\n";

			// navigate to execution directory if one is specified
			if(!execDir.empty()) boost::filesystem::current_path(execDir);

			if(execve(executableParam.c_str(), argumentsForExec.data(), environmentForExec.data()) == -1)
				std::cerr << "Unable to run executable " << executableParam << ", reason: " << strerror(errno) << "\n";
		} else {
		    #pragma omp critical (pids)
            TestRunner::getInstance().pids.push_back(pid);
			if(waitpid(pid, &retVal, 0) == -1)
				std::cerr << "Unable to wait for child process " << pid << ", reason: " << strerror(errno) << "\n";
		}
		return retVal;
	}

	TestResult TestRunner::runCommand(const string& stepName, const TestSetup& setup,
                       const PropertyView& testConfig, const string& cmd,
                       const string& producedFile, const string& execDir) const {

		vector<string> producedFiles;
		producedFiles.push_back(setup.stdOutFile);
		producedFiles.push_back(setup.stdErrFile);

		map<string,float> metricResults;
        //insert dummy vals
		metricResults["walltime"]=0;
		metricResults["cputime"]=0;
		metricResults["mem"]=0;

		if(!producedFile.empty()) {
			producedFiles.push_back(producedFile);
		}

		string outfile="";
		if(!setup.outputFile.empty()){
			producedFiles.push_back(setup.outputFile);
			outfile= " -o "+setup.outputFile;
		}

		// setup possible environment vars
		std::stringstream env;
		{
			if(!testConfig.get<vector<string>>("libPaths").empty()) {
				//set LD_LIBRARY_PATH
				env << "LD_LIBRARY_PATH=";
				for(const auto& ldPath : testConfig.get<vector<string>>("libPaths")) {
					env << ldPath << ":";
				}
				env<< "${LD_LIBRARY_PATH} ";
			}

			// set number of threads
			if(setup.numThreads){
				env<<"OMP_NUM_THREADS="<<setup.numThreads<<" ";
				env<<"IRT_NUM_WORKERS="<<setup.numThreads<<" ";
			}

			// set scheduling policy
			if(setup.sched==STATIC){
				env<<"IRT_SCHED_POLICY=IRT_SCHED_POLICY_STATIC ";
				env<<"IRT_LOOP_SCHED_POLICY=IRT_STATIC ";
				env<<"OMP_SCHEDULE=STATIC ";
			}
			else if(setup.sched==DYNAMIC){
				env<<"IRT_SCHED_POLICY=IRT_SCHED_POLICY_STATIC ";
				env<<"IRT_LOOP_SCHED_POLICY=IRT_DYNAMIC ";
				env<<"OMP_SCHEDULE=DYNAMIC ";
			}
			else if(setup.sched==GUIDED){
				env<<"IRT_SCHED_POLICY=IRT_SCHED_POLICY_STATIC ";
				env<<"IRT_LOOP_SCHED_POLICY=IRT_GUIDED ";
				env<<"OMP_SCHEDULE=GUIDED ";
			}
		}

		// if it is a mock-run do nothing
		if (setup.mockRun) {
			return TestResult(TestResult::ResultType::SUCCESS, stepName, 0, metricResults, "", "", 
				(execDir.empty() ? "" : "cd " + execDir + " && ") + env.str() + cmd + outfile);
		}

		string perfString("");
		vector<string> perfCodes;
		if(setup.perf){
			//cache load misses
			perfCodes.push_back(setup.load_miss);

			//cache write misses
			perfCodes.push_back(setup.store_miss);

			//flops
			perfCodes.push_back(setup.flops);

			//additional requested metrics
			BOOST_FOREACH(string s,setup.perf_metrics){
				perfCodes.push_back(s);
			}

			//build perf command
			perfString="perf stat -x , ";
			BOOST_FOREACH(string s,perfCodes){
				perfString=perfString+"-e "+s+" ";
			}

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
			if(actualReturnCode > 0)
				std::cerr << "Killed by signal " << actualReturnCode << "\n";
		}

		string output=readFile(setup.stdOutFile);
		string error=readFile(setup.stdErrFile);

		//get time, memory and perf values and remove them from stdError
		string stdErr;
		boost::char_separator<char> sep("\n");
		boost::tokenizer<boost::char_separator<char>> tok(error,sep);
		for(boost::tokenizer<boost::char_separator<char>>::iterator beg=tok.begin(); beg!=tok.end();++beg){
			string token(*beg);
			if(token.find("WALLTIME")==0) {
				metricResults["walltime"]=atof(token.substr(8).c_str());
			} else if(token.find("CPUTIME")==0) {
				metricResults["cputime"]=atof(token.substr(7).c_str());
				// check if we approached the cpu time limit. If so, print a warning
				if(((metricResults["cputime"]))/cpuTimeLimit > 0.95){
					std::cerr << "Killed by timeout, CPU time was " << metricResults["cputime"] << ", limit was " << cpuTimeLimit << " seconds\n";
					metricResults["timeout"]=1;
				}
			} else if (token.find("MEM")==0) {
				metricResults["mem"]=atof(token.substr(3).c_str());
			} else {
				//check perf metrics, otherwise append to stderr
				bool found=false;
				for(auto code : perfCodes) {
					if(token.find(code)!=token.npos){
						string value=token.substr(0,token.find(","));
						float intVal;
						//try cast to int
						try{
							intVal=boost::lexical_cast<float>(value);
						}catch(const boost::bad_lexical_cast &){
							//not counted or error
							intVal=-1;
						}

						//mark special perf metrics
						if(code.compare(setup.load_miss)==0)
							metricResults["load_miss"]=intVal;
						else if (code.compare(setup.store_miss)==0)
							metricResults["store_miss"]=intVal;
						else if (code.compare(setup.flops)==0)
							metricResults["flops"]=intVal;
						else
							metricResults[code]=intVal;

						found=true;
						break;
					}
				}
				//no metric -> it is stdErr
				if(!found)
					stdErr+=token+"\n";
			}
		}

		// check whether execution has been aborted by the user
		if (actualReturnCode == SIGINT || actualReturnCode == SIGQUIT) {
			return TestResult::userAborted(stepName);
		}
		// produce regular result
		return TestResult(retVal == 0 ? TestResult::ResultType::SUCCESS : TestResult::ResultType::FAILURE, stepName, 
			actualReturnCode, metricResults, output, stdErr, cmd, producedFiles, setup.numThreads, setup.sched);
	}




} // end namespace integration
} // end namespace driver
} // end namespace insieme

