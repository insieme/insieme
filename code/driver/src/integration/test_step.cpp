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
#include <csignal>

#include <sstream>
#include <regex>

#include "insieme/utils/assert.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/config.h"

#include<boost/tokenizer.hpp>


namespace insieme {
namespace driver {
namespace integration {

	namespace {

		namespace {

			TestResult runCommand(const TestSetup& setup, const PropertyView& testConfig, const string& cmd, const string& producedFile="") {

				vector<string> producedFiles;
				producedFiles.push_back(setup.stdOutFile);
				producedFiles.push_back(setup.stdErrFile);

				if(!producedFile.empty()) {
					producedFiles.push_back(producedFile);
				}

				string outfile="";
				if(!setup.outputFile.empty()){
					producedFiles.push_back(setup.outputFile);
					outfile= " -o "+setup.outputFile;
				}

				//setup possible environment vars
				std::stringstream env;
				{
					//set LD_LIBRARY_PATH
					env << "LD_LIBRARY_PATH=";
					for(const auto& ldPath : testConfig.get<vector<string>>("libPaths")) {
						env << ldPath << ":";
					}
					env<< "${LD_LIBRARY_PATH} ";
				}

				// if it is a mock-run do nothing
				if (setup.mockRun) {
					return TestResult(true,0,0,"","",env.str() + cmd + outfile);
				}

				//environment must be set BEFORE executables!
				string realCmd = env.str() + string(testConfig["time_executable"])+string(" -f \"\nTIME%e\nMEM%M\" ")+cmd + outfile +" >"+setup.stdOutFile+" 2>"+setup.stdErrFile;

				//TODO enable perf support
				//if(setup.enablePerf)

				//get return value, stdOut and stdErr
				int retVal=system(realCmd.c_str());

				//TODO change this to handle SIGINT signal
				if(retVal==512)
					exit(0);

			   if (WIFSIGNALED(retVal) &&
				   (WTERMSIG(retVal) == SIGINT || WTERMSIG(retVal) == SIGQUIT))
				   	   std::cout<<"killed"<<std::endl;

				string output=readFile(setup.stdOutFile);
				string error=readFile(setup.stdErrFile);

				float mem=0.0;
				float time=0.0;

				//get time and memory values and remove them from stdError
				string stdErr;
				boost::char_separator<char> sep("\n");
				boost::tokenizer<boost::char_separator<char>> tok(error,sep);
				for(boost::tokenizer<boost::char_separator<char>>::iterator beg=tok.begin(); beg!=tok.end();++beg){
				string token(*beg);
				if(token.find("TIME")==0)
					time=atof(token.substr(4).c_str());
				else if (token.find("MEM")==0)
					mem=atof(token.substr(3).c_str());
				else
					stdErr+=token+"\n";
				}

				// check whether execution has been aborted by the user
				if (WIFSIGNALED(retVal) && (WTERMSIG(retVal) == SIGINT || WTERMSIG(retVal) == SIGQUIT)) {
					return TestResult::userAborted(time, mem, output, stdErr, cmd);
				}

				// produce regular result
				return TestResult(retVal==0,time,mem,output,stdErr,cmd,producedFiles);
			}

			namespace fs = boost::filesystem;

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


			//TODO MAKE FLAGS STEP SPECIFIC

			TestStep createRefCompStep(const string& name, Language l) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
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
						cmd <<" -L"<<cur.string();
					}

					// add external libs
					for(const auto& cur : test.getLibNames()) {
						cmd <<" -l"<<cur;
					}

					// add input files
					for(const auto& cur : test.getFiles()) {
						cmd << " " << cur.string();
					}

					std::vector<string> flags=test.getCompilerArguments(name);
					// get all flags defined by properties
					for (string s: flags){
						cmd <<" "<<s;
					}

					//get definitions
					for_each(test.getDefinitions(name), [&](const std::pair<string,string>& def) {
						cmd<<"-D"<<def.first<<"="<<def.second<<" ";
					});

					// set output file, stdOutFile and stdErrFile
					set.outputFile=test.getDirectory().string()+"/"+test.getBaseName()+".ref";
					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".ref.comp.out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".ref.comp.err.out";

					// run it
					return runCommand(set, props, cmd.str());
				},std::set<std::string>(),COMPILE);
			}

			TestStep createRefRunStep(const string& name, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					std::stringstream cmd;
					TestSetup set=setup;
					auto props = test.getPropertiesFor(name);

					// start with executable
					cmd << test.getDirectory().string() << "/" << test.getBaseName() << ".ref";

					// add arguments
					cmd << " " << props["executionFlags"];

					// set output files
					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".ref.out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".ref.err.out";

					// run it
					return runCommand(set, props, cmd.str());
				}, deps,RUN);
			}
			
			TestStep createInsiemeccCompStep(const string& name, Language l) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
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
						cmd <<" -L"<<cur.string();
					}

					// add external libs
					for(const auto& cur : test.getLibNames()) {
						cmd <<" -l"<<cur;
					}

					// add input files
					for(const auto& cur : test.getFiles()) {
						cmd << " " << cur.string();
					}

					std::vector<string> flags=test.getCompilerArguments(name);
					// get all flags defined by properties
					for (string s: flags){
						cmd <<" "<<s;
					}

					//get definitions
					for_each(test.getDefinitions(name), [&](const std::pair<string,string>& def) {
						cmd<<"-D"<<def.first<<"="<<def.second<<" ";
					});

					//append intercept patterns
					for(const auto& cur : test.getInterceptedNameSpaces()) {
						cmd << " --intercept " << cur;
					}
					//append intercepted header file dirs
					for(const auto& cur : test.getInterceptedHeaderFileDirectories()) {
						cmd << " --intercept-include " << cur.string();
					}

					// set output file, stdOutFile and stdErrFile
					set.outputFile=test.getDirectory().string()+"/"+test.getBaseName()+".insiemecc";
					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".insiemecc.comp.out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".insiemecc.comp.err.out";

					// run it
					return runCommand(set, props, cmd.str());
				},std::set<std::string>(),COMPILE);
			}

			TestStep createInsiemeccRunStep(const string& name, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					std::stringstream cmd;
					TestSetup set=setup;
					auto props = test.getPropertiesFor(name);

					// start with executable
					cmd << test.getDirectory().string() << "/" << test.getBaseName() << ".insiemecc";

					// add arguments
					cmd << " " << props["executionFlags"];

					// set output files
					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".insiemecc.out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".insiemecc.err.out";

					// run it
					return runCommand(set, props, cmd.str());
				}, deps,RUN);
			}


			TestStep createMainSemaStep(const string& name, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;
					TestSetup set=setup;

					// start with executable
					cmd << props["compiler"];

					// enable semantic tests
					cmd << " -S";

					// also dump IR
					std::string irFile=test.getDirectory().string() + "/" + test.getBaseName() + ".ir";
					cmd << " --dump-ir " << irFile;

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
						cmd <<" "<<s;
					}

					//get definitions
					for_each(test.getDefinitions(name), [&](const std::pair<string,string>& def) {
						cmd<<"-D"<<def.first<<"="<<def.second<<" ";
					});

					//append intercept patterns
					for(const auto& cur : test.getInterceptedNameSpaces()) {
						cmd << " --intercept " << cur;
					}
					//append intercepted header file dirs
					for(const auto& cur : test.getInterceptedHeaderFileDirectories()) {
						cmd << " --intercept-include " << cur.string();
					}

					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".sema.comp.out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".sema.comp.err.out";

					// run it
					return runCommand(set, props, cmd.str(),irFile);
				}, deps,COMPILE);
			}

			TestStep createMainConversionStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;
					TestSetup set=setup;

					// start with executable
					cmd << props["compiler"];

					// determine backend
					string be = getBackendKey(backend);
					cmd << " -b " << be;

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
						cmd <<" "<<s;
					}

					//get definitions
					for_each(test.getDefinitions(name), [&](const std::pair<string,string>& def) {
						cmd<<"-D"<<def.first<<"="<<def.second<<" ";
					});

					//append intercept patterns
					for(const auto& cur : test.getInterceptedNameSpaces()) {
						cmd << " --intercept " << cur;
					}
					//append intercepted header file dirs
					for(const auto& cur : test.getInterceptedHeaderFileDirectories()) {
						cmd << " --intercept-include " << cur.string();
					}

					// set output file, stdOut file and stdErr file
					set.outputFile=test.getDirectory().string()+"/"+test.getBaseName()+".insieme."+be+"."+getExtension(l);
					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".conv.out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".conv.err.out";

					// run it
					return runCommand(set, props, cmd.str());
				}, deps,COMPILE);
			}

			TestStep createMainCompilationStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;
					TestSetup set=setup;

					// start with executable
					cmd << props["compiler"];

					// determine backend
					string be = getBackendKey(backend);

					// add intercepted include directories
					for(const auto& cur : test.getInterceptedHeaderFileDirectories()) {
						cmd << " -I" << cur.string();
					}

					// add runtime include directories
					if (backend == Runtime) {			// TODO: make this non-hardcoded -- it is ugly, but I don't have the time ...
						cmd << " -I "<< SRC_ROOT_DIR << "runtime/include";
						cmd << " -I "<< SRC_ROOT_DIR << "meta_information/include";
					}

					// add external lib dirs
					for(const auto& cur : test.getLibDirs()) {
						cmd <<" -L"<<cur.string();
					}

					// add external libs
					for(const auto& cur : test.getLibNames()) {
						cmd <<" -l"<<cur;
					}

					// add input file
					cmd << " " << test.getDirectory().string() << "/" << test.getBaseName() << ".insieme." << be << "." << getExtension(l);

					std::vector<string> flags=test.getCompilerArguments(name);
					// get all flags defined by properties
					for (string s: flags){
						cmd <<" "<<s;
					};

					//get definitions
					for_each(test.getDefinitions(name), [&](const std::pair<string,string>& def) {
						cmd<<"-D"<<def.first<<"="<<def.second<<" ";
					});

					// set output file, stdOut file and stdErr file
					set.outputFile=test.getDirectory().string()+"/"+test.getBaseName()+".insieme."+be;
					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".comp.out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".comp.err.out";

					// run it
					return runCommand(set, props, cmd.str());
				}, deps,COMPILE);
			}

			TestStep createMainExecuteStep(const string& name, Backend backend, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					std::stringstream cmd;
					TestSetup set=setup;
					auto props = test.getPropertiesFor(name);

					// determine backend
					string be = getBackendKey(backend);


					// start with executable
					cmd << test.getDirectory().string() << "/" << test.getBaseName() << ".insieme." << be;

					// add arguments
					cmd << " " << props["executionFlags"];

					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".insieme."+be+".out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".insieme."+be+".err.out";

					// run it
					return runCommand(set, props, cmd.str());
				}, deps,RUN);
			}

			TestStep createMainCheckStep(const string& name, Backend backend, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;
					TestSetup set=setup;

					// define comparison script
					cmd << props["sortdiff"];

					// determine backend
					string be = getBackendKey(backend);

					// start with executable
					cmd << " " << test.getDirectory().string() << "/" << test.getBaseName() << ".ref.out";

					// pipe result to output file
					cmd << " " << test.getDirectory().string() << "/" << test.getBaseName() << ".insieme." << be << ".out";

					// add awk pattern
					// TODO: generally remove outer quotation marks in properties if present - I don't have the time now but it needs to be done at some point
					string outputAwk = props["outputAwk"].substr(props["outputAwk"].find("\"")+1, props["outputAwk"].rfind("\"")-1);
					cmd << " '"<< outputAwk << "'";

					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".match.out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".match.err.out";

					// run it
					return runCommand(set, props, cmd.str());
				}, deps,CHECK);
			}
			
			TestStep createInsiemeccCheckStep(const string& name, Language l, const Dependencies& deps = Dependencies()) {
				return TestStep(name, [=](const TestSetup& setup, const IntegrationTestCase& test)->TestResult {
					auto props = test.getPropertiesFor(name);

					std::stringstream cmd;
					TestSetup set=setup;

					// define comparison script
					cmd << props["sortdiff"];

					// start with executable
					cmd << " " << test.getDirectory().string() << "/" << test.getBaseName() << ".ref.out";

					// pipe result to output file
					cmd << " " << test.getDirectory().string() << "/" << test.getBaseName() << ".insiemecc.out";

					// add awk pattern
					// TODO: generally remove outer quotation marks in properties if present - I don't have the time now but it needs to be done at some point
					string outputAwk = props["outputAwk"].substr(props["outputAwk"].find("\"")+1, props["outputAwk"].rfind("\"")-1);
					cmd << " '"<< outputAwk << "'";

					set.stdOutFile=test.getDirectory().string()+"/"+test.getBaseName()+".match.out";
					set.stdErrFile=test.getDirectory().string()+"/"+test.getBaseName()+".match.err.out";

					// run it
					return runCommand(set, props, cmd.str());
				}, deps,CHECK);
			}


		}

		std::map<std::string,TestStep> createFullStepList() {

			std::map<std::string,TestStep> list;

			auto add = [&](const TestStep& step) {
				list.insert({step.getName(), step});
			};

			// --- real steps ----

			add(createRefCompStep("ref_c_compile", C));
			add(createRefCompStep("ref_c++_compile", CPP));

			add(createRefRunStep("ref_c_execute", { "ref_c_compile" }));
			add(createRefRunStep("ref_c++_execute", { "ref_c++_compile" }));

			add(createInsiemeccCompStep("insiemecc_c_compile", C));
			add(createInsiemeccCompStep("insiemecc_c++_compile", CPP));

			add(createInsiemeccRunStep("insiemecc_c_execute", { "insiemecc_c_compile" }));
			add(createInsiemeccRunStep("insiemecc_c++_execute", { "insiemecc_c++_compile" }));

			add(createMainSemaStep("main_c_sema", C));
			add(createMainSemaStep("main_c++_sema", CPP));

			add(createMainConversionStep("main_seq_convert", Sequential, C));
			add(createMainConversionStep("main_run_convert", Runtime, C));

			add(createMainConversionStep("main_seq_c++_convert", Sequential, CPP));
			add(createMainConversionStep("main_run_c++_convert", Runtime, CPP));

			add(createMainCompilationStep("main_seq_compile", Sequential, C, { "main_seq_convert" }));
			add(createMainCompilationStep("main_run_compile", Runtime, C, { "main_run_convert" }));

			add(createMainCompilationStep("main_seq_c++_compile", Sequential, CPP, { "main_seq_c++_convert" }));
			add(createMainCompilationStep("main_run_c++_compile", Runtime, CPP, { "main_run_c++_convert" }));

			add(createMainExecuteStep("main_seq_execute", Sequential, { "main_seq_compile" }));
			add(createMainExecuteStep("main_run_execute", Runtime, { "main_run_compile" }));

			add(createMainExecuteStep("main_seq_c++_execute", Sequential, { "main_seq_c++_compile" }));
			add(createMainExecuteStep("main_run_c++_execute", Runtime, { "main_run_c++_compile" }));

			add(createMainCheckStep("main_seq_check", Sequential, C, { "main_seq_execute", "ref_c_execute" }));
			add(createMainCheckStep("main_run_check", Runtime, C, { "main_run_execute", "ref_c_execute" }));

			add(createMainCheckStep("main_seq_c++_check", Sequential, CPP, { "main_seq_c++_execute", "ref_c++_execute" }));
			add(createMainCheckStep("main_run_c++_check", Runtime, CPP, { "main_run_c++_execute", "ref_c++_execute" }));
			
			add(createInsiemeccCheckStep("insiemecc_c_check", C, { "insiemecc_c_execute", "ref_c_execute" }));
			add(createInsiemeccCheckStep("insiemecc_c++_check", CPP, { "insiemecc_c++_execute", "ref_c++_execute" }));

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
		auto props = test.getProperties();
		vector<TestStep> filteredSteps;

		for(const TestStep step:steps){
			string excludes=props["excludeSteps"];
			if(excludes.find(step.getName()) == std::string::npos)
				filteredSteps.push_back(step);
		}

		return filteredSteps;
	}

	namespace {

		void scheduleStep(const TestStep& step, vector<TestStep>& res, const IntegrationTestCase& test) {

			// check whether test is already present
			if (contains(res, step)) return;

			auto props = test.getProperties();

			// check that all dependencies are present
			for(const auto& cur : step.getDependencies()) {
				string excludes=props["excludeSteps"];
				if(excludes.find(step.getName()) != std::string::npos) {
					LOG(WARNING) << test.getName() << " has step with a dependency on an excluded step (" << step.getName() << ") -- fix test config!" << std::endl;
				}
					
				scheduleStep(getStepByName(cur), res, test);
			}

			// append step to schedule
			res.push_back(step);
		}

	}


	vector<TestStep> scheduleSteps(const vector<TestStep>& steps, const IntegrationTestCase& test) {
		vector<TestStep> res;
		for(const auto& cur : steps) {
			scheduleStep(cur, res, test);
		}
		return res;
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

} // end namespace integration
} // end namespace driver
} // end namespace insieme
