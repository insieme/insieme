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


#include <string>
#include <vector>
using namespace std;

namespace insieme {
namespace driver {
namespace integration {

	enum SchedulingPolicy{SCHED_UNDEFINED,STATIC,DYNAMIC,GUIDED};

	class TestResult {

		bool success;
		map<string,float> metricResults;

		string output;
		string errorOut;
		string cmd;
		std::vector<std::string> producedFiles;
		int numThreads;
		SchedulingPolicy sched;
	
		bool userabort;

	protected:
		map<string,string> staticResults;

	public:

		TestResult(bool success = true, map<string,float> metricResults=map<string,float>(), string output="", string errorOut="", string cmd=""
			,std::vector<std::string> producedFiles=std::vector<std::string>(),int numThreads=0,SchedulingPolicy sched=SCHED_UNDEFINED)
			: success(success),metricResults(metricResults),errorOut(errorOut),cmd(cmd),producedFiles(producedFiles),numThreads(numThreads),sched(sched),userabort(false){}

		static TestResult userAborted(map<string,float> metricResults, string output="", string errorOut="", string cmd="") {
			TestResult res(false, metricResults, output, errorOut, cmd);
			res.userabort = true;
			return res;
		}

		bool wasSuccessfull() const {
			return success;
		}

		// deletes all produced files
		void clean() const{
			for(const auto& cur : producedFiles) {
				remove(cur.c_str());
			}
		}

		operator bool() const {
			return success;
		}

		string getCmd() const{
			return cmd;
		}

		int getNumThreads() const{
			return numThreads;
		}

		string getFullOutput() const{
			return output+errorOut;
		}

		map<string,float> getMetrics() const{
			return metricResults;
		}

		bool hasBeenAborted() const {
			return userabort;
		}

		float getRuntime() const{
			return metricResults.find("time")->second;
		}

		float getMemory() const{
			return metricResults.find("mem")->second;
		}

		string getSchedulingString() const{
			if(sched==STATIC)
				return "static";
			if(sched==DYNAMIC)
				return "dynamic";
			if(sched==GUIDED)
				return "guided";
			return "undefined";
		}
	
	};

	class StaticResult : public TestResult{
		public:
		StaticResult(map<string,string> results){staticResults=results;};

		map<string,string> getStaticMetrics(){
			return staticResults;
		}
	};

} // end namespace integration
} // end namespace driver
} // end namespace insieme
