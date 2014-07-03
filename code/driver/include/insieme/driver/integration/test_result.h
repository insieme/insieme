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

#include "insieme/utils/logging.h"
using namespace std;

namespace insieme {
namespace driver {
namespace integration {

	enum SchedulingPolicy{SCHED_UNDEFINED,STATIC,DYNAMIC,GUIDED};

	class TestResult {

		string stepName;
		int retVal;
		bool success;
		map<string,float> metricResults;
		map<string,float> metricDeviation;

		string output;
		string errorOut;
		string cmd;
		std::vector<std::string> producedFiles;
		int numThreads;
		SchedulingPolicy sched;
	
		bool userabort;

	private:
		friend class boost::serialization::access;		
		template<class Archive>
		void serialize(Archive & ar, const unsigned int version)
		{
		        ar & success;
		        ar & metricResults;
		        ar & metricDeviation;
			ar & output;
			ar & errorOut;
			ar & cmd;
			ar & producedFiles;
			ar & numThreads;
			ar & sched;
			ar & userabort;
		}

	protected:
		map<string,string> staticResults;

	public:

		TestResult(string stepName="", int retVal = 0, bool success = true, map<string,float> metricResults=map<string,float>(), string output="", string errorOut="", string cmd="",
		std::vector<std::string> producedFiles=std::vector<std::string>(),int numThreads=0,SchedulingPolicy sched=SCHED_UNDEFINED)
			: stepName(stepName),retVal(retVal),success(success),metricResults(metricResults),metricDeviation(map<string,float>()),output(output),errorOut(errorOut),cmd(cmd),
				producedFiles(producedFiles),numThreads(numThreads),sched(sched),userabort(false){}

		static TestResult userAborted(string stepName, map<string,float> metricResults, string output="", string errorOut="", string cmd="") {
			TestResult res(stepName, -1, false, metricResults, output, errorOut, cmd);
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

		static TestResult returnAVG(vector<TestResult> t){
			TestResult ret;
			TestResult front=t.front();
			map<string,float> frontResults=front.getMetrics();
			float sum=0,avg,var;

			//iterate over all collected metrics
			for (auto metr=frontResults.begin();metr!=frontResults.end();metr++){
				vector<float> metricResults;
				sum=0;

				//iterate over all testRuns
				for(auto result=t.begin();result!=t.end();result++){
					map<string,float> testResults=result->getMetrics();
					
					//check if results are comparable
					if(frontResults.size()!=testResults.size() || front.getNumThreads() != result->getNumThreads() || front.getScheduling()!=result->getScheduling() 
						|| !front || !(*result)){
						LOG(WARNING) << "Test results of a metric not comparable, no median calculated!";
						return front;
					}
					sum+=testResults[metr->first];
					metricResults.push_back(testResults[metr->first]);
				}
				
				// calc avg
				avg=sum/t.size();

				//calc stddev
				float tmp=0;
				for(float val: metricResults)
					tmp+=(val-avg)*(val-avg);
				tmp/=t.size();
				var=sqrt(tmp);

				//insert into ret
				ret.insertMetric(metr->first,avg,var);
			}

			return ret;
		}


		static TestResult returnMedian(vector<TestResult> t){
			TestResult ret;
			TestResult front=t.front();
			map<string,float> frontResults=front.getMetrics();

			//iterate over all collected metrics
			for (auto metr=frontResults.begin();metr!=frontResults.end();metr++){
				vector<float> metricResults;

				//iterate over all testRuns
				for(auto result=t.begin();result!=t.end();result++){
					map<string,float> testResults=result->getMetrics();
					
					//check if results are comparable
					if(frontResults.size()!=testResults.size() || front.getNumThreads() != result->getNumThreads() || front.getScheduling()!=result->getScheduling() 
						|| !front || !(*result)){
						LOG(WARNING) << "Test results of a metric not comparable, no median calculated!";
						return front;
					}
					metricResults.push_back(testResults[metr->first]);
				}
				
				//calc median and insert into ret
				std::sort(metricResults.begin(),metricResults.end());
				int size=metricResults.size();
				if(size%2!=0)
					ret.insertMetric(metr->first,metricResults[size/2]);	
				else
					ret.insertMetric(metr->first,(metricResults[(size+1)/2]+metricResults[(size-1)/2])/2);	

			}

			return ret;
		}

		string getCmd() const{
			return cmd;
		}

		string getStepName() const{
			return stepName;
		}

		int getRetVal() const{
			return retVal;
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
			return metricResults.find("walltime")->second;
		}

		float getMemory() const{
			return metricResults.find("mem")->second;
		}

		float getRuntimeDev() const{
			return metricDeviation.find("walltime")->second;
		}

		float getMemoryDev() const{
			return metricDeviation.find("mem")->second;
		}

		void insertMetric(string name,float val){
			metricResults[name]=val;
		}

		void insertMetric(string name,float val, float dev){
			metricResults[name]=val;
			metricDeviation[name]=dev;
		}

		SchedulingPolicy getScheduling() const{
			return sched;
		}

		bool deviationAvailable() const{
			return metricResults.size()==metricDeviation.size();
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
