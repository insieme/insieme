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


#include <string>
#include <vector>

using namespace std;

namespace insieme {
namespace driver {
namespace integration {

	class TestResult {
	  public:
		enum class ResultType { SUCCESS, FAILURE, ABORT, OMITTED };

	  private:
		ResultType resType;

		string stepName;
		int retVal;
		float walltime;
		float memory;

		string output;
		string errorOut;
		string cmd;
		std::vector<std::string> producedFiles;

	  public:
		TestResult(const ResultType& resType, string stepName, int retVal,
		           float walltime = 0, float memory = 0, const string& output = "", const string& errorOut = "", const string& cmd = "",
		           std::vector<std::string> producedFiles = std::vector<std::string>())
		    : resType(resType), stepName(stepName), retVal(retVal), walltime(walltime), memory(memory), output(output),
		      errorOut(errorOut), cmd(cmd), producedFiles(producedFiles) {}

		static TestResult userAborted(const string& stepName) {
			TestResult res(ResultType::ABORT, stepName, -1);
			return res;
		}

		static TestResult stepOmitted(const string& stepName) {
			TestResult res(ResultType::OMITTED, stepName, -1);
			return res;
		}

		bool wasSuccessful() const {
			return resType == ResultType::SUCCESS;
		}

		bool wasOmitted() const {
			return resType == ResultType::OMITTED;
		}

		bool wasAborted() const {
			return resType == ResultType::ABORT;
		}

		// deletes all produced files
		void clean() const {
			for(const auto& cur : producedFiles) {
				remove(cur.c_str());
			}
		}

		string getCmd() const {
			return cmd;
		}

		string getStepName() const {
			return stepName;
		}

		int getRetVal() const {
			return retVal;
		}

		string getFullOutput() const {
			return output + errorOut;
		}

		float getRuntime() const {
			return walltime;
		}

		float getMemory() const {
			return memory;
		}
	};

} // end namespace integration
} // end namespace driver
} // end namespace insieme
