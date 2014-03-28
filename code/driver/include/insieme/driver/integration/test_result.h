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

	class TestResult {

		bool success;
		float runtime;
		float memory;
		string output;
		string errorOut;
		string cmd;
		std::vector<std::string> producedFiles;

		bool userabort;

	public:

		TestResult(bool success = true, float runtime = 0.0 , float memory = 0.0, string output="", string errorOut="", string cmd=""
				,std::vector<std::string> producedFiles=std::vector<std::string>())
			: success(success),runtime(runtime),memory(memory),output(output),errorOut(errorOut),cmd(cmd),producedFiles(producedFiles), userabort(false) {}

		static TestResult userAborted(float runtime = 0.0 , float memory = 0.0, string output="", string errorOut="", string cmd="") {
			TestResult res(false, runtime, memory, output, errorOut, cmd);
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

		float getRuntime() const{
			return runtime;
		}

		string getCmd() const{
			return cmd;
		}

		string getFullOutput() const{
			return output+errorOut;
		}

		float getMemory() const{
			return memory;
		}

		bool hasBeenAborted() const {
			return userabort;
		}
	};

} // end namespace integration
} // end namespace driver
} // end namespace insieme
