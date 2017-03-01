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
 *
 */
#include "insieme/driver/measure/system_info.h"

#include <regex>

#include "insieme/core/ir_node.h"
#include "insieme/driver/integration/tests.h"
#include "insieme/utils/config.h"

namespace insieme {
namespace driver {
namespace measure {

	using std::string;

	int SystemInfo::obtainSystemInformation() const {
		core::NodeManager manager;
		auto testCode = insieme::driver::integration::getCase("seq/c/hello_world")->load(manager);
		assert_true(testCode) << "SystemInfo dummy code invalid!";
		auto target = backend->convert(testCode);

		auto tempCompiler = compiler;
		for(const auto& e : backendCompilerDefs) {
			string temp = "-D" + string(e.first) + "=" + string(e.second);
			tempCompiler.addFlag(temp);
		}
		tempCompiler.addFlag(string("-I ") + utils::getPapiRootDir() + "include");
		tempCompiler.addFlag(string("-L ") + utils::getPapiRootDir() + "lib");
		tempCompiler.addFlag(string("-Wl,-rpath,") + utils::getPapiRootDir() + "lib -lpapi");

		auto binary = utils::compiler::compileToBinary(*target, tempCompiler);

		const string workDir = ".";

		ExecutionSetup executionSetup = ExecutionSetup().withEnvironment(env).withOutputDirectory(workDir);
		const int ret = executor->run(binary, executionSetup);
		boost::filesystem::remove(binary);

		if(ret != 0) { return ret; }

		const string reportFile = workDir + "/insieme_runtime.report";

		if(!boost::filesystem::exists(reportFile)) { return -1; }

		std::ifstream in(reportFile);
		if(!in.is_open()) { return -2; }

		data = string((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());

		in.close();
		boost::filesystem::remove(reportFile);

		if(data.empty()) {
			LOG(ERROR) << "Error while trying to obtain system information, result was empty\n";
			return -3;
		}

		return 0;
	}

	bool SystemInfo::isValid() const {
		return valid;
	}

	const string& SystemInfo::getFullInformation() const {
		return data;
	}

	unsigned SystemInfo::getNumberOfCPUsTotal() const {
		return queryGeneric<unsigned>("Number of CPUs:\\s+(\\d+)").front();
	}

	unsigned SystemInfo::getNumberOfCoresPerSocket() const {
		return queryGeneric<unsigned>("Number of cores per socket:\\s+(\\d+)").front();
	}

	unsigned SystemInfo::getNumberOfSockets() const {
		return queryGeneric<unsigned>("Number of sockets:\\s+(\\d+)").front();
	}

	unsigned SystemInfo::getNumberOfHWThreadsPerCore() const {
		return queryGeneric<unsigned>("Number of HW threads per core:\\s+(\\d+)").front();
	}

	bool SystemInfo::hasDVFSDomainPerCore() const {
		return (queryGenericString("CPU DVFS domain:\\s+(\\w+)").front() == string("cores"));
	}

	string SystemInfo::queryGenericStringSingle(const string& regex) const {
		const auto temp = queryGenericString(regex);
		string res;
		if(!temp.empty()) { res = temp.front(); }
		return res;
	}

	vector<string> SystemInfo::queryGenericString(const string& regexString) const {
		vector<string> retval;

		std::regex regex(regexString.c_str());
		std::match_results<string::const_iterator> matches;

		string::const_iterator start = data.begin();

		while(std::regex_search(start, data.cend(), matches, regex)) {
			// save all capturing groups
			for(unsigned i = 1; i < matches.size(); ++i) {
				retval.push_back(matches[i]);
			}
			// move iterator
			start = matches[0].second;
		}

		/**
		 * TODO:
		 * systeminfo inheritance, implement a base class and move the current content to autotuning systeminfo subclass
		 */

		return retval;
	}

} // end namespace measure
} // end namespace driver
} // end namespace insieme
