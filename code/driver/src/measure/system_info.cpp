/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
		auto testCode = insieme::driver::integration::getCase("hello_world")->load(manager);
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
