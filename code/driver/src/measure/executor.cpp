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

#include "insieme/driver/measure/measure.h"

#include <memory>
#include <boost/filesystem.hpp>

#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace measure {


	namespace {

		int runCommand(const std::string& cmd) {
			LOG(DEBUG) << "Running " << cmd << "\n";
			return system((cmd + "> /dev/null").c_str());
//			return system(cmd.c_str());
		}

		string setupEnv(const std::map<string,string>& env) {
			if (env.empty()) return "";

			std::stringstream res;
			res << join(" ", env, [](std::ostream& out, const std::pair<string,string>& cur) {
				out << cur.first << "=" << cur.second;
			});
			return res.str();
		}

	}


	int LocalExecutor::run(const std::string& binary, const std::map<string, string>& env, const string& dir) const {
		// create output directory
		return runCommand(setupEnv(env) + " IRT_INST_OUTPUT_PATH=" + dir + " " + binary.c_str());
	}

	ExecutorPtr makeLocalExecutor() {
		return std::make_shared<LocalExecutor>();
	}


	int RemoteExecutor::run(const std::string& binary, const std::map<string, string>& env, const string& dir) const {

		// extract name of file
		boost::filesystem::path path = binary;
		string binaryName = path.filename().string();

		// extract directory name
		string dirName = boost::filesystem::path(dir).filename().string();

		// create ssh-url
		std::string url = hostname;
		if (!username.empty()) {
			url = username + "@" + hostname;
		}

		std::string remoteDir = workdir + "/_remote_" + dirName;

		int res = 0;

		// start by creating a remote working directory
		if (res==0) res = runCommand("ssh " + url + " mkdir " + remoteDir);

		// copy binary
		if (res==0) res = runCommand("scp -q " + binary + " " + url + ":" + remoteDir);

		// execute binary
		if (res==0) res = runCommand("ssh " + url + " \"cd " + remoteDir + " && "  + setupEnv(env) + " ./" + binaryName + " && rm " + binaryName + "\"");

		// copy back log files
		if (res==0) res = runCommand("scp -q -r " + url + ":" + remoteDir + " .");

		// move files locally
		if (res==0) res = runCommand("mv -t " + dir + " _remote_" + dirName + "/*");

		// delete local files
		if (res==0) res = runCommand("rm -rf _remote_" + dirName);

		// delete remote working directory
		if (res==0) res = runCommand("ssh " + url + " rm -rf " + remoteDir);

		return res;

	}


	ExecutorPtr makeRemoteExecutor(const std::string& hostname, const std::string& username, const std::string& remoteWorkDir) {
		return std::make_shared<RemoteExecutor>(hostname, username, remoteWorkDir);
	}

} // end namespace measure
} // end namespace driver
} // end namespace insieme

