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

#include "insieme/driver/measure/measure.h"

#include <memory>
#include <boost/filesystem.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace measure {


	namespace {

		int runCommand(const std::string& cmd) {
			LOG(DEBUG) << "Running " << cmd << "\n";
			//			return system((cmd + "> /dev/null").c_str());
			return system(cmd.c_str());
		}

		string setupEnv(const std::map<string, string>& env) {
			if(env.empty()) { return ""; }

			std::stringstream res;
			res << join(" ", env, [](std::ostream& out, const std::pair<string, string>& cur) { out << cur.first << "=" << cur.second; });
			return res.str();
		}
	}


	int LocalExecutor::run(const std::string& binary, const std::map<string, string>& env, const string& dir) const {
	// create output directory
	#ifndef DISABLE_ENERGY
		// set capabilities for energy measurements, required for kernel versions 3.7 and newer
		// TODO: only do this for newer kernel versions or kernels that have this fix
		runCommand("sudo setcap cap_sys_rawio=ep " + binary);
		#endif
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
		boost::uuids::uuid u = uuidGen();
		dirName += toString(u);

		// create ssh-url
		std::string url = hostname;
		if(!username.empty()) { url = username + "@" + hostname; }

		std::string remoteDir = workdir + "/_remote_" + dirName;

		int res = 0;

		// start by creating a remote working directory
		if(res == 0) { res = runCommand("ssh " + url + " mkdir " + remoteDir); }

		// copy binary
		if(res == 0) { res = runCommand("scp -q " + binary + " " + url + ":" + remoteDir); }

		// execute binary
		switch(system) {
		case SSH:
			if(res == 0) {
				res = runCommand("ssh " + url + " \"cd " + remoteDir + " && " + setupEnv(env) + " ./" + binaryName + " && rm " + binaryName + "\"");
			}
			break;
		case SGE:
			if(res == 0) {
				res =
				    runCommand("ssh " + url + " \"cd " + remoteDir + " && qsub -sync yes -b yes -cwd -o std.out -e std.err -pe openmp 1 " + binaryName + "\"");
			}
			if(res == 0) { res = runCommand("ssh " + url + " \"cd " + remoteDir + " && rm " + binaryName + "\""); }
			break;
		case PBS:
			assert_fail() << "Not tested!";
			if(res == 0) {
				res = runCommand("ssh " + url + " \"cd " + remoteDir + " && qsub -l select=1:ncpus=4:mem=2gb -W block=true -- ./" + binaryName + "\"");
			}
			if(res == 0) { res = runCommand("ssh " + url + " \"cd " + remoteDir + " && rm " + binaryName + "\""); }
			break;
		case LL:
			std::string jobscript = "#!/bin/bash\n"
			                        "#@ energy_policy_tag=my_energy_tag\n"
			                        "#@ max_perf_decrease_allowed=1\n"
			                        "#@ wall_clock_limit = 00:05:00\n"
			                        "#@ job_name = insieme\n"
			                        "#@ job_type = parallel\n"
			                        "#@ class = test\n"
			                        "#@ node = 1\n"
			                        "#@ total_tasks = 1\n"
			                        "#@ node_usage = not_shared\n"
			                        "#@ initialdir = "
			                        + remoteDir + "\n"
			                                      "#@ output = job$(jobid).out\n"
			                                      "#@ error = job$(jobid).err\n"
			                                      "#@ notification=error\n"
			                                      "#@ notify_user=philipp.gschwandtner@uibk.ac.at\n"
			                                      "#@ restart=no\n"
			                                      "#@ queue\n"
			                                      ". /etc/profile\n"
			                                      ". /etc/profile.d/modules.sh\n"
			                        + setupEnv(env);
			if(res == 0) { res = runCommand("ssh " + url + " \"cd " + remoteDir + " && echo \"" + jobscript + "\" | llsubmit -s -\""); }
			// std::cout << "ssh " + url + " \"cd " + remoteDir + " && echo \"" + jobscript + "\" | llsubmit -s -\"\n";
			// if (res==0) res = runCommand("ssh " + url + " \"cd " + remoteDir + " && "  + setupEnv(env) + " ./" + binaryName + " && rm " + binaryName + "\"");
			// if (res==0) res = runCommand("ssh " + url + " \"cd " + remoteDir + " && rm " + binaryName + "\"");
			break;
		}

		// copy back log files
		if(res == 0) { res = runCommand("scp -q -r " + url + ":" + remoteDir + " ."); }

		// move files locally
		if(res == 0) { res = runCommand("mv -t " + dir + " _remote_" + dirName + "/*"); }

		// delete local files
		if(res == 0) { res = runCommand("rm -rf _remote_" + dirName); }

		// delete remote working directory
		if(res == 0) { res = runCommand("ssh " + url + " rm -rf " + remoteDir); }

		return res;
	}


	ExecutorPtr makeRemoteExecutor(const std::string& hostname, const std::string& username, const std::string& remoteWorkDir,
	                               RemoteExecutor::JobSystem system) {
		return std::make_shared<RemoteExecutor>(hostname, username, remoteWorkDir, system);
	}

	ExecutorPtr makeRemoteSSHExecutor(const std::string& hostname, const std::string& username, const std::string& remoteWorkDir) {
		return makeRemoteExecutor(hostname, username, remoteWorkDir, RemoteExecutor::SSH);
	}

	ExecutorPtr makeRemoteSGEExecutor(const std::string& hostname, const std::string& username, const std::string& remoteWorkDir) {
		return makeRemoteExecutor(hostname, username, remoteWorkDir, RemoteExecutor::SGE);
	}

	ExecutorPtr makeRemotePBSExecutor(const std::string& hostname, const std::string& username, const std::string& remoteWorkDir) {
		return makeRemoteExecutor(hostname, username, remoteWorkDir, RemoteExecutor::PBS);
	}

	ExecutorPtr makeRemoteLLExecutor(const std::string& hostname, const std::string& username, const std::string& remoteWorkDir) {
		return makeRemoteExecutor(hostname, username, remoteWorkDir, RemoteExecutor::LL);
	}


} // end namespace measure
} // end namespace driver
} // end namespace insieme
