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

#include "insieme/driver/measure/executor.h"

#include <memory>
#include <set>
#include <boost/filesystem.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "insieme/driver/measure/hardware.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace measure {

	namespace {

		const std::string rsyncCmd = "rsync --whole-file --quiet --archive";
		const std::string setcapCmd = "sudo setcap cap_sys_rawio=ep";

		int runCommand(const std::string& cmd, bool silent) {
			std::string modCmd = cmd;
			if(silent) { modCmd.append(" > /dev/null"); }
			LOG(INFO) << "Running " << modCmd << "\n";
			return system(modCmd.c_str());
		}

		string setupEnv(const std::map<string, string>& env) {
			if(env.empty()) { return ""; }

			std::stringstream res;
			res << join(" ", env, [](std::ostream& out, const std::pair<string, string>& cur) { out << cur.first << "=" << cur.second; });
			return res.str();
		}
	}


	int LocalExecutor::run(const std::string& binary, const ExecutionSetup& setup) const {
		// set capabilities for energy measurements, required for kernel versions 3.7 and newer
		if(setup.requiresRawIOCapabilities) { runCommand(setcapCmd + " " + binary, true); }
		std::stringstream ss;
		ss << setupEnv(setup.env) << " IRT_INST_OUTPUT_PATH=" << setup.outputDirectory << " " << wrapper << " " << binary << " " << join(" ", setup.params);
		return runCommand(ss.str(), setup.silent);
	}

	ExecutorPtr makeLocalExecutor() {
		return std::make_shared<LocalExecutor>();
	}

	int RemoteExecutor::run(const std::string& binary, const ExecutionSetup& setup) const {

		// extract directory name
		string dirName = boost::filesystem::path(setup.outputDirectory).filename().string();
		boost::uuids::uuid u = uuidGen();
		dirName += toString(u);

		// create ssh-url
		std::string url = hostname;
		if(!username.empty()) { url = username + "@" + hostname; }

		std::string remoteDir = workdir + "/_remote_" + dirName;

		int res = 0;

		// start by creating a remote working directory
		res = runCommand("ssh " + url + " mkdir " + remoteDir, setup.silent);
		assert_eq(res, 0) << "Failed to create remote directory " << remoteDir << " on " << url;

		string binaryName = binary;
		if(setup.requiresBinaryCopying) {
			// copy binary if required
			res = runCommand("scp -q " + binary + " " + url + ":" + remoteDir, setup.silent);
			assert_eq(res, 0) << "Failed to copy binary " << binary << " to " << url << ":" << remoteDir;
			// change path to new relative location of new binary
			binaryName = string("./") + boost::filesystem::path(binaryName).filename().string();
		}

		// execute binary
		std::stringstream ss;
		ss << "ssh " << url << " \"cd " << remoteDir << " && " << setupEnv(setup.env) << " " << binaryName << " " << join(" ", setup.params);
		if(setup.requiresBinaryCopying) { ss << " && rm " << binaryName; }
		ss << "\"";
		res = runCommand(ss.str(), setup.silent);
		assert_eq(res, 0) << "Failed to execute binary" << binaryName << " on " << url;

		// copy back remote directory
		// use rsync since it is as fast as scp, does not fail on empty directories and supports moving instead of copying
		res = runCommand(rsyncCmd + " --remove-source-files " + url + ":" + remoteDir + "/ " + setup.outputDirectory, setup.silent);
		assert_eq(res, 0) << "Failed to move generated files with rsync from " << remoteDir << " on " << url << " to " << setup.outputDirectory;
		res = runCommand("ssh " + url + " rm -rf " + remoteDir, setup.silent);
		assert_eq(res, 0) << "Failed to delete remote directory " << remoteDir << " on " << url;

		return res;
	}

	int MPIExecutor::run(const std::string& binary, const ExecutionSetup& setup) const {
		int res = 0;

		// extract directory name
		string dirName = boost::filesystem::path(setup.outputDirectory).filename().string();
		boost::uuids::uuid u = uuidGen();
		dirName += toString(u);
		string newBinaryLocation = binary;

		const std::string remoteWorkDir = "/tmp/" + dirName;
		if(setup.requiresBinaryCopying) { newBinaryLocation = remoteWorkDir + "/" + boost::filesystem::path(binary).filename().string(); }

		std::stringstream ss;
		// copy binary to all hosts
		for(const auto& h : setup.machine.getNodes()) {
			if(setup.requiresBinaryCopying) {
				res = runCommand(rsyncCmd + " " + binary + " " + h.getHostname() + ":" + remoteWorkDir + "/", setup.silent);
				assert_eq(res, 0) << "Failed to copy binary " << binary << " to " << h.getHostname() << ":" << remoteWorkDir;
				if(setup.requiresRawIOCapabilities) {
					res = runCommand("ssh -q -tt " + h.getHostname() + " " + setcapCmd + " " + newBinaryLocation, setup.silent);
					assert_eq(res, 0) << "Failed to set rawio capabilities for binary " << newBinaryLocation << " on " << h.getHostname();
				}
			} else {
				res = runCommand("ssh " + h.getHostname() + " mkdir -p " + remoteWorkDir, setup.silent);
				assert_eq(res, 0) << "Failed to create directory " << remoteWorkDir << " on " << h.getHostname();
			}
		}

		std::vector<std::string> hosts;
		for(const auto& n : setup.machine.getNodes()) {
			for(unsigned i = 0; i < n.getNumCores(); ++i) {
				hosts.push_back(n.getHostname());
			}
		}
		std::stringstream mpirun;
		mpirun << "mpirun -wdir " << remoteWorkDir << " -np " << setup.machine.getNumCores() << " " << options << " --host " << join(",", hosts);
		// forward all environment variables
		for(const auto& e : setup.env) {
			mpirun << " -x " << e.first;
		}
		ss << " " << setupEnv(setup.env) << " " << mpirun.str() << " " << newBinaryLocation << " " << join(" ", setup.params);

		// run mpi program
		res = runCommand(ss.str(), setup.silent);
		assert_eq(res, 0) << "Failed to run MPI job";

		// collect generated files
		for(const auto& h : setup.machine.getNodes()) {
			// use rsync since it is as fast as scp, does not fail on empty directories and supports moving instead of copying
			res = runCommand(rsyncCmd + " --remove-source-files " + h.getHostname() + ":" + remoteWorkDir + "/ " + setup.outputDirectory, setup.silent);
			assert_eq(res, 0) << "Failed to move generated files with rsync from " << remoteWorkDir << " on " << h.getHostname() << " to " << setup.outputDirectory;
			res = runCommand("ssh " + h.getHostname() + " rm -rf " + remoteWorkDir, setup.silent);
			assert_eq(res, 0) << "Failed to delete remote directory " << remoteWorkDir << " on " << h.getHostname();
		}

		return res;
	}

	ExecutorPtr makeRemoteExecutor(const std::string& hostname, const std::string& username, const std::string& remoteWorkDir) {
		return std::make_shared<RemoteExecutor>(hostname, username, remoteWorkDir);
	}

	ExecutorPtr makeMPIExecutor(const std::string& options) { return std::make_shared<MPIExecutor>(options); }

} // end namespace measure
} // end namespace driver
} // end namespace insieme
