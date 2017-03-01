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
#include "insieme/driver/measure/builder.h"

#include <sstream>

#include <boost/filesystem/operations.hpp>

#include "insieme/utils/config.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace driver {
namespace measure {

namespace bfs = boost::filesystem;
namespace nfs = utils::net;

int runCommand(const std::string& cmd) {
	LOG(ERROR) << "Running " << cmd << "\n";
	//		return system((cmd + "> /dev/null").c_str());
	return system(cmd.c_str());
}

boost::optional<utils::net::NetworkPath> buildRemote(const utils::VirtualPrintable& source, const Host& targetHost, const utils::compiler::Compiler& compiler) {
	// create a temporary local source file
	char sourceFile[] = P_tmpdir "/srcXXXXXX";
	int src = mkstemp(sourceFile);
	assert_ne(src, -1);
	close(src);

	// write source to file
	std::fstream srcFile(sourceFile, std::fstream::out);
	srcFile << source << "\n";
	srcFile.close();

	// build remotely
	auto res = buildRemote(toVector(nfs::NetworkPath(sourceFile)), "binary", targetHost, compiler);

	// delete source file
	if(boost::filesystem::exists(sourceFile)) { boost::filesystem::remove(sourceFile); }
	return res;
}

boost::optional<utils::net::NetworkPath> buildRemote(const vector<utils::net::NetworkPath>& sources, const string& targetFileName, const Host& targetHost,
                                                     const utils::compiler::Compiler& compilerSetup) {
	nfs::NetworkPath tmpDir = targetHost.getTempDir();

	// obtain a remote working directory
	int i = 0;
	while(!nfs::create_directories(tmpDir / format("work_dir_%d", i))) {
		i++;
	}
	nfs::NetworkPath workDir = tmpDir / format("work_dir_%d", i);
	assert_true(nfs::exists(workDir));

	bool res = true;

	// create build directory
	nfs::NetworkPath buildDir = workDir / "build";
	res = res && nfs::create_directories(buildDir);

	// copy local source to target machine (runtime is header only)
	res = res && nfs::copy(nfs::NetworkPath(utils::getInsiemeSourceRootDir() + "runtime/include"), buildDir / "include");

	// copy source file
	vector<string> inputFiles;
	for_each(sources, [&](const nfs::NetworkPath& src) {
		inputFiles.push_back(src.filename());
		res = res && nfs::copy(src, buildDir / inputFiles.back());
	});

	// build source file on remote machine
	res = res
	      && runCommand("ssh " + workDir.getUserHostnamePrefix() + " \""
	                                                               "cd "
	                    + buildDir.path.string() + " && " + compilerSetup.getCommand(inputFiles, "../" + targetFileName) + "\"")
	             == 0;

	// check whether everything was successful
	if(!res) {
		// clean up working directory
		nfs::remove_all(workDir);

		// return uninitialized optional containing no path
		return boost::optional<nfs::NetworkPath>();
	}

	// clean up sources
	nfs::remove_all(buildDir);

	// return path to binary
	return workDir / targetFileName;
}


} // end namespace measure
} // end namespace driver
} // end namespace insieme
