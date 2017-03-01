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
#include "insieme/utils/net/net_path.h"

#include <sstream>

#include <boost/filesystem/operations.hpp>

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {
namespace net {

namespace bfs = boost::filesystem;

int runCommand(const std::string& cmd) {
	LOG(DEBUG) << "Running " << cmd << "\n";
	return system((cmd + "> /dev/null").c_str());
	//		return system(cmd.c_str());
}

NetworkPath::NetworkPath(const bfs::path& path) : hostname(""), username(""), path(path) {}

NetworkPath::NetworkPath(const string& hostname, const bfs::path& path) : hostname(hostname), username(""), path(path) {}

NetworkPath::NetworkPath(const string& hostname, const string& username, const bfs::path& path) : hostname(hostname), username(username), path(path) {}

string NetworkPath::getUserHostnamePrefix() const {
	if(isLocal()) { return ""; }
	std::stringstream res;
	if(!username.empty()) { res << username << "@"; }
	res << hostname;
	return res.str();
}

NetworkPath NetworkPath::parent_path() const {
	return NetworkPath(hostname, username, path.parent_path());
}

bool NetworkPath::operator==(const NetworkPath& other) const {
	return hostname == other.hostname && username == other.username && path == other.path;
}

NetworkPath& NetworkPath::operator/=(const bfs::path& ext) {
	path = path / ext;
	return *this;
}

NetworkPath NetworkPath::operator/(const bfs::path& ext) const {
	NetworkPath res = *this;
	res /= ext;
	return res;
}

std::ostream& NetworkPath::printTo(std::ostream& out) const {
	if(!username.empty()) { return out << username << "@" << hostname << ":" << path.string(); }
	if(!hostname.empty()) { return out << hostname << ":" << path.string(); }
	return out << path.string();
}


bool exists(const NetworkPath& path) {
	if(path.isLocal()) { return bfs::exists(path.path); }

	// check remotely using an ssh connection
	return runCommand("ssh " + path.getUserHostnamePrefix() + " test -e " + toString(path.path)) == 0;
}

bool is_directory(const NetworkPath& path) {
	if(path.isLocal()) { return bfs::is_directory(path.path); }
	return runCommand("ssh " + path.getUserHostnamePrefix() + " test -d " + toString(path.path)) == 0;
}

bool create_directories(const NetworkPath& path) {
	if(path.isLocal()) { return bfs::create_directories(path.path); }
	string dir = toString(path.path);
	return runCommand("ssh " + path.getUserHostnamePrefix() + " \"test ! -e " + dir + " && mkdir -p " + dir + "\"") == 0;
}

bool remove(const NetworkPath& path) {
	if(path.isLocal()) { return bfs::remove(path.path); }
	string file = toString(path.path);
	return runCommand("ssh " + path.getUserHostnamePrefix() + " \"test -f " + file + " && rm " + file + "\"") == 0;
}

bool remove_all(const NetworkPath& path) {
	if(path.isLocal()) { return bfs::remove_all(path.path); }
	string dir = toString(path.path);
	return runCommand("ssh " + path.getUserHostnamePrefix() + " \"test -e " + dir + " && rm -rf " + dir + "\"") == 0;
}

bool copy(const NetworkPath& src, const NetworkPath& trg) {
	string srcFile = toString(src);
	string trgFile = toString(trg);
	return runCommand("scp -r -p " + srcFile + " " + trgFile) == 0;
}

} // end namespace net
} // end namespace utils
} // end namespace insieme
