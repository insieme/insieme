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

#include <boost/filesystem/path.hpp>

#include "insieme/utils/compiler/compiler.h"

namespace insieme {
namespace utils {
namespace net {

	using std::string;

	namespace bfs = boost::filesystem;

	/**
	 * The class used to represent paths to files within a network.
	 */
	class NetworkPath : public utils::Printable {
		/**
		 * The name of the host the file is located - empty
		 * for local hosts.
		 */
		string hostname;

		/**
		 * The name of the user to be used to log in on the remote
		 * host location - empty if the current users name should be used.
		 */
		string username;

	  public:
		/**
		 * The location of the file on the remote host.
		 */
		bfs::path path;

		NetworkPath(){};

		NetworkPath(const bfs::path& path);

		NetworkPath(const string& hostname, const bfs::path& path);

		NetworkPath(const string& hostname, const string& username, const bfs::path& path);

		// getter
		const string& getHostname() const {
			return hostname;
		}
		const string& getUsername() const {
			return username;
		}

		bool isLocal() const {
			return hostname.empty();
		}

		string filename() const {
			return path.filename().string();
		}
		string getUserHostnamePrefix() const;

		// navigation

		NetworkPath parent_path() const;

		// operators
		bool operator==(const NetworkPath& other) const;

		bool operator!=(const NetworkPath& other) const {
			return !(*this == other);
		}

		NetworkPath& operator/=(const bfs::path& path);
		NetworkPath operator/(const bfs::path& path) const;

		virtual std::ostream& printTo(std::ostream& out) const;
	};

	bool exists(const NetworkPath& path);

	bool is_directory(const NetworkPath& path);

	bool create_directories(const NetworkPath& path);

	bool remove(const NetworkPath& path);

	bool remove_all(const NetworkPath& path);

	bool copy(const NetworkPath& src, const NetworkPath& trg);

	/**
	 * Compiles the given source file using the given compiler setup  to the given target file.
	 * The file will be compiled on the target system using
	 */
	bool buildRemote(const bfs::path& source, const NetworkPath& target,
	                 const utils::compiler::Compiler& compilerSetup = utils::compiler::Compiler::getDefaultC99Compiler(),
	                 const bfs::path& remoteWorkDir = "/tmp");


} // end namespace net
} // end namespace utils
} // end namespace insieme
