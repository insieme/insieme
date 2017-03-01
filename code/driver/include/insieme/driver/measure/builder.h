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
#pragma once

#include <string>
#include <vector>

#include <boost/optional.hpp>
#include <boost/filesystem/path.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/net/net_path.h"
#include "insieme/utils/compiler/compiler.h"

namespace insieme {
namespace driver {
namespace measure {

	using std::string;
	using std::vector;

	struct Host {
		string hostname;
		string username;
		string tmpdir;
		string papi_home;

		Host(const string& hostname, const string& username = "", const string& papi_home = "~/libs/papi-latest", const string& tmpdir = "/tmp")
		    : hostname(hostname), username(username), tmpdir(tmpdir), papi_home(papi_home) {}

		utils::net::NetworkPath getTempDir() const {
			return utils::net::NetworkPath(hostname, username, tmpdir);
		}
	};

	boost::optional<utils::net::NetworkPath> buildRemote(const utils::VirtualPrintable& source, const Host& targetHost,
	                                                     const utils::compiler::Compiler& compiler = utils::compiler::Compiler::getDefaultC99Compiler());

	template <typename Printable>
	boost::optional<utils::net::NetworkPath> buildRemote(const Printable& source, const Host& targetHost,
	                                                     const utils::compiler::Compiler& compiler = utils::compiler::Compiler::getDefaultC99Compiler()) {
		return buildRemote(utils::toVirtualPrintable(source), targetHost, compiler);
	}

	/**
	 * Compiles the given source file using the given compiler setup  to the given target file.
	 * The file will be compiled on the target system using
	 */
	boost::optional<utils::net::NetworkPath> buildRemote(const vector<utils::net::NetworkPath>& sources, const string& targetFileName, const Host& targetHost,
	                                                     const utils::compiler::Compiler& compilerSetup = utils::compiler::Compiler::getDefaultC99Compiler());

} // end namespace measure
} // end namespace driver
} // end namespace insieme
