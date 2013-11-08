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

		utils::net::NetworkPath getTempDir() const { return utils::net::NetworkPath(hostname, username, tmpdir); }
	};

	boost::optional<utils::net::NetworkPath> buildRemote(
			const utils::VirtualPrintable& source, const Host& targetHost,
			const utils::compiler::Compiler& compiler = utils::compiler::Compiler::getDefaultC99Compiler()
	);

	template<typename Printable>
	boost::optional<utils::net::NetworkPath> buildRemote(
			const Printable& source, const Host& targetHost,
			const utils::compiler::Compiler& compiler = utils::compiler::Compiler::getDefaultC99Compiler()
	) {
		return buildRemote(utils::toVirtualPrintable(source), targetHost, compiler);
	}

	/**
	 * Compiles the given source file using the given compiler setup  to the given target file.
	 * The file will be compiled on the target system using
	 */
	boost::optional<utils::net::NetworkPath> buildRemote(
			const vector<utils::net::NetworkPath>& sources, const string& targetFileName, const Host& targetHost,
			const utils::compiler::Compiler& compilerSetup = utils::compiler::Compiler::getDefaultC99Compiler()
	);

} // end namespace measure
} // end namespace driver
} // end namespace insieme
