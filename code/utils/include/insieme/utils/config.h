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

#include <boost/filesystem/path.hpp>

namespace insieme {
namespace utils {

	using std::string;

	const boost::filesystem::path up("../");

	inline string getInsiemeSourceRootDir() {
		return (boost::filesystem::path(__FILE__).parent_path() / up / up / up / up).string();
	}

	inline string getInsiemeTestRootDir() {
		return getInsiemeSourceRootDir() + "/../test/";
	}

	inline string getInsiemeLibsRootDir() {
		return string(INSIEME_LIBS_HOME); // INSIEME_LIBS_HOME is supplied by cmake
	}

	inline string getInsiemeBuildRootDir() {
		return string(INSIEME_BUILD_ROOT); // INSIEME_BUILD_ROOT is supplied by cmake
	}

	inline string getPapiRootDir() {
	#ifdef PAPI_ROOT_DIR
		return string(PAPI_ROOT_DIR); // PAPI_ROOT_DIR is supplied by cmake
	#else
		return string("");
	#endif
	}

} // end namespace utils
} // end namespace insieme
