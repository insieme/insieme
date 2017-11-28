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

#include <functional>

#include <boost/filesystem/path.hpp>

#include "insieme/core/ir.h"
#include "insieme/frontend/frontend.h"

namespace insieme {
namespace frontend {
namespace utils {

	// ------- Test Utils -----

	namespace detail {

		void defaultConfig(insieme::frontend::ConversionJob&);

	}

	using JobConfigurator = std::function<void(insieme::frontend::ConversionJob&)>;

	/**
	 * A utility parsing a given file using the insieme frontend.
	 *
	 * @param mgr the manager used for creating the resulting program
	 * @param file the path to the file name to parse
	 * @param a job configurator specializing to configure the frontend conversion job
	 * @return the parsed program
	 */
	insieme::core::ProgramPtr parseFile(insieme::core::NodeManager& mgr, const boost::filesystem::path& file, const JobConfigurator& configurator = detail::defaultConfig);

	/**
	 * A utility parsing a given C/C++ code using the insieme frontend.
	 *
	 * @param mgr the manager used for creating the resulting program
	 * @param code the code to be parsed
	 * @param a job configurator specializing to configure the frontend conversion job
	 * @return the parsed program
	 */
	insieme::core::ProgramPtr parseCode(insieme::core::NodeManager& mgr, const std::string& code, const JobConfigurator& configurator = detail::defaultConfig);

	/**
	 * A utility parsing a given C/C++ type using the insieme frontend.
	 *
	 * @param mgr the manager used for creating the resulting program
	 * @param type the type to be parsed
	 * @param a job configurator specializing to configure the frontend conversion job
	 * @return the parsed type
	 */
	insieme::core::TypePtr parseType(insieme::core::NodeManager& mgr, const std::string& type, const JobConfigurator& configurator = detail::defaultConfig);

	/**
	 * A utility parsing a given C/C++ type using the insieme frontend.
	 *
	 * @param mgr the manager used for creating the resulting program
	 * @param header some header text to be included, e.g. include statements
	 * @param type the type to be parsed
	 * @param a job configurator specializing to configure the frontend conversion job
	 * @return the parsed type
	 */
	insieme::core::TypePtr parseType(insieme::core::NodeManager& mgr, const std::string& header, const std::string& type, const JobConfigurator& configurator = detail::defaultConfig);

} // end namespace utils
} // end namespace frontend
} // end namespace insieme
