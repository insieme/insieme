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
namespace core {
	class NodeManager;
namespace tu {
	class IRTranslationUnit;
}
}
namespace frontend {
	class ConversionJob;
}
namespace driver {
namespace utils {

	/**
	 * Checks whether the given file is a Insieme library file.
	 *
	 * @param file the file to be tested
	 * @return true if so, false otherwise
	 */
	bool isInsiemeLib(const boost::filesystem::path& file);

	/**
	 * Loads an Insieme library file.
	 *
	 * @param mgr the node manager to maintain the resulting IR nodes
	 * @param file the library to be loaded
	 * @return the loaded translation unit
	 */
	core::tu::IRTranslationUnit loadLib(core::NodeManager& mgr, const boost::filesystem::path& file);

	/**
	 * Saves an Insieme library to a file.
	 *
	 * @param unit the translation unit to be saved
	 * @param file the target location
	 */
	void saveLib(const core::tu::IRTranslationUnit& unit, const boost::filesystem::path& file);

	/**
	 * Filters the input files in the given conversion job, correctly handling the loading of libraries.
	 * The passed ConversionJob object will be modified accordingly.
	 *
	 * @return whether the files could be filtered correctly.
	 */
	bool filterInputFiles(core::NodeManager& mgr, insieme::frontend::ConversionJob& job);

} // end namespace utils
} // end namespace driver
} // end namespace insieme
