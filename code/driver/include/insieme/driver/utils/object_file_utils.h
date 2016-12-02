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
