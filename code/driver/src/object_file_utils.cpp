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

#include <fstream>

#include <boost/filesystem.hpp>

#include "insieme/driver/object_file_utils.h"

#include "insieme/frontend/tu/ir_translation_unit_io.h"

namespace insieme {
namespace driver {

	namespace {

		// some magic number to identify our files
		const long MAGIC_NUMBER = 42*42*42*42;

	}

	bool isInsiemeLib(const boost::filesystem::path& file) {

		// check existence
		if (!boost::filesystem::exists(file)) return false;

		// open file
		std::ifstream in(file.string(), std::ios::in | std::ios::binary);

		// consume the magic number
		long x; in >> x;

		// check magic number
		return x == MAGIC_NUMBER;
	}

	frontend::tu::IRTranslationUnit loadLib(core::NodeManager& mgr, const boost::filesystem::path& file) {
		assert(isInsiemeLib(file));

		// open file
		std::ifstream in(file.string(), std::ios::in | std::ios::binary);

		// consume the magic number
		long x; in >> x; assert(x == MAGIC_NUMBER);

		// load content
		return frontend::tu::load(in, mgr);
	}

	void saveLib(const frontend::tu::IRTranslationUnit& unit, const boost::filesystem::path& file) {

		// create all necessary directory
		boost::filesystem::create_directories(file.parent_path());

		std::ofstream out(file.string(), std::ios::out | std::ios::binary );
		out << MAGIC_NUMBER;		// start with magic number
		frontend::tu::dump(out, unit);	// dump the rest

		assert(boost::filesystem::exists(file));
	}

} // end namespace driver
} // end namespace insieme
