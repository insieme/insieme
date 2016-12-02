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

#include "insieme/driver/utils/object_file_utils.h"

#include <fstream>
#include <boost/filesystem.hpp>

#include "insieme/utils/logging.h"

#include "insieme/core/tu/ir_translation_unit.h"
#include "insieme/core/tu/ir_translation_unit_io.h"

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/utils/file_extensions.h"

namespace insieme {
namespace driver {
namespace utils {

	namespace {

		// some magic number to identify our files
		const long MAGIC_NUMBER = 42 * 42 * 42 * 42;
	}

	bool isInsiemeLib(const boost::filesystem::path& file) {
		// check existence
		if(!boost::filesystem::exists(file)) { return false; }

		// open file
		std::ifstream in(file.string(), std::ios::in | std::ios::binary);

		// consume the magic number
		long x;
		in >> x;

		// check magic number
		return x == MAGIC_NUMBER;
	}

	core::tu::IRTranslationUnit loadLib(core::NodeManager& mgr, const boost::filesystem::path& file) {
		assert_true(isInsiemeLib(file));

		// open file
		std::ifstream in(file.string(), std::ios::in | std::ios::binary);

		// consume the magic number
		long x;
		in >> x;
		assert(x == MAGIC_NUMBER);

		// load content
		return core::tu::load(in, mgr);
	}

	void saveLib(const core::tu::IRTranslationUnit& unit, const boost::filesystem::path& file) {
		// create all necessary directory
		boost::filesystem::create_directories(boost::filesystem::absolute(file).parent_path());

		std::ofstream out(file.string(), std::ios::out | std::ios::binary);
		out << MAGIC_NUMBER;           // start with magic number
		core::tu::dump(out, unit); // dump the rest

		assert_true(boost::filesystem::exists(file));
	}

	bool filterInputFiles(core::NodeManager& mgr, insieme::frontend::ConversionJob& job) {
		std::vector<frontend::path> inputs;
		std::vector<frontend::path> libs;
		std::vector<frontend::path> extLibs;

		for(const frontend::path& cur : job.getFiles()) {
			auto ext = boost::filesystem::extension(cur);
			if(ext == ".o" || ext == ".so") {
				if(isInsiemeLib(cur)) {
					libs.push_back(cur);
				} else {
					extLibs.push_back(cur);
				}
			} else if (frontend::utils::cExtensions.count(ext) || frontend::utils::cxxExtensions.count(ext)) {
				inputs.push_back(cur);
			} else {
				LOG(ERROR) << "Unrecognized file format: " << cur << "\n";
				return false;
			}
		}

		// update input files
		job.setFiles(inputs);
		job.setExtLibs(extLibs);

		// load libraries
		job.setLibs(::transform(libs, [&](const frontend::path& cur) {
			std::cout << "Loading " << cur << " ...\n";
			return loadLib(mgr, cur);
		}));
		return true;
	}

} // end namespace utils
} // end namespace driver
} // end namespace insieme
