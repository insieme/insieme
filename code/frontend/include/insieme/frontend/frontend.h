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
#include <map>

#include <boost/filesystem/path.hpp>

#include "insieme/frontend/clang.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_program.h"

namespace insieme {

namespace core {
	// some forward declarations
	class NodeManager;
	class Program;
	template<typename T> class Pointer;
	typedef Pointer<const Program> ProgramPtr;
}

namespace frontend {

	typedef boost::filesystem::path path;

	/**
	 * Used to report a parsing error occurred during the parsing of the input file
	 */
	struct ClangParsingError: public std::logic_error {
		ClangParsingError(const path& file_name): std::logic_error(file_name.string()) { }
	};


	using std::vector;
	using std::string;

	class ConversionJob : public ConversionSetup {

		/**
		 * The translation units to be converted.
		 */
		vector<path> files;

		/**
		 * Extra libraries to be considered for the conversion.
		 */
		vector<tu::IRTranslationUnit> libs;

	public:

		/**
		 * Creates a new conversion job covering a single file.
		 */
		ConversionJob(const path& file, const vector<path>& includeDirs = vector<path>())
			: ConversionSetup(includeDirs), files(toVector(file)) {}

		/**
		 * Creates a new conversion job covering the given files.
		 */
		ConversionJob(const vector<path>& files, const vector<path>& includeDirs = vector<path>())
			: ConversionSetup(includeDirs), files(files) {
			assert(!files.empty());
		}

		/**
		 * Obtains the one input files covered by this conversion job.
		 */
		const vector<path>& getFiles() const {
			return files;
		}

		/**
		 * Adds an additonal file to this conversion job.
		 */
		void addFile(const path& file) {
			files.push_back(file);
		}

		/**
		 * Exchanges the files covered by this conversion job by the given files.
		 */
		void setFiles(const vector<path>& files) {
			this->files = files;
		}

		/**
		 * Obtains a reference to the libs to be considered by this conversion job.
		 */
		const vector<tu::IRTranslationUnit>& getLibs() const {
			return libs;
		}

		/**
		 * Sets the libs to be considered by this conversion job.
		 */
		void setLibs(const vector<tu::IRTranslationUnit>& libs) {
			this->libs = libs;
		}

		/**
		 * Appends a library to the list of libraries considered by this conversion job.
		 */
		void addLib(const tu::IRTranslationUnit& unit) {
			libs.push_back(unit);
		}

		/**
		 * Triggers the actual conversion. The previously set up parameters will be used to attempt a conversion.
		 *
		 * @param manager the node manager to be used for building the IR
		 * @return the resulting, converted program
		 * @throws an exception if the conversion fails.
		 */
		core::ProgramPtr execute(core::NodeManager& manager) const;

		/**
		 * Triggers the conversion of the files covered by this job into a translation unit.
		 *
		 * @param manager the node manager to be used for building the IR
		 * @return the resulting, converted program
		 * @throws an exception if the conversion fails.
		 */
		tu::IRTranslationUnit toTranslationUnit(core::NodeManager& manager) const;

	};


} // end namespace frontend
} // end namespace insieme
