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

#include <map>
#include <boost/filesystem/path.hpp>

#include "insieme/frontend/tu/ir_translation_unit.h"

namespace insieme {
namespace frontend {

	using std::map;

	// a definition for the kind of path to be utilized by the following translation unit
	typedef boost::filesystem::path path;

	/**
	 * An entity describing a unit of work for the clang frontend conversion process.
	 */
	class ConversionSetup {
	public:

		/**
		 * A list of options to adjust the translation unit conversion.
		 */
		enum Option {
			PrintDiag		= 1<<0,

			OpenMP			= 1<<1,
			OpenCL			= 1<<2,
			Cilk			= 1<<3,

			WinCrossCompile	= 1<<4,
			TAG_MPI			= 1<<6,
		};

		/**
		 * A list of supported standards.
		 */
		enum Standard {
			Auto, C99, Cxx03
		};

		/**
		 * The default frontend configuration.
		 */
		static const unsigned DEFAULT_FLAGS;

	private:

		/**
		 * A list of include directories to be considered.
		 */
		vector<path> includeDirs;

		/**
		 * A list of include directories containing std library headers.
		 */
		vector<path> stdLibIncludeDirs;

		/**
		 * The C standard to be followed.
		 */
		Standard standard;

		/**
		 * A list of definitions to be passed to the preprocessor. Each
		 * entry maps an identifier to a value.
		 */
		map<string,string> definitions;

		/**
		 * The name of the configuration file of the intercepter.
		 */
		// TODO: this should not be a string pointing to a file!!!
		string intercepterConfigFile;

		/**
		 * Additional flags - a bitwise boolean combination of Options (see Option)
		 */
		unsigned flags;

	public:

		/**
		 * Creates a new setup covering the given include directories.
		 */
		ConversionSetup(const vector<path>& includeDirs = vector<path>());

		/**
		 * Allows to check for an option.
		 */
		bool hasOption(const Option option) const {
			return flags & option;
		}

		/**
		 * Updates the state of an option.
		 */
		void setOption(const Option option, bool status = true) {
			flags = (status)?(flags | option):( flags & ~option);
		}

		/**
		 * Updates the options set for the conversion process.
		 */
		void setOptions(unsigned options) {
			flags = options;
		}

		/**
		 * Obtains the standard to be used for parsing input files.
		 */
		const Standard& getStandard() const {
			return standard;
		}

		/**
		 * Updates the standard to be used for parsing input files.
		 */
		void setStandard(const Standard& standard) {
			this->standard = standard;
		}

		/**
		 * Obtains a reference to the currently defined definitions.
		 */
		const map<string,string>& getDefinitions() const {
			return definitions;
		}

		/**
		 * Updates the definitions to be used by the conversion process.
		 */
		void setDefinitions(const map<string,string>& definitions) {
			this->definitions = definitions;
		}

		/**
		 * Adds a pre-processor definition to this conversion job.
		 */
		void setDefinition(const string& name, const string& value = "") {
			this->definitions[name] = value;
		}

		/**
		 * Obtains a reference to the covered set of include directories.
		 */
		const vector<path>& getIncludeDirectories() const {
			return includeDirs;
		}

		/**
		 * Updates the set of considered include directories.
		 */
		void setIncludeDirectories(const vector<path>& includeDirectories) {
			this->includeDirs = includeDirectories;
		}

		/**
		 * Adds an additional include directory.
		 */
		void addIncludeDirectory(const path& directory) {
			this->includeDirs.push_back(directory);
		}

		/**
		 * Obtains a reference to the covered set of std-library include directories.
		 */
		const vector<path>& getStdLibIncludeDirectories() const {
			return stdLibIncludeDirs;
		}

		/**
		 * Updates the set of considered std-library include directories.
		 */
		void setStdLibIncludeDirectories(const vector<path>& includeDirectories) {
			this->stdLibIncludeDirs = includeDirectories;
		}

		/**
		 * Adds an additional std-library include directory.
		 */
		void addStdLibIncludeDirectory(const path& directory) {
			this->stdLibIncludeDirs.push_back(directory);
		}

		/**
		 * Obtains the name of the intercepter configuration file.
		 */
		const string& getIntercepterConfigFile() const {
			return intercepterConfigFile;
		}

		/**
		 * Updates the name of the intercepter configuration file.
		 */
		void setIntercepterConfigFile(const string& configFile) {
			this->intercepterConfigFile = configFile;
		}

		/**
		 * A utility method to determine whether the given file should be
		 * considered a C++ file or not. This decision will be influenced
		 * by the standard set within this setup. Only if set to auto
		 * (default) the extension will be evaluated.
		 */
		bool isCxx(const path& file) const;

	};


	/**
	 * This function converts a clang translation unit into an IR translation unit.
	 *
	 * @param manager the manager to be used for managing the resulting IR nodes
	 * @param unit the translation unit to be processed
	 * @param setup the setup for the conversion process to be respected
	 * @return the resulting translation unit
	 */
	tu::IRTranslationUnit convert(core::NodeManager& manager, const path& unit, const ConversionSetup& setup = ConversionSetup());

	/**
	 * This function converts a list of translation units into a list of IR translation units.
	 *
	 * @param manager the manager to be used for managing the resulting IR nodes
	 * @param units the translation units to be processed
	 * @param setup the setup for the conversion process to be respected
	 * @return the resulting translation units
	 */
	vector<tu::IRTranslationUnit> convert(core::NodeManager& manager, const vector<path>& units, const ConversionSetup& setup = ConversionSetup());


} // end namespace frontend
} // end namespace insieme
