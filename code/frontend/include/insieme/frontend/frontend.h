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
#include <string>
#include <vector>
#include <list>
#include <memory>

#include <boost/filesystem/path.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_program.h"

#include "insieme/frontend/tu/ir_translation_unit.h"

#include "insieme/frontend/extensions/frontend_plugin.h"

namespace insieme {
namespace frontend {

	using std::map;
	using std::set;
	using std::list;
	using std::shared_ptr;
	using std::vector;
	using std::string;


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
			Auto, C99, Cxx03, Cxx11
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
		 * A list of include directories containing system headers.
		 */
		vector<path> systemHeaderSearchPath;

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
		 * A list of string representing the regular expression to be intercepted
		 * by default "std::.*" and "__gnu_cxx::.*" are intercepted
		 */
		set<string> interceptions;

		/**
		 * Additional flags - a bitwise boolean combination of Options (see Option)
		 */
		unsigned flags;

        /**
         *  A map that contains all user plugins
         */
         typedef std::shared_ptr<extensions::FrontendPlugin> FrontendPluginPtr;
         std::list<FrontendPluginPtr> plugins;

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
		void setStandard(const Standard& standard);

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
		const vector<path>& getSystemHeadersDirectories() const {
			return systemHeaderSearchPath;
		}

		/**
		 * Updates the set of considered std-library include directories.
		 */
		void setSystemHeadersDirectories(const vector<path>& includeDirectories) {
			this->systemHeaderSearchPath = includeDirectories;
		}

		/**
		 * Adds an additional user defined header serach path
		 */
		void addSystemHeadersDirectory(const path& directory) {
			this->systemHeaderSearchPath.push_back(directory);
		}

		/**
		 * Updates the list of strings to be intercepted
		 */
		void setInterceptions(const vector<string>& toIntercept) {
			this->interceptions.insert(toIntercept.begin(), toIntercept.end());
		}

		/**
		 * Adds a single regular expression string to the intercetion set
		 */
		void setInterception(const string& toIntercept) {
			this->interceptions.insert(toIntercept);
		}

		/**
		 * Obtains a reference to the currently defined interceptions.
		 */
		const set<string>& getInterceptions() const {
			return interceptions;
		}

		/**
		 * A utility method to determine whether the given file should be
		 * considered a C++ file or not. This decision will be influenced
		 * by the standard set within this setup. Only if set to auto
		 * (default) the extension will be evaluated.
		 */
		bool isCxx(const path& file) const;

        /**
         *  Frontend plugin initialization method
         */
        void frontendPluginInit();

        /**
         *  Register a new frontend plugin
         */
        template <class T, class ... Args>
        void registerFrontendPlugin(const Args& ... args) {
            plugins.push_back(std::make_shared<T>(args ...));
        };

        /**
         *  Return the list of frontend plugins
         */
        const std::list<FrontendPluginPtr> getPlugins() const {
            return plugins;
        };
	};


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
		 * Creates an empty conversion job covering no files.
		 */
		ConversionJob() : ConversionSetup(vector<path>()) {}

		/**
		 * Creates a new conversion job covering a single file.
		 */
		ConversionJob(const path& file, const vector<path>& includeDirs = vector<path>())
			: ConversionSetup(includeDirs), files(toVector(file)) {
        }

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
		 * Determines whether this conversion job is processing a C++ file or not.
		 */
		bool isCxx() const;

		/**
		 * Triggers the actual conversion. The previously set up parameters will be used to attempt a conversion.
		 *
		 * @param manager the node manager to be used for building the IR
		 * @param fullApp a flag determining whether the result is expected to be a full application (entered via
		 * 				a main function) or a list of multiple entry points.
		 * @return the resulting, converted program
		 * @throws an exception if the conversion fails.
		 */
		core::ProgramPtr execute(core::NodeManager& manager, bool fullApp = true) const;

		/**
		 * Triggers the conversion of the files covered by this job into a translation unit.
		 *
		 * @param manager the node manager to be used for building the IR
		 * @return the resulting, converted program
		 * @throws an exception if the conversion fails.
		 */
		tu::IRTranslationUnit toTranslationUnit(core::NodeManager& manager) const;

	};


	/**
	 * Used to report a parsing error occurred during the parsing of the input file
	 */
	struct ClangParsingError: public std::logic_error {
		ClangParsingError(const path& file_name): std::logic_error(file_name.string()) { }
	};


} // end namespace frontend
} // end namespace insieme
