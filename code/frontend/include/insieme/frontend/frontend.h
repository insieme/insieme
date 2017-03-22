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

#include <map>
#include <string>
#include <vector>
#include <list>
#include <memory>

#include <boost/program_options.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/optional.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_program.h"

#include "insieme/core/tu/ir_translation_unit.h"
#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/frontend/extensions/frontend_cleanup_extension.h"

#include "insieme/utils/printable.h"

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
			PrintDiag = 1 << 0,
			NoWarnings = 1 << 1,
			NoDefaultExtensions = 1 << 2,
			DumpClangAST = 1 << 3,
			NoColor = 1 << 4,
		};

		/**
		 * A list of supported standards.
		 */
		enum Standard { Auto, C99, Cxx11, Cxx14 };

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
		insieme::utils::Lazy<vector<path>> systemHeaderSearchPath;

		/**
		 * Retrieve list (lazy evaluation)
		 */
		vector<path>& getSystemHeaderSearchPathInternal();

		/**
		 * The C standard to be followed.
		 */
		Standard standard;

		/**
		 * A list of definitions to be passed to the preprocessor. Each
		 * entry maps an identifier to a value.
		 */
		map<string, string> definitions;

		/**
		 * A regex to filter the clang AST dump.
		 */
		std::string clangASTDumpFilter;

		/**
		 * A list of include directories containing intercepted headers.
		 */
		vector<path> interceptedHeaderDirs;

		/**
		 * A list of regex strings which will be applied to the fully qualified name before the interceptor decides whether to intercept or not.
		 * Everything mathing an entry in this list will _not_ be intercepted.
		 */
		vector<string> interceptionWhitelist;

		/**
		 * A list of include directories containing system headers for a cross compilation.
		 */
		string crossCompilationSystemHeadersDir;

		/**
		 * A list of optimization flags (-f flags) that need to be used at least in the
		 * backend compiler
		 */
		set<string> fflags;

		/**
		 * Additional flags - a bitwise boolean combination of Options (see Option)
		 */
		unsigned flags;

	  protected:
		/**
		 *  A list that contains all user extensions that have been registered
		 */
		std::list<extensions::FrontendExtension::FrontendExtensionPtr> extensionList;

	  public:
		/**
		* Creates a new setup covering the given include directories.
		 */
		ConversionSetup(const vector<path>& includeDirs = vector<path>());

		/**
		 * Checks whether an extension is loaded.
		 * For use in e.g. isPrerequisiteMissing
		 */
		template <class ExtensionType>
		bool hasExtension() const {
			for(const auto& extPtr : getExtensions()) {
				const auto& extVal = *extPtr;
				if(typeid(ExtensionType) == typeid(extVal)) { return true; }
			}
			return false;
		}

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
			flags = (status) ? (flags | option) : (flags & ~option);
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
		const map<string, string>& getDefinitions() const {
			return definitions;
		}

		/**
		 * Obtains a reference to the clang AST dump filter regex.
		 */
		const std::string& getClangASTDumpFilter() const {
			return clangASTDumpFilter;
		}

		/**
		 * Set the clang AST dump filter regex.
		 */
		void setClangASTDumpFilter(const std::string& filter) {
			clangASTDumpFilter = filter;
		}

		/**
		 * Updates the definitions to be used by the conversion process.
		 */
		void setDefinitions(const map<string, string>& definitions) {
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
			return const_cast<ConversionSetup*>(this)->getSystemHeaderSearchPathInternal();
		}

		/**
		 * Updates the set of considered std-library include directories.
		 */
		void setSystemHeadersDirectories(const vector<path>& includeDirectories) {
			getSystemHeaderSearchPathInternal() = includeDirectories;
		}

		/**
		 * Adds an additional user defined header search path
		 */
		void addSystemHeadersDirectory(const path& directory) {
			getSystemHeaderSearchPathInternal().push_back(directory);
		}

		/**
		 * Obtains a reference to the covered set of include directories.
		 */
		const vector<path>& getInterceptedHeaderDirs() const {
			return interceptedHeaderDirs;
		}

		/**
		 * Updates the set of considered include directories.
		 */
		void setInterceptedHeaderDirs(const vector<path>& interceptedHeaderDirs) {
			this->interceptedHeaderDirs = interceptedHeaderDirs;
		}

		/**
		 * Adds an additional include directory.
		 */
		void addInterceptedHeaderDir(const path& directory) {
			this->interceptedHeaderDirs.push_back(directory);
		}

		/**
		 * Obtains a reference to the interception whitelist.
		 */
		const vector<string>& getInterceptionWhitelist() const {
			return interceptionWhitelist;
		}

		/**
		 * Adds a new regex pattern to the interception whitelist.
		 */
		void addInterceptionWhitelistEntry(const string& pattern) {
			this->interceptionWhitelist.push_back(pattern);
		}

		/**
		 * Obtains a reference to the covered set of include directories.
		 */
		const string& getCrossCompilationSystemHeadersDir() const {
			return crossCompilationSystemHeadersDir;
		}

		/**
		 * Updates the set of considered include directories.
		 */
		void setCrossCompilationSystemHeadersDir(const string& crossCompilationSystemHeadersDir) {
			this->crossCompilationSystemHeadersDir = crossCompilationSystemHeadersDir;
		}

		/**
		 * Adds a single optimization flag
		 */
		void addFFlag(const string& flag) {
			this->fflags.insert(flag);
		}

		/**
		 * Obtains a reference to the currently defined f flags
		 */
		const set<string>& getFFlags() const {
			return fflags;
		}

		/**
		 * A utility method to determine whether the given file should be
		 * considered a C++ file or not. This decision will be influenced
		 * by the standard set within this setup. Only if set to auto
		 * (default) the extension will be evaluated.
		 */
		bool isCxx(const path& file) const;

		/**
		 *  Return the list of all registered frontend extensions
		 */
		const std::list<extensions::FrontendExtension::FrontendExtensionPtr>& getExtensions() const {
			return extensionList;
		};

		/**
		 *  Return (the first) registered frontend extension of matching type
		 */
		template <class T>
		const std::shared_ptr<T> getExtension() const {
			for(const extensions::FrontendExtension::FrontendExtensionPtr ext : extensionList) {
				auto&& pext = *ext;
				if(typeid(pext) == typeid(T)) { return dynamic_pointer_cast<T>(ext); }
			}
			assert_fail() << "Requested frontend extension of type " << typeid(T).name() << " but no extension of that type was registered!";
			return nullptr;
		};
	};


	class ConversionJob : public ConversionSetup, public insieme::utils::Printable {
		/**
		 * The translation units to be converted.
		 */
		vector<path> files;

		/**
		 * The external libraries.
		 */
		vector<path> extLibs;

		/**
		 * Extra libraries to be considered for the conversion.
		 */
		vector<core::tu::IRTranslationUnit> libs;

		/**
		 * A vector of pairs. Each pair contains a frontend extension pointer and a
		 * lambda that was retrieved from the extension. This lambda will decide
		 * if the plugin gets registered and the plugin will be configured by the
		 * lambda.
		 */

		std::vector<std::pair<extensions::FrontendExtension::FrontendExtensionPtr, extensions::FrontendExtension::FlagHandler>> extensions;
		std::vector<string> unparsedOptions;

	  public:
		/**
		 * Creates an empty conversion job covering no files.
		 */
		ConversionJob() : ConversionSetup(vector<path>()) {}

		/**
		 * Creates a new conversion job covering a single file.
		 */
		ConversionJob(const path& file, const vector<path>& includeDirs = vector<path>()) : ConversionSetup(includeDirs), files(toVector(file)) {}

		/**
		 * Creates a new conversion job covering the given files.
		 */
		ConversionJob(const vector<path>& files, const vector<path>& includeDirs = vector<path>()) : ConversionSetup(includeDirs), files(files) {
			assert_false(files.empty());

			// The user defined headers path is extended with c source files directories
			auto inc = ConversionSetup::getIncludeDirectories();
			for(auto cur : files) {
				inc.push_back(cur.parent_path());
			}
			ConversionSetup::setIncludeDirectories(inc);
		}

		/**
		 * Obtains the one input files covered by this conversion job.
		 */
		const vector<path>& getFiles() const {
			return files;
		}

		/**
		 * Adds an additional file to this conversion job.
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
		 * Obtains the list of external libraries.
		 */
		const vector<path>& getExtLibs() const {
			return extLibs;
		}

		/**
		 * Exchanges thelist of external libraries.
		 */
		void setExtLibs(const vector<path>& extLibs) {
			this->extLibs = extLibs;
		}

		/**
		 * Obtains a reference to the libs to be considered by this conversion job.
		 */
		const vector<core::tu::IRTranslationUnit>& getLibs() const {
			return libs;
		}

		/**
		 * Sets the libs to be considered by this conversion job.
		 */
		void setLibs(const vector<core::tu::IRTranslationUnit>& libs) {
			this->libs = libs;
		}

		/**
		 * Appends a library to the list of libraries considered by this conversion job.
		 */
		void addLib(const core::tu::IRTranslationUnit& unit) {
			libs.push_back(unit);
		}

		/**
		 * Determines whether this conversion job is processing a C++ file or not.
		 */
		bool isCxx() const;

		/**
		 * Triggers the actual conversion. The previously set up parameters will be used to attempt a conversion.
		 * Automatically determines whether the supplied program contains multiple entry points.
		 *
		 * @param manager the node manager to be used for building the IR
		 * @return the resulting, converted program
		 * @throws an exception if the conversion fails.
		 */
		core::ProgramPtr execute(core::NodeManager& manager);

		/**
		 * Triggers the actual conversion. The previously set up parameters will be used to attempt a conversion.
		 *
		 * @param manager the node manager to be used for building the IR
		 * @param fullApp a flag determining whether the result is expected to be a full application (entered via
		 * 				a main function) or a list of multiple entry points.
		 * @return the resulting, converted program
		 * @throws an exception if the conversion fails.
		 */
		core::ProgramPtr execute(core::NodeManager& manager, bool fullApp);

		/**
		 * Triggers the conversion of the files covered by this job into a translation unit.
		 *
		 * @param manager the node manager to be used for building the IR
		 * @return the resulting, converted program
		 * @throws an exception if the conversion fails.
		 */
		core::tu::IRTranslationUnit toIRTranslationUnit(core::NodeManager& manager);

		/**
		 * Set unparsed options that might be used by a backend compiler
		 * @param unparsed options that might be used by a backend compiler
		 */
		void setUnparsedOptions(const std::vector<std::string>& unparsed) {
			this->unparsedOptions = unparsed;
		}

		/**
		 * Get unparsed options that might be used by a backend compiler
		 * @return unparsed options that might be used by a backend compiler
		 */
		std::vector<std::string> getUnparsedOptions() const {
			return this->unparsedOptions;
		}

		void registerExtensionFlags(boost::program_options::options_description& options);

		void registerDefaultExtensions();

		/**
		 *  Frontend extension initialization method
		 */
		void frontendExtensionInit();

		/**
		 *  Insert a new frontend extension. This DOES NOT mean that the
		 *  extension gets registered. The registerFlag method will return a
		 *  lambda that is called in frontendExtensionInit. This lambda decides
		 *  if the extension is registered. If the extension does not override the
		 *  registerFlag method it will be registered by default.
		 */
		template <class T, class Before = extensions::FrontendCleanupExtension, class... Args>
		void registerFrontendExtension(boost::program_options::options_description& extOptions, Args&&... args) {
			auto extensionPtr = std::make_shared<T>(args...);

			// don't do anything if the extension is already registered.
			for(auto it = extensions.begin(); it < extensions.end(); ++it) {
				auto&& pitfirst = *(it->first);
				if(typeid(pitfirst) == typeid(T)) {
					return;
				}
			}

			// insert the extension before "Before", unless it's not found
			bool inserted = false;
			for(auto it = extensions.begin(); it < extensions.end(); ++it) {
				auto&& pitfirst = *(it->first);
				if(typeid(pitfirst) == typeid(Before)) {
					extensions.insert(it, {extensionPtr, extensionPtr->registerFlag(extOptions)});
					inserted = true;
					break;
				}
			}
			if(!inserted) extensions.push_back({extensionPtr, extensionPtr->registerFlag(extOptions)});

			// some options were not parsed by the driver, parse them by the extension
			boost::program_options::parsed_options parsed =
			    boost::program_options::basic_command_line_parser<char>(this->unparsedOptions)
			        .options(extOptions)
			        .style((boost::program_options::command_line_style::default_style | boost::program_options::command_line_style::allow_long_disguise)
			               ^ boost::program_options::command_line_style::allow_guessing)
			        .allow_unregistered()
			        .run();
			boost::program_options::variables_map vm;
			boost::program_options::store(parsed, vm);
			boost::program_options::notify(vm);
		};

		/**
		 *  Force given frontend extension to be loaded, ignoring any flags.
		 */
		template <class T>
		void forceFrontendExtension() {
			auto extensionPtr = std::make_shared<T>();
			extensions.push_back({extensionPtr, [](const ConversionJob&){ return true; }});
		};

		/**
		 * Insert a frontend extension without concerning about cmd-line options for drivers
		 * We get from the extensions possible options and parse the arguments which were unknown to the driver
		 * Most useful to write tests involving extensions
		 */
		template <class T, class Before = extensions::FrontendCleanupExtension, class... Args>
		void registerFrontendExtension(Args&&... args) {
			boost::program_options::options_description extOptions;
			registerFrontendExtension<T, Before>(extOptions, args...);
		};

		/**
		 *  Prints the conversion setup
		 **/
		std::ostream& printTo(std::ostream& out) const;

	  private:

		/**
		 * An internal utility applying post-processing steps to the generated program.
		 */
		core::ProgramPtr applyPostProcessing(core::NodeManager& manager, core::ProgramPtr& program) const;
	};

} // end namespace frontend
} // end namespace insieme
