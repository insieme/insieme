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

	/**
	 * Used to report a parsing error occurred during the parsing of the input file
	 */
	struct ClangParsingError: public std::logic_error {
		ClangParsingError(const std::string& file_name): std::logic_error(file_name) { }
	};


	using std::vector;
	using std::string;

	class ConversionJob {

	public:

		/**
		 * A list of options to adjust the conversion job.
		 */
		enum Option {
			PrintDiag		= 1<<0,

			OpenMP			= 1<<1,
			OpenCL			= 1<<2,
			Cilk			= 1<<3,

			WinCrossCompile	= 1<<4,
			DumpCFG			= 1<<5,
			TAG_MPI			= 1<<6
		};

		/**
		 * The default frontend configuration.
		 */
		static const unsigned DEFAULT_FLAGS;

	private:

		/**
		 * The translation units to be converted.
		 */
		vector<string> files;

		/**
		 * A list of include directories to be considered.
		 */
		vector<string> includeDirs;

		/**
		 * A list of include directories containing std library headers.
		 */
		vector<string> stdLibIncludeDirs;

		/**
		 * The C standard to be followed.
		 */
		string standard;

		/**
		 * A list of definitions to be passed to the preprocessor.
		 */
		vector<string> definitions;

		/**
		 * The name of the configuration file of the intercepter.
		 */
		string intercepterConfigFile;

		/**
		 * Additional flags - a bitwise boolean combination of Options (see Option)
		 */
		unsigned flags;

	public:

		/**
		 * Creates a new conversion job covering a single file.
		 */
		ConversionJob(const string& file);

		/**
		 * Creates a new conversion job based on the given options.
		 */
		ConversionJob(const vector<string>& files = vector<string>(), const vector<string>& includeDirs = vector<string>());

		/**
		 * Obtains a reference to the files covered by this conversion job.
		 */
		const vector<string>& getFiles() const {
			return files;
		}

		/**
		 * Obtains the one input file covered by this conversion job if there is only one file.
		 */
		const string& getFile() const {
			assert(files.size() == 1u);
			return files[0];
		}

		/**
		 * Exchanges the files covered by this conversion job by the given file.
		 */
		void setFile(const string& file) {
			this->files.clear(); this->files.push_back(file);
		}

		/**
		 * Updates the files covered by this conversion job.
		 */
		void setFiles(const vector<string>& files) {
			this->files = files;
		}

		/**
		 * Adds a file to be covered by this conversion job.
		 */
		void addFile(const string& file) {
			this->files.push_back(file);
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
		const string& getStandard() const {
			return standard;
		}

		/**
		 * Updates the standard to be used for parsing input files.
		 */
		void setStandard(const string& standard) {
			this->standard = standard;
		}

		/**
		 * Obtains a reference to the currently defined definitions.
		 */
		const vector<string>& getDefinitions() const {
			return definitions;
		}

		/**
		 * Updates the definitions to be used by the conversion process.
		 */
		void setDefinitions(const vector<string>& definitions) {
			this->definitions = definitions;
		}

		/**
		 * Adds a pre-processor definition to this conversion job.
		 */
		void addDefinition(const string& name, const string& value = "");

		/**
		 * Obtains a reference to the covered set of include directories.
		 */
		const vector<string>& getIncludeDirectories() const {
			return includeDirs;
		}

		/**
		 * Updates the set of considered include directories.
		 */
		void setIncludeDirectories(const vector<string>& includeDirectories) {
			this->includeDirs = includeDirectories;
		}

		/**
		 * Adds an additional include directory.
		 */
		void addIncludeDirectory(const string& directory) {
			this->includeDirs.push_back(directory);
		}

		/**
		 * Obtains a reference to the covered set of std-library include directories.
		 */
		const vector<string>& getStdLibIncludeDirectories() const {
			return stdLibIncludeDirs;
		}

		/**
		 * Updates the set of considered std-library include directories.
		 */
		void setStdLibIncludeDirectories(const vector<string>& includeDirectories) {
			this->stdLibIncludeDirs = includeDirectories;
		}

		/**
		 * Adds an additional std-library include directory.
		 */
		void addStdLibIncludeDirectory(const string& directory) {
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
		 * Triggers the actual conversion. The previously set up parameters will be used to attempt a conversion.
		 *
		 * @param manager the node manager to be used for building the IR
		 * @return the resulting, converted program
		 * @throws an exception if the conversion fails.
		 */
		core::ProgramPtr execute(core::NodeManager& manager);
	};


} // end namespace frontend
} // end namespace insieme
