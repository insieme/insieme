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

#include <string>
#include <vector>
#include <set>
#include <fstream>

#include "insieme/utils/abstraction.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {
namespace compiler {

	const char* getDefaultCCompilerExecutable();
	const char* getDefaultCxxCompilerExecutable();

	// some common abbreviations
	using std::string;
	using std::vector;
	using std::set;

	class ExternalLibraries {
	  private:
		set<string> paths;

		set<string> libs;

	  public:
		void addPath(const string& path) {
			paths.insert(path);
		}

		void addLib(const string& lib) {
			libs.insert(lib);
		}

		set<string> getPaths() const {
			return paths;
		}

		set<string> getLibs() const {
			return libs;
		}
	};

	class Compiler : public Printable {
		string executable;

		vector<string> flags;

		ExternalLibraries libs;

		vector<string> incDirs;

		string standardOutput = "/dev/stdout";
		string standardErrorOutput = "/dev/stderr";

	  public:
		Compiler(const string& executable) : executable(executable), libs(), incDirs() {};

		static Compiler getDefaultC99Compiler();

		static Compiler getDefaultCppCompiler();

		static Compiler getRuntimeCompiler(const Compiler& baseCompiler = getDefaultC99Compiler());

		static Compiler getOpenCLCompiler(const Compiler& baseCompiler = getDefaultC99Compiler());

		static Compiler getOptimizedCompiler(const Compiler& base, const string& level = "3");

		static Compiler getDebugCompiler(const Compiler& base, const string& level = "3");

		const string& getExecutable() const {
			return executable;
		}

		const vector<string>& getFlags() const {
			return flags;
		}

		const string& getStandardOutput() const {
			return standardOutput;
		}

		void setStandardOutput(const string& standardOutput) {
			this->standardOutput = standardOutput;
		}

		const string& getStandardErrorOutput() const {
			return standardOutput;
		}

		void setStandardErrorOutput(const string& standardErrorOutput) {
			this->standardErrorOutput = standardErrorOutput;
		}

		void setSilent() {
			this->standardOutput = "/dev/null";
			this->standardErrorOutput = "/dev/null";
		}

		void addFlag(const string& flag) {
			flags.push_back(flag);
		}

		void addExternalLibrary(const string& path, const string& lib) {
			libs.addPath(path);
			libs.addLib(lib);
		}

		void addLibrary(const string& lib) {
			libs.addLib(lib);
		}

		void addIncludeDir(const string& path) {
			incDirs.push_back(path);
		}

		string getCommand(const vector<string>& inputFiles, const string& outputFile) const;

		std::ostream& printTo(std::ostream& out) const {
			return out << executable << " " << join(" ", flags);
		}

	};

	const vector<string> getDefaultCIncludePaths();
	const vector<string> getDefaultCppIncludePaths();


	/**
	 * Compiles the given source files using the defined compiler (by default, it is the default C compiler) and
	 * writes the resulting binary into the given target file.
	 *
	 * @param sourcefiles the files to be compiled
	 * @param targetfile the file to be produced
	 * @param compiler the compiler to be used for the compilation - the default is a C99 compiler
	 * @return true if successful, false otherwise
	 */
	bool compile(const vector<string>& sourcefiles, const string& targetfile, const Compiler& compiler = Compiler::getDefaultC99Compiler());

	/**
	 * Compiles the given source file using the defined compiler (by default, it is the default C compiler) and
	 * writes the resulting binary into the given target file.
	 *
	 * @param sourcefile the file to be compiled
	 * @param targetfile the file to be produced
	 * @param compiler the compiler to be used for the compilation - the default is a C99 compiler
	 * @return true if successful, false otherwise
	 */
	bool compile(const string& sourcefile, const string& targetfile, const Compiler& compiler = Compiler::getDefaultC99Compiler());

	/**
	 * Compiles the given source file using the defined compiler (by default, it is the default C compiler) and
	 * writes the resulting binary into a temporary location, whose name is returned.
	 *
	 * @param sourcefile the file to be compiled
	 * @param compiler the compiler to be used for the compilation - the default is a C99 compiler
	 * @return the name of the resulting binary file
	 */
	string compile(const string& sourcefile, const Compiler& compiler);

	/**
	 * Compiles the given source code using the given compiler and temporary source and target files.
	 *
	 * @param source the source code to be compiled
	 * @param compiler the compiler to be used for the compilation - the default is a C99 compiler
	 */
	bool compile(const VirtualPrintable& source, const Compiler& compiler = Compiler::getDefaultC99Compiler());

	/**
	 * Compiles the given source code using the given compiler and temporary source and target files.
	 *
	 * @param source the source code to be compiled
	 * @param compiler the compiler to be used for the compilation - the default is a C99 compiler
	 */
	template <typename Printable>
	typename std::enable_if<!std::is_base_of<VirtualPrintable, Printable>::value, bool>::type
	compile(const Printable& source, const Compiler& compiler = Compiler::getDefaultC99Compiler()) {
		return compile(toVirtualPrintable(source), compiler);
	}

	/**
	 * Compiles the given source code using the given compiler and temporary source and target files.
	 *
	 * @param source the source code to be compiled
	 * @param compiler the compiler to be used for the compilation - the default is a C99 compiler
	 * @return the name of the binary (or empty string if compilation failed)
	 */
	string compileToBinary(const VirtualPrintable& source, const Compiler& compiler = Compiler::getDefaultC99Compiler());

	/**
	 * Compiles the given source code using the given compiler and temporary source and target files.
	 *
	 * @param source the source code to be compiled
	 * @param compiler the compiler to be used for the compilation - the default is a C99 compiler
	 * @return the name of the binary (or empty string if compilation failed)
	 */
	template <typename Printable>
	typename std::enable_if<!std::is_base_of<VirtualPrintable, Printable>::value, string>::type
	compileToBinary(const Printable& source, const Compiler& compiler = Compiler::getDefaultC99Compiler()) {
		return compileToBinary(toVirtualPrintable(source), compiler);
	}

	/**
	 * Compiles the given source code using the given compiler to the given target file using a temporary
	 * source file-
	 *
	 * @param source the source code to be compiled
	 * @param target the name of the target file (binary)
	 * @param compiler the compiler to be used for the compilation - the default is a C99 compiler
	 * @return true if successful, false otherwise
	 */
	bool compileToBinary(const VirtualPrintable& source, const string& target, const Compiler& compiler = Compiler::getDefaultC99Compiler());

	/**
	 * Compiles the given source code using the given compiler to the given target file using a temporary
	 * source file-
	 *
	 * @param source the source code to be compiled
	 * @param target the name of the target file (binary)
	 * @param compiler the compiler to be used for the compilation - the default is a C99 compiler
	 * @return true if successful, false otherwise
	 */
	template <typename Printable>
	typename std::enable_if<!std::is_base_of<VirtualPrintable, Printable>::value, bool>::type
	compileToBinary(const Printable& source, const string& target, const Compiler& compiler = Compiler::getDefaultC99Compiler()) {
		return compileToBinary(toVirtualPrintable(source), target, compiler);
	}

	/**
	 * Determine if the currently executing instance of insieme has access to OpenCL headers
	 *
	 * @return true if available, false otherwise
	 */
	bool isOpenCLAvailable();
} // end namespace: compiler
} // end namespace: utils
} // end namespace: insieme
