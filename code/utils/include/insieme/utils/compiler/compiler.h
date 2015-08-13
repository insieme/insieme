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
#include <set>
#include <fstream>

#include "insieme/utils/logging.h"
#include "insieme/utils/printable.h"


namespace insieme {
namespace utils {
namespace compiler {

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

	class Compiler {
		string executable;

		vector<string> flags;

		ExternalLibraries libs;

		vector<string> incDirs;

		bool silent;

	  public:
		Compiler(const string& executable) : executable(executable), libs(), incDirs(), silent(false){};

		static Compiler getDefaultC99Compiler();

		static Compiler getDefaultCppCompiler();

		static Compiler getRuntimeCompiler(const Compiler& baseCompiler = getDefaultC99Compiler());

		static Compiler getOptimizedCompiler(const Compiler& base, const string& level = "3");

		static Compiler getDebugCompiler(const Compiler& base, const string& level = "3");

		const string& getExecutable() const {
			return executable;
		}

		const vector<string>& getFlags() const {
			return flags;
		}

		bool isSilent() const {
			return silent;
		}

		void setSilent(bool silent = true) {
			this->silent = silent;
		}

		void addFlag(const string& flag) {
			flags.push_back(flag);
		}

		void addExternalLibrary(const string& path, const string& lib) {
			libs.addPath(path);
			libs.addLib(lib);
		}

		void addIncludeDir(const string& path) {
			incDirs.push_back(path);
		}

		string getCommand(const vector<string>& inputFiles, const string& outputFile) const;
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


} // end namespace: compiler
} // end namespace: utils
} // end namespace: insieme
