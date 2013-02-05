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

#include "insieme/utils/compiler/compiler.h"

#include <cstdio>
#include <sstream>
#include <fstream>

#include <boost/filesystem.hpp>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/test/test_config.h"

namespace insieme {
namespace utils {
namespace compiler {

	namespace fs = boost::filesystem;

	Compiler Compiler::getDefaultC99Compiler() {
		// create a default version of a C99 compiler
//		Compiler res(C_COMPILER); // TODO: re-enable when constant is set properly
		Compiler res("gcc");
		res.addFlag("-x c");
		res.addFlag("-Wall");
		res.addFlag("--std=gnu99");
		res.addFlag("-Wl,--no-as-needed");
		return res;
	}

	Compiler Compiler::getDefaultC99CompilerO3() {
		Compiler res = getDefaultC99Compiler();
		res.addFlag("-O3");
		return res;
	}

	Compiler Compiler::getRuntimeCompiler() {
		Compiler res = getDefaultC99Compiler();
		res.addFlag("-I " SRC_ROOT_DIR "runtime/include -D_XOPEN_SOURCE=700 -D_GNU_SOURCE -ldl -lrt -lpthread -lm");
		return res;
	}

	Compiler Compiler::getRuntimeCompilerO3() {
		Compiler res = getRuntimeCompiler();
		res.addFlag("-O3");
		return res;
	}

	Compiler Compiler::getDefaultCppCompiler() {
		Compiler res("gcc");
		res.addFlag("-x c++");
		res.addFlag("-lstdc++");
		res.addFlag("-Wall");
		res.addFlag("--std=c++98");
		res.addFlag("-Wl,--no-as-needed");
		return res;
	}

	Compiler Compiler::getDefaultCppCompilerO3() {
		Compiler res = getDefaultCppCompiler();
		res.addFlag("-O3");
		return res;
	}

	string Compiler::getCommand(const vector<string>& inputFiles, const string& outputFile) const {
		// build up compiler command
		std::stringstream cmd;

		// some flags are known to be required to be place before the source file
		vector<string> before;
		vector<string> after;

		// split up flags
		for(auto cur : flags) {
			// the -x option has to be before the input file
			if (cur[0] == '-' && cur[1] == 'x') {
				before.push_back(cur);
			} else {
				after.push_back(cur);
			}
		}

		cmd << executable;
		cmd << " " << join(" ", before);
		cmd << " " << join(" ", inputFiles);
		cmd << " " << join(" ", after);
		cmd << " -o " << outputFile;

		return cmd.str();
	}



	bool compile(const vector<string>& sourcefile, const string& targetfile, const Compiler& compiler) {

		string&& cmd = compiler.getCommand(sourcefile, targetfile);

		LOG(DEBUG) << "Running command: " << cmd << "\n";
		int res = system(cmd.c_str());
		LOG(DEBUG) << "Result of command " << cmd << ": " << res << "\n";

		return res == 0;
	}

	bool compile(const string& sourcefile, const string& targetfile, const Compiler& compiler) {
		vector<string> files(1);
		files[0] = sourcefile;
		return compile(files, targetfile, compiler);
	}


	bool compile(const Printable& source, const Compiler& compiler) {

		string target = compileToBinary(source, compiler);

		if(target.empty()) return false;

		// delete target file
		if (boost::filesystem::exists(target)) {
			boost::filesystem::remove(target);
		}

		return true;
	}
	
	string compileToBinary(const Printable& source, const Compiler& compiler) {

		// create temporary target file name
		char targetFile[] = P_tmpdir "/trgXXXXXX";
		int trg = mkstemp(targetFile);
		assert(trg != -1);
		close(trg);

		LOG(DEBUG) << "Using temporary file " << string(targetFile) << " as a target file for compilation.";

		if (compileToBinary(source, targetFile, compiler)) {
			return string(targetFile);
		}
		return string();
	}


	bool compileToBinary(const Printable& source, const string& targetFile, const Compiler& compiler) {

		// create a temporary source file
		// TODO: replace with boost::filesystem::temp_directory_path() when version 1.46 is available
		char sourceFile[] = P_tmpdir "/srcXXXXXX";
		int src = mkstemp(sourceFile);
		assert(src != -1);
		close(src);

		LOG(DEBUG) << "Using temporary file " << string(sourceFile) << " as a source file for compilation.";

		// write source to file
		std::fstream srcFile(sourceFile, std::fstream::out);
		srcFile << source << "\n";
		srcFile.close();

		// conduct compilation
		bool res = compile(sourceFile, targetFile, compiler);
		
		// delete source file
		if (boost::filesystem::exists(sourceFile)) {
			boost::filesystem::remove(sourceFile);
		}

		// return success flag
		return res;
	}

} // end namespace compiler
} // end namespace utils
} // end namespace insieme
