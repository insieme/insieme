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

#define BOOST_FILESYSTEM_VERSION 3
#include <gtest/gtest.h>

#include "insieme/utils/compiler/compiler.h"

#include <iostream>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include "insieme/utils/container_utils.h"

using namespace insieme::utils;

namespace insieme {
namespace utils {
namespace compiler {


TEST(TargetCodeCompilerTest, helloWorldTest) {

	namespace fs = boost::filesystem;

	// create a dummy code file to be compiled
	fs::path dir = "./";
	fs::path srcFile = dir / "_ut_hello_world.c";
	fs::path binFile = dir / "_ut_hello_world";

	// ensure file does not exist yet
	ASSERT_FALSE(fs::exists(srcFile)) << "File " << srcFile << " should not exist!";
	ASSERT_FALSE(fs::exists(binFile)) << "File " << binFile << " should not exist!";


	// write hello world code into the source file
	fs::ofstream code;
	code.open(srcFile);
	ASSERT_TRUE(code.is_open()) << "Unable to open source file!";
	code << "#include <stdio.h>\n\n";
	code << "int main() {\n";
	code << "	printf(\"Hello World!\\n\");\n";
	code << "}\n\n";
	code.close();

	// file should exist now
	ASSERT_TRUE(fs::exists(srcFile));

	// compile the example code using the default compiler
	EXPECT_TRUE(compile(srcFile.string(), binFile.string()));


	// delete both files
	if (fs::exists(srcFile)) {
		fs::remove(srcFile);
	}

	if(fs::exists(binFile)) {
		fs::remove(binFile);
	}
}


TEST(TargetCodeCompilerTest, DirectHelloWorldTest) {

	// write some code
	string code =
			"#include <stdio.h>\n\n"
			"int main() {\n"
			"	printf(\"Hello World!\\n\");\n"
			"}\n\n";

	// compile using direct signature
	EXPECT_TRUE(compile(toPrintable(code)));

}

TEST(TargetCodeCompiler, GetIncludePaths) {
	EXPECT_FALSE(getDefaultCIncludePaths().empty());
	EXPECT_FALSE(getDefaultCIncludePaths().empty());
}


} // end namespace test
} // end namespace utils
} // end namespace insieme
