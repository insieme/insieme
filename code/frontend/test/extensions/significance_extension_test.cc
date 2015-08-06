/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <gtest/gtest.h>

#include "insieme/core/ir_visitor.h"

#include "insieme/frontend/frontend.h"

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/utils/compiler/compiler.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "../test_utils.inc"

using namespace insieme::driver;

namespace insieme {
namespace frontend {

using namespace core;

TEST(Significance, Task) {
	NodeManager man;
	IRBuilder builder(man);
	
	fs::path tmpFile;
	{
	
		// create a temporary source file
		Source file(
		    R"(
#include <stdio.h>
void bla() {
	printf("Wow\n");
}

int check(int x) {
	return x;
}

int main(int argc, char **argv) {
	int A[128];
	float B[64];
	#pragma omp task label(testlabel) in(A[0:127][10;20]) out(B[0:63][7;42]) \
		significant(ratio(1.0/argc)) tasktolerance(taskcheck(check(argc)),redo(5))
	bla();
}
		)");

		// check whether there is a temporary file
		tmpFile = file.getPath();
		EXPECT_TRUE(fs::exists(tmpFile));
		
		// parse temporary file
		core::NodeManager manager;
		const boost::filesystem::path& fileName = file;
		std::vector<std::string> argv = { "muha",  fileName.string(), "--ftask-significance" };
		cmd::Options options = cmd::Options::parse(argv);
		
		auto code = options.job.execute(manager);
		EXPECT_TRUE(code);
		
		// create target code using the runtime backend
		auto target = backend::runtime::RuntimeBackend::getDefault()->convert(code);
		
		// check for presence of expected metainfo
		string targetString = toString(*target);
		EXPECT_TRUE(targetString.find("\"testlabel\"") != string::npos);
		
		// see whether target code can be compiled
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getRuntimeCompiler();
		
		// build binary
		auto fn = utils::compiler::compileToBinary(*target, compiler);
		EXPECT_FALSE(fn.empty());
	}
}

} // namespace frontend
} // namespace insieme
