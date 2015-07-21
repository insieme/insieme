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
#include "insieme/core/checks/full_check.h"

#include "insieme/frontend/frontend.h"
#include "test_utils.inc"

namespace insieme {
namespace frontend {

using namespace core;

TEST(C89Declarations, Basic) {
	NodeManager man;
	IRBuilder builder(man);

	fs::path tmpFile;
	{

// create a temporary source file
Source file(
R"(
#include <stdio.h>

// C89 / K&R style declarations
int bla();
char alb();
unsigned foo();

int main() {
	int x;
	float y;
	char z;
	// call with some parameters
	bla(x);
	alb(x,y);
	foo(y,x,z);
	return 0;
}
)"
);

		// check whether there is a temporary file
		tmpFile = file.getPath();
		EXPECT_TRUE(fs::exists(tmpFile));

		// parse temporary file
		core::NodeManager manager;
		ConversionJob job(file);
		auto code = job.execute(manager);
		EXPECT_TRUE(code);

		// check that we adjusted the function types correctly
		auto msg = insieme::core::checks::check(code);
		EXPECT_EQ(msg.size(), 0);
	}
}

} // namespace frontend
} // namespace insieme
