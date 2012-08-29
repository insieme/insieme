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

#include <gtest/gtest.h>

#include "insieme/backend/sequential/sequential_backend.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/utils/compiler/compiler.h"

namespace insieme {
namespace backend {

	using namespace core;

	TEST(OperatorTest, NarrowTest) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		ProgramPtr prog = builder.parse(
			""
			"let int = int<4>;"
			""
			"let pair = struct {"
			"	int first;"
			"	int second;"
			"};"
			""
			"let obj = struct {"
			"	int a;"
			"	vector<int,4> b;"
			"	pair c;"
			"};"
			""
			"unit main() {"
			""
			"	ref<obj> o;"
			""
			"	// check element o.a\n"
			"	o.a = 0;"
			"	print(\"o.a = %d\\n\", *o.a);"
			""
			"	// updated member a directly\n"
			"	ref<int> x = ref.narrow(o, dp.member(dp.root, lit(\"a\")), lit(int));"
			"	x = 12;"
			"	print(\"Equal: %d\\n\", ref.eq(o.a, x));"
			"	print(\"x = %d \t o.a = %d\\n\", *x, *o.a);"
			""
			""
			"	// udpate element of b directly\n"
			"	o.b[2u] = 0;"
			"	print(\"o.b[2] = %d\\n\", *o.b[2u]);"
			""
			"	ref<int> y = ref.narrow(o, dp.element(dp.member(dp.root, lit(\"b\")), 2u), lit(int));"
			"	y = 12;"
			"	print(\"Equal: %d\\n\", ref.eq(o.b[2u], y));"
			"	print(\"y = %d \t o.b[2] = %d\\n\", *y, *o.b[2u]);"
			""
			""
			"	// expand a definition\n"
			"	ref<vector<int,4>> v = ref.expand(y, dp.element(dp.root, 2u), lit(vector<int,4>));"
			"	v[1u] = 10;"
			"	v[2u] = 14;"
			""
			"	print(\"Equal: %d\\n\", o.b == v);"
			"	print(\"v[1] = %d \t v[2] = %d \t o.v[1] = %d \t o.v[2] = %d \t y = %d\\n\","
			"			*v[1u], *v[2u], *o.b[1u], *o.b[2u], *y);"
			""
			""
			"	// handle nested element\n"
			"	ref<int> first = ref.narrow(o, dp.member(dp.member(dp.root, lit(\"c\")), lit(\"first\")), lit(int));"
			""
			"	// check reference equality\n"
			"	print(\"Equal: %d\\n\", ref.eq(o.c.first, first));"
			""
			"	// and the reverse\n"
			"	ref<obj> full = ref.expand(first, dp.member(dp.member(dp.root, lit(\"c\")), lit(\"first\")), lit(obj));"
			"	print(\"Equal: %d\\n\", ref.eq(o,full));"
			"}"
		).as<core::ProgramPtr>();

		// check whether parsing was successful
		ASSERT_TRUE(prog);

		// print program using pretty printer
		std::cout << core::printer::PrettyPrinter(prog);

		// build it using the sequential backend
		auto targetCode = sequential::SequentialBackend::getDefault()->convert(prog);

		std::cout << "Converted Code: \n" << *targetCode << "\n";

		// try compiling code
		EXPECT_TRUE(utils::compiler::compile(*targetCode));
	}


} // end namespace backend
} // end namespace insieme
