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

#include "insieme/core/transform/sequentialize.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/inline.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/normalize.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace core {
namespace transform {

	TEST(Manipulation, MultiReturnInlineSimple) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		CallExprAddress code = builder.parseAddressesStatement(R"1N5P1RE(
			alias int = int<4>;
			def fun = (a : int, b : int) -> int { 
			  if(a<4) { return a + 2*b; } 
			  return a - b; 
			};
			{
				var ref<int,f,f,plain> x = ref_var(0); 
				$x = fun(3,6)$; 
			}
        )1N5P1RE")[0].as<CallExprAddress>();

		ASSERT_TRUE(code);
		EXPECT_TRUE(check(code, checks::getFullCheck()).empty()) << check(code, checks::getFullCheck());

		CompoundStmtPtr inlined = builder.normalize(inlineMultiReturnAssignment(mgr, code.getAddressedNode()));

		// std::cout << printer::PrettyPrinter(code.getRootNode()) << "\n\ninlined:\n" << printer::PrettyPrinter(inlined) << "\n****\n";

		EXPECT_EQ("{{decl ref<bool,f,f,plain> v0 =  var(false);{if(3<4) {{v28 = 3+2*6;v0 = true;};};if(!v0) {{v28 = 3-6;v0 = true;};};};};}",
		          toString(printer::PrettyPrinter(core::analysis::normalize(inlined), printer::PrettyPrinter::PRINT_SINGLE_LINE)));
		EXPECT_TRUE(check(inlined, checks::getFullCheck()).empty()) << check(inlined, checks::getFullCheck());
	}

	TEST(Manipulation, MultiReturnInlineLooping) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		CallExprAddress code = builder.parseAddressesStatement(R"1N5P1RE(
			alias int = int<4>;
			def fun = (a : int, b : int) -> int { 
				if(a<4) { return a + 2*b; } 
				var ref<int,f,f,plain> x = ref_var(a); 
				while(true) { 
					x = x+1; 
					if(x>b) { return x - b; } 
				} 
			};
			{
				var ref<int,f,f,plain> x = ref_var(0); 
				$x = fun(3,6)$; 
			}
        )1N5P1RE")[0].as<CallExprAddress>();

		ASSERT_TRUE(code);
		EXPECT_TRUE(check(code, checks::getFullCheck()).empty()) << check(code, checks::getFullCheck());

		CompoundStmtPtr inlined = builder.normalize(inlineMultiReturnAssignment(mgr, code.getAddressedNode()));

		// std::cout << printer::PrettyPrinter(code.getRootNode()) << "\n\ninlined:\n" << printer::PrettyPrinter(inlined) << "\n****\n";

		EXPECT_EQ("{{decl ref<bool,f,f,plain> v0 =  var(false);{if(3<4) {{v29 = 3+2*6;v0 = true;};};if(!v0) {decl ref<int<4>,f,f,plain> v1 =  "
		          "var(3);while(true && !v0) {v1 = v1+1;if(v1>6) {{v29 = v1-6;v0 = true;};};};};};};}",
		          toString(printer::PrettyPrinter(core::analysis::normalize(inlined), printer::PrettyPrinter::PRINT_SINGLE_LINE)));
		EXPECT_TRUE(check(inlined, checks::getFullCheck()).empty()) << check(inlined, checks::getFullCheck());
	}


	TEST(Manipulation, MultiReturnInlineUnit) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		CallExprAddress code = builder.parseAddressesStatement("alias int = int<4>;"
		                                                       "def fun = (a : int, b : int) -> int { "
		                                                       "	if(a<4) { return a + 2*b; } "
		                                                       "	var ref<int,f,f,plain> x = ref_var(a); "
		                                                       "	while(true) { "
		                                                       "		x = x+1; "
		                                                       "		if(x>b) { return x - b; } "
		                                                       "	} "
		                                                       "}; "
		                                                       "{"
		                                                       "	var ref<int,f,f,plain> x = ref_var(0); "
		                                                       "	$fun(3,6)$; "
		                                                       "}")[0]
		                           .as<CallExprAddress>();

		ASSERT_TRUE(code);
		EXPECT_TRUE(check(code, checks::getFullCheck()).empty()) << check(code, checks::getFullCheck());

		CompoundStmtPtr inlined = inlineMultiReturnPlainCall(mgr, code.getAddressedNode());

		// std::cout << printer::PrettyPrinter(code.getRootNode()) << "\n\ninlined:\n" << printer::PrettyPrinter(inlined) << "\n****\n";

		EXPECT_EQ("{{decl ref<bool,f,f,plain> v0 =  var(false);{if(3<4) {{v0 = true;};};if(!v0) {decl ref<int<4>,f,f,plain> v1 =  var(3);while(true && !v0) "
		          "{v1 = v1+1;if(v1>6) {{v0 = true;};};};};};};}",
		          toString(printer::PrettyPrinter(core::analysis::normalize(inlined), printer::PrettyPrinter::PRINT_SINGLE_LINE)));
		EXPECT_TRUE(check(inlined, checks::getFullCheck()).empty()) << check(inlined, checks::getFullCheck());
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
