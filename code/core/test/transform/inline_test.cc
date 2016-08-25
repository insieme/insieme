/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

namespace insieme {
namespace core {
namespace transform {

	TEST(Manipulation, MultiReturnInlineSimple) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		CallExprAddress code = builder.parseAddressesProgram(R"1N5P1RE(
			alias int = int<4>;
			def fun = (a : int, b : int) -> int {
			  if(a<4) { return a + 2*b; }
			  return a - b;
			};
			int main() {
				var ref<int,f,f,plain> x = 0;
				$x = fun(3,6)$;
			}
        )1N5P1RE")[0].as<CallExprAddress>();

		ASSERT_TRUE(code);
		EXPECT_TRUE(check(code, checks::getFullCheck()).empty()) << check(code, checks::getFullCheck());

		code = code.switchRoot(builder.normalize(code.getRootNode()));

		CompoundStmtPtr inlined = builder.normalize(inlineMultiReturnAssignment(mgr, code.getAddressedNode()));

		// std::cout << printer::PrettyPrinter(code.getRootNode()) << "\n\ninlined:\n" << printer::PrettyPrinter(inlined) << "\n****\n";

		EXPECT_EQ("{{var ref<bool,f,f,plain> v1 = false;{if(3<4) {{v0 = 3+2*6;v1 = true;}}if(!*v1) {{v0 = 3-6;v1 = true;}}}}}",
		          toString(printer::PrettyPrinter(inlined, printer::PrettyPrinter::PRINT_SINGLE_LINE)));
		EXPECT_TRUE(check(inlined, checks::getFullCheck()).empty()) << check(inlined, checks::getFullCheck());
	}

	TEST(Manipulation, MultiReturnInlineLooping) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		CallExprAddress code = builder.parseAddressesStatement(R"1N5P1RE(
			alias int = int<4>;
			def fun = (a : int, b : int) -> int {
				if(a<4) { return a + 2*b; }
				var ref<int,f,f,plain> x = a;
				while(true) {
					x = x+1;
					if(x>b) { return x - b; }
				}
			};
			{
				var ref<int,f,f,plain> x = 0;
				$x = fun(3,6)$;
			}
        )1N5P1RE")[0].as<CallExprAddress>();

		ASSERT_TRUE(code);
		EXPECT_TRUE(check(code, checks::getFullCheck()).empty()) << check(code, checks::getFullCheck());

		code = code.switchRoot(builder.normalize(code.getRootNode()));

		CompoundStmtPtr inlined = builder.normalize(inlineMultiReturnAssignment(mgr, code.getAddressedNode()));

	printer::PrettyPrinter printer0(core::analysis::normalize(inlined), printer::PrettyPrinter::PRINT_SINGLE_LINE);
		EXPECT_EQ("decl fun000 : (ref<bool,f,f,plain>) -> bool;def fun000 = function (v0 : ref<ref<bool,f,f,plain>,f,f,plain>) -> bool {return !**v0;};{{var ref<bool,f,f,plain> v1 = false;{if(3<4) {{v0 = 3+2*6;v1 = true;}}if(!*v1) {var ref<int<4>,f,f,plain> v2 = 3;while(true && !*v1) {v2 = *v2+1;if(*v2>6) {{v0 = *v2-6;v1 = true;}}}}}}}",
			      toString(printer0));
		EXPECT_TRUE(check(inlined, checks::getFullCheck()).empty()) << check(inlined, checks::getFullCheck());
	}


	TEST(Manipulation, MultiReturnInlineUnit) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		CallExprAddress code = builder.parseAddressesStatement("alias int = int<4>;"
		                                                       "def fun = (a : int, b : int) -> int { "
		                                                       "	if(a<4) { return a + 2*b; } "
		                                                       "	var ref<int,f,f,plain> x = a; "
		                                                       "	while(true) { "
		                                                       "		x = x+1; "
		                                                       "		if(x>b) { return x - b; } "
		                                                       "	} "
		                                                       "}; "
		                                                       "{"
		                                                       "	var ref<int,f,f,plain> x = 0; "
		                                                       "	$fun(3,6)$; "
		                                                       "}")[0]
		                           .as<CallExprAddress>();

		ASSERT_TRUE(code);
		EXPECT_TRUE(check(code, checks::getFullCheck()).empty()) << check(code, checks::getFullCheck());

		CompoundStmtPtr inlined = inlineMultiReturnPlainCall(mgr, code.getAddressedNode());

		// std::cout << printer::PrettyPrinter(code.getRootNode()) << "\n\ninlined:\n" << printer::PrettyPrinter(inlined) << "\n****\n";

		EXPECT_EQ("decl fun000 : (ref<bool,f,f,plain>) -> bool;def fun000 = function (v0 : ref<ref<bool,f,f,plain>,f,f,plain>) -> bool {return !**v0;};{{var ref<bool,f,f,plain> v0 = false;{if(3<4) {{v0 = true;}}if(!*v0) {var ref<int<4>,f,f,plain> v1 = 3;while(true && !*v0) {v1 = *v1+1;if(*v1>6) {{v0 = true;}}}}}}}",
		          toString(printer::PrettyPrinter(core::analysis::normalize(inlined), printer::PrettyPrinter::PRINT_SINGLE_LINE)));
		EXPECT_TRUE(check(inlined, checks::getFullCheck()).empty()) << check(inlined, checks::getFullCheck());
	}

	TEST(Manipulation, inlineMultiReturn) {
		NodeManager nm;
		IRBuilder b(nm);

		// test: if callexpr is refassign
		{
			auto ir = b.parseAddressesStatement(R"( def fun = () -> int<4> { return 1; }; { var ref<int<4>> a; $a = fun()$; } )")[0].as<CallExprAddress>();

			ASSERT_TRUE(ir);
			EXPECT_TRUE(check(ir, checks::getFullCheck()).empty()) << check(ir, checks::getFullCheck());

			auto result = inlineMultiReturn(nm, ir);

			EXPECT_EQ("{{var ref<bool,f,f,plain> v0 = false;{{v1 = 1;v0 = true;}}}}",
				toString(printer::PrettyPrinter(core::analysis::normalize(result), printer::PrettyPrinter::PRINT_SINGLE_LINE)));
		}

		// test: if callexpr is not a refassign
		{
			auto ir = b.parseAddressesStatement(R"( def fun = () -> int<4> { var ref<int<4>> a = 3; return 1+2+a; }; { var ref<int<4>> a; $fun()$; } )")[0].as<CallExprAddress>();

			ASSERT_TRUE(ir);
			EXPECT_TRUE(check(ir, checks::getFullCheck()).empty()) << check(ir, checks::getFullCheck());

			auto result = inlineMultiReturn(nm, ir);


			EXPECT_EQ("{{var ref<bool,f,f,plain> v0 = false;{var ref<int<4>,f,f,plain> v1 = 3;{v0 = true;}}}}",
				toString(printer::PrettyPrinter(core::analysis::normalize(result), printer::PrettyPrinter::PRINT_SINGLE_LINE))) <<
				toString(printer::PrettyPrinter(core::analysis::normalize(result), printer::PrettyPrinter::PRINT_SINGLE_LINE));
		}
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
