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

#include "insieme/frontend/convert.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/tu/ir_translation_unit_check.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"

#include "test_utils.inc"

namespace insieme {
namespace frontend {

	TEST(Converter, ConverToTranslationUnit) {
		core::NodeManager mgr;

		ConversionSetup setup;
		setup.setStandard(ConversionSetup::Cxx03);
		auto tu = convert(mgr, SRC_DIR "/inputs/conversion_test.cpp", setup);

//		std::cout << tu << "\n";
		EXPECT_FALSE(tu.getFunctions().empty());

		auto messages = checkTU (tu);
		for (const core::checks::Message& msg : messages.getAll()){
			msg.printTo(std::cout) << std::endl;
		}
	}

	TEST(Converter, ConvertToProgram) {
		core::NodeManager mgr;

		ConversionSetup setup;
		setup.setStandard(ConversionSetup::Cxx03);
		auto tu = convert(mgr, SRC_DIR "/inputs/conversion_test.cpp", setup);

//		std::cout << tu << "\n";
		EXPECT_FALSE(tu.getFunctions().empty());

//		std::cout << "\n\n----------------------------------------------------------------------\n\n";

		auto program = tu::toProgram(mgr, tu);
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		string res = toString(core::printer::PrettyPrinter(program));

		EXPECT_PRED2(containsSubString, res, "let fun003 = recFun v37 {");		// even header
		EXPECT_PRED2(containsSubString, res, "let fun004 = recFun v38 {");		// odd header

		// check global variable setup
		EXPECT_PRED2(containsSubString, res, "counter := 10;");
		// split up this assertions because the local static
		// variables have some random suffix
		EXPECT_PRED2(containsSubString, res, "PI");
		EXPECT_PRED2(containsSubString, res, " := CreateStatic(type<real<8>>);");
		EXPECT_PRED2(containsSubString, res, "InitStatic(PI");
		EXPECT_PRED2(containsSubString, res, ", 3.0);");


	}

	TEST(Converter, Globals) {
		core::NodeManager mgr;

		auto tu = convert(mgr, SRC_DIR "/inputs/globals.c");

//		std::cout << tu << "\n";
		EXPECT_FALSE(tu.getFunctions().empty());
		EXPECT_FALSE(tu.getGlobals().empty());

		EXPECT_EQ( tu.getGlobals().size(), 4);  // 4 globals, no one cares for the poor extern and static one

	}

	TEST(Converter, ConversionSetup) {

		ConversionSetup setup;

		// check Auto
		setup.setStandard(ConversionSetup::Auto);

		EXPECT_TRUE(setup.isCxx("test.cpp"));
		EXPECT_TRUE(setup.isCxx("test/test.cpp"));
		EXPECT_TRUE(setup.isCxx("test.cc"));
		EXPECT_TRUE(setup.isCxx("test.cxx"));
		EXPECT_TRUE(setup.isCxx("test.C"));

		EXPECT_FALSE(setup.isCxx("test"));
		EXPECT_FALSE(setup.isCxx("test.c"));
		EXPECT_FALSE(setup.isCxx("test/test.a"));


		// check C99
		setup.setStandard(ConversionSetup::C99);

		EXPECT_FALSE(setup.isCxx("test.cpp"));
		EXPECT_FALSE(setup.isCxx("test/test.cpp"));
		EXPECT_FALSE(setup.isCxx("test.cc"));
		EXPECT_FALSE(setup.isCxx("test.cxx"));
		EXPECT_FALSE(setup.isCxx("test.C"));

		EXPECT_FALSE(setup.isCxx("test"));
		EXPECT_FALSE(setup.isCxx("test.c"));
		EXPECT_FALSE(setup.isCxx("test/test.a"));

		// check Cxx03
		setup.setStandard(ConversionSetup::Cxx03);

		EXPECT_TRUE(setup.isCxx("test.cpp"));
		EXPECT_TRUE(setup.isCxx("test/test.cpp"));
		EXPECT_TRUE(setup.isCxx("test.cc"));
		EXPECT_TRUE(setup.isCxx("test.cxx"));
		EXPECT_TRUE(setup.isCxx("test.C"));

		EXPECT_TRUE(setup.isCxx("test"));
		EXPECT_TRUE(setup.isCxx("test.c"));
		EXPECT_TRUE(setup.isCxx("test/test.a"));

	}

	TEST(Converter, EvenOdd) {

		// create a temporary source file
		Source file(
				R"(
					#define bool int

					#define true 1
					#define false 0

					bool even(unsigned x);
					bool odd(unsigned x);

					bool even(unsigned x) {
						return (x==0)?true:odd(x-1);
					}

					bool odd(unsigned x) {
						return (x==0)?false:even(x-1);
					}

					int main(int argc, char* argv[]) {
						int x = 10;
						even(x);
						odd(x);
						return 0;
					}

				)"
		);

		// check the resulting translation unit
		core::NodeManager manager;
		core::IRBuilder builder(manager);
		auto irtu = ConversionJob(file).toTranslationUnit(manager);
//		std::cout << irtu << "\n";

		// there should be 3 function symbols
		EXPECT_EQ(3u, irtu.getFunctions().size());

		auto print = [&](const core::NodePtr& node) {
			return toString(core::printer::PrettyPrinter(builder.normalize(node), core::printer::PrettyPrinter::PRINT_SINGLE_LINE | core::printer::PrettyPrinter::NO_LET_BINDINGS));
		};

		// among them, even and odd
		auto evenLit = builder.parseExpr("lit(\"even\":(uint<4>)->int<4>)").as<core::LiteralPtr>();
		auto oddLit = builder.parseExpr("lit(\"odd\":(uint<4>)->int<4>)").as<core::LiteralPtr>();
		ASSERT_TRUE(irtu[evenLit]);
		ASSERT_TRUE(irtu[oddLit]);

		// check their definition
		EXPECT_EQ("fun(uint<4> v1) -> int<4> {return (v1==0u)?1:odd(v1-1u);}", print(irtu[evenLit]));
		EXPECT_EQ("fun(uint<4> v1) -> int<4> {return (v1==0u)?0:even(v1-1u);}", print(irtu[oddLit]));

		// check resolved version
		auto even = irtu.resolve(evenLit);
		EXPECT_EQ("recFun v0 {v0 = fun(uint<4> v1) -> int<4> {return (v1==0u)?1:v4(v1-1u);};v4 = fun(uint<4> v5) -> int<4> {return (v5==0u)?0:v0(v5-1u);};}", print(even));

		auto odd = irtu.resolve(oddLit);
		EXPECT_EQ("recFun v0 {v0 = fun(uint<4> v1) -> int<4> {return (v1==0u)?0:v4(v1-1u);};v4 = fun(uint<4> v5) -> int<4> {return (v5==0u)?1:v0(v5-1u);};}", print(odd));

		auto program = tu::toProgram(manager, irtu);
//		dump(program);

		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);
	}

} // end frontend
} // end insieme
