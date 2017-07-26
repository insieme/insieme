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

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/utils/logging.h"


namespace insieme {
namespace backend {

	TEST(CppReferences, Dummy) {
		// while all tests are disabled ..
	}

//	TEST(CppReferences, Mutable) {
//		core::NodeManager mgr;
//		core::IRBuilder builder(mgr);
//
//		std::map<string, core::NodePtr> symbols;
//		symbols["cpp2ir"] = mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR();
//		symbols["ir2cpp"] = mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp();
//
//
//		// create a code fragment including some member functions
//		auto fragments = builder.parseAddressesProgram("let int = int<4>;"
//		                                               ""
//		                                               "let intRef = struct { ref<int> _cpp_ref };"
//		                                               ""
//		                                               "let f = lambda (intRef x)->unit { };"
//		                                               "let g = lambda (ref<int> x)->unit { };"
//		                                               ""
//		                                               "int main() {"
//		                                               "	"
//		                                               "	decl ref<int> a = var(12);"
//		                                               "	decl intRef b = ir2cpp(a);"
//		                                               "	decl ref<int> c = cpp2ir($b$);"
//		                                               "	"
//		                                               "	f(ir2cpp(a));"
//		                                               "	f(b);"
//		                                               "	"
//		                                               "	g(a);"
//		                                               "	g(cpp2ir(b));"
//		                                               "	return 0;"
//		                                               "}",
//		                                               symbols);
//
//
//		ASSERT_EQ(1, fragments.size());
//
//		// check type
//		EXPECT_PRED1(core::analysis::isCppRef, fragments[0].as<core::VariablePtr>()->getType());
//
//		// extract program
//		core::ProgramPtr res = fragments[0].getRootNode().as<core::ProgramPtr>();
//
//		// standard checks
//		ASSERT_TRUE(res);
//		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);
//
//		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
//		ASSERT_TRUE((bool)targetCode);
//
//		//		std::cout << *targetCode;
//
//		// check generated code
//		auto code = toString(*targetCode);
//		EXPECT_PRED2(containsSubString, code, "void f(int32_t& x");
//		EXPECT_PRED2(containsSubString, code, "void g(int32_t* x");
//		EXPECT_PRED2(containsSubString, code, "int32_t a = 12;");
//		EXPECT_PRED2(containsSubString, code, "int32_t& b = a;");
//		EXPECT_PRED2(containsSubString, code, "int32_t* c = &b;");
//		EXPECT_PRED2(containsSubString, code, "f(a);");
//		EXPECT_PRED2(containsSubString, code, "f(b);");
//		EXPECT_PRED2(containsSubString, code, "g(&a);");
//		EXPECT_PRED2(containsSubString, code, "g(&b);");
//
//		EXPECT_PRED2(notContainsSubString, code, "const int32_t& b = a;");
//
//		// check whether code is compiling
//		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
//		compiler.addFlag("-c"); // do not run the linker
//		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
//	}
//
//	TEST(CppReferences, Const) {
//		core::NodeManager mgr;
//		core::IRBuilder builder(mgr);
//
//		std::map<string, core::NodePtr> symbols;
//		symbols["cpp2ir"] = mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR();
//		symbols["ir2cpp"] = mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp();
//
//
//		// create a code fragment including some member functions
//		auto fragments = builder.parseAddressesProgram("let int = int<4>;"
//		                                               ""
//		                                               "let intRef = struct { src<int> _const_cpp_ref };"
//		                                               ""
//		                                               "let f = lambda (intRef x)->unit { };"
//		                                               "let g = lambda (ref<int> x)->unit { };"
//		                                               ""
//		                                               "int main() {"
//		                                               "	"
//		                                               "	decl ref<int> a = var(12);"
//		                                               "	decl intRef b = ir2cpp(a);"
//		                                               "	decl src<int> c = cpp2ir($b$);"
//		                                               "	"
//		                                               "	f(ir2cpp(a));"
//		                                               "	f(b);"
//		                                               "	"
//		                                               "	g(a);"
//		                                               "	g(cpp2ir(b));"
//		                                               "	return 0;"
//		                                               "}",
//		                                               symbols);
//
//
//		ASSERT_EQ(1, fragments.size());
//
//		// check type
//		EXPECT_PRED1(core::analysis::isConstCppRef, fragments[0].as<core::VariablePtr>()->getType());
//
//		// extract program
//		core::ProgramPtr res = fragments[0].getRootNode().as<core::ProgramPtr>();
//
//		// standard checks
//		ASSERT_TRUE(res);
//		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);
//
//		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
//		ASSERT_TRUE((bool)targetCode);
//
//		//		std::cout << *targetCode;
//
//		// check generated code
//		auto code = toString(*targetCode);
//		EXPECT_PRED2(containsSubString, code, "void f(const int32_t& x");
//		EXPECT_PRED2(containsSubString, code, "void g(int32_t* x");
//		EXPECT_PRED2(containsSubString, code, "int32_t a = 12;");
//		EXPECT_PRED2(containsSubString, code, "const int32_t& b = a;");
//		EXPECT_PRED2(containsSubString, code, "int32_t* c = &b;");
//		EXPECT_PRED2(containsSubString, code, "f(a);");
//		EXPECT_PRED2(containsSubString, code, "f(b);");
//		EXPECT_PRED2(containsSubString, code, "g(&a);");
//		EXPECT_PRED2(containsSubString, code, "g(&b);");
//
//		// check whether code is compiling
//		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
//		compiler.addFlag("-c"); // do not run the linker
//		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
//	}

} // namespace backend
} // namespace insieme
