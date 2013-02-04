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

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {

	TEST(CppSnippet, HelloWorld) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(
				"let int = int<4>;"
				"let Math = struct {};"
				""
				"let id = Math::(int a)->int { return a; };"
				""
				"let sum = Math::(int a, int b)->int { return a + b; };"
				""
				"int main() {"
				"	ref<Math> m;"
				"	"
				"	print(\"%d\\n\", m.id(12));"
				"	print(\"%d\\n\", m.sum(12,14));"
				"}"
		);

		ASSERT_TRUE(program);
		std::cout << "Program: " << *program << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		std::cout << "Converted: \n" << *converted << std::endl;

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(CppSnippet, Counter) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment implementing a counter class
		core::ProgramPtr program = builder.parseProgram(
				R"(
				let int = int<4>;
				
				let Counter = struct {
					int value;
				};
				
				let reset = Counter::()->unit {
					this->value = 0;
				};
				
				let inc = Counter::()->int {
					this->value = this->value + 1;
					return *this->value;
				};
				
				let dec = Counter::()->int {
					this->value = this->value - 1;
					return *this->value;
				};
				
				let get = Counter::()->int {
					return *this->value;
				};
				
				let set = Counter::(int x)->unit { 
					this->value = x;
				};
				
				let p = Counter::()->unit {
					print("%d\n", this->get());
				};
				
				int main() {
					ref<Counter> c;
					c.reset();
					c.p();
					c.inc();
					c.p();
					c.inc();
					c.p();
					c.dec();
					c.p();
					c.set(14);
					c.p();
				}
				)"
		);

		ASSERT_TRUE(program);
		std::cout << "Program: " << *program << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		std::cout << "Converted: \n" << *converted << std::endl;

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		// compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));

	}

	TEST(CppSnippet, Inheritance) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment implementing a counter class
		core::ProgramPtr program = builder.parseProgram(
				R"(
				let int = int<4>;

				let A = struct {
					int x;
				};

				let B = struct : A {
					int y;
				};

				let C = struct : B {
					int z;
				};

				int main() {

					// -------- handle an instance of A --------
					ref<A> a;
					a.x = 1;
					
					
					// -------- handle an instance of B --------
					ref<B> b;
					
					// direct access
					b.as(A).x = 1;
					b.y = 2;
					
					// indirect access of A's x
					auto bA = b.as(A);
					bA.x = 3;
					
					
					// -------- handle an instance of C --------
					ref<C> c;
					
					// access B's A's x
					c.as(B).as(A).x = 1;

					// access C's A's x
					c.as(B).y = 2;
					
					c.z = 3;
				}
				)"
		);

		ASSERT_TRUE(program);
		std::cout << "Program: " << core::printer::PrettyPrinter(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		std::cout << "Converted: \n" << *converted << std::endl;

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));

	}

} // namespace backend
} // namespace insieme
