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

#include <fstream>

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/config.h"

#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"


namespace insieme {
namespace backend {


	TEST(FunctionCall, SimpleFunctions) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
				R"(
				alias int = int<4>;

				def f = (a : int, b : int)->int {
					return a + b * a;
				};

				int<4> main() {
					f(12,15);
					return 0;
				}
				)"
		);

		ASSERT_TRUE(program);

		LOG(INFO) << "Printing the IR: " << core::printer::PrettyPrinter(program);

		LOG(INFO) << "Converting IR to C...";
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(FunctionCall, SimpleVarDecl) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
				R"(
				int<4> main() {
					var ref<int<4>,f,f,plain> c;
					var ref<int<4>,f,f,plain> v = 3;
					return c+v;
				}
				)"
		);

		ASSERT_TRUE(program);

		LOG(INFO) << "Printing the IR: " << core::printer::PrettyPrinter(program);

		LOG(INFO) << "Converting IR to C...";
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(FunctionCall, Templates) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram("int<4> main() {"
		                                                "  (dtype : type<'a>, size : type<'s>)->unit {"
		                                                "    var ref<'a> v0;"
		                                                "    var int<'s> v1 = lit(\"0\":int<'s>);"
		                                                "  } (type_lit(real<4>), type_lit(8));"
		                                                "  return 0;"
		                                                "}");

		ASSERT_TRUE(program);


		LOG(INFO) << "Printing the IR: " << core::printer::PrettyPrinter(program);

		LOG(INFO) << "Converting IR to C...";
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(FunctionCall, Pointwise) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// Operation: array.pointwise
		// Type: (('a,'b)->'c) -> (array<'a,'l>,array<'b,'l>)->array<'c,'l>

		std::map<string, core::NodePtr> symbols;
		symbols["v1"] = builder.parseExpr("*<ref<array<int<4>,4>>>{1,2,3,4}");
		symbols["v2"] = builder.parseExpr("*<ref<array<int<4>,4>>>{5,6,7,8}");

		core::ProgramPtr program = builder.parseProgram("unit main() {"
		                                                "	array_pointwise(int_add)(v1,v2);"
		                                                "}",
		                                                symbols);
		ASSERT_TRUE(program);


		auto converted = sequential::SequentialBackend::getDefault()->convert(program);

		string code = toString(*converted);
		EXPECT_FALSE(code.find("<?>") != string::npos);

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag(string("-I ") + utils::getInsiemeSourceRootDir() + "simple_backend/include/insieme/simple_backend/runtime/");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler)) << "Code: \n" << *converted;
	}


	TEST(FunctionCall, TypeLiterals) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a function accepting a type literal

		core::ProgramPtr program = builder.parseProgram("int<4> main() {"
		                                                "	(stype : type<'a>)->int<4> {"
		                                                "		return 5;"
		                                                "	} (type_lit(real<4>));"
		                                                "	return 0;"
		                                                "}");
		ASSERT_TRUE(program);

		LOG(DEBUG) << "Program: " << *program << std::endl;

		auto converted = sequential::SequentialBackend::getDefault()->convert(program);

		LOG(DEBUG) << "Converted: \n" << *converted << std::endl;

		string code = toString(*converted);
		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(containsSubString, code, "fun_1()");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(FunctionCall, GenericFunctionAndTypeLiteral) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a function accepting a type literal

		core::ProgramPtr program = builder.parseProgram(R"(
    		int<4> main() {
    			(data : ref<array<'a,'l>>)->uint<8> {
    				return sizeof(type_lit('a));
    			} (ref_null(type_lit(array<real<4>,12>),type_lit(f),type_lit(f)));
    			return 0;
    		}
    )");
		ASSERT_TRUE(program);

		LOG(DEBUG) << "Program: " << *program << std::endl;

		auto converted = sequential::SequentialBackend::getDefault()->convert(program);

		LOG(DEBUG) << "Converted: \n" << *converted << std::endl;

		string code = toString(*converted);
		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(containsSubString, code, "sizeof(float)");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(FunctionCall, RefNewCalls) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a function accepting a type literal
		core::TypePtr intType = manager.getLangBasic().getInt4();
		core::TypePtr type = builder.refType(intType);
		core::VariablePtr var = builder.variable(type, 1);

		core::ExpressionPtr init = builder.undefinedNew(intType);
		core::DeclarationStmtPtr decl = builder.declarationStmt(var, init);

		auto converted = sequential::SequentialBackend::getDefault()->convert(decl);

		LOG(DEBUG) << "Converted: \n" << *converted << std::endl;

		string code = toString(*converted);
		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(containsSubString, code, "int32_t* var_1 = (int32_t*)malloc(sizeof(int32_t))");
	}

	TEST(FunctionCall, FixedSizedArrayInit) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ExpressionPtr zero = builder.literal(manager.getLangBasic().getUInt8(), "0");
		core::ExpressionPtr offset = builder.parseExpr("<ref<array<uint<8>,3>>>{0ul,0ul,0ul}");
		core::ExpressionPtr extFun = builder.parseExpr("lit(\"call_vector\" : (ref<array<uint<8>,3>>)->unit )");
		core::ExpressionPtr call = builder.callExpr(manager.getLangBasic().getUnit(), extFun, toVector(offset));

		core::IRBuilder::EagerDefinitionMap symbols {{ "call", call }};

		// create a code fragment including the boolean constants
		core::ProgramPtr program = builder.parseProgram("int<4> main() { call; return 0; }", symbols);
		ASSERT_TRUE(program);

		LOG(DEBUG) << "Program: " << *program << std::endl;

		auto converted = sequential::SequentialBackend::getDefault()->convert(program);

		LOG(DEBUG) << "Converted: \n" << *converted << std::endl;

		string code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "call_vector((uint64_t(*)[3])(&(__insieme_type_2){{(uint64_t)0, (uint64_t)0, (uint64_t)0}}))");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(Literals, BoolLiterals) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including the boolean constants
		core::ProgramPtr program = builder.parseProgram("int<4> main() {"
		                                                "	true;"
		                                                "	false;"
		                                                "	return 0;"
		                                                "}");
		ASSERT_TRUE(program);

		LOG(DEBUG) << "Program: " << *program << std::endl;

		auto converted = sequential::SequentialBackend::getDefault()->convert(program);

		LOG(DEBUG) << "Converted: \n" << *converted << std::endl;

		string code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "true");
		EXPECT_PRED2(containsSubString, code, "false");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(Parallel, NestedLambdaTypeDeduction) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		core::ProgramPtr program = builder.parseProgram(
		    R"(
			alias int = int<4>;
			alias uint = uint<4>;

			def differentbla = (x : 'b) -> unit {
				auto m = x;
				auto l = m;
			};

			def bla = (f : 'a) -> unit {
				let anotherbla = (x : 'a) -> unit {
					auto m = x;
				};
				anotherbla(f);
				differentbla(f);
				parallel(job { auto l = f; });
			};

			int main() {
				// some bla
				var int x = 10;
				bla(x);
				return 0;
			}
			)");
		LOG(DEBUG) << "Program: " << *program << std::endl;

		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		LOG(DEBUG) << "Converted Seq: \n" << *converted << std::endl;
		auto converted_rt = runtime::RuntimeBackend::getDefault()->convert(program);
		LOG(DEBUG) << "Converted Run: \n" << *converted_rt << std::endl;
	}


	TEST(Arrays, Allocation) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment allocating an array on the stack and using it
		core::ProgramPtr program = builder.parseProgram(
		    R"(
			alias int = int<4>;
			alias uint = uint<4>;

			int main() {

				// get a size for the variable-sized arrays
				var uint<inf> size = num_cast(12,type_lit(uint<inf>));

				// create two fixed-sized arrays on the stack and the heap
				var ref<array<int,10>> a;
				var ref<array<int,10>> b = ref_new(type_lit(array<int,10>));
				var ref<array<int,10>> c = ref_new_init(*a);

				var ref<array<int,inf>> e = ref_null(type_lit(array<int,inf>),type_lit(f),type_lit(f));

				// use the arrays
				a[2] = 123;
				b[4] = 321;
				c[1] = 456;
				e[7] = 123;

				return 0;
			}
			)");

		ASSERT_TRUE(program);

		LOG(INFO) << "Program: " << *program << std::endl;

		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		LOG(INFO) << "Converted Seq: \n" << *converted << std::endl;

		string code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "__insieme_type_1 a;");
		EXPECT_PRED2(containsSubString, code, "__insieme_type_1* b = (__insieme_type_1*)malloc(sizeof(__insieme_type_1));");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(PrimitiveType, LongLong) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment allocating an array on the stack and using it
		core::ProgramPtr program = builder.parseProgram(
		    R"(
			alias longlong = int<16>;

			int<4> main() {
				// just create a long-long variable
				var int<16> a = 10l;
				// just create a long-long variable
				var uint<16> b = 10ul;
				return 0;
			}
			)");

		ASSERT_TRUE(program);

		std::cout << "Program: " << std::endl;
		dump(program);
		std::cout << std::endl;

		auto converted = sequential::SequentialBackend::getDefault()->convert(program);

		std::cout << "Converted: \n" << *converted << std::endl;

		string code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "long long a = (int64_t)10;");
		EXPECT_PRED2(containsSubString, code, "unsigned long long b = (uint64_t)10;");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(FunctionCall, GenericFunctionsWithLazy) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
				def f = (a : ref<'a>, c : ('a)=>bool, v : ()=>'a)->ref<'a> {
					if(c(*a)) {
						a = v();
					}
					return a;
				};
				int<4> main() {

				var ref<int<4>> a = 1;
				var ref<real<4>> b = 2.0f;

				f(a, (a : int<4>)=> true, ()=>3);
				f(b, (b : real<4>)=> true, ()=>4.0f);

				return 0;
			}
		)");

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		LOG(INFO) << "Converting IR to C...";
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(FunctionCall, PassLabmdaToBind) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram("def f = ()->int<4> { return 4; };"
		                                                "def g = (a : ()=>'a)->'a { return a(); };"
		                                                "int<4> main() {"
		                                                "	g(f);"
		                                                "	return 0;"
		                                                "}");

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		LOG(INFO) << "Converting IR to C...";
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(Debugging, DebugCodePrinting) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram("alias int = int<4>;"
		                                                ""
		                                                "def f = ()->int { return 4; };"
		                                                ""
		                                                "def g = (x : int)->int { return x + 2; };"
		                                                ""
		                                                "int main() {"
		                                                "	return g(f());"
		                                                "}");

		ASSERT_TRUE(program);

		// without debug code
		{
			// check for semantic errors
			EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

			// create backend instance
			auto be = sequential::SequentialBackend::getDefault();

			// upbdate backend configuration
			be->getConfiguration().addIRCodeAsComment = false;

			LOG(INFO) << "Converting IR to C...";
			auto converted = be->convert(program);
			LOG(INFO) << "Printing converted code: " << *converted;

			string code = toString(*converted);

			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
			EXPECT_PRED2(notContainsSubString, code, "{\n    _ = function() -> int<4> {\n        return g(f());\n    };\n}");

			// try compiling the code fragment
			utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
			compiler.addFlag("-lm");
			compiler.addFlag("-c"); // do not run the linker
			EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
		}

		// with debug code
		{
			// check for semantic errors
			EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

			// create backend instance
			auto be = sequential::SequentialBackend::getDefault();

			// update backend configuration
			be->getConfiguration().addIRCodeAsComment = true;

			LOG(INFO) << "Converting IR to C...";
			auto converted = be->convert(program);
			LOG(INFO) << "Printing converted code: " << *converted;

			string code = toString(*converted);

			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
			EXPECT_PRED2(containsSubString, code, "{main = function () -> int<4> {\n        return g(f());\n    };\n}");

			// try compiling the code fragment
			utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
			compiler.addFlag("-lm");
			compiler.addFlag("-c"); // do not run the linker
			EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
		}
	}

	TEST(Types, RecursiveTypesSimple) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
				R"(
					alias int = int<4>;

					def struct List { value : int<4>; next : ref<List>; };

					int main() {
						var ref<List> x;
						return 0;
					}
				)"
		);

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		LOG(INFO) << "Converting IR to C...";
		auto converted = be->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));

	}

	TEST(Types, RecursiveTypesMutual) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
				R"(
					alias int = int<4>;

					decl struct B;
					def struct A { value : int<4>; next : ref<B>; };
					def struct B { value : int<4>; next : ref<A>; };

					int main() {
						var ref<A> x;
						return 0;
					}
				)"
		);

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		LOG(INFO) << "Converting IR to C...";
		auto converted = be->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));

	}

	TEST(Functions, RecursiveFunctionSimple) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
			R"(
				alias int = int<4>;

				decl f : ()->unit;
				def f = ()->unit { f(); };

				int main() {
					f();
					return 0;
				}
			)"
		);

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		LOG(INFO) << "Converting IR to C...";
		auto converted = be->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));

	}

	TEST(Functions, RecursiveFunctionMutual) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
			R"(
				alias int = int<4>;

				decl f : ()->unit;
				decl g : ()->unit;

				def f = ()->unit { g(); };
				def g = ()->unit { f(); };

				int main() {
					f();
					g();
					return 0;
				}
			)"
		);

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		LOG(INFO) << "Converting IR to C...";
		auto converted = be->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));

	}

	TEST(Functions, RecursiveFunctionEvenOdd) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
			R"(
				alias int = int<4>;

				decl even : ( int )->bool;
				decl odd  : ( int )->bool;

				def even = ( x : int )->bool { return ( x == 0 ) ? true : odd(x-1); };
				def odd  = ( x : int )->bool { return ( x == 0 ) ? false : even(x-1); };

				int main() {
					even(10);
					odd(12);
					return 0;
				}
			)"
		);

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		LOG(INFO) << "Converting IR to C...";
		auto converted = be->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(notContainsSubString, code, "<?>");
		EXPECT_PRED2(notContainsSubString, code, "<a>");
		EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));

	}

	TEST(Initialization, Array) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
			R"(
			int<4> main() {
				var ref<array<int<4>,5>,f,f> v0 = *<ref<array<int<4>,5>,f,f,plain>>(v0) {1,2,3,4,5};
				//int arr_all[5] = {1,2,3,4,5};

				var ref<array<int<4>,5>,f,f> v1 = *<ref<array<int<4>,5>,f,f,plain>>(v1) {1,2};
				//int arr_partial[5] = {1,2};

				var ref<array<int<4>,5>,f,f> v2 = *<ref<array<int<4>,5>,f,f,plain>>(v2) {0};
				//int arr_zero[5] = {0};

				var ref<array<int<4>,3>,f,f> v3 = *<ref<array<int<4>,3>,f,f,plain>>(v3) {0,1,2};
				//int arr_implied[] = {0,1,2};

				var ref<array<array<int<4>,3>,2>,f,f> v4 = *<ref<array<array<int<4>,3>,2>,f,f,plain>>(v4) {*<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {1,2,3},*<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {4,5,6}};
				//int arr_multi[2][3] = {{1,2,3}, {4,5,6}};

				var ref<array<array<int<4>,3>,2>,f,f,plain> v5 = *<ref<array<array<int<4>,3>,2>,f,f,plain>>(v5) {*<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {1},*<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {4,5}};
				//int arr_multi[2][3] = {{1}, {4,5}};

				return 0;
			}
			)"
		);

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		LOG(INFO) << "Converting IR to C...";
		auto converted = be->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(containsSubString, code, "v0 = {{1, 2, 3, 4, 5}}");
		EXPECT_PRED2(containsSubString, code, "v1 = {{1, 2}}");
		EXPECT_PRED2(containsSubString, code, "v2 = {{0}}");
		EXPECT_PRED2(containsSubString, code, "v3 = {{0, 1, 2}}");
		EXPECT_PRED2(containsSubString, code, "v4 = {{{{1, 2, 3}}, {{4, 5, 6}}}}");
		EXPECT_PRED2(containsSubString, code, "v5 = {{{{1}}, {{4, 5}}}}");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));

	}

	TEST(Initialization, Struct) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
			R"(
			def struct A { a : int<4>; b : real<4>; };
			def struct B { a : int<4>; b : real<4>; c : uint<4>; };
			int<4> main() {
				var ref<A> v0 = *<ref<A>>(v0){ 1, 1.0f };
				//{ struct { int a; float b; } sif = { 1, 1.0f }; }

				var ref<B> v1 = *<ref<B>>(v1){ 1, 1.0f, 2u };
				//{ struct { int a; float b; unsigned c; } sifc = { .a = 1, .c = 2u }; }

				return 0;
			}
			)"
		);

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		LOG(INFO) << "Converting IR to C...";
		auto converted = be->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(containsSubString, code, "A v0 = {1, 1.0f}");
		EXPECT_PRED2(containsSubString, code, "B v1 = {1, 1.0f, 2u}");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(Initialization, StructTemp) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
			R"(
			def struct A { a : int<4>; b : real<4>; };

			def f = (a : A) ->unit { };

			int<4> main() {
				f(*<ref<A>>(ref_temp(type_lit(A))){ 1, 1.0f });

				var ref<A> v0;
				v0 = *<ref<A>>(ref_temp(type_lit(A))){ 1, 1.0f };
				return 0;
			}
			)"
		);

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		LOG(INFO) << "Converting IR to C...";
		auto converted = be->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(Initialization, Globals) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
			def struct S { x: int<4>; y: uint<4>; };
			int<4> main() {
				<ref<int<4>,f,f,plain>>(lit("initedGlobal" : ref<int<4>,f,f,plain>)) {5};
				<ref<S,f,f,plain>>(lit("y" : ref<S,f,f,plain>)) {1, 5u};
				<ref<array<S,3>,f,f,plain>>(lit("klaus_test" : ref<array<S,3>,t,f,plain>)) {*<ref<S,f,f,plain>>(ref_temp(type_lit(S))) {1, 2u}, *<ref<S,f,f,plain>>(ref_temp(type_lit(S))) {3, 4u}, *<ref<S,f,f,plain>>(ref_temp(type_lit(S))) {5, 6u}};
				<ref<array<char,255>,f,f,plain>>(lit("char_arr" : ref<array<char,255>,f,f,plain>)) {'\0'};
				<ref<array<int<4>,2>,f,f,plain>>(lit("arr" : ref<array<int<4>,2>,f,f,plain>)) {42, 43};

				*lit("initedGlobal":ref<int<4>>);
				*lit("y":ref<S>);
				ptr_from_array(lit("klaus_test":ref<array<S,3>,t,f>));
				ptr_from_array(lit("char_arr":ref<array<char,255>,f,f>));
				ptr_from_array(lit("arr":ref<array<int<4>,2>,f,f>));
				return 0;
			}
		)");

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		auto converted = be->convert(program);
		//std::cout << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(containsSubString, code, "{{1, 2u}, {3, 4u}, {5, 6u}}");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(Enum, Simple) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
			R"(using "ext.enum";
			int<4> main() {
				var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = (type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0);
				return 0;
			}
			)"
		);

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		LOG(INFO) << "Converting IR to C...";
		auto converted = be->convert(program);
		LOG(INFO) << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(containsSubString, code, "e v0 = eA;");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(Pointer, Simple) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
			def fun = (i : ptr<int<4>>) -> unit {};
			def pfun = () -> ptr<int<4>> {
				var ref<int<4>,f,f> v0;
				return ptr_from_ref(v0);
			};
			def pfun2 = (a : ptr<int<4>>) -> int<4> {
				return *ptr_to_ref(a);
			};

			def struct A {
				m1 : ptr<int<4>>;
			};
			def pAfun = (p : A) -> int<4> {
				return *ptr_to_ref(p.m1);
			};

			int<4> main() {
				var ref<int<4>,f,f> v0;
				var ref<ptr<int<4>,f,f>,f,f> v1 = ptr_from_ref(v0);
				fun(ptr_from_ref(v0));
				fun(*v1);
				pfun();
				pfun2(*v1);
				pfun2(pfun());

				var ref<int<4>,f,f> s = *ptr_to_ref(pfun()) + *ptr_to_ref(pfun());

				var ref<A> a;
				pAfun(*a);
				return 0;
			}
		)");

		ASSERT_TRUE(program);

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		//std::cout << "Converting IR to C...";
		auto converted = be->convert(program);
		//std::cout << "Printing converted code: " << *converted;

		string code = toString(*converted);

		EXPECT_PRED2(containsSubString, code, "*pfun() + *pfun()");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(VariableDeclTypes, InitExpr) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
		def union u {
			i : int<4>;
		};
		int<4> function main (v69 : ref<int<4>,f,f,plain>, v70 : ref<ptr<ptr<char>>,f,f,plain>){
			var ref<u,f,f,plain> v71 = <ref<u,f,f,plain>>(ref_decl(type_lit(ref<u,f,f,plain>))) {1};
			return 0;
		})");

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		//std::cout << "Converting IR to C...";
		auto converted = be->convert(program);
		//std::cout << "Printing converted code: " << *converted;

		string code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "u v71 = {1};");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(Pointers, Simple) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"string(
		decl IMP___assert_fail : (ptr<char,t,f>, ptr<char,t,f>, uint<4>, ptr<char,t,f>) -> unit;
		int<4> function IMP_main (v9 : ref<int<4>,f,f,plain>, v10 : ref<ptr<ptr<char>>,f,f,plain>){
			var ref<ptr<int<4>>,f,f,plain> v11 = ptr_null(type_lit(int<4>), type_lit(f), type_lit(f));
			var ref<ptr<int<4>>,f,f,plain> v15 = *v11;
			ptr_eq(*v11, *v15) && ptr_ne(ptr_from_array("This is a semantic ERROR!"), ptr_null(type_lit(char), type_lit(f), type_lit(f)))?unit_consume(0):
				IMP___assert_fail(ptr_cast(ptr_from_array("a == b && \"This is a semantic ERROR!\""), type_lit(t), type_lit(f)), ptr_cast(ptr_from_array("s"), type_lit(t), type_lit(f)), num_cast(9, type_lit(uint<4>)), ptr_from_array("int main(int, char **)"));
			return 0;
		})string");

		// check for semantic errors
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// create backend instance
		auto be = sequential::SequentialBackend::getDefault();

		//std::cout << "Converting IR to C...";
		auto converted = be->convert(program);
		//std::cout << "Printing converted code: " << *converted;

		string code = toString(*converted);
		EXPECT_PRED2(notContainsSubString, code, "ptr_");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-lm");
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

} // namespace backend
} // namespace insieme
