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
#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace backend {


	TEST(FunctionCall, SimpleFunctions) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(
				R"(
				alias int = int<4>;
				
				def f : (a : int, b : int)->int {
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
					var ref<int<4>,f,f,plain> v = ref_var_init(3);
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
		                                                "	(dtype : type<'a>, size : type<'s>)->ref<array<'a,'s>> {"
		                                                "		return ref_new_init(array_create(size, list_empty(dtype)));"
		                                                "	} (type_lit(real<4>), type_lit(7));"
		                                                "	return 0;"
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
		symbols["v1"] = builder.parseExpr("array_create(type_lit(4), [1,2,3,4])");
		symbols["v2"] = builder.parseExpr("array_create(type_lit(4), [5,6,7,8])");

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
		core::ExpressionPtr offset = builder.parseExpr("ref_var_init(array_create(type_lit(3),[0ul,0ul,0ul]))");
		core::ExpressionPtr extFun = builder.parseExpr("lit(\"call_vector\" : (ref<array<uint<8>,3>>)->unit )");
		core::ExpressionPtr call = builder.callExpr(manager.getLangBasic().getUnit(), extFun, toVector(offset));

		auto converted = sequential::SequentialBackend::getDefault()->convert(call);
		string code = toString(*converted);

		EXPECT_PRED2(containsSubString, code, "call_vector((uint64_t(*)[3])(&(__insieme_type_1){{(uint64_t)0, (uint64_t)0, (uint64_t)0}}))");
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

			def differentbla : (x : 'b) -> unit {
				auto m = x;
				auto l = m;
			};

			def bla : (f : 'a) -> unit {
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
				var ref<array<int,10>> a = ref_var_init(array_create(type_lit(10), list_empty(type_lit(int))));
				var ref<array<int,10>> b = ref_new_init(array_create(type_lit(10), list_empty(type_lit(int))));

//				// create two variable-sized arrays on the stack and the heap
//				var ref<array<int,size>> c = ref_var(type_lit(array<int,size>));
//				var ref<array<int,size>> d = ref_new(type_lit(array<int,size>));

				// create two unknown-sized arrays on the stack and the heap
				var ref<array<int,inf>> e = ref_null(type_lit(array<int,inf>),type_lit(f),type_lit(f));
				var ref<array<int,inf>> f = ref_null(type_lit(array<int,inf>),type_lit(f),type_lit(f));

				// use the arrays
				a[2] = 123;
				b[4] = 321;
//				c[6] = 123;
//				d[8] = 321;
				e[7] = 123;
				f[5] = 321;

				return 0;
			}
			)");

		ASSERT_TRUE(program);

		auto converted = sequential::SequentialBackend::getDefault()->convert(program);

		string code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "__insieme_type_1 a = {};");
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
				def f : (a : ref<'a>, c : ('a)=>bool, v : ()=>'a)->ref<'a> {
					if(c(*a)) {
						a = v();
					}
					return a;
				};
				int<4> main() {
				
				var ref<int<4>> a = ref_var_init(1);
				var ref<real<4>> b = ref_var_init(2.0f);
				
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

		core::ProgramPtr program = builder.parseProgram("def f : ()->int<4> { return 4; };"
		                                                "def g : (a : ()=>'a)->'a { return a(); };"
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

	TEST(FunctionCall, DebugCodePrinting) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram("alias int = int<4>;"
		                                                ""
		                                                "def f : ()->int { return 4; };"
		                                                ""
		                                                "def g : (x : int)->int { return x + 2; };"
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
			EXPECT_PRED2(notContainsSubString, code, "{\n    _ : function() -> int<4> {\n        return g(f());\n    };\n}");

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

			// upbdate backend configuration
			be->getConfiguration().addIRCodeAsComment = true;

			LOG(INFO) << "Converting IR to C...";
			auto converted = be->convert(program);
			LOG(INFO) << "Printing converted code: " << *converted;

			string code = toString(*converted);

			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
			EXPECT_PRED2(containsSubString, code, "{main : () -> int<4> {\n        return g(f());\n    };\n}");

			// try compiling the code fragment
			utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
			compiler.addFlag("-lm");
			compiler.addFlag("-c"); // do not run the linker
			EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
		}
	}

	TEST(FunctionCall, RecursiveTypesSimple) {
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

	TEST(FunctionCall, RecursiveTypesMutual) {
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


} // namespace backend
} // namespace insieme
