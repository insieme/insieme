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

#include "insieme/analysis/datalog/boolean_analysis.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace datalog {

	using namespace core;

	using SymbolTable = std::map<std::string,core::NodePtr>;

	IRBuilder& getBuilder() {
		static NodeManager mgr;
		static IRBuilder builder(mgr);
		return builder;
	}

	bool isTrue(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
		auto expr = getBuilder().parseExpr(code, symbols);
//		std::cout << *expr << "\n";
//		std::cout << dumpText(expr) << "\n";
		return isTrue(ExpressionAddress(expr));
	}

	bool isFalse(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
		return isFalse(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
	}

	bool mayBeTrue(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
		return mayBeTrue(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
	}

	bool mayBeFalse(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
		return mayBeFalse(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
	}


	TEST(BooleanValue, Constants) {

		EXPECT_TRUE(isTrue("true"));
		EXPECT_TRUE(isFalse("false"));
		EXPECT_TRUE(isTrue("true"));
		EXPECT_TRUE(isFalse("false"));

		EXPECT_TRUE(mayBeTrue("true"));
		EXPECT_TRUE(mayBeFalse("false"));

		EXPECT_FALSE(isTrue("false"));
		EXPECT_FALSE(isFalse("true"));

		EXPECT_FALSE(mayBeTrue("false"));
		EXPECT_FALSE(mayBeFalse("true"));
	}


	TEST(BooleanValue, StringConstants) {

		// check string constants (should be neither true nor false)
		EXPECT_FALSE(isTrue("\"x\""));
		EXPECT_FALSE(isFalse("\"x\""));
		EXPECT_TRUE(mayBeTrue("\"x\""));
		EXPECT_TRUE(mayBeFalse("\"x\""));

	}

	TEST(BooleanValue, FreeVariables) {

		IRBuilder& builder = getBuilder();
		SymbolTable symbols;
		symbols["x"] = builder.variable(builder.parseType("bool"));

		// check that the free variable "x" is neither definitely true nor false
		EXPECT_FALSE(isTrue("x",symbols));
		EXPECT_FALSE(isFalse("x",symbols));
		EXPECT_TRUE(mayBeTrue("x",symbols));
		EXPECT_TRUE(mayBeFalse("x",symbols));

	}

	TEST(BooleanValue, ReturnValue) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( isTrue("()->bool { return true; }()"));
		EXPECT_TRUE(isFalse("()->bool { return false; }()"));

	}

	TEST(BooleanValue, LocalVariable) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( isTrue("()->bool { var bool x = true; return x; }()"));
		EXPECT_TRUE(isFalse("()->bool { var bool x = false; return x; }()"));

	}

	TEST(BooleanValue, ParameterPassing) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( isTrue("(x : 'a)->'a { return x; }(true)"));
		EXPECT_TRUE(isFalse("(x : 'a)->'a { return x; }(false)"));

		// check order of arguments
		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { return a; }(true,false)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { return b; }(true,false)"));

	}

	TEST(BooleanValue, ParameterPassingToBind) {

		// test whether values can be passed to bind expressions
		EXPECT_TRUE( isTrue("( (x : 'a)=> x )(true)"));
		EXPECT_TRUE(isFalse("( (x : 'a)=> x )(false)"));

	}

	TEST(BooleanValue, ControlFlow) {

		// test whether control flow restrictions are considered
		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(true,true)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(true,false)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(false,true)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(false,false)"));

		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(true,true)"));
		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(true,false)"));
		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(false,true)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(false,false)"));

		EXPECT_TRUE(isFalse("!true"));
		EXPECT_TRUE( isTrue("!false"));

	}

	TEST(BooleanValue, HigherOrderFunction) {

		EXPECT_TRUE( isTrue("( a: ('a)-> 'a, b:'a )->'a { return a(b); }(id,true)"));

	}


	TEST(BooleanValue, BoundValues) {

		// check a simple constant value in a bind
		EXPECT_TRUE( isTrue("(()=>true)()"));
		EXPECT_TRUE(isFalse("(()=>false)()"));

		// check whether higher functions work with closures
		EXPECT_TRUE( isTrue("(f:()=>'a)->'a{ return f(); }(()=>true)"));

		// check proper operation of lazy boolean connectors (based on closures)
		EXPECT_TRUE( isTrue("true && true"));
		EXPECT_TRUE(isFalse("true && false"));
		EXPECT_TRUE(isFalse("false && true"));
		EXPECT_TRUE(isFalse("false && false"));

		EXPECT_TRUE( isTrue("true || true"));
		EXPECT_TRUE( isTrue("true || false"));
		EXPECT_TRUE( isTrue("false || true"));
		EXPECT_TRUE(isFalse("false || false"));


		// check the capturing of a variable
		EXPECT_TRUE( isTrue("(x: 'a)->'a { return (f:()=>'a)->'a{ return f(); }(()=>x); }(true)"));
		EXPECT_TRUE(isFalse("(x: 'a)->'a { return (f:()=>'a)->'a{ return f(); }(()=>x); }(false)"));

		// check the parameter of a bind expression
		EXPECT_TRUE( isTrue("(x: bool)->bool { return (f:('a)=>'a)->'a{ return f(true); }((y:bool)=>(true)); }(true)"));
		EXPECT_TRUE( isTrue("(x: bool)->bool { return (f:('a)=>'a)->'a{ return f(true); }((y:bool)=>(x)); }(true)"));
		EXPECT_TRUE( isTrue("(x: bool)->bool { return (f:('a)=>'a)->'a{ return f(true); }((y:bool)=>(y)); }(true)"));
		EXPECT_TRUE( isTrue("(x: bool)->bool { return (f:('a)=>'a)->'a{ return f(true); }((y:bool)=>(x && y)); }(true)"));

	}

	TEST(BooleanValue, MemoryState) {

		EXPECT_TRUE( isTrue("()->bool{ var ref<bool> a = ref_new(type_lit(bool)); a = true; return *a; }()"));
		EXPECT_TRUE(isFalse("()->bool{ var ref<bool> a = ref_new(type_lit(bool)); a = true; a = !a; return *a; }()"));


		// some more complex example
		auto code = R"(
			let negate = ( a : ref<bool> )->unit { a = !a; } in
			()->bool {
				var ref<bool> a = ref_new(type_lit(bool));
				a = false;
				negate(a);
				negate(a);
				negate(a);
				return *a;
			}()
		)";

		EXPECT_TRUE(isTrue(code));

	}

	TEST(BooleanValue, PassByReference) {

		// some more complex example
		auto code = R"(
			let forward = ( a : ref<bool> )->bool { return *a; } in
			()->bool {
				var ref<bool> a = ref_new(type_lit(bool));
				a = true;
				a = forward(a);
				return *a;
			}()
		)";

		EXPECT_TRUE(isTrue(code));

	}

	TEST(BooleanValue, Tuples) {

		// some more complex example
		auto code = R"(
			()->bool {
				var ( bool , bool ) a = ( true, false );
				return a.0;
			}()
		)";

		EXPECT_TRUE(isTrue(code));

		// try the other component
		code = R"(
			()->bool {
				var ( bool , bool ) a = ( true, false );
				return a.1;
			}()
		)";

		EXPECT_TRUE(isFalse(code));

	}

	TEST(BooleanValue, FailureDetection) {

		auto& builder = getBuilder();

		auto input = R"(()->bool{ var ref<bool> a = ref_new(type_lit(bool)); a = true; return *a; }())";
		EXPECT_TRUE(isTrue(input));

		input = "ref_deref";
		EXPECT_TRUE(isTrue(input));

		std::ofstream outputFile("/tmp/insieme_ir_text_dump.txt");
		if (outputFile.is_open()) {
			dumpText(builder.parse(input), outputFile, true);
			outputFile.close();
		}

		input = "(x : 'a)-> 'a{ return x; }(ref_deref)";
		EXPECT_TRUE(isTrue(input));

		EXPECT_TRUE( isTrue("()->bool{ var ref<bool> a = ref_new(type_lit(bool)); a = true; return *a; }()"));


	}

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme

