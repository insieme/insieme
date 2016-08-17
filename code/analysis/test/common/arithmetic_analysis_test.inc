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

#include "insieme/analysis/cba_interface.h"

#include "insieme/analysis/common/failure.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {

	using namespace core;
	using testing::Types;
	using SymbolTable = std::map<std::string,core::NodePtr>;

	IRBuilder& getBuilder() {
		static NodeManager mgr;
		static IRBuilder builder(mgr);
		return builder;
	}

	/**
	 * GTest-specific class to enable parametrized tests.
	 * The type-parametrized constructor fetches a function pointer to the
	 * analysis from the appropriate CBA backend to be used in the tests below.
	 */
	template <typename Backend>
	class ArithmeticValue : public testing::Test {

	protected:

		ArithmeticSet getValue(const core::ExpressionAddress& expr) {
			return insieme::analysis::getArithmeticValue<Backend>(expr);
		}

		ArithmeticSet getValue(const core::ExpressionPtr& expr) {
			return getValue(core::ExpressionAddress(expr));
		}

		ArithmeticSet getValue(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
			auto expr = getBuilder().parseExpr(code, symbols);
			return getValue(ExpressionAddress(expr));
		}

		bool isConstant(int c, const std::string& code, const SymbolTable& symbols = SymbolTable()) {
			auto values = getValue(code,symbols);

			EXPECT_FALSE(values.isUniversal());
			if (values.isUniversal()) return false;

			EXPECT_EQ(1, values.size()) << values;
			if (values.size() != 1) return false;

			auto& value = *values.begin();
			EXPECT_TRUE(value.isConstant()) << value;
			if (!value.isConstant()) return false;

			EXPECT_EQ(c, value.getIntegerValue());
			return c == value.getIntegerValue();
		}

		bool isUnknown(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
			auto values = getValue(code,symbols);
			return values.isUniversal();
		}

		bool isTrue(const core::ExpressionAddress& expr) {
			return insieme::analysis::isTrue<Backend>(expr);
		}

		bool isTrue(const string& ir) {
			NodeManager mgr;
			IRBuilder builder(mgr);
			auto in = builder.parseExpr(ir);
			return isTrue(ExpressionAddress(in));
		}

		bool isBoth(const core::ExpressionAddress& expr) {
			return insieme::analysis::mayBeTrue<Backend>(expr) && insieme::analysis::mayBeFalse<Backend>(expr);
		}

		bool isBoth(const string& ir) {
			NodeManager mgr;
			IRBuilder builder(mgr);
			auto in = builder.parseExpr(ir);
			return isBoth(ExpressionAddress(in));
		}

		bool isFalse(const core::ExpressionAddress& expr) {
			return insieme::analysis::isFalse<Backend>(expr);
		}

		bool isFalse(const string& ir) {
			NodeManager mgr;
			IRBuilder builder(mgr);
			auto in = builder.parseExpr(ir);
			return isFalse(ExpressionAddress(in));
		}

	};

	TYPED_TEST_CASE_P(ArithmeticValue);

	TYPED_TEST_P(ArithmeticValue, Constants) {

		// test some simple constants
		EXPECT_TRUE(this->isConstant(0,"0"));
		EXPECT_TRUE(this->isConstant(1,"1"));
		EXPECT_TRUE(this->isConstant(2,"2"));

		// some more constants
		EXPECT_TRUE(this->isConstant(-1,"-1"));

		// some other formats
		EXPECT_EQ("{15}", toString(this->getValue(getBuilder().literal("0xF", getBuilder().parseType("int<4>")))));
		EXPECT_EQ("{10}", toString(this->getValue(getBuilder().literal("012", getBuilder().parseType("int<4>")))));

		// some non-arithmetic constants
		EXPECT_TRUE(this->isUnknown("\"x\""));

	}

	TYPED_TEST_P(ArithmeticValue, FreeVariables) {

		IRBuilder& builder = getBuilder();

		// free variables should have an unknown value
		EXPECT_EQ("{v1}", toString(this->getValue(ExpressionAddress(builder.variable(builder.parseType("int<4>"), 1)))));

	}

	TYPED_TEST_P(ArithmeticValue, SideEffectFreeExpressions) {

		IRBuilder& builder = getBuilder();
		std::map<std::string,NodePtr> symbols;
		symbols["x"] = builder.literal("x",builder.parseType("int<4>"));
		symbols["r"] = builder.literal("r",builder.parseType("ref<int<4>>"));

		// free variables should have an unknown value
		EXPECT_EQ("{x}",   toString(this->getValue(builder.parseExpr("x", symbols))));
		EXPECT_EQ("{x+2}", toString(this->getValue(builder.parseExpr("x+2", symbols))));

		EXPECT_EQ("{*r}",     toString(this->getValue(builder.parseExpr("*r", symbols))));
		EXPECT_EQ("{*r+2}",   toString(this->getValue(builder.parseExpr("*r+2", symbols))));
		EXPECT_EQ("{x+*r+2}", toString(this->getValue(builder.parseExpr("*r+2+x", symbols))));

	}

	TYPED_TEST_P(ArithmeticValue, Arithmetic_101) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<string, NodePtr> symbols;
		symbols["e"] = builder.literal("e", builder.getLangBasic().getInt4());

		auto in = builder.parseStmt(
				"{"
				// constants
				"	0;"						// should be 0
				"	1;"						// should be 1
				"	e;"						// should be unknown

				// constant expressions
				"	1+1;"
				"	1+1+1;"
				"	1+2*3;"
				"	17+4;"

				// constant expressions with variables
				"	var int<4> x = 2;"
				"	var int<4> y = 3;"
				"	x;"
				"	2*x+1;"
				"	2*x+4*y;"
				"	2*x+4*y+e;"

				"	var ref<int<4>> z = 10;"			// L=13
				"	var ref<int<4>> w = 5;"				// L=14
				"	var ref<int<4>> a = z;"				// L=15
				"	var ref<ref<int<4>>> p = z;"		// L=16

				"	*z;"
				"	*p+2*z;"

				"	p = w;"
				"	*p+2*z+4*e*(*p)+a;"

				// boolean constraints
				"	1 < 2;"
				"	2*x+1 < 2;"
				"	2*x+1 > 2*x;"
				"	2*x+1 == (2+1)*x;"
				"	2*x < e;"

				// ternary operator
				"	(x<2)?1:2;"				// x is 2 => should be 2
				"	(x<3)?1:2;"				// x is 2 => should be 1
				"	(x<y)?1:2;"				// x is 2, y is 3 => should be 1
				"	(x<e)?1:2;"				// x is 2, e is unknown => should be {1,2}

				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);


		// check values
		EXPECT_EQ("{0}", toString(this->getValue(code[0].as<ExpressionAddress>())));
		EXPECT_EQ("{1}", toString(this->getValue(code[1].as<ExpressionAddress>())));
		EXPECT_EQ("{e}", toString(this->getValue(code[2].as<ExpressionAddress>())));

		EXPECT_EQ("{2}", toString(this->getValue(code[3].as<ExpressionAddress>())));
		EXPECT_EQ("{3}", toString(this->getValue(code[4].as<ExpressionAddress>())));
		EXPECT_EQ("{7}", toString(this->getValue(code[5].as<ExpressionAddress>())));
		EXPECT_EQ("{21}", toString(this->getValue(code[6].as<ExpressionAddress>())));

		EXPECT_EQ("{2}", toString(this->getValue(code[9].as<ExpressionAddress>())));
		EXPECT_EQ("{5}", toString(this->getValue(code[10].as<ExpressionAddress>())));
		EXPECT_EQ("{16}", toString(this->getValue(code[11].as<ExpressionAddress>())));
		EXPECT_EQ("{e+16}", toString(this->getValue(code[12].as<ExpressionAddress>())));

		EXPECT_EQ("{10}", toString(this->getValue(code[17].as<ExpressionAddress>())));
		EXPECT_EQ("{30}", toString(this->getValue(code[18].as<ExpressionAddress>())));

		EXPECT_EQ("{20*e+35}", toString(this->getValue(code[20].as<ExpressionAddress>())));

		// boolean constraints
		EXPECT_TRUE( this->isTrue(code[21].as<ExpressionAddress>()));
		EXPECT_TRUE(this->isFalse(code[22].as<ExpressionAddress>()));
		EXPECT_TRUE( this->isTrue(code[23].as<ExpressionAddress>()));
		EXPECT_TRUE(this->isFalse(code[24].as<ExpressionAddress>()));
		EXPECT_TRUE( this->isBoth(code[25].as<ExpressionAddress>()));

		// ternary operator
		EXPECT_EQ("{2}", toString(this->getValue(code[26].as<ExpressionAddress>())));
		EXPECT_EQ("{1}", toString(this->getValue(code[27].as<ExpressionAddress>())));
		EXPECT_EQ("{1}", toString(this->getValue(code[28].as<ExpressionAddress>())));
		EXPECT_EQ("{1,2}", toString(this->getValue(code[29].as<ExpressionAddress>())));

	}

	TYPED_TEST_P(ArithmeticValue, Arithmetic_102) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
			"{"
			"	var ref<int<4>> a = 3;"
			"	var ref<int<4>> b = 9 + (a * 5);"
			"	var ref<int<4>> c;"
			"	"
			"	c = b * 4;"
			"	if (c > 10) {"
			"		c = c - 10;"
			"	}"
			"	c * (60 + a);"
			"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		// check values
		EXPECT_EQ("{5418}", toString(this->getValue(code[5].as<ExpressionAddress>())));

		//createDotDump(analysis);
	}

		TYPED_TEST_P(ArithmeticValue, Arithmetic_103) {

			NodeManager mgr;
			IRBuilder builder(mgr);

			auto in = builder.parseStmt(
				"{"
				"	auto f = lit(\"xyz\": () -> int<4>);"
				"	f();"
				"}"
			).as<CompoundStmtPtr>();

			ASSERT_TRUE(in);
			CompoundStmtAddress code(in);

			// check values
			EXPECT_EQ("{-all-}", toString(this->getValue(code[1].as<ExpressionAddress>())));

			//createDotDump(analysis);
		}

		TYPED_TEST_P(ArithmeticValue, Arithmetic_Cast) {
			NodeManager mgr;
			IRBuilder builder(mgr);
			auto& base = builder.getLangBasic();

			ExpressionAddress expr(builder.castExpr(base.getInt4(), builder.intLit(12)));

			EXPECT_EQ("cast<int<4>>(12)", toString(*expr));

			EXPECT_EQ("{12}", toString(this->getValue(expr)));
		}



	TYPED_TEST_P(ArithmeticValue, LocalVariableDecl) {

		IRBuilder& builder = getBuilder();
		auto res = builder.parseAddressesStatement("{ auto x = 12; $x$; }")[0];
		EXPECT_EQ("{12}", toString(this->getValue(res.as<ExpressionAddress>())));

	}

	TYPED_TEST_P(ArithmeticValue, ReturnValue) {

		// test whether the return value of a function is deduced properly
		EXPECT_EQ("{5}", toString(this->getValue("()->int<4> { return 5; }()")));

	}

	TYPED_TEST_P(ArithmeticValue, LocalVariable) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( this->isTrue("()->bool { var bool x = true; return x; }()"));
		EXPECT_TRUE(this->isFalse("()->bool { var bool x = false; return x; }()"));

	}

	TYPED_TEST_P(ArithmeticValue, ParameterPassing) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( this->isTrue("(x : 'a)->'a { return x; }(true)"));
		EXPECT_TRUE(this->isFalse("(x : 'a)->'a { return x; }(false)"));

		// check order of arguments
		EXPECT_TRUE( this->isTrue("( a:'a, b:'a )->'a { return a; }(true,false)"));
		EXPECT_TRUE(this->isFalse("( a:'a, b:'a )->'a { return b; }(true,false)"));

	}

	TYPED_TEST_P(ArithmeticValue, ParameterPassingToBind) {

		// test whether values can be passed to bind expressions
		EXPECT_TRUE( this->isTrue("( (x : 'a)=> x )(true)"));
		EXPECT_TRUE(this->isFalse("( (x : 'a)=> x )(false)"));

	}

	TYPED_TEST_P(ArithmeticValue, ControlFlow) {

		// test whether control flow restrictions are considered
		EXPECT_TRUE( this->isTrue("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(true,true)"));
		EXPECT_TRUE(this->isFalse("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(true,false)"));
		EXPECT_TRUE(this->isFalse("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(false,true)"));
		EXPECT_TRUE(this->isFalse("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(false,false)"));

		EXPECT_TRUE( this->isTrue("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(true,true)"));
		EXPECT_TRUE( this->isTrue("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(true,false)"));
		EXPECT_TRUE( this->isTrue("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(false,true)"));
		EXPECT_TRUE(this->isFalse("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(false,false)"));

		EXPECT_TRUE(this->isFalse("!true"));
		EXPECT_TRUE( this->isTrue("!false"));

	}

	TYPED_TEST_P(ArithmeticValue, HigherOrderFunction) {

		EXPECT_TRUE(this->isTrue("( a: ('a)-> 'a, b:'a )->'a { return a(b); }(id,true)"));

	}

	TYPED_TEST_P(ArithmeticValue, BoundValues) {

		// check a simple constant value in a bind
		EXPECT_TRUE( this->isTrue("(()=>true)()"));
		EXPECT_TRUE(this->isFalse("(()=>false)()"));

		// check whether higher functions work with closures
		EXPECT_TRUE( this->isTrue("(f:()=>'a)->'a{ return f(); }(()=>true)"));

		// check proper operation of lazy boolean connectors (based on closures)
		EXPECT_TRUE( this->isTrue("true && true"));
		EXPECT_TRUE(this->isFalse("true && false"));
		EXPECT_TRUE(this->isFalse("false && true"));
		EXPECT_TRUE(this->isFalse("false && false"));

		EXPECT_TRUE( this->isTrue("true || true"));
		EXPECT_TRUE( this->isTrue("true || false"));
		EXPECT_TRUE( this->isTrue("false || true"));
		EXPECT_TRUE(this->isFalse("false || false"));


		// check the capturing of a variable
		EXPECT_TRUE( this->isTrue("(x: 'a)->'a { return (f:()=>'a)->'a{ return f(); }(()=>x); }(true)"));
		EXPECT_TRUE(this->isFalse("(x: 'a)->'a { return (f:()=>'a)->'a{ return f(); }(()=>x); }(false)"));

		// check the parameter of a bind expression
		EXPECT_TRUE( this->isTrue("(x: bool)->bool { return (f:('a)=>'a)->'a{ return f(true); }((y:bool)=>(true)); }(true)"));
		EXPECT_TRUE( this->isTrue("(x: bool)->bool { return (f:('a)=>'a)->'a{ return f(true); }((y:bool)=>(x)); }(true)"));
		EXPECT_TRUE( this->isTrue("(x: bool)->bool { return (f:('a)=>'a)->'a{ return f(true); }((y:bool)=>(y)); }(true)"));
		EXPECT_TRUE( this->isTrue("(x: bool)->bool { return (f:('a)=>'a)->'a{ return f(true); }((y:bool)=>(x && y)); }(true)"));

	}

	TYPED_TEST_P(ArithmeticValue, MemoryState) {

		EXPECT_TRUE( this->isTrue("()->bool{ var ref<bool> a = ref_new(type_lit(bool)); a = true; return *a; }()"));
		EXPECT_TRUE(this->isFalse("()->bool{ var ref<bool> a = ref_new(type_lit(bool)); a = true; a = !a; return *a; }()"));

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

		EXPECT_TRUE(this->isTrue(code));

	}

	TYPED_TEST_P(ArithmeticValue, PassByReference) {

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

		EXPECT_TRUE(this->isTrue(code));

	}

	TYPED_TEST_P(ArithmeticValue, Tuples) {

		// some more complex example
		auto code = R"(
			()->int<4> {
				var ( int<4> , int<4> ) a = ( 1, 2 );
				return a.0;
			}()
		)";

		EXPECT_TRUE(this->isConstant(1, code));

		// try the other component
		code = R"(
			()->int<4> {
				var ( int<4> , int<4> ) a = ( 1, 2 );
				return a.1;
			}()
		)";

		EXPECT_TRUE(this->isConstant(2, code));

	}

	REGISTER_TYPED_TEST_CASE_P(ArithmeticValue,
			Constants,
			FreeVariables,
			SideEffectFreeExpressions,
			Arithmetic_101,
			Arithmetic_102,
			Arithmetic_103,
			Arithmetic_Cast,
			LocalVariableDecl,
			ReturnValue,
			LocalVariable,
			ParameterPassing,
			ParameterPassingToBind,
			ControlFlow,
			HigherOrderFunction,
			BoundValues,
			MemoryState,
			PassByReference,
			Tuples
	);

} // end namespace analysis
} // end namespace insieme

