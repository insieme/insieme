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
	class BooleanValue : public testing::Test {

	protected:

		bool (*impl_isTrue)(const core::ExpressionAddress&);
		bool (*impl_isFalse)(const core::ExpressionAddress&);
		bool (*impl_mayBeTrue)(const core::ExpressionAddress&);
		bool (*impl_mayBeFalse)(const core::ExpressionAddress&);

		BooleanValue()
			: impl_isTrue(&(analysis<Backend, isTrueAnalysis>())),
			  impl_isFalse(&(analysis<Backend, isFalseAnalysis>())),
			  impl_mayBeTrue(&(analysis<Backend, mayBeTrueAnalysis>())),
			  impl_mayBeFalse(&(analysis<Backend, mayBeFalseAnalysis>())) {}

		bool isTrue(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
			auto expr = getBuilder().parseExpr(code, symbols);
			return impl_isTrue(ExpressionAddress(expr));
		}

		bool isFalse(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
			return impl_isFalse(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
		}

		bool mayBeTrue(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
			return impl_mayBeTrue(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
		}

		bool mayBeFalse(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
			return impl_mayBeFalse(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
		}

	};

	/**
	 * Type list of backends to be tested
	 */
	using CBA_Backends = Types<datalogEngine, haskellEngine>;

	/**
	 * Tell GTest that CBA_Interface tests shall be type-parametrized with CBA_Backends
	 */
	TYPED_TEST_CASE(BooleanValue, CBA_Backends);

	TYPED_TEST(BooleanValue, Constants) {

		EXPECT_TRUE(this->isTrue("true"));
		EXPECT_TRUE(this->isFalse("false"));
		EXPECT_TRUE(this->isTrue("true"));
		EXPECT_TRUE(this->isFalse("false"));

		EXPECT_TRUE(this->mayBeTrue("true"));
		EXPECT_TRUE(this->mayBeFalse("false"));

		EXPECT_FALSE(this->isTrue("false"));
		EXPECT_FALSE(this->isFalse("true"));

		EXPECT_FALSE(this->mayBeTrue("false"));
		EXPECT_FALSE(this->mayBeFalse("true"));

	}

	TYPED_TEST(BooleanValue, StringConstants) {

		// check string constants (should be neither true nor false)
		EXPECT_FALSE(this->isTrue("\"x\""));
		EXPECT_FALSE(this->isFalse("\"x\""));
		EXPECT_TRUE(this->mayBeTrue("\"x\""));
		EXPECT_TRUE(this->mayBeFalse("\"x\""));

	}

	TYPED_TEST(BooleanValue, FreeVariables) {

		IRBuilder& builder = getBuilder();
		SymbolTable symbols;
		symbols["x"] = builder.variable(builder.parseType("bool"));

		// check that the free variable "x" is neither definitely true nor false
		EXPECT_FALSE(this->isTrue("x",symbols));
		EXPECT_FALSE(this->isFalse("x",symbols));
		EXPECT_TRUE(this->mayBeTrue("x",symbols));
		EXPECT_TRUE(this->mayBeFalse("x",symbols));

	}

	TYPED_TEST(BooleanValue, LocalVariableDecl) {

		IRBuilder& builder = getBuilder();
		auto res = builder.parseAddressesStatement("{ auto x = true; $x$; }")[0];
		EXPECT_TRUE(this->impl_isTrue(res.as<VariableAddress>()));

	}

	TYPED_TEST(BooleanValue, ReturnValue) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( this->isTrue("()->bool { return true; }()"));
		EXPECT_TRUE(this->isFalse("()->bool { return false; }()"));

	}

	TYPED_TEST(BooleanValue, LocalVariable) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( this->isTrue("()->bool { var bool x = true; return x; }()"));
		EXPECT_TRUE(this->isFalse("()->bool { var bool x = false; return x; }()"));

	}

	TYPED_TEST(BooleanValue, ParameterPassing) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( this->isTrue("(x : 'a)->'a { return x; }(true)"));
		EXPECT_TRUE(this->isFalse("(x : 'a)->'a { return x; }(false)"));

		// check order of arguments
		EXPECT_TRUE( this->isTrue("( a:'a, b:'a )->'a { return a; }(true,false)"));
		EXPECT_TRUE(this->isFalse("( a:'a, b:'a )->'a { return b; }(true,false)"));

	}

	TYPED_TEST(BooleanValue, ParameterPassingToBind) {

		// test whether values can be passed to bind expressions
		EXPECT_TRUE( this->isTrue("( (x : 'a)=> x )(true)"));
		EXPECT_TRUE(this->isFalse("( (x : 'a)=> x )(false)"));

	}

	TYPED_TEST(BooleanValue, ControlFlow) {

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

	TYPED_TEST(BooleanValue, HigherOrderFunction) {

		EXPECT_TRUE(this->isTrue("( a: ('a)-> 'a, b:'a )->'a { return a(b); }(id,true)"));

	}

	TYPED_TEST(BooleanValue, BoundValues) {

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

	TYPED_TEST(BooleanValue, MemoryState) {

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

	TYPED_TEST(BooleanValue, PassByReference) {

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

	TYPED_TEST(BooleanValue, Tuples) {

		// some more complex example
		auto code = R"(
			()->bool {
				var ( bool , bool ) a = ( true, false );
				return a.0;
			}()
		)";

		EXPECT_TRUE(this->isTrue(code));

		// try the other component
		code = R"(
			()->bool {
				var ( bool , bool ) a = ( true, false );
				return a.1;
			}()
		)";

		EXPECT_TRUE(this->isFalse(code));

	}

	TYPED_TEST(BooleanValue, FailureDetection) {

		{
			auto input = "ref_assign";
			ASSERT_THROW(this->isFalse(input), AnalysisFailure);

			try {
				this->isFalse(input);
			} catch (const AnalysisFailure& af) {
				// TODO: better checking of error string
				//EXPECT_STREQ("Encountered 1 failures during analysis:\n\tError: Found a ref_assign whose parent is not a CallExpr!", af.what());
			}
		}

		{
			auto input = "(x : 'a)-> 'a{ return x; }(ref_assign)";
			ASSERT_THROW(this->isFalse(input), AnalysisFailure);
		}

	}

} // end namespace analysis
} // end namespace insieme

