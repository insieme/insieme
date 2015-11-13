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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/normalize.h"
#include "insieme/core/transform/manipulation.h"

namespace insieme {
namespace core {
namespace analysis {

	TEST(Normalizing, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto& basic = manager.getLangBasic();

		// create two variables not being normalized
		VariablePtr a = builder.variable(basic.getInt4(), 2);
		VariablePtr b = builder.variable(basic.getBool(), 3);

		// test some simple stuff
		EXPECT_EQ("AP(int<4>)", toString(normalize(basic.getInt4())));

		// -- free variables --

		// free variables should not be effected at all
		EXPECT_EQ(a, normalize(a));
		EXPECT_EQ(b, normalize(b));

		// test a compound
		EXPECT_EQ("AP({v2; v3;})", toString(normalize(builder.compoundStmt(a, b))));

		// test a nested compound
		EXPECT_EQ("AP({v2; {v3;};})", toString(normalize(builder.compoundStmt(a, builder.compoundStmt(b)))));


		// -- bound variables --

		EXPECT_EQ("AP(rec v0.{v0=fun(ref<int<4>,f,f,plain> v1) {ref_deref(v1);}}(v2))", toString(normalize(transform::outline(manager, StatementPtr(a)))));
		EXPECT_EQ("AP(rec v0.{v0=fun(ref<bool,f,f,plain> v1) {ref_deref(v1);}}(v3))", toString(normalize(transform::outline(manager, StatementPtr(b)))));

		EXPECT_EQ("AP(rec v0.{v0=fun(ref<int<4>,f,f,plain> v1, ref<bool,f,f,plain> v2) {ref_deref(v1); ref_deref(v2);}}(v2, v3))",
		          toString(normalize(transform::outline(manager, StatementPtr(builder.compoundStmt(a, b))))));
		EXPECT_EQ("AP(rec v0.{v0=fun(ref<int<4>,f,f,plain> v1, ref<bool,f,f,plain> v2) {ref_deref(v1); {ref_deref(v2);};}}(v2, v3))",
		          toString(normalize(transform::outline(manager, StatementPtr(builder.compoundStmt(a, builder.compoundStmt(b)))))));


		// test a function
		manager.setNextFreshID(5);
		NodePtr node = builder.parseStmt("{var int<4> a = 0; let f = (a : int<4>, b : int<4>)->int<4> { return a; } in f(a,a); }");
		EXPECT_EQ("AP({int<4> v0 = 0; rec v0.{v0=fun(ref<int<4>,f,f,plain> v1, ref<int<4>,f,f,plain> v2) {return ref_deref(v1);}}(v0, v0);})", toString(normalize(node)));
		EXPECT_EQ("AP({int<4> v0 = 0; rec v0.{v0=fun(ref<int<4>,f,f,plain> v1, ref<int<4>,f,f,plain> v2) {return ref_deref(v1);}}(v0, v0);})", toString(normalize(node)));


		// test normalization with existing free variables
		VariablePtr z = builder.variable(basic.getInt4(), 0); // create a v0!
		std::map<string, NodePtr> map;
		map["z"] = z;

		ExpressionPtr expr = builder.parseExpr("let f = ()->unit { z; } in f", map).as<ExpressionPtr>();

		ASSERT_TRUE(expr);

		EXPECT_EQ("AP(rec v1.{v1=fun() {v0;}})", toString(normalize(expr)));


		// test a sibling-compound
		EXPECT_EQ("{int<4> v0 = 1; {int<4> v1 = 2;}; {int<4> v2 = 3;};}",
		          toString(*normalize(builder.parseStmt("{var int<4> a = 1; { var int<4> b = 2; } { var int<4> c = 3; } }"))));

		EXPECT_EQ("{int<4> v0 = 1; {v0; int<4> v1 = 2;}; {int<4> v2 = 3; v0;};}",
		          toString(*normalize(builder.parseStmt("{var int<4> a = 1; { a; var int<4> b = 2; } { var int<4> c = 3; a; } }"))));

		EXPECT_EQ("{int<4> v0 = 1; int<4> v1 = 2;}", toString(*normalize(builder.parseStmt("{var int<4> a = 1; var int<4> b = 2; }"))));
	}

	TEST(Normalizing, MutualRecursion) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto code = builder.parseExpr("decl f : (int<4>)->int<4>;"
			                          "def f = (x : int<4>)->int<4> {"
			                          "	return (y : int<4>)->int<4> {"
			                          "		return f(y);"
			                          "	}(x);"
			                          "}; f(3)");

		EXPECT_EQ("rec v0.{v0=fun(ref<int<4>,f,f,plain> v1) {return rec v2.{v2=fun(ref<int<4>,f,f,plain> v3) {return v0(ref_deref(v3));}}(ref_deref(v1));}}(3)", toString(*normalize(code)));
	}

	TEST(Normalizing, VariablesInTypes) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto code = builder.parseExpr("def x = () -> unit {"
		                              "	var int<inf> v40 = 3;"
		                              "	var ref<array<int<4>,#v40>,f,f,plain> v50;"
		                              "}; x()");
		EXPECT_EQ("rec v0.{v0=fun() {int<inf> v1 = 3; ref<array<int<4>,v1>,f,f,plain> v2 = rec v0.{v0=fun(ref<type<'a>,f,f,plain> v1) {return ref_alloc(ref_deref(v1), mem_loc_stack);}}(type<array<int<4>,v1>>);}}()", toString(*normalize(code)));
	}


	TEST(Normalizing, TryCatch) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		VariablePtr var1 = builder.variable(builder.genericType("A"), 1);
		VariablePtr var2 = builder.variable(builder.genericType("A"), 2);

		CompoundStmtPtr body = builder.compoundStmt();
		CatchClausePtr clause1 = builder.catchClause(var1, builder.compoundStmt(var1));
		CatchClausePtr clause2 = builder.catchClause(var2, builder.compoundStmt(var2));

		NodePtr tryCatch = builder.tryCatchStmt(body, toVector(clause1));

		EXPECT_EQ("try {} catch (A v1) {v1;}", toString(*tryCatch));
		EXPECT_EQ("try {} catch (A v0) {v0;}", toString(*normalize(tryCatch)));

		tryCatch = builder.tryCatchStmt(body, toVector(clause1, clause2));

		EXPECT_EQ("try {} catch (A v1) {v1;} catch (A v2) {v2;}", toString(*tryCatch));
		EXPECT_EQ("try {} catch (A v0) {v0;} catch (A v1) {v1;}", toString(*normalize(tryCatch)));
	}

	TEST(Normalizing, Clone) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		FunctionTypePtr funType = builder.parseType("(A)->unit").as<FunctionTypePtr>();
		;
		VariablePtr var1 = builder.variable(builder.refType(builder.parseType("A")), 12);
		VariablePtr var2 = builder.variable(funType, 17);

		LambdaBindingPtr binding = builder.lambdaBinding(var2, builder.lambda(funType, toVector(var1), builder.compoundStmt()));
		LambdaExprPtr lambda = builder.lambdaExpr(var2, builder.lambdaDefinition(toVector(binding)));

		EXPECT_EQ("rec v17.{v17=fun(ref<A,f,f,plain> v12) {}}", toString(*lambda));
		EXPECT_EQ("rec v0.{v0=fun(ref<A,f,f,plain> v1) {}}", toString(*normalize(lambda)));
		EXPECT_EQ("rec v0.{v0=fun(ref<A,f,f,plain> v1) {}}", toString(*normalize(normalize(lambda))));


		// this was failing once - the normalized annotation was not properly transfered
		NodeManager mgr2;
		lambda = mgr2.get(lambda);
		EXPECT_EQ("rec v17.{v17=fun(ref<A,f,f,plain> v12) {}}", toString(*lambda));
		EXPECT_EQ("rec v0.{v0=fun(ref<A,f,f,plain> v1) {}}", toString(*normalize(lambda)));
		EXPECT_EQ("rec v0.{v0=fun(ref<A,f,f,plain> v1) {}}", toString(*normalize(normalize(lambda))));
	}

	TEST(Normalizing, ForLoops) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<string, NodePtr> symbols;
		symbols["e"] = builder.variable(builder.parseType("int<4>"), 100);

		auto code = "{"
		            "	var int<4> x = 123;"
		            "	var ref<int<4>> y = 12;"
		            "	var ref<int<4>> sum = 0;"
		            "	for(int<4> i = x-12+y+e .. x+12+y : x) {"
		            "		for(int<4> j = x-12+y+e .. x+12+y : x) {"
		            "			sum = sum + i + j;"
		            "		}"
		            "	}"
		            "}";

		auto a = builder.parseStmt(code, symbols);
		auto b = builder.parseStmt(code, symbols);

		//		dumpPretty(a);
		//		dumpPretty(b);
		//
		//		dumpPretty(normalize(a));
		//		dumpPretty(normalize(b));

		EXPECT_NE(a, b);
		EXPECT_NE(*a, *b);

		EXPECT_EQ(normalize(a), normalize(b));
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
