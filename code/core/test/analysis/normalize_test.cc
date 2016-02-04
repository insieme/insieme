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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/normalize.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/lang/array.h"

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

		EXPECT_EQ("AP(rec _.{_=fun(ref<int<4>,f,f,plain> v0) {ref_deref(v0);}}(v2))", toString(normalize(transform::outline(manager, StatementPtr(a)))));
		EXPECT_EQ("AP(rec _.{_=fun(ref<bool,f,f,plain> v0) {ref_deref(v0);}}(v3))", toString(normalize(transform::outline(manager, StatementPtr(b)))));

		EXPECT_EQ("AP(rec _.{_=fun(ref<int<4>,f,f,plain> v0, ref<bool,f,f,plain> v1) {ref_deref(v0); ref_deref(v1);}}(v2, v3))",
		          toString(normalize(transform::outline(manager, StatementPtr(builder.compoundStmt(a, b))))));
		EXPECT_EQ("AP(rec _.{_=fun(ref<int<4>,f,f,plain> v0, ref<bool,f,f,plain> v1) {ref_deref(v0); {ref_deref(v1);};}}(v2, v3))",
		          toString(normalize(transform::outline(manager, StatementPtr(builder.compoundStmt(a, builder.compoundStmt(b)))))));


		// test a function
		manager.setNextFreshID(5);
		NodePtr node = builder.parseStmt("{var int<4> a = 0; let f = (a : int<4>, b : int<4>)->int<4> { return a; } in f(a,a); }");
		EXPECT_EQ("AP({int<4> v0 = 0; rec _.{_=fun(ref<int<4>,f,f,plain> v0, ref<int<4>,f,f,plain> v1) {return ref_deref(v0);}}(v0, v0);})",
			      toString(normalize(node)));
		EXPECT_EQ("AP({int<4> v0 = 0; rec _.{_=fun(ref<int<4>,f,f,plain> v0, ref<int<4>,f,f,plain> v1) {return ref_deref(v0);}}(v0, v0);})",
			      toString(normalize(node)));
		
		// test normalization with existing free variables
		VariablePtr z = builder.variable(basic.getInt4(), 0); // create a v0!
		std::map<string, NodePtr> map;
		map["z"] = z;

		ExpressionPtr expr = builder.parseExpr("let f = ()->unit { z; } in f", map).as<ExpressionPtr>();

		ASSERT_TRUE(expr);

		EXPECT_EQ("AP(rec _.{_=fun() {v0;}})", toString(normalize(expr)));
		
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

		EXPECT_EQ("rec f.{f=fun(ref<int<4>,f,f,plain> v0) {return rec _.{_=fun(ref<int<4>,f,f,plain> v0) {return f(ref_deref(v0));}}(ref_deref(v0));}}(3)",
			      toString(*normalize(code)));
	}

	TEST(Normalizing, VariablesInTypes) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto code = builder.parseExpr("def x = () -> unit {"
		                              "	var int<inf> v40 = 3;"
		                              "	var ref<array<int<4>,#v40>,f,f,plain> v50;"
		                              "}; x()");
		EXPECT_EQ("rec x.{x=fun() {int<inf> v0 = 3; ref<array<int<4>,v0>,f,f,plain> v1 = v1;}}()",
			      toString(*normalize(code)));
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
		LambdaReferencePtr lambdaRef = builder.lambdaReference(funType, "f");

		LambdaBindingMap bindings = { { lambdaRef, builder.lambda(funType, toVector(var1), builder.compoundStmt()) } };
		LambdaExprPtr lambda = builder.lambdaExpr(lambdaRef, builder.lambdaDefinition(bindings));

		EXPECT_EQ("rec f.{f=fun(ref<A,f,f,plain> v12) {}}", toString(*lambda));
		EXPECT_EQ("rec f.{f=fun(ref<A,f,f,plain> v0) {}}", toString(*normalize(lambda)));
		EXPECT_EQ("rec f.{f=fun(ref<A,f,f,plain> v0) {}}", toString(*normalize(normalize(lambda))));


		// this was failing once - the normalized annotation was not properly transfered
		NodeManager mgr2;
		lambda = mgr2.get(lambda);
		EXPECT_EQ("rec f.{f=fun(ref<A,f,f,plain> v12) {}}", toString(*lambda));
		EXPECT_EQ("rec f.{f=fun(ref<A,f,f,plain> v0) {}}", toString(*normalize(lambda)));
		EXPECT_EQ("rec f.{f=fun(ref<A,f,f,plain> v0) {}}", toString(*normalize(normalize(lambda))));
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
	
	TEST(Normalizing, VarInType) {
        NodeManager mgr;
        IRBuilder builder(mgr);
		
		auto v192 = builder.variable(mgr.getLangBasic().getInt4(), 192);
		auto testVar = builder.variable(builder.numericType(v192));
		auto testCompound = builder.compoundStmt(builder.declarationStmt(v192, builder.intLit(4)), testVar);
        EXPECT_TRUE(analysis::contains(testCompound, v192));
        EXPECT_FALSE(analysis::contains(builder.normalize(testCompound), v192));
	}
	
	TEST(Normalizing, FreeVarInType) {
        NodeManager mgr;
        IRBuilder builder(mgr);
		
		auto v192 = builder.variable(mgr.getLangBasic().getInt4(), 192);
		auto testVar = builder.variable(builder.numericType(v192));
		auto testCompound = builder.compoundStmt(testVar);
        EXPECT_TRUE(analysis::contains(testCompound, v192));
        EXPECT_TRUE(analysis::contains(builder.normalize(testCompound), v192));
	}

	TEST(Normalizing, VarInTypeNested) {
        NodeManager mgr;
        IRBuilder builder(mgr);
		
		auto v192 = builder.variable(mgr.getLangBasic().getInt4(), 192);
		auto genType = builder.genericType("bla", toVector<TypePtr>(builder.numericType(v192)));
		auto outerType = builder.genericType("alb", toVector<TypePtr>(genType));
		auto testVar = builder.variable(outerType);
		auto testCompound = builder.compoundStmt(builder.declarationStmt(v192, builder.intLit(4)), testVar);
		//std::cout << dumpText(testCompound);
        EXPECT_TRUE(analysis::contains(testCompound, v192));
        EXPECT_FALSE(analysis::contains(builder.normalize(testCompound), v192));
		//std::cout << dumpText(builder.normalize(testCompound));
	}

	TEST(Normalizing, ArrayType) {
        NodeManager mgr;
        IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();
		
		auto v192 = builder.variable(mgr.getLangBasic().getUIntInf(), 192);
		auto v193 = builder.variable(mgr.getLangBasic().getUIntInf(), 193);
		auto arrType1 = builder.arrayType(basic.getInt4(), v192);
		auto arrType2 = builder.arrayType(arrType1, v193);
		
        EXPECT_TRUE(analysis::contains(arrType1, v192));
        EXPECT_TRUE(analysis::contains(builder.normalize(arrType1), v192));
		
        EXPECT_TRUE(analysis::contains(arrType2, v192));
        EXPECT_TRUE(analysis::contains(arrType2, v193));
        EXPECT_TRUE(analysis::contains(builder.normalize(arrType2), v192));
        EXPECT_TRUE(analysis::contains(builder.normalize(arrType2), v193));
		
		auto arrVar1 = builder.variable(arrType1);
		auto arrVar2 = builder.variable(arrType2);

		auto compound1 = builder.compoundStmt(builder.declarationStmt(v192, builder.intLit(4)), arrVar1, arrVar2);
        EXPECT_TRUE(analysis::contains(compound1, v192));
		EXPECT_TRUE(analysis::contains(compound1, v193));
		auto compound1N = builder.normalize(compound1);
		EXPECT_FALSE(analysis::contains(compound1N, v192));
		EXPECT_TRUE(analysis::contains(compound1N, v193));
		EXPECT_EQ(compound1N->getStatement(0).as<DeclarationStmtPtr>()->getVariable(),
			      lang::ArrayType(compound1N->getStatement(1).as<VariablePtr>()->getType()).getSize());
		EXPECT_EQ(v193, lang::ArrayType(compound1N->getStatement(2).as<VariablePtr>()->getType()).getSize());

		auto compound2 = builder.compoundStmt(builder.declarationStmt(v193, builder.intLit(4)), compound1);
		EXPECT_TRUE(analysis::contains(compound2, v192));
        EXPECT_TRUE(analysis::contains(compound2, v193));
		auto compound2N = builder.normalize(compound2);
        EXPECT_FALSE(analysis::contains(compound2N, v192));
        EXPECT_FALSE(analysis::contains(compound2N, v193));		
		EXPECT_EQ(compound2N->getStatement(0).as<DeclarationStmtPtr>()->getVariable(),
			      lang::ArrayType(compound2N->getStatement(1).as<CompoundStmtPtr>()->getStatement(2).as<VariablePtr>()->getType()).getSize());
		//std::cout << dumpColor(compound2N) << dumpColor(compound2N->getStatement(1).as<CompoundStmtPtr>()->getStatement(2).as<VariablePtr>()->getType());
	}

	
	TEST(Normalizing, Special) {
        NodeManager mgr;
        IRBuilder builder(mgr);

		auto testAddrs = builder.parseAddressesStatement(R"({
			{
				var uint<inf> v2 = 1;
				var uint<inf> v3 = 2;
				$var ref<array<array<real<4>,#v3>,#v2>,f,f> v4;$
			}
		})");

		ASSERT_EQ(testAddrs.size(), 1);

		auto testcode = testAddrs[0].getRootNode();
		
		//std::cout << "INPUT: ----------------------\n" << testcode << "-------------------------\n";
		//std::cout << "OUTPUT: ----------------------\n" << builder.normalize(testcode) << "-------------------------\n";

		auto testNormalized = builder.normalize(testcode);
		auto addrNorm = testAddrs[0].switchRoot(testNormalized);
		auto arrType = lang::ArrayType(analysis::getReferencedType(addrNorm.getAddressedNode().as<DeclarationStmtPtr>().getVariable().getType()));
		auto size1 = arrType.getSize();
		auto size2 = lang::ArrayType(arrType.getElementType()).getSize();
		EXPECT_NE(size1, size2);
	}


	TEST(Normalizing, DeclarationStmt) {
        NodeManager mgr;
        IRBuilder builder(mgr);
	
		auto testExpr = builder.parseExpr(R"(
			def testLambda = () -> int<4> { var int<4> v; return v; };
			testLambda()
		)").as<CallExprPtr>();
	
		{
			auto lambda = testExpr->getFunctionExpr().as<LambdaExprPtr>();
			auto ret = lambda->getBody()->getStatement(1).as<ReturnStmtPtr>();
			auto retVar = ret->getReturnDeclStmt()->getVariable();
			auto var = lambda->getBody()->getStatement(0).as<DeclarationStmtPtr>()->getVariable();
			EXPECT_NE(retVar, var);
		}

		auto normalized = builder.normalize(testExpr);
		{
			auto lambda = normalized->getFunctionExpr().as<LambdaExprPtr>();
			auto ret = lambda->getBody()->getStatement(1).as<ReturnStmtPtr>();
			auto retVar = ret->getReturnDeclStmt()->getVariable();
			auto var = lambda->getBody()->getStatement(0).as<DeclarationStmtPtr>()->getVariable();
			EXPECT_NE(retVar, var);
		}

	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
