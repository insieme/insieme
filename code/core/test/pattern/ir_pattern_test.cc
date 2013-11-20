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

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/lang/basic.h"

#include "insieme/utils/logging.h"

#ifndef TEST
// avoiding warnings in eclipse, enabling auto completions
#define TEST void fun
#endif

using namespace insieme::utils::log;

namespace insieme {
namespace core {
namespace pattern {

bool isMatch(const TreePatternPtr& pattern, const NodePtr& node) {
	return pattern->match(node);
}

bool noMatch(const TreePatternPtr& pattern, const NodePtr& node) {
	return !isMatch(pattern, node);
}

template<typename T>
vector<T> filterNull(const vector<T>& list) {
	vector<T> res;
	for (auto cur : list) {
		if (cur) res.push_back(cur);
	}
	return res;
}

TEST(IRConvert, Basic) {
	NodeManager manager;
	auto t = [&manager](const string& typespec) { 
		return IRBuilder(manager).parseType(typespec); 
	};
	
	TypePtr tupleA = t("(int<4>,float<8>,uint<1>)");
	TypePtr tupleB = t("(int<4>,float<8>)");


	TreePatternPtr patternA = aT(atom(t("float<8>")));
	EXPECT_PRED2(isMatch, patternA, tupleA);
	TreePatternPtr patternB = aT(atom(t("uint<8>")));
	EXPECT_PRED2(noMatch, patternB, tupleA);
	TreePatternPtr patternC = irp::tupleType(any << atom(t("float<8>")) << any);
	EXPECT_PRED2(isMatch, patternC, tupleA);
	EXPECT_PRED2(noMatch, patternC, tupleB);

	TreePatternPtr patternD = irp::tupleType(*any << atom(t("float<8>")) << *any);
	EXPECT_PRED2(isMatch, patternD, tupleA);
	EXPECT_PRED2(isMatch, patternD, tupleB);

	TreePatternPtr patternE = irp::tupleType(*any << atom(t("uint<1>")) << *any);
	EXPECT_PRED2(isMatch, patternE, tupleA);
	EXPECT_PRED2(noMatch, patternE, tupleB);
}


TEST(IRPattern, Types) {
	NodeManager manager;
	auto pt = [&manager](const string& typespec) { 
		return IRBuilder(manager).parseType(typespec); 
	};
	
	TypePtr int8Type = pt("int<8>");
	TypePtr genericA = pt("megatype<ultratype<int<8>,666>>");
	TypePtr genericB = pt("megatype<ultratype<int<8>,B>>");
	
	TreePatternPtr patternA = irp::genericType("megatype", single(any));
	EXPECT_PRED2(noMatch, patternA, int8Type);
	EXPECT_PRED2(isMatch, patternA, genericA);

	TreePatternPtr patternB = irp::genericType("ultratype", any << any);
	EXPECT_PRED2(noMatch, patternB, int8Type);
	EXPECT_PRED2(noMatch, patternB, genericA);
	EXPECT_PRED2(noMatch, aT(patternB), int8Type);
	EXPECT_PRED2(noMatch, aT(patternB), genericA);
	EXPECT_PRED2(isMatch, aT(patternB), genericB);

	TreePatternPtr patternC = irp::genericType("ultratype", single(any), single(any));
	EXPECT_PRED2(noMatch, patternC, int8Type);
	EXPECT_PRED2(noMatch, patternC, genericA);
	EXPECT_PRED2(noMatch, aT(patternC), int8Type);
	EXPECT_PRED2(isMatch, aT(patternC), genericA);
	EXPECT_PRED2(noMatch, aT(patternC), genericB);
}


TEST(IRPattern, literal) {
	NodeManager manager;
	auto pe = [&manager](const string& typespec) { 
		return IRBuilder(manager).parseExpr(typespec); 
	};

	ExpressionPtr exp1 = pe("4.2 + 3.1");
	ExpressionPtr exp2 = pe("4 - 2");
	ExpressionPtr exp3 = pe("(4.2 + 3.1) * (real<8>)(4 - 2)");
	
	TreePatternPtr patternA = irp::callExpr(manager.getLangBasic().getRealAdd(), *any);
	EXPECT_PRED2(isMatch, patternA, exp1);
	EXPECT_PRED2(noMatch, patternA, exp2);
	EXPECT_PRED2(noMatch, patternA, exp3);
	
	TreePatternPtr patternB = node(any << irp::literal("int.sub") << *any);
	EXPECT_PRED2(noMatch, patternB, exp1);
	EXPECT_PRED2(isMatch, patternB, exp2);
	EXPECT_PRED2(noMatch, patternB, exp3);
}

TEST(IRPattern, variable) {
	NodeManager manager;
	auto at = [&manager](const string& str) { return irp::atom(manager, str); };

	IRBuilder builder(manager);
	std::map<std::string, core::NodePtr> symbols;
	symbols["x"] = builder.variable(builder.parseType("int<8>"));

	StatementPtr var1 = builder.parseExpr("x", symbols);

	TreePatternPtr pattern1 = irp::variable(at("int<8>"), any);
	EXPECT_PRED2(isMatch, pattern1, var1);
}

TEST(IRPattern, lambdaExpr) {
	NodeManager manager;

	ExpressionPtr exp1 = IRBuilder(manager).parseExpr("(int<4> i, int<8> v) -> int<4> { return i; }");

	TreePatternPtr pattern1 = irp::lambdaExpr(any, irp::lambdaDefinition(*any));
	EXPECT_PRED2(isMatch, pattern1, exp1);
}

TEST(IRPattern, lambda) {
	NodeManager manager;

	NodePtr node = IRBuilder(manager).parseExpr("(int<8> i, int<8> v) -> int<8> return i;").
				   as<core::LambdaExprPtr>()->getLambda();

	TreePatternPtr pattern1 = irp::lambda(any, irp::variable(var("x"), any) << irp::variable(var("x"), any), any);
	EXPECT_PRED2(isMatch, pattern1, node);
}

TEST(IRPattern, callExpr) {
	NodeManager manager;

	ExpressionPtr exp1 = IRBuilder(manager).parseExpr("4 + 5");

	TreePatternPtr pattern1 = irp::callExpr(any, irp::literal(any, any) , *any);
	EXPECT_PRED2(isMatch, pattern1, exp1);
}

TEST(IRPattern, declarationStmt) {
	NodeManager manager;
	auto at = [&manager](const string& str) { return irp::atom(manager, str); };

	StatementPtr stmt1 = IRBuilder(manager).parseStmt("int<4> i = 3;");

	TreePatternPtr pattern1 = irp::declarationStmt(any, any);
	TreePatternPtr pattern2 = irp::declarationStmt(irp::variable(at("int<4>"), any), at("3"));
	EXPECT_PRED2(isMatch, pattern1, stmt1);
	EXPECT_PRED2(isMatch, pattern2, stmt1);
}

TEST(IRPattern, ifStmt) {
	NodeManager manager;
	auto at = [&manager](const string& str) { return irp::atom(manager, str); };
	auto ps = [&manager](const string& str) { return IRBuilder(manager).parseStmt(str); };

	StatementPtr stmt1 = ps("if( false ) { return 0; } else { return 1+2; }");
	StatementPtr stmt2 = ps("if( false ) { return 0; }");
	StatementPtr stmt3 = ps("if(1 != 0) { return 0; }");

	TreePatternPtr pattern1 = irp::ifStmt(any, at("{ return 0; }"), irp::returnStmt(any));
	TreePatternPtr pattern2 = irp::ifStmt(any, irp::returnStmt(any), irp::returnStmt(any));
	TreePatternPtr pattern3 = irp::ifStmt(any, irp::returnStmt(at("1")|at("0")), irp::returnStmt(any));
	TreePatternPtr pattern4 = irp::ifStmt(any, at("{ return 0; }"), any);
	TreePatternPtr pattern5 = irp::ifStmt(any, at("{ return 0; }"), at("{ }"));
	TreePatternPtr pattern6 = irp::ifStmt(at("1 != 0"), at("{ return 0; }"), at("{ }"));

	EXPECT_PRED2(isMatch, pattern1, stmt1);
	EXPECT_PRED2(isMatch, pattern2, stmt1);
	EXPECT_PRED2(isMatch, pattern3, stmt1);
	EXPECT_PRED2(isMatch, pattern4, stmt1);
	EXPECT_PRED2(noMatch, pattern5, stmt1);
	EXPECT_PRED2(noMatch, pattern6, stmt1);

	EXPECT_PRED2(noMatch, pattern1, stmt2);
	EXPECT_PRED2(noMatch, pattern2, stmt2);
	EXPECT_PRED2(noMatch, pattern3, stmt2);
	EXPECT_PRED2(isMatch, pattern4, stmt2);
	EXPECT_PRED2(isMatch, pattern5, stmt2);
	EXPECT_PRED2(noMatch, pattern6, stmt2);

	EXPECT_PRED2(noMatch, pattern1, stmt3);
	EXPECT_PRED2(noMatch, pattern2, stmt3);
	EXPECT_PRED2(noMatch, pattern3, stmt3);
	EXPECT_PRED2(isMatch, pattern4, stmt3);
	EXPECT_PRED2(isMatch, pattern5, stmt3);
	EXPECT_PRED2(isMatch, pattern6, stmt3);
}

TEST(IRPattern, forStmt) {
	NodeManager manager;
	auto at = [&manager](const string& str) { return irp::atom(manager, str); };
	auto ps = [&manager](const string& str) { return IRBuilder(manager).parseStmt(str); };

	StatementPtr stmt1 = ps("for(int<4> i = 30 .. 5 : -5) { int<4> i = 3;}");
	StatementPtr stmt2 = ps("for(int<4> i = 0 .. 10 : 2) { return 0; }");
	StatementPtr stmt3 = ps("for(int<4> i = 0 .. 5) { 7; 6; continue; 8; }");
	StatementPtr stmt4 = ps("for(int<4> i = 0 .. 2) { for(int<4> j = 0 .. 2){ return 0; } }");

	TreePatternPtr pattern1 = irp::forStmt(var("x"), any, at("5"), at("-5"), irp::declarationStmt(var("y"), at("3")));
	TreePatternPtr pattern2 = irp::forStmt(any, any, at("10"), at("2"), irp::returnStmt(at("0")));
	TreePatternPtr pattern3 = irp::forStmt(any, any, at("5"), at("1"), irp::compoundStmt(*any << irp::continueStmt() << any));
	TreePatternPtr pattern4 = irp::forStmt(var("i"), var("x"), var("y"), var("z"), irp::compoundStmt(irp::forStmt(var("j"), var("x"), var("y"), var("z"), irp::compoundStmt(*any)) << *any));

	EXPECT_PRED2(isMatch, pattern1, stmt1);
	EXPECT_PRED2(isMatch, pattern2, stmt2);
	EXPECT_PRED2(isMatch, pattern3, stmt3);
	EXPECT_PRED2(isMatch, pattern4, stmt4);
}

TEST(IRPattern, Addresses) {
	NodeManager manager;
	auto at = [&manager](const string& str) { return irp::atom(manager, str); };
	auto ps = [&manager](const string& str) { return StatementAddress(IRBuilder(manager).parseStmt(str)); };

	StatementAddress stmt1 = ps("for(int<4> i = 30 .. 5 : -5) { int<4> i = 3;}");
	StatementAddress stmt2 = ps("for(int<4> i = 0 .. 2) { for(int<4> j = 0 .. 2){ 7; return 0; } }");

	TreePatternPtr pattern1 = irp::forStmt(var("x"), any, at("5"), at("-5"), irp::declarationStmt(var("y"), at("3")));
	TreePatternPtr pattern2 = irp::forStmt(var("i"), var("x"), var("y"), var("z"), irp::compoundStmt(irp::forStmt(var("j"), var("x"), var("y"), var("z"), irp::compoundStmt(*var("b", any))) << *any));

	// addresses always point to first encounter
	auto match = pattern1->matchAddress(stmt1);
	EXPECT_TRUE(match);
	ASSERT_TRUE(match->isVarBound("x"));
	ASSERT_TRUE(match->isVarBound("y"));
	EXPECT_EQ(stmt1.getAddressOfChild(0,0), match->getVarBinding("x").getValue());
	EXPECT_EQ(stmt1.getAddressOfChild(3,0,0), match->getVarBinding("y").getValue());

	match = pattern2->matchAddress(stmt2);
	EXPECT_TRUE(match);
	EXPECT_EQ(stmt2.getAddressOfChild(0,0), match->getVarBinding("i").getValue());
	EXPECT_EQ(stmt2.getAddressOfChild(0,1), match->getVarBinding("x").getValue());
	EXPECT_EQ(stmt2.getAddressOfChild(1), match->getVarBinding("y").getValue());
	EXPECT_EQ(stmt2.getAddressOfChild(2), match->getVarBinding("z").getValue());

	EXPECT_EQ(
			toString(toVector<NodeAddress>(
					stmt2.getAddressOfChild(3,0,3,0),
					stmt2.getAddressOfChild(3,0,3,1)
			)),
			toString(match->getVarBinding("b").getList())
	);
}

//TEST(IRPattern, whileStmt) {
//	NodeManager manager;
//	auto at = [&manager](string str) { return irp::atom(manager, str); };
//	auto ps = [&manager](string str) { return parse::parseStatement(manager, str); };
//
//	StatementPtr stmt1 = ps(" { decl ref<int<4>>:i = 30; while((i < 40)){ (i++); }; }");
//	StatementPtr stmt2 = ps(" while((ref<int<4>>:i < 40)){ { }; { decl int<4>:b = (i++);}; }");
//
//	auto tree1 = toTree(stmt1);
//	auto tree2 = toTree(stmt2);
//
//	TreePatternPtr pattern1 = irp::compoundStmt(irp::declarationStmt(var("x"), at("30")) << irp::whileStmt(irp::callExpr(irp::literal("int.lt", any),
//								irp::callExpr(irp::literal("ref.deref", any), var("x") << *any) << *any), irp::compoundStmt(*any << irp::callExpr(any, var("x") << *any))));
//	TreePatternPtr pattern2 = irp::whileStmt(irp::callExpr(irp::literal("int.lt", any), irp::callExpr(irp::literal("ref.deref", any), var("x") << *any) << *any),
//											 aT(irp::declarationStmt(any, aT(var("x")))));
//
//	/*MatchOpt mo = pattern1->isMatch(tree1);
//	if (mo){
//		MatchValue mv = mo->getVarBinding("x");
//		std::cout << mv.getTree()->getAttachedValue<NodePtr>();
//	}*/
//
//	EXPECT_PRED2(isMatch, pattern1, tree1) << *stmt1;
//	EXPECT_PRED2(isMatch, pattern2, tree2) << *stmt2;
//}


TEST(IRPattern, AbitrarilyNestedLoop) {

	// create a pattern for an arbitrarily nested loop
	auto pattern = rT(var("loops", irp::forStmt(var("iter",any), any, any, any, aT(recurse) | aT(!irp::forStmt()))));
//	auto pattern = rT(var("loops", irp::forStmt(var("iter",any), any, any, any, step(recurse) | !aT(irp::forStmt()))));

	//EXPECT_EQ("", toString(pattern));

	// create loops and test the pattern
	NodeManager mgr;
	IRBuilder builder(mgr);
	// not perfectly nested loop

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	auto match = pattern->matchPointer(builder.parseStmt(
		"for( uint<4> i = 10u .. 50u) { "
		"	v[i]; "
		"	for(uint<4> j = 5u .. 25u) { "
		"		v[i+j]; "
		"	} "
		"}", symbols).as<ForStmtPtr>());

    EXPECT_TRUE(match);
    EXPECT_EQ(2u, match->getVarBinding("loops").getList().size());
    EXPECT_EQ(2u, match->getVarBinding("iter").getList().size());


    match = pattern->matchPointer(builder.parseStmt(
 		"for(uint<4> k = 1u .. 6u) { "
    	"	v[k]; "
		"	for(uint<4> i = 10u .. 50u) { "
		"		v[i]; "
		"		for(uint<4> j = 5u .. 25u) { "
		"			v[i+j]; "
		"		} "
		"		v[i]; "
		"    } "
		"}", symbols).as<ForStmtPtr>());
    EXPECT_TRUE(match);
    EXPECT_EQ(3u, match->getVarBinding("loops").getList().size());
    EXPECT_EQ(3u, match->getVarBinding("iter").getList().size());

}

TEST(TargetFilter, InnerMostForLoop) {

	core::NodeManager manager;
	IRBuilder builder(manager);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	core::NodePtr node = builder.parseStmt(
		"for(uint<4> l = 8u .. 70u : 3u) { "
		"	l; "
		"	for(uint<4> i = 10u .. 50u) { "
		"		for(uint<4> j = 3u .. 25u ) { "
		"			j; "
		"			for(uint<4> k = 2u .. 100u ) { "
		"				v[i+j]; "
		"			} "
		"           ref<uint<4>> a = var(3u); "
		"			a = i; "
		"		} "
		"	} "
		"}", symbols);

	EXPECT_TRUE(node);

	core::NodeAddress root(node);

	core::NodeAddress for1(node);
	core::NodeAddress for2 = for1.getAddressOfChild(3,1);
	core::NodeAddress for3 = for2.getAddressOfChild(3,0);
	core::NodeAddress for4 = for3.getAddressOfChild(3,1);

	EXPECT_EQ(core::NT_ForStmt, for1->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for2->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for3->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for4->getNodeType());

	// try getting innermost loop
	TreePatternPtr pattern = irp::innerMostForLoop();
	EXPECT_EQ(toVector(for4), irp::collectAll(pattern, root));

	EXPECT_EQ(toVector(for4), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoop(2);
	EXPECT_EQ(toVector(for3), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoop(3);
	EXPECT_EQ(toVector(for2), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoop(4);
	EXPECT_EQ(toVector(for1), irp::collectAll(pattern, root));


	// try getting innermost loop
	pattern = irp::innerMostForLoopNest();
	EXPECT_EQ(toVector(for4), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoopNest(1);
	EXPECT_EQ(toVector(for4), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoopNest(2);
	EXPECT_EQ(toVector(for3), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoopNest(3);
	EXPECT_EQ(toVector(for2), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoopNest(4);
	EXPECT_EQ(toVector(for1), irp::collectAll(pattern, root));

}


TEST(TargetFilter, InnerMostForLoop2) {

	core::NodeManager manager;
	IRBuilder builder(manager);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	core::NodePtr node = builder.parseStmt(
		"for(uint<4> l = 8u .. 70u : 3u) {"
		"	l; "
		"	for(uint<4> i = 10u .. 50u) {"
		"		for(uint<4> j = 3u .. 25u) {"
		"			j; "
		"			for(uint<4> k = 2u .. 100u) { "
		"				v[i+j]; "
		"			}; "
		"           ref<uint<4>> a = var(3u); "
		"			a = i; "
		"		}"
		"	}"
		"	for(uint<4> k = 2u .. 100u) {"
		"		v[l+k];"
		"	}"
		"}", symbols);

	EXPECT_TRUE(node);

	core::NodeAddress root(node);

	core::NodeAddress for1(node);
	core::NodeAddress for2 = for1.getAddressOfChild(3,1);
	core::NodeAddress for3 = for2.getAddressOfChild(3,0);
	core::NodeAddress for4 = for3.getAddressOfChild(3,1);
	core::NodeAddress for5 = for1.getAddressOfChild(3,2);

	EXPECT_EQ(core::NT_ForStmt, for1->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for2->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for3->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for4->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for5->getNodeType());

	// try getting innermost loop
	TreePatternPtr pattern = irp::innerMostForLoop();
	EXPECT_EQ(toVector(for4, for5), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoop(1);
	EXPECT_EQ(toVector(for4, for5), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoop(2);
	EXPECT_EQ(toVector(for1, for3), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoop(3);
	EXPECT_EQ(toVector(for2), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoop(4);
	EXPECT_EQ(toVector(for1), irp::collectAll(pattern, root));


	// try getting innermost loop
	pattern = irp::innerMostForLoopNest();
	EXPECT_EQ(toVector(for4, for5), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoopNest(1);
	EXPECT_EQ(toVector(for4, for5), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoopNest(2);
	EXPECT_EQ(toVector(for3), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoopNest(3);
	EXPECT_EQ(toVector(for2), irp::collectAll(pattern, root));

	pattern = irp::innerMostForLoopNest(4);
	EXPECT_EQ(toVector(for1), irp::collectAll(pattern, root));

}

TEST(TargetFilter, AllForLoops) {

	core::NodeManager manager;
	IRBuilder builder(manager);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	core::NodePtr node = builder.parseStmt(
		"for(uint<4> l = 8u .. 70u : 3u) {"
		"	l; "
		"	for(uint<4> i = 10u .. 50u) {"
		"		for(uint<4> j = 3u .. 25u) {"
		"			j; "
		"			for(uint<4> k = 2u .. 100u) { "
		"				v[i+j]; "
		"			}; "
		"           ref<uint<4>> a = var(3u); "
		"			a = i; "
		"		}"
		"	}"
		"	for(uint<4> k = 2u .. 100u) {"
		"		v[l+k];"
		"	}"
		"}", symbols);

	EXPECT_TRUE(node);

	core::NodeAddress root(node);

	core::NodeAddress for1(node);
	core::NodeAddress for2 = for1.getAddressOfChild(3,1);
	core::NodeAddress for3 = for2.getAddressOfChild(3,0);
	core::NodeAddress for4 = for3.getAddressOfChild(3,1);
	core::NodeAddress for5 = for1.getAddressOfChild(3,2);

	EXPECT_EQ(core::NT_ForStmt, for1->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for2->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for3->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for4->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for5->getNodeType());

	// try getting innermost loop
	TreePatternPtr pattern = all(var("x", irp::forStmt()));
	auto match = pattern->matchAddress(root);
	ASSERT_TRUE(match);
	EXPECT_EQ(toVector(for1, for2, for3, for4, for5), match->getVarBinding("x").getList());

}

TEST(PatternTests, AllVarDecls) {

	core::NodeManager manager;
	IRBuilder builder(manager);

	core::NodePtr node = builder.normalize(builder.parseStmt(
			R"(
				{
					int<4> a = 1;
					int<4> b = 2;
					a;
					int<4> c = a + b;
				}
			)"
	));

	ASSERT_TRUE(node);


	auto decl = irp::declarationStmt(var("x"), any);
	auto notDecl = !irp::declarationStmt(any, any);

	auto pattern = irp::compoundStmt(*(decl | notDecl));

	// match the pattern
	auto res = pattern->matchPointer(node);

	ASSERT_TRUE(res);
	EXPECT_EQ("Match({x=[AP(v0),AP(v1),null,AP(v2)]})", toString(*res));

}

TEST(PatternTests, VarUsage) {

	core::NodeManager manager;
	IRBuilder builder(manager);

	core::NodePtr code = builder.normalize(builder.parseStmt(
			R"(
				{
					int<4> a = 1;
					int<4> b = 2;
					//a;
					int<4> c = a + b;

					int<4> d = 3;
					int<4> e = d + a;
				}
			)"
	));

	ASSERT_TRUE(code);

	auto x = var("x");
	auto decl = irp::declarationStmt(x, any);
	auto use = !irp::declarationStmt(any, any) & step(x);

//	auto pattern = aT(decl) & !aT(use);
	auto used = aT(decl) & aT(var("y", use));
	auto unused = aT(decl) & !aT(use);
	auto allUses = aT(decl) & aT(use) & all(var("y", use));

//	std::cout << used << "\n";
//	std::cout << unused << "\n";
//	std::cout << allUses << "\n";

	auto res1 = used->matchPointer(code);
	ASSERT_TRUE(res1);
//	std::cout << "Match: " << *res1 << "\n";
	EXPECT_EQ("AP(v0)", toString(res1->getVarBinding("x")));
//	EXPECT_EQ("AP(int.add(v0, v1))", toString(res1->getVarBinding("y")));

	auto res2 = unused->matchPointer(code);
	ASSERT_TRUE(res2);
//	std::cout << "Match: " << *res2 << "\n";

	auto res3 = allUses->matchPointer(code);
	ASSERT_TRUE(res3);
//	std::cout << "Match: " << *res3 << "\n";
	EXPECT_EQ("AP(v0)", toString(res3->getVarBinding("x").getValue()));
	EXPECT_EQ("[AP(int.add(v0, v1)),AP(int.add(v3, v0))]", toString(res3->getVarBinding("y").getList()));

//	std::cout << "x=" << res->getVarBinding("x") << "\n";
//	std::cout << "y=" << res->getVarBinding("y") << "\n";
//	std::cout << "z=" << res->getVarBinding("z") << "\n";
}

} // end namespace pattern
} // end namespace core
} // end namespace insieme

