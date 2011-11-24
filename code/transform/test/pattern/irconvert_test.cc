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

#include "insieme/transform/pattern/irconvert.h"
#include "insieme/transform/pattern/irpattern.h"
#include "insieme/core/parser/ir_parse.h"

#include "insieme/utils/logging.h"

#ifndef TEST
// avoiding warnings in eclipse, enabling auto completions
#define TEST void fun
#endif

using namespace insieme::utils::log;

namespace insieme {
using namespace core;
	
namespace transform {
namespace pattern {

bool match(const TreePatternPtr& pattern, const TreePtr& tree) {
	return pattern->match(tree);
}

bool notMatch(const TreePatternPtr& pattern, const TreePtr& tree) {
	return !match(pattern, tree);
}

TEST(IRConvert, Basic) {
	NodeManager manager;
	auto t = [&manager](string typespec) { return parse::parseType(manager, typespec); };
	
	TypePtr tupleA = t("(int<4>,float<8>,uint<1>)");
	TypePtr tupleB = t("(int<4>,float<8>)");

	//LOG(INFO) << *tupleA;

	auto treeA = toTree(tupleA);
	auto treeB = toTree(tupleB);
	//LOG(INFO) << treeA;

	TreePatternPtr patternA = aT(atom(toTree(t("float<8>"))));
	EXPECT_PRED2(match, patternA, treeA);
	TreePatternPtr patternB = aT(atom(toTree(t("uint<8>"))));
	EXPECT_PRED2(notMatch, patternB, treeA);
	TreePatternPtr patternC = irp::tupleType(any << atom(toTree(t("float<8>"))) << any);
	EXPECT_PRED2(match, patternC, treeA);
	EXPECT_PRED2(notMatch, patternC, treeB);

	TreePatternPtr patternD = irp::tupleType(*any << atom(toTree(t("float<8>"))) << *any);
	EXPECT_PRED2(match, patternD, treeA);
	EXPECT_PRED2(match, patternD, treeB);

	TreePatternPtr patternE = irp::tupleType(*any << atom(toTree(t("uint<1>"))) << *any);
	EXPECT_PRED2(match, patternE, treeA);
	EXPECT_PRED2(notMatch, patternE, treeB);
}


TEST(IRPattern, Types) {
	NodeManager manager;
	auto pt = [&manager](string str) { return parse::parseType(manager, str); };
	
	TypePtr int8Type = pt("int<8>");
	TypePtr genericA = pt("megatype<ultratype<int<8>,666>>");
	TypePtr genericB = pt("megatype<ultratype<int<8>,B>>");
	
	auto int8TypeTree = toTree(int8Type);
	auto genericATypeTree = toTree(genericA);
	auto genericBTypeTree = toTree(genericB);

	TreePatternPtr patternA = irp::genericType("megatype", single(any));
	EXPECT_PRED2(notMatch, patternA, int8TypeTree); 
	EXPECT_PRED2(match, patternA, genericATypeTree);

	TreePatternPtr patternB = irp::genericType("ultratype", any << any);
	EXPECT_PRED2(notMatch, patternB, int8TypeTree);
	EXPECT_PRED2(notMatch, patternB, genericATypeTree);
	EXPECT_PRED2(notMatch, aT(patternB), int8TypeTree);
	EXPECT_PRED2(notMatch, aT(patternB), genericATypeTree);
	EXPECT_PRED2(match, aT(patternB), genericBTypeTree);

	TreePatternPtr patternC = irp::genericType("ultratype", single(any), single(any));
	EXPECT_PRED2(notMatch, patternC, int8TypeTree);
	EXPECT_PRED2(notMatch, patternC, genericATypeTree);
	EXPECT_PRED2(notMatch, aT(patternC), int8TypeTree);
	EXPECT_PRED2(match, aT(patternC), genericATypeTree);
	EXPECT_PRED2(notMatch, aT(patternC), genericBTypeTree);
}


TEST(IRPattern, literal) {
	NodeManager manager;
	auto pe = [&manager](string str) { return parse::parseExpression(manager, str); };
	ExpressionPtr exp1 = pe("(4.2 + 3.1)");
	ExpressionPtr exp2 = pe("(4 - 2)");
	ExpressionPtr exp3 = pe("((4.2 + 3.1) * (4 - 2))");
	
	auto tree1 = toTree(exp1);
	auto tree2 = toTree(exp2);
	auto tree3 = toTree(exp3);

	TreePatternPtr patternA = irp::callExpr(manager.getLangBasic().getRealAdd(), *any);
	EXPECT_PRED2(match, patternA, tree1);
	EXPECT_PRED2(notMatch, patternA, tree2);
	EXPECT_PRED2(notMatch, patternA, tree3);
	
	TreePatternPtr patternB = node(irp::literal("int.sub", any) << *any);
	EXPECT_PRED2(notMatch, patternB, tree1);
	EXPECT_PRED2(match, patternB, tree2);
	EXPECT_PRED2(notMatch, patternB, tree3);
}

TEST(IRPattern, variable) {
	NodeManager manager;
	auto at = [&manager](string str) { return irp::atom(manager, str); };

	StatementPtr var1 = parse::parseExpression(manager, "int<8>:x");
	auto tree1 = toTree(var1);
	TreePatternPtr pattern1 = irp::variable(at("int<8>"), any);
	EXPECT_PRED2(match, pattern1, tree1);
}

TEST(IRPattern, lambdaExpr) {
	NodeManager manager;
	auto at = [&manager](string str) { return irp::atom(manager, str); };

	ExpressionPtr exp1 = parse::parseExpression(manager, "fun (int<4>:i, int<8>:v) -> int<4> { return i }");

	auto tree1 = toTree(exp1);
	TreePatternPtr pattern1 = irp::lambdaExpr(any, irp::lambdaDefinition(*any));
	EXPECT_PRED2(match, pattern1, tree1);
}

TEST(IRPattern, lambda) {
	NodeManager manager;
	auto at = [&manager](string str) { return irp::atom(manager, str); };

	NodePtr node = parse::parseIR(manager, "(int<8>:i, int<8>:v) -> int<4> { return i }");

	auto tree1 = toTree(node);
	TreePatternPtr pattern1 = irp::lambda(any, irp::variable(var("x"), any) << irp::variable(var("x"), any), any);
	EXPECT_PRED2(match, pattern1, tree1);
}

TEST(IRPattern, callExpr) {
	NodeManager manager;
	auto at = [&manager](string str) { return irp::atom(manager, str); };

	ExpressionPtr exp1 = parse::parseExpression(manager, "( 4 + 5 )");

	auto tree1 = toTree(exp1);
	TreePatternPtr pattern1 = irp::callExpr(any, irp::literal(any, any) , *any);
	EXPECT_PRED2(match, pattern1, tree1);
}

TEST(IRPattern, declarationStmt) {
	NodeManager manager;
	auto at = [&manager](string str) { return irp::atom(manager, str); };

	StatementPtr stmt1 = parse::parseStatement(manager, "decl int<4>:i = 3");
	auto tree1 = toTree(stmt1);
	TreePatternPtr pattern1 = irp::declarationStmt(any, any);
	TreePatternPtr pattern2 = irp::declarationStmt(irp::variable(at("int<4>"), any), at("3"));
	EXPECT_PRED2(match, pattern1, tree1);
	EXPECT_PRED2(match, pattern2, tree1);
}

TEST(IRPattern, ifStmt) {
	NodeManager manager;
	auto at = [&manager](string str) { return irp::atom(manager, str); };
	auto ps = [&manager](string str) { return parse::parseStatement(manager, str); };

	StatementPtr stmt1 = ps("if(0) { return 0; } else { return (1+2); }");
	StatementPtr stmt2 = ps("if(0) { return 0; }");
	StatementPtr stmt3 = ps("if((1 != 0)) { return 0; }");

	auto tree1 = toTree(stmt1);
	auto tree2 = toTree(stmt2);
	auto tree3 = toTree(stmt3);

	TreePatternPtr pattern1 = irp::ifStmt(any, at("{ return 0; }"), irp::returnStmt(any));
	TreePatternPtr pattern2 = irp::ifStmt(any, irp::returnStmt(any), irp::returnStmt(any));
	TreePatternPtr pattern3 = irp::ifStmt(any, irp::returnStmt(at("1")|at("0")), irp::returnStmt(any));
	TreePatternPtr pattern4 = irp::ifStmt(any, at("{ return 0; }"), any);
	TreePatternPtr pattern5 = irp::ifStmt(any, at("{ return 0; }"), at("{ }"));
	TreePatternPtr pattern6 = irp::ifStmt(at("(1 != 0)"), at("{ return 0; }"), at("{ }"));

	EXPECT_PRED2(match, pattern1, tree1);
	EXPECT_PRED2(match, pattern2, tree1);
	EXPECT_PRED2(match, pattern3, tree1);
	EXPECT_PRED2(match, pattern4, tree1);
	EXPECT_PRED2(notMatch, pattern5, tree1);
	EXPECT_PRED2(notMatch, pattern6, tree1);

	EXPECT_PRED2(notMatch, pattern1, tree2);
	EXPECT_PRED2(notMatch, pattern2, tree2);
	EXPECT_PRED2(notMatch, pattern3, tree2);
	EXPECT_PRED2(match, pattern4, tree2);
	EXPECT_PRED2(match, pattern5, tree2);
	EXPECT_PRED2(notMatch, pattern6, tree1);

	EXPECT_PRED2(notMatch, pattern1, tree3);
	EXPECT_PRED2(notMatch, pattern2, tree3);
	EXPECT_PRED2(notMatch, pattern3, tree3);
	EXPECT_PRED2(match, pattern4, tree3);
	EXPECT_PRED2(match, pattern5, tree3);
	EXPECT_PRED2(match, pattern6, tree3);
}

TEST(IRPattern, forStmt) {
	NodeManager manager;
	auto at = [&manager](string str) { return irp::atom(manager, str); };
	auto ps = [&manager](string str) { return parse::parseStatement(manager, str); };

	StatementPtr stmt1 = ps("for(decl int<4>:i = 30 .. 5 : -5) { decl int<4>:i = 3;}");
	StatementPtr stmt2 = ps("for(decl int<4>:i = 0 .. 10 : 2) { return 0; }");
	StatementPtr stmt3 = ps("for(decl int<4>:i = 0 .. 5 : 1) { 7; 6; continue; 8; }");
	StatementPtr stmt4 = ps("for(decl int<4>:i = 0 .. 2 : 1) { for(decl int<4>:i = 0 .. 2 : 1){ return 0; }; }");

	auto tree1 = toTree(stmt1);
	auto tree2 = toTree(stmt2);
	auto tree3 = toTree(stmt3);
	auto tree4 = toTree(stmt4);

	TreePatternPtr pattern1 = irp::forStmt(var("x"), any, at("5"), at("-5"), irp::declarationStmt(var("x"), at("3")));
	TreePatternPtr pattern2 = irp::forStmt(any, any, at("10"), at("2"), irp::returnStmt(at("0")));
	TreePatternPtr pattern3 = irp::forStmt(any, any, at("5"), at("1"), irp::compoundStmt(*any << irp::continueStmt << any));
	TreePatternPtr pattern4 = irp::forStmt(var("i"), var("x"), var("y"), var("z"), irp::compoundStmt(irp::forStmt(var("i"), var("x"), var("y"), var("z"), irp::compoundStmt(*any)) << *any));

	EXPECT_PRED2(match, pattern1, tree1);
	EXPECT_PRED2(match, pattern2, tree2);
	EXPECT_PRED2(match, pattern3, tree3);
	EXPECT_PRED2(match, pattern4, tree4);
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
//	/*MatchOpt mo = pattern1->match(tree1);
//	if (mo){
//		MatchValue mv = mo->getVarBinding("x");
//		std::cout << mv.getTree()->getAttachedValue<NodePtr>();
//	}*/
//
//	EXPECT_PRED2(match, pattern1, tree1) << *stmt1;
//	EXPECT_PRED2(match, pattern2, tree2) << *stmt2;
//}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme

