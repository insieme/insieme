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

#include <sstream>
#include <stdlib.h>


#include <gtest/gtest.h>

#define BOOST_SPIRIT_DEBUG

#include "insieme/core/identifier.h"
#include "insieme/core/types.h"
#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/ast_builder.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::core::parse;

#ifndef TEST
// avoiding warnings in eclipse, enabling auto completions
#define TEST void fun
#endif

TEST(IRParser, TypeTests) {

	string testStr("testGenType");
	NodeManager manager;
	IRParser parser(manager);
	ASTBuilder builder(manager);

	auto intType = builder.genericType("int", vector<TypePtr>(), toVector(IntTypeParam::getVariableIntParam('a')));
	EXPECT_EQ(intType, parser.parseType("int<#a>"));
	EXPECT_EQ(intType, parser.parseType("(|int<#a>|)"));

	auto intPairType = builder.tupleType(toVector<TypePtr>(intType, intType));
	EXPECT_EQ(intPairType, parser.parseType("(int<#a>, int<#a>)"));

	auto funType = builder.functionType(intPairType->getElementTypes(), intType);
	EXPECT_EQ(funType, parser.parseType("(int<#a>, int<#a>) -> int<#a>"));

	auto captureFunType = builder.functionType(intPairType->getElementTypes(), toVector<TypePtr>(intType, intType, intType), intType);
	EXPECT_EQ(captureFunType, parser.parseType("[int<#a>, int<#a>](int<#a>, int<#a>, int<#a>) -> int<#a>"));

	auto arrayType = builder.arrayType(intType);
	EXPECT_EQ(arrayType, parser.parseType("array<int<#a>, 1>"));

	auto vectorType = builder.vectorType(intType, IntTypeParam::getConcreteIntParam(10));
	EXPECT_EQ(vectorType, parser.parseType("vector<int<#a>, 10>"));

	auto refType = builder.refType(intType);
	EXPECT_EQ(refType, parser.parseType("ref<int<#a>>"));

	auto multiParamType = builder.genericType("multi", toVector<TypePtr>(builder.typeVariable("tvar"), intType, intPairType), 
		toVector(IntTypeParam::getConcreteIntParam(2), IntTypeParam::getInfiniteIntParam(), IntTypeParam::getVariableIntParam('v')));
	EXPECT_EQ(multiParamType, parser.parseType("multi<'tvar,int<#a>,(int<#a>,int<#a>),2,#inf,#v>"));

	EXPECT_THROW(parser.parseType("fail1<#1,'alpha>"), ParseException);
	EXPECT_THROW(parser.parseType("(fail2"), ParseException);
	EXPECT_THROW(parser.parseType("fail3)"), ParseException);
	EXPECT_THROW(parser.parseType("int -> bool"), ParseException);


	// vector parameter test
	{
		TypeVariablePtr var = builder.typeVariable("a");
		TypePtr vector = builder.vectorType(var, IntTypeParam::getVariableIntParam('l'));

		auto funType = builder.functionType(TypeList(), toVector<TypePtr>(vector, var), var);
		EXPECT_EQ(funType, parser.parseType("(vector<'a,#l>, 'a)->'a"));

		EXPECT_EQ(NT_VectorType, static_pointer_cast<const FunctionType>(parser.parseType("(vector<'a,#l>, 'a)->'a"))->getArgumentTypes()[0]->getNodeType());
	}
}

TEST(IRParser, ExpressionTests) {

	string testStr("testGenType");
	NodeManager manager;
	IRParser parser(manager);
	ASTBuilder builder(manager);

	// literal
	EXPECT_EQ(builder.intLit(455), parser.parseExpression("lit<int<4>, 455>"));
	EXPECT_EQ(builder.uintLit(7), parser.parseExpression("lit<uint<4>, 7>"));
	
	// variable
	VariablePtr v = dynamic_pointer_cast<const Variable>(parser.parseExpression("int<4>:var")); 
	EXPECT_TRUE(!!v && v->getType() == manager.basic.getInt4());
	EXPECT_EQ(builder.castExpr(manager.basic.getUInt4(), builder.intLit(5)), parser.parseExpression("CAST<uint<4>>(lit<int<4>,5>)"));

	// merge all
	auto mergeAll = manager.basic.getLiteral("mergeAll");
	EXPECT_EQ(mergeAll, parser.parseExpression("op<mergeAll>"));
//    EXPECT_EQ(builder.callExpr(mergeAll), parser.parseExpression("(op<mergeAll>())"));

    // lambda using definition
// TODO add statement to test once it is there
    auto lambda = dynamic_pointer_cast<const LambdaExpr>( parser.parseExpression(
        "fun [uint<2>, real<4>](real<8>)->int<4>:lambda in { [uint<2>, real<4>](real<8>)->int<4>:lambda = [uint<2>:c1, real<4>:c2](real<8>:p)->int<4>{} }"));
    EXPECT_TRUE(lambda != 0);
    EXPECT_EQ( lambda->getCaptureList().size(), 2u );
    EXPECT_EQ( lambda->getParameterList().size(), 1u);
    EXPECT_FALSE( lambda->isRecursive() );
    EXPECT_TRUE( lambda->getLambda()->isCapturing() );
    EXPECT_EQ( lambda->getLambda()->getType(), builder.functionType(toVector(manager.basic.getUInt2(), manager.basic.getFloat()),
            toVector(manager.basic.getDouble()), manager.basic.getInt4()) );

//TODO add nicer test for captureInitExpr
    auto captureInit = dynamic_pointer_cast<const CaptureInitExpr>(parser.parseExpression("# uint<2>:a, real<4>:b # fun [uint<2>, real<4>](real<8>)->int<4>:\
            lambda in { [uint<2>, real<4>](real<8>)->int<4>:lambda = [uint<2>:c1, real<4>:c2](real<8>:p)->int<4>{} }"));
    EXPECT_TRUE(captureInit != 0);

	// jobExpr
	// needs something appropriate as argument
//	auto parsedJob = parser.parseExpression("job< 0 >[]{default: 1}");
//	std::cout << "JOB: " << parsedJob << std::endl;

	// tupleExpr
    std::vector<ExpressionPtr> exprVec;
    exprVec.push_back(v);
// TODO add another expression
//    exprVec.push_back(callExpr);
	auto builtTuple = builder.tupleExpr(exprVec);
	auto parsedTuple = parser.parseExpression("tuple[int<4>:var]");
	EXPECT_TRUE(!!builtTuple && !!parsedTuple && builtTuple->getType() == parsedTuple->getType());

	// vectorExpr
	auto vectorExpr = builder.vectorExpr(toVector<ExpressionPtr>(builder.intLit(0), builder.intLit(3)));
    EXPECT_EQ(vectorExpr, parser.parseExpression("vector<int<4>,2>(0, lit<int<4>, 3>)"));

    // structExpr
    Identifier first("first");
    Identifier second("second");
    std::pair<Identifier, ExpressionPtr> elem1 = std::make_pair( first, builder.literal("F", manager.basic.getChar()));
    std::pair<Identifier, ExpressionPtr> elem2 = std::make_pair( second, builder.literal("1", manager.basic.getInt4()));
    auto structExpr = builder.structExpr(toVector(elem1, elem2));
    EXPECT_EQ( structExpr, parser.parseExpression("struct{first:lit<char, F>, second: 1}"));

    // unionExpr
    std::pair<Identifier, TypePtr> elem3 = std::make_pair( first, manager.basic.getChar());
    std::pair<Identifier, TypePtr> elem4 = std::make_pair( second, manager.basic.getInt4());

    auto unionExpr = builder.unionExpr(builder.unionType(toVector(elem3, elem4)), first, builder.literal("1", manager.basic.getChar()));
    EXPECT_EQ( unionExpr, parser.parseExpression("union< union< first:char, second:int<4> > >{ first:'1' }"));

    // memberAccessExpr
    auto memberAccessExpr = builder.memberAccessExpr(structExpr, first);
    EXPECT_EQ(memberAccessExpr, parser.parseExpression("(struct{first:lit<char, F>, second: 1}).first"));

    // tupleProjectionExpr
    auto builtTupleProjectionExpr = dynamic_pointer_cast<const TupleProjectionExpr>(builder.tupleProjectionExpr(builtTuple, 0));
    auto parsedTupleProjectionExpr = dynamic_pointer_cast<const TupleProjectionExpr>(parser.parseExpression("(tuple[int<4>:v]).0"));
    EXPECT_TRUE(!!builtTupleProjectionExpr && !! parsedTupleProjectionExpr);
    EXPECT_EQ(builtTupleProjectionExpr->getIndex(), parsedTupleProjectionExpr->getIndex());
    EXPECT_EQ(builtTupleProjectionExpr->getSubExpression()->getType(), parsedTupleProjectionExpr->getSubExpression()->getType());

    auto markerExpr = builder.markerExpr(builder.intLit(42), 42);
    EXPECT_EQ(markerExpr, parser.parseExpression("<me id = 42> 42 </me>"));
}

TEST(IRParser, StatementTests) {
    NodeManager manager;
    IRParser parser(manager);
    ASTBuilder builder(manager);

    // break statement
    EXPECT_EQ(builder.breakStmt(), parser.parseStatement("break"));

    // continue statement
    EXPECT_EQ(builder.continueStmt(), parser.parseStatement("continue"));

    // return statement
    EXPECT_EQ(builder.returnStmt(builder.intLit(-1)), parser.parseStatement("return -1"));

    // declaration statement
    auto builtDeclarationStmt = dynamic_pointer_cast<const DeclarationStmt>
        (builder.declarationStmt(builder.variable(manager.basic.getInt8()), builder.literal("42", manager.basic.getInt8())));
    auto parsedDeclarationStmt = dynamic_pointer_cast<const DeclarationStmt>(parser.parseStatement("decl int<8>:var = 42"));
    EXPECT_EQ(builtDeclarationStmt->getVariable()->getType(), parsedDeclarationStmt->getVariable()->getType());
    EXPECT_EQ(builtDeclarationStmt->getInitialization(), parsedDeclarationStmt->getInitialization());

    // compound statement
    vector<StatementPtr> stmts;
    // empty comound statement
    EXPECT_EQ(builder.compoundStmt(stmts), parser.parseStatement("{}"));
    stmts.push_back(builder.intLit(7));
    stmts.push_back(builder.breakStmt());
    stmts.push_back(builder.returnStmt(builder.intLit(0)));
    auto compoundStmt = builder.compoundStmt(stmts); // CAUTION! will be reused later
    EXPECT_EQ(compoundStmt, parser.parseStatement("{ 7; break; return 0 }"));

    // while statement
    auto whileStmt = builder.whileStmt(builder.intLit(1), compoundStmt);
    EXPECT_EQ(whileStmt, parser.parseStatement("while(1){ 7; break; return 0 }"));

    // for statement
    auto forStmt = builder.forStmt(builtDeclarationStmt, compoundStmt, builder.intLit(7), builder.intLit(-1));
    EXPECT_EQ(forStmt, parser.parseStatement("for(i = 42 .. 7 : -1) { 7; break; return 0 }" ));
}

TEST(IRParser, InteractiveTest) {

	string testStr("testGenType");
	if(getenv("IR_PARSE_STR")) testStr = string(getenv("IR_PARSE_STR"));
	NodeManager nm;
	try {
		TypePtr t = parseType(nm, testStr);
		std::cout << "--------------------------------------\n Parsing succeeded, "
			<< "result: \n" << t << std::endl << "--------------------------------------\n";
	} catch(ParseException e) {
		std::cout << "--------------------------------------\n Parsing failed \n"
			<< std::endl << "--------------------------------------\n";
	}
}
