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

#include "insieme/core/ir_types.h"
#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/printer/pretty_printer.h"

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
	IRBuilder builder(manager);

	auto intType = builder.genericType("int", vector<TypePtr>(), toVector<IntTypeParamPtr>(VariableIntTypeParam::get(manager, 'a')));
	EXPECT_EQ(intType, parser.parseType("int<#a>"));
	EXPECT_EQ(intType, parser.parseType("(|int<#a>|)"));

	auto intPairType = builder.tupleType(toVector<TypePtr>(intType, intType));
	EXPECT_EQ(intPairType, parser.parseType("(int<#a>, int<#a>)"));

	auto funType = builder.functionType(intPairType->getElementTypes(), intType);
	EXPECT_EQ(funType, parser.parseType("(int<#a>, int<#a>) -> int<#a>"));
/* not needed any more
	auto captureFunType = builder.functionType(intPairType->getElementTypes(), toVector<TypePtr>(intType, intType, intType), intType);
	EXPECT_EQ(captureFunType, parser.parseType("[int<#a>, int<#a>](int<#a>, int<#a>, int<#a>) -> int<#a>"));
*/
	auto arrayType = builder.arrayType(intType);
	EXPECT_EQ(arrayType, parser.parseType("array<int<#a>, 1>"));

	auto vectorType = builder.vectorType(intType, ConcreteIntTypeParam::get(manager, 10));
	EXPECT_EQ(vectorType, parser.parseType("vector<int<#a>, 10>"));

	auto refType = builder.refType(intType);
	EXPECT_EQ(refType, parser.parseType("ref<int<#a>>"));

	auto multiParamType = builder.genericType("multi", toVector<TypePtr>(builder.typeVariable("tvar"), intType, intPairType), 
		toVector<IntTypeParamPtr>(ConcreteIntTypeParam::get(manager, 2), InfiniteIntTypeParam::get(manager), VariableIntTypeParam::get(manager, 'v')));
	EXPECT_EQ(multiParamType, parser.parseType("multi<'tvar,int<#a>,(int<#a>,int<#a>),2,#inf,#v>"));

	EXPECT_THROW(parser.parseType("fail1<#1,'alpha>"), ParseException);
	EXPECT_THROW(parser.parseType("(fail2"), ParseException);
	EXPECT_THROW(parser.parseType("fail3)"), ParseException);
	EXPECT_THROW(parser.parseType("int -> bool"), ParseException);


	// vector parameter test
	{
		TypeVariablePtr var = builder.typeVariable("a");
		TypePtr vector = builder.vectorType(var, VariableIntTypeParam::get(manager, 'l'));

		auto funType = builder.functionType(toVector<TypePtr>(vector, var), var);
		EXPECT_EQ(funType, parser.parseType("(vector<'a,#l>, 'a)->'a"));

		EXPECT_EQ(NT_VectorType, static_pointer_cast<const FunctionType>(parser.parseType("(vector<'a,#l>, 'a)->'a"))->getParameterTypes()[0]->getNodeType());
	}
}

TEST(IRParser, ExpressionTests) {
	string testStr("testGenType");
	NodeManager manager;
	IRParser parser(manager);
	IRBuilder builder(manager);

	// literal
    EXPECT_EQ(builder.literal("42.7", manager.getLangBasic().getDouble()), parser.parseExpression("42.7"));
    EXPECT_EQ(builder.intLit(455), parser.parseExpression("lit<int<4>, 455>"));
	EXPECT_EQ(builder.uintLit(7), parser.parseExpression("lit<uint<4>, 7>"));

	EXPECT_EQ(builder.getIntParamLiteral(5), parser.parseExpression("lit<intTypeParam<5>, 5>"));

	// variable
	VariablePtr v = dynamic_pointer_cast<const Variable>(parser.parseExpression("int<4>:var"));
	EXPECT_TRUE(!!v && v->getType() == manager.getLangBasic().getInt4());
	EXPECT_EQ(builder.castExpr(manager.getLangBasic().getUInt4(), builder.intLit(5)), parser.parseExpression("CAST<uint<4>>(lit<int<4>,5>)"));

	// merge all
	auto mergeAll = manager.getLangBasic().getLiteral("mergeAll");
	EXPECT_EQ(mergeAll, parser.parseExpression("op<mergeAll>"));
    EXPECT_EQ(builder.callExpr(mergeAll), parser.parseIR("(op<mergeAll>())"));

    // lambda using definition
// TODO add statement to test once it is there
    auto lambda = dynamic_pointer_cast<const LambdaExpr>( parser.parseExpression(
        "fun (real<8>)->int<4>:lambda in { (real<8>)->int<4>:lambda = (real<8>:p)->int<4> {\
            { break; } } }"));
    EXPECT_TRUE(lambda != 0);
    EXPECT_EQ(1u, lambda->getParameterList().size());
    EXPECT_FALSE( lambda->isRecursive() );

//    EXPECT_TRUE( lambda->getLambda()->isCapturing() );
    EXPECT_EQ( lambda->getLambda()->getType(), builder.functionType(//toVector(manager.getLangBasic().getUInt2(), manager.getLangBasic().getFloat()),
            toVector(manager.getLangBasic().getDouble()), manager.getLangBasic().getInt4()) );
    EXPECT_EQ( builder.compoundStmt(builder.breakStmt()), lambda->getBody() );

    // jobExpr
    vector<std::pair<ExpressionPtr, ExpressionPtr> > guardedStmts;
/*
    auto parsedJob = dynamic_pointer_cast<const JobExpr>(parser.parseExpression("job< (op<MinRange>(lit<uint<4>, 2>)) >[decl int<4>:var = 42]{ \
            default: [ uint<2>:a, real<4>:b ] fun [uint<2>, real<4>]()->int<4>:\
            lambda in { [uint<2>, real<4>]()->int<4>:lambda = [uint<2>:c1, real<4>:c2]()->int<4>{ continue } }}"));
    EXPECT_TRUE(parsedJob != 0);
    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getLiteral("MinRange"), builder.uintLit(2)), parsedJob->getThreadNumRange());
*/

    // bindExpr
    auto parsedBind1 = dynamic_pointer_cast<const BindExpr>(parser.parseExpression("bind(uint<8>:unbound){ \
        (op<array.subscript.1D>(array<'a,1>:arr, unbound)) }"));
    EXPECT_EQ(1u, parsedBind1->getParameters().size());
    EXPECT_EQ(manager.getLangBasic().getArraySubscript1D(), parsedBind1->getCall()->getFunctionExpr());

    auto parsedBind2 = dynamic_pointer_cast<const BindExpr>(parser.parseExpression("bind(real<8>:unbound){ (fun (real<8>, int<4>)->int<4>:lambda in { \
            (real<8>, int<4>)->int<4>:lambda = (real<8>:p, int<4>:q)->int<4> {\
            { unbound; return q; } } }(unbound, 0)) }"));
    EXPECT_EQ(1u, parsedBind1->getParameters().size());
    EXPECT_EQ(2u, parsedBind1->getCall()->getArguments().size());

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
    StringValuePtr first = StringValue::get(manager, "first");
    StringValuePtr second = StringValue::get(manager, "second");
    std::pair<StringValuePtr, ExpressionPtr> elem1 = std::make_pair( first, builder.literal("F", manager.getLangBasic().getChar()));
    std::pair<StringValuePtr, ExpressionPtr> elem2 = std::make_pair( second, builder.literal("1", manager.getLangBasic().getInt4()));
    auto structExpr = builder.structExpr(toVector(elem1, elem2));
    EXPECT_EQ( structExpr, parser.parseExpression("struct{first:lit<char, F>, second: 1}"));

    // unionExpr
    std::pair<StringValuePtr, TypePtr> elem3 = std::make_pair( first, manager.getLangBasic().getChar());
    std::pair<StringValuePtr, TypePtr> elem4 = std::make_pair( second, manager.getLangBasic().getInt4());

    auto unionExpr = builder.unionExpr(builder.unionType(toVector(elem3, elem4)), first, builder.literal("1", manager.getLangBasic().getChar()));
    EXPECT_EQ( unionExpr, parser.parseExpression("union< union< first:char, second:int<4> > >{ first:'1' }"));

    auto markerExpr = builder.markerExpr(builder.intLit(42), 42);
    EXPECT_EQ(markerExpr, parser.parseExpression("<me id = 42> 42 </me>"));
}

TEST(IRParser, StatementTests) {
    NodeManager manager;
    IRParser parser(manager);
    IRBuilder builder(manager);

    // break statement
    EXPECT_EQ(builder.breakStmt(), parser.parseStatement("break"));

    // continue statement
    EXPECT_EQ(builder.continueStmt(), parser.parseStatement("continue"));

    // return statement
    EXPECT_EQ(builder.returnStmt(builder.intLit(-1)), parser.parseStatement("return -1"));
    EXPECT_EQ(builder.returnStmt(builder.getLangBasic().getUnitConstant()), parser.parseStatement("return unit"));

    // declaration statement
    auto builtDeclarationStmt = builder.declarationStmt(builder.variable(manager.getLangBasic().getInt4()), builder.literal("42", manager.getLangBasic().getInt4()));
    auto parsedDeclarationStmt = dynamic_pointer_cast<const DeclarationStmt>(parser.parseStatement("decl int<4>:var = 42"));
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
    EXPECT_EQ(compoundStmt, parser.parseStatement("{ 7; break; return 0; }"));

    // variable reuse
    auto dbl = dynamic_pointer_cast<const CompoundStmt>(parser.parseStatement("{real<8>:dbl; dbl;}"));
    EXPECT_TRUE(dbl != 0);
    EXPECT_EQ(2u, dbl->getStatements().size());

    // while statement
    auto whileStmt = builder.whileStmt(builder.intLit(1), compoundStmt);
    EXPECT_EQ(whileStmt, parser.parseStatement("while(1){ 7; break; return 0; }"));

    // for statement
    auto builtForStmt = dynamic_pointer_cast<const ForStmt>(builder.forStmt(builtDeclarationStmt, builder.intLit(7), builder.intLit(-1), compoundStmt));
    auto parsedForStmt = dynamic_pointer_cast<const ForStmt>(parser.parseStatement("for(decl int<4>:i = 42 .. 7 : -1) { 7; break; return 0; }" ));
    EXPECT_TRUE(!!builtForStmt && !!parsedForStmt);
    EXPECT_EQ(builtForStmt->getStep(), parsedForStmt->getStep());
    EXPECT_EQ(builtForStmt->getStart(), parsedForStmt->getStart());
    EXPECT_EQ(builtForStmt->getEnd(), parsedForStmt->getEnd());
    EXPECT_EQ(compoundStmt, parsedForStmt->getBody());

    // if statement
    auto ifStmt = builder.ifStmt(builder.intLit(0), compoundStmt);
    EXPECT_EQ(ifStmt, parser.parseStatement("if(0) { 7; break; return 0; }"));
    ifStmt = builder.ifStmt(builder.intLit(1), builder.returnStmt(builder.intLit(0)), builder.returnStmt(builder.intLit(-1)));
    EXPECT_EQ(ifStmt, parser.parseStatement("if(1) return 0 else return -1"));

    // switch statement
    vector<std::pair<ExpressionPtr, StatementPtr> > cases;
    auto switchStmt = builder.switchStmt(builder.intLit(42), cases);
 //   EXPECT_EQ(switchStmt, parser.parseStatement("switch ( 42 ) {}"));

    cases.push_back(std::make_pair(builder.intLit(0), builder.returnStmt(builder.intLit(0))));
    cases.push_back(std::make_pair(builder.intLit(1), builder.returnStmt(builder.intLit(1))));
    switchStmt = builder.switchStmt(builder.intLit(42), cases);
    EXPECT_EQ(switchStmt, parser.parseStatement("switch(42 ){case 0: return 0 case 1: return 1}"));

    switchStmt = builder.switchStmt(builder.intLit(42), cases, builder.returnStmt(builder.intLit(42)));
    EXPECT_EQ(switchStmt, parser.parseStatement("switch( 42) {case 0: return 0 case 1: return 1 default: return 42}"));

    cases.clear();
    switchStmt = builder.switchStmt(builder.intLit(42), cases, builder.returnStmt(builder.intLit(42)));
    EXPECT_EQ(switchStmt, parser.parseStatement("switch (42){default: return 42}")); // does it make any sense to support this?

    // marker statement
    auto markerStmt = builder.markerStmt(whileStmt, 7);
    EXPECT_EQ(markerStmt, parser.parseStatement("<ms id = 7> while(1){ 7; break; return 0; } </ms>"));

    auto tmp = parser.parseStatement("(op<ref.var>((op<undefined>(lit<type<vector<'res,#l>>, arbitraryText>))))");

//    std::cout << printer::PrettyPrinter(tmp) << std::endl;
    // pointwise operator implementation with simple means
/*    auto vectorPointwise = parser.parseStatement("\
        fun (('elem, 'elem) -> 'res:fct) -> (vector<'elem, #l>, vector<'elem, #l>) -> vector<'res, #l>  { \
            return bind(vector<'elem, #l>:ba, vector<'elem,#l>:bb) { \
               ( fun (vector<'elem, #l>:a, vector<'elem,#l>:b, ('elem, 'elem) -> 'res:fct2) -> vector<'res, #l>{ { \
                   decl ref<vector<'res, #l> >:result = (op<ref.var>((op<undefined>(lit<type<vector<'res, #l> >, arbitraryText>)))); \
                   for(decl int<4>:i = 0 .. (op<int.type.param.to.int>(lit<intTypeParam, l>)) : 1) \
                        (op<ref.assign>((op<array.ref.elem.1D>(result, i)), (fct2((op<vector.subscript>(a, i)), (op<vector.subscript>(b, i))))) ); \
                   return (op<ref.deref>(result)); \
               } } (ba, bb, fct) ) \
            } \
        }");

    auto vectorPointwiseUnary = parser.parseStatement("\
        fun (('elem) -> 'res:fct3) -> (vector<elem, #l>) -> vector<'res, #l> { \
            return bind(vector<'elem, #l>:ba2) { \
                ( fun(vector<'elem, #l>:a2, ('elem) -> 'res:fct4) -> vector<'res, #l>{ { \
                    decl ref<vector<'res, 'l> >:result2 = (op<ref.var>((op<undefined>(lit<type<vector<'res, #l> >, arbitraryText>)))); \
                    for(decl int<4>:i2 = 0 .. (op<int.type.param.to.int>(lit<intTypeParam, l>)) : 1) \
                        (op<ref.assign>((op<array.ref.elem.1D>(result2, i2)), (fct4((op<vector.subscript>(a2, i2)) )))); \
                    return (op<ref.deref>(result2)); \
                } } (ba2, fct3) ) \
            } \
        }");// } ");
*/


try {
    // clCreateBuffer implementation
/*    auto clCreateBuffer = parser.parseStatement("\
        fun (struct<identifier:int<8> >:context, uint<4>:flags, uint<4>:size, ref<array<'a, 1> >:host_ptr, ref<uint<4> >:errorcode_ret )->ref<array<'a, 1> > {{ \
            return (op<ref.new>( (op<array.create.1D>( (op<undefined>( ( op<array.subscript.1D>(host_ptr, 0)) )), \
                (size / (op<sizeof>( ( op<array.subscript.1D>(host_ptr, 0 )) )) ) )) )); \
        }}"
    );
*/
    // clEnqueueWriteBuffer implementation
    auto clEnqueueWriteBuffer = parser.parseStatement("\
        fun (struct<identifier:int<8> >:command_queue, ref<array<'a, 1> >:devicePtr, bool:blocking, uint<4>:offset, uint<4>:cb, array<'a, 1>:hostPtr, \
             uint<4>:nWait, array<struct<identifier:int<8> >, 1>:waitForEvents, struct<identifier:int<8> >:event) -> ref<uint<4> > {{ \
             (op<ref.assign>(devicePtr, hostPtr )); \
             return lit<uint<4>, 0>;\
        }}"
    );

    // clEnqueueReadBuffer implementation
    auto clEnqueueReadBuffer = parser.parseStatement("\
        fun (struct<identifier:int<8> >:command_queue, ref<array<'a, 1> >:devicePtr, bool:blocking, uint<4>:offset, uint<4>:cb, array<'a, 1>:hostPtr, \
             uint<4>:nWait, array<struct<identifier:int<8> >, 1>:waitForEvents, struct<identifier:int<8> >:event) -> ref<uint<4> > {{ \
             (op<ref.assign>(hostPtr, devicePtr )); \
             return lit<uint<4>, 0>;\
        }}"
    );

    // clEnqueueCopyBuffer implementation
    auto clEnqueueCopyBuffer = parser.parseStatement("\
        fun (struct<identifier:int<8> >:command_queue, ref<array<'a, 1> >:src, ref<array<'a, 1> >:dst, uint<4>:srcOffset, uint<4>:dstOffset, uint<4>:cb, \
             uint<4>:nWait, array<struct<identifier:int<8> >, 1>:waitForEvents, struct<identifier:int<8> >:event) -> ref<uint<4> > {{ \
             (op<ref.assign>(dst, (op<ref.deref>(src)) )); \
             return lit<uint<4>, 0>;\
        }}"
    );
} catch(lang::LiteralNotFoundException e) {
    std::cout << e.what() << std::endl;
    throw lang::LiteralNotFoundException("");
}
//    std::cout << printer::PrettyPrinter(vectorPointwise) << std::endl;
}

TEST(IRParser, OperationTests) {
    NodeManager manager;
    IRParser parser(manager);
    IRBuilder builder(manager);

    // binary operations

    auto assignment = dynamic_pointer_cast<const CallExpr>(parser.parseExpression("( ref<uint<4>>:a = 7)"));
    EXPECT_EQ(manager.getLangBasic().getRefAssign(), assignment->getFunctionExpr());
    EXPECT_EQ(builder.refType(manager.getLangBasic().getUInt4()), assignment->getArgument(0)->getType());
    EXPECT_EQ(builder.castExpr(manager.getLangBasic().getUInt4(), builder.intLit(7)), assignment->getArgument(1));

    auto add = dynamic_pointer_cast<const CallExpr>(parser.parseExpression("( ref<int<4>>:a + uint<4>:b)"));
    EXPECT_EQ(manager.getLangBasic().getSignedIntAdd(), add->getFunctionExpr());
    EXPECT_EQ(manager.getLangBasic().getInt4(), add->getArgument(0)->getType());
    EXPECT_EQ(manager.getLangBasic().getInt4(), add->getArgument(1)->getType());

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getInt4(), manager.getLangBasic().getSignedIntAdd(), builder.intLit(0), builder.intLit(1)),
        parser.parseExpression("(0 + 1)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getDouble(), manager.getLangBasic().getRealSub(), builder.literal("42.0", manager.getLangBasic().getDouble()),
        builder.literal("41.0", manager.getLangBasic().getDouble())),
        parser.parseExpression("(42.0 - 41.0)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getUInt4(), manager.getLangBasic().getUnsignedIntMul(),
        builder.literal("7", manager.getLangBasic().getUInt4()), builder.castExpr(manager.getLangBasic().getUInt4(), builder.literal("6", manager.getLangBasic().getInt4()))),
        parser.parseExpression("(lit<uint<4>, 7> * lit<int<4>, 6> )"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getRealDiv(), builder.literal("5", manager.getLangBasic().getFloat()), builder.literal("9.6", manager.getLangBasic().getFloat())),
        parser.parseExpression("(lit<real<4>, 5> / lit<real<4>, 9.6>)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getSignedIntMod(), builder.literal("53452", manager.getLangBasic().getInt2()),
        builder.literal("32", manager.getLangBasic().getInt2())),
        parser.parseExpression("(lit<int<2>, 53452> % lit<int<2>, 32>)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getSignedIntAnd(), builder.literal("255", manager.getLangBasic().getInt8()),
        builder.literal("7", manager.getLangBasic().getInt8())),
        parser.parseExpression("(lit<int<8>, 255> & lit<int<8>, 7>)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getUnsignedIntOr(), builder.literal("255", manager.getLangBasic().getUInt2()),
        builder.literal("169", manager.getLangBasic().getUInt2())),
        parser.parseExpression("(lit<uint<2>, 255> | lit<uint<2>, 169>)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getSignedIntLShift(), builder.literal("3456", manager.getLangBasic().getInt4()),
        builder.literal("1", manager.getLangBasic().getInt4())),
        parser.parseExpression("(lit<int<4>, 3456> << lit<int<4>, 1>)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getUnsignedIntRShift(), builder.literal("546", manager.getLangBasic().getUInt4()),
        builder.literal("8", manager.getLangBasic().getInt4())),
        parser.parseExpression("(lit<uint<4>, 546> >> lit<int<4>, 8>)"));

    // unary operations

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getUInt2(), manager.getLangBasic().getUnsignedIntNot(), builder.literal("231", manager.getLangBasic().getUInt2())),
        parser.parseExpression("( ~ lit<uint<2>, 231>)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getInt4(), manager.getLangBasic().getSignedIntAdd(), builder.literal("0", manager.getLangBasic().getInt4()),
        builder.literal("100", manager.getLangBasic().getInt4())),
        parser.parseExpression("( +100 )"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getInt8(), manager.getLangBasic().getSignedIntSub(), builder.literal("0", manager.getLangBasic().getInt8()),
        builder.literal("100", manager.getLangBasic().getInt8())),
        parser.parseExpression("( -lit<int<8>, 100>)"));

    auto builtPreInc = builder.callExpr(manager.getLangBasic().getUInt8(), manager.getLangBasic().getUnsignedIntPreInc(),
        builder.variable(builder.refType(manager.getLangBasic().getUInt8())));
    auto parsedPreInc = dynamic_pointer_cast<const CallExpr>(parser.parseExpression("( ++ ref<uint<8>>:pri )"));
    EXPECT_EQ(builtPreInc->getFunctionExpr(), parsedPreInc->getFunctionExpr());
    EXPECT_EQ(builtPreInc->getArgument(0)->getType(), parsedPreInc->getArgument(0)->getType());

    auto parsedPostInc = dynamic_pointer_cast<const CallExpr>(parser.parseExpression("( ref<int<4>>:poi++ )"));
    EXPECT_EQ(manager.getLangBasic().getSignedIntPostInc(), parsedPostInc->getFunctionExpr());
    EXPECT_EQ(builder.refType(manager.getLangBasic().getInt4()), parsedPostInc->getArgument(0)->getType());

    auto parsedPreDec = dynamic_pointer_cast<const CallExpr>(parser.parseExpression("( --ref<int<2>>:prd )"));
    EXPECT_EQ(manager.getLangBasic().getSignedIntPreDec(), parsedPreDec->getFunctionExpr());
    EXPECT_EQ(builder.refType(manager.getLangBasic().getInt2()), parsedPreDec->getArgument(0)->getType());

    auto parsedPostDec = dynamic_pointer_cast<const CallExpr>(parser.parseExpression("(ref<uint<16>>:pod -- )"));
    EXPECT_EQ(manager.getLangBasic().getUnsignedIntPostDec(), parsedPostDec->getFunctionExpr());
    EXPECT_EQ(builder.refType(manager.getLangBasic().getUInt16()), parsedPostDec->getArgument(0)->getType());

    // logical operations
    std::vector<VariablePtr> lazyArgs;
    std::vector<TypePtr> argTypes;

    auto builtAnd = builder.callExpr(manager.getLangBasic().getBool(), manager.getLangBasic().getBoolLAnd(), builder.literal("true", manager.getLangBasic().getBool()),
        builder.bindExpr(lazyArgs, builder.callExpr( builder.lambdaExpr(builder.functionType(argTypes, manager.getLangBasic().getBool()), lazyArgs,
                builder.returnStmt(builder.literal("true", manager.getLangBasic().getBool()))) ))  );
    auto parsedAnd = dynamic_pointer_cast<const CallExpr>(parser.parseExpression("(lit<bool, true> && lit<bool, true> )"));
    EXPECT_EQ(builtAnd->getFunctionExpr(), parsedAnd->getFunctionExpr());
    EXPECT_EQ(builtAnd->getArgument(0), parsedAnd->getArgument(0));
    EXPECT_EQ(builtAnd->getArgument(1)->getType(), parsedAnd->getArgument(1)->getType());

    auto parsedOr = dynamic_pointer_cast<const CallExpr>(parser.parseExpression("(lit<bool, false> || lit<bool, true> )"));
    EXPECT_EQ(manager.getLangBasic().getBoolLOr(), parsedOr->getFunctionExpr());
    EXPECT_EQ(builder.literal(manager.getLangBasic().getBool(), "false"), parsedOr->getArgument(0));
    EXPECT_EQ(builtAnd->getArgument(1)->getType(), parsedAnd->getArgument(1)->getType());

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getBoolLNot(), builder.literal("false", manager.getLangBasic().getBool())),
        parser.parseExpression("(! lit<bool, false> )"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getBoolEq(), builder.literal("false", manager.getLangBasic().getBool()), builder.literal("true", manager.getLangBasic().getBool())),
        parser.parseExpression("(false == true )"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getCharNe(), builder.literal("a", manager.getLangBasic().getChar()), builder.literal("b", manager.getLangBasic().getChar())),
        parser.parseExpression("('a' != 'b')"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getUnsignedIntNe(), builder.literal("1", manager.getLangBasic().getUInt4()),
            builder.castExpr(manager.getLangBasic().getUInt4(), builder.literal("0", manager.getLangBasic().getInt4()))),
        parser.parseExpression("(lit<uint<4>, 1> != 0)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getSignedIntLt(), builder.literal("5", manager.getLangBasic().getInt4()), builder.literal("7", manager.getLangBasic().getInt4())),
        parser.parseExpression("(5<7)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getRealLe(), builder.literal("5.3", manager.getLangBasic().getDouble()),
            builder.literal("7.7", manager.getLangBasic().getDouble())),
        parser.parseExpression("(5.3 <= 7.7)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getRealGt(), builder.literal("1234.0", manager.getLangBasic().getFloat()),
            builder.literal("78.9", manager.getLangBasic().getFloat())),
        parser.parseExpression("(lit<real<4>, 1234.0> > lit<real<4>, 78.9>)"));

    EXPECT_EQ(builder.callExpr(manager.getLangBasic().getRealGe(), builder.literal("5.3", manager.getLangBasic().getDouble()),
            builder.castExpr(manager.getLangBasic().getDouble(), builder.literal("3", manager.getLangBasic().getUInt2()))),
        parser.parseExpression("(5.3 >= lit<uint<2>, 3>)"));

}

TEST(IRParser, ProgramTest) {
    NodeManager manager;
    IRParser parser(manager);
    IRBuilder builder(manager);

    // program with main
    ProgramPtr mainProg = parser.parseProgram("main: fun ()->int<4>:\
            mainfct in { ()->int<4>:mainfct = ()->int<4>{ continue } }");

    EXPECT_FALSE(mainProg->hasAnnotations());
    EXPECT_EQ(1u, mainProg->getEntryPoints().size());

    // multiple entry points
    ProgramPtr mep = parser.parseProgram("fun (uint<2>, real<8>)->int<4>:f1 in { (uint<2>, real<8>)->int<4>:f1 = (uint<2>:c1, real<8>:p)->int<4> {\
            { return 0; } } } fun ()->unit:f2 in{()->unit:f2=()->unit {{ break; }}}");

    EXPECT_FALSE(mep->hasAnnotations());
    EXPECT_EQ(2u, mep->getEntryPoints().size());
}

TEST(IRParser, IRTest) {
    NodeManager manager;
    IRParser parser(manager);
    IRBuilder builder(manager);

    // program with main
    ProgramPtr mainProg = static_pointer_cast<const Program>(parser.parseIR("main: fun ()->int<4>:\
         mainfct in { ()->int<4>:mainfct = ()->int<4>{ continue } }"));


    EXPECT_FALSE(mainProg->hasAnnotations());
    EXPECT_EQ(1u, mainProg->getEntryPoints().size());

    // expression
    auto assignment = static_pointer_cast<const CallExpr>(parser.parseIR("( ref<uint<4>>:a = 7)"));
    EXPECT_EQ(manager.getLangBasic().getRefAssign(), assignment->getFunctionExpr());
    EXPECT_EQ(builder.refType(manager.getLangBasic().getUInt4()), assignment->getArgument(0)->getType());
    EXPECT_EQ(builder.castExpr(manager.getLangBasic().getUInt4(), builder.intLit(7)), assignment->getArgument(1));

    // type
	auto intType = builder.genericType("int", vector<TypePtr>(), toVector<IntTypeParamPtr>(VariableIntTypeParam::get(manager, 'a')));
	EXPECT_EQ(intType, static_pointer_cast<const Type>(parser.parseIR("int<#a>")));
	EXPECT_EQ(intType, static_pointer_cast<const Type>(parser.parseIR("(|int<#a>|)")));

	// lambda
	LambdaPtr lambda = dynamic_pointer_cast<const Lambda>(parser.parseIR("(real<8>:p)->int<4> {\
            { break; } }"));
	EXPECT_FALSE(!lambda);
	EXPECT_EQ(builder.functionType(toVector(manager.getLangBasic().getReal8()), manager.getLangBasic().getInt4()), lambda->getType());
	EXPECT_EQ(1u, lambda->getBody()->getChildList().size());
}


TEST(IRParser, InteractiveTest) {

	string testStr("1");
	if(getenv("IR_PARSE_STR")) testStr = string(getenv("IR_PARSE_STR"));
	NodeManager nm;
	try {
		ExpressionPtr t = parseExpression(nm, testStr);
		std::cout << "--------------------------------------\n Parsing succeeded, "
			<< "result: \n" << t << std::endl << "--------------------------------------\n";
	} catch(ParseException e) {
		std::cout << "--------------------------------------\n Parsing failed \n"
			<< std::endl << "--------------------------------------\n";
	}
}

TEST(IRParser, FunctionType) {
    NodeManager manager;
    IRParser parser(manager);
    IRBuilder builder(manager);

    // parse a function plain and a closure type
    TypePtr a = builder.genericType("A");
    TypePtr b = builder.genericType("B");

    FunctionTypePtr funA = builder.functionType(toVector(a), b, true);
    FunctionTypePtr funB = builder.functionType(toVector(a), b, false);

    EXPECT_EQ(funA, parser.parseType("(A) -> B"));
    EXPECT_EQ(funB, parser.parseType("(A) => B"));
}
