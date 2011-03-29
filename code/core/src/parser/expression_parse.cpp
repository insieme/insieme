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

#include "insieme/core/parser/expression_parse_part.h"
#include "insieme/core/parser/expression_parse.h"
#include "insieme/core/parser/statement_parse.h"
#include "insieme/core/parser/operator_parse.h"
#include "insieme/core/parser/type_parse.h"
#include "insieme/core/expressions.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ast_builder.h"

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_ascii.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

// ----------------------- SPIRIT QI/PHOENIX survival hints
// What to do if
// - error: invalid initialization ... --> check whether ph::ref is used to pass references
//                                     --> check if you're using the right placeholders (char_ counts as auto attrib, literals and plain characters do NOT)
// - error: template substitution failure --> check potential ambiguities, try supplying default parameters explicitly
// - error: some operator can not be applied (eg =) --> attribute type may be different from expected
// - error: invalid use of void expression --> trying to do a ph::ref of a reference? Placeholders are already references!
// - error: <some callable> is not a class, struct or union in ph::bind --> check argument count and type
// - other: use phoenix::bind and phoenix::construct instead of plain calls
// ----------------------- - Peter
// - error: has no member named ‘parse’ --> check your operators (>>!)
// - error: is not a class, struct, or union type --> chek function name and argument list

namespace insieme {
namespace core {
namespace parse {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace ph = boost::phoenix;


LiteralPtr buildDoubleLiteral(NodeManager& nodeMan, int integer, vector<char> fraction) {
    ASTBuilder build(nodeMan);


    return build.literal(nodeMan.basic.getDouble(), toString(integer) + "." + toString(join("", fraction)));
}

LiteralPtr buildIntLiteral(NodeManager& nodeMan, int val) {
    ASTBuilder build(nodeMan);
    return build.intLit(val);
}

LiteralPtr buildTrueLiteral(NodeManager& nodeMan) {
    ASTBuilder build(nodeMan);

    return build.literal("true", nodeMan.basic.getBool());
}

LiteralPtr buildFalseLiteral(NodeManager& nodeMan) {
    ASTBuilder build(nodeMan);

    return build.literal("false", nodeMan.basic.getBool());
}


CallExprPtr buildCallExpr(NodeManager& nodeMan, const ExpressionPtr& callee, ExpressionList arguments) {
    //TODO determine return type by inference (arguments may be empty!)
    ASTBuilder build(nodeMan);
    return build.callExpr(callee, arguments);
}


LambdaPtr lambdaGetHelper(NodeManager& nodeMan, const TypePtr& retType, const VariableList& captureList, const VariableList& params, const StatementPtr& body ) {
    // build a stmtExpr bc the builder cannot at the moment
    ASTBuilder build(nodeMan);
//    vector<StatementPtr> stmts;
    vector<TypePtr> captureTypes;
    vector<TypePtr> paramTypes;

    // construct function type
    for_each(captureList, [&](const VariablePtr var) {
        captureTypes.push_back(var->getType());
    });
    for_each(params, [&](const VariablePtr var) {
        paramTypes.push_back(var->getType());
    });

//    return Lambda::get(nodeMan, retType, captureList, params, build.compoundStmt(stmts));
    return build.lambda(build.functionType(captureTypes, paramTypes, retType), captureList, params, body);
}

JobExprPtr jobHelp(NodeManager& manager, const ExpressionPtr& threadNumRange, const ExpressionPtr& defaultStmt,
        const GuardedStmts guardedStmts, const vector<DeclarationStmtPtr>& localDecls) {

    if(!dynamic_pointer_cast<const LambdaExpr>(defaultStmt) && !dynamic_pointer_cast<const CaptureInitExpr>(defaultStmt)) {
        throw ParseException();
    }

    for_each(guardedStmts, [&](std::pair<ExpressionPtr, ExpressionPtr> guardedStmt) {
        //TODO add check for guard
        if(!dynamic_pointer_cast<const LambdaExpr>(guardedStmt.second) && !dynamic_pointer_cast<const CaptureInitExpr>(guardedStmt.second))
            throw ParseException();
    });

    return JobExpr::get(manager, threadNumRange, defaultStmt, guardedStmts, localDecls);
}


void callDepthCheck(bool reset, unsigned& callDepthCount) {
    if(!reset) {
        if(callDepthCount > 1000) throw ParseException();
    } else {
        callDepthCount = 0;
    }
}


string buildString(string s) {
    return s;
}

ExpressionGrammar::ExpressionGrammar(NodeManager& nodeMan, StatementGrammar* stmtGrammar)
    : ExpressionGrammar::base_type(expressionRule), typeG(new TypeGrammar(nodeMan)), varTab(nodeMan) {

 //   typeG = new TypeGrammar(nodeMan);
    exprGpart = new ExpressionGrammarPart(nodeMan, this, typeG);
    opG = new OperatorGrammar(nodeMan, this);
    if(stmtGrammar == NULL) {
        stmtG = new StatementGrammar(nodeMan, this, typeG);
        deleteStmtG = true;
    }
    else {
        stmtG = stmtGrammar;
        deleteStmtG = false;
    }

    auto nManRef = ph::ref(nodeMan);
    auto basicRef = ph::ref(nodeMan.basic);

    auto intTypeRef = ph::cref(nodeMan.basic.getInt4());

    // RULES ---------------------------------------------------- | ACTIONS ----------------------------------------------------------------------------------

    // terminals, no skip parser
    literalString = exprGpart->literalString;
//        *(qi::char_ - ">");

    // nonterminals, skip parser

    //// Let me tell you a little story about the folly of C++ compilers:
    //// Since there are 2 different Literal::get methods with the same number of parameters, when you directly
    //// pass it to phoenix::bind, the compiler is too stupid to figure out which one you mean. By creating this
    //// method pointer and using it instead, we help the compiler figure out which one to choose.
    //void (Literal::*get)(NodeManager&, const TypePtr&, const string&) = &Literal::get;
    // Fixed by adding parserGet to Literal, TODO should probably just remove one of the gets in Literal

    // literals --------------------------------------------------------------------------------------------------------------------------
    charLiteral = exprGpart->charLiteral;
//        ( qi::lit("'") >> qi::char_ >> "'")                         [ qi::_val = ph::bind(&buildCharLiteral, nManRef, qi::_1) ];

    // -----------------------------------------------------------------------------------------------------------------------------------

    literalExpr = exprGpart->literalExpr;
//        ( qi::lit("lit<") >> typeG->typeRule >> ','
//        >> literalString >> '>' )                                   [ qi::_val = ph::bind(&Literal::parserGet, nManRef, qi::_1, qi::_2) ];

    opExpr = exprGpart->opExpr;
//        ( qi::lit("op<") >> literalString >> '>' )                  [ qi::_val = ph::bind(&lang::BasicGenerator::getLiteral, basicRef, qi::_1) ];

    variableExpr = exprGpart->variableExpr;
//        ( typeG->typeRule >> ':' >> typeG->identifier )             [ qi::_val = ph::bind(&VariableTable::get, &varTab, qi::_1, qi::_2) ]
//        | typeG->identifier                                         [ qi::_val = ph::bind(&VariableTable::lookup, &varTab, qi::_1) ];

    // specialized type to ensure type safety
    funVarExpr = exprGpart->funVarExpr;
//        ( typeG->functionType >> ':' >> typeG->identifier )         [ qi::_val = ph::bind(&VariableTable::get, &varTab, qi::_1, qi::_2) ];

    callExpr =
        ( '('  >> /*( variableExpr                                             [ qi::_a = qi::_1 ]
          | literalExpr                                             [ qi::_a = qi::_1 ]
          | opExpr                                                  [ qi::_a = qi::_1 ]
          | captureInitExpr                                         [ qi::_a = qi::_1 ]
          | castExpr                                                [ qi::_a = qi::_1 ]
          | callExpr                                                [ qi::_a = qi::_1 ]
         )*/
         expressionRule >> '(' >> -(expressionRule                  [ ph::push_back(qi::_b, qi::_1) ]
          % ',') >> ')' >> ')')                                     [ qi::_val = ph::bind(&buildCallExpr, nManRef, qi::_1, qi::_b) ];

    castExpr = exprGpart->castExpr;
//        ( qi::lit("CAST<") >> typeG->typeRule
//        >> '>' >> '(' >> expressionRule >> ')' )                    [ qi::_val = ph::bind(&CastExpr::get, nManRef, qi::_1, qi::_2) ];

    // --------------------------------------------------------------------------------------

    lambda = //TODO remove capture init list
        ( -(qi::lit("[") >> (variableExpr                           [ ph::push_back(qi::_a, qi::_1) ]
          % ',') >> ']') >> '(' >> -(variableExpr                   [ ph::push_back(qi::_b, qi::_1) ]
          % ',') >> ')' >> qi::lit("->")
        >> typeG->typeRule >> '{' >> stmtG->statementRule
        >> '}')                                                     [ qi::_val = ph::bind(&lambdaGetHelper, nManRef, qi::_3, qi::_a, qi::_b, qi::_4 )];

    lambdaDef =
        ( qi::lit("{") >> +((funVarExpr >> '=' >> lambda)           [ ph::insert(qi::_a, ph::bind(&makePair<VariablePtr, LambdaPtr>, qi::_1, qi::_2)) ]
          % ',') >> '}')                                            [ qi::_val = ph::bind(&LambdaDefinition::get, nManRef, qi::_a) ];

    lambdaExpr =
        ( qi::lit("fun")  >> funVarExpr >> qi::lit("in") >>
        lambdaDef)                                                  [ qi::_val = ph::bind(&LambdaExpr::get, nManRef, qi::_1, qi::_2) ]
        | ( qi::lit("fun")  >> lambda )                             [ qi::_val = ph::bind(&LambdaExpr::get, nManRef, qi::_1 ) ];

    bindExpr = exprGpart->bindExpr;

    jobExpr =
        ( qi::lit("job<") >> expressionRule >> '>' >> '['
        >> -(stmtG->declarationStmt                                 [ ph::push_back(qi::_a, qi::_1) ]
          % ',') >> ']' >> '{'
        >> *( qi::lit("if") >> expressionRule >> qi::lit("do")
          >> expressionRule )                                       [ ph::push_back(qi::_b, ph::bind(&makePair<ExpressionPtr, ExpressionPtr>, qi::_1, qi::_2)) ]
        >> qi::lit("default:") >> expressionRule >> '}' )           [ qi::_val = ph::bind(&jobHelp, nManRef, qi::_1, qi::_4, qi::_b, qi::_a ) ];

    tupleExpr = exprGpart->tupleExpr;
//        ( qi::lit("tuple[") >> -(expressionRule                     [ ph::push_back(qi::_a, qi::_1) ]
//          % ',') >> ']' )                                           [ qi::_val = ph::bind(&TupleExpr::get, nManRef, qi::_a) ];

    vectorExpr = exprGpart->vectorExpr;
//        ( qi::lit("vector") >> '<' >>
//        (typeG->typeRule >> ',' >> typeG->intTypeParam)             [ qi::_a = ph::bind(&VectorType::get, nManRef, qi::_1, qi::_2 ) ]
//        >> '>' >> '(' >> -(expressionRule                           [ ph::push_back(qi::_b, qi::_1) ]
//          % ',') >> ')' )                                           [ qi::_val = ph::bind(&VectorExpr::get, nManRef, qi::_a, qi::_b ) ];

    structExpr = exprGpart->structExpr;
//        ( qi::lit("struct") >> '{' >> -(
//          (typeG->identifier >> ':' >> expressionRule )             [ ph::push_back(qi::_a, ph::bind(&makePair<IdentifierPtr, ExpressionPtr>, qi::_1, qi::_2 )) ]
//          % ',') >> '}' )                                           [ qi::_val = ph::bind(&StructExpr::get, nManRef, qi::_a ) ];

    unionExpr = exprGpart->unionExpr;
//        ( qi::lit("union") >> '<' >> typeG->typeRule >> '>'
//        >> '{' >> typeG->identifier >> ':'
//        >> expressionRule >> '}' )                                  [ qi::_val = ph::bind(&UnionExpr::get, nManRef, qi::_1, qi::_2, qi::_3) ];

    memberAccessExpr = exprGpart->memberAccessExpr;
//        ( '(' >> expressionRule >> qi::lit(").")
//        >> typeG->identifier )                                      [ qi::_val = ph::bind(&MemberAccessExpr::get, nManRef, qi::_1, qi::_2) ];

    tupleProjectionExpr = exprGpart->tupleProjectionExpr;
//        ( '(' >> expressionRule >> qi::lit(").")
//        >> qi::ulong_long )                                         [ qi::_val = ph::bind(&TupleProjectionExpr::get, nManRef, qi::_1, qi::_2) ];

    markerExpr = exprGpart->markerExpr;
//        ( '<' >> qi::lit("me") >> qi::lit("id") >> '='
//        >> qi::ulong_long >> '>' >> expressionRule >> '<'
//        >> '/' >> qi::lit("me") >> '>')                             [ qi::_val = ph::bind(&MarkerExpr::get, nManRef, qi::_2, qi::_1) ];

    doubleExpr =
        ( qi::int_ >> '.' >> +(qi::char_('0')|qi::char_('1')
          |qi::char_('2')|qi::char_('3')|qi::char_('4')
          |qi::char_('5')|qi::char_('6')|qi::char_('7')
          |qi::char_('8')|qi::char_('9')) )                         [ qi::_val = ph::bind(&buildDoubleLiteral, nManRef, qi::_1, qi::_2) ];

    boolExpr =
        qi::lit("true")                                             [ qi::_val = ph::bind(&buildTrueLiteral, nManRef) ]
        | qi::lit("false")                                          [ qi::_val = ph::bind(&buildFalseLiteral, nManRef) ];
    // --------------------------------------------------------------------------------------



    expressionRule =
        lambdaExpr                                                  [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | bindExpr                                                    [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | boolExpr                                                    [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | callExpr                                                    [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | literalExpr                                                 [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | opExpr                                                      [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | castExpr                                                    [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | jobExpr                                                     [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | tupleExpr                                                   [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | vectorExpr                                                  [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | structExpr                                                  [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | unionExpr                                                   [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | charLiteral                                                 [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | memberAccessExpr                                            [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | tupleProjectionExpr                                         [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | markerExpr                                                  [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | variableExpr                                                [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | doubleExpr                                                  [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
//      | qi::bool_                                                   [ qi::_val = ph::bind(&builtBoolLiteral, nManRef, qi::_1) ]
      | opG->operatorRule                                           [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
      | qi::int_                                                    [ qi::_val = ph::bind(&buildIntLiteral, nManRef, qi::_1) ];
//      | qi::double_                                                 [ qi::_val = ph::bind(&buildDoubleLiteral, nManRef, qi::_1) ];

    // --------------------------------------------------------------------------------------
/*    BOOST_SPIRIT_DEBUG_NODE(literalString);
    BOOST_SPIRIT_DEBUG_NODE(charLiteral);
    BOOST_SPIRIT_DEBUG_NODE(lambda);
    BOOST_SPIRIT_DEBUG_NODE(captureInitExpr);
    BOOST_SPIRIT_DEBUG_NODE(lambdaDef);
    BOOST_SPIRIT_DEBUG_NODE(lambdaExpr);
    BOOST_SPIRIT_DEBUG_NODE(jobExpr);
    BOOST_SPIRIT_DEBUG_NODE(tupleExpr);
    BOOST_SPIRIT_DEBUG_NODE(vectorExpr);
    BOOST_SPIRIT_DEBUG_NODE(structExpr);
    BOOST_SPIRIT_DEBUG_NODE(unionExpr);
    BOOST_SPIRIT_DEBUG_NODE(memberAccessExpr);
    BOOST_SPIRIT_DEBUG_NODE(tupleProjectionExpr);
    BOOST_SPIRIT_DEBUG_NODE(markerExpr);
    // --------------------------------------------------------------------------------------
    BOOST_SPIRIT_DEBUG_NODE(literalExpr);
    BOOST_SPIRIT_DEBUG_NODE(opExpr);
    BOOST_SPIRIT_DEBUG_NODE(variableExpr);
    BOOST_SPIRIT_DEBUG_NODE(callExpr);
    BOOST_SPIRIT_DEBUG_NODE(castExpr);
    BOOST_SPIRIT_DEBUG_NODE(expressionRule);
    BOOST_SPIRIT_DEBUG_NODE(doubleExpr);*/
}

ExpressionGrammar::~ExpressionGrammar() {
    delete typeG;
    delete exprGpart;
    delete opG;
    if(deleteStmtG)
        delete stmtG;
}

} // namespace parse 
} // namespace core
} // namespace insieme

