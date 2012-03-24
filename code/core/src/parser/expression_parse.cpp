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
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"

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
// - error: is not a class, struct, or union type --> check function name and argument list

namespace insieme {
namespace core {
namespace parse {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace ph = boost::phoenix;

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr>::
        doubleLiteralHelp(const int integer, const vector<char>& fraction) {
    IRBuilder build(nodeMan);

    return build.literal(nodeMan.getLangBasic().getDouble(), build.stringValue(toString(integer) + "." + toString(join("", fraction))));
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr>::
        intLiteralHelp(const int val) {
    IRBuilder build(nodeMan);
    return build.intLit(val);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr>::
        boolLiteralHelp(const bool flag) {
    IRBuilder build(nodeMan);

    if(flag)
        return build.literal(nodeMan.getLangBasic().getBool(), "true");

    return build.literal(nodeMan.getLangBasic().getBool(), "false");
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr>::
        callExprHelp(const ExpressionPtr& callee, vector<ExpressionPtr>& arguments) {
    //TODO determine return type by inference (arguments may be empty!)
    IRBuilder build(nodeMan);
    return build.callExpr(callee, arguments);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr, class LambdaPtr, class LambdaDefinitionPtr>
LambdaPtr ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr>::
        lambdaHelp(const TypePtr& retType, const vector<ExpressionPtr>& paramsExpr, const StatementPtr& body) {
    // build a stmtExpr bc the builder cannot at the moment
    IRBuilder build(nodeMan);
    vector<VariablePtr> params;
    vector<TypePtr> paramTypes;

    // TODO make cast faster
    // construct function type
    for_each(paramsExpr, [&](const ExpressionPtr paramExpr) {
        if(VariablePtr var = dynamic_pointer_cast<const Variable>(paramExpr)) {
            paramTypes.push_back(var->getType());
            params.push_back(var);
        } else
			throw SemanticException("Invalid element in argument list of lambda");
    });

//    return Lambda::get(nodeMan, retType, captureList, params, build.compoundStmt(stmts));
    return build.lambda(build.functionType(paramTypes, retType, true), params, body);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr, class LambdaPtr, class LambdaDefinitionPtr>
LambdaDefinitionPtr ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr>::
        lambdaDefHelp(const vector<ExpressionPtr>& funVarExpr, const vector<LambdaPtr>& lambdaExpr ) {
    //TODO make conversion faster
	vector<LambdaBindingPtr> defs;

    if(funVarExpr.size() != lambdaExpr.size())
		throw SemanticException("LambdaDefinition has illegal form\n");

    auto J = lambdaExpr.begin();
    for(auto I = funVarExpr.begin(); I != funVarExpr.end(); ++I, ++J) {
        if(VariablePtr def = dynamic_pointer_cast<const Variable>(*I) )
        	defs.push_back(LambdaBinding::get(nodeMan, def, *J));
        else
			throw ParseException("LambdaDefinitions must be of form '{' funVarExpr '=' lambda '}'");
    }

    return LambdaDefinition::get(nodeMan, defs);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr>::
        lambdaExprHelp(const ExpressionPtr& variableExpr, const LambdaDefinitionPtr& def) {
    if(VariablePtr variable = dynamic_pointer_cast<const Variable>(variableExpr)) {
        return LambdaExpr::get(nodeMan, variable, def);
    }
    else
        throw ParseException("LambdaExpr must be of type 'fun in' LambdaDefinition OR 'fun' Lambda");
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr>::
        lambdaExprHelp(const LambdaPtr& lambda) {
    return LambdaExpr::get(nodeMan, lambda);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr>::
        jobExprHelp(const ExpressionPtr& threadNumRange, const ExpressionPtr& defaultStmt, const vector<std::pair<ExpressionPtr, ExpressionPtr> >& guardedStmts,
                const vector<StatementPtr>& localDeclStmts) {
    if(!dynamic_pointer_cast<const LambdaExpr>(defaultStmt) && !dynamic_pointer_cast<const BindExpr>(defaultStmt)) {
        throw ParseException("Default expression of job must be a LambdaExpr or BindExpr");
    }

    for_each(guardedStmts, [](std::pair<ExpressionPtr, ExpressionPtr> guardedStmt) {
        //TODO add check for guard
        if(!dynamic_pointer_cast<const LambdaExpr>(guardedStmt.second) && !dynamic_pointer_cast<const BindExpr>(guardedStmt.second))
            throw ParseException();
    });

    vector<DeclarationStmtPtr> localDecls;
    // TODO make cast more efficient
    for_each(localDeclStmts, [&](const StatementPtr& decl) {
        if(DeclarationStmtPtr d = dynamic_pointer_cast<const DeclarationStmt>(decl))
            localDecls.push_back(d);
        else
            throw ParseException("JobExpr has illegal form");
    });

    vector<GuardedExprPtr> guardedExprs;
    IRBuilder builder(nodeMan);
    for_each(guardedStmts, [&](const std::pair<ExpressionPtr, ExpressionPtr>& cur) {
    	LambdaExprPtr guard = dynamic_pointer_cast<LambdaExprPtr>(cur.first);
    	if (!guard) {
    		throw ParseException("JobExpr has illegal guard format!");
    	}
    	guardedExprs.push_back(builder.guardedExpr(guard, cur.second));
    });

    return IRBuilder(nodeMan).jobExpr(threadNumRange, localDecls, guardedExprs, defaultStmt);
}


void callDepthCheck(bool reset, unsigned& callDepthCount) {
    if(!reset) {
		if(callDepthCount > 1000) throw ParseException("Call Depth is too Long");
    } else {
        callDepthCount = 0;
    }
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getLiteralExpr() {
    return exprGpart->literalExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
qi::rule<ParseIt, string()> ExpressionGrammar<T, U, V, W, X, Y, Z>::getLiteralString() {
    return exprGpart->literalString;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getCharLiteral() {
    return exprGpart->charLiteral;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getOpExpr() {
    return exprGpart->opExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getVariableExpr() {
    return exprGpart->variableExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getFunVarExpr() {
    return exprGpart->funVarExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
qi::rule<ParseIt, T(), qi::locals<vector<T> >, qi::space_type> ExpressionGrammar<T, U, V, W, X, Y, Z>::getCallExpr() {
    return ( '('  >> /*( variableExpr                              [ qi::_a = qi::_1 ]
      | literalExpr                                             [ qi::_a = qi::_1 ]
      | opExpr                                                  [ qi::_a = qi::_1 ]
      | captureInitExpr                                         [ qi::_a = qi::_1 ]
      | castExpr                                                [ qi::_a = qi::_1 ]
      | callExpr                                                [ qi::_a = qi::_1 ]
     )*/
     expressionRule >> '(' >> -(expressionRule                  [ ph::push_back(qi::_a, qi::_1) ]
      % ',') >> ')' >> ')')                                     [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::callExprHelp, this,
                                                                      qi::_1, qi::_a) ];

}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getCastExpr() {
    return exprGpart->castExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getBindExpr() {
    return exprGpart->bindExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
qi::rule<ParseIt, LambdaPtr(), qi::locals<vector<T> >, qi::space_type> ExpressionGrammar<T, U, V, W, X, Y, Z>::getLambda() {
    return ( '(' >> -(variableExpr                                  [ ph::push_back(qi::_a, qi::_1) ]
         % ',') >> ')' >> qi::lit("->")
       >> typeG->typeRule >> '{' >> stmtG->statementRule
       >> '}')                                                      [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::lambdaHelp, this,
                                                                        qi::_2, qi::_a, qi::_3 )];

}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
qi::rule<ParseIt, LambdaDefinitionPtr(), qi::locals<vector<ExpressionPtr>, vector<LambdaPtr> >, qi::space_type> ExpressionGrammar<T, U, V, W, X, Y, Z>::
        getLambdaDef() {
    return ( qi::lit("{") >> +((funVarExpr >> '=' >> lambda)        [ ph::push_back(qi::_a, qi::_1), ph::push_back(qi::_b, qi::_2)]
        % ',') >> '}')                                              [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::lambdaDefHelp, this,
                                                                        qi::_a, qi::_b) ];

}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getLambdaExpr() {
    return ( qi::lit("fun")  >> funVarExpr >> qi::lit("in") >>
        lambdaDef)                                                  [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::lambdaExprHelp, this,
                                                                        qi::_1, qi::_2) ]
        | ( qi::lit("fun")  >> lambda )                             [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::lambdaExprHelp, this,
                                                                        qi::_1 ) ];
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
qi::rule<ParseIt, T(), qi::locals<vector<U>, vector<std::pair<T, T> > >, qi::space_type> ExpressionGrammar<T, U, V, W, X, Y, Z>::getJobExpr() {
    return ( qi::lit("job<") >> expressionRule >> '>' >> '['
        >> -(stmtG->declarationStmt                                 [ ph::push_back(qi::_a, qi::_1) ]
          % ',') >> ']' >> '{'
        >> *( qi::lit("if") >> expressionRule >> qi::lit("do")
          >> expressionRule )                                       [ ph::push_back(qi::_b, ph::bind(&makePair<ExpressionPtr, ExpressionPtr>, qi::_1, qi::_2)) ]
        >> qi::lit("default:") >> expressionRule >> '}' )           [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::jobExprHelp, this,
                                                                        qi::_1, qi::_4, qi::_b, qi::_a ) ];

}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getTupleExpr() {
    return exprGpart->tupleExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getVectorExpr() {
    return exprGpart->vectorExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getStructExpr() {
    return exprGpart->structExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getUnionExpr() {
    return exprGpart->unionExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getMemberAccessExpr() {
    return exprGpart->memberAccessExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getTupleProjectionExpr() {
    return exprGpart->tupleProjectionExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getMarkerExpr() {
    return exprGpart->markerExpr;
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getIntExpr() {
    return qi::int_                                                 [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::intLiteralHelp,
            this, qi::_1) ];
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getDoubleExpr() {
    return ( qi::int_ >> '.' >> +(qi::char_('0')|qi::char_('1')
        |qi::char_('2')|qi::char_('3')|qi::char_('4')
        |qi::char_('5')|qi::char_('6')|qi::char_('7')
        |qi::char_('8')|qi::char_('9')) )                           [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::doubleLiteralHelp, this,
                                                                        qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getBoolExpr() {
    return qi::lit("true")                                          [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::boolLiteralHelp, this, true) ]
         | qi::lit("false")                                         [ qi::_val = ph::bind(&ExpressionGrammar<T, U, V, W, X, Y, Z>::boolLiteralHelp, this, false) ];
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
Rule ExpressionGrammar<T, U, V, W, X, Y, Z>::getExpressionRule() {
    return lambdaExpr                                               [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | bindExpr                                                 [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | boolExpr                                                 [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | callExpr                                                 [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | literalExpr                                              [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | opExpr                                                   [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | castExpr                                                 [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | jobExpr                                                  [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | tupleExpr                                                [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | vectorExpr                                               [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | structExpr                                               [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | unionExpr                                                [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | charLiteral                                              [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | memberAccessExpr                                         [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | tupleProjectionExpr                                      [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | markerExpr                                               [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | variableExpr                                             [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | doubleExpr                                               [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | opG->operatorRule                                        [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ]
         | intExpr                                                  [ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ];

}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
ExpressionGrammar<T, U, V, W, X, Y, Z>::ExpressionGrammar(NodeManager& nMan, StatementGrammar<U, T, V, W, X, Y, Z>* stmtGrammar)
    : ExpressionGrammar::base_type(expressionRule), typeG(new TypeGrammar<V, W, X>(nMan)), varTab(nMan), nodeMan(nMan) {

 //   typeG = new TypeGrammar(nodeMan);
    exprGpart = new ExpressionGrammarPart<T, U, V, W, X, Y, Z>(nodeMan, this, typeG);
    opG = new OperatorGrammar<T, U, V, W, X, Y, Z>(nodeMan, this);
    if(stmtGrammar == NULL) {
        stmtG = new StatementGrammar<U>(nodeMan, this, typeG);
        deleteStmtG = true;
    }
    else {
        stmtG = stmtGrammar;
        deleteStmtG = false;
    }

    // RULES ---------------------------------------------------- | ACTIONS ----------------------------------------------------------------------------------

    // terminals, no skip parser
    literalString = exprGpart->literalString;

    // nonterminals, skip parser

    //// Let me tell you a little story about the folly of C++ compilers:
    //// Since there are 2 different Literal::get methods with the same number of parameters, when you directly
    //// pass it to phoenix::bind, the compiler is too stupid to figure out which one you mean. By creating this
    //// method pointer and using it instead, we help the compiler figure out which one to choose.
    //void (Literal::*get)(NodeManager&, const TypePtr&, const string&) = &Literal::get;
    // Fixed by adding parserGet to Literal, TODO should probably just remove one of the gets in Literal

    // literals --------------------------------------------------------------------------------------------------------------------------
    charLiteral = getCharLiteral();

    // -----------------------------------------------------------------------------------------------------------------------------------

    literalExpr = getLiteralExpr();

    opExpr = getOpExpr();

    variableExpr = getVariableExpr();

    // specialized type to ensure type safety
    funVarExpr = getFunVarExpr();

    callExpr = getCallExpr();

    castExpr = getCastExpr();

    // --------------------------------------------------------------------------------------

    lambda = getLambda();

    lambdaDef = getLambdaDef();

    lambdaExpr = getLambdaExpr();

    bindExpr = getBindExpr();

    jobExpr = getJobExpr();

    tupleExpr = getTupleExpr();

    vectorExpr = getVectorExpr();

    structExpr = getStructExpr();

    unionExpr = getUnionExpr();

    memberAccessExpr = getMemberAccessExpr();

    tupleProjectionExpr = getTupleProjectionExpr();

    markerExpr = getMarkerExpr();

    intExpr = getIntExpr();

    doubleExpr = getDoubleExpr();

    boolExpr = getBoolExpr();

    // --------------------------------------------------------------------------------------

    expressionRule = getExpressionRule();

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

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
ExpressionGrammar<T, U, V, W, X, Y, Z>::~ExpressionGrammar() {
    delete typeG;
    delete exprGpart;
    delete opG;
    if(deleteStmtG)
        delete stmtG;
}

// Explicit Template Instantiation
template<>
struct ExpressionGrammar<ExpressionPtr> { };

} // namespace parse 
} // namespace core
} // namespace insieme

