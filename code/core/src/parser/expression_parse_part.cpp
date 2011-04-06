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

// workaround for bug: if ExpressionPtr is used as fixed template class, the compiler assumes all ExpressionPtr in the arguments should be of type template T
// in their definition
typedef ExpressionPtr ExprTmplt;


template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::charLiteralHelp(char val) {
    ASTBuilder build(nodeMan);
    return build.literal(nodeMan.basic.getChar(), toString(val));
}

// obsolete
LiteralPtr buildNat(NodeManager& nodeMan, uint64_t val) {
    ASTBuilder build(nodeMan);
    return build.literal(nodeMan.basic.getUInt8(), toString(val));
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::literalHelp(const TypePtr& typeExpr, const string& name) {
    if(TypePtr type = dynamic_pointer_cast<const Type>(typeExpr))
        return Literal::get(nodeMan, type, name);

    throw ParseException("LiteralPtr must be of form 'lit<' TypePtr ',' string '>'");
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::opHelp(const string& name) {

    return nodeMan.basic.getLiteral(name);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::variableHelp(const TypePtr& type, const IdentifierPtr& id) {
    return exprG->varTab.get(type, id);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::variableHelp(const IdentifierPtr& id) {
    return exprG->varTab.lookup(id);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::castHelp(const TypePtr& type, const ExpressionPtr& subExpr) {
    return CastExpr::get(nodeMan, type, subExpr);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::bindExprHelp(const vector<ExpressionPtr>& paramsExpr, ExpressionPtr& callExpr) {
    vector<VariablePtr> params;

    //TODO make cast more efficient
    for_each(paramsExpr, [&](ExpressionPtr paramExpr) {
        if(VariablePtr param = dynamic_pointer_cast<const Variable>(paramExpr))
            params.push_back(param);
        else
            throw ParseException("Arguments of BindExpr must be Variables");
    });


    if(CallExprPtr call = dynamic_pointer_cast<const CallExpr>(callExpr))
        return BindExpr::get(nodeMan, params, call);
    else
        throw ParseException("body of BindExpr must ba a CallExpr");
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::tupleHelp(const vector<ExpressionPtr>& elements) {
    return TupleExpr::get(nodeMan, elements);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::vectorHelp(const TypePtr& generalType, const vector<ExpressionPtr > & elements) {
    if(VectorTypePtr vecType = dynamic_pointer_cast<const VectorType>(generalType))
        return VectorExpr::get(nodeMan, vecType, elements);
    else
        throw ParseException("Type of a vector must be a VectorType");
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::structHelp(const vector<std::pair<IdentifierPtr, ExpressionPtr> >& elements) {
    return StructExpr::get(nodeMan, elements);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::unionHelp(const TypePtr& type, const IdentifierPtr& memberName, const ExpressionPtr& member) {
    return UnionExpr::get(nodeMan, type, memberName, member);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::memberAccessHelp(const ExpressionPtr& subExpr, const IdentifierPtr& member) {
    return MemberAccessExpr::get(nodeMan, subExpr, member);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::tupleProjectionHelp(const ExpressionPtr& subExpr, const unsigned idx) {
    return TupleProjectionExpr::get(nodeMan, subExpr, idx);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
ExpressionPtr ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::markerExprHelp(const ExpressionPtr& subExpr, const unsigned id) {
    return MarkerExpr::get(nodeMan, subExpr, id);
}

//--------------------------------------------------------------------------------------------------------------------------------------------

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getCharLiteral() {
    return ( qi::lit("'") >> qi::char_ >> "'")                      [ qi::_val = ph::bind(&ExpressionGrammarPart::charLiteralHelp, this, qi::_1) ];
}

template<typename T, typename U, typename V, typename W, typename X>
qi::rule<ParseIt, string()> ExpressionGrammarPart<T, U, V, W, X>::getLiteralString() {
    return *(qi::char_ - ">");
}

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getLiteralExpr() {
    return ( qi::lit("op<") >> literalString >> '>' )               [ qi::_val = ph::bind(&ExpressionGrammarPart::opHelp, this, qi::_1) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getOpExpr() {
    return  ( qi::lit("lit<") >> typeG->typeRule >> ','
        >> literalString >> '>' )                                   [ qi::_val = ph::bind(&ExpressionGrammarPart::literalHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getVariableExpr() {
    return ( typeG->typeRule >> ':' >> typeG->identifier )          [ qi::_val = ph::bind(&ExpressionGrammarPart::variableHelp, this, qi::_1, qi::_2) ]
        | typeG->identifier                                         [ qi::_val = ph::bind(&ExpressionGrammarPart::variableHelp, this, qi::_1) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getFunVarExpr() {
    return ( typeG->functionType >> ':' >> typeG->identifier )      [ qi::_val = ph::bind(&ExpressionGrammarPart::variableHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getCastExpr() {
    return ( qi::lit("CAST<") >> typeG->typeRule
        >> '>' >> '(' >> exprG->expressionRule >> ')' )             [ qi::_val = ph::bind(&ExpressionGrammarPart::castHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X>
qi::rule<ParseIt, T(), qi::locals<vector<T> >, qi::space_type> ExpressionGrammarPart<T, U, V, W, X>::getBindExpr() {
    return ( qi::lit("bind") >> '(' >> -( variableExpr              [ ph::push_back(qi::_a, qi::_1) ]
        % ',') >> ')' >> '{' >> exprG->callExpr >> '}' )            [ qi::_val = ph::bind(&ExpressionGrammarPart::bindExprHelp, this, qi::_a, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X>
qi::rule<ParseIt, T(), qi::locals<vector<T> >, qi::space_type> ExpressionGrammarPart<T, U, V, W, X>::getTupleExpr() {
    return ( qi::lit("tuple") >> '[' >> -(exprG->expressionRule     [ ph::push_back(qi::_a, qi::_1) ]
        % ',') >> ']' )                                             [ qi::_val = ph::bind(&ExpressionGrammarPart::tupleHelp, this, qi::_a) ];
}

template<typename T, typename U, typename V, typename W, typename X>
qi::rule<ParseIt, T(), qi::locals<V, vector<T> >, qi::space_type> ExpressionGrammarPart<T, U, V, W, X>::getVectorExpr() {
    auto nManRef = ph::ref(nodeMan);
    return ( qi::lit("vector") >> '<' >>
        (typeG->typeRule >> ',' >> typeG->intTypeParam)             [ qi::_a = ph::bind(&VectorType::get, nManRef, qi::_1, qi::_2 ) ]
        >> '>' >> '(' >> -(exprG->expressionRule                    [ ph::push_back(qi::_b, qi::_1) ]
          % ',') >> ')' )                                           [ qi::_val = ph::bind(&ExpressionGrammarPart::vectorHelp, this, qi::_a, qi::_b ) ];
}

template<typename T, typename U, typename V, typename W, typename X>
qi::rule<ParseIt, T(), qi::locals<vector<std::pair<X, T> > >, qi::space_type> ExpressionGrammarPart<T, U, V, W, X>::getStructExpr() {
    return ( qi::lit("struct") >> '{' >> -(
        (typeG->identifier >> ':' >> exprG->expressionRule )        [ ph::push_back(qi::_a, ph::bind(&makePair<IdentifierPtr, ExpressionPtr>, qi::_1, qi::_2 )) ]
          % ',') >> '}' )                                           [ qi::_val = ph::bind(&ExpressionGrammarPart::structHelp, this, qi::_a ) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getUnionExpr() {
    return ( qi::lit("union") >> '<' >> typeG->typeRule >> '>'
        >> '{' >> typeG->identifier >> ':'
        >> exprG->expressionRule >> '}' )                           [ qi::_val = ph::bind(&ExpressionGrammarPart::unionHelp, this, qi::_1, qi::_2, qi::_3) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getMemberAccessExpr() {
    return ( '(' >> exprG->expressionRule >> qi::lit(").")
        >> typeG->identifier )                                      [ qi::_val = ph::bind(&ExpressionGrammarPart::memberAccessHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getTupleProjectionExpr() {
    return ( '(' >> exprG->expressionRule >> qi::lit(").")
        >> qi::ulong_long )                                         [ qi::_val = ph::bind(&ExpressionGrammarPart::tupleProjectionHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule ExpressionGrammarPart<T, U, V, W, X>::getMarkerExpr() {
    return ( '<' >> qi::lit("me") >> qi::lit("id") >> '='
        >> qi::ulong_long >> '>' >> exprG->expressionRule >> '<'
        >> '/' >> qi::lit("me") >> '>')                             [ qi::_val = ph::bind(&ExpressionGrammarPart::markerExprHelp, this, qi::_2, qi::_1) ];
}

template<typename T, typename U, typename V, typename W, typename X>
ExpressionGrammarPart<T, U, V, W, X>::ExpressionGrammarPart(NodeManager& nMan, ExpressionGrammar<T, U, V, W, X>* exprGram, TypeGrammar<V, W, X>* typeGram)
    : ExpressionGrammarPart::base_type(expressionPart), exprG(exprGram), typeG(typeGram), nodeMan(nMan) {

    auto basicRef = ph::ref(nodeMan.basic);

    auto intTypeRef = ph::cref(nodeMan.basic.getInt4());
    // RULES ---------------------------------------------------- | ACTIONS ----------------------------------------------------------------------------------

    // terminals, no skip parser
    literalString = getLiteralString();


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

    castExpr = getCastExpr();

    // --------------------------------------------------------------------------------------

    bindExpr = getBindExpr();/*
        ( qi::lit("bind") >> '(' >> -( variableExpr                 [ ph::push_back(qi::_a, qi::_1) ]
          % ',') >> ')' >> '{' >> exprG->callExpr >> '}' )          [ qi::_val = ph::bind(&ExpressionGrammarPart::bindExprHelp, this, qi::_a, qi::_2) ];
*/
    tupleExpr = getTupleExpr();

    vectorExpr = getVectorExpr();

    structExpr = getStructExpr();

    unionExpr = getUnionExpr();

    memberAccessExpr = getMemberAccessExpr();

    tupleProjectionExpr = getTupleProjectionExpr();

    markerExpr = getMarkerExpr();

    // --------------------------------------------------------------------------------------
//    BOOST_SPIRIT_DEBUG_NODE(literalString);
//    BOOST_SPIRIT_DEBUG_NODE(charLiteral);
//    BOOST_SPIRIT_DEBUG_NODE(lambdaDef);
//    BOOST_SPIRIT_DEBUG_NODE(lambdaExpr);
//    BOOST_SPIRIT_DEBUG_NODE(tupleExpr);
//    BOOST_SPIRIT_DEBUG_NODE(vectorExpr);
//    BOOST_SPIRIT_DEBUG_NODE(structExpr);
//    BOOST_SPIRIT_DEBUG_NODE(unionExpr);
//    BOOST_SPIRIT_DEBUG_NODE(memberAccessExpr);
//    BOOST_SPIRIT_DEBUG_NODE(tupleProjectionExpr);
//    BOOST_SPIRIT_DEBUG_NODE(markerExpr);
    // --------------------------------------------------------------------------------------
//    BOOST_SPIRIT_DEBUG_NODE(literalExpr);
//    BOOST_SPIRIT_DEBUG_NODE(opExpr);
//    BOOST_SPIRIT_DEBUG_NODE(variableExpr);
//    BOOST_SPIRIT_DEBUG_NODE(callExpr);
//    BOOST_SPIRIT_DEBUG_NODE(castExpr);
//    BOOST_SPIRIT_DEBUG_NODE(expressionRule);
}

template<typename T, typename U, typename V, typename W, typename X>
ExpressionGrammarPart<T, U, V, W, X>::~ExpressionGrammarPart() {
}

// Explicit Template Instantiation
template struct ExpressionGrammarPart<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>;

} // namespace parse
} // namespace core
} // namespace insieme

