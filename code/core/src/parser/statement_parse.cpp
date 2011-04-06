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

#include "insieme/core/parser/statement_parse.h"

#include "insieme/core/parser/expression_parse.h"
#include "insieme/core/parser/type_parse.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ast_builder.h"

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_ascii.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

namespace insieme {
namespace core {
namespace parse {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace ph = boost::phoenix;

template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::breakHelp() {
    return BreakStmt::get(nodeMan);
}

template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::continueHelp() {
    return ContinueStmt::get(nodeMan);
}

template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::returnHelp(ExpressionPtr ret) {
    return ReturnStmt::get(nodeMan, ret);
}


template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::
    declarationHelp(ExpressionPtr varExpr, ExpressionPtr initExpr) {
    if(VariablePtr var = dynamic_pointer_cast<const Variable>(varExpr))
        return DeclarationStmt::get(nodeMan, var, initExpr);

    throw ParseException();
}

template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::compoundHelp(vector<StatementPtr>  stmts) {
    return CompoundStmt::get(nodeMan, stmts);
}

template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::
    whileHelp(ExpressionPtr condition, StatementPtr body) {
    return WhileStmt::get(nodeMan, condition, body);
}

template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::
    forHelp(StatementPtr loopVarStmt, ExpressionPtr end, ExpressionPtr step, StatementPtr body) {
    ASTBuilder builder(nodeMan);
    if(DeclarationStmtPtr loopVar = dynamic_pointer_cast<const DeclarationStmt>(loopVarStmt))
        return ForStmt::get(nodeMan, loopVar, body, end, step);

    throw ParseException();
}

template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::
    ifHelp(const ExpressionPtr& condition, const StatementPtr& body, const boost::optional<StatementPtr>& elseBody) {
    if(elseBody)
        return IfStmt::get(nodeMan, condition, body, *elseBody);

    return IfStmt::get(nodeMan, condition, body);

}

template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::
    switchHelp(const ExpressionPtr& switchExpr, const vector<std::pair<ExpressionPtr, StatementPtr> >& cases, const boost::optional<StatementPtr>& defaultCase) {
    if(defaultCase)
        return SwitchStmt::get(nodeMan, switchExpr, cases, *defaultCase);

    return SwitchStmt::get(nodeMan, switchExpr, cases);
}

template<class StatementPtr, class ExpressionPtr, class TypePtr, class IntTypeParamPtr, class IdentifierPtr>
StatementPtr StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>::
    markerHelp(const StatementPtr& subStmt, const unsigned int id) {
    return MarkerStmt::get(nodeMan, subStmt, id);
}

template<typename T, typename U, typename V, typename W, typename X>
Rule StatementGrammar<T, U, V, W, X>::getBreak(){
    return qi::lit("break")                                         [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::breakHelp, this) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule StatementGrammar<T, U, V, W, X>::getContinue(){
    return qi::lit("continue")                                      [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::continueHelp, this) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule StatementGrammar<T, U, V, W, X>::getReturn() {
    ASTBuilder builder(nodeMan);
    return (qi::lit("return") >> qi::lit("unit"))                   [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::returnHelp, this, builder.getBasicGenerator().getUnitConstant()) ]
         | (qi::lit("return") >> exprG->expressionRule)             [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::returnHelp, this, qi::_1) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule StatementGrammar<T, U, V, W, X>::getDeclaration() {
    return (qi::lit("decl") >> exprG->variableExpr >> '='
        >> exprG->expressionRule )                                  [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::declarationHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X>
qi::rule<ParseIt, T(), qi::locals<vector<T> >,  qi::space_type> StatementGrammar<T, U, V, W, X>::getCompound() {
    return ('{' >> *( statementRule >> ';' )                        [ ph::push_back(qi::_a, qi::_1) ]
        >> '}' )                                                    [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::compoundHelp, this, qi::_a) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule StatementGrammar<T, U, V, W, X>::getWhile() {
    return (qi::lit("while") >> '(' >> exprG->expressionRule
        >> ')' >> statementRule )                                   [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::whileHelp, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule StatementGrammar<T, U, V, W, X>::getFor() {
    return (qi::lit("for") >> '(' >> declarationStmt >> qi::lit("..")
        >> exprG->expressionRule >> ':' >> exprG->expressionRule
        >> ')' >> statementRule)                                    [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::forHelp, this, qi::_1, qi::_2, qi::_3, qi::_4) ];
}

template<typename T, typename U, typename V, typename W, typename X>
Rule StatementGrammar<T, U, V, W, X>::getIf() {
    return (qi::lit("if") >> '(' >> exprG->expressionRule >> ')'
        >> statementRule
          >>  -(qi::lit("else") >> statementRule))                  [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::ifHelp, this, qi::_1, qi::_2, qi::_3) ];

}

template<typename T, typename U, typename V, typename W, typename X>
qi::rule<ParseIt, T(), qi::locals<vector<std::pair<U, T> > >, qi::space_type> StatementGrammar<T, U, V, W, X>::getSwitch() {
    return (qi::lit("switch") >> '(' >> exprG->expressionRule >> ')'
        >> '{' >> *(qi::lit("case") >> exprG->expressionRule
          >> ':' >> statementRule)                                  [ ph::push_back(qi::_a, ph::bind(&makePair<ExpressionPtr, StatementPtr>, qi::_1, qi::_2)) ]
        >> -( qi::lit("default") >> ':' >> statementRule) >> '}' )  [ qi::_val = ph::bind(&StatementGrammar<T, U, V, W, X>::switchHelp, this, qi::_1, qi::_a, qi::_3) ];
}
template<typename T, typename U, typename V, typename W, typename X>
Rule StatementGrammar<T, U, V, W, X>::getMarker() {
    return ('<' >> qi::lit("ms") >> qi::lit("id") >> '='
        >> qi::ulong_long >> '>' >> statementRule >> '<'
        >> '/' >> qi::lit("ms") >> '>')                             [ qi::_val = ph::bind(&StatementGrammar::markerHelp, this, qi::_2, qi::_1) ];
}


template<typename T, typename U, typename V, typename W, typename X>
StatementGrammar<T, U, V, W, X>::StatementGrammar(NodeManager& nMan, ExpressionGrammar<U, T, V, W, X>* exprGram, TypeGrammar<V, W, X>* typeGram)
    : StatementGrammar<T, U, V, W, X>::base_type(statementRule), nodeMan(nMan) {

    if(typeGram == NULL) {
        exprG = new ExpressionGrammar<U, T, V, W, X>(nodeMan, this);
        typeG = new TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr>(nodeMan);
        deleteFields = true;
    } else {
        exprG = exprGram;
        typeG = typeGram;
        deleteFields = false;
    }
ASTBuilder builder(nodeMan);

    // RULES --------------------------------------------------------------------------------------------------------------

    breakStmt = getBreak();

    continueStmt = getContinue();

    returnStmt = getReturn();

    declarationStmt = getDeclaration();

    compoundStmt = getCompound();

    whileStmt = getWhile();

    forStmt = getFor();

    ifStmt = getIf();

    switchStmt = getSwitch();

    markerStmt = getMarker();

    // -------------------------------------------------------------------------------------------------------------------

    statementRule =
        breakStmt                                                   [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | continueStmt                                                [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | returnStmt                                                  [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | declarationStmt                                             [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | compoundStmt                                                [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | whileStmt                                                   [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | forStmt                                                     [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | ifStmt                                                      [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | switchStmt                                                  [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | markerStmt                                                  [ qi::_val = ph::construct<StatementPtr>(qi::_1) ]
      | exprG->expressionRule                                       [ qi::_val = ph::construct<StatementPtr>(qi::_1) ];

//    BOOST_SPIRIT_DEBUG_NODE(breakStmt);
//    BOOST_SPIRIT_DEBUG_NODE(continueStmt);
//    BOOST_SPIRIT_DEBUG_NODE(returnStmt);
//    BOOST_SPIRIT_DEBUG_NODE(declarationStmt );
//    BOOST_SPIRIT_DEBUG_NODE(compoundStmt);
//    BOOST_SPIRIT_DEBUG_NODE(whileStmt);
//    BOOST_SPIRIT_DEBUG_NODE(forStmt);
//    BOOST_SPIRIT_DEBUG_NODE(ifStmt);
//    BOOST_SPIRIT_DEBUG_NODE(switchStmt);
//    BOOST_SPIRIT_DEBUG_NODE(markerStmt);
}

template<typename T, typename U, typename V, typename W, typename X>
StatementGrammar<T, U, V, W, X>::~StatementGrammar() {
    if(deleteFields) {
        delete exprG;
        delete typeG;
    }
}

// Explicit Template Instantiation
template struct StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr>;

} // namespace parse
} // namespace core
} // namespace insieme

