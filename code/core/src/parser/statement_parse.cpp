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

DeclarationStmtPtr declarationHelp(NodeManager& nodeMan, VariablePtr var, ExpressionPtr initExpr) {
    return DeclarationStmt::get(nodeMan, var, initExpr);
}

CompoundStmtPtr compoundHelp(NodeManager& nodeMan, Stmts stmts) {
    return CompoundStmt::get(nodeMan, stmts);
}

ForStmtPtr forHelp(NodeManager& nodeMan, DeclarationStmtPtr loopVar, ExpressionPtr end, ExpressionPtr step, StatementPtr body) {
    ASTBuilder builder(nodeMan);
//    DeclarationStmtPtr loopVar = builder.declarationStmt(exprG->varTab.get(start->getType(), id), start);
    return ForStmt::get(nodeMan, loopVar, body, end, step);
}

IfStmtPtr ifHelp(NodeManager& nodeMan, const ExpressionPtr& condition, const StatementPtr& body, const boost::optional<StatementPtr>& elseBody) {
    if(elseBody)
        return IfStmt::get(nodeMan, condition, body, *elseBody);

    return IfStmt::get(nodeMan, condition, body);

}

SwitchStmtPtr switchHelp(NodeManager& nodeMan, const ExpressionPtr& switchExpr, const Cases& cases, const boost::optional<StatementPtr>& defaultCase) {
    if(defaultCase)
        return SwitchStmt::get(nodeMan, switchExpr, cases, *defaultCase);

    return SwitchStmt::get(nodeMan, switchExpr, cases);
}

StatementGrammar::StatementGrammar(NodeManager& nodeMan)
    : StatementGrammar::base_type(statementRule), typeG(new TypeGrammar(nodeMan))/*, exprG(new ExpressionGrammar(nodeMan))*/ {

    exprG = new ExpressionGrammar(nodeMan, this);

    auto nManRef = ph::ref(nodeMan);
    auto basicRef = ph::ref(nodeMan.basic);

    // RULES --------------------------------------------------------------------------------------------------------------

    breakStmt =
        qi::lit("break")                                            [ qi::_val = ph::bind(&BreakStmt::get, nManRef) ];

    continueStmt =
        qi::lit("continue")                                         [ qi::_val = ph::bind(&ContinueStmt::get, nManRef) ];

    returnStmt =
        (qi::lit("return") >> exprG->expressionRule)                [ qi::_val = ph::bind(&ReturnStmt::get, nManRef, qi::_1) ];

    declarationStmt =
        (qi::lit("decl") >> exprG->variableExpr >> '='
        >> exprG->expressionRule )                                  [ qi::_val = ph::bind(&declarationHelp, nManRef, qi::_1, qi::_2) ];

    compoundStmt =
        ('{' >> *( statementRule >> ';' )                           [ ph::push_back(qi::_a, qi::_1) ]
        >> '}' )                                                    [ qi::_val = ph::bind(&compoundHelp, nManRef, qi::_a) ];

    whileStmt =
        (qi::lit("while") >> '(' >> exprG->expressionRule
        >> ')' >> statementRule )                                   [ qi::_val = ph::bind(&WhileStmt::get, nManRef, qi::_1, qi::_2) ];

    forStmt =
        (qi::lit("for") >> '(' >> declarationStmt /*>> '='
        >> exprG->expressionRule*/ >> qi::lit("..")
        >> exprG->expressionRule >> ':' >> exprG->expressionRule
        >> ')' >> statementRule)                                    [ qi::_val = ph::bind(&forHelp, nManRef, qi::_1, qi::_2, qi::_3, qi::_4) ];

    ifStmt =
        (qi::lit("if") >> '(' >> exprG->expressionRule >> ')'
        >> statementRule
          >>  -(qi::lit("else") >> statementRule))                  [ qi::_val = ph::bind(&ifHelp, nManRef, qi::_1, qi::_2, qi::_3) ];

    switchStmt =
        (qi::lit("switch") >> '(' >> exprG->expressionRule >> ')'
          >> '{' >> *(qi::lit("case") >> exprG->expressionRule
            >> ':' >> statementRule)                               [ ph::push_back(qi::_a, ph::bind(&makePair<ExpressionPtr, StatementPtr>, qi::_1, qi::_2)) ]
        >> -( qi::lit("default") >> ':' >> statementRule) >> '}' ) [ qi::_val = ph::bind(&switchHelp, nManRef, qi::_1, qi::_a, qi::_3) ];

    markerStmt =
        ('<' >> qi::lit("ms") >> qi::lit("id") >> '='
        >> qi::ulong_long >> '>' >> statementRule >> '<'
        >> '/' >> qi::lit("ms") >> '>')                            [ qi::_val = ph::bind(&MarkerStmt::get, nManRef, qi::_2, qi::_1) ];

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

StatementGrammar::~StatementGrammar() {
    delete exprG;
    delete typeG;
}

} // namespace parse
} // namespace core
} // namespace insieme

