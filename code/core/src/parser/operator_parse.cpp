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

#include "insieme/core/parser/operator_parse.h"

#include "insieme/core/parser/expression_parse.h"
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

CallExprPtr getBinaryOperation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, const ExpressionPtr a, const ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    ExpressionPtr B = a->getType() == b->getType() ? b : builder.castExpr(a->getType(), b);

    return builder.callExpr(a->getType(), generator->getOperator(a->getType(), op), toVector(a, B));
}

CallExprPtr getSignOperation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, const ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    ExpressionPtr A = builder.literal("0", b->getType());

    return builder.callExpr(b->getType(), generator->getOperator(b->getType(), op), toVector(A, b));
}

CallExprPtr getInt4Operation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, const ExpressionPtr a, const ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    ExpressionPtr B = nodeMan.basic.getInt4() == b->getType() ? b : builder.castExpr(nodeMan.basic.getInt4(), b);

    return builder.callExpr(a->getType(), generator->getOperator(a->getType(), op), toVector(a, B));
}

CallExprPtr getUnaryOperation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, const ExpressionPtr a) {
    ASTBuilder builder(nodeMan);

    return builder.callExpr(generator->getOperator(a->getType(), op), toVector(a));
}

OperatorGrammar::OperatorGrammar(NodeManager& nodeMan, ExpressionGrammar* exprGram)
    : OperatorGrammar::base_type(operatorRule), exprG(exprGram), generator(new lang::BasicGenerator(nodeMan)) {

    auto nManRef = ph::ref(nodeMan);

/*
    assignment =
        ( '(' >> exprG->expressionRule >> '='
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lang::BasicGenerator::Operator::, qi::_1, qi::_2) ];
*/
    addition =
        ( '(' >> exprG->expressionRule >> '+'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lang::BasicGenerator::Operator::Add, qi::_1, qi::_2) ];

    subtraction =
        ( '(' >> exprG->expressionRule >> '-'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lang::BasicGenerator::Operator::Sub, qi::_1, qi::_2) ];

    multiplication =
        ( '(' >> exprG->expressionRule >> '*'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lang::BasicGenerator::Operator::Mul, qi::_1, qi::_2) ];

    division =
        ( '(' >> exprG->expressionRule >> '/'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lang::BasicGenerator::Operator::Div, qi::_1, qi::_2) ];

    modulo =
        ( '(' >> exprG->expressionRule >> '%'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lang::BasicGenerator::Operator::Mod, qi::_1, qi::_2) ];

    and_ =
        ( '(' >> exprG->expressionRule >> '&'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lang::BasicGenerator::Operator::And, qi::_1, qi::_2) ];

    or_ =
        ( '(' >> exprG->expressionRule >> '|'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lang::BasicGenerator::Operator::Or, qi::_1, qi::_2) ];

    xor_ =
        ( '(' >> exprG->expressionRule >> '^'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lang::BasicGenerator::Operator::Xor, qi::_1, qi::_2) ];

    lShift =
        ( '(' >> exprG->expressionRule >> qi::lit("<<")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getInt4Operation, nManRef, generator, lang::BasicGenerator::Operator::LShift, qi::_1, qi::_2) ];

    rShift =
        ( '(' >> exprG->expressionRule >> qi::lit(">>")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getInt4Operation, nManRef, generator, lang::BasicGenerator::Operator::RShift, qi::_1, qi::_2) ];

    not_ =
        ( qi::lit("(") >> qi::lit("~")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getUnaryOperation, nManRef, generator, lang::BasicGenerator::Operator::Not, qi::_1) ];

    plus =
        ( qi::lit("(") >> '+'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getSignOperation, nManRef, generator, lang::BasicGenerator::Operator::Add, qi::_1) ];

    minus =
        ( qi::lit("(") >> '-'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getSignOperation, nManRef, generator, lang::BasicGenerator::Operator::Sub, qi::_1) ];

    //--------------------------------------------------------------------------------------------------------------------------------

    operatorRule =
          addition                                                    [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | subtraction                                                 [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | multiplication                                              [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | division                                                    [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | modulo                                                      [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | and_                                                        [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | or_                                                         [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | xor_                                                        [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | lShift                                                      [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | rShift                                                      [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | not_                                                        [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | plus                                                        [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | minus                                                       [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        ;

}

OperatorGrammar::~OperatorGrammar() {
    delete generator;
}

} // namespace parse
} // namespace core
} // namespace insieme
