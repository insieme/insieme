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

typedef lang::BasicGenerator::Operator lBo;

TypePtr getDerefedType(ExpressionPtr& expr, ASTBuilder& builder) {
    if(RefTypePtr ref = dynamic_pointer_cast<const RefType>(expr->getType())) {
        expr = builder.deref(expr);
        return ref->getElementType();
    }

    return expr->getType();
}

CallExprPtr getBinaryOperation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = aType == bType ? b : builder.castExpr(aType, b);

    return builder.callExpr(aType, generator->getOperator(aType, op), a, B);
}

CallExprPtr getInt4Operation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = nodeMan.basic.getInt4() == bType ? b : builder.castExpr(nodeMan.basic.getInt4(), b);

    return builder.callExpr(aType, generator->getOperator(aType, op), a, B);
}

CallExprPtr getUnaryOperation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, ExpressionPtr a) {
    ASTBuilder builder(nodeMan);

    // if argument is a reference, automatically deref it
    TypePtr aType = getDerefedType(a, builder);

    return builder.callExpr(generator->getOperator(aType, op), a);
}

CallExprPtr getSignOperation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if argument is a reference, automatically deref it
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr A = builder.literal("0", bType);

    return builder.callExpr(b->getType(), generator->getOperator(bType, op), A, b);
}

CallExprPtr getInplaceOperation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, ExpressionPtr a) {
    ASTBuilder builder(nodeMan);

    if(RefTypePtr ref = dynamic_pointer_cast<const RefType>(a->getType())) {
        return builder.callExpr(generator->getOperator(ref->getElementType(), op), a);
    }

    throw ParseException();
}

CallExprPtr getLazyOperation(NodeManager& nodeMan, const lang::BasicGenerator* generator, const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr A = aType == nodeMan.basic.getBool() ? a : builder.castExpr(nodeMan.basic.getBool(), a);
    ExpressionPtr B = bType == nodeMan.basic.getBool() ? b : builder.castExpr(nodeMan.basic.getBool(), b);

    return builder.callExpr(aType, generator->getOperator(aType, op), a, builder.createCallExpr(builder.returnStmt(b), nodeMan.basic.getBool()));
}


OperatorGrammar::OperatorGrammar(NodeManager& nodeMan, ExpressionGrammar* exprGram)
    : OperatorGrammar::base_type(operatorRule), exprG(exprGram), generator(new lang::BasicGenerator(nodeMan)) {

    auto nManRef = ph::ref(nodeMan);

/*
    assignment =
        ( '(' >> exprG->expressionRule >> '='
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::, qi::_1, qi::_2) ];
*/
    addition =
        ( '(' >> exprG->expressionRule >> '+'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Add, qi::_1, qi::_2) ];

    subtraction =
        ( '(' >> exprG->expressionRule >> '-'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Sub, qi::_1, qi::_2) ];

    multiplication =
        ( '(' >> exprG->expressionRule >> '*'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Mul, qi::_1, qi::_2) ];

    division =
        ( '(' >> exprG->expressionRule >> '/'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Div, qi::_1, qi::_2) ];

    modulo =
        ( '(' >> exprG->expressionRule >> '%'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Mod, qi::_1, qi::_2) ];

    and_ =
        ( '(' >> exprG->expressionRule >> '&'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::And, qi::_1, qi::_2) ];

    or_ =
        ( '(' >> exprG->expressionRule >> '|'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Or, qi::_1, qi::_2) ];

    xor_ =
        ( '(' >> exprG->expressionRule >> '^'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Xor, qi::_1, qi::_2) ];

    lShift =
        ( '(' >> exprG->expressionRule >> qi::lit("<<")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getInt4Operation, nManRef, generator, lBo::LShift, qi::_1, qi::_2) ];

    rShift =
        ( '(' >> exprG->expressionRule >> qi::lit(">>")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getInt4Operation, nManRef, generator, lBo::RShift, qi::_1, qi::_2) ];

    // --------------------------------------------------------------------------------------

    not_ =
        ( qi::lit("(") >> qi::lit("~")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getUnaryOperation, nManRef, generator, lBo::Not, qi::_1) ];

    plus =
        ( qi::lit("(") >> '+'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getSignOperation, nManRef, generator, lBo::Add, qi::_1) ];

    minus =
        ( qi::lit("(") >> '-'
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getSignOperation, nManRef, generator, lBo::Sub, qi::_1) ];

    // --------------------------------------------------------------------------------------

    preInc =
        ( qi::lit("(") >> qi::lit("++")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getInplaceOperation, nManRef, generator, lBo::PreInc, qi::_1) ];

    postInc =
        ( qi::lit("(")
          >> exprG->expressionRule >> qi::lit("++") >> ')' )        [ qi::_val = ph::bind(&getInplaceOperation, nManRef, generator, lBo::PostInc, qi::_1) ];

    preDec =
        ( qi::lit("(") >> qi::lit("--")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getInplaceOperation, nManRef, generator, lBo::PreDec, qi::_1) ];

    postDec =
        ( qi::lit("(")
          >> exprG->expressionRule >> qi::lit("--") >> ')' )        [ qi::_val = ph::bind(&getInplaceOperation, nManRef, generator, lBo::PostDec, qi::_1) ];

    // --------------------------------------------------------------------------------------

    lAnd =
        ( '(' >> exprG->expressionRule >> qi::lit("&&")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getLazyOperation, nManRef, generator, lBo::LAnd, qi::_1, qi::_2) ];

    lOr =
        ( '(' >> exprG->expressionRule >> qi::lit("||")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getLazyOperation, nManRef, generator, lBo::LOr, qi::_1, qi::_2) ];

    lNot =
        ( '(' >> qi::lit("!")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getUnaryOperation, nManRef, generator, lBo::LNot, qi::_1) ];

    Eq =
        ( '(' >> exprG->expressionRule >> qi::lit("==")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Eq, qi::_1, qi::_2) ];

    Ne =
        ( '(' >> exprG->expressionRule >> qi::lit("!=")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Ne, qi::_1, qi::_2) ];

    Lt =
        ( '(' >> exprG->expressionRule >> qi::lit("<")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Lt, qi::_1, qi::_2) ];

    Le =
        ( '(' >> exprG->expressionRule >> qi::lit("<=")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Le, qi::_1, qi::_2) ];

    Gt =
        ( '(' >> exprG->expressionRule >> qi::lit(">")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Gt, qi::_1, qi::_2) ];

    Ge =
        ( '(' >> exprG->expressionRule >> qi::lit(">=")
          >> exprG->expressionRule >> ')' )                         [ qi::_val = ph::bind(&getBinaryOperation, nManRef, generator, lBo::Ge, qi::_1, qi::_2) ];

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
        | preInc                                                      [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | postInc                                                     [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | preDec                                                      [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | postDec                                                     [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | lAnd                                                        [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | lOr                                                         [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | lNot                                                        [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | Eq                                                          [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | Ne                                                          [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | Lt                                                          [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | Le                                                          [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | Gt                                                          [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | Ge                                                          [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        ;

}

OperatorGrammar::~OperatorGrammar() {
    delete generator;
}

} // namespace parse
} // namespace core
} // namespace insieme
