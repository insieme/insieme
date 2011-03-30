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
#include <boost/spirit/home/phoenix/bind/bind_member_function.hpp>


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

CallExprPtr OperatorGrammar::getAssignmentHelper(ExpressionPtr a, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    ExpressionPtr tmp = a;
    TypePtr aType = getDerefedType(tmp, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = aType == bType ? b : builder.castExpr(aType, b);

    return builder.callExpr(nodeMan.basic.getUnit(), generator->getRefAssign(), a, B);
}

CallExprPtr OperatorGrammar::getBinaryOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = aType == bType ? b : builder.castExpr(aType, b);

    return builder.callExpr(aType, generator->getOperator(aType, op), a, B);
}

CallExprPtr OperatorGrammar::getInt4OpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = nodeMan.basic.getInt4() == bType ? b : builder.castExpr(nodeMan.basic.getInt4(), b);

    return builder.callExpr(aType, generator->getOperator(aType, op), a, B);
}

CallExprPtr OperatorGrammar::getUnaryOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a) {
    ASTBuilder builder(nodeMan);

    // if argument is a reference, automatically deref it
    TypePtr aType = getDerefedType(a, builder);

    return builder.callExpr(generator->getOperator(aType, op), a);
}

CallExprPtr OperatorGrammar::getSignOperation(const lang::BasicGenerator::Operator& op, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if argument is a reference, automatically deref it
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr A = builder.literal("0", bType);

    return builder.callExpr(b->getType(), generator->getOperator(bType, op), A, b);
}

CallExprPtr OperatorGrammar::getInplaceOperation(const lang::BasicGenerator::Operator& op, ExpressionPtr a) {
    ASTBuilder builder(nodeMan);

    if(RefTypePtr ref = dynamic_pointer_cast<const RefType>(a->getType())) {
        return builder.callExpr(generator->getOperator(ref->getElementType(), op), a);
    }

    throw ParseException();
}

CallExprPtr OperatorGrammar::getLazyOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr A = aType == nodeMan.basic.getBool() ? a : builder.castExpr(nodeMan.basic.getBool(), a);
    ExpressionPtr B = bType == nodeMan.basic.getBool() ? b : builder.castExpr(nodeMan.basic.getBool(), b);

    return builder.callExpr(aType, generator->getOperator(aType, op), a, builder.createCallExprFromBody(builder.returnStmt(b), nodeMan.basic.getBool(), true));
}

CallExprPtr OperatorGrammar::getBoolOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b) {
    ASTBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = aType == bType ? b : builder.castExpr(aType, b);

    return builder.callExpr(nodeMan.basic.getBool(), generator->getOperator(aType, op), a, B);
}

//#define getHelp(op) get##op##Helper()

#define getBinary(op,symbol) qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::get##op() { \
    return ( '(' >> exprG->expressionRule >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar::getBinaryOpHelper, this, lBo::op, qi::_1, qi::_2) ]; \
}
#define getInt4(op, symbol) qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::get##op() { \
    return ( '(' >> exprG->expressionRule >> symbol \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar::getInt4OpHelper, this, lBo::op, qi::_1, qi::_2) ]; \
}
#define getUnary(op, symbol) qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::get##op() { \
    return ( qi::lit("(") >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar::getUnaryOpHelper, this, lBo::op, qi::_1) ]; \
}
#define getInplacePre(op, symbol) qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::get##op() { \
    return ( qi::lit("(") >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar::getInplaceOperation, this, lBo::op, qi::_1) ]; \
}
#define getInplacePost(op, symbol) qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::get##op() { \
    return ( qi::lit("(") \
      >> exprG->expressionRule >> qi::lit(symbol) >> ')' )          [ qi::_val = ph::bind(&OperatorGrammar::getInplaceOperation, this, lBo::op, qi::_1) ]; \
}
#define getLazy(op, symbol) qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::get##op() { \
    return ( '(' >> exprG->expressionRule >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar::getLazyOpHelper, this, lBo::op, qi::_1, qi::_2) ]; \
}
#define getBool(op, symbol) qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::get##op() { \
    return ( '(' >> exprG->expressionRule >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar::getBoolOpHelper, this, lBo::op, qi::_1, qi::_2) ]; \
}


getBinary(Add, "+")
getBinary(Sub, "-")
getBinary(Mul, "*")
getBinary(Div, "/")
getBinary(Mod, "%")
getBinary(And, "&")
getBinary(Or, "|")
getBinary(Xor, "^")
getInt4(LShift, "<<")
getInt4(RShift, ">>")
getUnary(Not, "~")
getInplacePre(PreInc, "++")
getInplacePost(PostInc, "++")
getInplacePre(PreDec, "--")
getInplacePost(PostDec, "--")
getLazy(LAnd, "&&")
getLazy(LOr, "||")
getUnary(LNot, "!")
getBool(Eq, "==")
getBool(Ne, "!=")
getBool(Lt, "<")
getBool(Le, "<=")
getBool(Gt, ">")
getBool(Ge, ">=")
#undef getBinary
#undef getInt4
#undef getUnary
#undef getInplacePre
#undef getInplacePost
#undef getLazy
#undef getBool

qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::getPlus() { \
    return ( qi::lit("(") >> '+'
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar::getSignOperation, this, lBo::Add, qi::_1) ];
}
qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::getMinus() { \
    return ( qi::lit("(") >> '-'
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar::getSignOperation, this, lBo::Sub, qi::_1) ];
}

qi::rule<ParseIt, CallExprPtr(), qi::space_type> OperatorGrammar::getAssignment() {
    return ( '(' >> exprG->expressionRule >> '='
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar::getAssignmentHelper, this, qi::_1, qi::_2) ];
}

OperatorGrammar::OperatorGrammar(NodeManager& nMan, ExpressionGrammar* exprGram)
    : OperatorGrammar::base_type(operatorRule), exprG(exprGram), generator(new lang::BasicGenerator(nMan)), nManRef(ph::ref(nMan)), nodeMan(nMan) {

//    nManRef = ph::ref(nodeMan);


    assignment = getAssignment();

    addition = getAdd();

    subtraction = getSub();

    multiplication = getMul();

    division = getDiv();

    modulo = getMod();

    and_ = getAnd();

    or_ = getOr();

    xor_ = getXor();

    lShift = getLShift();

    rShift = getRShift();

    // --------------------------------------------------------------------------------------

    not_ = getNot();

    plus = getPlus();

    minus = getMinus();

    // --------------------------------------------------------------------------------------

    preInc = getPreInc();

    postInc = getPostInc();

    preDec = getPreDec();

    postDec = getPostDec();

    // --------------------------------------------------------------------------------------

    lAnd = getLAnd();

    lOr = getLOr();

    lNot = getLNot();

    Eq = getEq();

    Ne = getNe();

    Lt = getLt();

    Le = getLe();

    Gt = getGt();

    Ge = getGe();

    //--------------------------------------------------------------------------------------------------------------------------------

    operatorRule =
          assignment                                                  [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
        | addition                                                    [ qi::_val = ph::construct<CallExprPtr>(qi::_1) ]
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
