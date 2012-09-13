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
#include "insieme/core/ir_builder.h"

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

TypePtr getDerefedType(ExpressionPtr& expr, const IRBuilder& builder) {
    if(RefTypePtr ref = dynamic_pointer_cast<const RefType>(expr->getType())) {
        expr = builder.deref(expr);
        return ref->getElementType();
    }

    return expr->getType();
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class StringValuePtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr OperatorGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, StringValuePtr, LambdaPtr, LambdaDefinitionPtr>::
        assignmentHelper(ExpressionPtr& a, ExpressionPtr& b) {
    IRBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    ExpressionPtr tmp = a;
    TypePtr aType = getDerefedType(tmp, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = aType == bType ? b : builder.castExpr(aType, b);

    return builder.callExpr(nodeMan.getLangBasic().getUnit(), generator->getRefAssign(), a, B);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class StringValuePtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr OperatorGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, StringValuePtr, LambdaPtr, LambdaDefinitionPtr>::
        binaryOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr& a, ExpressionPtr& b) {
    IRBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = aType == bType ? b : builder.castExpr(aType, b);

    return builder.callExpr(aType, generator->getOperator(aType, op), a, B);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class StringValuePtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr OperatorGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, StringValuePtr, LambdaPtr, LambdaDefinitionPtr>::
        int4OpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr& a, ExpressionPtr& b) {
    IRBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = builder.getLangBasic().isUnsignedInt(bType) ?
    		nodeMan.getLangBasic().getUInt4() == bType ? b : builder.castExpr(nodeMan.getLangBasic().getUInt4(), b) :
    		nodeMan.getLangBasic().getInt4() == bType ? b : builder.castExpr(nodeMan.getLangBasic().getInt4(), b);

    return builder.callExpr(aType, generator->getOperator(aType, op), a, B);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class StringValuePtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr OperatorGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, StringValuePtr, LambdaPtr, LambdaDefinitionPtr>::
        unaryOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr& a) {
    IRBuilder builder(nodeMan);

    // if argument is a reference, automatically deref it
    TypePtr aType = getDerefedType(a, builder);

    return builder.callExpr(generator->getOperator(aType, op), a);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class StringValuePtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr OperatorGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, StringValuePtr, LambdaPtr, LambdaDefinitionPtr>::
        signOperation(const lang::BasicGenerator::Operator& op, ExpressionPtr& b) {
    IRBuilder builder(nodeMan);

    // if argument is a reference, automatically deref it
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr A = builder.literal(bType, "0");

    return builder.callExpr(b->getType(), generator->getOperator(bType, op), A, b);
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class StringValuePtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr OperatorGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, StringValuePtr, LambdaPtr, LambdaDefinitionPtr>::
        inplaceOperation(const lang::BasicGenerator::Operator& op, ExpressionPtr& a) {
    IRBuilder builder(nodeMan);

	switch(op) {

	case lang::BasicGenerator::PreInc:  return builder.callExpr(a->getType(), generator->getGenPreInc(), a);
	case lang::BasicGenerator::PreDec:  return builder.callExpr(a->getType(), generator->getGenPreDec(), a);
	case lang::BasicGenerator::PostInc: return builder.callExpr(a->getType(), generator->getGenPostInc(), a);
	case lang::BasicGenerator::PostDec: return builder.callExpr(a->getType(), generator->getGenPostDec(), a);

	default:
		break;
	}
	//builder.callExpr(a->getType(), 
	//if(RefTypePtr ref = dynamic_pointer_cast<const RefType>(a->getType())) {
	//	return builder.callExpr(generator->getOperator(ref->getElementType(), op), a);
	//}

	throw SemanticException("Inplace Operations only work on reference variables");
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class StringValuePtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr OperatorGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, StringValuePtr, LambdaPtr, LambdaDefinitionPtr>::
        lazyOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr& a, ExpressionPtr& b) {
    IRBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr A = aType == nodeMan.getLangBasic().getBool() ? a : builder.castExpr(nodeMan.getLangBasic().getBool(), a);
    ExpressionPtr B = bType == nodeMan.getLangBasic().getBool() ? b : builder.castExpr(nodeMan.getLangBasic().getBool(), b);

    return builder.callExpr(aType, generator->getOperator(aType, op), a, builder.createCallExprFromBody(builder.returnStmt(b), nodeMan.getLangBasic().getBool(), true));
}

template<class ExpressionPtr, class StatementPtr, class TypePtr, class IntTypeParamPtr, class StringValuePtr, class LambdaPtr, class LambdaDefinitionPtr>
ExpressionPtr OperatorGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, StringValuePtr, LambdaPtr, LambdaDefinitionPtr>::
        boolOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr& a, ExpressionPtr& b) {
    IRBuilder builder(nodeMan);

    // if arguments are references, automatically deref them
    TypePtr aType = getDerefedType(a, builder);
    TypePtr bType = getDerefedType(b, builder);

    ExpressionPtr B = aType == bType ? b : builder.castExpr(aType, b);

    return builder.callExpr(nodeMan.getLangBasic().getBool(), generator->getOperator(aType, op), a, B);
}

//#define getHelp(op) get##op##Helper()

#define getBinary(op,symbol) template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z> \
    qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::get##op() { \
    return ( '(' >> exprG->expressionRule >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::\
              binaryOpHelper, this, lBo::op, qi::_1, qi::_2) ]; \
}
#define getInt4(op,symbol) template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z> \
    qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::get##op() { \
    return ( '(' >> exprG->expressionRule >> symbol \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::\
              int4OpHelper, this, lBo::op, qi::_1, qi::_2) ]; \
}
#define getUnary(op, symbol) template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z> \
    qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::get##op() { \
    return ( qi::lit("(") >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::\
                                                                          unaryOpHelper, this, lBo::op, qi::_1) ]; \
}
#define getInplacePre(op, symbol) template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z> \
    qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::get##op() { \
    return ( qi::lit("(") >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::\
                                                                          inplaceOperation, this, lBo::op, qi::_1) ]; \
}
#define getInplacePost(op, symbol) template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z> \
    qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::get##op() { \
    return ( qi::lit("(") \
      >> exprG->expressionRule >> qi::lit(symbol) >> ')' )          [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::\
                                                                          inplaceOperation, this, lBo::op, qi::_1) ]; \
}
#define getLazy(op, symbol) template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z> \
    qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::get##op() { \
    return ( '(' >> exprG->expressionRule >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::\
                                                                          lazyOpHelper, this, lBo::op, qi::_1, qi::_2) ]; \
}
#define getBool(op, symbol) template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z> \
    qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::get##op() { \
    return ( '(' >> exprG->expressionRule >> qi::lit(symbol) \
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::\
              boolOpHelper, this, lBo::op, qi::_1, qi::_2) ]; \
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

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::getPlus() {
    return ( qi::lit("(") >> '+'
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::
              signOperation, this, lBo::Add, qi::_1) ];
}
template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::getMinus() {
    return ( qi::lit("(") >> '-'
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::
              signOperation, this, lBo::Sub, qi::_1) ];
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::getAssignment() {
    return ( '(' >> exprG->expressionRule >> '='
      >> exprG->expressionRule >> ')' )                             [ qi::_val = ph::bind(&OperatorGrammar<T, U, V, W, X, Y, Z>::
              assignmentHelper, this, qi::_1, qi::_2) ];
}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
qi::rule<ParseIt, T(), qi::space_type> OperatorGrammar<T, U, V, W, X, Y, Z>::getOperator() {
    return assignment                                               [ qi::_val = ph::construct<T>(qi::_1) ]
        | addition                                                  [ qi::_val = ph::construct<T>(qi::_1) ]
        | subtraction                                               [ qi::_val = ph::construct<T>(qi::_1) ]
        | multiplication                                            [ qi::_val = ph::construct<T>(qi::_1) ]
        | division                                                  [ qi::_val = ph::construct<T>(qi::_1) ]
        | modulo                                                    [ qi::_val = ph::construct<T>(qi::_1) ]
        | and_                                                      [ qi::_val = ph::construct<T>(qi::_1) ]
        | or_                                                       [ qi::_val = ph::construct<T>(qi::_1) ]
        | xor_                                                      [ qi::_val = ph::construct<T>(qi::_1) ]
        | lShift                                                    [ qi::_val = ph::construct<T>(qi::_1) ]
        | rShift                                                    [ qi::_val = ph::construct<T>(qi::_1) ]
        | not_                                                      [ qi::_val = ph::construct<T>(qi::_1) ]
        | plus                                                      [ qi::_val = ph::construct<T>(qi::_1) ]
        | minus                                                     [ qi::_val = ph::construct<T>(qi::_1) ]
        | preInc                                                    [ qi::_val = ph::construct<T>(qi::_1) ]
        | postInc                                                   [ qi::_val = ph::construct<T>(qi::_1) ]
        | preDec                                                    [ qi::_val = ph::construct<T>(qi::_1) ]
        | postDec                                                   [ qi::_val = ph::construct<T>(qi::_1) ]
        | lAnd                                                      [ qi::_val = ph::construct<T>(qi::_1) ]
        | lOr                                                       [ qi::_val = ph::construct<T>(qi::_1) ]
        | lNot                                                      [ qi::_val = ph::construct<T>(qi::_1) ]
        | Eq                                                        [ qi::_val = ph::construct<T>(qi::_1) ]
        | Ne                                                        [ qi::_val = ph::construct<T>(qi::_1) ]
        | Lt                                                        [ qi::_val = ph::construct<T>(qi::_1) ]
        | Le                                                        [ qi::_val = ph::construct<T>(qi::_1) ]
        | Gt                                                        [ qi::_val = ph::construct<T>(qi::_1) ]
        | Ge                                                        [ qi::_val = ph::construct<T>(qi::_1) ];
}


template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
OperatorGrammar<T, U, V, W, X, Y, Z>::OperatorGrammar(NodeManager& nMan, ExpressionGrammar<T, U, V, W, X, Y, Z>* exprGram)
    : OperatorGrammar::base_type(operatorRule), exprG(exprGram), generator(new lang::BasicGenerator(nMan)), nManRef(ph::ref(nMan)), nodeMan(nMan) {

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

    operatorRule = getOperator();

}

template<typename T, typename U, typename V, typename W, typename X, typename Y,  typename Z>
OperatorGrammar<T, U, V, W, X, Y, Z>::~OperatorGrammar() {
    delete generator;
}

// Explicit Template Instantiation
template struct OperatorGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, StringValuePtr, LambdaPtr, LambdaDefinitionPtr>;


} // namespace parse
} // namespace core
} // namespace insieme
