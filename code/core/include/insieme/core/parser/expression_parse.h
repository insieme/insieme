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

#pragma once

#include "insieme/core/parser/ir_parse.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {
namespace parse {

#define Rule qi::rule<ParseIt, T(), qi::space_type>

// FW Declaration
template<typename T, typename U, typename V> struct TypeGrammar;
template<typename T, typename U, typename V, typename W, typename X, typename Y, typename Z> struct ExpressionGrammarPart;
template<typename U, typename T, typename V, typename W, typename X, typename Y, typename Z> struct StatementGrammar;
template<typename T, typename U, typename V, typename W, typename X, typename Y, typename Z> struct OperatorGrammar;

// helper function to be able to use std::make_pair along with ph::push_back
template<typename T, typename U>
std::pair<T, U> makePair (T first, U second) {
    return std::make_pair(first, second);
}

/* moved to ir_parse.h
class VariableTable {
...
};
*/

// Parser usage
// T = ExpressionPtr
// U = StatementPtr
// V = TypePtr
// W = IntTypeParamPtr
// X = IdentifierPtr
// Y = Lambda
// Z = LambdaDef
template<
	typename T = ExpressionPtr, 
	typename U = StatementPtr, 
	typename V = TypePtr, 
	typename W = IntTypeParamPtr, 
	typename X = StringValuePtr,
	typename Y = LambdaPtr, 
	typename Z = LambdaDefinitionPtr
>
struct ExpressionGrammar : public qi::grammar<ParseIt, T(), qi::space_type> {
    TypeGrammar<V, W, X> *typeG; // pointer for weak coupling
    ExpressionGrammarPart<T, U, V, W, X, Y, Z> *exprGpart;
    StatementGrammar<U, T, V, W, X, Y, Z>* stmtG;
    OperatorGrammar<T, U, V, W, X, Y, Z>* opG;
    VariableTable varTab;
    NodeManager& nodeMan;
    bool deleteStmtG;

    ExpressionGrammar(NodeManager& nodeMan, StatementGrammar<U, T, V, W, X, Y, Z>* stmtGrammar = NULL);
    virtual ~ExpressionGrammar();

    // terminal rules, no skip parsing
    qi::rule<ParseIt, string()> literalString;

    // nonterminal rules with skip parsing
    qi::rule<ParseIt, T(), qi::space_type> literalExpr;
    qi::rule<ParseIt, T(), qi::space_type> opExpr;
    qi::rule<ParseIt, T(), qi::space_type> variableExpr;
    qi::rule<ParseIt, T(), qi::space_type> funVarExpr;

    qi::rule<ParseIt, T(), qi::locals<vector<T> >, qi::space_type> callExpr;
    qi::rule<ParseIt, T(), qi::space_type> castExpr;

    qi::rule<ParseIt, T(), qi::space_type> expressionRule;

    // literals -----------------------------------------------------------------------------
    qi::rule<ParseIt, T(), qi::space_type> charLiteral;

    // --------------------------------------------------------------------------------------
    qi::rule<ParseIt, Y(), qi::locals<vector<T> >, qi::space_type> lambda;
    qi::rule<ParseIt, Z(), qi::locals<vector<T>, vector<Y> >, qi::space_type> lambdaDef;
    qi::rule<ParseIt, T(), qi::space_type> lambdaExpr;

    qi::rule<ParseIt, T(), qi::space_type> bindExpr;

    qi::rule<ParseIt, T(), qi::locals<vector<U>, vector<std::pair<T, T> > >, qi::space_type> jobExpr;
    qi::rule<ParseIt, T(), qi::space_type> tupleExpr;
    qi::rule<ParseIt, T(), qi::space_type> vectorExpr;
    qi::rule<ParseIt, T(), qi::space_type> structExpr;
    qi::rule<ParseIt, T(), qi::space_type> unionExpr;

    qi::rule<ParseIt, T(), qi::space_type> memberAccessExpr;
    qi::rule<ParseIt, T(), qi::space_type> tupleProjectionExpr;
    qi::rule<ParseIt, T(), qi::space_type> markerExpr;

    qi::rule<ParseIt, T(), qi::space_type> intExpr;
    qi::rule<ParseIt, T(), qi::space_type> doubleExpr;
    qi::rule<ParseIt, T(), qi::space_type> boolExpr;

    // --------------------------------------------------------------------------------------

    // member functions applying the rules
    virtual qi::rule<ParseIt, string()> getLiteralString();
    virtual qi::rule<ParseIt, T(), qi::locals<vector<T> >, qi::space_type> getCallExpr();
    virtual qi::rule<ParseIt, Y(), qi::locals<vector<T> >, qi::space_type> getLambda();
    virtual qi::rule<ParseIt, Z(), qi::locals<vector<T>, vector<Y> >, qi::space_type> getLambdaDef();
    virtual qi::rule<ParseIt, T(), qi::locals<vector<U>, vector<std::pair<T, T> > >, qi::space_type> getJobExpr();
    #define get(op) virtual Rule get##op ();
    get(LiteralExpr)
    get(CharLiteral)
    get(OpExpr)
    get(VariableExpr)
    get(FunVarExpr)
    get(CastExpr)
    get(BindExpr)
    get(LambdaExpr)
    get(ExpressionRule)
    get(TupleExpr)
    get(VectorExpr)
    get(StructExpr)
    get(UnionExpr)
    get(MemberAccessExpr)
    get(TupleProjectionExpr)
    get(MarkerExpr)
    get(IntExpr)
    get(DoubleExpr)
    get(BoolExpr)
    #undef get

private:
    // member functions providing the rules
    virtual T doubleLiteralHelp(const int integer, const vector<char>& fraction);
    virtual T intLiteralHelp(const int val);
    virtual Y lambdaHelp(const V& retType, const vector<T>& paramsExpr, const U& body);
    virtual Z lambdaDefHelp(const vector<T>& funVarExpr, const vector<Y>& lambdaExpr );
    virtual T lambdaExprHelp(const T& variableExpr, const Z& def);
  	virtual T lambdaExprHelp(const Y& lambda);
    virtual T jobExprHelp(const T& threadNumRange, const T& defaultStmt, const vector<std::pair<T, T> >& guardedStmts, const vector<U>& localDeclStmts);
    virtual T callExprHelp(const T& callee, const vector<T>& arguments);
    virtual T boolLiteralHelp(const bool flag);
};

} // namespace parse
} // namespace core
} // namespace insieme
