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
template<typename V, typename W, typename X> struct TypeGrammar;
template<typename T, typename U, typename V, typename W, typename X, typename Y, typename Z> struct ExpressionGrammar;

template<typename T = ExpressionPtr, typename U = StatementPtr, typename V = TypePtr, typename W = IntTypeParamPtr, typename X = StringValuePtr,
        typename Y = LambdaPtr, typename Z = LambdaDefinitionPtr>
struct ExpressionGrammarPart : public qi::grammar<ParseIt, T(), qi::space_type> {
    ExpressionGrammar<T, U, V, W, X, Y, Z>* exprG;
    TypeGrammar<V, W, X> *typeG; // pointer for weak coupling

    NodeManager& nodeMan;

    ExpressionGrammarPart(NodeManager& nMan, ExpressionGrammar<T, U, V, W, X, Y, Z>* exprGram, TypeGrammar<V, W, X>* typeGram);
    virtual ~ExpressionGrammarPart();

    // terminal rules, no skip parsing
    qi::rule<ParseIt, string()> literalString;

    // literals -----------------------------------------------------------------------------
    qi::rule<ParseIt, T(), qi::space_type> charLiteral;

    // nonterminal rules with skip parsing
    qi::rule<ParseIt, T(), qi::space_type> literalExpr;
    qi::rule<ParseIt, T(), qi::space_type> opExpr;
    qi::rule<ParseIt, T(), qi::space_type> variableExpr;
    qi::rule<ParseIt, T(), qi::space_type> funVarExpr;

    qi::rule<ParseIt, T(), qi::space_type> castExpr;

    // --------------------------------------------------------------------------------------

    qi::rule<ParseIt, T(), qi::locals<vector<T> >, qi::space_type> bindExpr;

    qi::rule<ParseIt, T(), qi::locals<vector<T> >, qi::space_type> tupleExpr;
    qi::rule<ParseIt, T(), qi::locals<V, vector<T> >, qi::space_type> vectorExpr;
    qi::rule<ParseIt, T(), qi::locals<vector<std::pair<X, T> >>, qi::space_type> structExpr;
    qi::rule<ParseIt, T(), qi::space_type> unionExpr;

    qi::rule<ParseIt, T(), qi::space_type> memberAccessExpr;
    qi::rule<ParseIt, T(), qi::space_type> tupleProjectionExpr;
    qi::rule<ParseIt, T(), qi::space_type> markerExpr;

    // --------------------------------------------------------------------------------------

    qi::rule<ParseIt, T(), qi::space_type> expressionPart;

    // member functions applying the rules
    virtual qi::rule<ParseIt, string()> getLiteralString();
    virtual qi::rule<ParseIt, T(), qi::locals<vector<T> >, qi::space_type> getBindExpr();
    virtual qi::rule<ParseIt, T(), qi::locals<vector<T> >, qi::space_type> getTupleExpr();
    virtual qi::rule<ParseIt, T(), qi::locals<V, vector<T> >, qi::space_type> getVectorExpr();
    virtual qi::rule<ParseIt, T(), qi::locals<vector<std::pair<X, T> > >, qi::space_type> getStructExpr();
    #define get(op) virtual Rule get##op ();
    get(LiteralExpr)
    get(OpExpr)
    get(VariableExpr)
    get(FunVarExpr)
    get(CastExpr)
    get(CharLiteral)
    get(UnionExpr)
    get(MemberAccessExpr)
    get(TupleProjectionExpr)
    get(MarkerExpr)
    #undef get

private:
    // member functions providing the rules
    virtual T charLiteralHelp(char val);
    virtual T literalHelp(const V& typeExpr, const string& name);
    virtual T opHelp(const string& name);
    virtual T variableHelp(const V& type, const X& id);
    virtual T variableHelp(const X& id);
    virtual T castHelp(const V& type, const T& subExpr);
    virtual T bindExprHelp(const vector<T>& paramsExpr, T& callExpr);
    virtual T tupleHelp(const vector<T>& elements);
    virtual T vectorHelp(const V& type, const vector<T>& elements);
    virtual T structHelp(const vector<std::pair<X, T> >& elements);
    virtual T unionHelp(const V& type, const X& memberName, const T& member);
    virtual T memberAccessHelp(const T& subExpr, const X& member);
    virtual T tupleProjectionHelp(const T& subExpr, const unsigned idx);
    virtual T markerExprHelp(const T& subExpr, const unsigned id);
};

}
}
}
