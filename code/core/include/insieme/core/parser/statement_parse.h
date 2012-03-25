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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/parser/ir_parse.h"

namespace insieme {
namespace core {
namespace parse {

#define Rule qi::rule<ParseIt, T(), qi::space_type>

// FW Declaration
template<typename T, typename U, typename V> struct TypeGrammar;
template<typename T, typename U, typename V, typename W, typename X, typename Y, typename Z> struct ExpressionGrammar;

// Parser usage
// T = StatementPtr
// U = ExpressionPtr
// V = TypePtr
// W = IntTypeParamPtr
// X = IdentifierPtr
// Y = LambdaPtr
// Z = LambdaDefinitionPtr
template<typename T = StatementPtr, typename U = ExpressionPtr, typename V = TypePtr, typename W = IntTypeParamPtr, typename X = StringValuePtr,
        typename Y = LambdaPtr, typename Z = LambdaDefinitionPtr>
struct StatementGrammar : public qi::grammar<ParseIt, T(), qi::space_type> {
    TypeGrammar<TypePtr, IntTypeParamPtr, StringValuePtr> *typeG;        // pointer for weak coupling
    ExpressionGrammar<U, T, V, W, X, Y, Z> *exprG;  // pointer for weak coupling
    bool deleteFields;         // flag which determines if exprG has been passed as an argument to the constructor or created inside it

    NodeManager& nodeMan;

    StatementGrammar(NodeManager& nodeMan, ExpressionGrammar<U, T, V, W, X, Y, Z>* exprGram = NULL, TypeGrammar<V, W, X>* typeGram = NULL);
    virtual ~StatementGrammar();

    qi::rule<ParseIt, T(), qi::space_type> statementRule;
    qi::rule<ParseIt, T(), qi::space_type> breakStmt;
    qi::rule<ParseIt, T(), qi::space_type> continueStmt;
    qi::rule<ParseIt, T(), qi::space_type> returnStmt;
    qi::rule<ParseIt, T(), qi::locals<vector<T> >,  qi::space_type> compoundStmt;
    qi::rule<ParseIt, T(), qi::space_type> declarationStmt;
    qi::rule<ParseIt, T(), qi::space_type> whileStmt;
    qi::rule<ParseIt, T(), qi::space_type> forStmt;
    qi::rule<ParseIt, T(), qi::space_type> ifStmt;
    qi::rule<ParseIt, T(), qi::locals<vector<std::pair<U, T> > >, qi::space_type> switchStmt;
    qi::rule<ParseIt, T(), qi::space_type> markerStmt;

    // member functions applying the rules
    virtual qi::rule<ParseIt, T(), qi::locals<vector<T> >,  qi::space_type> getCompound();
    virtual qi::rule<ParseIt, T(), qi::locals<vector<std::pair<U, T> > >, qi::space_type> getSwitch();
    #define get(op) virtual Rule get##op ();
    get(Break)
    get(Continue)
    get(Return)
    get(Declaration)
    get(While)
    get(For)
    get(If)
    get(Marker)
    #undef get

private:
    // member functions providing the rules
    virtual T breakHelp();
    virtual T continueHelp();
    virtual T returnHelp(const U& ret);

    virtual T declarationHelp(const U& varExpr, const U& initExpr);
    virtual T compoundHelp(const vector<T>& stmts);
    virtual T whileHelp(const U& condition, const T& body);
    virtual T forHelp(const T& loopVar, const U& end, const U& step, const T& body);
    virtual T forHelp(const T& loopVar, const U& end, const T& body);
    virtual T ifHelp(const U& condition, const T& body, const boost::optional<T>& elseBody);
    virtual T switchHelp(const U& switchExpr, const vector<std::pair<U, T> >& cases, const boost::optional<T>& defaultCase);
    virtual T markerHelp(const T& subStmt, const unsigned int id);
};

}
}
}
