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

#include "insieme/core/ir_program.h"
#include "insieme/core/parser/ir_parse.h"

namespace insieme {
namespace core {
namespace parse {

// parser usage:
// T = ProgramPtr
// U = ExpressionPtr
template <typename P = ProgramPtr, typename T = ExpressionPtr, typename U = StatementPtr, typename V = TypePtr, typename W = IntTypeParamPtr,
        typename X = StringValuePtr, typename Y = LambdaPtr, typename Z = LambdaDefinitionPtr>
struct ProgramGrammar : public qi::grammar<ParseIt, P(), qi::space_type> {
    ExpressionGrammar<T, U, V, W, X, Y, Z> *exprG;   // pointer for weak coupling

    NodeManager& nodeMan;

    ProgramGrammar(NodeManager& nMan);
    virtual ~ProgramGrammar();

    qi::rule<ParseIt, P(), qi::space_type> programRule;
    qi::rule<ParseIt, P(), qi::locals<vector<T> >, qi::space_type> program;

    // member functions applying the rules
    qi::rule<ParseIt, P(), qi::locals<vector<T> >, qi::space_type> getProgram();
    qi::rule<ParseIt, P(), qi::space_type> getProgramRule();

    // member functions providing the rules
    P mainProgramHelp(const T& mainProg);
    P programHelp(const vector<T>& progs);

};


// parser usage:
// T = ProgramPtr
// U = ExpressionPtr
template <typename P = ProgramPtr, typename T = ExpressionPtr, typename U = StatementPtr, typename V = TypePtr, typename W = IntTypeParamPtr,
        typename X = StringValuePtr, typename Y = LambdaPtr, typename Z = LambdaDefinitionPtr>
struct IRGrammar : public qi::grammar<ParseIt, NodePtr(), qi::space_type> {
    ProgramGrammar<P, T, U, V, W, X, Y, Z> *progG;			// pointer for weak coupling
    StatementGrammar<U, T, V, W, X, Y, Z> *stmtG;

    NodeManager& nodeMan;

    IRGrammar(NodeManager& nMan);
    ~IRGrammar();

    qi::rule<ParseIt, NodePtr(), qi::space_type> irRule;
    qi::rule<ParseIt, P(), qi::space_type> mainProg;
    qi::rule<ParseIt, T(), qi::space_type> optVarExpr;
    qi::rule<ParseIt, Y(), qi::locals<vector<T> >, qi::space_type> lambda;


    // member functions applying the rules
    virtual qi::rule<ParseIt, Y(), qi::locals<vector<T> >, qi::space_type> getLambda();
    qi::rule<ParseIt, NodePtr(), qi::space_type> getIRRule();

private:
    virtual T optVarHelp(const V& type, const X& id);
    virtual T optVarHelp(const X& id);
    virtual Y lambdaHelp(const V& retType, const vector<T>& paramsExpr, const U& body);


};

}
}
}
