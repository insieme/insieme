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

#include "insieme/core/expressions.h"
#include "insieme/core/parser/ir_parse.h"

namespace insieme {
namespace core {
namespace parse {

typedef vector<StatementPtr> Stmts;
typedef vector<std::pair<ExpressionPtr, StatementPtr> > Cases;
#define Rule qi::rule<ParseIt, T(), qi::space_type>

// FW Declaration
template<typename V> struct TypeGrammar;
template<typename U> struct ExpressionGrammar;

template<typename T>
struct StatementGrammar : public qi::grammar<ParseIt, T(), qi::space_type> {
    TypeGrammar<TypePtr> *typeG;        // pointer for weak coupling
    ExpressionGrammar<ExpressionPtr> *exprG;  // pointer for weak coupling
    bool deleteFields;         // flag which determines if exprG has been passed as an argument to the constructor or created inside it

    NodeManager& nodeMan;

    StatementGrammar(NodeManager& nodeMan, ExpressionGrammar<ExpressionPtr>* exprGram = NULL, TypeGrammar<TypePtr>* typeGram = NULL);
    ~StatementGrammar();

    qi::rule<ParseIt, T(), qi::space_type> statementRule;
    qi::rule<ParseIt, T(), qi::space_type> breakStmt;
    qi::rule<ParseIt, T(), qi::space_type> continueStmt;
    qi::rule<ParseIt, T(), qi::space_type> returnStmt;
    qi::rule<ParseIt, T(), qi::locals<Stmts>,  qi::space_type> compoundStmt;
    qi::rule<ParseIt, T(), qi::space_type> declarationStmt;
    qi::rule<ParseIt, T(), qi::space_type> whileStmt;
    qi::rule<ParseIt, T(), qi::space_type> forStmt;
    qi::rule<ParseIt, T(), qi::space_type> ifStmt;
    qi::rule<ParseIt, T(), qi::locals<Cases>, qi::space_type> switchStmt;
    qi::rule<ParseIt, T(), qi::space_type> markerStmt;

    // member functions applying the rules
    virtual qi::rule<ParseIt, T(), qi::locals<Stmts>,  qi::space_type> getCompound();
    virtual qi::rule<ParseIt, T(), qi::locals<Cases>, qi::space_type> getSwitch();
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
    virtual T returnHelp(ExpressionPtr ret);

    virtual T declarationHelp(ExpressionPtr varExpr, ExpressionPtr initExpr);
    virtual T compoundHelp(Stmts stmts);
    virtual T whileHelp(ExpressionPtr condition, StatementPtr body);
    virtual T forHelp(StatementPtr loopVar, ExpressionPtr end, ExpressionPtr step, StatementPtr body);
    virtual T ifHelp(const ExpressionPtr& condition, const StatementPtr& body, const boost::optional<StatementPtr>& elseBody);
    virtual T switchHelp(const ExpressionPtr& switchExpr, const Cases& cases, const boost::optional<StatementPtr>& defaultCase);
    virtual T markerHelp(const StatementPtr& subStmt, const unsigned int id);
};

}
}
}
