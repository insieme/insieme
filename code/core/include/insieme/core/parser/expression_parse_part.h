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

typedef std::vector<std::pair<ExpressionPtr, ExpressionPtr> > GuardedStmts;
typedef vector<VariablePtr> VariableList;
typedef std::map<VariablePtr, LambdaPtr, compare_target<VariablePtr> > Definitions;
typedef std::vector<std::pair<IdentifierPtr, ExpressionPtr> > Members;

// FW Declaration
struct TypeGrammar;
struct ExpressionGrammar;

struct ExpressionGrammarPart : public qi::grammar<ParseIt, ExpressionPtr(), qi::space_type> {
    ExpressionGrammar* exprG;
    TypeGrammar *typeG; // pointer for weak coupling

    ExpressionGrammarPart(NodeManager& nodeMan, ExpressionGrammar* exprGram, TypeGrammar* typeGram);
    ~ExpressionGrammarPart();

    // terminal rules, no skip parsing
    qi::rule<ParseIt, string()> literalString;

    // nonterminal rules with skip parsing
    qi::rule<ParseIt, LiteralPtr(), qi::space_type> literalExpr;
    qi::rule<ParseIt, ExpressionPtr(), qi::space_type> opExpr;
    qi::rule<ParseIt, VariablePtr(), qi::space_type> variableExpr;
    qi::rule<ParseIt, VariablePtr(), qi::space_type> funVarExpr;

    qi::rule<ParseIt, CastExprPtr(), qi::space_type> castExpr;

    // literals -----------------------------------------------------------------------------
    qi::rule<ParseIt, LiteralPtr(), qi::space_type> charLiteral;

    // --------------------------------------------------------------------------------------

    qi::rule<ParseIt, BindExprPtr(), qi::locals<VariableList, ExpressionList>, qi::space_type> bindExpr;

    qi::rule<ParseIt, TupleExprPtr(), qi::locals<ExpressionList>, qi::space_type> tupleExpr;
    qi::rule<ParseIt, VectorExprPtr(), qi::locals<VectorTypePtr, ExpressionList>, qi::space_type> vectorExpr;
    qi::rule<ParseIt, StructExprPtr(), qi::locals<Members>, qi::space_type> structExpr;
    qi::rule<ParseIt, UnionExprPtr(), qi::space_type> unionExpr;

    qi::rule<ParseIt, MemberAccessExprPtr(), qi::space_type> memberAccessExpr;
    qi::rule<ParseIt, TupleProjectionExprPtr(), qi::space_type> tupleProjectionExpr;
    qi::rule<ParseIt, MarkerExprPtr(), qi::space_type> markerExpr;

    // --------------------------------------------------------------------------------------

    qi::rule<ParseIt, ExpressionPtr(), qi::space_type> expressionPart;
};

}
}
}
