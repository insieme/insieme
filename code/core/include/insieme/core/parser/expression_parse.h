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

namespace insieme {
namespace core {
namespace parse {

typedef std::vector<std::pair<ExpressionPtr, ExpressionPtr> > GuardedStmts;
typedef vector<VariablePtr> VariableList;
typedef std::map<VariablePtr, LambdaPtr, compare_target<VariablePtr> > Definitions;
typedef std::vector<std::pair<Identifier, ExpressionPtr> > Members;
// FW Declaration
struct TypeGrammar;

class VariableTable {
	NodeManager& nodeMan;
	std::map<Identifier, VariablePtr> table;

public:
	VariableTable(NodeManager& nodeMan) : nodeMan(nodeMan) { }

	VariablePtr get(const TypePtr& typ, const Identifier& id);
};

struct ExpressionGrammar : public qi::grammar<ParseIt, ExpressionPtr(), qi::space_type> {
	TypeGrammar *typeG; // pointer for weak coupling
	VariableTable varTab;
	
	ExpressionGrammar(NodeManager& nodeMan);
	~ExpressionGrammar();

	// terminal rules, no skip parsing
	qi::rule<ParseIt, string()> literalString;

	// nonterminal rules with skip parsing
	qi::rule<ParseIt, LiteralPtr(), qi::space_type> literalExpr;
	qi::rule<ParseIt, ExpressionPtr(), qi::space_type> opExpr;
	qi::rule<ParseIt, VariablePtr(), qi::space_type> variableExpr;
    qi::rule<ParseIt, VariablePtr(), qi::space_type> funVarExpr;
	
	qi::rule<ParseIt, CallExprPtr(), qi::locals<ExpressionList>, qi::space_type> callExpr;
	qi::rule<ParseIt, CastExprPtr(), qi::space_type> castExpr;

	qi::rule<ParseIt, ExpressionPtr(), qi::space_type> expressionRule;

	// literals -----------------------------------------------------------------------------
	qi::rule<ParseIt, LiteralPtr(), qi::space_type> charLiteral;

	// --------------------------------------------------------------------------------------
    qi::rule<ParseIt, LambdaPtr(), qi::locals<VariableList, VariableList>, qi::space_type> lambda;
    qi::rule<ParseIt, LambdaDefinitionPtr(), qi::locals<Definitions>, qi::space_type> lambdaDef;
    qi::rule<ParseIt, LambdaExprPtr(), qi::space_type> lambdaExpr;

    qi::rule<ParseIt, CaptureInitExprPtr(), qi::locals<std::vector<ExpressionPtr> >, qi::space_type> captureInitExpr;

    qi::rule<ParseIt, JobExprPtr(), qi::locals</*vector<DeclarationStmt>,*/ GuardedStmts>, qi::space_type> jobExpr;
    qi::rule<ParseIt, TupleExprPtr(), qi::locals<ExpressionList>, qi::space_type> tupleExpr;
	qi::rule<ParseIt, VectorExprPtr(), qi::locals<VectorTypePtr, ExpressionList>, qi::space_type> vectorExpr;
	qi::rule<ParseIt, StructExprPtr(), qi::locals<Members>, qi::space_type> structExpr;
    qi::rule<ParseIt, UnionExprPtr(), qi::space_type> unionExpr;
    // --------------------------------------------------------------------------------------
};

}
}
}
