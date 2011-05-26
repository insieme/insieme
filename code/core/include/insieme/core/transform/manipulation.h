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

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_address.h"

namespace insieme {
namespace core {
namespace transform {

// TODO: merge this file and the node replacer.h

/**
 * A utility function to insert a statement within a compound statement block.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement within which the element should be inserted
 * @param statement the statement to be inserted
 * @param index the index at which the element should be inserted (0 ... before current node 0)
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementPtr& statement, unsigned index);

/**
 * A utility function to insert a list of statements within a compound statement block.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement within which the element should be inserted
 * @param statements the statements to be inserted
 * @param index the index at which the element should be inserted (0 ... before current node 0)
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementList& statements, unsigned index);

/**
 * A utility function to insert a statement before another statement.
 * If the target statement is contained within a compound statement the supplied statement will be inserted,
 * otherwise a new compound statement will be generated
 *
 * @param manager the manager used to create new nodes
 * @param target The statement before which the new statement should be inserted
 * @param statement the statement to be inserted
 * @return the root node of the modified AST tree (according to the root of the target address)
 */
NodePtr insertBefore(NodeManager& manager, const StatementAddress& target, const StatementPtr& statement);

/**
 * A utility function to insert a list of statements before another statement.
 * If the target statement is contained within a compound statement the supplied statements will be inserted,
 * otherwise a new compound statement will be generated
 *
 * @param manager the manager used to create new nodes
 * @param target The statement before which the new statement should be inserted
 * @param statements the statements to be inserted
 * @return the root node of the modified AST tree (according to the root of the target address)
 */
NodePtr insertBefore(NodeManager& manager, const StatementAddress& target, const StatementList& statements);

/**
 * A utility function to insert a statement after another statement.
 * If the target statement is contained within a compound statement the supplied statement will be inserted,
 * otherwise a new compound statement will be generated
 *
 * @param manager the manager used to create new nodes
 * @param target The statement after which the new statement should be inserted
 * @param statement the statement to be inserted
 * @return the root node of the modified AST tree (according to the root of the target address)
 */
NodePtr insertAfter(NodeManager& manager, const StatementAddress& target, const StatementPtr& statement);

/**
 * A utility function to insert a list of statements after another statement.
 * If the target statement is contained within a compound statement the supplied statements will be inserted,
 * otherwise a new compound statement will be generated
 *
 * @param manager the manager used to create new nodes
 * @param target The statement before which the new statement should be inserted
 * @param statements the statements to be inserted
 * @return the root node of the modified AST tree (according to the root of the target address)
 */
NodePtr insertAfter(NodeManager& manager, const StatementAddress& target, const StatementList& statements);

/**
 * A utility function to remove a statement from a compound statement block.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement from which a statement should be removed
 * @param index the index of the statement to be removed (counting starts with 0)
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr remove(NodeManager& manager, const CompoundStmtAddress& target, unsigned index);

/**
 * A utility function to move a statement within a compound statement block.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement which should be manipulated
 * @param index the index of the statement to be moved (counting starts with 0)
 * @param displacement the amount of displacement (-1 .. one up, 2 .. two down)
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr move(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, int displacement);

/**
 * A utility function replacing a statement within a compound statement block with another statement.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement which should be altered
 * @param index the index of the statement to be replaced (counting starts with 0)
 * @param replacement the statement to be inserted as a replacement
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementPtr& replacement);

/**
 * A utility function replacing a statement within a compound statement block with a list of statements.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement which should be altered
 * @param index the index of the statement to be replaced (counting starts with 0)
 * @param replacements the statements to be inserted as a replacement
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementList& replacements);


/**
 * Tries to inline the given function call into an expression. Hence, the result will be an equivalent
 * expression not calling another function. However, if the constrain to transform the call into an
 * equivalent expression is not satisfiable, the given call expression is returned.
 *
 * @param manager the manager to be used to create and maintain nodes which might have to be created
 * @param call the call expression to be inlined
 * @return the inlined expression
 */
ExpressionPtr tryInlineToExpr(NodeManager& manager, const CallExprPtr& call);

/**
 * Replaces the parameter with the given index within the given lambda by the given value and returns
 * the resulting, restructured lambda. This will effect the inner structure as well as the type of the
 * given lambda. If fixing the parameter is not possible (e.g. if the argument is forwarded to some
 * external function / literal) the original lambda will be returned.
 *
 * @param manager the manager to be used to create and maintain nodes which might have to be created
 * @param lambda the lambda for which a parameter should be replaced
 * @param index the index of the parameter to be replaced
 * @param value the value to be used instead of the parameter (needs to be a value of the same type)
 */
LambdaExprPtr tryFixParameter(NodeManager& manager, const LambdaExprPtr& lambda, unsigned index, const ExpressionPtr& value);

/**
 * Fixes the given variable by the given value. Each occurrences of the variable will be replaced by the given value. Further,
 * whenever, the variable is passed to a function, the parameter will be eliminated.
 *
 * @param manager the manager to be used to create and maintain nodes which might have to be created
 * @param statement the statement within which the given variable should be fixed
 * @param var the variable to be fixed
 * @param value the value to be used instead of the variable
 */
StatementPtr fixVariable(NodeManager& manager, const StatementPtr& statement, const VariablePtr& var, const ExpressionPtr& value);

/** Builds a lambda expression that can be called in place of [root].
 ** Captures all free variables and returns a capture init expression.
 ** This is the statement version that generates an initialized lambda returning unit.
 ** 
 ** @param manager the manager used to create new nodes
 ** @param root the target statement that should form the body of the extracted lambda
 ** @param passAsArguments an optional list of variables that will be passed as arguments instead of captured
 ** @return the CaptureInitExprPtr initializing the generated lambda (only valid in the calling context!)
 ** */
BindExprPtr extractLambda(NodeManager& manager, const StatementPtr& root, std::vector<VariablePtr> passAsArguments = toVector<VariablePtr>());

/** Builds a lambda expression that can be called in place of [root].
 ** Captures all free variables and returns a capture init expression.
 ** This is the expression version that generates an initialized lambda returning the same value/type root would have returned.
 ** 
 ** @param manager the manager used to create new nodes
 ** @param root the target expression that should form the return value of the extracted lambda
 ** @param passAsArguments an optional list of variables that will be passed as arguments instead of captured
 ** @return the CaptureInitExprPtr initializing the generated lambda (only valid in the calling context!)
 ** */
BindExprPtr extractLambda(NodeManager& manager, const ExpressionPtr& root, std::vector<VariablePtr> passAsArguments = toVector<VariablePtr>());

LambdaExprPtr privatizeVariables(NodeManager& manager, const LambdaExprPtr& root, const std::vector<VariablePtr>& varsToPrivatize);

} // end namespace transform
} // end namespace core
} // end namespace insieme
