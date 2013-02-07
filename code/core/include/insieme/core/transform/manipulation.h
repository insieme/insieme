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

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/types/substitution.h"

namespace insieme {
namespace core {

namespace transform {
	
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
 * A utility function removing the given list of statements from a code fraction. All statement
 * addresses should have the same root node. The root of the modified code fraction will be returned.
 *
 * @param manager the manager used to create the modified version
 * @param stmts the list of statements to be removed
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr remove(NodeManager& manager, const vector<StatementAddress>& stmts);

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
 * Replaces the given call expression with an expression where the computation of a bind on
 * position index is replaced a its bound values and the actual computation of the bind
 * is moved inside the lambda to be called.
 *
 * Let
 *
 * 			fun ( ..., bind(){...}, ...)
 *
 * be the call and fun a lambda expression. The resulting call will be
 *
 * 			fun' ( ..., BoundVariables(bind(){...}, ...)
 *
 * such that the bind is computed within fun' and the bound variables within the bind are passed
 * as additional arguments.
 *
 * @param manager the manager to be used to create and maintain nodes which might have to be created
 * @param call the call to be modified
 * @param index the index of the bind-argument to be pushed inside
 * @return the fixed function call
 */
CallExprPtr pushBindIntoLambda(NodeManager& manager, const CallExprPtr& call, unsigned index);

/**
 * Fixes the given variable by the given value. Each occurrences of the variable will be replaced by the given value. Further,
 * whenever, the variable is passed to a function, the parameter will be eliminated.
 *
 * @param manager the manager to be used to create and maintain nodes which might have to be created
 * @param node the code fragment within which the given variable should be fixed
 * @param var the variable to be fixed
 * @param value the value to be used instead of the variable
 */
NodePtr fixVariable(NodeManager& manager, const NodePtr& node, const VariablePtr& var, const ExpressionPtr& value);

/**
 * A generic version of the fixVariable function declared above.
 * 
 * @param manager the manager to be used to create and maintain nodes which might have to be created
 * @param node the code fragment within which the given variable should be fixed
 * @param var the variable to be fixed
 * @param value the value to be used instead of the variable
 */
template<typename T>
Pointer<T> fixVariable(NodeManager& manager, const Pointer<T>& node, const VariablePtr& var, const ExpressionPtr& value) {
	return fixVariable(manager, NodePtr(node), var, value).as<Pointer<T>>();
}

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
 * Inlines the given call expression into a statement if possible. The target of the call has to
 * be a lambda expression which's body does not contain any 'free' return, break or continue statements.
 * If the constrain to transform the call into an equivalent statement is not satisfiable, the given call
 * expression is returned.
 *
 * @param manager the manager to be used to create and maintain nodes which might have to be created
 * @param call the call expression to be inlined
 * @return the inlined expression
 */
StatementPtr tryInlineToStmt(NodeManager& manager, const CallExprPtr& call);

/**
 * Tests whether the given statement can be outlined. Statements can only be outlined if they
 * do not contain any 'free' return, break or continue statements. Hence, every return, break
 * or continue has to target a node within the given code fragment.
 *
 * @param stmt the statement to be tested
 * @return true if it could be outlined, false otherwise
 */
bool isOutlineAble(const StatementPtr& stmt);

/**
 * Outlines the given stmt by moving it into a lambda. The lambda will be requesting all free
 * variables within the statement as an argument. The call passing those parameters to the generated
 * lambda will be returned.
 *
 * @param manager the manager to be used to create and maintain nodes which might have to be created
 * @param stmt the statement to be outlined - it must not contain a break, continue or return.
 * @return a call to the function being outlined
 */
CallExprPtr outline(NodeManager& manager, const StatementPtr& stmt);

/**
 * Outlines a given expression by moving it into a isolated lambda. The lambda will request all
 * variables accessed within the given expression as an argument. The call passing those parameters
 * to the generated lambda will be returned.
 *
 * @param manager the manager to be used to create and maintain nodes which might have to be created
 * @param expr the expression to be outlined
 * @return a substitute evaluating the given expression being located within an outlined function
 */
CallExprPtr outline(NodeManager& manager, const ExpressionPtr& expr);

/** Inlines the evaluation of the given lazy expression.
 ** 
 ** @param manager the manager used to create new nodes
 ** @param lazy the target lazy expression
 ** @return the inlined expression equivalent to the lazy call (=inlineExpression(builder.call(lazy))
 ** */
ExpressionPtr evalLazy(NodeManager& manager, const ExpressionPtr& lazy);

/** Builds a lambda expression that can be called in place of [root].
 ** Captures all free variables and returns a bound expression.
 ** This is the statement version that generates an initialized lambda returning unit.
 ** 
 ** @param manager the manager used to create new nodes
 ** @param root the target statement that should form the body of the extracted lambda
 ** @return the CaptureInitExprPtr initializing the generated lambda (only valid in the calling context!)
 ** */
BindExprPtr extractLambda(NodeManager& manager, const StatementPtr& root);

/** Builds a lambda expression that can be called in place of [root].
 ** Captures all free variables and returns a bound expression.
 ** This is the statement version that generates an initialized lambda returning unit.
 **
 ** @param manager the manager used to create new nodes
 ** @param root the target statement that should form the body of the extracted lambda
 ** @param passAsArguments a list of variables that will be passed as arguments instead of captured
 ** @return the CaptureInitExprPtr initializing the generated lambda (only valid in the calling context!)
 ** */
BindExprPtr extractLambda(NodeManager& manager, const StatementPtr& root, const std::vector<VariablePtr>& passAsArguments);


LambdaExprPtr privatizeVariables(NodeManager& manager, const LambdaExprPtr& root, const std::vector<VariablePtr>& varsToPrivatize);

/**
 * Instantiates the type variables within the given lambda based on the given variable instantiation and
 * returns the same lambda, having its generic parameters fixed.
 *
 * @param manager the manager used to create new nodes
 * @param lambda the lambda to be instantiated
 * @param variableInstantiation the substitution describing the instantiation of the various type and int-param variables.
 * @return the instantiated lambda expression
 */
LambdaExprPtr instantiate(NodeManager& manager, const LambdaExprPtr& lambda, const types::SubstitutionOpt& variableInstantiation);

/**
 * Creates a top-level structure for the program prog.
 * Inserted *after* a (potential) existing top level struct
 */
DeclarationStmtPtr createGlobalStruct(NodeManager& manager, ProgramPtr& prog, const NamedValueList& globals);

/**
 * Makes the Variable var available at the scope enclosing location. Accomplished by forwarding it through the call graph.
 * This overload should be used if you don't know the exact location where the variable is available.
 *
 * @param manager the manager used to create new nodes
 * @param var the the variable to be made available
 * @param location indicating where the var should be made available
 * @param outNewRoot output parameter containing the root of the manipulated IR
 * @return the variable alias to use or a null pointer if the variable is not found
 */
//VariablePtr makeAvailable(NodeManager& manager, const VariablePtr& var, const NodeAddress& location, NodePtr& outNewRoot);

/**
 * Makes the Variable var available at the scope enclosing location. Accomplished by forwarding it through the call graph.
 * This overload should be used if you *do* know the *exact* location where the variable is available.
 *
 * @param manager the manager used to create new nodes
 * @param var the the variable to be made available
 * @param location indicating where the var should be made available
 * @param outNewRoot output parameter containing the root of the manipulated IR
 * @return the variable alias to use or a null pointer if the variable is not found
 */
//VariablePtr makeAvailable(NodeManager& manager, const VariableAddress& var, const NodeAddress& location, NodePtr& outNewRoot);

/**
 * Replaces the given expression by the given variable of the root-node context. The variable will
 * be passed as an argument through all functions between the root node and the targeted expression. If the
 * variable is already passed along, it will not be added again.
 *
 * @param manager the manager used to create new nodes
 * @param target the expression to be replaced by the variable
 * @param var the variable to be implanted
 * @return the address to the implanted variable
 */
VariableAddress pushInto(NodeManager& manager, const ExpressionAddress& target, const VariablePtr& var);

/**
 * Removes superfluous lambda arguments.
 *
 * @param manager the manager used to create new nodes
 * @param lambda to be fixed
 * @return a fixed version of the lambda
 */
LambdaExprPtr correctRecursiveLambdaVariableUsage(NodeManager& manager, const LambdaExprPtr& lambda);

/**
 * Converts the given job into a function of type ()->unit containing a parallel pfor-loop processing the
 * thread group range interval if possible. The loop will only iterate at most the number of
 * times indicated by the lower boundary of the job range.
 *
 * If a redistribute operation is triggered within the body, the conversion will
 * fail and a null-pointer will be returned.
 *
 * @param job the job to be converted into a pfor-based processing step
 * @return a lazy-expression (function without arguments) to be called for conducting the work
 * 		represented by the job or null if it can not be converted
 */
ExpressionPtr tryToPFor(const JobExprPtr& job);

} // end namespace transform
} // end namespace core
} // end namespace insieme
