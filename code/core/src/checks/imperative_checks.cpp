/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/checks/imperative_checks.h"
#include "insieme/core/printer/pretty_printer.h"

#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {
namespace checks {

#define CAST(TargetType, value) static_pointer_cast<const TargetType>(value)


	// the actual implementation of the undeclared variable check
	namespace {

		using std::vector;

		typedef boost::unordered_set<VariablePtr, hash_target<VariablePtr>, equal_target<VariablePtr>> VariableSet;

		/**
		 * A use-once check for declared variables.
		 */
		class VarDeclarationCheck : public IRVisitor<void, Address> {
			/**
			 * The set of currently declared variables.
			 */
			VariableSet declaredVariables;

			/**
			 * The address of nodes referencing undeclared variables.
			 */
			std::vector<VariableAddress> undeclaredVariableUsage;

		  public:
			VarDeclarationCheck(VariableSet& predefined) : IRVisitor<void, Address>(false), declaredVariables(predefined), undeclaredVariableUsage() {}

			std::vector<VariableAddress>& getUndeclaredVariableUsages() {
				return undeclaredVariableUsage;
			}

		  protected:
			void visitVariable(const VariableAddress& cur) {
				// check whether variable has been declared
				if(declaredVariables.find(cur.getAddressedNode()) == declaredVariables.end()) {
					// => add to list of undeclared variables
					undeclaredVariableUsage.push_back(cur);
				}
			}

			/**
			 * A special handling of declaration statements, which are introducing new variables.
			 */
			void visitDeclarationStmt(const DeclarationStmtAddress& cur) {
				// first: add newly declared variable to set of declared variables (in order to be able to use them in the initialization already)
				declaredVariables.insert(cur->getVariable());

				// second => recursive check of initialization expression.
				visit(cur->getInitialization());
			}

			/**
			 * A special handling of for loops, which are introducing a iterator variable.
			 */
			void visitForStmt(const ForStmtAddress& cur) {
				// first => recursive check of boundaries
				visit(cur->getStart());
				visit(cur->getEnd());
				visit(cur->getStep());

				// add iterator variable to set of declared variables
				VariablePtr iterator = cur->getIterator();
				declaredVariables.insert(iterator);

				// check body
				visit(cur->getBody());

				// remove iterator again
				declaredVariables.erase(iterator);
			}

			/**
			 * A special handling for catch clauses which are introducing a variable capturing the exception.
			 */
			void visitCatchClause(const CatchClauseAddress& cur) {
				// add exception varialbe to list of declared variables
				VariablePtr var = cur->getVariable();
				declaredVariables.insert(var);

				// check body
				visit(cur->getBody());

				// remove iterator again
				declaredVariables.erase(var);
			}

			/**
			 * A special handling of scopes ... Variables defined within the given scope
			 * are removed from the defined set after the block.
			 */
			void visitCompoundStmt(const CompoundStmtAddress& cur) {
				// copy current scope
				VariableSet currentScope = declaredVariables;

				// check compound stmts (new scope) ...
				visitNode(cur);

				// reset current scope
				declaredVariables = currentScope;
			}

			/**
			 * Terminates this check for undeclared variables at the borderline
			 * to an alternative scope.
			 */
			void visitJobExpr(const JobExprAddress& cur) {
				// backup current scope
				VariableSet currentScope = declaredVariables;

				visit(cur->getThreadNumRange());

				// .. and check the body
				visit(cur->getBody());

				// restore context scope
				declaredVariables = currentScope;
			}

			/**
			 * Terminates this check for undeclared variables at the borderline to an
			 * alternative scope.
			 */
			void visitLambdaExpr(const LambdaExprAddress& cur) {
				// nothing to do => terminal state
			}

			/**
			 * Adds the parameters of the bind expression temporarily to the set of defined
			 * variables and verifies the usage of variables in the nested call expression.
			 */
			void visitBindExpr(const BindExprAddress& cur) {

				// copy current scope
				VariableSet currentScope = declaredVariables;

				// add bind parameters
				for(const auto& param : cur.getAddressedNode()->getParameters()) {
					declaredVariables.insert(param);
				}

				// check nested call statement
				visit(cur->getCall());

				// reset current scope
				declaredVariables = currentScope;

			}

			/**
			 * Prune search tree by ignoring types.
			 */
			void visitType(const TypeAddress&) {}

			/**
			 * Implements the default behavior for all other nodes, which is about
			 * recursively descending to all children - depth first.
			 */
			void visitNode(const NodeAddress& node) {
				// progress recursively by default
				for(int i = 0, e = node.getAddressedNode()->getChildList().size(); i < e; i++) {
					visit(node.getAddressOfChild(i));
				}
			}
		};

		OptionalMessageList conductCheck(VarDeclarationCheck& check, const NodeAddress& root) {
			// run check
			check.visit(root);

			// use results
			OptionalMessageList res;
			std::vector<VariableAddress> errors = check.getUndeclaredVariableUsages();
			if(errors.empty()) { return res; }

			// convert into messages
			for_each(errors, [&](const VariableAddress& cur) {
				add(res, Message(cur, EC_IMPERATIVE_UNDECLARED_VARIABLE_USAGE,
				                 format("Access to undeclared variable %s of type %s", toString(*(cur.getAddressedNode())).c_str(),
				                        toString(*(cur->getType())).c_str()),
				                 Message::ERROR));
			});

			return res;
		}
	}


	OptionalMessageList UndeclaredVariableCheck::visitLambdaDefinition(const LambdaDefinitionAddress& lambdaDef) {
		OptionalMessageList res;

		LambdaReferenceSet recFunctions;
		for (const auto& cur : lambdaDef.getAddressedNode()) {
			recFunctions.insert(cur->getReference());
		}

		for_each(lambdaDef->getDefinitions(), [&](const LambdaBindingAddress& cur) {

			// assemble set of defined variables
			VariableSet declared;

			// add parameters
			auto paramList = cur.getAddressedNode()->getLambda()->getParameterList();
			declared.insert(paramList.begin(), paramList.end());

			// run check on body ...
			VarDeclarationCheck check(declared);

			// trigger check
			addAll(res, conductCheck(check, cur->getLambda()));
		});

		return res;
	}


	#undef CAST

} // end namespace check
} // end namespace core
} // end namespace insieme
