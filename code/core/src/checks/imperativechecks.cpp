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

#include "insieme/core/checks/imperativechecks.h"

#include <boost/unordered_set.hpp>

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {
namespace checks {

#define CAST(TargetType, value) \
	static_pointer_cast<const TargetType>(value)


// the actual implementation of the undeclared variable check
namespace {

	typedef boost::unordered_set<VariablePtr, hash_target<VariablePtr>, equal_target<VariablePtr>> VariableSet;

	/**
	 * A use-once check for declared variables.
	 */
	class VarDeclarationCheck : public ASTVisitor<void, Address> {

		/**
		 * The set of currently declared variables.
		 */
		VariableSet& declaredVariables;

		/**
		 * The address of nodes referencing undeclared variables.
		 */
		std::vector<VariableAddress> undeclaredVariableUsage;

	public:

		VarDeclarationCheck(VariableSet& predefined) : declaredVariables(predefined), undeclaredVariableUsage() {}

		std::vector<VariableAddress>& getUndeclaredVariableUsages() {
			return undeclaredVariableUsage;
		}

	protected:

		void visitVariable(const VariableAddress& cur) {
			// check whether variable has been declared
			if (declaredVariables.find(cur.getAddressedNode()) == declaredVariables.end()) {
				// => not declared => add to list of undeclared variables
				undeclaredVariableUsage.push_back(cur);
			}
		}

		/**
		 * A special handling of declaration statements, which are introducing new variables.
		 */
		void visitDeclarationStmt(const DeclarationStmtAddress& cur) {
			// first => recursive check of initialization expression.
			visit(cur->getInitialization());

			// second: add newly declared variable to set of declared variables
			declaredVariables.insert(cur->getVariable());
		}

		/**
		 * Terminates this check for undeclared variables at the borderline
		 * to an alternative scope.
		 */
		void visitJob(const JobExprAddress& cur) {
			// only check initialization expressions of job
			handleNewScope(cur);
		}

		/**
		 * Terminates this check for undeclared variables at the borderline
		 * to an alternative scope.
		 */
		void visitLambda(const LambdaExprAddress& cur) {
			// only check initialization of capture list
			handleNewScope(cur);
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
			for (int i=0, e=node->getChildList().size(); i<e; i++) {
				visit(node.getAddressOfChild(i));
			}
		}

	private:

		/**
		 * Takes the given node and checks all the initialization expressions within
		 * its child list.
		 */
		void handleNewScope(const NodeAddress& cur) {
			// get immediate declaration child nodes of the given node
			for (int i=0, e=cur->getChildList().size(); i<e; i++) {
				NodeAddress child = cur.getAddressOfChild(i);
				if (child->getNodeType() == NT_DeclarationStmt) {
					visit(static_address_cast<const DeclarationStmt>(child)->getInitialization());
				}
			}

			// TODO: determine which scope the initializer of a job-branch function should have!

		}

	};


	OptionalMessageList checkLambda(const LambdaExprAddress& cur) {
		// assemble set of defined variables
		VariableSet declared;
		for_each(cur->getCaptureList(), [&declared](const DeclarationStmtPtr& cur) {
			declared.insert(cur->getVariable());
		});

		auto paramList = cur->getParams();
		declared.insert(paramList.begin(), paramList.end());

		// run check
		VarDeclarationCheck check(declared);
		check.visit(cur);

		// use results
		OptionalMessageList res;
		std::vector<VariableAddress> errors = check.getUndeclaredVariableUsages();
		if (errors.empty()) {
			return res;
		}

		// convert into messages
		for_each(errors, [&](const VariableAddress& cur) {
			add(res, Message(cur,
						EC_IMPERATIVE_UNDECLARED_VARIABLE_USAGE,
						format("Access to undeclared variable %s of type %s",
								toString(*(cur.getAddressedNode())).c_str(),
								toString(*(cur->getType())).c_str()),
						Message::ERROR));
		});

		return res;
	}

	OptionalMessageList checkJob(const JobExprAddress& cur) {

	}
}


OptionalMessageList UndeclaredVariableCheck::visitNode(const NodeAddress& address) {

	// to check: are all variables used within constructs declared properly
	// - variable scopes: functions, jobs; declarations are only valid after their specification

	OptionalMessageList res;

	switch(address->getNodeType()) {
	case NT_LambdaExpr:
		res = checkLambda(static_address_cast<const LambdaExpr>(address));
		break;
	case NT_JobExpr:
		res = checkJob(static_address_cast<const JobExpr>(address));
		break;
	default: ;
	}

	return res;
}


#undef CAST

} // end namespace check
} // end namespace core
} // end namespace insieme
