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

#include "insieme/core/checks/imperative_checks.h"

#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {
namespace checks {

#define CAST(TargetType, value) \
	static_pointer_cast<const TargetType>(value)


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

		VarDeclarationCheck(VariableSet& predefined)
			: IRVisitor<void, Address>(false), declaredVariables(predefined), undeclaredVariableUsage() {}

		std::vector<VariableAddress>& getUndeclaredVariableUsages() {
			return undeclaredVariableUsage;
		}

	protected:

		void visitVariable(const VariableAddress& cur) {
			// check whether variable has been declared
			if (declaredVariables.find(cur.getAddressedNode()) == declaredVariables.end()) {

				// check whether it is a recursive function variable
				auto visitor = makeLambdaVisitor([&](const LambdaDefinitionPtr& def)->bool {
					return def->getDefinitionOf(cur);
				});
				bool isRecVariable = visitPathBottomUpInterruptible(cur, visitor);

				// => not declared => add to list of undeclared variables
				if (!isRecVariable) undeclaredVariableUsage.push_back(cur);
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

			// check scope of all local declarations
			VariableSet localVars;
			std::size_t numDecls = cur->getLocalDecls().size();
			for (std::size_t i=0; i<numDecls; i++) {
				const DeclarationStmtAddress decl = cur->getLocalDecls()->getElement(i);

				// check variables within local variable initialization
				visit(decl->getInitialization());

				// ... and collect local variables
				localVars.insert(decl->getVariable());
			}

			// add local variables to set of declared variables ..
			declaredVariables.insert(localVars.begin(), localVars.end());

			// .. and check job branches specifications
			std::size_t numChildren = cur->getChildList().size();
			for (std::size_t i=1+numDecls; i<numChildren; i++) {
				// check scopes within current child
				visit(cur->getThreadNumRange());
			}

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
		 * Checks the special variable scope rules within a bind expression. The function
		 * for which arguments should be bound as well as non-parameter arguments to the
		 * resulting call have to be part of the local scope.
		 */
		void visitBindExpr(const BindExprAddress& cur) {
			// get list of parameters
			const vector<VariablePtr>& params = cur.getAddressedNode()->getParameters()->getElements();

			// check call expressions
			const CallExprAddress call = cur->getCall();

			// start with function
			visit(call->getFunctionExpr());

			// check parameters
			std::size_t numArgs = call->getArguments().size();
			for (std::size_t i=0; i<numArgs; i++) {
				const ExpressionAddress cur = call->getArgument(i);

				// check whether variable is a bind-parameter
				if (cur->getNodeType() == NT_Variable
					&& contains(params, static_pointer_cast<const Variable>(cur.getAddressedNode()))) {
					continue;
				}

				// check whether all variables of the expression are within the current scope
				visit(cur);
			}
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
			for (int i=0, e=node.getAddressedNode()->getChildList().size(); i<e; i++) {
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

}


OptionalMessageList UndeclaredVariableCheck::visitLambdaDefinition(const LambdaDefinitionAddress& lambdaDef) {

	OptionalMessageList res;

	VariableSet recFunctions;
	for_each(lambdaDef.getAddressedNode()->getDefinitions(), [&recFunctions](const LambdaBindingPtr& cur) {
		recFunctions.insert(cur->getVariable());
	});

	for_each(lambdaDef->getDefinitions(), [&](const LambdaBindingAddress& cur) {

		// assemble set of defined variables
		VariableSet declared;

		// add recursive function variables
		declared.insert(recFunctions.begin(), recFunctions.end());

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


// ------------------------------ Declared Once Check -----------------------

namespace {

	class SingleDeclarationCheck : public IRCheck {

		/**
		 * The type used to link variables to the node containing their declaration.
		 */
		typedef boost::unordered_map<VariablePtr, const NodePtr, hash_target<VariablePtr>, equal_target<VariablePtr>> DeclarationMap;

		/**
		 * The map linking variables to their declaration.
		 */
		DeclarationMap declarations;

	public:

		/**
		 * A simple constructor for this check.
		 */
		SingleDeclarationCheck() : IRCheck(false) {}

		/**
		 * Visits a declaration - if the same variable has already been
		 * declared somewhere else, an error will be added to the message list.
		 */
		OptionalMessageList visitDeclarationStmt(const DeclarationStmtAddress& cur) {
			OptionalMessageList res;

			// just test declard variable
			testVariable(res, cur->getVariable(), cur, (cur.getDepth()>1)?cur.getParentNode():cur.getAddressedNode());

			return res;
		}

//		OptionalMessageList visitLambdaExpr(const LambdaExprAddress& cur) {
//			OptionalMessageList res;
//
//			// test all captured variables ...
//			for_each(cur->getCaptureList(), [&](const DeclarationStmtPtr& decl) {
//				testVariable(res, decl->getVariable(), cur, cur.getAddressedNode());
//			});
//
//			// ... and parameters
//			for_each(cur->getParams(), [&](const VariablePtr& param) {
//				testVariable(res, param, cur, cur.getAddressedNode());
//			});
//
//			return res;
//		}
//
//		OptionalMessageList visitRecLambdaDefinition(const RecLambdaDefinitionAddress& cur) {
//			OptionalMessageList res;
//
//			// test variables used as recursive functions ...
//			for_each(cur->getDefinitions(), [&](const std::pair<VariablePtr, LambdaExprPtr> definition) {
//				testVariable(res, definition.first, cur, cur.getAddressedNode());
//			});
//
//			return res;
//		}

	private:

		void testVariable(OptionalMessageList& res, const VariablePtr& var, const NodeAddress& current, const NodePtr& context) {
			// try register variable ...
			auto info = declarations.insert(std::make_pair(var, context));

			// check whether variable has been declared before at a different place
			if (info.second || *context == *((info.first)->second)) {
				// not yet registered, everything is fine.
				return;
			}

			// register failed => already declared
			add(res, Message(current,
					EC_IMPERATIVE_ILLEGAL_VARIABLE_REUSE,
					format("Illegal re-use of variable %s of type %s",
							toString(*(var)).c_str(),
							toString(*(var->getType())).c_str()),
					Message::WARNING));
		}

	};


}


OptionalMessageList DeclaredOnceCheck::visitNode(const NodeAddress& address) {
	// use private instance of single declaration check
	CheckPtr check = makeVisitOnce(make_check<SingleDeclarationCheck>());
	return check->visit(address);
}


#undef CAST

} // end namespace check
} // end namespace core
} // end namespace insieme
