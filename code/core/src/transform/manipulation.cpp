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

#include <algorithm>

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/ast_builder.h"

namespace insieme {
namespace core {
namespace transform {

using namespace std;

/**
 * A utility function to apply an arbitrary manipulation to the statements within a compound statement.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement which should be manipulated
 * @param manipulator the manipulation to be applied as a functor
 * @param preservePtrAnnotationsWhenModified if enabled, new nodes created due to the manipulation will
 * 				get a copy of the annotations of the original node by default, this feature is disabled
 * 				and it should be used with care. In case on of the resulting nodes is already present
 * 				within the manager, the present node and its version of the annotations will be preserved
 * 				and returned.
 * @return the root node of the modified AST tree (according to the root of the address)
 */
template<typename Manipulator>
NodePtr manipulate(NodeManager& manager, const CompoundStmtAddress& target, Manipulator manipulator, bool preservePtrAnnotationsWhenModified = false) {

	// get and manipulate statement list
	vector<StatementPtr> list = target->getStatements();

	// apply manipulation
	manipulator(list);

	// create and apply replacement
	CompoundStmtPtr replacement = CompoundStmt::get(manager, list);
	return replaceNode(manager, target, replacement, preservePtrAnnotationsWhenModified);
}


NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementPtr& statement, unsigned index, bool preservePtrAnnotationsWhenModified) {
	// use generic manipulation function
	return manipulate(manager, target, [index, &statement](vector<StatementPtr>& list){
		// limit index and insert element
		unsigned pos = min((unsigned)(list.size()), index);
		list.insert(list.begin() + pos, statement);
	}, preservePtrAnnotationsWhenModified);
}

NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementList& statements, unsigned index, 
		bool preservePtrAnnotationsWhenModified) {
	// use generic manipulation function
	return manipulate(manager, target, [index, &statements](vector<StatementPtr>& list){
		// limit index and insert element
		unsigned pos = min((unsigned)(list.size()), index);
		list.insert(list.begin() + pos, statements.cbegin(), statements.cend());
	}, preservePtrAnnotationsWhenModified);
}


NodePtr insertBefore(NodeManager& manager, const StatementAddress& target, const StatementPtr& statement, bool preservePtrAnnotationsWhenModified) {
	auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress());
	if(compoundParent) {
//		auto statements = compoundParent->getStatements();
//		auto targetloc = std::find(statements.cbegin(), statements.cend(), target);
//		assert(targetloc!=statements.cend() && "Could not find target Statement in compound");
//		return insert(manager, compoundParent, statement, targetloc-statements.cbegin(), preservePtrAnnotationsWhenModified);
		return insert(manager, compoundParent, statement, target.getIndex(), preservePtrAnnotationsWhenModified);
	} else {
		ASTBuilder build(manager);
		auto newCompound = build.compoundStmt(statement, target.getAddressedNode());
		return replaceNode(manager, target, newCompound, preservePtrAnnotationsWhenModified);
	}
}

NodePtr insertBefore(NodeManager& manager, const StatementAddress& target, const StatementList& statements, bool preservePtrAnnotationsWhenModified) {
	auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress());
	if(compoundParent) {
		return insert(manager, compoundParent, statements, target.getIndex(), preservePtrAnnotationsWhenModified);
	} else {
		ASTBuilder build(manager);
		StatementList allStatements;
		allStatements.insert(allStatements.begin(), statements.cbegin(), statements.cend());
		allStatements.push_back(target.getAddressedNode());
		auto newCompound = build.compoundStmt(allStatements);
		return replaceNode(manager, target, newCompound, preservePtrAnnotationsWhenModified);
	}
}

NodePtr insertAfter(NodeManager& manager, const StatementAddress& target, const StatementPtr& statement, bool preservePtrAnnotationsWhenModified) {
	auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress());
	if(compoundParent) {
		return insert(manager, compoundParent, statement, target.getIndex()+1, preservePtrAnnotationsWhenModified);
	} else {
		ASTBuilder build(manager);
		auto newCompound = build.compoundStmt(target.getAddressedNode(), statement);
		return replaceNode(manager, target, newCompound, preservePtrAnnotationsWhenModified);
	}
}

NodePtr insertAfter(NodeManager& manager, const StatementAddress& target, const StatementList& statements, bool preservePtrAnnotationsWhenModified) {
	auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress());
	if(compoundParent) {
		return insert(manager, compoundParent, statements, target.getIndex()+1, preservePtrAnnotationsWhenModified);
	} else {
		ASTBuilder build(manager);
		StatementList allStatements;
		allStatements.push_back(target.getAddressedNode());
		allStatements.insert(allStatements.end(), statements.cbegin(), statements.cend());
		auto newCompound = build.compoundStmt(allStatements);
		return replaceNode(manager, target, newCompound, preservePtrAnnotationsWhenModified);
	}
}


NodePtr remove(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, bool preservePtrAnnotationsWhenModified) {
	// use generic manipulation function
	return manipulate(manager, target, [index](vector<StatementPtr>& list){
		// remove element
		assert( index < list.size() && "Index out of range!");
		list.erase(list.begin() + index);
	}, preservePtrAnnotationsWhenModified);
}

NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementPtr& replacement, bool preservePtrAnnotationsWhenModified) {
	// use generic manipulation function
	return manipulate(manager, target, [index, replacement](vector<StatementPtr>& list){
		// remove element
		assert( index < list.size() && "Index out of range!");
		list[index] = replacement;
	}, preservePtrAnnotationsWhenModified);
}

NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementList& replacements, 
		bool preservePtrAnnotationsWhenModified) {
	// use generic manipulation function
	return manipulate(manager, target, [index, &replacements](vector<StatementPtr>& list){
		// remove element
		assert( index < list.size() && "Index out of range!");
		list.erase(list.begin() + index);
		list.insert(list.begin() + index, replacements.cbegin(), replacements.cend());
	}, preservePtrAnnotationsWhenModified);
}

NodePtr move(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, int displacement, bool preservePtrAnnotationsWhenModified) {

	// shortcut for no offset
	if (displacement == 0) {
		return target.getRootNode();
	}

	// use generic manipulation function
	return manipulate(manager, target, [index, displacement](vector<StatementPtr>& list){
		// check index
		assert( index < list.size() && "Index out of range!");

		// limit displacement
		int newPos = index + displacement;
		newPos = max(min((int)(list.size()-1), newPos), 0);

		StatementPtr element = list[index];
		list.erase(list.begin() + index);
		list.insert(list.begin() + newPos, element);
	}, preservePtrAnnotationsWhenModified);
}

namespace {

	class InlineSubstituter : public NodeMapping {

		bool successful;

		utils::map::PointerMap<VariablePtr, ExpressionPtr>& replacements;
		utils::set::PointerSet<VariablePtr> replacedOnce;

	public:

		InlineSubstituter(utils::map::PointerMap<VariablePtr, ExpressionPtr>& replacements)
			: successful(true), replacements(replacements) { }

		const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			if (!successful) {
				return ptr;
			}

			if (ptr->getNodeType() != NT_Variable) {
				if (ptr->getNodeType() == NT_LambdaExpr) {
					// entering new scope => ignore this
					return ptr;
				}
				return ptr->substitute(ptr->getNodeManager(), *this);
			}

			const VariablePtr& var = static_pointer_cast<const Variable>(ptr);

			// check whether variable has been already encountered
			if (replacedOnce.find(var) != replacedOnce.end()) {
				// variable may only be replaced once!
				successful = false;
				return ptr;
			}

			auto pos = replacements.find(var);
			if (pos != replacements.end()) {
				ExpressionPtr res = pos->second;

				// to ensure a single evaluation!
				NodeType type = res->getNodeType();
				if (type != NT_Variable && type != NT_Literal) {
					replacedOnce.insert(var);
				}
				return res;
			}

			// no modification required
			return ptr;
		}

		bool wasSuccessful() const {
			return successful;
		}
	};




	ExpressionPtr tryInlineBindToExpr(NodeManager& manager, const CallExprPtr& call) {

		// extract bind and call expression
		assert(call->getFunctionExpr()->getNodeType() == NT_BindExpr && "Illegal argument!");
		BindExprPtr bind = static_pointer_cast<const BindExpr>(call->getFunctionExpr());

		// process call recursively
		CallExprPtr innerCall = bind->getCall();
		ExpressionPtr inlined = tryInlineToExpr(manager, innerCall);

		// check for matching argument number
		auto parameter = bind->getParameters();
		auto arguments = call->getArguments();
		if (parameter.size() != arguments.size()) {
			return call;
		}

		// substituted call arguments with bind parameters
		utils::map::PointerMap<VariablePtr, ExpressionPtr> replacements;

		replacements.insert(
				make_paired_iterator(parameter.begin(), arguments.begin()),
				make_paired_iterator(parameter.end(), arguments.end())
		);

		// substitute variables within body
		InlineSubstituter substituter(replacements);
		ExpressionPtr res = static_pointer_cast<const Expression>(substituter.mapElement(0, inlined));

		// check result
		if (!substituter.wasSuccessful()) {
			return call;
		}
		return res;
	}

	ExpressionPtr tryInlineToExprInternal(NodeManager& manager, const CallExprPtr& call) {

		// Step 1 - get capture init and lambda expression
		ExpressionPtr target = call->getFunctionExpr();
		LambdaExprPtr lambda;

		// check for bind expression ...
		if (target->getNodeType() == NT_BindExpr) {
			return tryInlineBindToExpr(manager, call);
		}

		// check for lambda ...
		if (target->getNodeType() == NT_LambdaExpr) {
			lambda = static_pointer_cast<const LambdaExpr>(target);
		} else {
			// no in-lining possible
			return call;
		}

		// Step 2 - check body => has to be a return statement
		StatementPtr bodyStmt = lambda->getLambda()->getBody();

		if (CompoundStmtPtr compound = dynamic_pointer_cast<const CompoundStmt>(bodyStmt)) {
			const vector<StatementPtr>& stmts = compound->getStatements();
			if (stmts.size() == 1) {
				bodyStmt = stmts[0];
			} else {
				// no in-lining possible (to many statements)
				return call;
			}
		}

		// check for expression
		ExpressionPtr body;
		if (ReturnStmtPtr returnStmt = dynamic_pointer_cast<const ReturnStmt>(bodyStmt)) {
			body = returnStmt->getReturnExpr();
		} else {
			// no in-lining possible (not a simple expression)
			return call;
		}

		// Step 3 - collect variables replacements
		const Lambda::ParamList& paramList = lambda->getParameterList();

		utils::map::PointerMap<VariablePtr, ExpressionPtr> replacements;

		// add call parameters
		int index = 0;
		::for_each(call->getArguments(), [&](const ExpressionPtr& cur) {
			replacements.insert(std::make_pair(paramList[index++], cur));
		});

		// Step 4 - substitute variables within body
		InlineSubstituter substituter(replacements);
		ExpressionPtr res = static_pointer_cast<const Expression>(substituter.mapElement(0, body));

		// check result
		if (substituter.wasSuccessful()) {
			return res;
		}
		return call;
	}

}


ExpressionPtr tryInlineToExpr(NodeManager& manager, const CallExprPtr& call) {

	bool successful = true;
	ExpressionPtr res = call;
	while(successful && res->getNodeType() == NT_CallExpr) {
		ExpressionPtr tmp = tryInlineToExprInternal(manager, static_pointer_cast<const CallExpr>(res));
		successful = (*tmp != *res);
		res = tmp;
	}
	return res;
}

// ------------------------------ lambda extraction -------------------------------------------------------------------

namespace { 
	/**
	 * Will certainly determine the declaration status of variables inside a block.
	 */
	struct LambdaDeltaVisitor : public ASTVisitor<bool, Address> {
		utils::set::PointerSet<VariablePtr> declared;
		utils::set::PointerSet<VariablePtr> undeclared;

		// do not visit types
		LambdaDeltaVisitor() : ASTVisitor<bool, Address>(false) {}

		bool visitNode(const NodeAddress& node) { return true; } // default behaviour: continue visiting

		bool visitDeclarationStmt(const DeclarationStmtAddress &decl) {
			declared.insert(decl->getVariable());
			return true;
		}

		bool visitVariable(const VariableAddress& var) {
			auto vp = var.getAddressedNode();
			if(declared.find(vp) == declared.end()) undeclared.insert(vp);
			return true;
		}

		// due to the structure of the IR, nested lambdas can never reuse outer variables
		//  - also prevents variables in LamdaDefinition from being inadvertently captured
		bool visitLambdaExpr(const LambdaExprAddress&) {
			return false;
		}
	};

	NodePtr extractLambdaImpl(NodeManager& manager, const StatementPtr& root, bool preservePtrAnnotationsWhenModified, ASTBuilder::CaptureInits& captures,
			utils::map::PointerMap<NodePtr, NodePtr>& replacements, std::vector<VariablePtr>& passAsArguments) {
		LambdaDeltaVisitor ldv;
		visitAllPrunable(StatementAddress(root), ldv);

		// sort set to ensure code identity
		std::vector<VariablePtr> undeclared(ldv.undeclared.cbegin(), ldv.undeclared.cend());
		std::sort(undeclared.begin(), undeclared.end(), [](const VariablePtr& p1, const VariablePtr& p2) { return p1->getId() > p2->getId(); });

		ASTBuilder build(manager);
		for_each(undeclared, [&](VariablePtr p) {
			auto var = build.variable(p->getType());
			if(std::find(passAsArguments.cbegin(), passAsArguments.cend(), p) == passAsArguments.end()) 
				captures[var] = p;
			replacements[p] = var;
		});

		// replace arguments with mapped variables
		for_each(passAsArguments, [&](VariablePtr& v) { 
			if(replacements.find(v) != replacements.end()) {
				v = static_pointer_cast<const Variable>(replacements[v]);
			}
		});

		return replaceAll(manager, root, replacements, preservePtrAnnotationsWhenModified);
	}
}

BindExprPtr extractLambda(NodeManager& manager, const StatementPtr& root, bool preservePtrAnnotationsWhenModified,
		std::vector<VariablePtr> passAsArguments) {
	ASTBuilder build(manager);
	ASTBuilder::CaptureInits captures;
	utils::map::PointerMap<NodePtr, NodePtr> replacements;
	StatementPtr newStmt = static_pointer_cast<const Statement>(extractLambdaImpl(manager, root, 
		preservePtrAnnotationsWhenModified, captures, replacements, passAsArguments));
	return build.lambdaExpr(newStmt, captures, passAsArguments);
}

BindExprPtr extractLambda(NodeManager& manager, const ExpressionPtr& root, bool preservePtrAnnotationsWhenModified,
		std::vector<VariablePtr> passAsArguments) {
	ASTBuilder build(manager);
	ASTBuilder::CaptureInits captures;
	utils::map::PointerMap<NodePtr, NodePtr> replacements;
	ExpressionPtr newExpr = static_pointer_cast<const Expression>(extractLambdaImpl(manager, root, 
		preservePtrAnnotationsWhenModified, captures, replacements, passAsArguments));
	auto body = build.returnStmt(newExpr);
	return build.lambdaExpr(root->getType(), body, captures, passAsArguments);
}

LambdaExprPtr privatizeVariables(NodeManager& manager, const LambdaExprPtr& root, const std::vector<VariablePtr>& varsToPrivatize, 
		bool preservePtrAnnotationsWhenModified) {
	
	auto body = root->getBody();

	ASTBuilder build(manager);
	utils::map::PointerMap<NodePtr, NodePtr> replacements;
	for_each(varsToPrivatize, [&](VariablePtr p) {
		auto var = build.variable(p->getType());
		replacements[p] = var;
	});
	auto newBody = replaceAll(manager, body, replacements, preservePtrAnnotationsWhenModified);

	return dynamic_pointer_cast<const LambdaExpr>(newBody);
}

} // end namespace transform
} // end namespace core
} // end namespace insieme
