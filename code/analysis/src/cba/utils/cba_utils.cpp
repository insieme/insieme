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

#include "insieme/analysis/cba/utils/cba_utils.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	NodeAddress getSurroundingFreeFunction(const NodeAddress& cur) {
		static const ExpressionAddress none;

		// move up until reaching requested function
		if (cur.isRoot()) return none; // there is none

		// stop decent at lambda or binds
		auto type = cur->getNodeType();
		if (type != NT_Lambda && type != NT_BindExpr) {
			return getSurroundingFreeFunction(cur.getParentAddress());
		}

		auto fun = cur;

		// check whether function is a free function
		if (fun.isRoot()) return none;

		// if lambda is not directly called it is a free function
		auto user = (type == NT_Lambda) ? fun.getParentAddress(4) : fun.getParentAddress();
		auto call = user.isa<CallExprAddress>();
		if (!call || call->getFunctionExpr() != fun) return fun;

		// otherwise continue search
		return getSurroundingFreeFunction(user);
	}

	LambdaAddress getSurroundingRecursiveFunction(const NodeAddress& cur) {
		static const LambdaAddress none;

		// check whether current node is a function
		if (cur->getNodeType() == NT_Lambda) {

			auto lambda = cur.as<LambdaAddress>();
			auto lambdaVar = cur.getParentAddress(1).as<LambdaBindingPtr>()->getVariable();
			auto lambdaDef = cur.getParentAddress(2).as<LambdaDefinitionAddress>();

			if (lambdaDef->isRecursive(lambdaVar)) {
				return lambda;
			}
		}

		// check for root nodes
		if (cur.isRoot()) return none;

		// process recursively
		return getSurroundingRecursiveFunction(cur.getParentAddress());
	}

	vector<ExpressionAddress> getAllFreeFunctions(const core::NodeAddress& root) {
		vector<ExpressionAddress> res;

		// collect all terms in the code
		visitDepthFirst(root, [&](const ExpressionAddress& cur) {

			// only interested in lambdas and binds
			if (!(cur.isa<LambdaExprPtr>() || cur.isa<BindExprPtr>())) return;

			// must not be root
			if (cur.isRoot()) return;

			// it must not be the target of a call expression
			auto parent = cur.getParentAddress();
			if (auto call = parent.isa<CallExprAddress>()) {
				if (call->getFunctionExpr() == cur) {
					return;
				}
			}

			// keep this one
			res.push_back(cur);
		});

		return res;
	}


	VariableAddress getDefinitionPoint(const VariableAddress& varAddress) {

		// extract the variable
		VariablePtr var = varAddress.getAddressedNode();

		// start walking up the address
		NodeAddress cur = varAddress;

		// check the parent
		while (!cur.isRoot()) {
			auto pos = cur.getIndex();
			cur = cur.getParentAddress();
			switch(cur->getNodeType()) {

			case NT_Parameters: {
				return varAddress;	// this variable is a parameter definition
			}

			case NT_Lambda: {

				// check parameters
				for(auto param : cur.as<LambdaAddress>()->getParameters()) {
					if (param.as<VariablePtr>() == var) {
						return param;		// found it
					}
				}

				// otherwise continue with parent
				break;
			}

			case NT_LambdaBinding: {
				// check the bound variable
				auto boundVar = cur.as<LambdaBindingAddress>()->getVariable();
				if (boundVar.as<VariablePtr>() == var) {
					return boundVar;
				}

				// keep on searching
				break;
			}

			case NT_LambdaDefinition: {
				// check whether variable is bound
				if (auto binding = cur.as<LambdaDefinitionAddress>()->getBindingOf(var)) {
					return binding->getVariable();
				}

				// keep on searching
				break;
			}

			case NT_BindExpr: {
				// check parameters
				for(auto param : cur.as<BindExprAddress>()->getParameters()) {
					if (param.as<VariablePtr>() == var) {
						return param;		// found it
					}
				}

				// not here
				break;
			}

			case NT_CompoundStmt: {

				// check whether there is an earlier declaration
				auto compound = cur.as<CompoundStmtAddress>();
				for(int i = pos; i >= 0; i--) {
					if (auto decl = compound[i].isa<DeclarationStmtAddress>()) {
						if (decl->getVariable().as<VariablePtr>() == var) {
							return decl->getVariable();
						}
					}
				}

				// otherwise continue with parent
				break;
			}

			case NT_ForStmt: {
				// check whether it is an iterator
				auto iter = cur.as<ForStmtAddress>()->getIterator();
				if (var == iter.as<VariablePtr>()) {
					return iter;
				}
				// otherwise continue with parent
				break;
			}

			default: break;
			}
		}

		// the variable is a free variable in this context
		return VariableAddress(var);
	}


	StatementPtr getRootStmt(const NodeAddress& node) {
		auto stmt = node.as<StatementPtr>();
		if (node.isRoot()) return stmt;
		auto res = getRootStmt(node.getParentAddress());
		return (res) ? res : stmt;
	}

	bool isRecursiveCall(const CallExprAddress& call) {
		if (!call) return false;

		// check function
		auto fun = call->getFunctionExpr();

		// needs to be a variable
		auto var = fun.isa<VariableAddress>();
		if (!var) return false;

		// and this variable needs to be a recursive function
		auto def = getDefinitionPoint(var);
		return !def.isRoot() && def.getParentNode().isa<LambdaBindingPtr>();
	}
//
//	LambdaDefinitionAddress getRecursiveDefinition(const CallExprAddress& call) {
//		assert(isRecursiveCall(call));
//
//		// get definition point
//		auto var = call->getFunctionExpr().as<VariableAddress>();
//		auto def = getDefinitionPoint(var);
//
//		// get enclosing definition
//		return def.getParentAddress(2).as<LambdaDefinitionAddress>();
//
//	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
