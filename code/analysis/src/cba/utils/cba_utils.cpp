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

	ExpressionAddress getSurroundingFreeFunction(const NodeAddress& cur) {
		static const ExpressionAddress none;

		// move up until reaching requested function
		if (cur.isRoot()) return none; // there is none

		// stop decent at lambda or binds
		auto type = cur->getNodeType();
		if (type != NT_LambdaExpr && type != NT_BindExpr) {
			return getSurroundingFreeFunction(cur.getParentAddress());
		}

		auto fun = cur.as<ExpressionAddress>();

		// check whether function is a free function
		if (fun.isRoot()) return none;

		// if lambda is not directly called it is a free function
		auto user = fun.getParentAddress();
		auto call = user.isa<CallExprAddress>();
		if (!call || call->getFunctionExpr() != fun) return fun;

		// otherwise continue search
		return getSurroundingFreeFunction(user);
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


	bool isMemoryConstructor(const StatementAddress& address) {
		StatementPtr stmt = address;

		// literals of a reference type are memory locations
		if (auto lit = stmt.isa<LiteralPtr>()) {
			return lit->getType().isa<RefTypePtr>();
		}

		// memory allocation calls are
		return core::analysis::isCallOf(stmt, stmt->getNodeManager().getLangBasic().getRefAlloc());
	}

	ExpressionAddress getLocationDefinitionPoint(const core::StatementAddress& stmt) {
		assert(isMemoryConstructor(stmt));

		// globals are globals => always the same
		if (auto lit = stmt.isa<LiteralPtr>()) {
			return LiteralAddress(lit);
		}

		// locations created by ref.alloc calls are created at the call side
		assert(stmt.isa<CallExprAddress>());
		return stmt.as<CallExprAddress>();
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

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
