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

#include "insieme/analysis/cba/framework/cba.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	CBA& getCBA(const NodeAddress& node) {
		typedef std::shared_ptr<CBA> CBA_Ptr;

		// obtain CBA context from root node
		core::StatementAddress root = getAnalysisRoot(node);
		if (!root->hasAttachedValue<CBA_Ptr>()) {
			root->attachValue<CBA_Ptr>(std::make_shared<CBA>(root));
		}

		// run analysis
		return *root->getAttachedValue<CBA_Ptr>();
	}

	NodeInstance getSurroundingFreeFunction(const NodeInstance& cur) {
		static const ExpressionInstance none;

		// move up until reaching requested function
		if (cur.isRoot()) return none; // there is none

		// stop decent at lambda or binds
		auto type = cur->getNodeType();
		if (type != NT_Lambda && type != NT_BindExpr) {
			return getSurroundingFreeFunction(cur.getParentInstance());
		}
		auto fun = cur;

		// check whether function is a free function
		if (fun.isRoot()) return none;

		// if lambda is not directly called it is a free function
		auto user = (type == NT_Lambda) ? fun.getParentInstance(4) : fun.getParentInstance();

		// if user is a program, it is not a free function
		if (auto prog = user.isa<ProgramPtr>()) return none;

		// if lambda is used as an argument to a call => it is a free function
		auto call = user.isa<CallExprInstance>();
		if (!call) return fun;
		if (auto callTrg = call->getFunctionExpr().isa<LambdaExprInstance>()) {
			if (callTrg->getLambda() != fun) return fun;
		}

		// otherwise continue search
		return getSurroundingFreeFunction(user);
	}

	LambdaInstance getSurroundingRecursiveFunction(const NodeInstance& cur) {
		static const LambdaInstance none;

		// check whether current node is a function
		if (cur->getNodeType() == NT_Lambda) {

			auto lambda = cur.as<LambdaInstance>();
			auto lambdaVar = cur.getParentInstance(1).as<LambdaBindingPtr>()->getVariable();
			auto lambdaDef = cur.getParentInstance(2).as<LambdaDefinitionInstance>();

			if (lambdaDef->isRecursive(lambdaVar)) {
				return lambda;
			}
		}

		// check for root nodes
		if (cur.isRoot()) return none;

		// process recursively
		return getSurroundingRecursiveFunction(cur.getParentInstance());
	}

	vector<ExpressionInstance> getAllFreeFunctions(const core::NodeInstance& root) {
		vector<ExpressionInstance> res;

		// collect all terms in the code
		visitDepthFirst(root, [&](const ExpressionInstance& cur) {

			// only interested in lambdas and binds
			if (!(cur.isa<LambdaExprPtr>() || cur.isa<BindExprPtr>())) return;

			// must not be root
			if (cur.isRoot()) return;

			// it must not be the target of a call expression
			auto parent = cur.getParentInstance();
			if (auto call = parent.isa<CallExprInstance>()) {
				if (call->getFunctionExpr() == cur) {
					return;
				}
			}

			// keep this one
			res.push_back(cur);
		});

		return res;
	}


	VariableInstance getDefinitionPoint(const VariableInstance& varAddress) {

		// extract the variable
		VariablePtr var = varAddress.getAddressedNode();

		// start walking up the address
		NodeInstance cur = varAddress;

		// check the parent
		while (!cur.isRoot()) {
			auto pos = cur.getIndex();
			cur = cur.getParentInstance();
			switch(cur->getNodeType()) {

			case NT_Parameters: {
				return varAddress;	// this variable is a parameter definition
			}

			case NT_Lambda: {

				// check parameters
				for(auto param : cur.as<LambdaInstance>()->getParameters()) {
					if (param.as<VariablePtr>() == var) {
						return param;		// found it
					}
				}

				// otherwise continue with parent
				break;
			}

			case NT_LambdaBinding: {
				// check the bound variable
				auto boundVar = cur.as<LambdaBindingInstance>()->getVariable();
				if (boundVar.as<VariablePtr>() == var) {
					return boundVar;
				}

				// keep on searching
				break;
			}

			case NT_LambdaDefinition: {
				// check whether variable is bound
				if (auto binding = cur.as<LambdaDefinitionInstance>()->getBindingOf(var)) {
					return binding->getVariable();
				}

				// keep on searching
				break;
			}

			case NT_BindExpr: {
				// check parameters
				for(auto param : cur.as<BindExprInstance>()->getParameters()) {
					if (param.as<VariablePtr>() == var) {
						return param;		// found it
					}
				}

				// not here
				break;
			}

			case NT_CompoundStmt: {

				// check whether there is an earlier declaration
				auto compound = cur.as<CompoundStmtInstance>();
				for(int i = pos; i >= 0; i--) {
					if (auto decl = compound[i].isa<DeclarationStmtInstance>()) {
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
				auto iter = cur.as<ForStmtInstance>()->getIterator();
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
		return VariableInstance(var);
	}

	StatementInstance getAnalysisRoot(const NodeInstance& node) {
		// the root node is the outermost statement (or expression, since expressions are statements)
		auto stmt = node.isa<StatementInstance>();
		if (node.isRoot()) return stmt;
		auto res = getAnalysisRoot(node.getParentInstance());
		// special case - use body of lambdas
		if (auto lambda = res.isa<LambdaExprInstance>()) return lambda->getBody();
		// otherwise take outermost
		return (res) ? res : stmt;
	}

	bool isRecursiveCall(const CallExprInstance& call) {
		if (!call) return false;

		// check function
		auto fun = call->getFunctionExpr();

		// needs to be a variable
		auto var = fun.isa<VariableInstance>();
		if (!var) return false;

		// and this variable needs to be a recursive function
		auto def = getDefinitionPoint(var);
		return !def.isRoot() && def.getParentNode().isa<LambdaBindingPtr>();
	}

	bool isCapturedValue(const core::ExpressionInstance& value) {

		// check whether there are at least 2 parents (call and bind expression)
		if (value.getDepth() < 2) return false;

		// it has to be the argument of a call expression within a bind expression
		auto bind = value.getParentInstance(2).isa<BindExprInstance>();
		return bind && bind->isBoundExpression(value);
	}

	bool isSynchronizingFunction(const core::ExpressionPtr& fun) {
		const auto& base = fun->getNodeManager().getLangBasic();
		return  base.isParallel(fun) ||
				base.isMerge(fun) ||
				base.isMergeAll(fun) ||
				base.isChannelSend(fun) ||
				base.isChannelRecv(fun) ||
				base.isRedistribute(fun);
	}

	namespace detail {

		bool isThreadBody(const StatementInstance& stmt) {
			// the root is a thread starter
			if (getAnalysisRoot(stmt) == stmt) return true;

			// if it is the body of a free lambda it is also a thread starter
			auto freeFun = getSurroundingFreeFunction(stmt);
			if (!freeFun) return false;

			// in case the surrounding callable is a bind => handle it
			if (auto bind = freeFun.isa<BindExprInstance>()) {
				return stmt == bind->getCall();
			}

			// in case it it is a function, handle it as well
			if (auto lambda = freeFun.isa<LambdaInstance>()) {
				return stmt == lambda->getBody();
			}

			// it is not a thread starter otherwise
			return false;
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
