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

#include "insieme/analysis/cba/framework/cba.h"

#include "insieme/analysis/cba/utils/cba_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	const StateSetType Sin("Sin");			// in-state of statements
	const StateSetType Sout("Sout");		// out-state of statements
	const StateSetType Stmp("Stmp");		// temporary states of statements (assignment only)


	using std::set;

	using namespace core;

	CBA::CBA(const StatementAddress& root)
		: root(root),
		  solver([&](const set<SetID>& sets) {
				Constraints res;
				for (auto set : sets) {
					this->addConstraintsFor(set, res);
//					std::cout << "Resolving set " << set << "...\n";
//					Constraints tmp;
//					this->addConstraintsFor(set, tmp);
//					std::cout << "Constraints: " << tmp << "\n";
//					if(!all(tmp, [&](const ConstraintPtr& cur) { return set == cur->getOutputs()[0]; })) { std::cout << "WARNING\n"; }
//					res.add(tmp);
				}
				return res;
		  }),
		  setCounter(0), idCounter(0) {
//
//		// fill dynamicCalls
//		core::visitDepthFirst(root, [&](const CallExprAddress& call) {
//			auto fun = call->getFunctionExpr();
//			if (fun.isa<LiteralPtr>() || fun.isa<LambdaExprPtr>() || fun.isa<BindExprPtr>()) return;
//			this->dynamicCalls.push_back(call);
//		});
//
//		// fill dynamic call labels
//		dynamicCallLabels = ::transform(dynamicCalls, [&](const CallExprAddress& cur) { return getLabel(cur); });
//		dynamicCallLabels.push_back(0);
//
//		// obtain list of callable functions
//		callables = getAllCallableTerms(*this, root);
//
//		// and a list of all free functions
//		freeFunctions.clear();
//		for(const auto& cur : callables) {
//			if (!contains(freeFunctions, cur.definition)) {
//				freeFunctions.push_back(cur.definition);
//			}
//		}
//
//		// and a list of all memory locations
//		locations = getAllLocations(*this, root);

	};

	void CBA::addConstraintsFor(const SetID& set, Constraints& res) {

		// get container
		auto pos = set2container.find(set);
		assert_true(pos != set2container.end()) << "Unknown set type encountered: " << set << "\n";

		// let container create constraints
		pos->second->addConstraintsFor(*this, set, res);
	}


	namespace {

		bool collectUsesOfVariable(CBA& context, const VariableAddress& var, vector<Label>& res) {
			assert(getDefinitionPoint(var) == var);

			NodeAddress root;
			if (auto decl = var.getParentAddress().isa<DeclarationStmtAddress>()) {
				root = decl.getParentAddress();
			} else if (auto params = var.getParentAddress().isa<ParametersAddress>()) {
				if (auto lambda = params.getParentAddress().isa<LambdaAddress>()) {
					root = lambda->getBody();
				} else if (auto bind = params.getParentAddress().isa<BindExprAddress>()) {
					root = bind->getCall();
				} else {
					assert_fail() << "Unknown parent type for Parameters: " << params.getParentAddress()->getNodeType();
				}
			} else {
				assert_fail() << "Unknown parent of variable definition: " << var.getParentAddress()->getNodeType();
			}

			// there should be a root now
			assert(root);

			bool allFine = true;
			visitDepthFirstPrunable(root, [&](const ExpressionAddress& cur) {
				// stop if already failed
				if (!allFine) return true;

				// only process local scope
				if (cur->getNodeType() == NT_LambdaExpr) return true;

				// for the rest, only interested in variables
				if (*cur != *var) return false;

				// if variable is used as a function => found a call
				auto call = cur.getParentAddress().isa<CallExprAddress>();

				// if it is not a call, we don't care
				if (!call) return false;

				// check out whether it is a call to the function or passed as an argument
				if (call->getFunctionExpr() == cur) {
					// it is the target function => collect this one
					res.push_back(context.getLabel(call));
				} else {
					// it is an argument
					if (auto fun = call->getFunctionExpr().isa<LambdaExprAddress>()) {
						assert(call[cur.getIndex()-2] == cur);
						// ok - it is a static call => we may follow the parameter
						allFine = allFine && collectUsesOfVariable(context, fun->getParameterList()[cur.getIndex()-2], res);
					}
				}

				return false;

			});

			return allFine;

		}

		CBA::OptCallSiteList getUsesOfVariable(CBA& context, const VariableAddress& def) {
			static const CBA::OptCallSiteList fail;
			vector<Label> res;
			bool success = collectUsesOfVariable(context, def, res);
			return success ? res : fail;
		}

		CBA::OptCallSiteList getStaticUses(CBA& context, const ExpressionAddress& function) {
			static const CBA::OptCallSiteList unknown;

			// there is nothing we can do for the root
			if (function.isRoot()) return unknown;

			// option A: the lambda is created as an argument of a call expression
			auto parent = function.getParentAddress();
			if (auto call = parent.isa<CallExprAddress>()) {
				assert(call->getFunctionExpr() != function);

				// check whether target function is fixed
				if (auto fun = call->getFunctionExpr().isa<LambdaExprAddress>()) {
					// collect all uses of corresponding function parameter
					assert(call[function.getIndex()-2] == function);
					return getUsesOfVariable(context, fun->getParameterList()[function.getIndex()-2]);
				} else {
					return unknown;
				}
			}

			// option B: the lambda is created as the value of a definition
			if (auto decl = parent.isa<DeclarationStmtAddress>()) {
				// simply collect all uses of the variable
				return getUsesOfVariable(context, decl->getVariable());
			}

			return unknown;
		}

		bool isFunction(const core::ExpressionAddress& expr) {
			return expr->getNodeType() == NT_LambdaExpr || expr->getNodeType() == NT_BindExpr;
		}

		ExpressionAddress tryObtainingFunction(const core::ExpressionAddress& expr) {
			static const ExpressionAddress unknown;

			// check for null
			if (!expr) return unknown;

			// check whether we already have one
			if (isFunction(expr)) return expr;

			// otherwise we are only supporting variables
			auto var = expr.isa<VariableAddress>();
			if (!var) return unknown;

			// get definition of variable
			auto def = getDefinitionPoint(var);

			// if it is a free variable => there is nothing we can do
			if (def.isRoot()) return unknown;

			// if variable is declared => consider declaration
			auto parent = def.getParentAddress();
			if (auto decl = parent.isa<DeclarationStmtAddress>()) {
				return tryObtainingFunction(decl->getInitialization());
			}

			// if it is an lambda parameter => follow argument
			if (auto param = parent.isa<ParametersAddress>()) {

				if (param.isRoot()) return unknown;

				auto userOffset = param.getParentNode().isa<LambdaPtr>() ? 5 : 2;
				auto user = param.getParentAddress(userOffset);

				// continue with proper argument
				auto call = user.isa<CallExprAddress>();
				if (call) return tryObtainingFunction(call[param.getIndex()]);
			}

			// otherwise there is nothing we can do
			return unknown;
		}

	}


	const CBA::OptCallSiteList& CBA::getAllStaticUses(const core::ExpressionAddress& fun) {

		// check the cache
		auto pos = callSiteCache.find(fun);
		if (pos != callSiteCache.end()) {
			return pos->second;
		}

		// compute call-site list
		return callSiteCache[fun] = getStaticUses(*this, fun);
	}

	const CBA::OptCallSiteList& CBA::getAllStaticPredecessors(const core::StatementAddress& stmt) {
		static const CBA::OptCallSiteList root = toVector<Label>(0);
		auto fun = getSurroundingFreeFunction(stmt);
		if (!fun) {
			return root;
		}

		return getAllStaticUses(fun);
	}

	const CBA::ContextFreeCallableList& CBA::getContextFreeCallableCandidate(const core::CallExprAddress& call) {
		// only supported for known dynamic calls
		assert(contains(getDynamicCalls(), call));

		auto pos = contextFreeCallableCandidateCache.find(call);
		if (pos != contextFreeCallableCandidateCache.end()) {
			return (pos->second) ? *pos->second : freeFunctions;
		}

		// compute free functions
		auto& res = contextFreeCallableCandidateCache[call];
		if (auto fun = tryObtainingFunction(call->getFunctionExpr())) {
			res = toVector(fun);
		}
		return (res) ? *res : freeFunctions;
	}

	void CBA::plot(std::ostream& out) const {
		const Constraints& constraints = solver.getConstraints();
		const Assignment& ass = solver.getAssignment();


		// get solutions as strings
		auto solution = ass.toStringMap();

		out << "digraph G {";

		// print names of all sets
		for(auto cur : indices) {
			cur.second->plot(*this, solution, out);
		}

		// print constraints
		for(auto cur : constraints) {
			out << "\n\t";
			cur->writeDotEdge(out, ass);
		}

		out << "\n}\n";
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
