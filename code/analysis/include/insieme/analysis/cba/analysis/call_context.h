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

#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/framework/generator/data_value_constraint_generator.h"

#include "insieme/analysis/cba/analysis/functions.h"

namespace insieme {
namespace analysis {
namespace cba {

	// -------------------- Context Constraints ------------

	template<typename C> class ContextPredecessorGenerator;

	struct context_predecessor_analysis : public set_analysis<Label, ContextPredecessorGenerator> {};

	extern const context_predecessor_analysis pred;

	// -------------------------------------- Context Predecessor Constraints -----------------------------

	template<typename C, typename ... E> class DataValueConstraintGenerator;

	template<typename Context>
	class ContextPredecessorGenerator : public DataValueConstraintGenerator<Context> {			// TODO: change the base-type to something without context

		CBA& cba;

	public:

		ContextPredecessorGenerator(CBA& cba) : DataValueConstraintGenerator<Context>(cba), cba(cba) {}

	private:

		set<Label> getReachingPredecessor(const CallExprInstance& call) {

			set<Label> res;

			// get surrounding function
			auto fun = getSurroundingFreeFunction(call);
			if (!fun) {
				// not a free function, can only by called by the root context
				res.insert(0);
			} else {
				// add all potential callers
				for(const auto& caller : cba.getCallSiteManager().getCaller(Callee(fun))) {
					const auto& call = caller.getCall();
					if (causesContextShift(call)) {
						res.insert(cba.getLabel(call));
					} else {
						for(Label l : getReachingPredecessor(call)) {
							res.insert(l);
						}
					}
				}
			}

			// check whether it is a recursive function
			if (auto recFun = getSurroundingRecursiveFunction(call)) {

				// get list of recursive calls
				auto lambda = recFun.as<LambdaInstance>();
				auto lambdaVar = recFun.getParentInstance(1).as<LambdaBindingPtr>()->getVariable();
				auto lambdaDef = recFun.getParentInstance(2).as<LambdaDefinitionInstance>();

				for(const auto& recCall : lambdaDef->getRecursiveCallsOf(lambdaVar)) {
					res.insert(cba.getLabel(concat(lambdaDef, recCall)));
				}
			}

			return res;
		}

	public:

		void visitCallExpr(const CallExprInstance& call, const Context& ctxt, Constraints& constraints) {
			// restrict context
			assert_true(ctxt == Context()) << "This resolver only operates on the default context - given: " << ctxt;

			// check that call is a dynamic call
			auto funType = call->getFunctionExpr()->getNodeType();
			assert(funType != NT_LambdaExpr && funType != NT_BindExpr);
			if (funType == NT_LambdaExpr || funType == NT_BindExpr) return;		// not interested

			// fill predecessor set
			auto pred_res = cba.getSet(pred, cba.getLabel(call));

			// collect a list of all potential predecessors
			for(auto cur : getReachingPredecessor(call)) {
				constraints.add(elem(cur, pred_res));
			}

		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
