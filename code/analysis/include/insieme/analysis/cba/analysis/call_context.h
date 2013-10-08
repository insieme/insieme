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
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/analysis/cba/analysis/functions.h"

namespace insieme {
namespace analysis {
namespace cba {

	// -------------------- Context Constraints ------------

	template<typename C> class ContextPredecessorGenerator;
	typedef TypedSetType<Label,ContextPredecessorGenerator> ContextPredecessorType;

	extern const ContextPredecessorType pred;

	// -------------------------------------- Context Predecessor Constraints -----------------------------

	template<typename Context>
	class ContextPredecessorGenerator : public ConstraintGenerator<Context> {

		CBA& cba;

	public:

		ContextPredecessorGenerator(CBA& cba) : ConstraintGenerator<Context>(cba), cba(cba) {}

	private:

		set<Label> getReachingPredecessor(const CallExprAddress& call) {

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
				auto lambda = recFun.as<LambdaAddress>();
				auto lambdaVar = recFun.getParentAddress(1).as<LambdaBindingPtr>()->getVariable();
				auto lambdaDef = recFun.getParentAddress(2).as<LambdaDefinitionAddress>();

				for(const auto& recCall : lambdaDef->getRecursiveCallsOf(lambdaVar)) {
					res.insert(cba.getLabel(lambdaDef >> recCall));
				}
			}

			return res;
		}

	public:

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
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

			// TODO: old version had additional optimizations

//std::cout << " # Processing Call: " << call << " = " << *call << "\n";
//			// get surrounding free function
//			auto fun = getSurroundingFreeFunction(call);
//
//			// check whether there is a surrounding free function
//			if (!fun) {
//std::cout << " - no surrounding free function\n";
//				// => this function can only reached statically
//				constraints.add(elem(0, pred_res));
//
//				// consider potential recursive case
//				if (auto recLambda = getSurroundingRecursiveFunction(call)) {
//
//					// get recursive calls
//					for (const Caller& cur : cba.getCallSiteManager().getCaller(Callee(recLambda))) {
//
//						// check whether it is a recursive call
//						const auto& call = cur.getCall();
//						if (causesContextShift(call)) continue;
//
//						// add call site to potential label
//						auto l_call = cba.getLabel(call);
//						constraints.add(elem(l_call, pred_res));
//					}
//				}
//
//				return;
//			}
//
//			// wrap targeted function into a callee instance
//			Callee callee(fun);
//
//			// get call sites for surrounding function
//			const vector<Caller>& caller = cba.getCallSiteManager().getCaller(callee);
//
//			// special case: all calls to functions are statically bound => callee is not free
//			if (!cba.getCallSiteManager().isFree(callee)) {
//std::cout << " - it is not calling a free function\n";
//std::cout << " - callee: " << callee.getDefinition() << " = " << *callee.getDefinition() << "\n";
//std::cout << " - caller: " << caller << "\n";
//
//				// just add caller-contexts to the resulting set
//				for(const CallExprAddress& cur : caller) {
//
//					// if the call is a
//					if (causesContextShift(cur)) {
//						auto l_call = cba.getLabel(cur);
//						std::cout << "Adding " << cur << " = " << l_call << "\n";
//						constraints.add(elem(l_call, pred_res));
//					} else {
//
//					}
//
//				}
//
//				// and done
//				return;
//			}
//std::cout << " - it is calling a free function\n";
//			// all kind of calling-contexts need to be considered
//			auto callContexts = generateSequences<Context::call_context::size>(cba.getDynamicCallLabels());
//
//			// the dynamic callers are creating new contexts
//			for(const CallExprAddress& cur : caller) {
//
//				// ignore static calls
//				if (!causesContextShift(cur)) continue;
//
//				auto l_call = cba.getLabel(cur);
//				auto l_fun = cba.getLabel(cur->getFunctionExpr());
//
//				// for all potential source-contexts (context of call site)
//				for(const auto& callCtxt : callContexts) {
//
//					auto F_dynCall = cba.getSet(F, l_fun, Context(callCtxt));
//					constraints.add(elemIf(callee, F_dynCall, l_call, pred_res));
//				}
//			}

		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
