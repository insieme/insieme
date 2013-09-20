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

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
			// restrict context
			assert_true(ctxt == Context()) << "This resolver only operates on the default context - given: " << ctxt;

			// check that call is a dynamic call
			auto funType = call->getFunctionExpr()->getNodeType();
			assert(funType != NT_LambdaExpr && funType != NT_BindExpr);
			if (funType == NT_LambdaExpr || funType == NT_BindExpr) return;		// not interested

			// fill predecessor set
			auto pred_res = cba.getSet(pred, cba.getLabel(call));

			// get surrounding free function
			auto fun = getSurroundingFreeFunction(call);

			// check whether there is a surrounding free function
			if (!fun) {
				// => this function can only reached statically
				constraints.add(elem(0, pred_res));

				// consider potential recursive case
				if (isRecursiveCall(call)) {
					// TODO: add all recursive calls within definition block!
					constraints.add(elem(cba.getLabel(call), pred_res));
				}

				return;
			}

			// wrap targeted function into a callee instance
			Callee callee(fun);

			// get call sites for surrounding function
			const vector<Caller>& caller = cba.getCallSiteManager().getCaller(callee);

			// special case: all calls to functions are statically bound => callee is not free
			if (!cba.getCallSiteManager().isFree(callee)) {

				// just add caller-contexts to resulting set
				for(const CallExprAddress& cur : caller) {
					auto l_call = cba.getLabel(cur);
					constraints.add(elem(l_call, pred_res));
				}

				// and done
				return;
			}

			// all kind of calling-contexts need to be considered
			auto callContexts = generateSequences<Context::call_context::size>(cba.getDynamicCallLabels());

			// the dynamic callers are creating new contexts
			for(const CallExprAddress& cur : caller) {

				// ignore static calls
				if (!causesContextShift(cur)) continue;

				auto l_call = cba.getLabel(cur);
				auto l_fun = cba.getLabel(cur->getFunctionExpr());

				// for all potential source-contexts (context of call site)
				for(const auto& callCtxt : callContexts) {

					auto F_dynCall = cba.getSet(F, l_fun, Context(callCtxt));
					constraints.add(elemIf(callee, F_dynCall, l_call, pred_res));
				}
			}

		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
