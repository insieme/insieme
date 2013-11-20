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

#include "insieme/analysis/cba/framework/call_string_filter.h"

#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/framework/call_site_manager.h"

#include "insieme/core/ir_visitor.h"

#include "insieme/utils/assert.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::set;

	CallStringFilter::CallStringFilter(CBA& cba) : cba(cba) {

		// zero is always included
		allCallsSet.insert(0);
		allCallsList.push_back(0);

		// collect all dynamic call sites
		core::visitDepthFirst(cba.getRoot(), [&](const CallExprAddress& cur) {
			// collect all calls causing a context switch
			if (!causesContextShift(cur)) return;

			auto l = cba.getLabel(cur);
			allCallsSet.insert(l);
			allCallsList.push_back(l);
		});

	}


	const set<Label>& CallStringFilter::getAllPotentialPredecessors(const Label& label) {
		static const set<Label> empty;

		// check whether label is a dynamic call site
		if (allCallsSet.find(label) == allCallsSet.end()) return empty;

		// check map
		auto pos = predecessors.find(label);
		if (pos != predecessors.end()) {
			return pos->second;
		}

		// compute, cache and return result
		return predecessors[label] = computePotentialPredecessors(label);
	}

	set<Label> CallStringFilter::computePotentialPredecessors(const Label& label) {

		// special case: label = 0
		if (label == 0) {
			set<Label> res;
			res.insert(0);
			return res;
		}

		// in case the label is not a dynamic call site, use the empty set
		if (allCallsSet.find(label) == allCallsSet.end()) {
			return set<Label>();
		}

		// get call and check validity
		auto stmt = cba.getStmt(label);
		assert_true(stmt.isa<CallExprAddress>()) << "\nLabel: " << label << "\nStmt: " << stmt;
		auto call = stmt.as<CallExprAddress>();
		assert_true(causesContextShift(call)) << "\nLabel: " << label << "\nCall: " << call << " = " << *call << "\nDynamicCallSites: " << allCallsList << "\n";

		// the rest is done by the other signature
		return computePotentialPredecessors(call.as<StatementAddress>());
	}

	std::set<Label> CallStringFilter::computePotentialPredecessors(const StatementAddress& stmt) {

		// get surrounding free function
		auto fun = getSurroundingFreeFunction(stmt);

		// get locations surrounding function might get used
		set<Label> res;
		if (!fun) {
			// there is no free surrounding function => only root context can reach this call
			res.insert(0);
		} else {
			// get all potential uses of the surrounding function
			res = getAllStaticUses(fun);
		}

		// handle recursive calls
		if (auto recFun = getSurroundingRecursiveFunction(stmt)) {

			// get all callers of the lambda
			for (const Caller& cur : cba.getCallSiteManager().getCaller(Callee(recFun))) {

				const auto& call = cur.getCall();

				// check whether this call is creating a fresh context
				if (causesContextShift(call)) {
					res.insert(cba.getLabel(call));
				} else {
					// get surrounding contexts
					const auto& outerCtxts = computePotentialPredecessors(call.as<StatementAddress>());
					res.insert(outerCtxts.begin(), outerCtxts.end());
				}
			}
		}

		return res;
	}

	set<Label> CallStringFilter::getAllStaticUses(const Callee& fun) {

		// compute call-site list
		const vector<Caller>& callers = cba.getCallSiteManager().getCaller(fun);
		if (callers == cba.getCallSiteManager().getFreeCallers(fun.getNumParams())) {
			return allCallsSet;
		}

		set<Label> res;
		for(const CallExprAddress& cur : callers) {
			if (causesContextShift(cur)) {
				res.insert(cba.getLabel(cur));
			} else {
				// get surrounding contexts
				const auto& outerCtxts = computePotentialPredecessors(cur.as<StatementAddress>());
				res.insert(outerCtxts.begin(), outerCtxts.end());
			}
		}
		return res;
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
