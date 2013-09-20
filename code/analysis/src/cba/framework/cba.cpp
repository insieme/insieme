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
		  setCounter(0), idCounter(0), callSiteMgr(root),
		  set2container(), dynamicCallLabels()
	{ };

	void CBA::addConstraintsFor(const SetID& set, Constraints& res) {

		// get container
		auto pos = set2container.find(set);
		assert_true(pos != set2container.end()) << "Unknown set type encountered: " << set << "\n";

		// let container create constraints
		pos->second->addConstraintsFor(*this, set, res);
	}


	const CBA::OptCallSiteList& CBA::getAllStaticUses(const core::ExpressionAddress& fun) {

		// check the cache
		auto pos = callSiteCache.find(fun);
		if (pos != callSiteCache.end()) {
			return pos->second;
		}

		// compute call-site list
		const vector<Caller>& callers = getCallSiteManager().getCaller(Callee(fun));
		if (callers == getCallSiteManager().getFreeCallers(fun->getType().as<FunctionTypePtr>()->getParameterTypes().size())) {
			return callSiteCache[fun] = CBA::OptCallSiteList();
		}


		vector<Label> res;
		for(auto cur : callers) {
			res.push_back(getLabel(cur.getCall()));
		}
		return callSiteCache[fun] = res;
	}

	CBA::OptCallSiteList CBA::getAllStaticPredecessors(const core::CallExprAddress& call) {

		// TODO: clean up this mess ..

		static const CBA::OptCallSiteList root = toVector<Label>(0);
		auto fun = getSurroundingFreeFunction(call);
		if (!fun) {
			auto res = root;

			if (isRecursiveCall(call)) {
				// add current label to candidate list (todo: also add labels of all other recursive calls in the definition)
				res->push_back(getLabel(call));
			}

			return res;
		}

		// get static uses
		auto res = getAllStaticUses(fun);
		if (!res) return res;

		// if static uses are clear, we also have to add recursive cases
		if (isRecursiveCall(call)) {
			// add current label to candidate list (todo: also add labels of all other recursive calls in the definition)
			res->push_back(getLabel(call));
		}

		return res;
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
