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

#include "insieme/utils/set_constraint/solver2.h"

#include <utility>
#include <unordered_set>
#include <vector>

namespace insieme {
namespace utils {
namespace set_constraint_2 {

	using std::set;
	using std::map;
	using std::pair;
	using std::vector;


	Assignment solve(const Constraints& constraints, Assignment initial) {

		// the data structure representing the graph this algorithm is based on
		typedef map<SetID, set<const Constraint*>> Edges;

		// the work-list
		vector<SetID> workList;

		// build data structures for the graph
		Assignment res = initial;
		Edges edges;

		// 1. create list of edges
		for(auto& cur : constraints.getList()) {
			// trigger init-routines
			cur->init(res, workList);

			// add edges
			for (auto set : cur->getInputs()) {
				edges[set].insert(&*cur);
			}
		}

		// TODO: make work list unique (there are duplicates)

		// 2. solve constraints
		while(!workList.empty()) {
			// retrieve first element
			SetID head = workList.back();
			workList.pop_back();

			// process outgoing edges
			for (const Constraint* cur : edges[head]) {
				const Constraint& cc = *cur;

				// trigger update
				bool change = cc.update(res);

				// register outputs in work-list
				if (change) {
					for (auto cur : cc.getOutputs()) {
						workList.push_back(cur);
					}
				}
			}
		}

		// done
		return res;
	};


	const Assignment& LazySolver::solve(const SetID& set) {
		std::set<SetID> sets; sets.insert(set);
		return solve(sets);
	}

	const Assignment& LazySolver::solve(const std::set<SetID>& sets) {

		// create the work-list for this resolution
		vector<SetID> worklist;

		// 1. start by resolving requested sets
		resolveConstraints(sets, worklist);

		// 2. solve constraints
		while(!worklist.empty()) {
			// retrieve first element
			SetID head = worklist.back();
			worklist.pop_back();

			// process outgoing edges
			for (const Constraint* cur : edges[head]) {
				const Constraint& cc = *cur;

				// add and resolve list of used filters
				resolveConstraints(cc, worklist);

				// trigger update
				bool change = cc.update(ass);

				// register outputs in work-list
				if (change) {
					for (auto cur : cc.getOutputs()) {
						worklist.push_back(cur);
					}
				}
			}
		}


		// return assignment - TODO: find way to project to requested values
		return ass;
	}


	bool LazySolver::hasUnresolvedInput(const Constraint& cur) {

		// check cache
		auto pos = resolvedConstraints.find(&cur);
		if (pos != resolvedConstraints.end()) {
			return false;
		}

		// check dependencies
		bool hasUnresolvedInputs = any(cur.getInputs(), [&](const SetID& cur) { return resolved.find(cur) == resolved.end(); });
		if (!hasUnresolvedInputs) resolvedConstraints.insert(&cur);
		return hasUnresolvedInputs;
	}

	void LazySolver::resolveConstraints(const Constraint& cur, vector<SetID>& worklist) {

		// check whether there is anything to do
		if (!hasUnresolvedInput(cur)) return;

		// resolve constraints
		resolveConstraints(cur.getUsedInputs(ass), worklist);
	}

	void LazySolver::resolveConstraints(const std::set<SetID>& sets, vector<SetID>& worklist) {

		// if there is nothing to do => done
		if (sets.empty()) return;

		// filter out sets already resolved
		std::set<SetID> missing;
		for(auto cur : sets) {
			if (!contains(resolved, cur)) missing.insert(cur);
		}

		// check whether there is anything to do
		if (missing.empty()) return;

		// add current to resolved set
		for(auto cur : missing) {
			resolved.insert(cur);
		}

		// obtain and integrate new constraints
		std::set<SetID> nextGeneration;
		for(auto cur : resolver(missing)) {

			// copy constraint to internal list of constraints
			constraints.add(cur);

			// trigger init-routines
			cur->init(ass, worklist);

			// add edges to elements depending on
			for (auto set : cur->getInputs()) {
				edges[set].insert(&*cur);
			}

			// collect missing inputs
			if (cur->hasAssignmentDependentDependencies()) {
				// we need to obtain the used inputs
				for (auto set : cur->getUsedInputs(ass)) {
					if (!contains(resolved, set)) {
						nextGeneration.insert(set);
					}
				}
			} else {
				// this is a faster way of obtaining inputs
				for (auto set : cur->getInputs()) {
					if (!contains(resolved, set)) {
						nextGeneration.insert(set);
					}
				}
			}
		}

		// resolve next generation
		resolveConstraints(nextGeneration, worklist);
	}


	Assignment solve(const SetID& set, const ConstraintResolver& resolver, Assignment initial) {
		// use set-based interface
		std::set<SetID> sets; sets.insert(set);
		return solve(sets, resolver, initial);
	}

	// a lazy solver implementation
	Assignment solve(const std::set<SetID>& sets, const ConstraintResolver& resolver, Assignment initial) {
		//create resulting assignment
		return LazySolver(resolver, initial).solve(sets);
	}


} // end namespace set_constraint
} // end namespace utils
} // end namespace insieme
