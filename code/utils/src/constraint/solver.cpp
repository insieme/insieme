/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/utils/constraint/solver.h"

#include "insieme/utils/assert.h"

#include <utility>
#include <unordered_set>
#include <vector>

namespace insieme {
namespace utils {
namespace constraint {

	using std::set;
	using std::map;
	using std::pair;
	using std::vector;

	namespace {

		// the data structure representing the graph this algorithm is based on
		typedef std::unordered_map<Variable, set<const Constraint*>> Edges;

		void collectDependentValues(const Variable& id, const Edges& edges, set<Variable>& res) {
			// get dependent values of current step
			auto pos = edges.find(id);
			if(pos == edges.end()) { return; }

			// collect recursively all dependent value IDs
			for(const auto& cur : pos->second) {
				for(const auto& curID : cur->getOutputs()) {
					if(res.insert(curID).second) { // if the element is new
						collectDependentValues(curID, edges, res);
					}
				}
			}
		}

		set<Variable> getDependentValues(const vector<Variable>& ids, const Edges& edges) {
			set<Variable> res;
			for(auto id : ids) {
				collectDependentValues(id, edges, res);
			}
			return res;
		}
	}


	Assignment solve(const Constraints& constraints, Assignment initial) {
		// the work-list
		vector<Variable> workList;

		// build data structures for the graph
		Assignment res = initial;
		Edges edges;

		// 1. create list of edges
		for(auto& cur : constraints.getList()) {
			// trigger init-routines
			cur->init(res, workList);

			// add edges
			for(auto set : cur->getInputs()) {
				edges[set].insert(&*cur);
			}

			// and dynamic dependencies
			if(cur->hasDynamicDependencies() && cur->updateDynamicDependencies(res)) {
				for(const auto& in : cur->getUsedInputs(res)) {
					edges[in].insert(&*cur);
				}
			}
		}

		// TODO: make work list unique (there are duplicates)

		// 2. solve constraints
		while(!workList.empty()) {
			// retrieve first element
			Variable head = workList.back();
			workList.pop_back();

			// process outgoing edges
			for(const Constraint* curConstraint : edges[head]) {
				const Constraint& cc = *curConstraint;

				// update dynamic dependencies if necessary
				if(cc.hasDynamicDependencies() && cc.updateDynamicDependencies(res)) {
					for(const auto& cur : cc.getUsedInputs(res)) {
						edges[cur].insert(&cc);
					}
				}

				// trigger update
				auto change = cc.update(res);

				// register outputs in work-list
				if(change != Constraint::Unchanged) {
					for(auto cur : cc.getOutputs()) {
						workList.push_back(cur);
					}
				}

				// if the output value has been altered (not just incremented) all dependent values need to be cleared
				if(change == Constraint::Altered) {
					// get updated outputs
					auto outputs = cc.getOutputs();

					// get all depending sets
					set<Variable> dependent = getDependentValues(outputs, edges);

					// check for cycles
					//					if (dependent.contains(head)) { /* TODO: implement recovery */ }

					// clear all dependent sets and re-schedule them
					for(auto cur : dependent) {
						if(!contains(outputs, cur)) {
							res.clear(cur); // do not reset value causing this operation
						}
					}
				}
			}
		}

		// done
		return res;
	};


	const Assignment& LazySolver::solve(const Variable& set) {
		std::set<Variable> sets;
		sets.insert(set);
		return solve(sets);
	}

	const Assignment& LazySolver::solve(const std::set<Variable>& sets) {
		// create the work-list for this resolution
		vector<Variable> worklist;

		// 1. start by resolving requested sets
		resolveConstraints(std::vector<Variable>(sets.begin(), sets.end()), worklist);

		// 2. solve constraints
		while(!worklist.empty()) {
			// retrieve first element
			Variable head = worklist.back();
			worklist.pop_back();

			// process outgoing edges
			for(const Constraint* curConstraint : edges[head]) {
				const Constraint& cc = *curConstraint;

				// update dynamic dependencies if necessary
				if(cc.hasDynamicDependencies() && cc.updateDynamicDependencies(ass)) {
					for(const auto& cur : cc.getUsedInputs(ass)) {
						edges[cur].insert(&cc);
					}
				}

				// add and resolve list of used filters
				resolveConstraints(cc, worklist);

				// trigger update
				auto change = cc.update(ass);

				// register outputs in work-list
				if(change != Constraint::Unchanged) {
					for(auto cur : cc.getOutputs()) {
						worklist.push_back(cur);
					}
				}

				// if the output value has been altered (not just incremented) all dependent values need to be cleared
				if(change == Constraint::Altered) {
					// get updated outputs
					auto outputs = cc.getOutputs();

					// get all depending sets
					set<Variable> dependent = getDependentValues(outputs, edges);

					// check for cycles
					//					if (dependent.contains(head)) { /* TODO: implement recovery */ }

					// clear all dependent sets and re-schedule them
					for(auto cur : dependent) {
						if(!contains(outputs, cur)) {
							ass.clear(cur); // do not reset value causing this operation
						}
					}
				}
			}
		}

		// check all constraints (only for debugging, not valid if resets are involved)
		//		assert_true(all(constraints, [&](const ConstraintPtr& cur)->bool {
		//			auto res = cur->check(ass);
		//			if (!res) std::cout << "Constraint " << *cur << " violated!\n";
		//			return res;
		//		}));

		// return assignment - TODO: find way to project to requested values
		return ass;
	}


	bool LazySolver::hasUnresolvedInput(const Constraint& cur) {
		// check cache
		auto pos = resolvedConstraints.find(&cur);
		if(pos != resolvedConstraints.end()) { return false; }

		// assert that all constraints with dynamic dependencies have unresolved inputs
		if(cur.hasDynamicDependencies()) { return true; }

		// check dependencies
		bool hasUnresolvedInputs = any(cur.getInputs(), [&](const Variable& cur) { return resolved.find(cur) == resolved.end(); });
		if(!hasUnresolvedInputs) { resolvedConstraints.insert(&cur); }
		return hasUnresolvedInputs;
	}

	void LazySolver::resolveConstraints(const Constraint& cur, vector<Variable>& worklist) {
		// check whether there is anything to do
		if(!hasUnresolvedInput(cur)) { return; }

		// resolve constraints
		resolveConstraints(cur.getUsedInputs(ass), worklist);
	}

	void LazySolver::resolveConstraints(const std::vector<Variable>& values, vector<Variable>& worklist) {
		// if there is nothing to do => done
		if(values.empty()) { return; }

		// filter out sets already resolved
		std::set<Variable> missing;
		for(auto cur : values) {
			if(!contains(resolved, cur)) { missing.insert(cur); }
		}

		// check whether there is anything to do
		if(missing.empty()) { return; }

		// add current to resolved set
		for(auto cur : missing) {
			resolved.insert(cur);
		}

		// obtain and integrate new constraints
		std::set<Variable> nextGeneration;
		for(auto cur : resolver(missing)) {
			// copy constraint to internal list of constraints
			constraints.add(cur);

			// trigger init-routines
			cur->init(ass, worklist);

			// add edges to elements depending on
			for(auto set : cur->getInputs()) {
				edges[set].insert(&*cur);
			}

			// collect missing inputs
			if(cur->hasAssignmentDependentDependencies()) {
				// update dynamic dependencies if required
				if(cur->hasDynamicDependencies()) { cur->updateDynamicDependencies(ass); }
			}

			// register dependencies
			for(const auto& dep : cur->getUsedInputs(ass)) {
				edges[dep].insert(&*cur);
			}
			// and add outputs to working list (since dependencies have changed)
			for(auto o : cur->getOutputs()) {
				worklist.push_back(o);
			}
			// collect yet unresolved inputs for resolution
			for(auto set : cur->getUsedInputs(ass)) {
				if(!contains(resolved, set)) { nextGeneration.insert(set); }
			}
		}

		// resolve next generation
		resolveConstraints(std::vector<Variable>(nextGeneration.begin(), nextGeneration.end()), worklist);
	}


	Assignment solve(const Variable& set, const ConstraintResolver& resolver, Assignment initial) {
		// use set-based interface
		std::set<Variable> sets;
		sets.insert(set);
		return solve(sets, resolver, initial);
	}

	// a lazy solver implementation
	Assignment solve(const std::set<Variable>& sets, const ConstraintResolver& resolver, Assignment initial) {
		// create resulting assignment
		return LazySolver(resolver, initial).solve(sets);
	}


} // end namespace constraint
} // end namespace utils
} // end namespace insieme
