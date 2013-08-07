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


	namespace {

		struct SolverContext {
			const ConstraintResolver& resolver;
			Assignment res;

			std::set<SetID> resolved;
			typedef map<SetID, set<const Constraint*>> Edges;
		};

		void solveInternal(SolverContext& context, const SetID& set) {

			// create a workList

		}

	}


	Assignment solve(const SetID& set, const ConstraintResolver& resolver, Assignment initial) {
		// solve a single set
		SolverContext context = { resolver, initial };
		solveInternal(context, set);
		return context.res;
	}

	// a lazy solver implementation
	Assignment solve(const std::set<SetID>& sets, const ConstraintResolver& resolver, Assignment initial) {

		//create resulting assignment
		SolverContext context = { resolver, initial };

		// just resolve set by set
		for (auto cur : sets) {
			solveInternal(context, cur);
		}

		// done
		return context.res;
	}


} // end namespace set_constraint
} // end namespace utils
} // end namespace insieme
