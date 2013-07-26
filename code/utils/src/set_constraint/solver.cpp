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

#include "insieme/utils/set_constraint/solver.h"

#include <utility>
#include <vector>

namespace insieme {
namespace utils {
namespace set_constraint {

	using std::set;
	using std::map;
	using std::pair;
	using std::vector;


	Assignment solve(const Constraints& constraints, const Assignment& initial) {

		// the data structure representing the graph this algorithm is based on
		typedef map<Set, set<const Constraint*>> Edges;

		// the work-list
		vector<Set> workList;

		// build data structures for the graph
		Assignment res = initial;
		Edges edges;

		// 1. create list of edges
		for(const Constraint& cur : constraints) {
			switch(cur.getKind()) {
			case Constraint::Elem: {
				// if e \in A  ... add value to result
				res[cur.getA()].insert(cur.getValue());
				workList.push_back(cur.getA());
				break;
			}
			case Constraint::Subset: {
				// if B \subset C ... add edge
				edges[cur.getB()].insert( &cur );
				break;
			}
			case Constraint::SubsetIf: {
				// if e \in A => B \subset C ... add two edges
				edges[cur.getA()].insert( &cur );
				edges[cur.getB()].insert( &cur );
				break;
			}
			}
		}

		// a utility function merging sets
		auto addAll = [&](const Set& srcSet, const Set& trgSet) {
			// get actual set
			auto& trg = res[trgSet];

			// add values to target set
			bool newData = false;
			for(const auto& x : res[srcSet]) {
				newData = trg.insert(x).second || newData;
			}

			// if target set has changed => add it to worklist
			if (newData) workList.push_back(trgSet);
		};

		// TODO: make work list unique (there are duplicates)

		// 2. solve constraints
		while(!workList.empty()) {
			// retrieve first element
			Set head = workList.back();
			workList.pop_back();

			// process outgoing edges
			for (const Constraint* cur : edges[head]) {
				const Constraint& cc = *cur;

				// depending on operation
				if (cc.getKind() == Constraint::Subset) {

					// add all elements of set B to set C
					addAll(cc.getB(), cc.getC());

				} else {
					assert(cc.getKind() == Constraint::SubsetIf);

					// if e is in A add all B to C
					if (contains(res[cc.getA()], cc.getValue())) {
						addAll(cc.getB(), cc.getC());
					}
				}
			}
		}

		// done
		return res;
	}


} // end namespace set_constraint
} // end namespace utils
} // end namespace insieme
