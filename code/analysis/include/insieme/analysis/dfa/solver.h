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

#include <set>
#include <queue>
#include <map>

#include "insieme/core/ir_expressions.h"
#include "insieme/analysis/cfg.h"

namespace insieme {
namespace analysis {
namespace dfa {

/**
 * Implementation of the Worklist Queue utilized for the worklist algorithm. 
 *
 * The queue should provide enqueue() and dequeue() operations and it must not
 * happen that the same cfg::BlockPtr appears twice.
 */
class WorklistQueue {

	// maintains the queue of blocks
	std::queue<cfg::BlockPtr> block_queue;

	// for fast operation in the queue, we keep the set of blocks 
	// currently in the queue. 
	std::set<cfg::BlockPtr> block_set;

public :
	WorklistQueue() { }

	/**
	 * Insert an element x at the back of the queue. 
	 *
	 * Postconditions: size() will be incremented by 1
	 */
	void enqueue(const cfg::BlockPtr& block);

	/**
	 * Remove the element which is in the front of the queue and returns a copy
	 * of it, i.e. the element least recently inserted 
	 *
	 * Precondition: empty() is false
	 * Postcondition: size() will be decremented by 1
	 */
	cfg::BlockPtr dequeue();

	size_t size() const { return block_set.size(); }

	bool empty() const { return block_set.empty(); }

};


/** 
 * The Solver implements a generic solver for DataFlow Problems. 
 *
 * The solver is based on the worklist algorithm, detailed explanation can be
 * found: http://www.cs.rice.edu/~harv/my_papers/worklist.pdf
 */
template <class Problem>
class Solver {
	
	const CFG& cfg;

public:
	Solver(const CFG& cfg) : cfg(cfg) { }

	void solve() {

		Problem df_p(cfg);
		df_p.initialize();

		WorklistQueue q;

		std::map<cfg::BlockPtr, typename Problem::value_type> solver_data;

		auto fill_queue = 
			[&] (const cfg::BlockPtr& block) { 
				typename Problem::value_type x = df_p.top();
		
				std::for_each(block->predecessors_begin(), block->predecessors_end(),
					[&]( const cfg::BlockPtr& pred) {
						typename Problem::value_type v = df_p.transfer_func(df_p.top(), *pred);
						x = df_p.meet(x, v);
					});

				solver_data[block] = x;
				
				if ( df_p.getLattice().is_weaker_than(x, df_p.top()) && x != df_p.top() ) {
					q.enqueue(block); 
				}
			};
	
		cfg.visitDFS( fill_queue );


		// iterate through the elements of the queue until the queue is empty
		while (!q.empty()) {
			
			cfg::BlockPtr block = q.dequeue();
			
			std::for_each(block->successors_begin(), block->successors_end(),
				[&]( const cfg::BlockPtr& succ) {
					
					typename Problem::value_type tmp = 
						df_p.meet(solver_data[succ], df_p.transfer_func(solver_data[block], *block));
					
					if (df_p.getLattice().is_weaker_than(tmp, solver_data[succ]) && tmp != solver_data[succ]) {
						solver_data[succ] = tmp;
						q.enqueue(succ);
					}
					
				});
		}

		std::cout << solver_data << std::endl;
	}

};


} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace  
