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
#include "insieme/analysis/dfa/problem.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/string_utils.h"

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

namespace detail {

template<typename DirTag> 
struct CFGIterTraits {};

template <> 
struct CFGIterTraits<ForwardAnalysisTag> {
	typedef typename cfg::Block::predecessors_iterator	PrevBItr;
	typedef typename cfg::Block::successors_iterator 	NextBItr;
	typedef typename cfg::Block::const_iterator      	StmtItr;

	static PrevBItr PrevBegin(const cfg::BlockPtr B) {
		return B->predecessors_begin(); 
	}
	static PrevBItr PrevEnd(const cfg::BlockPtr B) { 
		return B->predecessors_end(); 
	}

	static NextBItr NextBegin(const cfg::BlockPtr B) { 
		return B->successors_begin(); 
	}
	static NextBItr NextEnd(const cfg::BlockPtr B) { 
		return B->successors_end(); 
	}

	static StmtItr StmtBegin(const cfg::BlockPtr B) { 
		return B->stmt_begin(); 
	}
	static StmtItr StmtEnd(const cfg::BlockPtr B) { 
		return B->stmt_end(); 
	}

	// static BlockEdge PrevEdge(const CFGBlock *B, const CFGBlock *Prev) {
	//  return BlockEdge(Prev, B, 0);
	// }

	// static BlockEdge NextEdge(const CFGBlock *B, const CFGBlock *Next) {
	//  return BlockEdge(B, Next, 0);
	// }
};

template <> 
struct CFGIterTraits<BackwardAnalysisTag> {

	typedef typename cfg::Block::successors_iterator	PrevBItr;
	typedef typename cfg::Block::predecessors_iterator 	NextBItr;
	typedef typename cfg::Block::const_iterator      	StmtItr;

	static PrevBItr PrevBegin(const cfg::BlockPtr B) {
		return B->successors_begin(); 
	}
	static PrevBItr PrevEnd(const cfg::BlockPtr B) { 
		return B->successors_end(); 
	}

	static NextBItr NextBegin(const cfg::BlockPtr B) { 
		return B->predecessors_begin(); 
	}
	static NextBItr NextEnd(const cfg::BlockPtr B) { 
		return B->predecessors_end(); 
	}

	// rbegin / rend todo
	static StmtItr StmtBegin(const cfg::BlockPtr B) { 
		return B->stmt_begin(); 
	}
	static StmtItr StmtEnd(const cfg::BlockPtr B) { 
		return B->stmt_end(); 
	}
	//static BlockEdge PrevEdge(const CFGBlock *B, const CFGBlock *Prev) {
	//return BlockEdge(B, Prev, 0);
	//}

	//static BlockEdge NextEdge(const CFGBlock *B, const CFGBlock *Next) {
	//return BlockEdge(Next, B, 0);
	//}
};



} // end anonymous namespace


/** 
 * The Solver implements a generic solver for DataFlow Problems. 
 *
 * The solver is based on the worklist algorithm, detailed explanation can be
 * found: http://www.cs.rice.edu/~harv/my_papers/worklist.pdf
 */
template <class Problem>
class Solver {
	
	const CFG& cfg;

	Problem df_p;

protected:

	inline typename Problem::value_type 
	initial_value(const cfg::BlockPtr block, const Problem& p, ForwardAnalysisTag) const {
		return cfg.isEntry(block) ? p.init() : p.top();
	}

	inline typename Problem::value_type 
	initial_value(const cfg::BlockPtr block, const Problem& p, BackwardAnalysisTag) const {
		return cfg.isExit(block) ? p.init() : p.top();
	}

public:

	/**
	 * Map which stores the dataflow value for each block of the CFG
	 */
	typedef std::map<size_t, typename Problem::value_type> CFGBlockMap;

	Solver(const CFG& cfg) : cfg(cfg), df_p(cfg) { }

	inline const Problem& getProblemInstance() const { return df_p; }
	inline Problem& getProblemInstance() { return df_p; }

	inline CFGBlockMap solve() {

		using namespace detail;

		typedef typename Problem::value_type 	value_type;
		typedef typename Problem::direction_tag direction_tag;

		df_p.initialize();

		WorklistQueue q;

		CFGBlockMap solver_data;

		auto&& fill_queue = 
			[&] (const cfg::BlockPtr& block) -> void { 

				value_type x(this->initial_value(block, df_p, direction_tag()));

				std::for_each(
					CFGIterTraits<direction_tag>::PrevBegin(block), 
					CFGIterTraits<direction_tag>::PrevEnd(block),
					[&]( const cfg::BlockPtr& pred) {
						value_type&& v = df_p.transfer_func(df_p.top(), pred);
						x = df_p.meet(x, v);
					});
				
				//LOG(DEBUG) << x;

				solver_data[ block->getBlockID() ] = x; // df_p.transfer_func(x, block);
				
				if ( df_p.getLattice().is_strictly_weaker_than(x, df_p.top()) ) {
					q.enqueue(block); 
				}
			};
	
		cfg.visitDFS( fill_queue );

		// Initial state of analysis 
		LOG(DEBUG) << "@@@@@@@@@@@@@@@@@@@";
		LOG(DEBUG) << "@@ Initial State @@";
		LOG(DEBUG) << "@@@@@@@@@@@@@@@@@@@";
		printDataflowData(LOG_STREAM(DEBUG), solver_data); 

		// iterate through the elements of the queue until the queue is empty
		while (!q.empty()) {
			
			cfg::BlockPtr block = q.dequeue();
			
			std::for_each(
				CFGIterTraits<direction_tag>::NextBegin(block), 
				CFGIterTraits<direction_tag>::NextEnd(block),
				[&]( const cfg::BlockPtr& succ) {
					
					value_type&& tmp = df_p.meet(
						solver_data[ succ->getBlockID() ], 
						df_p.transfer_func(solver_data[ block->getBlockID() ], block)
					);

					if (df_p.getLattice().is_strictly_weaker_than(tmp, solver_data[ succ->getBlockID() ])) {
						solver_data[ succ->getBlockID() ] = tmp;
						q.enqueue(succ);
					}
					
				});

			// LOG(DEBUG) << "AFTER ITER";
			// LOG(DEBUG) << solver_data;
		}

		LOG(DEBUG) << "@@@@@@@@@@@@@@@@@";
		LOG(DEBUG) << "@@ Final State @@";
		LOG(DEBUG) << "@@@@@@@@@@@@@@@@@";
		printDataflowData(LOG_STREAM(DEBUG), solver_data); 

		return std::move(solver_data);
	}

	/** 
	 * Print the dataflow values (for debugging purposes)
	 */
	static void printDataflowData(std::ostream& out, const CFGBlockMap& data) {

		out << join("\n", data, 
			[&](std::ostream& jout, const typename CFGBlockMap::value_type& cur) { 
				jout << cur.first << " -> " << cur.second;
			});
	}

};


} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace  
