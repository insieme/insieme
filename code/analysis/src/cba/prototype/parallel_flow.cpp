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

#include "insieme/analysis/cba/prototype/parallel_flow.h"


namespace insieme {
namespace analysis {
namespace cba {
namespace prototype {


	namespace {

		Assignment merge(const Assignment& a, const Assignment& b) {
			Assignment res = a;
			for(auto cur : b) {
				res[cur.first].insert(cur.second.begin(), cur.second.end());
			}
			return res;
		}

		void updateInNaive(const Node& node, const Graph& graph) {

			auto& g = graph.asBoostGraph();

			// just merge all the input sets

			typedef boost::graph_traits<Graph::GraphType> GraphTraits;

//			std::cout << "Node: " << node << "\n";
//			std::cout << "Predecessors: ";
			typename GraphTraits::in_edge_iterator in_i, in_end;
			for (std::tie(in_i, in_end) = boost::in_edges(graph.getVertexDescriptor(node), g); in_i != in_end; ++in_i) {
				auto e = *in_i;
				auto src = boost::source(e, g);
//				std::cout << g[src] << "\n";
				node.before = merge(node.before, g[src].after);
			}
//			std::cout << "\n";
		}



		bool update(const Node& node, const Graph& g) {

			// create a backup of the old state
			Node old = node;

//			std::cout << "Updating: " << node << "\n";

			// update in-set  ... that's the tricky part
			updateInNaive(node, g);

			// update out-set
			node.after = node.before;
			if (node.getType() == Node::Write) {
				node.after[node.getVar()] = utils::set::toSet<std::set<Value>>(node.getValue());
			}

//			std::cout << old.before << " != " << node.before << " : " << (old.before != node.before) << "\n";
//			std::cout << old.after << " != " << node.after << " : " << (old.after != node.after) << "\n";
//			std::cout << "After:    " << node << "\n\n";


			// check whether there was a change
			return old.before != node.before || old.after != node.after;
		}

	}

	void solve(const Graph& graph) {

		bool change = true;
		while(change) {
			change = false;
			for(auto cur = graph.vertexBegin(); cur != graph.vertexEnd(); ++cur) {
				change = update(*cur, graph) || change;
			}
		}

	}

} // end namespace prototype
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
