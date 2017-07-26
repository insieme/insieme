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

#include <gtest/gtest.h>

#include <sstream>

#include "insieme/utils/graph_utils.h"
#include "insieme/utils/set_utils.h"

using std::pair;
using std::string;
using std::vector;
using std::list;

namespace insieme {
namespace utils {
namespace graph {


	TEST(GraphUtils, SimpleGraph) {
		// create a graph
		Graph<int> graph;
		EXPECT_EQ("({},{})", toString(graph));

		EXPECT_FALSE(graph.containsVertex(10));
		EXPECT_FALSE(graph.containsVertex(12));
		EXPECT_FALSE(graph.containsVertex(14));

		// add a node
		EXPECT_TRUE(graph.addVertex(12));
		EXPECT_FALSE(graph.containsVertex(10));
		EXPECT_TRUE(graph.containsVertex(12));
		EXPECT_FALSE(graph.containsVertex(14));

		// add a duplicate
		EXPECT_FALSE(graph.addVertex(12));
		EXPECT_FALSE(graph.containsVertex(10));
		EXPECT_TRUE(graph.containsVertex(12));
		EXPECT_FALSE(graph.containsVertex(14));

		// add another node
		EXPECT_TRUE(graph.addVertex(14));
		EXPECT_FALSE(graph.containsVertex(10));
		EXPECT_TRUE(graph.containsVertex(12));
		EXPECT_TRUE(graph.containsVertex(14));

		// add an edge
		EXPECT_TRUE(graph.addEdge(10, 12));
		EXPECT_TRUE(graph.containsVertex(10));
		EXPECT_TRUE(graph.containsVertex(12));
		EXPECT_TRUE(graph.containsVertex(14));

		// add same edge again
		EXPECT_FALSE(graph.addEdge(10, 12));

		// but the reverse should be allowed - once
		EXPECT_TRUE(graph.addEdge(12, 10));
		EXPECT_FALSE(graph.addEdge(12, 10));

		EXPECT_FALSE(graph.containsVertex(1));
		EXPECT_FALSE(graph.containsVertex(2));
		graph.addEdge(1, 2);
		EXPECT_TRUE(graph.containsVertex(1));
		EXPECT_TRUE(graph.containsVertex(2));
	}

	TEST(GraphUtils, LabeledGraph) {
		Graph<int, char> graph;

		// check printing of a small graph (dot plot)
		EXPECT_TRUE(graph.addEdge(1, 2, '+'));

		std::stringstream buffer;
		graph.printGraphViz(buffer);
		EXPECT_EQ("digraph G {\n0[label=\"1\" ];\n1[label=\"2\" ];\n0->1 [label=\"+\" ];\n}\n", buffer.str());

		EXPECT_TRUE(graph.addEdge(2, 4, '+'));
		EXPECT_TRUE(graph.addEdge(4, 8, '+'));

		EXPECT_EQ(static_cast<std::size_t>(4), graph.getNumVertices());
		EXPECT_EQ(static_cast<std::size_t>(3), graph.getNumEdges());

		EXPECT_TRUE(graph.addEdge(1, 8, '-'));

		EXPECT_EQ(static_cast<std::size_t>(4), graph.getNumVertices());
		EXPECT_EQ(static_cast<std::size_t>(4), graph.getNumEdges());

		// check labels
		auto res = graph.getLabel(2, 4);
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ('+', *res); }

		res = graph.getLabel(1, 8);
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ('-', *res); }

		EXPECT_FALSE(graph.getLabel(2, 8));
		EXPECT_FALSE(graph.getLabel(4, 2));
	}

	TEST(GraphUtils, PointerGraph) {
		int a = 1;
		int b = 2;
		int c = 2; // c == b

		int* p1 = &a;
		int* p2 = &b;
		int* p3 = &c;

		PointerGraph<int*> graph;

		// first two should be fine
		EXPECT_TRUE(graph.addVertex(p1));
		EXPECT_TRUE(graph.addVertex(p2));

		// this should not work since *p3 = *p2 => same value
		EXPECT_FALSE(graph.addVertex(p3));
	}


	TEST(GraphUtils, CycleDetection) {
		// build a dummy graph including a cycle
		Graph<int> graph;

		auto cycle = detectCycle(graph.asBoostGraph());
		EXPECT_TRUE(cycle.empty()) << cycle;

		// start with no cycle
		graph.addEdge(1, 2);
		graph.addEdge(2, 3);
		graph.addEdge(3, 4);

		cycle = detectCycle(graph.asBoostGraph());
		EXPECT_TRUE(cycle.empty()) << cycle;

		// close the cycle
		graph.addEdge(4, 1);

		cycle = detectCycle(graph.asBoostGraph());
		EXPECT_FALSE(cycle.empty()) << cycle;
		EXPECT_EQ(4, cycle.size());


		// create something with extra nodes
		graph = Graph<int>();

		graph.addEdge(1, 2);
		graph.addEdge(1, 3);
		graph.addEdge(1, 4);
		graph.addEdge(2, 3);
		graph.addEdge(3, 4);
		graph.addEdge(4, 5);
		graph.addEdge(5, 3);

		cycle = detectCycle(graph.asBoostGraph());
		EXPECT_FALSE(cycle.empty()) << cycle;
		EXPECT_EQ(3, cycle.size());
	}

	TEST(GraphUtils, SCCComputation) {
		// build a dummy graph
		Graph<int> graph;

		auto res = computeSCCGraph(graph.asBoostGraph());
		EXPECT_EQ(0, res.getNumVertices()) << res;
		EXPECT_EQ(0, res.getNumEdges()) << res;

		// add a node pair
		graph.addEdge(1, 2);
		res = computeSCCGraph(graph.asBoostGraph());
		EXPECT_EQ(2, res.getNumVertices()) << res;
		EXPECT_EQ(1, res.getNumEdges()) << res;
		EXPECT_EQ("({{1},{2}},{({1},{2})})", toString(res));

		// close the cycle
		graph.addEdge(2, 1);
		res = computeSCCGraph(graph.asBoostGraph());
		EXPECT_EQ(1, res.getNumVertices()) << res;
		EXPECT_EQ(0, res.getNumEdges()) << res;
		EXPECT_EQ("({{1,2}},{})", toString(res));

		// add two more edges
		graph.addEdge(1, 3);
		graph.addEdge(1, 4);
		res = computeSCCGraph(graph.asBoostGraph());
		EXPECT_EQ(3, res.getNumVertices()) << res;
		EXPECT_EQ(2, res.getNumEdges()) << res;
		EXPECT_EQ("({{1,2},{3},{4}},{({1,2},{3}),({1,2},{4})})", toString(res));
	}

	TEST(GraphUtils, TopologicalOrder) {
		// build a dummy graph
		Graph<int> graph;

		EXPECT_EQ("[]", toString(getTopologicalOrder(graph)));

		// add a nodes
		graph.addEdge(1, 2);
		EXPECT_EQ("[1,2]", toString(getTopologicalOrder(graph)));

		graph.addEdge(2, 3);
		EXPECT_EQ("[1,2,3]", toString(getTopologicalOrder(graph)));
	}

} // end namespace graph
} // end namespace utils
} // end namespace insieme
