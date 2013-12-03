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

#include <gtest/gtest.h>

#include <sstream>

#include "insieme/utils/graph_utils.h"

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
	EXPECT_TRUE(graph.addEdge(10,12));
	EXPECT_TRUE(graph.containsVertex(10));
	EXPECT_TRUE(graph.containsVertex(12));
	EXPECT_TRUE(graph.containsVertex(14));

	// add same edge again
	EXPECT_FALSE(graph.addEdge(10,12));

	// but the reverse should be allowed - once
	EXPECT_TRUE(graph.addEdge(12,10));
	EXPECT_FALSE(graph.addEdge(12,10));

	EXPECT_FALSE(graph.containsVertex(1));
	EXPECT_FALSE(graph.containsVertex(2));
	graph.addEdge(1,2);
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
	auto res = graph.getLabel(2,4);
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ('+', *res);

	res = graph.getLabel(1,8);
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ('-', *res);

	EXPECT_FALSE(graph.getLabel(2,8));
	EXPECT_FALSE(graph.getLabel(4,2));

}

TEST(GraphUtils, PointerGraph) {

	int a = 1;
	int b = 2;
	int c = 2;  // c == b

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



} // end namespace graph
} // end namespace utils
} // end namespace insieme
