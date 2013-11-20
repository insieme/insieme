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

#include "insieme/analysis/cba/prototype/parallel_flow.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace prototype {

	namespace {

		namespace ug = utils::graph;

		void createDotDump(const Graph& graph) {
//			std::cout << "Creating Dot-Dump for " << analysis.getNumSets() << " sets and " << analysis.getNumConstraints() << " constraints ...\n";
			{
				// open file
				std::ofstream out("graph.dot", std::ios::out );

				// write file
				ug::printGraphViz(
						out,
						graph.asBoostGraph(),
						ug::default_label(),
						ug::no_label(),
						ug::default_deco(),
						[](std::ostream& out, const Edge& e) {
							switch(e) {
							case Seq: out << ", style=\"solid\"";  break;
							case Par: out << ", style=\"dashed\""; break;
							case Com: out << ", style=\"dotted\""; break;
							}
						}
				);
			}

			// create svg
			system("dot -Tsvg graph.dot -o graph.svg");
		}

	}

	TEST(CBA_Parallel, GraphBuilder) {

		Graph graph;

		// built the graph
		Node a = Node::write("x", 1);
		Node b = Node::write("x", 2);
		Node c = Node::read("x");
		Node d = Node::read("x");
		Node e = Node::write("x", 5);

		Node f = Node::write("x", 6);
		Node g = Node::write("x", 7);

		graph.addEdge(a, b, Par);
		graph.addEdge(b, c, Par);
		graph.addEdge(c, d, Par);
		graph.addEdge(d, e, Par);

		graph.addEdge(a, f, Par);
		graph.addEdge(f, c, Par);

		graph.addEdge(b, g, Par);
		graph.addEdge(g, d, Par);

		// solve the data flow equations
		solve(graph);

		EXPECT_EQ("{x={1}}", toString(graph.getVertex(a).after));
		EXPECT_EQ("{x={5}}", toString(graph.getVertex(e).after));

		createDotDump(graph);
	}


	TEST(CBA_Parallel, DiamondNoAssign) {

		Graph graph;

		Node a = Node::write("x", 0);
		Node b = Node::noop();
		Node c = Node::noop();
		Node d = Node::read("x");

		graph.addEdge(a,b, Par);
		graph.addEdge(a,c, Par);
		graph.addEdge(b,d, Par);
		graph.addEdge(c,d, Par);

		solve(graph);

		EXPECT_EQ("{x={0}}", toString(graph.getVertex(d).before));

		createDotDump(graph);

	}

	TEST(CBA_Parallel, DiamondOneAssign) {

		Graph graph;

		Node a = Node::write("x", 0);
		Node b = Node::write("x", 1);
		Node c = Node::noop();
		Node d = Node::read("x");

		graph.addEdge(a,b, Par);
		graph.addEdge(a,c, Par);
		graph.addEdge(b,d, Par);
		graph.addEdge(c,d, Par);

		solve(graph);

		EXPECT_EQ("{x={1}}", toString(graph.getVertex(d).before));

		createDotDump(graph);

	}

	TEST(CBA_Parallel, DiamondTwoAssign) {

		Graph graph;

		Node a = Node::write("x", 0);
		Node b = Node::write("x", 1);
		Node c = Node::write("x", 2);
		Node d = Node::read("x");

		graph.addEdge(a,b, Par);
		graph.addEdge(a,c, Par);
		graph.addEdge(b,d, Par);
		graph.addEdge(c,d, Par);

		solve(graph);

		EXPECT_EQ("{x={1,2}}", toString(graph.getVertex(d).before));

		createDotDump(graph);

	}


	TEST(CBA_Parallel, ThreeDiamonds) {

		Graph graph;

		{
			Node a = Node::write("x", 0);
			Node b = Node::noop();
			Node c = Node::noop();
			Node d = Node::read("x");

			graph.addEdge(a,b, Par);
			graph.addEdge(a,c, Par);
			graph.addEdge(b,d, Par);
			graph.addEdge(c,d, Par);

			solve(graph);

			EXPECT_EQ("{x={0}}", toString(graph.getVertex(d).before));
		}

		{
			Node a = Node::write("x", 0);
			Node b = Node::write("x", 1);
			Node c = Node::noop();
			Node d = Node::read("x");

			graph.addEdge(a,b, Par);
			graph.addEdge(a,c, Par);
			graph.addEdge(b,d, Par);
			graph.addEdge(c,d, Par);

			solve(graph);

			EXPECT_EQ("{x={1}}", toString(graph.getVertex(d).before));
		}

		{
			Node a = Node::write("x", 0);
			Node b = Node::write("x", 1);
			Node c = Node::write("x", 2);
			Node d = Node::read("x");

			graph.addEdge(a,b, Par);
			graph.addEdge(a,c, Par);
			graph.addEdge(b,d, Par);
			graph.addEdge(c,d, Par);

			solve(graph);

			EXPECT_EQ("{x={1,2}}", toString(graph.getVertex(d).before));
		}

		{
			// built the graph
			Node a = Node::write("x", 1);
			Node b = Node::write("x", 2);
			Node c = Node::read("x");
			Node d = Node::read("x");
			Node e = Node::write("x", 5);

			Node f = Node::write("x", 6);
			Node g = Node::write("x", 7);

			graph.addEdge(a, b, Seq);
			graph.addEdge(b, c, Seq);
			graph.addEdge(c, d, Seq);
			graph.addEdge(d, e, Seq);

			graph.addEdge(a, f, Par);
			graph.addEdge(f, c, Par);

			graph.addEdge(b, g, Par);
			graph.addEdge(g, d, Par);

			// solve the data flow equations
			solve(graph);

			EXPECT_EQ("{x={1}}", toString(graph.getVertex(a).after));
//			EXPECT_EQ("{x={6,7}}", toString(graph.getVertex(d).before));
			EXPECT_EQ("{x={5}}", toString(graph.getVertex(e).after));

		}

		createDotDump(graph);

	}

	TEST(CBA_Parallel, TwoInterleavedSideThreads) {

		Graph graph;

		// built the graph
		Node a = Node::write("x", 1);
		Node b = Node::write("x", 2);
		Node c = Node::read("x");
		Node d = Node::read("x");
		Node e = Node::write("x", 5);

		Node f = Node::write("x", 6);
		Node g = Node::write("x", 7);

		graph.addEdge(a, b, Seq);
		graph.addEdge(b, c, Seq);
		graph.addEdge(c, d, Seq);
		graph.addEdge(d, e, Seq);

		graph.addEdge(a, f, Par);
		graph.addEdge(f, c, Par);

		graph.addEdge(b, g, Par);
		graph.addEdge(g, d, Par);

		// solve the data flow equations
		solve(graph);

		EXPECT_EQ("{x={1}}", toString(graph.getVertex(a).after));
//		EXPECT_EQ("{x={6,7}}", toString(graph.getVertex(d).before));
		EXPECT_EQ("{x={5}}", toString(graph.getVertex(e).after));

		createDotDump(graph);

	}

} // end namespace prototype
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
