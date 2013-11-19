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

		void createDotDump(const Graph& graph) {
//			std::cout << "Creating Dot-Dump for " << analysis.getNumSets() << " sets and " << analysis.getNumConstraints() << " constraints ...\n";
			{
				// open file
				std::ofstream out("graph.dot", std::ios::out );

				// write file
				utils::graph::printGraphViz(out, graph.asBoostGraph());
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

		graph.addEdge(a, b);
		graph.addEdge(b, c);
		graph.addEdge(c, d);
		graph.addEdge(d, e);

		graph.addEdge(a, f);
		graph.addEdge(f, c);

		graph.addEdge(b, g);
		graph.addEdge(g, d);

		// solve the data flow equations
		solve(graph);

		EXPECT_EQ("{x={1}}", toString(graph.getVertex(a).after));
		EXPECT_EQ("{x={5}}", toString(graph.getVertex(e).after));

		createDotDump(graph);
	}

} // end namespace prototype
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
