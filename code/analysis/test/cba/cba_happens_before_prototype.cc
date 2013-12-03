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

#include "insieme/analysis/cba/prototype/happens_before_tree.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace prototype {

//	namespace {
//
//		namespace ug = utils::graph;
//
//		void createDotDump(const Graph& graph) {
////			std::cout << "Creating Dot-Dump for " << analysis.getNumSets() << " sets and " << analysis.getNumConstraints() << " constraints ...\n";
//			{
//				// open file
//				std::ofstream out("graph.dot", std::ios::out );
//
//				// write file
//				ug::printGraphViz(
//						out,
//						graph.asBoostGraph(),
//						ug::default_label(),
//						ug::no_label(),
//						ug::default_deco(),
//						[](std::ostream& out, const Edge& e) {
//							switch(e) {
//							case Seq: out << ", style=\"solid\"";  break;
//							case Par: out << ", style=\"dashed\""; break;
//							case Com: out << ", style=\"dotted\""; break;
//							}
//						}
//				);
//			}
//
//			// create svg
//			system("dot -Tsvg graph.dot -o graph.svg");
//		}
//
//	}

	TEST(CBA_HappensBefore, Construction) {

		auto a = writeSet(1);
		auto b = writeSet(2);
		auto c = writeSet(3);
		auto d = writeSet(4,5);

		Relation e;
		Relation r1 = par(seq(a), seq(b));
		Relation r2 = seq(r1, c);
		Relation r3 = seq(r2, d);

		EXPECT_EQ("{}", toString(e));
		EXPECT_EQ("{{1},{2}}", toString(r1));
		EXPECT_EQ("{{1},{2}}->{3}", toString(r2));
		EXPECT_EQ("{{1},{2}}->{3}->{4,5}", toString(r3));

		EXPECT_EQ("{1,2}", 	toString(r1->getMostRecentWrite()));
		EXPECT_EQ("{3}", 	toString(r2->getMostRecentWrite()));
		EXPECT_EQ("{4,5}", 	toString(r3->getMostRecentWrite()));

	}

	TEST(CBA_HappensBefore, OverlappingWriteTestCase) {

		auto a = writeSet(1);
		auto b = writeSet(2);
		auto c = writeSet(3);
		auto d = writeSet(4);

		Relation e;
		Relation r1 = seq(a);
		Relation r2 = seq(r1, b);
		Relation r3 = seq(r1, c);
		Relation r4 = par(r2,r3);
		Relation r5 = seq(r3, d);
		Relation r6 = par(r4, r5);

		EXPECT_EQ("{{{1}->{2},{1}->{3}},{1}->{3}->{4}}", toString(r6));
		EXPECT_EQ("{2,4}", toString(r6->getMostRecentWrite()));

	}


} // end namespace prototype
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
