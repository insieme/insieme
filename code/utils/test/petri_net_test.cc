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
 *
 */
#include <gtest/gtest.h>

#include <string>

#include "insieme/utils/petri_net/petri_net.h"
#include "insieme/utils/petri_net/petri_net_io.h"

#include "insieme/utils/set_utils.h"

namespace insieme {
namespace utils {
namespace petri_net {

	using std::string;

	TEST(PetriNet, BuildAndDump) {
		typedef PetriNet<string, string> PetriNet;

		PetriNet net;

		net.addPlace("Start");
		net.addPlace("Inner");
		net.addPlace("End");

		net.addPrePlace("Start", "Step1");
		net.addPostPlace("Step1", "Inner");
		net.addPrePlace("Inner", "Step2");
		net.addPostPlace("Step2", "End");

		// plot(net);
	}

	TEST(PetriNet, Marking) {
		typedef PetriNet<string, string> PetriNet;
		typedef PetriNet::marking_type Marking;

		PetriNet net;

		net.addPlace("Start");
		net.addPlace("Inner");
		net.addPlace("End");

		net.addPrePlace("Start", "Step1");
		net.addPostPlace("Step1", "Inner");
		net.addPrePlace("Inner", "Step2");
		net.addPostPlace("Step2", "End");


		Marking marking(net);
		EXPECT_EQ("[0,0,0]", toString(marking));
		EXPECT_EQ("{}", toString(marking.getSuccessors()));

		marking.setMarking("Start");
		EXPECT_EQ("[1,0,0]", toString(marking));
		EXPECT_EQ("{[0,1,0]}", toString(marking.getSuccessors()));

		marking.setMarking("Start", 0);
		marking.setMarking("Inner", 1);
		EXPECT_EQ("[0,1,0]", toString(marking));
		EXPECT_EQ("{[0,0,1]}", toString(marking.getSuccessors()));

		marking.setMarking("Inner", 0);
		marking.setMarking("End", 1);
		EXPECT_EQ("[0,0,1]", toString(marking));
		EXPECT_EQ("{}", toString(marking.getSuccessors()));
	}

	TEST(PetriNet, StateGraphSimple) {
		typedef PetriNet<string, string> PetriNet;
		typedef PetriNet::marking_type Marking;

		PetriNet net;

		net.addPlace("Start");
		net.addPlace("Inner");
		net.addPlace("End");

		net.addPrePlace("Start", "Step1");
		net.addPostPlace("Step1", "Inner");
		net.addPrePlace("Inner", "Step2");
		net.addPostPlace("Step2", "End");

		// create full state graph
		auto stateGraph = extractStateGraph(Marking(net, {1, 0, 0}));
		EXPECT_EQ(3, stateGraph.getNumStates());
		// plot(stateGraph, "stateGraph.svg");
	}

	TEST(PetriNet, StateGraphComplex) {
		typedef PetriNet<string, string> PetriNet;
		typedef PetriNet::marking_type Marking;

		PetriNet net;

		net.addPlace("Start");
		net.addPlace("Inner");
		net.addPlace("Side");
		net.addPlace("End");

		net.addPrePlace("Start", "Step1");
		net.addPostPlace("Step1", "Inner");
		net.addPrePlace("Inner", "Step2");
		net.addPostPlace("Step2", "End");

		net.addPrePlace("Inner", "Step3");
		net.addPostPlace("Step3", "Inner");

		net.addPrePlace("Inner", "Step4");
		net.addPostPlace("Step4", "Side");
		net.addPrePlace("Side", "Step5");
		net.addPostPlace("Step5", "Inner");

		// create full state graph
		// plot(net);
		auto stateGraph = extractStateGraph(Marking(net, {1, 0, 0, 0}));

		EXPECT_EQ(4, stateGraph.getNumStates());

		EXPECT_TRUE(stateGraph.containsMarking(Marking(net, {1, 0, 0, 0})));
		EXPECT_TRUE(stateGraph.containsMarking(Marking(net, {0, 1, 0, 0})));
		EXPECT_TRUE(stateGraph.containsMarking(Marking(net, {0, 0, 1, 0})));
		EXPECT_TRUE(stateGraph.containsMarking(Marking(net, {0, 0, 0, 1})));

		// plot(net, "net.svg");
		// plot(stateGraph, "stateGraph.svg");
	}

	TEST(PetriNet, StateGraphComplex2) {
		typedef PetriNet<string, int> PetriNet;
		typedef PetriNet::marking_type Marking;

		PetriNet net;

		net.addPlace("A");
		net.addPlace("B");
		net.addPlace("C");
		net.addPlace("D");
		net.addPlace("E");

		net.addPrePlace("A", 1);
		net.addPostPlace(1, "B");

		net.addPrePlace("A", 2);
		net.addPostPlace(2, "C");
		net.addPostPlace(2, "E");

		net.addPrePlace("B", 3);
		net.addPostPlace(3, "D");

		net.addPrePlace("C", 4);
		net.addPostPlace(4, "D");

		// create full state graph
		// plot(net);
		auto stateGraph = extractStateGraph(Marking(net, {1, 0, 0, 0, 0}));

		EXPECT_EQ(5, stateGraph.getNumStates());

		EXPECT_TRUE(stateGraph.containsMarking(Marking(net, {1, 0, 0, 0, 0})));
		EXPECT_TRUE(stateGraph.containsMarking(Marking(net, {0, 1, 0, 0, 0})));
		EXPECT_TRUE(stateGraph.containsMarking(Marking(net, {0, 0, 0, 1, 0})));
		EXPECT_TRUE(stateGraph.containsMarking(Marking(net, {0, 0, 1, 0, 1})));
		EXPECT_TRUE(stateGraph.containsMarking(Marking(net, {0, 0, 0, 1, 1})));

		EXPECT_FALSE(stateGraph.containsMarking(Marking(net, {0, 0, 0, 0, 1})));

		// plot(net, "net.svg");
		// plot(stateGraph, "stateGraph.svg");
	}

	TEST(PetriNet, StateGraphComplex3) {
		typedef PetriNet<string, int> PetriNet;
		typedef PetriNet::marking_type Marking;

		PetriNet net;

		net.addPlace("A");
		net.addPlace("B", 3);
		net.addPlace("C");

		net.addPostPlace(1, "A");
		net.addPostPlace(2, "B");

		net.addPrePlace("B", 3);
		net.addPostPlace(3, "C");

		net.addPrePlace("A", 4);

		// create full state graph
		// plot(net);
		auto stateGraph = extractStateGraph(Marking(net, {0, 0, 0}));

		EXPECT_EQ(16, stateGraph.getNumStates());

		// plot(net, "net.svg");
		// plot(stateGraph, "stateGraph.svg");
	}

	TEST(PetriNet, StateGraphComplex4) {
		typedef PetriNet<string, int> PetriNet;
		typedef PetriNet::marking_type Marking;

		PetriNet net;

		net.addPlace("A");
		net.addPlace("B");
		net.addPlace("C");

		net.addPlace("A1");
		net.addPlace("B1");
		net.addPlace("C1");

		net.addPlace("A2");
		net.addPlace("B2");
		net.addPlace("C2");

		net.addPlace("Ch", 2);

		net.addPrePlace("A", 1);
		net.addPostPlace(1, "B");
		net.addPostPlace(1, "A1");
		net.addPostPlace(1, "A2");

		net.addPrePlace("B", 2);
		net.addPrePlace("C1", 2);
		net.addPrePlace("C2", 2);
		net.addPostPlace(2, "C");

		{
			net.addPrePlace("A1", 3);
			net.addPostPlace(3, "B1");
			net.addPostPlace(3, "Ch");

			net.addPrePlace("B1", 4);
			net.addPostPlace(4, "B1");
			net.addPostPlace(4, "Ch");

			net.addPrePlace("B1", 5);
			net.addPostPlace(5, "C1");
			net.addPostPlace(5, "Ch");
		}

		{
			net.addPrePlace("A2", 6);
			net.addPostPlace(6, "B2");
			net.addPrePlace("Ch", 6);

			net.addPrePlace("B2", 7);
			net.addPostPlace(7, "B2");
			net.addPrePlace("Ch", 7);

			net.addPrePlace("B2", 8);
			net.addPostPlace(8, "C2");
			net.addPrePlace("Ch", 8);
		}
		// plot(net, "net.svg");

		// create full state graph
		//		plot(net);
		auto stateGraph = extractStateGraph(Marking(net, {1, 0, 0, 0, 0, 0, 0, 0, 0, 0}));

		EXPECT_EQ(20, stateGraph.getNumStates());
		//		plot(stateGraph, "stateGraph.svg");
	}

} // end namespace petri_net
} // end namespace core
} // end namespace insieme
