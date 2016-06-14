/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/utils/automata/automata.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {
namespace automata {

	using namespace set;

	typedef Automata<>::state_type State;

	TEST(Automata, Basic) {
		Automata<> a;

		EXPECT_EQ(1u, a.getNumStates());
		EXPECT_TRUE(a.containsState(a.getInitialState()));

		State s = a.getNewState();
		a.setInitialState(s);

		EXPECT_EQ(s, a.getInitialState());

		EXPECT_FALSE(a.isFinalState(s));

		a.setFinalStates(s);
		EXPECT_TRUE(a.isFinalState(s));
	}

	TEST(Automata, Transitions) {
		// create a new automata accepting a single X
		Automata<> a;

		State s1 = a.getNewState();
		State s2 = a.getNewState();

		a.setInitialState(s1);
		a.setFinalState(s2);

		a.addTransition(s1, 'X', s2);

		string print = toDotGraph(a);

		EXPECT_PRED2(containsSubString, print, "digraph");
		EXPECT_PRED2(containsSubString, print, "label=\"X\"");

		a.addEpsilonTransition(s2, s1);
		print = toDotGraph(a);

		EXPECT_PRED2(containsSubString, print, "digraph");
		EXPECT_PRED2(containsSubString, print, "label=\"X\"");
		EXPECT_PRED2(containsSubString, print, "label=\"<eps>\"");
	}

	TEST(Automata, EpsilonClosure) {
		// create a new automata accepting a single X
		Automata<> a;

		State s1 = a.getNewState();
		State s2 = a.getNewState();
		State s3 = a.getNewState();

		a.setInitialState(s1);
		a.setFinalState(s2);

		a.addTransition(s1, 'X', s2);

		// get epsilon closure
		EXPECT_EQ(toSet<std::set<State>>(s1), a.getEpsilonClosure(s1));
		EXPECT_EQ(toSet<std::set<State>>(s2), a.getEpsilonClosure(s2));
		EXPECT_EQ(toSet<std::set<State>>(s3), a.getEpsilonClosure(s3));


		// add an edge 1 -> 2
		a.addEpsilonTransition(s1, s2);
		EXPECT_EQ(toSet<std::set<State>>(s1, s2), a.getEpsilonClosure(s1));
		EXPECT_EQ(toSet<std::set<State>>(s2), a.getEpsilonClosure(s2));
		EXPECT_EQ(toSet<std::set<State>>(s3), a.getEpsilonClosure(s3));


		// add an edge 2 -> 3
		a.addEpsilonTransition(s2, s3);
		EXPECT_EQ(toSet<std::set<State>>(s1, s2, s3), a.getEpsilonClosure(s1));
		EXPECT_EQ(toSet<std::set<State>>(s2, s3), a.getEpsilonClosure(s2));
		EXPECT_EQ(toSet<std::set<State>>(s3), a.getEpsilonClosure(s3));

		// add an edge 3 -> 3 (should not change anything
		a.addEpsilonTransition(s3, s3);
		EXPECT_EQ(toSet<std::set<State>>(s1, s2, s3), a.getEpsilonClosure(s1));
		EXPECT_EQ(toSet<std::set<State>>(s2, s3), a.getEpsilonClosure(s2));
		EXPECT_EQ(toSet<std::set<State>>(s3), a.getEpsilonClosure(s3));

		// create a cycle 3 -> 2
		a.addEpsilonTransition(s3, s2);
		EXPECT_EQ(toSet<std::set<State>>(s1, s2, s3), a.getEpsilonClosure(s1));
		EXPECT_EQ(toSet<std::set<State>>(s2, s3), a.getEpsilonClosure(s2));
		EXPECT_EQ(toSet<std::set<State>>(s2, s3), a.getEpsilonClosure(s3));

		// create a cycle 2 -> 1
		a.addEpsilonTransition(s2, s1);
		EXPECT_EQ(toSet<std::set<State>>(s1, s2, s3), a.getEpsilonClosure(s1));
		EXPECT_EQ(toSet<std::set<State>>(s1, s2, s3), a.getEpsilonClosure(s2));
		EXPECT_EQ(toSet<std::set<State>>(s1, s2, s3), a.getEpsilonClosure(s3));
	}

	TEST(Automata, ReachableStates) {
		// create a new automata accepting a single X
		Automata<> a;

		State s1 = a.getNewState();
		State s2 = a.getNewState();
		State s3 = a.getNewState();

		a.setInitialState(s1);
		a.setFinalState(s2);

		a.addTransition(s1, 'X', s2);
		EXPECT_EQ(toSet<std::set<State>>(s1, s2), a.getReachableStates(s1));
		EXPECT_EQ(toSet<std::set<State>>(s2), a.getReachableStates(s2));
		EXPECT_EQ(toSet<std::set<State>>(s3), a.getReachableStates(s3));

		a.addEpsilonTransition(s2, s3);
		EXPECT_EQ(toSet<std::set<State>>(s1, s2, s3), a.getReachableStates(s1));
		EXPECT_EQ(toSet<std::set<State>>(s2, s3), a.getReachableStates(s2));
		EXPECT_EQ(toSet<std::set<State>>(s3), a.getReachableStates(s3));

		a.addEpsilonTransition(s2, s1);
		EXPECT_EQ(toSet<std::set<State>>(s1, s2, s3), a.getReachableStates(s1));
		EXPECT_EQ(toSet<std::set<State>>(s1, s2, s3), a.getReachableStates(s2));
		EXPECT_EQ(toSet<std::set<State>>(s3), a.getReachableStates(s3));
	}

	TEST(Automata, NFAConversion) {
		// create a new automata accepting a single X
		Automata<> a;

		State s1 = a.getNewState();
		State s2 = a.getNewState();

		a.setInitialState(s1);
		a.setFinalState(s2);

		a.addTransition(s1, 'X', s2);
		a.addEpsilonTransition(s2, s1);

		auto res = toNFA(a);

		string eNFA = toDotGraph(a);
		string NFA = toDotGraph(res);

		EXPECT_PRED2(containsSubString, eNFA, "0 -> 1 [label=\"X\"]");
		EXPECT_PRED2(containsSubString, eNFA, "1 -> 0 [label=\"<eps>\"]");

		EXPECT_PRED2(containsSubString, NFA, "0 -> 1 [label=\"X\"]");
		EXPECT_PRED2(containsSubString, NFA, "1 -> 1 [label=\"X\"]");


		// another example
		a.setInitialState(s2);
		res = toNFA(a);

		eNFA = toDotGraph(a);
		NFA = toDotGraph(res);

		EXPECT_PRED2(containsSubString, eNFA, "0 -> 1 [label=\"<eps>\"]");
		EXPECT_PRED2(containsSubString, eNFA, "1 -> 0 [label=\"X\"]");

		EXPECT_PRED2(containsSubString, NFA, "0 -> 0 [label=\"X\"]");
		EXPECT_PRED2(notContainsSubString, NFA, "1");
	}

	TEST(Automata, Accept) {
		// create a new automata accepting a single X
		eNFA<> a;

		State s1 = a.getNewState();
		State s2 = a.getNewState();

		a.setInitialState(s1);
		a.setFinalState(s2);

		a.addTransition(s1, 'X', s2);

		auto n = toNFA(a);
		EXPECT_FALSE(accepts(n, string("")));
		EXPECT_TRUE(accepts(n, string("X")));
		EXPECT_FALSE(accepts(n, string("XX")));

		EXPECT_EQ(accepts(n, string("")), accepts(a, string("")));
		EXPECT_EQ(accepts(n, string("X")), accepts(a, string("X")));
		EXPECT_EQ(accepts(n, string("XX")), accepts(a, string("XX")));
	}

	TEST(Automata, Accept2) {
		// a larger example
		Automata<> a;

		State s1 = a.getNewState();
		State s2 = a.getNewState();
		State s3 = a.getNewState();

		a.setInitialState(s1);
		a.setFinalState(s3);

		a.addTransition(s1, 'a', s2);
		a.addTransition(s2, 'b', s2);
		a.addTransition(s2, 'c', s2);
		a.addTransition(s2, 'd', s3);

		// should work for eNFA
		EXPECT_FALSE(accepts(a, string("")));
		EXPECT_FALSE(accepts(a, string("X")));
		EXPECT_FALSE(accepts(a, string("XX")));

		EXPECT_TRUE(accepts(a, string("ad")));
		EXPECT_TRUE(accepts(a, string("abbbd")));
		EXPECT_TRUE(accepts(a, string("abcbd")));
		EXPECT_TRUE(accepts(a, string("acbcd")));

		EXPECT_TRUE(accepts(a, string("abd")));
		EXPECT_TRUE(accepts(a, string("abcd")));

		EXPECT_FALSE(accepts(a, string("abda")));
		EXPECT_FALSE(accepts(a, string("d")));

		// and for NFA
		auto n = toNFA(a);

		EXPECT_FALSE(accepts(n, string("")));
		EXPECT_FALSE(accepts(n, string("X")));
		EXPECT_FALSE(accepts(n, string("XX")));

		EXPECT_TRUE(accepts(n, string("ad")));
		EXPECT_TRUE(accepts(n, string("abbbd")));
		EXPECT_TRUE(accepts(n, string("abcbd")));
		EXPECT_TRUE(accepts(n, string("acbcd")));

		EXPECT_TRUE(accepts(n, string("abd")));
		EXPECT_TRUE(accepts(n, string("abcd")));

		EXPECT_FALSE(accepts(n, string("abda")));
		EXPECT_FALSE(accepts(n, string("d")));
	}

	TEST(Automata, IntegerTest) {
		typedef eNFA<int>::state_type EState;

		eNFA<int> a;

		EState s1 = a.getNewState();
		EState s2 = a.getNewState();
		EState s3 = a.getNewState();

		a.addTransition(s1, 1, s2);
		a.addTransition(s2, 2, s2);
		a.addTransition(s2, 3, s3);

		a.setInitialState(s1);
		a.setFinalState(s3);

		EXPECT_TRUE(accepts(a, toVector(1, 2, 3)));
		EXPECT_TRUE(accepts(a, toVector(1, 3)));
		EXPECT_TRUE(accepts(a, toVector(1, 2, 2, 3)));

		EXPECT_FALSE(accepts(a, toVector(1, 2)));
	}


} // end namespace automata
} // end namespace core
} // end namespace insieme
