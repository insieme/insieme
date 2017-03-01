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
#pragma once

#include <map>
#include <set>
#include <vector>
#include <sstream>

#include <boost/noncopyable.hpp>
#include <boost/dynamic_bitset.hpp>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/range.h"

namespace insieme {
namespace utils {
namespace automata {

	using std::vector;


	/**
	 * This class provides a generic implementation of an e-NFA (non-deterministic
	 * finite automata with epsilon transitions). Since e-NFAs have the widest range
	 * of features and all other automatas are just specializations of NFAs, it
	 * represents the foundation and fallback implementation of all FAs within this
	 * utility collection.
	 *
	 * This implementation also serves as a builder class for all other automata.
	 * It is designed such that manipulation operations like adding / removing
	 * states / transitions or merging two automata has low complexity. For conducting
	 * the actual acceptance tests, the represented automata may be converted into
	 * an equivalent NFA for even further since conducting matching operations within
	 * such FAs is much more efficient.
	 *
	 * Usage:
	 * 	To create an automata, simply create an instance of this class.
	 *
	 * 		Automata<> a;
	 *
	 * 	It will create an automata containing a single (initial) state and no transition.
	 * 	New states can be added by invoking
	 *
	 * 		state_type s = a.getNewState();
	 *
	 * 	where state_type is defined as a member type (for all FAs). Transitions can
	 * 	then be added using
	 *
	 * 		a.addTransition(s1, p, s2);
	 *
	 * 	where p is the character / pattern to be matched along this path. Finally, using
	 * 	the accept method, it can be tested whether a string of characters is accepted
	 * 	by this automata
	 *
	 * 		a.accepts(s1, begin, end);
	 *
	 * 	or for convenience
	 *
	 * 		accepts(a, container);
	 *
	 *
	 * @tparam Pattern the kind of letter / pattern to be checked along the transitions
	 * @tparam Matcher the routine used to check a pattern, default: == operator
	 */
	template <typename Pattern = char, typename Matcher = std::equal_to<Pattern>>
	class Automata : public boost::noncopyable {
		/**
		 * The internal class used to represent a state.
		 */
		struct State : public boost::noncopyable {
			/**
			 * The type used to store outgoing transitions to other states. Each
			 * pattern (the label of the outgoing transition) is mapped to pointers
			 * to the successor states.
			 */
			typedef std::multimap<Pattern, const State*> TransitionMap;

			/**
			 * The out-going transitions of this state.
			 */
			TransitionMap transitions;
		};

	  public:
		// -- Some member type definitions ----------------

		/**
		 * The type to be used to handle states of this automata implementation.
		 */
		typedef const State* state_type;

		/**
		 * The type used to represent transitions within this automata.
		 */
		typedef typename State::TransitionMap::value_type transition_type;

		/**
		 * The type of a functor allowing to extract the pattern from an transition.
		 */
		typedef extractFirst<transition_type> pattern_extractor;

		/**
		 * The type of a functor allowing to extract the target state from an transition.
		 */
		typedef extractSecond<transition_type> target_extractor;

		/**
		 * The type of iterator to be used for iterating over a list of transitions.
		 */
		typedef typename State::TransitionMap::const_iterator transition_iterator;


	  private:
		// -- Member Fields --------------------------

		/**
		 * The initial state of this e-NFA.
		 */
		state_type initState;

		/**
		 * The final or accepting states of this e-NFA.
		 */
		std::set<state_type> finalStates;

		/**
		 * A collection of all involved / owned states for memory management.
		 */
		std::set<state_type> allStates;

		/**
		 * The epsilon pattern.
		 */
		Pattern epsilon;

	  public:
		/**
		 * Creates a new automata using the given pattern as an epsilon token.
		 *
		 * @param epsilon the pattern to be considered as an epsilon.
		 */
		Automata(const Pattern& epsilon = Pattern()) : epsilon(epsilon) {
			// initialize the initial state
			setInitialState(getNewState());
		}

		/**
		 * Destroys this automata properly.
		 */
		~Automata() {
			for_each(allStates, [](const state_type cur) { delete cur; });
		}

		// -- Getter/Setter --------------------------------------

		/**
		 * Obtains the total number of states currently maintained by this
		 * automata.
		 *
		 * @return the number of managed states
		 */
		std::size_t getNumStates() const {
			return allStates.size();
		}

		/**
		 * Obtains a reference to the pattern considered to be epsilon by this
		 * e-NFA implementation.
		 *
		 * @return a reference to an epsilon instance.
		 */
		const Pattern& getEpsilonPattern() const {
			return epsilon;
		}

		/**
		 * Allows to check whether the given state is part of this automata.
		 *
		 * @param state the state to be checked
		 * @return true if it is part of this automata, false otherwise
		 */
		bool containsState(state_type state) const {
			return allStates.find(state) != allStates.end();
		}

		/**
		 * The terminal case for the list-variant of the contains state method.
		 *
		 * @return always true.
		 */
		bool containsStates() {
			return true;
		}

		/**
		 * A recursive implementation of the contains states check for a given
		 * list of states.
		 */
		template <typename... T>
		bool containsStates(state_type first, T... rest) {
			return containsState(first) && containsState(rest...);
		}

		/**
		 * Updates the initial state of this automata.
		 *
		 * @param state the new state to be the initial state
		 */
		void setInitialState(state_type state) {
			assert_true(containsState(state)) << "Initial state must by a local state!";
			initState = state;
		}

		/**
		 * Obtains the currently selected initial state.
		 *
		 * @return the currently selected initial state
		 */
		state_type getInitialState() const {
			return initState;
		}

		/**
		 * Test whether the given state is a final state.
		 *
		 * @param state the state to be tested
		 * @return true if so, false otherwise
		 */
		bool isFinalState(state_type state) const {
			return contains(finalStates, state);
		}

		/**
		 * Updates the final states to the given set of states.
		 *
		 * @param state the first state
		 * @param rest the remaining states
		 */
		template <typename... T>
		void setFinalStates(state_type state, T... rest) {
			assert_true(containsState(state, rest...)) << "Final states have to be part of this automata!";
			finalStates = set::toSet<std::set<state_type>>(state, rest...);
		}

		/**
		 * Updates this automata to only have a single final state.
		 *
		 * @param state the new final / accepting state
		 */
		void setFinalState(state_type state) {
			assert_true(containsState(state)) << "Final state should be handled by this automata!";
			setFinalStates(state);
		}

		/**
		 * Obtains a reference to the set of final states of this automata.
		 *
		 * @return a reference to the set of final states of this automata.
		 */
		const std::set<state_type>& getFinalStates() const {
			return finalStates;
		}


		// -- State and Transition manipulation -------------------

		/**
		 * Creates and returns a new state within this automata.
		 *
		 * @return the newly generated state
		 */
		state_type getNewState() {
			state_type state = new State();
			allStates.insert(state);
			return state;
		}

		/**
		 * Adds an epsilon transition between the two given states.
		 *
		 * @param from the source of the transition
		 * @param to the sink of the transition
		 */
		void addEpsilonTransition(state_type from, state_type to) {
			assert_true(containsStates(from, to)) << "From and to states have to be part of this automata!";
			State* source = const_cast<State*>(from);
			source->transitions.insert(std::make_pair(epsilon, to));
		}

		/**
		 * Adds a transition between the two given states labeled with the given pattern.
		 *
		 * @param from the source of the transition
		 * @param pattern the label of the transition
		 * @param to the sink of the transition
		 */
		void addTransition(state_type from, const Pattern& pattern, state_type to) {
			assert_true(containsStates(from, to)) << "From and to states have to be part of this automata!";
			// add transition to state
			State* source = const_cast<State*>(from);
			source->transitions.insert(std::make_pair(pattern, to));
		}

		/**
		 * Obtains a range covering all outgoing transitions leafing the given state.
		 *
		 * @param state the state which's outgoing transitions are requested
		 * @return a pair containing the start / end of the requested range
		 */
		utils::range<transition_iterator> getOutgoingTransitions(state_type state) const {
			return utils::make_range(state->transitions.begin(), state->transitions.end());
		}

		/**
		 * Computes the set of all reachable states within this automata.
		 *
		 * @return the set of reachable states within this automata
		 */
		std::set<state_type> getReachableStates() const {
			return getReachableStates(initState);
		}

		/**
		 * Computes the set of all reachable states within this automata
		 * when starting from the given state.
		 *
		 * @param start the starting state
		 * @return the set of reachable states
		 */
		std::set<state_type> getReachableStates(state_type start) const {
			std::set<state_type> res;
			addReachableStates(start, res);
			return res;
		}

		/**
		 * Computes the epsilon closure of the given state.
		 *
		 * @param state the state which's closure is requested
		 * @return the set of all states within the state's closure
		 */
		std::set<state_type> getEpsilonClosure(state_type state) const {
			std::set<state_type> res;
			buildEpsilonClosure(state, res);
			return res;
		}

	  private:
		/**
		 * A utility function implementing the actual computation of reachable states.
		 */
		void addReachableStates(state_type state, std::set<state_type>& res) const {
			// add new state ...
			if(!res.insert(state).second) {
				return; // has been present already => no further resolution required
			}

			// resolve successors
			for_each(state->transitions, [&](const transition_type& cur) { this->addReachableStates(cur.second, res); });
		}

		/**
		 * A utility function implementing the actual computation of epsilon closures.
		 */
		void buildEpsilonClosure(state_type state, std::set<state_type>& res) const {
			// add new state ...
			if(!res.insert(state).second) {
				return; // has been present already => no further resolution required
			}

			// resolve successors
			for_each(state->transitions.equal_range(epsilon), [&](const transition_type& cur) { this->buildEpsilonClosure(cur.second, res); });
		}

	  public:
		/**
		 * Determines whether the given range is accepted by this automata. Computing
		 * whether a range is accepted by an e-NFA is rather expensive. Usually converting
		 * the e-NFA in an NFA or even DFA results in a significant reduction of the matching
		 * times.
		 *
		 * @param begin the begin of the range
		 * @param end the end of the range
		 * @return true if accepted, false otherwise
		 */
		template <typename Iter>
		bool accept(Iter begin, Iter end) const {
			typedef typename Iter::value_type CharType;
			typedef typename std::set<state_type> StateSet;

			// maintain list of potential current states
			//  + use initial states to start with
			StateSet current = getEpsilonClosure(getInitialState());

			// work through string
			for_each(begin, end, [&](const CharType& character) {

				// collect new set of states
				StateSet newStates;
				for_each(current, [&](const state_type& state) {
					for_each(this->getOutgoingTransitions(state), [&](const transition_type& cur) {
						static Matcher matcher;
						if(matcher(cur.first, character)) { set::insertAll(newStates, this->getEpsilonClosure(cur.second)); }
					});
				});

				current = newStates;
			});

			// it is an accept if one of the final states is reached
			return !set::intersect(current, finalStates).empty();
		}

		/**
		 * Imports all states from the given automata, thereby clearing the given
		 * automata. Hence, after the operation, the handed in automate will be empty.
		 * Effectively, the ownership of all states of the given automata is transfered
		 * to this automata.
		 *
		 * @param other the automata which's state should be imported - it will be cleared
		 */
		void import(Automata<Pattern, Matcher>& other) {
			// import all states of the other automata
			allStates.insert(other.allStates.begin(), other.allStates.end());

			// re-initialize other automata
			other.allStates.clear();
			other.finalStates.clear();

			// restore invariant
			other.initState = other.getNewState();
		}
	};


	/**
	 * A alias for the automata naming it as an e-NFA.
	 *
	 * @tparam Pattern the kind of letter / pattern to be checked along the transitions
	 * @tparam Matcher the routine used to check a pattern, default: == operator
	 */
	template <typename Pattern = char, typename Matcher = std::equal_to<Pattern>>
	class eNFA : public Automata<Pattern, Matcher> {};

	/**
	 * An NFA implementation managing states in a more compact way than the default
	 * Automata / eNFA implementation. However, after a creation of an NFA, its number
	 * of states can no longer be changed.
	 *
	 * @tparam Pattern the kind of letter / pattern to be checked along the transitions
	 * @tparam Matcher the routine used to check a pattern, default: == operator
	 */
	template <typename Pattern = char, typename Matcher = std::equal_to<Pattern>>
	class NFA {
	  public:
		// -- Some member type definitions ----------------

		/**
		 * The type to be used to handle states of this automata implementation.
		 */
		typedef size_t state_type;

		/**
		 * The type used to represent transitions within this automata.
		 */
		typedef std::pair<Pattern, state_type> transition_type;

		/**
		 * The type of a functor allowing to extract the pattern from an transition.
		 */
		typedef extractFirst<transition_type> pattern_extractor;

		/**
		 * The type of a functor allowing to extract the target state from an transition.
		 */
		typedef extractSecond<transition_type> target_extractor;

		/**
		 * The type of iterator to be used for iterating over a list of transitions.
		 */
		typedef typename std::set<transition_type>::const_iterator transition_iterator;


	  private:
		// -- internal type definitions ---

		/**
		 * A type used to represent subsets of states.
		 */
		typedef boost::dynamic_bitset<> StateMask;

		/**
		 * The initial state of this NFA.
		 */
		state_type initialState;

		/**
		 * The container for the transitions modeled as an adjacency list.
		 */
		vector<std::set<transition_type>> transitions;

		/**
		 * A mask identifying the subset of states being accepting states.
		 */
		StateMask finalStates;

	  public:
		/**
		 * Creates a new NFA with the given number of states.
		 *
		 * @param numStates the number of states to be contained
		 */
		NFA(unsigned numStates) : initialState(0), transitions(numStates), finalStates(numStates) {
			assert_gt(numStates, 0) << "NFA must not be empty!";
		};

		/**
		 * Allows to test whether a given state is part of this NFA.
		 *
		 * @param state the state to be tested
		 * @return true if so, false otherwise
		 */
		bool hasState(state_type state) const {
			return 0 <= state && state < transitions.size();
		}

		/**
		 * Adds a transition between the two given states labeled with the given pattern.
		 *
		 * @param from the source of the transition
		 * @param pattern the label of the transition
		 * @param to the sink of the transition
		 */
		void addTransition(state_type from, const Pattern& pattern, state_type to) {
			assert_true(hasState(from) && hasState(to)) << "From and to-state have to be present!";
			// add transition
			transitions[from].insert(std::make_pair(pattern, to));
		}

		/**
		 * Updates the initial state of this NFA.
		 *
		 * @param state the new initial state
		 */
		void setInitialState(state_type state) {
			assert_true(hasState(state)) << "Initial state has to be present!";
			initialState = state;
		}

		/**
		 * Obtains the current initial state.
		 *
		 * @return the current initial state.
		 */
		state_type getInitialState() const {
			return initialState;
		}

		/**
		 * Adds a state to the list of final states.
		 *
		 * @param state the state to be added
		 */
		void addFinalState(state_type state) {
			assert_true(hasState(state)) << "Final state must be a state of this automata!";
			finalStates[state] = true;
		}

		/**
		 * Tests whether the given state is a final state of this NFA.
		 *
		 * @param state the state to be tested
		 * @return true if so, false otherwise
		 */
		bool isFinalState(state_type state) const {
			assert_true(hasState(state)) << "State not part of this automata!";
			return finalStates[state];
		}

		/**
		 * Obtains a range covering all outgoing transitions leafing the given state.
		 *
		 * @param state the state which's outgoing transitions are requested
		 * @return a pair containing the start / end of the requested range
		 */
		std::pair<transition_iterator, transition_iterator> getOutgoingTransitions(state_type state) const {
			return std::make_pair(transitions[state].begin(), transitions[state].end());
		}

		/**
		 * Determines whether the given range is accepted by this automata. Computing
		 * whether a range is accepted by an e-NFA is rather expensive. Usually converting
		 * the e-NFA in an NFA or even DFA results in a significant reduction of the matching
		 * times.
		 *
		 * @param begin the begin of the range
		 * @param end the end of the range
		 * @return true if accepted, false otherwise
		 */
		template <typename Iter>
		bool accept(Iter begin, Iter end) const {
			typedef typename Iter::value_type CharType;

			// use a bit-mask to record active states
			std::size_t numStates = transitions.size();
			StateMask mask(numStates);

			// use initial states to start with
			mask[initialState] = true;

			// work through string
			for_each(begin, end, [&](const CharType& character) {

				// collect new set of states
				StateMask newStates(numStates);
				for(state_type i = 0; i < numStates; i++) {
					if(mask[i]) {
						for_each(this->transitions[i], [&](const transition_type& cur) {
							static Matcher matcher;
							if(matcher(cur.first, character)) { newStates[cur.second] = true; }
						});
					}
				}

				mask = newStates;
			});

			// it is an accept if one of the final states is reached
			mask &= finalStates;
			return mask.any();
		}
	};


	/**
	 * Converts the given automata into an equivalent NFA by eliminating all
	 * epsilon transitions.
	 *
	 * @tparam P the type of pattern used to label edges
	 * @tparam M the matching routine used
	 * @param automata the automata to be converted
	 * @return an equivalent NFA
	 */
	template <typename P, typename M>
	NFA<P, M> toNFA(const Automata<P, M>& automata) {
		typedef typename Automata<P, M>::state_type State;
		typedef const std::set<State> StateSet;

		// get all reachable states
		StateSet reachable = automata.getReachableStates();

		// assign each state its state set ID
		std::map<State, unsigned> ids;
		std::map<StateSet, unsigned> setIds;

		for_each(reachable, [&](State cur) {
			StateSet set = automata.getEpsilonClosure(cur);
			auto pos = setIds.find(set);
			if(pos != setIds.end()) {
				ids[cur] = pos->second;
			} else {
				unsigned newId = setIds.size();
				setIds[set] = newId;
				ids[cur] = newId;
			}
		});

		// get epsilon
		P epsilon = automata.getEpsilonPattern();

		// create resulting NFA
		NFA<P, M> res(setIds.size());

		// fix initial state
		res.setInitialState(ids[automata.getInitialState()]);

		// add transitions
		for_each(setIds, [&](const typename std::map<StateSet, unsigned>::value_type& cur) {
			const StateSet& set = cur.first;
			unsigned from = cur.second;

			for_each(set, [&](const typename Automata<>::state_type& elementState) {
				for_each(automata.getOutgoingTransitions(elementState), [&](const typename Automata<>::transition_type& cur) {
					static typename Automata<P, M>::pattern_extractor extractPattern;
					static typename Automata<P, M>::target_extractor extractTarget;

					P pattern = extractPattern(cur);
					if(pattern != epsilon) { // skip epsilon transitons
						unsigned to = ids[extractTarget(cur)];
						res.addTransition(from, pattern, to);
					}
				});
			});
		});

		// add final states
		for_each(automata.getFinalStates(), [&](State cur) { res.addFinalState(ids[cur]); });

		// done
		return res;
	}

	/**
	 * A generic wrapper allowing to test whether the given automata
	 * is accepting the given range.
	 *
	 * @param automata the automata to be used for testing
	 * @param begin the begin of the range
	 * @param end the end of the range
	 * @return true if accepted, false otherwise false
	 */
	template <template <typename P, typename M> class A, typename P, typename M, typename Iterator>
	bool accepts(const A<P, M>& automata, Iterator begin, Iterator end) {
		return automata.accept(begin, end);
	}

	/**
	 * A generic wrapper allowing to test whether the given automata
	 * is accepting the sequence stored within the given container.
	 *
	 * @param automata the automata to be used for testing
	 * @param container the content to be tested
	 * @return true if accepted, false otherwise false
	 */
	template <template <typename P, typename M> class A, typename P, typename M, typename Container>
	bool accepts(const A<P, M>& automata, const Container& container) {
		return accepts(automata, container.begin(), container.end());
	}


	// -- Automata Printer --------------------------------------------


	namespace detail {

		/**
		 * A utility struct generating a DOT description of an automata.
		 */
		template <template <typename P, typename M> class A, typename P, typename M>
		struct GraphPrinter {
			typedef typename A<P, M>::state_type State;
			typedef typename A<P, M>::transition_type Transition;

			std::set<State> resolved;

			std::map<State, int> index;

			const A<P, M>& automata;

			GraphPrinter(const A<P, M>& automata) : automata(automata) {}

			int getStateIndex(State cur) {
				auto res = index.find(cur);
				if(res != index.end()) { return res->second; }
				int newIndex = index.size();
				index.insert(std::make_pair(cur, newIndex));
				return newIndex;
			}

			void print(std::stringstream& out, State cur) {
				// test whether state has been resolved before
				if(resolved.find(cur) != resolved.end()) { return; }
				resolved.insert(cur);

				// add this state
				int from = getStateIndex(cur);
				if(automata.isFinalState(cur)) { out << "    " << from << " [shape=doublecircle]\n"; }

				// handle individual transitons
				vector<State> next;
				for_each(automata.getOutgoingTransitions(cur), [&](const Transition& transition) {
					out << "    " << from << " -> " << this->getStateIndex(transition.second);
					if(transition.first) {
						out << " [label=\"" << transition.first << "\"]\n";
					} else {
						out << " [label=\"<eps>\"]\n";
					}
					next.push_back(transition.second);
				});

				// print successor states
				for_each(next, [&](State successor) { this->print(out, successor); });
			}

			string print() {
				std::stringstream ss;
				ss << "digraph g {\n";

				State initState = automata.getInitialState();
				int initID = getStateIndex(initState);
				ss << "    " << initID << " [label=I]\n";

				// print remaining states
				print(ss, initState);

				ss << "}\n";
				return ss.str();
			}
		};
	}

	/**
	 * Provides a DOT description of the graphical representation of the given
	 * automata.
	 *
	 * @param automata the automata to be printed.
	 * @return a dot plot representing the given automata
	 */
	template <template <typename P, typename M> class A, typename P, typename M>
	string toDotGraph(const A<P, M>& automata) {
		// use graph printer and its internal state for the printing
		return detail::GraphPrinter<A, P, M>(automata).print();
	}


} // end namespace automata
} // end namespace utils
} // end namespace insieme
