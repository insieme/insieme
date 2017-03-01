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

#include <set>
#include <map>
#include <vector>
#include <ostream>

#include <boost/operators.hpp>

#include "insieme/utils/assert.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace utils {
namespace petri_net {

	/**
	 * Within this header file some utility classes and functions for modeling and processing Petri nets are implemented.
	 *
	 * The three main classes are:
	 * 		- PetriNet   .. the class that can be utilized to model a Petri net
	 * 		- Marking    .. a class to model the state of the network (marker assignment)
	 * 		- StateGraph .. a graph summarizing all the markings and transitions between those possible within a petri net
	 */

	// forward declarations
	template <typename Place, typename Transition>
	class PetriNet;
	template <typename Place, typename Transition>
	class Marking;

	/**
	 * The clss utilized to model a PetriNet.
	 *
	 * @tparam Place the type to be used to represent a place within the network
	 * @tparam Transition the type used for modeling transitions within the network
	 */
	template <typename Place = int, typename Transition = int>
	class PetriNet : public boost::equality_comparable<PetriNet<Place, Transition>> {
	  public:
		typedef std::size_t place_idx;
		typedef std::size_t transition_idx;
		typedef Marking<Place, Transition> marking_type;

	  private:
		place_idx p_counter;

		std::map<Place, place_idx> p_index;     // an index of the set of places
		std::map<place_idx, unsigned> capacity; // stores the set of places and their capacity

		std::map<Transition, transition_idx> t_index;                 // an index on the set of transitions
		std::map<transition_idx, std::vector<place_idx>> pre_places;  // a map between transitions (by their index) and predecessor places
		std::map<transition_idx, std::vector<place_idx>> post_places; // a map between transitions (by their index) and successor places

	  public:
		PetriNet() : p_counter(0) {}

		// -- Place Handling --

		void addPlace(const Place& place, unsigned capacity = 1) {
			assert_false(containsPlace(place)) << "Duplicated place: " << place << "\n";
			place_idx idx = p_counter++;
			p_index[place] = idx;
			this->capacity[idx] = capacity;
		}

		void removePlace(const Place& place) {
			assert_true(getNumPreTransitions(place) == 0 && getNumPostTransitions(place) == 0);
			auto pos = p_index.find(place);
			if(pos == p_index.end()) { return; }
			capacity.erase(capacity.find(pos->second));
			p_index.erase(pos);
		}

		bool containsPlace(const Place& place) const {
			return p_index.find(place) != p_index.end();
		}

		bool containsPlace(place_idx idx) const {
			return capacity.find(idx) != capacity.end();
		}

		std::size_t getNumPlaces() const {
			return p_index.size();
		}

		std::size_t getMaxPlaceID() const {
			return p_counter - 1;
		}

		place_idx getPlaceIndex(const Place& place) const {
			assert_true(containsPlace(place)) << "Place " << place << " not registered yet!";
			return p_index.find(place)->second;
		}

		unsigned getCapacity(const Place& place) const {
			assert_true(containsPlace(place)) << "Place " << place << " not registered yet!";
			return getCapacity(getIndex(place));
		}

		unsigned getCapacity(place_idx idx) const {
			assert_true(containsPlace(idx)) << "Place with index " << idx << " not registered yet!";
			return capacity.find(idx)->second;
		}

		unsigned getNumPreTransitions(const Place& p) const {
			auto pos = p_index.find(p);
			if(pos == p_index.end()) { return 0; }

			auto index = pos->second;
			unsigned count = 0;
			for(const auto& cur : post_places) {
				if(contains(cur.second, index)) { count++; }
			}
			return count;
		}

		unsigned getNumPostTransitions(const Place& p) const {
			auto pos = p_index.find(p);
			if(pos == p_index.end()) { return 0; }

			auto index = pos->second;
			unsigned count = 0;
			for(const auto& cur : pre_places) {
				if(contains(cur.second, index)) { count++; }
			}
			return count;
		}

		// -- Transition Handling --

		transition_idx addTransition(const Transition& transition) {
			auto pos = t_index.find(transition);
			if(pos != t_index.end()) { return pos->second; }
			transition_idx idx = t_index.size();
			return t_index[transition] = idx;
		}

		std::size_t getNumTransitions() const {
			return t_index.size();
		}

		void addPrePlace(const Place& place, const Transition& transition) {
			assert_true(containsPlace(place)) << "Place " << place << " not registered yet!";
			auto t_idx = addTransition(transition);
			auto p_idx = getPlaceIndex(place);
			auto& places = pre_places[t_idx];
			if(contains(places, p_idx)) { return; }
			places.push_back(p_idx);
		}

		const vector<place_idx>& getPrePlaces(const Transition& transition) {
			return getPrePlaces(getPlaceIndex(transition));
		}

		const vector<place_idx>& getPrePlaces(const transition_idx& transition) const {
			static const vector<place_idx> empty;
			auto pos = pre_places.find(transition);
			return (pos != pre_places.end()) ? pos->second : empty;
		}

		void addPostPlace(const Transition& transition, const Place& place) {
			assert_true(containsPlace(place)) << "Place " << place << " not registered yet!";
			auto t_idx = addTransition(transition);
			auto p_idx = getPlaceIndex(place);
			auto& places = post_places[t_idx];
			if(contains(places, p_idx)) { return; }
			places.push_back(p_idx);
		}

		const vector<place_idx>& getPostPlaces(const Transition& transition) {
			return getPostPlaces(getPlaceIndex(transition));
		}

		const vector<place_idx>& getPostPlaces(const transition_idx& transition) const {
			static const vector<place_idx> empty;
			auto pos = post_places.find(transition);
			return (pos != post_places.end()) ? pos->second : empty;
		}

		bool operator==(const PetriNet<Place, Transition>& other) const {
			// check for identity
			if(this == &other) { return false; }

			// TODO: this could be done much more efficiently - and independent
			return p_index == other.p_index && capacity == other.capacity && t_index == other.t_index && pre_places == other.pre_places
			       && post_places == other.post_places;
		}

		/**
		 * Writes a DOT dump of this network to the given output stream.
		 */
		void dumpTo(std::ostream& out) const {
			// start with header
			out << "digraph PetriNet {\n";

			// add places
			for(const auto& cur : p_index) {
				out << "\tp" << cur.second << "[shape=circle label=\"" << cur.first << ", c=" << getCapacity(cur.second) << "\"]\n";
			}

			// add transitions
			for(const auto& cur : t_index) {
				out << "\tt" << cur.second << "[shape=box label=\"" << cur.first << "\"]\n";
			}

			// add arcs
			for(const auto& cur : pre_places) {
				transition_idx t = cur.first;
				for(const auto& pre : cur.second) {
					out << "\tp" << pre << " -> t" << t << "\n";
				}
			}

			for(const auto& cur : post_places) {
				transition_idx t = cur.first;
				for(const auto& post : cur.second) {
					out << "\tt" << t << " -> p" << post << "\n";
				}
			}

			// done
			out << "}\n";
		}
	};

	/**
	 * The marker class is summarizing the state of a network by listing the number of marks present within
	 * various places of a Petri network.
	 */
	template <typename Place, typename Transition>
	class Marking : public boost::equality_comparable<Marking<Place, Transition>>,
	                public boost::less_than_comparable<Marking<Place, Transition>>,
	                public utils::Printable {
	  public:
		typedef PetriNet<Place, Transition> net_type;
		typedef typename net_type::place_idx place_idx;
		typedef typename net_type::transition_idx transition_idx;
		typedef std::size_t mark_type;

	  private:
		// the network this marking is referencing to
		const net_type* net;

		// the marking (indexed by the consecutive place_idx of the network)
		std::vector<mark_type> marking;

	  public:
		Marking() : net(nullptr), marking() {}
		Marking(const net_type& net) : net(&net), marking(net.getMaxPlaceID() + 1) {}
		Marking(const net_type& net, const std::vector<mark_type>& marking) : net(&net), marking(marking) {
			assert_eq(marking.size(), net.getMaxPlaceID() + 1);
		}
		Marking(const Marking& other) : net(other.net), marking(other.marking) {}
		Marking(Marking&& other) = default;

		void setMarking(const Place& place, mark_type mark = 1) {
			setMarking(net->getPlaceIndex(place), mark);
		}

		void setMarking(place_idx id, mark_type mark = 1) {
			marking[id] = mark;
		}

		mark_type getMarking(place_idx id) const {
			return marking[id];
		}

		bool isEnabled() const {
			return true;
		}

		template <typename T>
		bool isEnabled(const T& t) const {
			// check pre- and post-places
			const auto& pre = net->getPrePlaces(t);
			const auto& post = net->getPostPlaces(t);
			return all(pre, [&](place_idx p) -> bool { return marking[p] > 0; })
			       && all(post, [&](place_idx p) -> bool { return marking[p] < net->getCapacity(p) + (contains(pre, p) ? 1 : 0); });
		}

		template <typename... Ts>
		bool isEnabled(const Transition& t, const Ts&... rest) const {
			return isEnabled(t) && apply(t).isEnabled(rest...);
		}

		const Marking& apply() const {
			return *this;
		}

		template <typename T>
		Marking apply(const T& t) const {
			return Marking(*this).fireTransition(t);
		}

		template <typename... Ts>
		Marking apply(const Transition& t, const Ts&... rest) const {
			return apply(t).apply(rest...);
		}

		Marking& fireTransition() {
			// nothing to do
			return *this;
		}

		template <typename T>
		Marking& fireTransition(const T& t) {
			assert_true(isEnabled(t)) << "Transition " << t << " is not enabled at marking " << *this << "\n";
			// reduce pre-conditions
			for_each(net->getPrePlaces(t), [&](place_idx p) { --marking[p]; });
			for_each(net->getPostPlaces(t), [&](place_idx p) { ++marking[p]; });
			return *this;
		}

		template <typename... Ts>
		Marking& fireTransition(const Transition& t, const Ts&... rest) {
			// apply transition and the rest
			return fireTransiton(t).fireTransition(rest...);
		}


		std::set<Marking> getSuccessors() const {
			// just check all transitions whether they can be fired
			std::set<Marking> res;

			// run through all transitions
			auto num_transitions = net->getNumTransitions();
			for(transition_idx t = 0; t < num_transitions; ++t) {
				if(isEnabled(t)) { res.insert(apply(t)); }
			}

			// done
			return res;
		}

		bool operator==(const Marking& other) const {
			// same net, same marking
			return net == other.net && marking == other.marking;
		}

		bool operator<(const Marking& other) const {
			assert_true(net == other.net);
			return marking < other.marking;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << marking;
		}
	};

	/**
	 * A class representing the state graph of a Petri Net. The set of nodes is given my by the reachable markings and the
	 * edges by the possible transition between those markings.
	 */
	template <typename Place, typename Transition>
	struct StateGraph : public boost::equality_comparable<StateGraph<Place, Transition>>, public utils::Printable {
		typedef Marking<Place, Transition> marking_type;
		typedef std::size_t marking_idx;

		std::map<marking_type, marking_idx> nodes;
		std::set<std::pair<marking_idx, marking_idx>> edges;

	  public:
		marking_idx getIndex(const marking_type& marking) {
			auto pos = nodes.find(marking);
			if(pos != nodes.end()) { return pos->second; }
			return nodes[marking] = nodes.size();
		}

		bool containsMarking(const marking_type& marking) {
			return nodes.find(marking) != nodes.end();
		}

		std::size_t getNumStates() const {
			return nodes.size();
		}

		void addMarking(const marking_type& a) {
			getIndex(a);
		}

		void addEdge(const marking_type& a, const marking_type& b) {
			auto idxA = getIndex(a);
			auto idxB = getIndex(b);
			edges.insert(std::make_pair(idxA, idxB));
		}

		std::size_t getNumEdges() const {
			return edges.size();
		}

		bool operator==(const StateGraph<Place, Transition>& other) const {
			if(this == &other) { return true; }
			return nodes == other.nodes && edges == other.edges;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "StateGraph(" << nodes.size() << " States, " << edges.size() << " Transitions)";
		}

		void dumpTo(std::ostream& out) const {
			// start with header
			out << "digraph StateGraph {\n";

			// add states
			for(const auto& cur : nodes) {
				out << "\tm" << cur.second << "[shape=box label=\"" << cur.first << "\"]\n";
			}

			// add edges
			for(const auto& cur : edges) {
				out << "\tm" << cur.first << " -> m" << cur.second << "\n";
			}

			// done
			out << "}\n";
		}
	};

	/**
	 * A function extracting a state graph from a given initial marking. The resulting state graph will
	 * contain all reachable states starting from the given initial marking.
	 */
	template <typename P, typename T>
	StateGraph<P, T> extractStateGraph(const Marking<P, T>& initial) {
		typedef Marking<P, T> marking;

		// create resulting graph
		StateGraph<P, T> res;

		// the set of processed markings
		std::set<marking> resolved;

		// the working queue
		std::vector<marking> queue;
		queue.push_back(initial);

		// initial is always included
		res.addMarking(initial);

		while(!queue.empty()) {
			// get current node
			marking cur = queue.back();
			queue.pop_back();

			// mark current state being resolved
			resolved.insert(cur);

			// get all successors
			for(const auto& m : cur.getSuccessors()) {
				res.addEdge(cur, m);
				if(!contains(resolved, m)) { queue.push_back(m); }
			}
		}

		// done
		return res;
	}

} // end namespace petri_net
} // end namespace utils
} // end namespace insieme
