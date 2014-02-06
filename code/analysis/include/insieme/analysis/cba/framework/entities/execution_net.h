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

#pragma once

#include <set>

#include "insieme/analysis/cba/framework/context.h"
#include "insieme/analysis/cba/framework/entities/thread_region.h"
#include "insieme/analysis/cba/framework/entities/channel.h"


#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/petri_net/petri_net.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * An execution network is a Petri net where:
	 * 		- places are thread phases and channel states
	 * 		- transitions are introduced by
	 * 				- non-deterministic choice
	 * 				- spawn / merge operations
	 * 				- channel operations
	 * 				- redistribute operations
	 */

	/**
	 * A place can either be:
	 * 		- a thread region
	 * 		- a channel buffer
	 * 		- a auxiliary construct for non-deterministic choices
	 */
	enum PlaceKind {
		Reg, Chl, Aux
	};

	template<typename Context>
	struct Place : public utils::Printable {

		PlaceKind kind;

		int id;

		Place() : kind(Aux), id(0) {}

		Place(PlaceKind kind, int id)
			: kind(kind), id(id) {}

		bool operator==(const Place<Context>& other) const {
			return kind == other.kind && id == other.id;
		}

		bool operator<(const Place<Context>& other) const {
			// compare kinds
			if (kind != other.kind) return kind < other.kind;

			// otherwise compare ids
			return id < other.id;
		}

		std::ostream& printTo(std::ostream& out) const {
			if (kind == Aux && id == 0) return out << "dummy";
			switch(kind) {
			case Reg: 		return out << "R(" << id << ")";
			case Chl:		return out << "C(" << id << ")";
			case Aux: 		return out << "A(" << id << ")";
			}
			assert_fail() << "Invalid object state!";
			return out << "-unknown-";
		}
	};


	/**
	 * A transition can be:
	 * 		- a spawn
	 * 		- a merge
	 * 		- a redistribute
	 * 		- a channel send / recv operation
	 */
	enum TransitionKind {
		Spawn,
		Merge,
		Send,
		Recv,
		Redistribute,
		Choice
	};

	template<typename Context>
	struct Transition : public utils::Printable {

		TransitionKind kind;

		int id;

		Transition() : kind(Spawn), id(0) {}

		Transition(TransitionKind kind, int id)
			: kind(kind), id(id) {}

		bool operator==(const Transition<Context>& other) const {
			return kind == other.kind && id == other.id;
		}

		bool operator<(const Transition<Context>& other) const {
			// compare kinds
			if (kind != other.kind) return kind < other.kind;

			// otherwise compare ids
			return id < other.id;
		}

		std::ostream& printTo(std::ostream& out) const {
			switch(kind) {
			case Spawn: 		return out << "Sp(" << id << ")";
			case Merge: 		return out << "M(" << id << ")";
			case Send: 			return out << "S(" << id << ")";
			case Recv: 			return out << "R(" << id << ")";
			case Redistribute: 	return out << "Rd(" << id << ")";
			case Choice: 		return out << "C(" << id << ")";
			}
			assert_fail() << "Invalid object state!";
			return out << "-unknown-";
		}

	};

	template<typename Context>
	class ExecutionNet :
			public utils::petri_net::PetriNet<Place<Context>, Transition<Context>>,
			public utils::Printable
	{

		int id;

		std::set<Place<Context>> initialPlaces;

	public:

		ExecutionNet() : id(0) {}

		Place<Context> createRegion(const ThreadRegion<Context>& region) {
			Place<Context> place(Reg, ++id);
			addPlace(place);
			return place;
		}

		Place<Context> createChannel(const Channel<Context>& channel) {
			Place<Context> place(Chl, ++id);
			addPlace(place);		// TODO: add channel size
			return place;
		}

		Place<Context> createAuxiliary() {
			Place<Context> place(Aux, ++id);
			addPlace(place);
			return place;
		}

		void markInitial(const Place<Context>& place) {
			assert_true(containsPlace(place)) << "Unknown place " << place;
			initialPlaces.insert(place);
		}

		unsigned getNumInitialPlaces() const {
			return initialPlaces.size();
		}

		const std::set<Place<Context>>& getInitialPlaces() const {
			return initialPlaces;
		}

		Transition<Context> createTransition(const ProgramPoint<Context>& point) {
			TransitionKind kind = Spawn;
			if (point.isSpawn()) {
				kind = Spawn;
			} else if (point.isMerge() || point.isMergeAll()) {
				kind = Merge;
			} else if (point.isSend()) {
				kind = Send;
			} else if (point.isRecv()) {
				kind = Recv;
			} else if (point.isRedistribute()) {
				kind = Redistribute;
			} else {
				assert_fail() << "Unsupported sync-point encountered: " << point;
			}
			Transition<Context> trans(kind, ++id);
			return trans;
		}

		void link(const Place<Context>& a, const Place<Context>& b) {
			Transition<Context> trans(Choice, ++id);
			addPrePlace(a, trans);
			addPostPlace(trans, b);
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "ExecutionNet(" << this->getNumPlaces() << " Places / " << this->getNumTransitions() << " Transitions / " << initialPlaces.size() << " Initial Places)";
		}

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
