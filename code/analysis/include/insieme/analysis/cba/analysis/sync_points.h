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

#include "insieme/analysis/cba/framework/analysis_type.h"
#include "insieme/analysis/cba/framework/entities/program_point.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/analysis/cba/analysis/reaching_sync_points.h"
#include "insieme/analysis/cba/analysis/thread_bodies.h"

#include "insieme/analysis/cba/utils/cba_utils.h"
#include "insieme/analysis/cba/utils/constraint_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * This class implements an analysis collecting all synchronization points that are processed before
	 * a given program point without any other synchronization point in between. This analysis is mainly
	 * utilized for computing thread regions.
	 */

	// ----------------- reaching sync points ---------------

	template<typename Context> class SyncPointsConstraintGenerator;

	struct sync_points_analysis {
		template<typename C> struct lattice   { typedef utils::constraint::SetLattice<ProgramPoint<typename C::context_type>> type; };
		template<typename C> struct generator { typedef SyncPointsConstraintGenerator<typename C::context_type> type; };
		template<typename C> struct params    { typedef std::tuple<AnalysisType> type; };
		template<typename C> struct one_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
		template<typename C> struct all_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
	};

	extern const sync_points_analysis SyncPoints;

	namespace {

		template<typename Context, typename SyncPointSetType>
		ConstraintPtr createReachingSyncPointsClosureConstraint(CBA& cba, const SyncPointSetType& set);

		template<typename Context, typename SyncPointSetType>
		ConstraintPtr createAddInnerThreadConstraint(CBA& cba, const SyncPointSetType& set);

	}

	template<typename Context>
	class SyncPointsConstraintGenerator : public ConstraintGenerator {

	public:

		SyncPointsConstraintGenerator(CBA& cba) {}

		virtual void addConstraints(CBA& cba, const sc::ValueID& value, Constraints& constraints) {

			// compute teh type of the
			typedef sc::TypedValueID<typename sync_points_analysis::lattice<analysis_config<Context>>::type> value_id_type;

			// This is a recursive definition:
			value_id_type SPs(value);

			//  - the exit-point of the application is a sync point
			ProgramPoint<Context> end(ProgramPoint<Context>::Out, getAnalysisRoot(cba.getRoot()), Context());
			constraints.add(elem(end, SPs));

			//  - all reaching sync points of sync points are sync points
			constraints.add(createReachingSyncPointsClosureConstraint<Context>(cba, SPs));

			//  - when spawning a thread, the end of the thread is a sync point
			constraints.add(createAddInnerThreadConstraint<Context>(cba, SPs));
		}

		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const {
			out << "SyncPoints = " << value;
		}

	};

	namespace {

		template<typename Context, typename SyncPointSetType>
		class ReachingSyncPointsClosureConstraint : public utils::constraint::Constraint {

			CBA& cba;

			SyncPointSetType SPs;

			mutable std::vector<SyncPointSetType> sync_points;

			mutable std::vector<ValueID> inputs;

		public:

			ReachingSyncPointsClosureConstraint(CBA& cba, const SyncPointSetType& set) :
				Constraint(toVector<ValueID>(set), toVector<ValueID>(set), true, true), cba(cba), SPs(set) {
				inputs.push_back(SPs);
			}

			virtual UpdateResult update(Assignment& ass) const {

				// get the set to be modified
				set<ProgramPoint<Context>>& set = ass[SPs];

				// just collect all values from the input sets
				bool newElement = false;
				for(const auto& cur : sync_points) {
					for(const auto& point : ass[cur]) {
						newElement = set.insert(point).second || newElement;
					}
				}

				// done
				return (newElement) ? Incremented : Unchanged;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get current set of sync points
				const set<ProgramPoint<Context>>& all_sync_points = ass[SPs];

				// collect all preceding sync-point sets
				bool changed = false;
				for(const auto& cur : all_sync_points) {

					// get set of preceding sync points
					auto stmt = cur.getStatement();

					// extract the current set
					SyncPointSetType cur_set;
					auto call = stmt.template isa<CallExprAddress>();
					if (call && isSynchronizingFunction(call->getFunctionExpr())) {
						// for call expressions it is the set of sync points reaching the tmp-state
						// (after arguments, before processing the function itself)
						cur_set = cba.getSet(RSPtmp, stmt, cur.getContext());
					} else {
						// for rest it is the out state
						cur_set = cba.getSet(RSPout, stmt, cur.getContext());
					}

					if (!contains(sync_points,cur_set)) {
						sync_points.push_back(cur_set);
						inputs.push_back(cur_set);
						changed = true;
					}
				}

				return changed;
			}

			virtual const std::vector<ValueID>& getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

			virtual bool check(const Assignment& ass) const {

				// check whether dependencies are fixed
				if (updateDynamicDependencies(ass)) return false;

				// get the resulting set
				const set<ProgramPoint<Context>>& set = ass[SPs];

				// check whether all expected values are present
				for(const auto& cur : sync_points) {
					for(const auto& point : ass[cur]) {
						// if not present => fail
						if (set.find(point) == set.end()) return false;
					}
				}

				// everything is fine
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {

				// print merged in thread dependencies
				for(const auto& cur : sync_points) {
					out << cur << " -> " << SPs << "[label=\"subset\"]\n";
				}

				// and the default dependencies
				return out << SPs << " -> " << SPs << "[label=\"reaching sync closure\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "ReachingSyncClosure";
			}
		};


		template<typename Context, typename SyncPointSetType>
		ConstraintPtr createReachingSyncPointsClosureConstraint(CBA& cba, const SyncPointSetType& set) {
			return std::make_shared<ReachingSyncPointsClosureConstraint<Context,SyncPointSetType>>(cba, set);
		}


		template<typename Context, typename SyncPointSetType>
		class AddInnerThreadConstraint : public utils::constraint::Constraint {

			typedef TypedValueID<typename lattice<thread_body_analysis, analysis_config<Context>>::type> thread_body_set_id_type;

			CBA& cba;

			SyncPointSetType SPs;

			// the sets containing sets of spanned thread bodies
			mutable std::vector<thread_body_set_id_type> thread_bodies;

			mutable std::vector<ValueID> inputs;

			const core::lang::BasicGenerator& basic;

		public:

			AddInnerThreadConstraint(CBA& cba, const SyncPointSetType& set) :
				Constraint(toVector<ValueID>(set), toVector<ValueID>(set), true, true), cba(cba), SPs(set), basic(cba.getRoot()->getNodeManager().getLangBasic()) {
				inputs.push_back(SPs);
			}

			virtual UpdateResult update(Assignment& ass) const {

				// get the set to be modified
				set<ProgramPoint<Context>>& set = ass[SPs];

				// for all thread bodies in the program => add the end-point of the thread to the list of sync points
				bool newElement = false;
				for(const auto& cur : thread_bodies) {
					for(const auto& body : ass[cur]) {

						// build a program point marking the end of the thread body
						auto end = ProgramPoint<Context>(ProgramPoint<Context>::Out, body.getBody(), body.getContext());

						// add end of thread to program point
						newElement = set.insert(end).second || newElement;
					}
				}

				// done
				return (newElement) ? Incremented : Unchanged;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get current set of sync points
				const set<ProgramPoint<Context>>& all_sync_points = ass[SPs];

				// for all sync points ...
				bool changed = false;
				for(const auto& cur : all_sync_points) {
					// ... that are spawn points ...
					auto call = cur.getStatement().template isa<CallExprAddress>();
					if (call && basic.isParallelOp(call->getFunctionExpr())) {

						// ... add the list of potential spanned bodies to the inputs
						auto cur_bodies = cba.getSet(ThreadBodies, call, cur.getContext());

						// add it if not already present
						if (!contains(thread_bodies, cur_bodies)) {
							thread_bodies.push_back(cur_bodies);
							inputs.push_back(cur_bodies);
							changed = true;
						}
					}
				}

				// cone
				return changed;
			}

			virtual const std::vector<ValueID>& getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

			virtual bool check(const Assignment& ass) const {

				// check whether dependencies are fixed
				if (updateDynamicDependencies(ass)) return false;

				// get the set to be modified
				const set<ProgramPoint<Context>>& set = ass[SPs];

				// for all thread bodies in the program => add the end-point of the thread to the list of sync points
				for(const auto& cur : thread_bodies) {
					for(const auto& body : ass[cur]) {

						// build a program point marking the end of the thread body
						auto end = ProgramPoint<Context>(ProgramPoint<Context>::Out, body.getBody(), body.getContext());

						// check whether this sync point is present
						if (set.find(end) == set.end()) return false;
					}
				}

				// everything is fine
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {

				// print merged in thread dependencies
				for(const auto& cur : thread_bodies) {
					out << cur << " -> " << SPs << "[label=\"reaches thread\"]\n";
				}

				out << SPs << " -> " << SPs << "[label=\"for all spawn points\"]\n";
				return out;
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "AddInnerThreadConstraint";
			}
		};

		template<typename Context, typename SyncPointSetType>
		ConstraintPtr createAddInnerThreadConstraint(CBA& cba, const SyncPointSetType& set) {
			return std::make_shared<AddInnerThreadConstraint<Context,SyncPointSetType>>(cba, set);
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
