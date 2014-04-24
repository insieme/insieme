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
#include "insieme/analysis/cba/framework/entities/thread_region.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/analysis/cba/analysis/reaching_sync_points.h"
#include "insieme/analysis/cba/analysis/sync_points.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * This class implements an analysis collecting all thread regions within a given program.
	 */

	// ----------------- reaching sync points ---------------

	template<typename Context> class ThreadRegionsConstraintGenerator;

	struct thread_regions_analysis {
		template<typename C> struct lattice   { typedef utils::constraint::SetLattice<ThreadRegion<typename C::context_type>> type; };
		template<typename C> struct generator { typedef ThreadRegionsConstraintGenerator<typename C::context_type> type; };
		template<typename C> struct params    { typedef std::tuple<AnalysisType> type; };
		template<typename C> struct one_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
		template<typename C> struct all_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
	};

	extern const thread_regions_analysis ThreadRegions;

	namespace {

		template<typename Context, typename ThreadRegionSetType>
		ConstraintPtr createThreadRegionGenerationConstraint(CBA& cba, const ThreadRegionSetType& set);

	}

	template<typename Context>
	class ThreadRegionsConstraintGenerator : public ConstraintGenerator {

	public:

		ThreadRegionsConstraintGenerator(CBA& cba) {}

		virtual void addConstraints(CBA& cba, const sc::ValueID& value, Constraints& constraints) {

			// This is a recursive definition:
			auto TRs = cba.getSet<Context>(ThreadRegions);
			assert_eq(TRs, value) << "Invalid target set!";

			// form all pairs between sync points and reaching sync points
			constraints.add(createThreadRegionGenerationConstraint<Context>(cba, TRs));

			// that's all ...
		}

		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const {
			out << "ThreadRegions = " << value;
		}

	};

	namespace {

		template<typename Context, typename ThreadRegionSetType>
		class ThreadRegionGeneratorConstraint : public utils::constraint::Constraint {

			typedef TypedValueID<typename lattice<reaching_sync_points_in_analysis, analysis_config<Context>>::type> reaching_sync_points_set_id_type;

			CBA& cba;

			ThreadRegionSetType TRs;

			// a map linking sync points to the set of reaching sync points
			mutable std::map<ProgramPoint<Context>, reaching_sync_points_set_id_type> sync_point_pairs;

			mutable std::vector<ValueID> inputs;

		public:

			ThreadRegionGeneratorConstraint(CBA& cba, const ThreadRegionSetType& set) :
				Constraint(toVector<ValueID>(cba.getSet<Context>(SyncPoints)), toVector<ValueID>(set), true, true), cba(cba), TRs(set) {
				inputs.push_back(cba.getSet<Context>(SyncPoints));
			}

			virtual UpdateResult update(Assignment& ass) const {

				// get the set to be modified
				set<ThreadRegion<Context>>& set = ass[TRs];

				// for all the sync points ...
				bool newElement = false;
				for(const auto& cur : sync_point_pairs) {

					// get the end of the region
					const auto& end = cur.first;

					// for all starts of the regions
					for(const auto& begin : ass[cur.second]) {

						// skip empty regions
						if (begin == end) continue;

						// build the thread region
						ThreadRegion<Context> region(begin, end);

						// add end of thread to program point
						newElement = set.insert(region).second || newElement;
					}
				}

				// done
				return (newElement) ? Incremented : Unchanged;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get current set of sync points
				const set<ProgramPoint<Context>>& all_sync_points = ass[cba.getSet(SyncPoints)];

				// for all sync points ...
				bool changed = false;
				for(const auto& cur : all_sync_points) {

					// we can skip entry points of threads
					if (cur.getState() == ProgramPoint<Context>::In) continue;

					// check whether point is known
					auto pos = sync_point_pairs.find(cur);
					if (pos != sync_point_pairs.end()) continue; // already known

					// this is a new one
					auto stmt = cur.getStatement();

					// get the preceeding sync points
					reaching_sync_points_set_id_type cur_set;
					auto call = stmt.template isa<CallExprInstance>();
					if (call && isSynchronizingFunction(call->getFunctionExpr())) {
						// for call expressions it is the set of sync points reaching the tmp-state
						// (after arguments, before processing the function itself)
						cur_set = cba.getSet(RSPtmp, stmt, cur.getContext());
					} else {
						// for rest it is the out state
						cur_set = cba.getSet(RSPout, stmt, cur.getContext());
					}

					sync_point_pairs[cur] = cur_set;
					inputs.push_back(cur_set);
					changed = true;
				}

				// done
				return changed;
			}

			virtual const std::vector<ValueID>& getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

			virtual bool check(const Assignment& ass) const {
				// get the set to be modified
				const set<ThreadRegion<Context>>& set = ass[TRs];

				// for all the sync points ...
				for(const auto& cur : sync_point_pairs) {

					// get the end of the region
					const auto& end = cur.first;

					// for all starts of the regions
					for(const auto& begin : ass[cur.second]) {

						// skip empty regions
						if (begin == end) continue;

						// build the thread region
						ThreadRegion<Context> region(begin, end);

						// if the region is not present => fail
						if (set.find(region) == set.end()) return false;
					}
				}

				// done
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {

				// print merged in thread dependencies
				for(const auto& cur : sync_point_pairs) {
					out << cur.second << " -> " << TRs << "[label=\"depends on\"]\n";
				}

				out << cba.getSet<Context>(SyncPoints) << " -> " << TRs << "[label=\"for all sync points\"]\n";
				return out;
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "ThreadRegionGeneration";
			}
		};

		template<typename Context, typename ThreadRegionSetType>
		ConstraintPtr createThreadRegionGenerationConstraint(CBA& cba, const ThreadRegionSetType& set) {
			return std::make_shared<ThreadRegionGeneratorConstraint<Context,ThreadRegionSetType>>(cba, set);
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
