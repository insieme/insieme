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
#include "insieme/analysis/cba/framework/entities/thread_body.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/analysis/cba/analysis/thread_bodies.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * An analysis that determines the bodies of the threads merged by a merge-all call.
	 *
	 * The result is of type
	 * 		set<set<ThreadBody>>
	 * where the outer set is the list of merged threads (all of them) and the inner set
	 * models the alternatives the spawned threads may follow.
	 */

	template<typename Context> class MergeAllThreadBodyConstraintGenerator;

	struct merge_all_thread_body_analysis {
		template<typename C> struct lattice   { typedef utils::constraint::SetLattice<std::set<ThreadBody<typename C::context_type>>> type; };
		template<typename C> struct generator { typedef MergeAllThreadBodyConstraintGenerator<typename C::context_type> type; };
		template<typename C> struct params    { typedef std::tuple<AnalysisType, Label, typename C::context_type> type; };
		template<typename C> struct one_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
		template<typename C> struct all_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
	};

	extern const merge_all_thread_body_analysis MergeAllThreadBodies;

	namespace {

		template<typename Context, typename SpawnPointsValueType, typename ThreadBodySetType>
		ConstraintPtr createThreadBodyMergeAllConstraint(CBA& cba, const SpawnPointsValueType& spawns, const ThreadBodySetType& res);

	}


	template<typename Context>
	class MergeAllThreadBodyConstraintGenerator : public DataValueConstraintGenerator<Context> {

		typedef DataValueConstraintGenerator<Context> super;

		CBA& cba;

		const core::lang::BasicGenerator& basic;

	public:

		MergeAllThreadBodyConstraintGenerator(CBA& cba)
			: super(cba), cba(cba), basic(cba.getRoot()->getNodeManager().getLangBasic()) { };

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

			// only interested in merge-all calls
			if (basic.isMergeAll(call->getFunctionExpr())) {

				// get the list of reaching spawn points
				auto spawns = cba.getSet(ReachingSpawnPointsTmp, call, ctxt);

				// get the result set
				auto res = cba.getSet(MergeAllThreadBodies, call, ctxt);

				// add the constraint
				constraints.add(createThreadBodyMergeAllConstraint<Context>(cba, spawns, res));
			}

			// nothing else is interesting (no default processing)
		}

	};


	namespace {

		/**
		 * --- Merge All Constraint ----
		 */

		template<
			typename Context,
			typename SpawnPointsValueType,
			typename ThreadBodySetType
		>
		class ThreadBodyMergeAllConstraint : public utils::constraint::Constraint {

			typedef TypedValueID<typename lattice<thread_body_analysis, analysis_config<Context>>::type> ThreadBodyValueID;

			CBA& cba;

			ThreadBodySetType res;

			// the value listing reaching spawns
			SpawnPointsValueType spawns;

			// the bodies to be collected
			mutable std::vector<ThreadBodyValueID> bodies;

			// the inputs this constraint is depending on
			mutable std::vector<ValueID> inputs;

		public:

			ThreadBodyMergeAllConstraint(CBA& cba, const SpawnPointsValueType& spawns, const ThreadBodySetType& res) :
				Constraint(toVector<ValueID>(spawns), toVector<ValueID>(res), true, true), cba(cba), res(res), spawns(spawns) {
				inputs.push_back(spawns);
			}

			virtual UpdateResult update(Assignment& ass) const {

				// get set to be extended
				set<set<ThreadBody<Context>>>& is = ass[res];

				// just collect all bodies from the bodies set
				set<set<ThreadBody<Context>>> should;
				for(const auto& bodySet : bodies) {
					// add the body set of the current spawn
					should.insert(ass[bodySet]);
				}

				// check for changes
				if (is == should) return Unchanged;

				// if it is a subset => fine
				bool isSubset = utils::set::isSubset(is, should);

				// update set
				is = should;

				// return level of modification
				return (isSubset) ? Incremented : Altered;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get current set of merged thread groups
				const set<ProgramPoint<Context>>& sps = ass[spawns];

				// for each job
				bool changed = false;
				for(const auto& spawn : sps) {

					// add dependency to spawn point of current group
					auto curBodySet = cba.getSet(ThreadBodies, spawn.getStatement(), spawn.getContext());
					if (!contains(bodies, curBodySet)) {
						bodies.push_back(curBodySet);
						inputs.push_back(curBodySet);
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

				// get set to be checked
				const set<set<ThreadBody<Context>>>& set = ass[res];

				// just collect all bodies from the bodies set
				for(const auto& bodySet : bodies) {
					if (set.find(ass[bodySet]) == set.end()) return false;
				}

				// all fine
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {

				// print merged in thread dependencies
				for(const auto& cur : bodies) {
					out << cur << " -> " << res << "[label=\"in\"]\n";
				}

				// and the default dependencies
				return out << spawns << " -> " << res << "[label=\"merged spawns\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "ThreadBodies(" << spawns << ") in " << res;
			}
		};


		template<typename Context, typename SpawnPointsValueType, typename ThreadBodySetType>
		ConstraintPtr createThreadBodyMergeAllConstraint(CBA& cba, const SpawnPointsValueType& spawns, const ThreadBodySetType& res) {
			return std::make_shared<ThreadBodyMergeAllConstraint<Context, SpawnPointsValueType, ThreadBodySetType>>(cba, spawns, res);
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
