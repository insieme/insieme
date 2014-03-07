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

#include "insieme/analysis/cba/analysis/jobs.h"
#include "insieme/analysis/cba/analysis/callables.h"
#include "insieme/analysis/cba/analysis/thread_groups.h"
#include "insieme/analysis/cba/analysis/reaching_spawn_points.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ----------------- jobs ---------------

	template<typename Context> class ThreadBodyConstraintGenerator;

	struct thread_body_analysis : public dependent_set_analysis<ThreadBody, ThreadBodyConstraintGenerator> {};

	extern const thread_body_analysis ThreadBodies;

	namespace {

		template<typename Context, typename JobSetType, typename ThreadBodySetType>
		ConstraintPtr createThreadBodySpawnConstraint(CBA& cba, const JobSetType& jobs, const ThreadBodySetType& res, const CallExprAddress& spawn, const Context& spawnCtxt);

		template<typename Context, typename ThreadGroupSetType, typename ThreadBodySetType>
		ConstraintPtr createThreadBodyMergeConstraint(CBA& cba, const ThreadGroupSetType& groups, const ThreadBodySetType& res);

	}


	template<typename Context>
	class ThreadBodyConstraintGenerator : public DataValueConstraintGenerator<Context> {

		typedef DataValueConstraintGenerator<Context> super;

		CBA& cba;

		const core::lang::BasicGenerator& basic;

	public:

		ThreadBodyConstraintGenerator(CBA& cba)
			: super(cba), cba(cba), basic(cba.getRoot()->getNodeManager().getLangBasic()) { };

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

			// check whether this is a call to parallel
			auto fun = call->getFunctionExpr();
			if (basic.isParallelOp(fun)) {

				// get job set
				auto jobs = cba.getSet(Jobs, call[0], ctxt);

				// get result set
				auto res = cba.getSet(ThreadBodies, call, ctxt);

				// add constraint
				constraints.add(createThreadBodySpawnConstraint(cba, jobs, res, call, ctxt));
			}

			// another case: if it is the merge part
			if (basic.isMerge(fun)) {

				// get list of potential thread groups merged here => get bodies at spawn points
				auto groups = cba.getSet(ThreadGroups, call[0], ctxt);

				// get the result set
				auto res = cba.getSet(ThreadBodies, call, ctxt);

				// add the constraint
				constraints.add(createThreadBodyMergeConstraint<Context>(cba, groups, res));
			}

			/**
			 * NOTE: the merge all can not be supported here since a merge is merging more than one thread.
			 * Hence the data structure of a set of Thread-bodies is not sufficient to represent this information.
			 */

			// nothing else is interesting (no default processing)
		}

	};


	namespace {

		template<
			typename Context,
			typename JobSetType,
			typename ThreadBodySetType
		>
		class ThreadBodySpawnConstraint : public utils::constraint::Constraint {

			typedef typename lattice<control_flow_analysis_data, analysis_config<Context>>::type callable_lattice_type;

			CBA& cba;

			JobSetType in;

			ThreadBodySetType out;

			CallExprAddress spawn;

			Context spawnCtxt;

			// the bodies to be collected
			mutable std::map<Job<Context>, TypedValueID<callable_lattice_type>> bodies;

			// the inputs this constraint is depending on
			mutable std::vector<ValueID> inputs;

		public:

			ThreadBodySpawnConstraint(CBA& cba, const JobSetType& job, const ThreadBodySetType& set, const CallExprAddress& spawn, const Context& spawnCtxt) :
				Constraint(toVector<ValueID>(job), toVector<ValueID>(set), true, true), cba(cba), in(job), out(set), spawn(spawn), spawnCtxt(spawnCtxt) {
				inputs.push_back(job);
			}

			virtual UpdateResult update(Assignment& ass) const {

				// get the set to be modified
				set<ThreadBody<Context>>& set = ass[out];

				// just collect all bodies from the bodies set
				bool newElement = false;
				for(const auto& cur : bodies) {
					const std::set<Callable<Context>>& callables = ass[cur.second];
					for(const auto& callable : callables) {
						for (const Context& rootCtxt : getThreadContexts(cur.first)) {
							// take the body of the callable the the root ctxt
							auto body = ThreadBody<Context>(callable.getBody(), rootCtxt);

							// see whether the body is already known => otherwise add it
							newElement = set.insert(body).second || newElement;
						}
					}
				}

				// done
				return (newElement) ? Incremented : Unchanged;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get current set of sync points
				const set<Job<Context>>& jobs = ass[in];

				// for each job
				bool changed = false;
				for(const auto& job : jobs) {

					// get body sets
					if (bodies.find(job) != bodies.end()) continue;

					// register body reference
					auto curBodySet = cba.getSet(C, job.getAddress()->getDefaultExpr(), job.getContext());
					bodies[job] = curBodySet;
					inputs.push_back(curBodySet);
					changed = true;
				}

				return changed;
			}

			virtual const std::vector<ValueID>& getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

			virtual bool check(const Assignment& ass) const {

				// check whether dependencies are fixed
				if (updateDynamicDependencies(ass)) return false;

				// get the set to be modified
				const set<ThreadBody<Context>>& set = ass[out];

				// just collect all bodies from the bodies set
				for(const auto& cur : bodies) {
					const std::set<Callable<Context>>& callables = ass[cur.second];
					for(const auto& callable : callables) {
						for (const Context& rootCtxt : getThreadContexts(cur.first)) {
							// take the body of the callable the the root ctxt
							auto body = ThreadBody<Context>(callable.getBody(), rootCtxt);

							// see whether the body is really there
							if (set.find(body) == set.end()) return false;
						}
					}
				}

				// done
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {

				// print merged in thread dependencies
				for(const auto& cur : bodies) {
					out << cur.second << " -> " << this->out << "[label=\"may spawn\"]\n";
				}

				// and the default dependencies
				return out << in << " -> " << this->out << "[label=\"defines jobs\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "ThreadBodies(" << in << ") in " << this->out;
			}

			vector<Context> getThreadContexts(const Job<Context>& job) const {
				typedef typename Context::thread_id thread_id;

				// init result
				vector<Context> res;

				// get label of spawn point
				auto spawnLabel = cba.getLabel(spawn);

				// build root context of spawned threads
				// TODO: support thread-groups larger than 1
				auto threadContext = spawnCtxt.threadContext;
				threadContext >>= thread_id(spawnLabel, spawnCtxt.callContext, 0);
				res.push_back(Context(typename Context::call_context(), threadContext));

				// if it is not only a sequential task ...
				if (!job.isTask()) {
					for(unsigned i=1; i<Context::thread_ctxt_size; i++) {
						// ... add contexts for additional IDs in the group
						auto threadContext = spawnCtxt.threadContext;
						threadContext >>= thread_id(spawnLabel, spawnCtxt.callContext, i);
						res.push_back(Context(typename Context::call_context(), threadContext));
					}
				}

				// done
				return res;
			}
		};


		template<typename Context, typename JobSetType, typename ThreadBodySetType>
		ConstraintPtr createThreadBodySpawnConstraint(CBA& cba, const JobSetType& jobs, const ThreadBodySetType& res, const CallExprAddress& spawn, const Context& spawnCtxt) {
			return std::make_shared<ThreadBodySpawnConstraint<Context, JobSetType, ThreadBodySetType>>(cba, jobs, res, spawn, spawnCtxt);
		}



		template<
			typename Context,
			typename ThreadGroupSetType,
			typename ThreadBodySetType
		>
		class ThreadBodyMergeConstraint : public utils::constraint::Constraint {

			typedef TypedValueID<typename lattice<thread_body_analysis, analysis_config<Context>>::type> ThreadBodyValueID;

			CBA& cba;

			ThreadGroupSetType groups;

			ThreadBodySetType res;

			// the bodies to be collected
			mutable std::vector<ThreadBodyValueID> bodies;

			// the inputs this constraint is depending on
			mutable std::vector<ValueID> inputs;

		public:

			ThreadBodyMergeConstraint(CBA& cba, const ThreadGroupSetType& groups, const ThreadBodySetType& res) :
				Constraint(toVector<ValueID>(groups), toVector<ValueID>(res), true, true), cba(cba), groups(groups), res(res) {
				inputs.push_back(groups);
			}

			virtual UpdateResult update(Assignment& ass) const {

				// get set to be extended
				set<ThreadBody<Context>>& set = ass[res];

				// just collect all bodies from the bodies set
				bool newElement = false;
				for(const auto& spawn : bodies) {
					for(const auto& body : ass[spawn]) {
						// see whether the body is already known => otherwise add it
						newElement = set.insert(body).second || newElement;
					}
				}

				// done
				return (newElement) ? Incremented : Unchanged;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get current set of merged thread groups
				const set<ThreadGroup<Context>>& gs = ass[groups];

				// for each job
				bool changed = false;
				for(const auto& group : gs) {

					// add dependency to spawn point of current group
					auto curBodySet = cba.getSet(ThreadBodies, group.getAddress(), group.getContext());
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
				const set<ThreadBody<Context>>& set = ass[res];

				// just collect all bodies from the bodies set
				for(const auto& spawn : bodies) {
					for(const auto& body : ass[spawn]) {
						if (set.find(body) == set.end()) return false;
					}
				}

				// all fine
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {

				// print merged in thread dependencies
				for(const auto& cur : bodies) {
					out << cur << " -> " << res << "[label=\"subset\"]\n";
				}

				// and the default dependencies
				return out << groups << " -> " << res << "[label=\"merged groups\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "ThreadBodies(" << groups << ") in " << res;
			}
		};


		template<typename Context, typename ThreadGroupSetType, typename ThreadBodySetType>
		ConstraintPtr createThreadBodyMergeConstraint(CBA& cba, const ThreadGroupSetType& groups, const ThreadBodySetType& res) {
			return std::make_shared<ThreadBodyMergeConstraint<Context, ThreadGroupSetType, ThreadBodySetType>>(cba, groups, res);
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
