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

		template<
			typename Context,
			typename JobSetType,
			typename ThreadBodySetType
		>
		class ThreadBodyConstraint : public utils::constraint::Constraint {

			typedef typename lattice<control_flow_analysis_data, analysis_config<Context>>::type callable_lattice_type;

			CBA& cba;

			JobSetType in;

			ThreadBodySetType out;

			Context rootCtxt;

			// the bodies to be collected
			mutable std::vector<TypedValueID<callable_lattice_type>> bodies;

			// the inputs this constraint is depending on
			mutable std::vector<ValueID> inputs;

		public:

			ThreadBodyConstraint(CBA& cba, const JobSetType& job, const ThreadBodySetType& set, const Context& rootCtxt) :
				Constraint(toVector<ValueID>(job), toVector<ValueID>(out), true, true), cba(cba), in(job), out(set), rootCtxt(rootCtxt) {
				inputs.push_back(job);
			}

			virtual UpdateResult update(Assignment& ass) const {

				// get the set to be modified
				set<ThreadBody<Context>>& set = ass[out];

				// just collect all bodies from the bodies set
				bool newElement = false;
				for(const auto& cur : bodies) {
					const std::set<Callable<Context>>& callables = ass[cur];
					for(const auto& callable : callables) {

						// take the body of the callable the the root ctxt
						auto body = ThreadBody<Context>(callable.getBody(), rootCtxt);

						// see whether the body is already known => otherwise add it
						newElement = set.insert(body).second || newElement;
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
					auto curBodySet = cba.getSet(C, job.getAddress()->getDefaultExpr(), job.getContext());
					if (!contains(bodies, curBodySet)) {
						bodies.push_back(curBodySet);
						inputs.push_back(curBodySet);
						changed = true;
					}
				}

				return changed;
			}

			virtual std::vector<ValueID> getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

			virtual bool check(const Assignment& ass) const {
				assert_not_implemented();
				return false;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {

				// print merged in thread dependencies
				for(const auto& cur : bodies) {
					out << cur << " -> " << this->out << "[label=\"may spawn\"]\n";
				}

				// and the default dependencies
				return out << in << " -> " << this->out << "[label=\"defines jobs\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "ThreadBodies(" << in << ") in out";
			}
		};


		template<typename Context, typename JobSetType, typename ThreadBodySetType>
		ConstraintPtr createThreadBodyConstraint(CBA& cba, const JobSetType& jobs, const ThreadBodySetType& res, const Context& rootCtxt) {
			return std::make_shared<ThreadBodyConstraint<Context, JobSetType, ThreadBodySetType>>(cba, jobs, res, rootCtxt);
		}

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
			typedef typename Context::thread_id thread_id;

			// check whether this is a call to parallel
			auto fun = call->getFunctionExpr();
			if (basic.isParallelOp(fun)) {

				// get job set
				auto jobs = cba.getSet(Jobs, call[0], ctxt);

				// get result set
				auto res = cba.getSet(ThreadBodies, call, ctxt);

				// build root context of spawned threads
				// TODO: support thread-groups larger than 1
				auto threadContext = ctxt.threadContext;
				threadContext >>= thread_id(cba.getLabel(call), ctxt.callContext);
				auto rootCtxt = Context(typename Context::call_context(), threadContext);

				// add constraint
				constraints.add(createThreadBodyConstraint(cba, jobs, res, rootCtxt));
			}

			// nothing else is interesting (no default processing)
		}

//		void visitJobExpr(const JobExprAddress& job, const Context& ctxt, Constraints& constraints) {
//
//			// this expression is creating a job
//			auto value = getJobFromConstructor(job, ctxt);
//			auto J_res = cba.getSet(Jobs, job, ctxt);
//
//			// add constraint fixing this job
//			constraints.add(elem(value, J_res));
//
//		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
