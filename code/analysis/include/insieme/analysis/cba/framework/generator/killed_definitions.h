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
#include "insieme/analysis/cba/framework/analysis.h"

#include "insieme/analysis/cba/framework/entities/definition.h"

#include "insieme/analysis/cba/framework/generator/basic_program_point.h"
#include "insieme/analysis/cba/framework/generator/reaching_definitions.h"

#include "insieme/analysis/cba/analysis/jobs.h"
#include "insieme/analysis/cba/analysis/thread_groups.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ----------------- killed definitions ---------------

	template<typename Context> class KilledDefsInConstraintGenerator;
	template<typename Context> class KilledDefsTmpConstraintGenerator;
	template<typename Context> class KilledDefsOutConstraintGenerator;

	template<template<typename C> class G>
	struct kill_set_analysis : public location_based_set_analysis<Definition, G> {
		template<typename C> struct lattice   { typedef utils::constraint::SetIntersectLattice<Definition<typename C::context_type>> type; };
	};

	struct killed_defs_in_analysis  : public kill_set_analysis< KilledDefsInConstraintGenerator> {};
	struct killed_defs_tmp_analysis : public kill_set_analysis<KilledDefsTmpConstraintGenerator> {};
	struct killed_defs_out_analysis : public kill_set_analysis<KilledDefsOutConstraintGenerator> {};

	extern const killed_defs_in_analysis  KDin;
	extern const killed_defs_tmp_analysis KDtmp;
	extern const killed_defs_out_analysis KDout;

	namespace {

		template<typename RefValue, typename KDValue, typename RDValue, typename Context>
		ConstraintPtr killedDefsAssign(const Location<Context>& loc, const TypedValueID<RefValue>& updatedRef, const TypedValueID<RDValue>& reaching_in, const TypedValueID<KDValue>& in_state, const TypedValueID<KDValue>& out_state);

		template<typename TGValue, typename KDValue, typename Context>
		ConstraintPtr killedDefsMerge(CBA& cba, const Location<Context>& loc, const TypedValueID<TGValue>& threadGroup, const TypedValueID<KDValue>& in_state, const TypedValueID<KDValue>& out_state);

	}


	template<typename Context>
	class KilledDefsInConstraintGenerator
		: public BasicInConstraintGenerator<
		  	  killed_defs_in_analysis,
		  	  killed_defs_tmp_analysis,
		  	  killed_defs_out_analysis,
		  	  KilledDefsInConstraintGenerator<Context>,
		  	  Context,
		  	  Location<Context>
		  > {

		typedef BasicInConstraintGenerator<
			  	  killed_defs_in_analysis,
			  	  killed_defs_tmp_analysis,
			  	  killed_defs_out_analysis,
			  	  KilledDefsInConstraintGenerator<Context>,
			  	  Context,
			  	  Location<Context>
			 > super;

		CBA& cba;

	public:

		KilledDefsInConstraintGenerator(CBA& cba) : super(cba, KDin, KDtmp, KDout), cba(cba) {}

	};

	template<typename Context>
	struct KilledDefsTmpConstraintGenerator : public ConstraintGenerator {

		KilledDefsTmpConstraintGenerator(CBA&) {}

		virtual void addConstraints(CBA& cba, const sc::ValueID& value, Constraints& constraints) {
			// nothing to do here

			const auto& data = cba.getValueParameters<Label,Context,Location<Context>>(value);
			Label label = std::get<1>(data);
			const auto& call = cba.getStmt(label).as<CallExprAddress>();
			const auto& ctxt = std::get<2>(data);
			const auto& loc  = std::get<3>(data);

			// obtain
			auto KD_tmp = cba.getSet(KDtmp, call, ctxt, loc);
			assert_eq(value, KD_tmp) << "Queried a non KD_tmp id!";

			// create constraints
			for(const auto& cur : call) {
				auto KD_out = cba.getSet(KDout, cur, ctxt, loc);
				constraints.add(subset(KD_out, KD_tmp));
			}

			// and the function
			constraints.add(subset(cba.getSet(KDout, call->getFunctionExpr(), ctxt, loc), KD_tmp));

		}

		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const {

			const auto& data = cba.getValueParameters<Label,Context,Location<Context>>(value);
			const auto& stmt = cba.getStmt(std::get<1>(data));
			const auto& ctxt = std::get<2>(data);
			const auto& loc = std::get<3>(data);

			out << "KDtmp : " << stmt << " : " << ctxt << " : " << loc;
		}

	};

	template<typename Context>
	class KilledDefsOutConstraintGenerator
		: public BasicOutConstraintGenerator<
		  	  killed_defs_in_analysis,
		  	  killed_defs_tmp_analysis,
		  	  killed_defs_out_analysis,
		  	  KilledDefsOutConstraintGenerator<Context>,
		  	  Context,
		  	  Location<Context>
		  > {

		typedef BasicOutConstraintGenerator<
				  killed_defs_in_analysis,
				  killed_defs_tmp_analysis,
				  killed_defs_out_analysis,
				  KilledDefsOutConstraintGenerator<Context>,
				  Context,
				  Location<Context>
			 > super;


		CBA& cba;

	public:

		KilledDefsOutConstraintGenerator(CBA& cba) : super(cba, KDin, KDtmp, KDout), cba(cba) {}

		virtual void visit(const NodeAddress& addr, const Context& ctxt, const Location<Context>& loc, Constraints& constraints) {
			// we can stop at the creation point - no definitions will be killed before
			if (loc.getAddress() == addr) {

				auto KD_out = cba.getSet(KDout, loc.getAddress(), ctxt, loc);
				constraints.add(elem(iset<Definition<Context>>::empty(), KD_out));
				return;
			}

			// all others should be handled as usual
			super::visit(addr, ctxt, loc, constraints);
		}

		// TODO: the parallel evaluation of call-arguments may kill multiple definitions => here the union needs to be formed instead of the
		//		 the intersection.

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, const Location<Context>& loc, Constraints& constraints) {
			const auto& base = call->getNodeManager().getLangBasic();

			// a special case: assignments
			auto fun = call.as<CallExprPtr>()->getFunctionExpr();
			if (base.isRefAssign(fun)) {

				// check referenced location
				//		- if not referenced => out = in
				//		- if only reference => in definitions are not forwarded
				//		- if on of many => in definition + local definition
				// all handled by the killedDefs constraint

				// get involved sets
				auto KD_tmp = cba.getSet(KDtmp, call, ctxt, loc);		// the definitions reaching the assignment (after processing all arguments)
				auto KD_out = cba.getSet(KDout, call, ctxt, loc);		// the definitions
				auto R_trg = cba.getSet(R, call[0], ctxt);				// set of references locations

				auto RD_tmp = cba.getSet(RDtmp, call, ctxt, loc);		// the definitions reaching the call

				// add constraint
				constraints.add(killedDefsAssign(loc, R_trg, RD_tmp, KD_tmp, KD_out));

				// done
				return;
			}

			// another special case: parallel merge
			if (base.isMerge(fun)) {

				// In this case we have to:
				//		- compute the set of merged thread groups
				//		- if there is only one (100% save to assume it is this group) we can
				//		  merge the killed definitions at the end of the thread group with the killed definitions of the in-set
				//		- otherwise we can not be sure => no operation

				// get involved sets
				auto KD_tmp = cba.getSet(KDtmp, call, ctxt, loc);
				auto KD_out = cba.getSet(KDout, call, ctxt, loc);

				auto tg = cba.getSet(ThreadGroups, call[0], ctxt);

				// add constraint
				constraints.add(killedDefsMerge(cba, loc, tg, KD_tmp, KD_out));

				// done
				return;
			}



			// use default treatment
			super::visitCallExpr(call, ctxt, loc, constraints);

			// done
			return;
		}

	};

	namespace {

		// -- assign constraint ---------

		template<
			typename Context,
			typename KilledDefValue,
			typename ReachingDefValue,
			typename RefValue
		>
		class KilledDefsAssignConstraint : public Constraint {

			typedef typename KilledDefValue::less_op_type less_op;

			const Location<Context> loc;
			const TypedValueID<KilledDefValue> in;
			const TypedValueID<KilledDefValue> out;
			const TypedValueID<ReachingDefValue> reaching_in;
			const TypedValueID<RefValue> ref;

		public:

			KilledDefsAssignConstraint(
					const Location<Context>& loc,
					const TypedValueID<KilledDefValue>& in, const TypedValueID<KilledDefValue>& out,
					const TypedValueID<ReachingDefValue>& reaching_in, const TypedValueID<RefValue>& ref)
				: Constraint(toVector<ValueID>(in, ref, reaching_in), toVector<ValueID>(out), true),
				  loc(loc), in(in), out(out), reaching_in(reaching_in), ref(ref) {}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				const static less_op less;

				// get reference to current value
				iset<Definition<Context>>& value = ass[out];

				// compute new value
				iset<Definition<Context>> updated = getUpdatedValue(ass);

				// check whether something has changed
				if (less(value,updated) && less(updated,value)) return Constraint::Unchanged;

				// check whether new value is proper subset
				auto res = (less(value, updated) ? Constraint::Incremented : Constraint::Altered);

				// update value
				value = updated;

				// return change-summary
				return res;
			}

			virtual bool check(const Assignment& ass) const {
				const static less_op less;
				return less(getUpdatedValue(ass), ass[out]);
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				return
					out << in << " -> " << this->out << "[label=\"" << *this << "\"]\n"
						<< reaching_in << " -> " << this->out << "[label=\"" << *this << "\"]\n";
			}

			virtual std::ostream& writeDotEdge(std::ostream& out, const Assignment& ass) const {
				return
					out << in << " -> " << this->out << "[label=\"" << in << " sub " << this->out << "\"]\n"
						<< reaching_in << " -> " << this->out << "[label=\"if killed (" << ref << " == {" << loc << "})\"" << ((isKill(ass))?"":" style=dotted") << "]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "if " << ref << " references " << loc << " => combine_kills(" << reaching_in << "," << in << ") in " << this->out;
			}

			virtual std::set<ValueID> getUsedInputs(const Assignment& ass) const {

				// create result set
				std::set<ValueID> res;

				// ref and in are always required
				res.insert(ref);
				res.insert(in);

				// reaching_in is required if reference is matched
				if (isKill(ass)) {
					res.insert(reaching_in);
				}

				return res;
			}

		private:

			bool isKill(const Assignment& ass) const {

				// check reference
				const set<Reference<Context>>& refs = ass[ref];

				// check whether this definition is addressing the full memory location
				return refs.size() == 1u && *refs.begin() == Reference<Context>(loc);
			}

			iset<Definition<Context>> getUpdatedValue(const Assignment& ass) const {

				iset<Definition<Context>> res = ass[in];		// all in-values are always included
				res.universal = false;

				// if it is only referencing the observed location => it is a new definition
				if (isKill(ass)) {
					const set<Definition<Context>>& reaching = ass[reaching_in];
					res.insert(reaching.begin(), reaching.end());
				}

				// done
				return res;
			}

		};


		template<typename RefValue, typename KDValue, typename RDValue, typename Context>
		ConstraintPtr killedDefsAssign(const Location<Context>& loc, const TypedValueID<RefValue>& updatedRef, const TypedValueID<RDValue>& reaching_in, const TypedValueID<KDValue>& in_state, const TypedValueID<KDValue>& out_state) {
			return std::make_shared<KilledDefsAssignConstraint<Context,KDValue,RDValue,RefValue>>(loc, in_state, out_state, reaching_in, updatedRef);
		}


		// -- merge constraint ---------

		template<
			typename Context,
			typename ThreadGroupValue,
			typename KilledDefValue
		>
		class KilledDefsMergeConstraint : public Constraint {

			typedef typename KilledDefValue::less_op_type less_op;

			CBA& cba;
			const Location<Context> loc;
			const TypedValueID<ThreadGroupValue> thread_group;
			const TypedValueID<KilledDefValue> in_state;
			const TypedValueID<KilledDefValue> out_state;

			// the killed thread states to be merged in (only if thread_group points to a single thread)
			mutable vector<TypedValueID<KilledDefValue>> thread_out_states;
			mutable vector<ValueID> dependencies;

		public:

			KilledDefsMergeConstraint(
					CBA& cba,
					const Location<Context>& loc,
					const TypedValueID<ThreadGroupValue>& thread_group,
					const TypedValueID<KilledDefValue>& in_state,
					const TypedValueID<KilledDefValue>& out_state)
				: Constraint(toVector<ValueID>(thread_group, in_state), toVector<ValueID>(out_state), true, true),
				  cba(cba), loc(loc), thread_group(thread_group), in_state(in_state), out_state(out_state) {}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				const static less_op less;

				// update dependencies
				bool changed = updateDynamicDependencies(ass);
				if (changed) return Constraint::DependencyChanged;

				// get reference to current value
				iset<Definition<Context>>& value = ass[out_state];

				// compute new value
				iset<Definition<Context>> updated = getUpdatedValue(ass);

				// check whether something has changed
				if (less(value,updated) && less(updated,value)) return Constraint::Unchanged;

				// check whether new value is proper subset
				auto res = (less(value, updated) ? Constraint::Incremented : Constraint::Altered);

				// update value
				value = updated;

				// return change-summary
				return res;
			}

			virtual bool check(const Assignment& ass) const {
				const static less_op less;
				return less(getUpdatedValue(ass), ass[out_state]);
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {

				// print merged in thread dependencies
				for(const auto& cur : thread_out_states) {
					out << cur << " -> " << out_state << "[label=\"" << *this << "\"]\n";
				}

				// and the default dependencies
				return
					out << thread_group << " -> " << this->out_state << "[label=\"" << *this << "\"]\n"
						<< in_state << " -> " << this->out_state << "[label=\"" << *this << "\"]\n";
			}

			virtual std::ostream& writeDotEdge(std::ostream& out, const Assignment& ass) const {

				// print dynamic dependencies
				updateDynamicDependencies(ass);
				for(const auto& cur : dependencies) {
					out << cur << " -> " << out_state << "[label=\"depends\"]\n";
				}

				// and the default dependencies
				return
					out << thread_group << " -> " << this->out_state << "[label=\"merges\"" << ((thread_out_states.empty())?" style=dotted":"") << "]\n"
						<< in_state << " -> " << this->out_state << "[label=\"merges\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "if " << thread_group << " is unique merging killed set of group and " << in_state << " into " << out_state;
			}

// TODO: switch to vectors
			virtual std::set<ValueID> getUsedInputs(const Assignment& ass) const {

				// create result set
				std::set<ValueID> res;

				// thread groups and in-state values are always required
				res.insert(thread_group);
				res.insert(in_state);

				// update thread out state list and thereby all dynamic dependencies
				updateDynamicDependencies(ass);
				for(auto cur : dependencies) {
					res.insert(cur);
				}

				// done
				return res;
			}

		private:

			bool updateDynamicDependencies(const Assignment& ass) const {

				// TODO: this is a prototype implementation
				//	  Required:
				//			- cleanup
				//			- move this to base class


				// clear lists
				thread_out_states.clear();
				vector<ValueID> newDependencies;

				// get set of merged threads
				const set<ThreadGroup<Context>>& groups = ass[thread_group];

				// if there is not exactly one thread => no states to merge (TODO: maybe not, we can still compute the intersection of all thread groups)
				if (groups.size() != 1u) {
					auto res = (newDependencies != dependencies);
					dependencies = newDependencies;
					return res;
				}

				// get merged group
				const ThreadGroup<Context>& group = *groups.begin();

				// get spawning point of threads
				auto spawnPoint = group.getAddress().template as<CallExprAddress>();

				// get potential list of jobs to be started at spawn point
				auto jobValue = cba.getSet(Jobs, spawnPoint[0], group.getContext());
				newDependencies.push_back(jobValue);
				const set<Job<Context>>& jobs = ass[jobValue];

				// if there is more than 1 candidate => we are done (TODO: maybe not, we can still compute the intersection of jobs)
				if (jobs.size() != 1u) {
					auto res = (newDependencies != dependencies);
					dependencies = newDependencies;
					return res;
				}

				// obtain body of job
				const Job<Context>& job = *jobs.begin();
				const JobExprAddress& jobExpr = job.getAddress();
				assert_true(jobExpr->getGuardedExprs().empty()) << "Only non-guarded jobs are supported so far.";

				// get set containing list of bodies
				auto C_body = cba.getSet(C, jobExpr->getDefaultExpr(), job.getContext());

				// get list of body functions
				newDependencies.push_back(C_body);
				const std::set<Callable<Context>>& bodies = ass[C_body];
				if (bodies.size() != 1u) {
					auto res = (newDependencies != dependencies);
					dependencies = newDependencies;
					return res;
				}

				// get the body of the job
				auto body = bodies.begin()->getBody();

				// get state at end of the body
				const Context& spawnContext = group.getContext();

				// TODO: consider possibility of multiple threads
				typedef typename Context::thread_id thread_id;

				auto threadContext = spawnContext.threadContext;
				threadContext >>= thread_id(cba.getLabel(spawnPoint), spawnContext.callContext);
				auto innerContext = Context(typename Context::call_context(), threadContext);		// call context in thread is default one again

				auto KD_out_set = cba.getSet(KDout, body, innerContext, loc);
				thread_out_states.push_back(KD_out_set);
				newDependencies.push_back(KD_out_set);

				auto res = (newDependencies != dependencies);
				dependencies = newDependencies;
				return res;
			}

			iset<Definition<Context>> getUpdatedValue(const Assignment& ass) const {

				iset<Definition<Context>> res = ass[in_state];		// all in-values are always included

				// merge in effects of thread-out-states if there are any
				for(const auto& cur : thread_out_states) {
					const iset<Definition<Context>>& out = ass[cur];
					res.insert(out.begin(), out.end());
				}

				// done
				return res;
			}

		};


		template<typename TGValue, typename KDValue, typename Context>
		ConstraintPtr killedDefsMerge(CBA& cba, const Location<Context>& loc, const TypedValueID<TGValue>& threadGroup, const TypedValueID<KDValue>& in_state, const TypedValueID<KDValue>& out_state) {
			return std::make_shared<KilledDefsMergeConstraint<Context,TGValue,KDValue>>(cba,loc,threadGroup,in_state,out_state);
		}
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
