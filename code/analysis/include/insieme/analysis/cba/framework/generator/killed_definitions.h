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

	// forward definitions
	struct reaching_defs_tmp_analysis;
	extern const reaching_defs_tmp_analysis RDtmp;

	// ----------------- killed definitions ---------------

	template<typename Context> class KilledDefsInConstraintGenerator;
	template<typename Context> class KilledDefsTmpConstraintGenerator;
	template<typename Context> class KilledDefsOutConstraintGenerator;

	template<template<typename C> class G>
	struct kill_set_analysis : public location_based_set_analysis<Definition, G> {
		template<typename C> struct lattice   { typedef utils::constraint::SetIntersectLattice<Definition<typename C::context_type>> type; };
		template<typename C> struct one_meet_assign_op_type { typedef set_intersect_meet_assign_op<Definition<typename C::context_type>> type; };
		template<typename C> struct all_meet_assign_op_type { typedef set_union_meet_assign_op<Definition<typename C::context_type>> type; };
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

			virtual std::vector<ValueID> getUsedInputs(const Assignment& ass) const {

				// create result set
				std::vector<ValueID> res;

				// ref and in are always required
				res.push_back(ref);
				res.push_back(in);

				// reaching_in is required if reference is matched
				if (isKill(ass)) {
					res.push_back(reaching_in);
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

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
