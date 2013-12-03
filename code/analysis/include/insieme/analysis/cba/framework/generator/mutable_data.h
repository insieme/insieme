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

#include "insieme/core/lang/basic.h"

#include "insieme/analysis/cba/framework/analysis.h"
#include "insieme/analysis/cba/framework/generator/basic_program_point.h"

#include "insieme/analysis/cba/analysis/data_paths.h"

namespace insieme {
namespace analysis {
namespace cba {


	template<typename Context, typename BaseAnalysis>
	class ImperativeInStateConstraintGenerator;

	template<typename Context, typename BaseAnalysis>
	class ImperativeTmpStateConstraintGenerator;

	template<typename Context, typename BaseAnalysis>
	class ImperativeOutStateConstraintGenerator;


	template<
		typename BaseAnalysis,							// the analysis this analysis is extending
		template<typename C, typename A> class G		// the location state generator class (has additional parameter)
	>
	struct location_data_analysis {
		template<typename C> struct lattice   { typedef typename cba::lattice<BaseAnalysis,C>::type type; };
		template<typename C> struct generator { typedef G<typename C::context_type, BaseAnalysis> type; };
		template<typename C> struct params    { typedef std::tuple<AnalysisType, Label, typename C::context_type, Location<typename C::context_type>> type; };
	};

	template<typename A>
	struct location_data_in_analysis : public location_data_analysis<A, ImperativeInStateConstraintGenerator> {};

	template<typename A>
	struct location_data_tmp_analysis : public location_data_analysis<A, ImperativeTmpStateConstraintGenerator> {};

	template<typename A>
	struct location_data_out_analysis : public location_data_analysis<A, ImperativeOutStateConstraintGenerator> {};


	template<typename Context, typename BaseAnalysis>
	class ImperativeInStateConstraintGenerator : public BasicInConstraintGenerator<StateSetType, StateSetType,ImperativeInStateConstraintGenerator<Context, BaseAnalysis>,Context, Location<Context>> {

		typedef BasicInConstraintGenerator<StateSetType, StateSetType,ImperativeInStateConstraintGenerator<Context, BaseAnalysis>,Context, Location<Context>> super;

		CBA& cba;

	public:

		ImperativeInStateConstraintGenerator(CBA& cba)
			: super(cba, Sin, Sout), cba(cba) {}

		// TODO: the following two functions should be moved into a common base class of the In and Out State converter

		/**
		 * Produces a humna-readable representation of the value represented by the given value ID.
		 */
		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const {

			auto& data = cba.getValueParameters<int,Context,Location<Context>>(value);
			int label = std::get<1>(data);
			const core::NodeAddress& node = cba.getStmt(label);
			const Context& ctxt = std::get<2>(data);
			const Location<Context>& location = std::get<3>(data);

			out << value << " = Sin - " << getAnalysisName<BaseAnalysis>() << "@" << location
						 << "[l" << label << " = " << node->getNodeType() << " : "
						 << node << " = " << core::printer::PrettyPrinter(node, core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE) << " : "
						 << ctxt << "]";
		}

		void connectStateSetsImpl(const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, const Location<Context>& location, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getLocationDataSet<BaseAnalysis>(a, al, ac, location);
			auto s_out = cba.getLocationDataSet<BaseAnalysis>(b, bl, bc, location);

			// state information entering the set is also leaving it
			constraints.add(subset(s_in, s_out));

		}

		template<typename E, typename L>
		void connectStateSetsIfImpl(const E& value, const TypedValueID<L>& set, const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, const Location<Context>& location, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getLocationDataSet<BaseAnalysis>(a, al, ac, location);
			auto s_out = cba.getLocationDataSet<BaseAnalysis>(b, bl, bc, location);

			// state information entering the set is also leaving it
			if (ac == bc) {
				constraints.add(subsetIf(value, set, s_in, s_out));
			} else {
				auto pre = cba.getSet(pred, bc.callContext.back());
				constraints.add(subsetIf(ac.callContext.back(), pre, value, set, s_in, s_out));
			}
		}
	};


	template<typename Context, typename BaseAnalysis>
	struct ImperativeTmpStateConstraintGenerator : public ConstraintGenerator {

		ImperativeTmpStateConstraintGenerator(CBA&) {}

		virtual void addConstraints(CBA& cba, const sc::ValueID& value, Constraints& constraints) {
			// nothing to do here

			const auto& data = cba.getValueParameters<Label,Context,Location<Context>>(value);
			Label label = std::get<1>(data);
			const auto& call = cba.getStmt(label).as<CallExprAddress>();
			const auto& ctxt = std::get<2>(data);
			const auto& loc  = std::get<3>(data);

			// obtain
			auto S_tmp = cba.getLocationDataSet<BaseAnalysis>(Stmp, label, ctxt, loc);
			assert_eq(value, S_tmp) << "Queried a non S_tmp id!";

			// create constraints
			for(const auto& cur : call) {
				auto l_arg = cba.getLabel(cur);
				auto S_out = cba.getLocationDataSet<BaseAnalysis>(Sout, l_arg, ctxt, loc);
				constraints.add(subset(S_out, S_tmp));
			}

			// and the function
			auto l_fun = cba.getLabel(call->getFunctionExpr());
			constraints.add(subset(cba.getLocationDataSet<BaseAnalysis>(Sout, l_fun, ctxt, loc), S_tmp));

		}

		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const {

			const auto& data = cba.getValueParameters<Label,Context,Location<Context>>(value);
			Label label = std::get<1>(data);
			const auto& stmt = cba.getStmt(label);
			const auto& ctxt = std::get<2>(data);
			const auto& loc = std::get<3>(data);

			out << value << " = Stmp - " << getAnalysisName<BaseAnalysis>() << "@" << loc
				 << "[l" << label << " = " << stmt->getNodeType() << " : "
				 << stmt << " = " << core::printer::PrettyPrinter(stmt, core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE) << " : "
				 << ctxt << "]";

		}

	};

	namespace {

		/**
		 * A constraint filter checking whether the current assignment of a given set is exceeding a given
		 * element.
		 */
		template<
			typename StructLattice,
			typename Element
		>
		class ExceedingElementsFilter : public utils::constraint::detail::Filter<false> {
			TypedValueID<StructLattice> a;
			const Element& e;
		public:
			ExceedingElementsFilter(const TypedValueID<StructLattice>& a, const Element& e)
				: a(a), e(e) {}
			bool operator()(const Assignment& ass) const {
				typedef typename StructLattice::base_lattice::value_type base_value_type;
				const base_value_type& set = ass[a];
				return (set.size() - (contains(set, e)?1:0)) > 0;
			}
			void print(std::ostream& out) const {
				out << "|" << a << " - {" << e << "}| > 0";
			}
			utils::constraint::detail::ValueIDs getInputs() const {
				return toVector<ValueID>(a);
			}
			void addUsedInputs(const Assignment& ass, std::set<ValueID>& used) const {
				used.insert(a);
			}
		};

		template<typename StructLattice, typename Element, typename InSet, typename OutSet>
		utils::constraint::ConstraintPtr subsetIfExceeding(const TypedValueID<StructLattice>& set, const Element& e, const InSet& a, const OutSet& b) {
			return combine(ExceedingElementsFilter<StructLattice,Element>(set,e), e_sub(a,b));
		}

		/**
		 * A custom constraint for the data flow equation solver updating the value
		 * of a memory location when conducting a write operation.
		 */
		template<typename ValueLattice, typename RefLattice, typename Context>
		struct WriteConstraint : public Constraint {

			typedef typename RefLattice::base_lattice::value_type ref_set_type;
			typedef typename ref_set_type::value_type ref_type;

			typedef typename ValueLattice::manager_type mgr_type;
			typedef typename ValueLattice::value_type value_type;
			typedef typename ValueLattice::meet_assign_op_type meet_assign_op_type;
			typedef typename ValueLattice::projection_op_type projection_op_type;
			typedef typename ValueLattice::mutation_op_type mutation_op_type;
			typedef typename ValueLattice::less_op_type less_op_type;

			// the manager used for data value objects
			mgr_type& mgr;

			// the location this write operation is working on
			Location<Context> loc;

			// the value covering the read reference
			TypedValueID<RefLattice> ref;

			// the value to be assigned within this operation
			TypedValueID<ValueLattice> in_value;

			// the old state of the memory location before the assignment
			TypedValueID<ValueLattice> old_state;

			// the new state of the memory location after the assignment
			TypedValueID<ValueLattice> new_state;

		public:

			WriteConstraint(
					mgr_type& mgr,
					const Location<Context> loc,
					const TypedValueID<RefLattice>& ref, const TypedValueID<ValueLattice>& in_value,
					const TypedValueID<ValueLattice>& old_state, const TypedValueID<ValueLattice>& new_state)
				: Constraint(toVector<ValueID>(ref, in_value, old_state), toVector<ValueID>(new_state)),
				  mgr(mgr), loc(loc), ref(ref), in_value(in_value), old_state(old_state), new_state(new_state) {}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				// compute updated value and add it to the result value
				meet_assign_op_type meet_assign_op;
				return meet_assign_op(ass[new_state], getUpdatedData(ass)) ? Constraint::Incremented : Constraint::Unchanged;
			}

			virtual bool check(const Assignment& ass) const {
				less_op_type less_op;
				return less_op(getUpdatedData(ass), ass[new_state]);
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				return
					out << ref << " -> " << new_state << "[label=\"" << *this << "\"]\n"
					    << in_value << " -> " << new_state << "[label=\"" << *this << "\"]\n"
					    << old_state << " -> " << new_state << "[label=\"" << *this << "\"]\n";
			}

			virtual std::ostream& writeDotEdge(std::ostream& out, const Assignment& ass) const {
				return
					out << ref << " -> " << new_state << "[label=\"" << loc << " touched by " << ref << "\"" << ((isReferenced(ass))?"":" style=dotted") << "]\n"
					    << in_value << " -> " << new_state << "[label=\"write " << in_value << " to " << new_state << "\"" << ((isReferenced(ass))?"":" style=dotted") << "]\n"
					    << old_state << " -> " << new_state << "[label=\"" << old_state << " in " << new_state << "\"" << ((!isUniquelyReferenced(ass))?"":" style=dotted") << "]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << loc << " touched by " << ref << " => update(" << old_state << "," << in_value << ") in " << new_state;
			}

			virtual bool hasAssignmentDependentDependencies() const {
				return true;
			}

			virtual std::set<ValueID> getUsedInputs(const Assignment& ass) const {
				std::set<ValueID> res;
				res.insert(ref);

				// the old state is needed if reference is not unique
				if (!isUniquelyReferenced(ass)) {
					res.insert(old_state);
				}

				// the in value is required in case the covered location is referenced
				if (isReferenced(ass)) {
					res.insert(in_value);
				}

				return res;
			}

		private:

			bool isUniquelyReferenced(const Assignment& ass) const {
				const ref_set_type& ref_set = ass[ref];
				return (ref_set.empty() || (ref_set.size() == 1u && *ref_set.begin() == loc));		// important: here path of reference must be root!
			}

			bool isReferenced(const Assignment& ass) const {
				// obtain set of references
				const ref_set_type& ref_set = ass[ref];
				for(const auto& cur : ref_set) {
					if (cur.getLocation() == loc) return true;
				}
				return false;
			}

			value_type getUpdatedData(const Assignment& ass) const {

				// get list of accessed data paths in memory location
				const ref_set_type& ref_set = ass[ref];

				// if no reference is yet fixed, no operation can be conducted
				if (ref_set.empty()) return value_type();

				// get list of data paths
				vector<DataPath> paths;
				for(const auto& cur : ref_set) {
					if (cur.getLocation() == loc) {
						paths.push_back(cur.getDataPath());
					}
				}

				// if covered location is not referenced => no update
				if (paths.empty()) return ass[old_state];

				// get current value of location
				const value_type& mem_value = ass[old_state];

				// get value written to the mem_location
				const value_type& input = ass[in_value];

				// get all variations of the memory location that could result when updating the paths
				meet_assign_op_type meet_assign_op;
				mutation_op_type mutation_op;

				value_type res;
				for(const auto& cur : paths) {
					meet_assign_op(res, mutation_op(mgr, mem_value, cur, input));
				}

				// done
				return res;
			}

		};

		template<typename ValueLattice, typename RefLattice, typename Context>
		ConstraintPtr write(typename ValueLattice::manager_type& mgr, const Location<Context>& loc, const TypedValueID<RefLattice>& ref, const TypedValueID<ValueLattice>& in_value, const TypedValueID<ValueLattice>& old_state, const TypedValueID<ValueLattice>& new_state) {
			return std::make_shared<WriteConstraint<ValueLattice,RefLattice, Context>>(mgr, loc, ref, in_value, old_state, new_state);
		}

	}


	template<typename Context, typename BaseAnalysis>
	class ImperativeOutStateConstraintGenerator : public BasicOutConstraintGenerator<StateSetType, StateSetType,ImperativeOutStateConstraintGenerator<Context, BaseAnalysis>,Context, Location<Context>> {

		typedef BasicOutConstraintGenerator<StateSetType, StateSetType,ImperativeOutStateConstraintGenerator<Context, BaseAnalysis>,Context, Location<Context>> super;

		CBA& cba;

	public:

		ImperativeOutStateConstraintGenerator(CBA& cba)
			: super(cba, Sin, Sout), cba(cba) {
		}

		/**
		 * Produces a humna-readable representation of the value represented by the given value ID.
		 */
		virtual void printValueInfo(std::ostream& out, const CBA& cba, const sc::ValueID& value) const {

			auto& data = cba.getValueParameters<int,Context,Location<Context>>(value);
			int label = std::get<1>(data);
			const core::NodeAddress& node = cba.getStmt(label);
			const Context& ctxt = std::get<2>(data);
			const Location<Context>& location = std::get<3>(data);

			out << value << " = Sout - " << getAnalysisName<BaseAnalysis>() << "@" << location
						 << "[l" << label << " = " << node->getNodeType() << " : "
						 << node << " = " << core::printer::PrettyPrinter(node, core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE) << " : "
						 << ctxt << "]";
		}

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, const Location<Context>& location, Constraints& constraints) {
			const auto& base = call->getNodeManager().getLangBasic();

			// one special case: assignments
			auto fun = call.as<CallExprPtr>()->getFunctionExpr();
			if (base.isRefAssign(fun)) {

				// get some labels
				auto l_call = cba.getLabel(call);
				auto l_rhs = cba.getLabel(call[0]);
				auto l_lhs = cba.getLabel(call[1]);

//				// ---- S_out of args => S_tmp of call (only if other location is possible)
//
				auto S_tmp = cba.getLocationDataSet<BaseAnalysis>(Stmp, l_call, ctxt, location);
//
//				// ---- combine S_tmp to S_out ...
//
//				// add rule: loc \in R[rhs] => A[lhs] \sub Sout[call]

				auto R_rhs = cba.getSet(R, l_rhs, ctxt);
				auto A_value = cba.getSet<BaseAnalysis>(l_lhs, ctxt);
				auto S_out = cba.getLocationDataSet<BaseAnalysis>(Sout, l_call, ctxt, location);
//				constraints.add(subsetIf(location, R_rhs, A_value, S_out));
//
//				// add rule: |R[rhs]\{loc}| > 0 => Stmp[call] \sub Sout[call]
//				constraints.add(subsetIfExceeding(R_rhs, location, S_tmp, S_out));

				// ---- add assignment rule ----
				constraints.add(write(cba.template getDataManager(A_value), location, R_rhs, A_value, S_tmp, S_out));

				// done
				return;
			}

			// everything else is treated using the default procedure
			super::visitCallExpr(call, ctxt, location, constraints);
		}


		void connectStateSetsImpl(const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, const Location<Context>& location, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getLocationDataSet<BaseAnalysis>(a, al, ac, location);
			auto s_out = cba.getLocationDataSet<BaseAnalysis>(b, bl, bc, location);

			// state information entering the set is also leaving it
			constraints.add(subset(s_in, s_out));

		}

		template<typename E, typename L>
		void connectStateSetsIfImpl(const E& value, const TypedValueID<L>& set, const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, const Location<Context>& location, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getLocationDataSet<BaseAnalysis>(a, al, ac, location);
			auto s_out = cba.getLocationDataSet<BaseAnalysis>(b, bl, bc, location);

			// state information entering the set is also leaving it
			constraints.add(subsetIf(value, set, s_in, s_out));
		}
	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
