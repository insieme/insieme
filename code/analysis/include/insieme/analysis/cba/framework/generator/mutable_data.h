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


	template<typename Context, typename ElementSetType>
	class ImperativeInStateConstraintGenerator;

	template<typename Context, typename ElementSetType>
	class ImperativeOutStateConstraintGenerator;

	template<typename Context, typename ElementSetType>
	class ImperativeInStateConstraintGenerator : public BasicInConstraintGenerator<StateSetType, StateSetType,ImperativeInStateConstraintGenerator<Context, ElementSetType>,Context> {

		typedef BasicInConstraintGenerator<StateSetType, StateSetType,ImperativeInStateConstraintGenerator<Context, ElementSetType>,Context> super;

		const ElementSetType& dataSet;

		// the one location this instance is working for
		Location<Context> location;

		CBA& cba;

	public:

		ImperativeInStateConstraintGenerator(CBA& cba, const ElementSetType& dataSet, const Location<Context>& location)
			: super(cba, Sin, Sout, *this), dataSet(dataSet), location(location), cba(cba) {}

		void connectStateSets(const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getSet(a, al, ac, location, dataSet);
			auto s_out = cba.getSet(b, bl, bc, location, dataSet);

			// state information entering the set is also leaving it
			constraints.add(subset(s_in, s_out));

		}

		template<typename E, typename L>
		void connectStateSetsIf(const E& value, const TypedValueID<L>& set, const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getSet(a, al, ac, location, dataSet);
			auto s_out = cba.getSet(b, bl, bc, location, dataSet);

			// state information entering the set is also leaving it
			if (ac == bc) {
				constraints.add(subsetIf(value, set, s_in, s_out));
			} else {
				auto pre = cba.getSet(pred, bc.callContext.back());
				constraints.add(subsetIf(ac.callContext.back(), pre, value, set, s_in, s_out));
			}
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


	template<typename Context, typename ElementSetType>
	class ImperativeOutStateConstraintGenerator : public BasicOutConstraintGenerator<StateSetType, StateSetType,ImperativeOutStateConstraintGenerator<Context, ElementSetType>,Context> {

		typedef BasicOutConstraintGenerator<StateSetType, StateSetType,ImperativeOutStateConstraintGenerator<Context, ElementSetType>,Context> super;

		typedef typename ElementSetType::lattice_type lattice_type;
		typedef typename lattice_type::manager_type mgr_type;

		const ElementSetType& dataSet;

		// the one location this instance is working for
		Location<Context> location;

		CBA& cba;

	public:

		ImperativeOutStateConstraintGenerator(CBA& cba, const ElementSetType& dataSet, const Location<Context>& location)
			: super(cba, Sin, Sout, *this), dataSet(dataSet), location(location), cba(cba) {
		}

		mgr_type& getDataManager() {
			return cba.getDataManager<lattice_type>();
		}

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
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
				auto R_rhs = cba.getSet(R<Context>(), l_rhs, ctxt);
				auto S_out_rhs = cba.getSet(Sout, l_rhs, ctxt, location, dataSet);
				auto S_out_lhs = cba.getSet(Sout, l_lhs, ctxt, location, dataSet);
				auto S_tmp = cba.getSet(Stmp, l_call, ctxt, location, dataSet);
				constraints.add(subsetIfExceeding(R_rhs, location, S_out_rhs, S_tmp));
				constraints.add(subsetIfExceeding(R_rhs, location, S_out_lhs, S_tmp));
//
//				// ---- combine S_tmp to S_out ...
//
//				// add rule: loc \in R[rhs] => A[lhs] \sub Sout[call]
				auto A_value = cba.getSet(dataSet, l_lhs, ctxt);
				auto S_out = cba.getSet(Sout, l_call, ctxt, location, dataSet);
//				constraints.add(subsetIf(location, R_rhs, A_value, S_out));
//
//				// add rule: |R[rhs]\{loc}| > 0 => Stmp[call] \sub Sout[call]
//				constraints.add(subsetIfExceeding(R_rhs, location, S_tmp, S_out));

				// ---- add assignment rule ----
				constraints.add(write(getDataManager(), this->location, R_rhs, A_value, S_tmp, S_out));

				// done
				return;
			}

			// everything else is treated using the default procedure
			super::visitCallExpr(call, ctxt, constraints);
		}


		void connectStateSets(const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getSet(a, al, ac, location, dataSet);
			auto s_out = cba.getSet(b, bl, bc, location, dataSet);

			// state information entering the set is also leaving it
			constraints.add(subset(s_in, s_out));

		}

		template<typename E, typename L>
		void connectStateSetsIf(const E& value, const TypedValueID<L>& set, const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getSet(a, al, ac, location, dataSet);
			auto s_out = cba.getSet(b, bl, bc, location, dataSet);

			// state information entering the set is also leaving it
			constraints.add(subsetIf(value, set, s_in, s_out));
		}
	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
