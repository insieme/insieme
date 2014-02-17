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

#include <map>

#include "insieme/analysis/cba/framework/analysis_type.h"
#include "insieme/analysis/cba/framework/generator/data_value_constraint_generator.h"
#include "insieme/analysis/cba/framework/generator/mutable_data.h"

#include "insieme/analysis/cba/framework/entities/data_index.h"
#include "insieme/analysis/cba/framework/entities/data_value.h"

#include "insieme/analysis/cba/framework/analysis.h"
#include "insieme/analysis/cba/framework/cba.h"

#include "insieme/analysis/cba/analysis/reachability.h"
#include "insieme/analysis/cba/analysis/callables.h"
#include "insieme/analysis/cba/analysis/references.h"
#include "insieme/analysis/cba/analysis/functions.h"
#include "insieme/analysis/cba/analysis/call_context.h"
#include "insieme/analysis/cba/analysis/jobs.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/lang/basic.h"

#include "insieme/utils/functional_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * TODO: document
	 * 	  - it is assumed that the default constructed value type is the "unknown" value
	 */

	using namespace core;

	namespace {

		StatementAddress getBody(const ContextFreeCallable& fun) {
			if (auto lambda = fun.isa<LambdaExprAddress>()) {
				return lambda->getBody();
			}
			if (auto bind = fun.isa<BindExprAddress>()) {
				return bind->getCall();
			}
			assert_fail() << "Unsupported function type encountered: " << fun->getNodeType();
			return StatementAddress();
		}

		template<typename Lattice>
		struct StructBuilder : public utils::constraint::detail::Executor {
			typedef std::map<FieldIndex, TypedValueID<Lattice>> element_map;
			typedef typename Lattice::manager_type manager_type;
			typedef typename Lattice::value_type value_type;

			manager_type& mgr;
			element_map elements;
			TypedValueID<Lattice> res;
		public:
			StructBuilder(manager_type& mgr, const element_map& elements, const TypedValueID<Lattice>& res)
				: mgr(mgr), elements(elements), res(res) {}
			utils::constraint::detail::ValueIDs getInputs() const {
				utils::constraint::detail::ValueIDs res;
				for (const auto& cur : elements) res.push_back(cur.second);
				return res;
			}
			utils::constraint::detail::ValueIDs getOutputs() const {
				return toVector<ValueID>(res);
			}
			void print(std::ostream& out) const {
				out << "struct {" << join(",", elements, [](std::ostream& out, const typename element_map::value_type& cur) {
					out << *cur.first.getName() << "=" << cur.second;
				}) << "} in " << res;
			}
			value_type extract(const Assignment& ass) const {
				std::map<FieldIndex, value_type> data;
				for (const auto& cur : elements) {
					data[cur.first] = ass[cur.second];
				}
				return mgr.compound(data);
			}
			bool update(Assignment& ass) const {
				return addAll<typename Lattice::meet_assign_op_type>(extract(ass), ass[res]);
			}
			bool check(const Assignment& ass) const {
				return isSubset<typename Lattice::less_op_type>(extract(ass), ass[res]);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << join("\n", elements, [&](std::ostream& out, const typename element_map::value_type& cur) {
					out << cur.second << " -> " << res << label;
				});
			}
			void addUsedInputs(const Assignment& ass, std::vector<ValueID>& used) const {
				for(const auto& cur : elements) {
					used.push_back(cur.second);
				}
			}
		};

		template<typename Lattice>
		struct StructProject : public utils::constraint::detail::Executor {
			typedef std::map<FieldIndex, TypedValueID<Lattice>> element_map;
			typedef typename Lattice::manager_type manager_type;
			typedef typename Lattice::value_type value_type;

			TypedValueID<Lattice> in;
			FieldIndex field;
			TypedValueID<Lattice> res;
		public:
			StructProject(const TypedValueID<Lattice>& in, const FieldIndex& field, const TypedValueID<Lattice>& res)
				: in(in), field(field), res(res) {}
			utils::constraint::detail::ValueIDs getInputs() const {
				return toVector<ValueID>(in);
			}
			utils::constraint::detail::ValueIDs getOutputs() const {
				return toVector<ValueID>(res);
			}
			void print(std::ostream& out) const {
				out << in << "." << field << " in " << res;
			}
			value_type extract(const Assignment& ass) const {
				static const typename Lattice::projection_op_type projection_op;
				return projection_op(ass[in], field);
			}
			bool update(Assignment& ass) const {
				return addAll<typename Lattice::meet_assign_op_type>(extract(ass), ass[res]);
			}
			bool check(const Assignment& ass) const {
				return isSubset<typename Lattice::less_op_type>(extract(ass), ass[res]);
			}
			void writeDotEdge(std::ostream& out, const string& label) const {
				out << in << " -> " << res << label;
			}
			void addUsedInputs(const Assignment& ass, std::vector<ValueID>& used) const {
				used.push_back(in);
			}
		};

		/**
		 * A custom constraint for the data flow equation solver obtaining the value
		 * from a location a given reference is pointing to it.
		 */
		template<typename NestedAnalysesType, typename ValueLattice, typename RefLattice, typename Context>
		struct ReadConstraint : public Constraint {

			typedef typename RefLattice::base_lattice::value_type ref_set_type;
			typedef typename ref_set_type::value_type ref_type;

			typedef typename ValueLattice::value_type value_type;
			typedef typename ValueLattice::meet_assign_op_type meet_assign_op_type;
			typedef typename ValueLattice::projection_op_type projection_op_type;
			typedef typename ValueLattice::less_op_type less_op_type;

			// the enclosing analysis instance
			CBA& cba;

			// the read operation itself
			CallExprAddress readOp;

			// the context of the read operation
			Context ctxt;

			// the value covering the read reference
			TypedValueID<RefLattice> ref;

			// the set to be updated
			TypedValueID<ValueLattice> res;

			// a map of referenced locations to their values
			mutable std::map<Location<Context>, TypedValueID<ValueLattice>> loc_value_map;

			// the input values reference by this constraint
			mutable std::vector<ValueID> inputs;

		public:

			ReadConstraint(
					CBA& cba, const CallExprAddress& call, const Context& ctxt,
					const TypedValueID<RefLattice>& ref, const TypedValueID<ValueLattice>& res
				) : Constraint(toVector<ValueID>(ref), toVector<ValueID>(res), true, true),
				  cba(cba), readOp(call), ctxt(ctxt), ref(ref), res(res)
			{
				inputs.push_back(ref);
			}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				// read input data and add it to result value
				meet_assign_op_type meet_assign_op;
				return meet_assign_op(ass[res], getReadData(ass)) ? Constraint::Incremented : Constraint::Unchanged;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get list of references
				const set<Reference<Context>>& refs = ass[ref];

				bool changed = false;
				for(const auto& cur : refs) {

					// get the current location
					const auto& loc = cur.getLocation();

					// check whether location has already been referenced before
					if (loc_value_map.find(loc) != loc_value_map.end()) continue;

					// it is a new location
					auto valueSet = cba.getSet(Stmp<NestedAnalysesType>(), readOp, ctxt, loc);
					loc_value_map[loc] = valueSet;
					inputs.push_back(valueSet);
					changed = true;
				}

				// indicated whether some dependencies have changed
				return changed;
			}

			virtual bool check(const Assignment& ass) const {

				// check whether dependencies are up-to-date
				if (updateDynamicDependencies(ass)) return false;

				// check whether data to be read is in result set
				less_op_type less_op;
				return less_op(getReadData(ass), ass[res]);
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				for(const auto& cur : loc_value_map) {
					out << cur.second << " -> " << res << "[label=\"reads\"]\n";
				}
				return out << ref << " -> " << res << "[label=\"defined reference\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "*" << ref << " in " << res;
			}

			virtual const std::vector<ValueID>& getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

		private:

			value_type getReadData(const Assignment& ass) const {
				typedef typename generator<NestedAnalysesType, analysis_config<Context>>::type BaseGenerator;

				// instances of required operators
				meet_assign_op_type meet_assign_op;
				projection_op_type projection_op;

				// initialize empty result
				value_type res;

				// collect data from all referenced memory locations
				const std::set<Reference<Context>>& refs = ass[ref];
				for(const auto& cur : refs) {

					// get targeted location
					const auto& loc = cur.getLocation();

					// special handling for reading global any-refs => undefined values in those cases
					if (loc.isUnknown()) {

						// add the unknown value to the result
						auto unknownValue = BaseGenerator(cba).getUnknownValue();
						unknownValue = getUndefinedValue(cba.template getDataManager(this->res), readOp->getType(), unknownValue);

						// include unknown value
						meet_assign_op(res, unknownValue);
						continue;
					}

					// get current value of location
					const value_type& mem_value = ass[loc_value_map[loc]];

					// collect all values from the loc_value referenced by the paths
					meet_assign_op(res, projection_op(mem_value, cur.getDataPath()));
				}

				// return result
				return res;
			}

		};

		template<typename NestedAnalysesType, typename ValueLattice, typename RefLattice, typename Context>
		ConstraintPtr read(CBA& cba, const CallExprAddress& readOp, const Context& readCtxt, const TypedValueID<RefLattice>& ref, const TypedValueID<ValueLattice>& res) {
			return std::make_shared<ReadConstraint<NestedAnalysesType,ValueLattice,RefLattice,Context>>(cba, readOp, readCtxt, ref, res);
		}


		/**
		 * ----------------------------- Bound Value Collector Constraint --------------------------------
		 */

		/**
		 * A custom constraint collecting bound values from a call site depending on the called
		 * bind closure.
		 */
		template<typename ValueAnalysisType, typename ValueLattice, typename CallableLattice, typename Context>
		struct BoundValueCollectorConstraint : public Constraint {

			typedef typename ValueLattice::value_type value_type;
			typedef typename ValueLattice::meet_assign_op_type meet_assign_op_type;
			typedef typename ValueLattice::projection_op_type projection_op_type;
			typedef typename ValueLattice::less_op_type less_op_type;

			// the enclosing analysis instance
			CBA& cba;

			// the list of callees referenced at the call site
			TypedValueID<CallableLattice> callees;

			// the bind expression looking for
			BindExprAddress bind;

			// the address of the captured value to be collected (with unspecified context)
			ExpressionAddress capturedValue;

			// the set to be updated
			TypedValueID<ValueLattice> res;

			// the list of value sources where captured values are defined
			mutable std::vector<TypedValueID<ValueLattice>> sources;

			// the inputs referenced by this constraint
			mutable std::vector<ValueID> inputs;

		public:

			BoundValueCollectorConstraint(
					CBA& cba, const TypedValueID<CallableLattice>& callees,
					const BindExprAddress& bind, const ExpressionAddress& capturedValue,
					const TypedValueID<ValueLattice>& res
				) : Constraint(toVector<ValueID>(callees), toVector<ValueID>(res), true, true),
				  cba(cba), callees(callees), bind(bind), capturedValue(capturedValue), res(res)
			{
				inputs.push_back(callees);
			}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				// the operator merging values
				meet_assign_op_type meet_assign_op;

				// update the value by merging all known sources
				auto& value = ass[res];
				bool changed = false;
				for(const auto& cur : sources) {
					changed = meet_assign_op(value, ass[cur]) || changed;
				}

				// check whether something has changed
				return (changed) ? Constraint::Incremented : Constraint::Unchanged;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get list of references
				const set<Callable<Context>>& funs = ass[callees];

				bool changed = false;
				for(const auto& cur : funs) {

					// just interested in the processed bind
					if (cur.getDefinition() != bind) continue;

					// get the set representing the value captured by the current bind
					auto captured = cba.getSet(ValueAnalysisType(), capturedValue, cur.getContext());

					// add it to the source-list
					if (::contains(sources, captured)) continue;
					sources.push_back(captured);
					inputs.push_back(captured);
					changed = true;
				}

				// indicated whether some dependencies have changed
				return changed;
			}

			virtual bool check(const Assignment& ass) const {

				// check whether dependencies are up-to-date
				if (updateDynamicDependencies(ass)) return false;

				// check whether data to be read is in result set
				less_op_type less_op;
				const auto& value = ass[res];
				for(const auto& cur : sources) {
					if (!less_op(ass[cur], value)) return false;
				}

				// everything is fine
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				for(const auto& cur : sources) {
					out << cur << " -> " << res << "[label=\"passed to\"]\n";
				}
				return out << callees << " -> " << res << "[label=\"defines\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "boundValues(" << callees << ") in " << res;
			}

			virtual const std::vector<ValueID>& getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

		};

		template<typename ValueAnalysisType, typename Context, typename ValueLattice, typename CallableLattice>
		ConstraintPtr collectCapturedValue(CBA& cba, const TypedValueID<CallableLattice>& callables, const BindExprAddress& bind, const ExpressionAddress& capturedValue, const TypedValueID<ValueLattice>& res) {
			return std::make_shared<BoundValueCollectorConstraint<ValueAnalysisType,ValueLattice,CallableLattice,Context>>(cba, callables, bind, capturedValue, res);
		}


		/**
		 * ----------------------------- Job Value Collector Constraint --------------------------------
		 */

		/**
		 * A custom constraint collecting values bound at the creation point of a job.
		 */
		template<typename ValueAnalysisType, typename ValueLattice, typename JobLattice, typename Context>
		struct JobValueCollectorConstraint : public Constraint {

			typedef typename ValueLattice::value_type value_type;
			typedef typename ValueLattice::meet_assign_op_type meet_assign_op_type;
			typedef typename ValueLattice::projection_op_type projection_op_type;
			typedef typename ValueLattice::less_op_type less_op_type;

			typedef typename lattice<job_analysis_data, analysis_config<Context>>::type CallableLattice;

			// the enclosing analysis instance
			CBA& cba;

			// the list of jobs considered by this constraint
			TypedValueID<JobLattice> jobs;

			// the bind expression looking for
			BindExprAddress bind;

			// the address of the captured value to be collected (with unspecified context)
			ExpressionAddress capturedValue;

			// the set to be updated
			TypedValueID<ValueLattice> res;

			// the list of referenced thread bodies
			mutable std::vector<TypedValueID<CallableLattice>> bodies;

			// the list of captured values
			mutable std::vector<TypedValueID<ValueLattice>> sources;

			// the inputs referenced by this constraint
			mutable std::vector<ValueID> inputs;

		public:

			JobValueCollectorConstraint(
					CBA& cba, const TypedValueID<JobLattice>& jobs,
					const BindExprAddress& bind, const ExpressionAddress& capturedValue,
					const TypedValueID<ValueLattice>& res
				) : Constraint(toVector<ValueID>(jobs), toVector<ValueID>(res), true, true),
				  cba(cba), jobs(jobs), bind(bind), capturedValue(capturedValue), res(res)
			{
				inputs.push_back(jobs);
			}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				// the operator merging values
				meet_assign_op_type meet_assign_op;

				// update the value by merging all known sources
				auto& value = ass[res];
				bool changed = false;
				for(const auto& cur : sources) {
					changed = meet_assign_op(value, ass[cur]) || changed;
				}

				// check whether something has changed
				return (changed) ? Constraint::Incremented : Constraint::Unchanged;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get list of covered jobs
				const set<Job<Context>>& job_set = ass[jobs];

				bool changed = false;
				for(const auto& j : job_set) {

					// get set of job-bodies
					auto body = cba.getSet(C, j.getAddress()->getDefaultExpr(), j.getContext());

					// register body
					if (!::contains(bodies, body)) {
						bodies.push_back(body);
						inputs.push_back(body);
						changed = true;
					}

					// get list of references
					const set<Callable<Context>>& funs = ass[body];

					for(const auto& cur : funs) {

						// just interested in the processed bind
						if (cur.getDefinition() != bind) continue;

						// get the set representing the value captured by the current bind
						auto captured = cba.getSet(ValueAnalysisType(), capturedValue, cur.getContext());

						// add it to the source-list
						if (::contains(sources, captured)) continue;
						sources.push_back(captured);
						inputs.push_back(captured);
						changed = true;
					}

				}

				// indicated whether some dependencies have changed
				return changed;
			}

			virtual bool check(const Assignment& ass) const {

				// check whether dependencies are up-to-date
				if (updateDynamicDependencies(ass)) return false;

				// check whether data to be read is in result set
				less_op_type less_op;
				const auto& value = ass[res];
				for(const auto& cur : sources) {
					if (!less_op(ass[cur], value)) return false;
				}

				// everything is fine
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				for(const auto& cur : bodies) {
					out << cur << " -> " << res << "[label=\"depends\"]\n";
				}
				for(const auto& cur : sources) {
					out << cur << " -> " << res << "[label=\"passed to\"]\n";
				}
				return out << jobs << " -> " << res << "[label=\"defines\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "boundValues(bodiesOf(" << jobs << ")) in " << res;
			}

			virtual const std::vector<ValueID>& getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

		};

		template<typename ValueAnalysisType, typename Context, typename ValueLattice, typename JobLattice>
		ConstraintPtr collectCapturedJobValues(CBA& cba, const TypedValueID<JobLattice>& jobs, const BindExprAddress& bind, const ExpressionAddress& capturedValue, const TypedValueID<ValueLattice>& res) {
			return std::make_shared<JobValueCollectorConstraint<ValueAnalysisType,ValueLattice,JobLattice,Context>>(cba, jobs, bind, capturedValue, res);
		}
	}


	template<
		typename ValueAnalysisType,
		typename VariableAnalysisType,
		typename Context
	>
	class DataFlowConstraintGenerator : public DataValueConstraintGenerator<Context> {

		typedef DataValueConstraintGenerator<Context> super;

	public:

		typedef typename lattice<ValueAnalysisType, analysis_config<Context>>::type lattice_type;
		typedef typename lattice_type::manager_type mgr_type;

		typedef typename lattice_type::value_type value_type;

	private:

		// the two set types to deal with
		const ValueAnalysisType& A;		// the value set (labels -> values)
		const VariableAnalysisType& a;		// the variable set (variables -> values)

		CBA& cba;

		mgr_type& valueMgr;

		value_type unknown;		// the value to be assigned to unknown values

	public:

		DataFlowConstraintGenerator(CBA& cba, const ValueAnalysisType& A, const VariableAnalysisType& a, const value_type& unknown = value_type())
			: super(cba), A(A), a(a), cba(cba), valueMgr(cba.getDataManager<lattice_type>()), unknown(unknown) { };

		mgr_type& getValueManager() {
			return valueMgr;
		}

		value_type getUnknownValue() const {
			return unknown;
		}

		value_type getUnknownValue(const TypePtr& type) const {
			return getUndefinedValue(valueMgr, type, unknown);
		}

		template<typename V>
		value_type atomic(const V& value) {
			return getValueManager().atomic(value);
		}

		void visitCompoundStmt(const CompoundStmtAddress& compound, const Context& ctxt, Constraints& constraints) {

			// only interested in lambda bodies
			if (compound.isRoot()) return;
			if (compound.getParentNode()->getNodeType() != NT_Lambda) return;

			// TODO: identify return statements more efficiently

			auto l_body = cba.getLabel(compound);
			auto A_body = cba.getSet(A, l_body, ctxt);

			// since value of a compound is the value of return statements => visit those
			visitDepthFirstPrunable(compound, [&](const StatementAddress& stmt) {
				// prune inner functions
				if (stmt.isa<LambdaExprAddress>()) return true;

				// visit return statements
				if (auto returnStmt = stmt.isa<ReturnStmtAddress>()) {
					// connect value of return statement with body value
					auto l_return = cba.getLabel(returnStmt->getReturnExpr());
					auto A_return = cba.getSet(A, l_return, ctxt);

					// add constraint - forward in case end of return expression is reachable
					auto R_ret = cba.getSet(Rout, l_return, ctxt);
					constraints.add(subsetIf(Reachable(), R_ret, A_return, A_body));

					// TODO: this is just a performance improvement - but for now disabled
//						visit(returnStmt, ctxt, constraints);
					return true;
				}

				return false;
			});

		}

		void visitStatement(const StatementAddress& stmt, const Context& ctxt, Constraints& constraints) {
			// not other statement has a value => nothing to do
		}

		void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {
			// external literals are by default unknown values - could be overloaded by sub-classes
			auto A_lit = cba.getSet(A, literal, ctxt);
			constraints.add(subset(getUnknownValue(literal->getType()), A_lit));
		}

		void visitVariable(const VariableAddress& variable, const Context& ctxt, Constraints& constraints) {

			// ----- Part I: read variable value -------

			// add constraint a(var) \subset A(var)
			auto var = cba.getVariable(variable);
			auto l_var = cba.getLabel(variable);

			auto a_var = cba.getSet(a, var, ctxt);
			auto A_var = cba.getSet(A, l_var, ctxt);

			constraints.add(subset(a_var, A_var));


			// ----- Part II: add constraints for variable definition point ------

			// let it be handled by the definition point
			VariableAddress def = getDefinitionPoint(variable);
			if (def != variable) {
				visit(def, ctxt, constraints);
				return;
			}

			// ok - this is the definition point
			// => check type of variable (determined by parent)

			// no parent: free variable, needs to assume an unknown value
			if (def.isRoot()) {
				constraints.add(subset(unknown, a_var));
				return;
			}

			// so, there should be a parent
			auto parent = def.getParentAddress();
			switch(parent->getNodeType()) {

				// if the variable is declared imperatively => just handle declaration statement
				case NT_DeclarationStmt: {

					// check whether it is a for-loop
					if (!parent.isRoot() && parent.isa<ForStmtAddress>()) {

						// a for-loop iterator is unknown by default
						// TODO: find a better solution for this ...
//						constraints.add(elem(T(), a_var));
						break;
					}

					// TODO: consider for-loops

					auto decl = parent.as<DeclarationStmtAddress>();

					// add constraint r(var) \subset C(init)
					auto l_init = cba.getLabel(decl->getInitialization());

					// TODO: distinguish between control and data flow!
					auto A_init = cba.getSet(A, l_init, ctxt);
					constraints.add(subset(A_init, a_var));		// TODO: add cba (passed by argument)

					// finally, add constraints for init expression
//						visit(decl->getInitialization(), ctxt, constraints);

					break;
				}

				// the variable may be a parameter of a lambda or bind
				case NT_Parameters: {

					// this should not be the end
					assert(!parent.isRoot());

//					// obtain the set containing all the potential predecessor of the current call in the cba
//					auto predecessor_ctxt = cba.getSet(pred, ctxt.callContext.back());

					// get containing callee (lambda or bind)
					Callee callee(parent.getParentAddress());
					assert(callee.isLambda() || callee.isBind());

					// get all callers
					const vector<Caller>& callers = cba.getCallSiteManager().getCaller(callee);

					// extract index of parameter within parameter list
					auto param_index = variable.getIndex();

					// connect parameter to arguments of all potential calls
					for(const Caller& cur : callers) {

						const auto& call = cur.getCall();
						auto l_call = cba.getLabel(call);
						auto l_cur_call_fun = cba.getLabel(call->getFunctionExpr());
						const auto& arg = call[param_index];
						auto l_arg = cba.getLabel(arg);


						// ----------- compute list of call contexts -----------

						// get full list of surrounding contexts
						set<Context> contexts;
						if (causesContextShift(call)) {

							// skip this call if it is not the one referenced by the call context
							if (ctxt.callContext.back() != l_call) continue;

							// take all surrounding contexts
							contexts = cba.getSurroundingContexts(ctxt);

						} else {
							// we will just stay within the current context
							contexts.insert(ctxt);
						}

						// ------- special case: argument is a bound value of a call expression -------

						// check whether it is a bound argument
						if (isBoundValueInFreeBind(arg)) {

							// we have to get the value bound to the argument at the creation point of the closure

							// get bind expression
							auto bind = call.getParentAddress().as<BindExprAddress>();
							auto num_params = bind->getParameters()->size();

							// for all the potential call contexts
							for(const auto& callCtxt : contexts) {

								// get call creating current context
								auto l_bind_call = callCtxt.callContext.back();
								if (l_bind_call == 0) continue;

								// check number of arguments of bind call
								auto bindCall = cba.getStmt(l_bind_call).template as<CallExprAddress>();
								if (bindCall.size() != num_params) continue;

								// get potential contexts of the bind call
								for(const auto& bindCallCtxt : cba.getSurroundingContexts(callCtxt)) {

									// get value of function called by bind call
									auto l_fun_bind_call = cba.getLabel(bindCall->getFunctionExpr());
									auto C_fun_bind_call = cba.getSet(C, l_fun_bind_call, bindCallCtxt);

									// create a constraint collecting all captured values
									constraints.add(collectCapturedValue<ValueAnalysisType,Context>(cba, C_fun_bind_call, bind, arg, a_var));

								}

							}

							// it might also be the case that the bind is called as the body of a job (most jobs have bind-bodies)
							if (ctxt.isEmptyCallContext()) {

								// if the call context is empty, we are probably in the root of a thread
								if (ctxt.isEmptyThreadContext()) continue;	// if context is empty, we are not in a thread!

								// get thread call context
								const auto& spawnID = ctxt.threadContext.front();
								const auto& spawnStmt = cba.getStmt(spawnID.getSpawnLabel()).template as<CallExprAddress>();
								const auto& spawnCtxt = Context(spawnID.getSpawnContext());

								auto J_spawned_job = cba.getSet(Jobs, spawnStmt[0], spawnCtxt);

								constraints.add(collectCapturedJobValues<ValueAnalysisType,Context>(cba, J_spawned_job, bind, arg, a_var));

							}

							// done
							continue;
						}


						// ----------- handle parameter passing ------------

						// link argument within all potential call contexts to target
						for(const auto& callCtxt : contexts) {

							// get value of function targeted by current call within call context
							auto F_cur_call = cba.getSet(F, l_cur_call_fun, callCtxt);

							// get value of argument within call
							auto A_arg = cba.getSet(A, l_arg, callCtxt);

							// also check whether this call can actually be reached
							auto reachable_call = cba.getSet(Rin, l_call, callCtxt);

							// link argument with parameter if context is valid and function is correct
							// TODO: you may wanna filter statically dispatched functions
							// TODO: filter pre-decessor context
							constraints.add(subsetIf(Reachable(), reachable_call, callee, F_cur_call, A_arg, a_var));
						}

					}

					// this should be it
					break;
				}

				// recursive variables may be skipped (no general data flow involved here)
				case NT_LambdaBinding: {
					break;
				}

				default: {
					// fail at this point
					assert_fail() << "Unsupported parent type encountered: " << parent->getNodeType() << "\n";
					break;
				}
			}

		}

		void visitStructExpr(const StructExprAddress& expr, const Context& ctxt, Constraints& constraints) {

			// collect values of all fields
			std::map<FieldIndex, TypedValueID<lattice_type>> elements;
			for(const core::NamedValueAddress& cur : expr->getMembers()) {
				elements[cur.getAddressedNode()->getName()] = cba.getSet(A, cur->getValue(), ctxt);
			}

			// combine it
			constraints.add(
					utils::constraint::build(
							StructBuilder<lattice_type>(this->getValueManager(), elements, cba.getSet(A, expr, ctxt))
					)
			);

		}

		void visitLambdaExpr(const LambdaExprAddress& lambda, const Context& ctxt, Constraints& constraints) {
			// nothing to do here => magic happens at call site
		}

		void visitBindExpr(const BindExprAddress& bind, const Context& ctxt, Constraints& constraints) {
			// nothing to do here => magic happens at call site
		}

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

			// the value of the call expression is the result of the function

			auto fun = call->getFunctionExpr();

			// get resulting set
			auto l_call = cba.getLabel(call);
			auto A_call = cba.getSet(A, l_call, ctxt);

			// create context of target body
			Context innerCtxt = ctxt;
			if (causesContextShift(call)) {
				innerCtxt.callContext <<= l_call;
			}

			// get list of potential targets
			const vector<Callee>& targets = cba.getCallSiteManager().getCallee(call);

			// if target is fixed => no conditions on constraint edge
			if (targets.size() == 1u) {

				const auto& fun = targets[0];

				// special handling for literals
				if (fun.isLiteral()) {

					// one special case: if it is a read operation
					const auto& base = call->getNodeManager().getLangBasic();
					if (base.isRefDeref(targets[0].getDefinition())) {

						// read value from memory location
						auto l_trg = this->cba.getLabel(call[0]);
						auto R_trg = this->cba.getSet(R, l_trg, ctxt);

						// add read constraint
						constraints.add(read<ValueAnalysisType>(cba, call, ctxt, R_trg, A_call));

					}

					// another case: accessing struct members
					if (base.isCompositeMemberAccess(targets[0].getDefinition())) {

						// get input struct value
						auto A_in = this->cba.getSet(A, call[0], ctxt);

						// get field name
						auto field = call[1].as<core::LiteralPtr>()->getValue();

						// project value of field from input struct to output struct
						constraints.add(
								utils::constraint::build(
									StructProject<lattice_type>(A_in, field, A_call)
								)
						);
					}

					// and always: if it is the undefined literal
					if (base.isUndefined(targets[0].getDefinition())) {

						// built up resulting value
						auto value = getUnknownValue(call->getType());

						// in this case initialize the value with the undefined value
						constraints.add(subset(value, A_call));
					}

					// no other literals supported by default - overloads may add more
					return;
				}

				// for the rest, connect the result of the body with the value of the call
				auto l_body = cba.getLabel(fun.getBody());
				auto A_body = cba.getSet(A, l_body, innerCtxt);

				// take over value of function body
				constraints.add(subset(A_body, A_call));
				return;		// and done
			}

			// if there is more than 1 potential target a constraint depending
			// on the value of the function expression is added (to test which
			// function of the potential list of functions is actually called)

			// NOTE: - Optimization - we only need to know the body, not the context it was created in (for binds)
			//  => we can iterate through the list of free functions, not the callables

			// get set representing the value of the function expression
			auto l_fun = cba.getLabel(fun);
			auto F_fun = cba.getSet(F, l_fun, ctxt);

			// process all potential targets
			for(const Callee& cur : targets) {
				// connect target body with result value
				auto l_body = cba.getLabel(cur.getBody());
				auto A_body = cba.getSet(A, l_body, innerCtxt);
				// .. if the current target is actually targeted!
				constraints.add(subsetIf(cur, F_fun, A_body, A_call));
			}

		}


		void visitNode(const NodeAddress& node, const Context& ctxt, Constraints& constraints) {
			std::cout << "Reached unsupported Node Type: " << node->getNodeType() << "\n";
			assert(false);
		}

	protected:

		template<typename value_type, typename fun_type, typename data_mgr>
		class packer {

			fun_type fun;
			data_mgr& mgr;

		public:

			packer(const fun_type& fun, data_mgr& mgr)
				: fun(fun), mgr(mgr) {}

			template<typename ... Args>
			value_type operator()(const Args& ... args) const {
				return mgr.atomic(fun(args...));
			}
		};

		template<typename fun_type>
		packer<value_type,fun_type,mgr_type> pack(const fun_type& fun) {
			return packer<value_type,fun_type,mgr_type>(fun, this->getValueManager());
		}

		template<typename E>
		ConstraintPtr elem(const E& e, const TypedValueID<lattice_type>& set) {
			return subset(atomic(e), set);
		}


	private:

		bool isBoundValueInFreeBind(const ExpressionAddress& expr) const {

			// ok, we need at least 2 levels of parents
			if (expr.getDepth() < 3) return false;

			// the first needs to be a call
			auto call = expr.getParentAddress().isa<CallExprAddress>();
			if (!call) return false;

			// the second a bind
			auto bind = call.getParentAddress().isa<BindExprAddress>();
			if (!bind) return false;

			// and the expression must be bound
			assert(bind->getCall() == call);
			if (!bind->isBoundExpression(expr)) return false;

			// test whether bind is free (not statically bound)
			call = bind.getParentAddress().isa<CallExprAddress>();
			return !call || call->getFunctionExpr() != bind;
		}

		bool isBoundValueInFreeBind(const NodeAddress& node) const {
			if (node->getNodeCategory() != NC_Expression) return false;
			return isBoundValueInFreeBind(node.as<ExpressionAddress>());
		}

	};



	template<
		typename T,
		typename AnalysisType,
		typename Context
	>
	struct BasicDataFlowConstraintGenerator : DataFlowConstraintGenerator<AnalysisType,AnalysisType,Context> {

		BasicDataFlowConstraintGenerator(CBA& cba, const AnalysisType& A, const AnalysisType& a)
			: DataFlowConstraintGenerator<AnalysisType,AnalysisType,Context>(cba,A,a) { };
	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
