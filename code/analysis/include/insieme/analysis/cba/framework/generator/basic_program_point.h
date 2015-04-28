/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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
#include "insieme/analysis/cba/framework/generator/data_value_constraint_generator.h"

#include "insieme/analysis/cba/framework/analysis.h"
#include "insieme/analysis/cba/framework/cba.h"

#include "insieme/analysis/cba/framework/entities/formula.h"

#include "insieme/analysis/cba/analysis/boolean.h"
#include "insieme/analysis/cba/analysis/reachability.h"
#include "insieme/analysis/cba/analysis/jobs.h"
#include "insieme/analysis/cba/analysis/thread_groups.h"
#include "insieme/analysis/cba/analysis/thread_list.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/int_type.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;
	template<typename Context, typename ... ExtraParams> class DataValueConstraintGenerator;

	// ----------------------------------------------------------------------------------------------------------------------------
	//
	//													Program State Constraints Generator
	//
	// ----------------------------------------------------------------------------------------------------------------------------

	// TODO: rewrite Collector => Derived

	template<
		typename InSetIDType,			// the type of the in-set handled by this constraint generator
		typename TmpSetIDType,			// the type of the tmp-set handled by this constraint generator (temporal results within call expressions and others)
		typename OutSetIDType, 			// the type of the out-set handled by this constraint generator
		typename Derived, 				// the derived constraint generator type
		typename Context,
		typename ... ExtraParams
	>
	class BasicInOutConstraintGenerator : public DataValueConstraintGenerator<Context, ExtraParams...> {

	protected:

		typedef DataValueConstraintGenerator<Context, ExtraParams...> super;

		// the sets to be used for in/out states
		const InSetIDType& Ain;
		const TmpSetIDType& Atmp;
		const OutSetIDType& Aout;

		CBA& cba;

	public:

		BasicInOutConstraintGenerator(CBA& cba, const InSetIDType& Ain, const TmpSetIDType& Atmp, const OutSetIDType& Aout)
			: super(cba), Ain(Ain), Atmp(Atmp), Aout(Aout), cba(cba) {}

	protected:

		template<typename SetTypeA, typename SetTypeB>
		void connectSets(const SetTypeA& a, const StatementInstance& al, const Context& ac, const SetTypeB& b, const StatementInstance& bl, const Context& bc, const ExtraParams& ... args, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			connectStateSets(a, cba.getLabel(al), ac, b, cba.getLabel(bl), bc, args..., constraints);
		}

		template<typename E, typename L, typename SetTypeA, typename SetTypeB>
		void connectSetsIf(const E& value, const TypedVariable<L>& set, const SetTypeA& a, const StatementInstance& al, const Context& ac, const SetTypeB& b, const StatementInstance& bl, const Context& bc, const ExtraParams& ... args, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			connectStateSetsIf(value, set, a, cba.getLabel(al), ac, b, cba.getLabel(bl), bc, args..., constraints);
		}

		template<typename F, typename SetTypeA, typename SetTypeB>
		void connectSetsIf(const F& filter, const SetTypeA& a, const StatementInstance& al, const Context& ac, const SetTypeB& b, const StatementInstance& bl, const Context& bc, const ExtraParams& ... args, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			connectStateSetsIfFilter(filter, a, cba.getLabel(al), ac, b, cba.getLabel(bl), bc, args..., constraints);
		}

		template<typename SetTypeA, typename SetTypeB>
		void connectStateSets(const SetTypeA& a, Label al, const Context& ac, const SetTypeB& b, Label bl, const Context& bc, const ExtraParams& ... args, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			static_cast<Derived*>(this)->template connectStateSetsImpl(a,al,ac,b,bl,bc,args...,constraints);
		}

		template<typename E, typename L, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIf(const E& value, const TypedVariable<L>& set, const SetTypeA& a, Label al, const Context& ac, const SetTypeB& b, Label bl, const Context& bc, const ExtraParams& ... args, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			static_cast<Derived*>(this)->template connectStateSetsIfImpl(value,set,a,al,ac,b,bl,bc,args...,constraints);
		}

		template<typename F, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIfFilter(const F& filter, const SetTypeA& a, Label al, const Context& ac, const SetTypeB& b, Label bl, const Context& bc, const ExtraParams& ... args, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			static_cast<Derived*>(this)->template connectStateSetsIfFilterImpl(filter,a,al,ac,b,bl,bc,args...,constraints);
		}


		// -- default implementations --

		template<typename SetTypeA, typename SetTypeB>
		void connectStateSetsImpl (
					const SetTypeA& a, Label al, const Context& ac,
					const SetTypeB& b, Label bl, const Context& bc,
					const ExtraParams& ... params,
					Constraints& constraints
				) const {

			auto A = cba.getVar(a, al, ac, params...);
			auto B = cba.getVar(b, bl, bc, params...);
			constraints.add(subset(A, B));
		}

		template<typename E, typename L, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIfImpl (
					const E& value, const TypedVariable<L>& set,
					const SetTypeA& a, Label al, const Context& ac,
					const SetTypeB& b, Label bl, const Context& bc,
					const ExtraParams& ... params,
					Constraints& constraints
				) const {

			// simple version (no check of contexts)
			auto A = cba.getVar(a, al, ac, params...);
			auto B = cba.getVar(b, bl, bc, params...);
			constraints.add(subsetIf(value, set, A, B));
		}

		template<typename Filter, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIfFilterImpl (
					const Filter& filter,
					const SetTypeA& a, Label al, const Context& ac,
					const SetTypeB& b, Label bl, const Context& bc,
					const ExtraParams& ... params,
					Constraints& constraints
				) const {

			// simple version (no check of contexts)
			auto A = cba.getVar(a, al, ac, params...);
			auto B = cba.getVar(b, bl, bc, params...);
			constraints.add(combine(filter, e_sub(A, B)));
		}

	};



	namespace {

		/**
		 * A factory function for a constrain linking the head of a thread to its spawn point(s).
		 */
		template<typename Context, typename ValueLattice, typename TmpAnalysisType>
		ConstraintPtr createSpawnPointConnectorConstraint(CBA& cba, const typename Context::thread_context& nestedContext, const TypedVariable<ValueLattice>& startPoint, const TmpAnalysisType& tmp);

		// an overload for one extra parameter
		template<typename Context, typename ValueLattice, typename TmpAnalysisType, typename Arg0>
		ConstraintPtr createSpawnPointConnectorConstraint(CBA& cba, const typename Context::thread_context& nestedContext, const TypedVariable<ValueLattice>& startPoint, const TmpAnalysisType& tmp, const Arg0&);

		// an overload for two extra parameter
		template<typename Context, typename ValueLattice, typename TmpAnalysisType, typename Arg0, typename Arg1>
		ConstraintPtr createSpawnPointConnectorConstraint(CBA& cba, const typename Context::thread_context& nestedContext, const TypedVariable<ValueLattice>& startPoint, const TmpAnalysisType& tmp, const Arg0&, const Arg1&);

	}

	template<
		typename InSetIDType,
		typename TmpSetIDType,
		typename OutSetIDType,
		typename Derived,
		typename Context,
		typename ... ExtraParams
	>
	class BasicInConstraintGenerator : public BasicInOutConstraintGenerator<InSetIDType, TmpSetIDType, OutSetIDType, Derived, Context, ExtraParams...> {

		typedef BasicInOutConstraintGenerator<InSetIDType, TmpSetIDType, OutSetIDType, Derived, Context, ExtraParams...> super;

		CBA& cba;

	public:

		BasicInConstraintGenerator(CBA& cba, const InSetIDType& Ain, const TmpSetIDType& Atmp, const OutSetIDType& Aout)
			: super(cba, Ain, Atmp, Aout), cba(cba) {}

		void connectCallToBody(const CallExprInstance& call, const Context& callCtxt, const StatementInstance& body, const Context& trgCtxt, const Callee& callee, const ExtraParams& ... args, Constraints& constraints) {

			// filter out invalid contexts
			if (!cba.isValid(callCtxt) || !cba.isValid(trgCtxt)) return;

			// check whether given call / target context is actually valid
			auto l_call = cba.getLabel(call);
			if (causesContextShift(call)) {
				if (callCtxt.callContext << l_call != trgCtxt.callContext) return;
			} else if (callCtxt.callContext != trgCtxt.callContext) {
				return;
			}

			// check proper number of arguments
			if (callee.getNumParams() != call.size()) return;

			// get label for the body expression
			auto l_body = cba.getLabel(body);

			// get labels for call-site
			auto l_fun = cba.getLabel(call->getFunctionExpr());
			auto F_call = cba.getVar(F, l_fun, callCtxt);

			// check whether call-site is within a bind
			bool isCallWithinBind = (!call.isRoot() && call.getParentNode()->getNodeType() == NT_BindExpr);
			auto bind = (isCallWithinBind) ? call.getParentInstance().as<BindExprInstance>() : BindExprInstance();
			// special case: if this is a bind with no free parameters
			if (bind && bind->getParameters().empty()) {
				// connect in of call site with in of body
				this->connectStateSetsIf(callee, F_call, this->Ain, l_call, callCtxt, this->Ain, l_body, trgCtxt, args..., constraints);
			} else {
				// connect body with tmp of call
				this->connectStateSetsIf(callee, F_call, this->Atmp, l_call, callCtxt, this->Ain, l_body, trgCtxt, args..., constraints);
			}

		}

		void visitCallExpr(const CallExprInstance& call, const Context& ctxt, const ExtraParams& ... args, Constraints& constraints) {

			// special handling only for calls in bind expressions
			if (call.isRoot() || call.getParentNode()->getNodeType() != NT_BindExpr) {
				// run standard procedure
				this->visitExpression(call, ctxt, args..., constraints);
				return;
			}

			// ----- we have a call in a bind expression ----
			auto bind = call.getParentInstance().as<BindExprInstance>();
			if (bind.isRoot()) return;	// nothing to do

			auto user = bind.getParentInstance();

			// check for direct calls ...
			if (user->getNodeType() == NT_CallExpr && user.as<CallExprInstance>()->getFunctionExpr() == bind) {

				// it is one => no change in context
				this->connectSets(this->Ain, bind, ctxt, this->Ain, call, ctxt, args..., constraints);

			} else if (ctxt.isEmptyCallContext()) {

				// if the call context is empty, we are probably in the root of a thread
				if (ctxt.isEmptyThreadContext()) return;	// if context is empty, we are not in a thread!

				// get spawning point
				const auto& spawnID = ctxt.threadContext.front();
				const auto& spwanStmt = cba.getStmt(spawnID.getSpawnLabel());
				const auto& spawnCallCtxt = spawnID.getSpawnContext();

				// special case: if the last component is the root thread => use root thread
				typename Context::thread_id mainThread;
				if (ctxt.threadContext.back() == mainThread) {

					// get thread context of spawning point
					auto spawnThreadCtxt = ctxt.threadContext << typename Context::thread_id();

					// construct context of spawn call
					const auto& spawnCtxt = Context(spawnCallCtxt, spawnThreadCtxt);

					// connect to spawn location (but we have to guess the thread context)
					// TODO: this should actually be Atmp ...
					this->connectSets(this->Atmp, spwanStmt, spawnCtxt, this->Ain, call, ctxt, args..., constraints);

				} else {

					// use a custom constraint collecting all potential spawn points
					auto head = cba.getVar(this->Ain, call, ctxt, args...);
					constraints.add(createSpawnPointConnectorConstraint<Context>(cba, ctxt.threadContext, head, this->Atmp, args...));

				}

			} else {

				// it is no direct call => change in context possible
				Callee callee(bind);
				for (const Caller& cur : cba.getCallSiteManager().getCaller(callee)) {

					// may be reached from any context
					for(auto& l : cba.getDynamicCallLabels()) {

						// nobody calls context 0
						if (l != 0 && ctxt.callContext.startsWith(0)) continue;

						Context srcCtxt = ctxt;
						srcCtxt.callContext >>= l;

						// connect call site with body
						connectCallToBody(cur, srcCtxt, call, ctxt, bind, args..., constraints);
					}
				}
			}

		}

		void visitCompoundStmt(const CompoundStmtInstance& stmt, const Context& ctxt, const ExtraParams& ... args, Constraints& constraints) {

			// TODO: check whether it is a function body => otherwise default handling
			if (stmt.isRoot()) return;

			auto parent = stmt.getParentInstance();

			// handle lambda
			if (auto lambda = parent.isa<LambdaInstance>()) {

				// get all call sites
				Callee callee(lambda);
				const vector<Caller>& caller = cba.getCallSiteManager().getCaller(callee);

				// link in all potential call sites
				for(const Caller& cur : caller) {

					// check whether context has to be shifted
					if (causesContextShift(cur)) {

						// produce all potential call sites
						for (auto& l : cba.getDynamicCallLabels()) {

							// nobody calls context 0
							if (l != 0 && ctxt.callContext.startsWith(0)) continue;

							Context srcCtxt = ctxt;
							srcCtxt.callContext >>= l;

							// connect call site with body
							connectCallToBody(cur.getCall(), srcCtxt, stmt, ctxt, callee, args..., constraints);
						}

					} else {

						// no context switch required
						connectCallToBody(cur.getCall(), ctxt, stmt, ctxt, callee, args..., constraints);

					}

				}

				// done
				return;
			}

			// use default handling
			visitStatement(stmt, ctxt, args..., constraints);

		}


		void visitStatement(const StatementInstance& stmt, const Context& ctxt, const ExtraParams& ... args, Constraints& constraints) {

			// determine predecessor based on parent
			if (stmt.isRoot()) return;		// no predecessor

			// check out parent
			auto parent = stmt.getParentInstance();

			// TODO: turn this into a visitor!

			// special case: if current expression is an argument of a bind-call expression
			if (stmt.getDepth() >=2) {
				if (auto call = parent.isa<CallExprInstance>()) {
					if (auto bind = call.getParentInstance().isa<BindExprInstance>()) {
						// if this is a bound expression predecessor is the bind, not the call
						if (bind->isBoundExpression(stmt.as<ExpressionInstance>())) {
							// connect bind with stmt - skip the call
							this->connectSets(this->Ain, bind, ctxt, this->Ain, stmt, ctxt, args..., constraints);
							// and done
							return;
						}
					}
				}
			}

			// if the parent is a call expression the state transitions need to be chained up
			if (auto call = parent.isa<CallExprInstance>()) {

				// we have to distinguish the chain of non-captured values evaluation (during execution)
				if (!isCapturedValue(stmt.as<ExpressionInstance>())) {

					// the first to evaluate is the function
					if (call->getFunctionExpr() == stmt) {
						// in this case the in of the call is the in of the stmt
						this->connectSets(this->Ain, call, ctxt, this->Ain, stmt, ctxt, args..., constraints);
						return;
					}

					// otherwise it is a argument and we connect it to its predecessor (the first predecessor is the function expr)
					auto pre = parent.getInstanceOfChild(stmt.getIndex() - 1).as<ExpressionInstance>();
					while(isCapturedValue(pre)) {	// skip captured values
						if (pre == call->getFunctionExpr()) {	// we reached the edge here
							this->connectSets(this->Ain, call, ctxt, this->Ain, stmt, ctxt, args..., constraints);
							return;
						} else {
							pre = parent.getInstanceOfChild(pre.getIndex() - 1).as<ExpressionInstance>();
						}
					}

					// connect out of predecessor to in of current statement
					this->connectSets(this->Aout, pre, ctxt, this->Ain, stmt, ctxt, args..., constraints);
					return;

				} else {
					// and the chain of captured value evaluations (during evaluation of a bind)
					auto bind = call.getParentInstance().as<BindExprInstance>();

					// the first to evaluate is the function
					if (call->getFunctionExpr() == stmt) {
						// in this case the in of the call is the in of the stmt
						this->connectSets(this->Ain, bind, ctxt, this->Ain, stmt, ctxt, args..., constraints);
						return;
					}

					// otherwise it is a argument and we connect it to its predecessor (the first predecessor is the function expr)
					auto pre = parent.getInstanceOfChild(stmt.getIndex() - 1).as<ExpressionInstance>();
					while(!isCapturedValue(pre)) {	// skip captured values
						if (pre == call->getFunctionExpr()) {	// we reached the edge here
							this->connectSets(this->Ain, bind, ctxt, this->Ain, stmt, ctxt, args..., constraints);
							return;
						} else {
							pre = parent.getInstanceOfChild(pre.getIndex() - 1).as<ExpressionInstance>();
						}
					}

					// skip evaluation of function expression when forming bind
					if (pre == call->getFunctionExpr()) {
						this->connectSets(this->Ain, bind, ctxt, this->Ain, stmt, ctxt, args..., constraints);
						return;
					}

					// connect out of predecessor to in of current statement
					this->connectSets(this->Aout, pre, ctxt, this->Ain, stmt, ctxt, args..., constraints);
					return;
				}
			}

			// a simple case - it is just a nested expression
			if (auto expr = parent.isa<ExpressionInstance>()) {
				// parent is an expression => in of parent is in of current stmt
				this->connectSets(this->Ain, expr, ctxt, this->Ain, stmt, ctxt, args..., constraints);
				return;	// done
			}

			// if it is a named value of struct expression
			if (auto value = parent.isa<NamedValueInstance>()) {
				// TODO: actually here the evaluation should be chained up!
				// the grant-parent should be a struct expression => link in with this in state
				auto expr = parent.getParentInstance(2).as<StructExprInstance>();
				this->connectSets(this->Ain, expr, ctxt, this->Ain, stmt, ctxt, args..., constraints);
				return; // done
			}

			// handle full-expressions
			if (auto compound = parent.isa<CompoundStmtInstance>()) {

				// parent is a compound, predecessor is one statement before
				auto pos = stmt.getIndex();

				// special case: first statement
				if (pos == 0) {
					this->connectSets(this->Ain, compound, ctxt, this->Ain, stmt, ctxt, args..., constraints);
					return;	// done
				}

				// general case - link with predecessor
				auto prev = compound[pos-1];

				// do not link with previous control statements
				switch(prev->getNodeType()) {
				case NT_ReturnStmt: case NT_ContinueStmt: case NT_BreakStmt: return;
				default: break;
				}

				this->connectSets(this->Aout, prev, ctxt, this->Ain, stmt, ctxt, args..., constraints);
				return;	// done
			}

			// handle simple statements
			if (parent.isa<ReturnStmtInstance>() || parent.isa<DeclarationStmtInstance>()) {
				// in is the in of the stmt
				this->connectSets(this->Ain, parent.as<StatementInstance>(), ctxt, this->Ain, stmt, ctxt, args..., constraints);
				return; // done
			}

			// handle if stmt
			if (auto ifStmt = parent.isa<IfStmtInstance>()) {

				// check whether which part the current node is

				auto cond = ifStmt->getCondition();
				if (cond == stmt) {

					// connect in with if-stmt in with condition in
					this->connectSets(this->Ain, ifStmt, ctxt, this->Ain, stmt, ctxt, args..., constraints);

				} else if (ifStmt->getThenBody() == stmt) {

					// connect out of condition with in of body if condition may be true
					auto l_cond = cba.getLabel(cond);
					auto B_cond = cba.getVar(B, l_cond, ctxt);
					this->connectSetsIf(true, B_cond, this->Aout, cond, ctxt, this->Ain, stmt, ctxt, args..., constraints);

				} else if (ifStmt->getElseBody() == stmt) {

					// connect out of condition with in of body if condition may be false
					auto l_cond = cba.getLabel(cond);
					auto B_cond = cba.getVar(B, l_cond, ctxt);
					this->connectSetsIf(false, B_cond, this->Aout, cond, ctxt, this->Ain, stmt, ctxt, args..., constraints);

				} else {
					assert_fail() << "No way!\n";
				}

				// dones
				return;
			}

			// handle while stmt
			if (auto whileStmt = parent.isa<WhileStmtInstance>()) {

				// check which part of a while the current node is
				auto cond = whileStmt->getCondition();
				auto l_cond = cba.getLabel(cond);
				auto B_cond = cba.getVar(B, l_cond, ctxt);
				if (cond == stmt) {
					// connect in of while to in of condition
					this->connectSets(this->Ain, whileStmt, ctxt, this->Ain, stmt, ctxt, args..., constraints);

					// also, in case loop is looping, out of body is in of condition
					this->connectSetsIf(true, B_cond, this->Aout, whileStmt->getBody(), ctxt, this->Ain, stmt, ctxt, args..., constraints);

				} else if (whileStmt->getBody() == stmt) {
					// connect out of condition with in of body
					this->connectSetsIf(true, B_cond, this->Aout, cond, ctxt, this->Ain, stmt, ctxt, args..., constraints);
				} else {
					assert_fail() << "No way!";
				}
				return;
			}

			// handle for stmt
			if (auto forStmt = parent.isa<ForStmtInstance>()) {

				// check which part of a while the current node is
				auto body = forStmt->getBody();
				if (body == stmt) {

					// connect out of declaration, end and step to in of body
					this->connectSets(this->Aout, forStmt->getDeclaration(), ctxt, this->Ain, body, ctxt, args..., constraints);
					this->connectSets(this->Aout, forStmt->getEnd(),         ctxt, this->Ain, body, ctxt, args..., constraints);
					this->connectSets(this->Aout, forStmt->getStep(),        ctxt, this->Ain, body, ctxt, args..., constraints);

					// also, since it is a loop, the out of the loop body is the in of the next iteration
					// TODO: consider continues and breaks!
					this->connectSets(this->Aout, body, ctxt, this->Ain, body, ctxt, args..., constraints);

				} else {

					// connect in of for with in of declaration, end and step (current stmt)
					this->connectSets(this->Ain, forStmt, ctxt, this->Ain, stmt, ctxt, args..., constraints);

				}
				return;
			}

			// handle marker stmt
			if (auto markerStmt = parent.isa<MarkerStmtInstance>()) {
				// marker stmts are just ignored
				this->connectSets(this->Ain, markerStmt, ctxt, this->Ain, stmt, ctxt, args..., constraints);
				return;
			}

			assert_fail() << "Unsupported parent type encountered: " << parent->getNodeType();
		}
	};


	namespace detail {

		// -- merge constraint (for merging parallel control flows) ---------

		template<
			typename Context,
			typename ThreadOutAnalysisType,
			typename ThreadGroupValue,
			typename Lattice,
			typename ... ExtraParams
		>
		class ParallelMergeConstraint : public Constraint {

			typedef typename Lattice::value_type value_type;
			typedef typename Lattice::less_op_type less_op;

			typedef typename all_meet_assign_op_type<ThreadOutAnalysisType,analysis_config<Context>>::type all_meet_assign_op_type;
			typedef typename one_meet_assign_op_type<ThreadOutAnalysisType,analysis_config<Context>>::type one_meet_assign_op_type;

			CBA& cba;
			const ThreadOutAnalysisType& out;						// the type of analysis / values to be aggregated from the exit node of the threads
			const TypedVariable<ThreadGroupValue> thread_group;
			const TypedVariable<Lattice> in_state;
			const TypedVariable<Lattice> out_state;

			const std::tuple<ExtraParams...> params;

			// the killed thread states to be merged in (only if thread_group points to a single thread)
			mutable vector<TypedVariable<Lattice>> thread_out_states;
			mutable vector<Variable> dependencies;

			// the full list of dependencies
			mutable vector<Variable> allInputs;

		public:

			ParallelMergeConstraint(
					CBA& cba,
					const ThreadOutAnalysisType& out,
					const TypedVariable<ThreadGroupValue>& thread_group,
					const TypedVariable<Lattice>& in_state,
					const TypedVariable<Lattice>& out_state,
					const ExtraParams& ... params)
				: Constraint(toVector<Variable>(thread_group, in_state), toVector<Variable>(out_state), true, true),
				  cba(cba), out(out), thread_group(thread_group), in_state(in_state), out_state(out_state), params(std::make_tuple(params...)) {
				allInputs.push_back(thread_group);
				allInputs.push_back(in_state);
			}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				const static less_op less;

				// get reference to current value
				auto& value = ass[out_state];

				// compute new value
				auto updated = getUpdatedValue(ass);

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
				updateDynamicDependenciesInternal(ass);
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

			virtual const std::vector<Variable>& getUsedInputs(const Assignment& ass) const {
				// update thread out state list and all dynamic dependencies
				if (updateDynamicDependenciesInternal(ass)) {
					// ... and if those have changed, update the inputs list
					allInputs.clear();
					allInputs.push_back(thread_group);
					allInputs.push_back(in_state);
					for(const auto& cur : dependencies) allInputs.push_back(cur);
				}

				// return current list of dependencies
				return allInputs;
			}

		private:

			bool updateDynamicDependenciesInternal(const Assignment& ass) const {

				// TODO: this is a prototype implementation
				//	  Required:
				//			- cleanup
				//	  Done:
				//			- move this to base class


				// clear lists
				thread_out_states.clear();
				vector<Variable> newDependencies;

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
				auto spawnPoint = group.getCreationPoint().template as<CallExprInstance>();

				// get potential list of jobs to be started at spawn point
				auto jobValue = cba.getVar(Jobs, spawnPoint[0], group.getContext());
				newDependencies.push_back(jobValue);
				const set<Job<Context>>& jobs = ass[jobValue];

				// if there is more than 1 candidate => we are done (TODO: maybe not, we can still compute the intersection of jobs)
				assert_le(jobs.size(), 1u) << "Only direct dependency supported so far!";
				if (jobs.size() != 1u) {
					auto res = (newDependencies != dependencies);
					dependencies = newDependencies;
					return res;
				}

				// obtain body of job
				const Job<Context>& job = *jobs.begin();
				const JobExprInstance& jobExpr = job.getCreationPoint();

				// get set containing list of bodies
				auto C_body = cba.getVar(C, jobExpr->getBody(), job.getContext());

				// get list of body functions
				newDependencies.push_back(C_body);
				const std::set<Callable<Context>>& bodies = ass[C_body];
				assert_le(bodies.size(), 1u) << "Only direct dependency supported so far!";
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

				auto out_set = getVariable(out, utils::int_type<0>(), params, body, innerContext);
				thread_out_states.push_back(out_set);
				newDependencies.push_back(out_set);

				// check whether something has changed
				auto res = (newDependencies != dependencies);
				if (res) dependencies = newDependencies;
				return res;
			}

			value_type getUpdatedValue(const Assignment& ass) const {
				static const all_meet_assign_op_type all_meet_assign;
				static const one_meet_assign_op_type one_meet_assign;


				value_type res = ass[in_state];		// all in-values are always included

				// if there is no thread-out-state known, we are done
				if (thread_out_states.empty()) return res;

				// combine thread_out_states

				// merge in effects of thread-out-states if there are any
				value_type merged;
				for(const auto& cur : thread_out_states) {
					const value_type& out = ass[cur];

					// merge in effects
					one_meet_assign(merged, out);
				}

				// combine the all- and one-path effects
				all_meet_assign(res, merged);

				// done
				return res;
			}

		private:

			template<int i, typename ValueType, typename Tuple, typename ... Args>
			sc::TypedVariable<typename lattice<ValueType,analysis_config<Context>>::type>
			getVariable(const ValueType& type, const utils::int_type<i>& c, const Tuple& t, const Args& ... args) const {
				return getVariable(type, utils::int_type<i+1>(), t, args..., std::get<i>(t));
			}

			template<typename ValueType, typename Tuple, typename ... Args>
			sc::TypedVariable<typename lattice<ValueType,analysis_config<Context>>::type>
			getVariable(const ValueType& type, const utils::int_type<sizeof...(ExtraParams)>& c, const Tuple& t, const Args& ... args) const {
				return cba.getVar(type, args...);
			}

		};


		template<typename Context, typename TGValue, typename ThreadOutAnalysisType, typename DataValue, typename ... ExtraParams>
		ConstraintPtr parallelMerge(CBA& cba, const ThreadOutAnalysisType& out, const TypedVariable<TGValue>& threadGroup, const TypedVariable<DataValue>& in_state, const TypedVariable<DataValue>& out_state, const ExtraParams& ... params) {
			return std::make_shared<ParallelMergeConstraint<Context,ThreadOutAnalysisType,TGValue,DataValue,ExtraParams...>>(cba,out,threadGroup,in_state,out_state, params...);
		}

	}

	namespace detail {

		// checks whether there is an element within the first set that is <= an element in the second set
		bool le(const std::set<Formula>& sa, const std::set<Formula>& sb);

		// create a filter determining whether loop is entered
		bool loop_entered(const std::set<Formula>& l, const std::set<Formula>& u, const std::set<Formula>& s);

		// create a filter determining whether loop is not entered
		bool loop_not_entered(const std::set<Formula>& l, const std::set<Formula>& u, const std::set<Formula>& s);

	}



	template<
		typename InSetIDType,
		typename TmpSetIDType,
		typename OutSetIDType,
		typename Derived,
		typename Context,
		typename ... ExtraParams
	>
	class BasicOutConstraintGenerator : public BasicInOutConstraintGenerator<InSetIDType, TmpSetIDType, OutSetIDType, Derived, Context, ExtraParams...> {

		typedef BasicInOutConstraintGenerator<InSetIDType, TmpSetIDType, OutSetIDType, Derived, Context, ExtraParams...> super;

		CBA& cba;

	public:

		BasicOutConstraintGenerator(CBA& cba, const InSetIDType& Ain, const TmpSetIDType& Atmp, const OutSetIDType& Aout)
			: super(cba, Ain, Atmp, Aout), cba(cba) {}


		void visitCallExpr(const CallExprInstance& call, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {

			// things to do:
			//  - link in of call with in of arguments
			//  - link out of arguments with in of function
			//  - link out of function with out of call

			auto l_call = cba.getLabel(call);

			// create inner call context
			Context innerCtxt = ctxt;
			if (causesContextShift(call)) {
				innerCtxt.callContext <<= l_call;
			}

			// get list of potential targets
			const vector<Callee>& targets = cba.getCallSiteManager().getCallee(call);

			// if target is fixed => no condition on constraint edge
			if (targets.size() == 1u) {

				// special case - call is a merge call
				auto& basic = call->getNodeManager().getLangBasic();
				if (core::analysis::isCallOf(call.as<CallExprPtr>(), basic.getMerge())) {

					// In this case we have to:
					//		- compute the set of merged thread groups
					//		- if there is only one (100% save to assume it is this group) we can
					//		  merge the killed definitions at the end of the thread group with the killed definitions of the in-set
					//		- otherwise we can not be sure => no operation

					// get involved sets
					auto A_tmp = cba.getVar(this->Atmp, call, ctxt, params...);
					auto A_out = cba.getVar(this->Aout, call, ctxt, params...);

					auto tg = cba.getVar(ThreadGroups, call[0], ctxt);

					// add constraint
					constraints.add(detail::parallelMerge<Context>(cba, this->Aout, tg, A_tmp, A_out, params...));

					// done
					return;
				}

				// skip handling of literals
				if (targets[0].isLiteral()) {
					// we assume literals have no affect
					this->connectStateSets(this->Atmp, l_call, ctxt, this->Aout, l_call, ctxt, params..., constraints);
					return;
				}

				// get label of body
				auto l_body = cba.getLabel(targets[0].getBody());

				// just connect out of body with out of call
				this->connectStateSets(this->Aout, l_body, innerCtxt, this->Aout, l_call, ctxt, params..., constraints);

				// and done
				return;
			}

			// if there are multiple targets => check the value of the function expression

			// get set of potential target functions
			auto l_fun = cba.getLabel(call->getFunctionExpr());
			auto F_fun = cba.getVar(F, l_fun, ctxt);

			for(const Callee& cur : targets) {

				// get label of body
				auto l_body = cba.getLabel(cur.getBody());

				// just connect out of body with out of call if function fits
				this->connectStateSetsIf(cur, F_fun, this->Aout, l_body, innerCtxt, this->Aout, l_call, ctxt, params..., constraints);
			}

		}

		void visitBindExpr(const BindExprInstance& bind, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			auto l_bind = cba.getLabel(bind);

			// the out-effect is the effect of the last bound arguments
			auto call = bind->getCall();
			for(int i = (call.size()-1); i >= 0; --i) {
				auto cur = call[i];
				if (isCapturedValue(cur)) {
					this->connectStateSets(this->Aout, cba.getLabel(cur), ctxt, this->Aout, l_bind, ctxt, params..., constraints);
					return;
				}
			}

			// if no argument is captured connect out to in state (this way functions are not captured)
			this->connectStateSets(this->Ain, l_bind, ctxt, this->Aout, l_bind, ctxt, params..., constraints);

			// and no more ! (in particular not the effects of the inner call)
		}

		void visitExpression(const ExpressionInstance& expr, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			// for most expressions: just connect in and out
			auto l_cur = cba.getLabel(expr);
			this->connectStateSets(this->Ain, l_cur, ctxt, this->Aout, l_cur, ctxt, params..., constraints);
		}

		void visitCompoundStmt(const CompoundStmtInstance& stmt, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {

			// special case: empty compound
			if (stmt.empty()) {
				this->connectSets(this->Ain, stmt, ctxt, this->Aout, stmt, ctxt, params..., constraints);
				return;
			}

			// connect with last statement (if reachable)
			auto last = stmt[stmt.size()-1];
			auto type = last->getNodeType();
			if (!(type == NT_ReturnStmt || type == NT_ContinueStmt || type==NT_BreakStmt)) {
				this->connectSetsIf(Reachable(), cba.getVar(Rout, last, ctxt), this->Aout, last, ctxt, this->Aout, stmt, ctxt, params..., constraints);
			}

			// also connect stmt-out with all returns if it is a lambda body
			if (!stmt.isRoot() && !stmt.getParentNode().isa<LambdaPtr>()) return;

			// TODO: locate return statements more efficiently

			vector<ReturnStmtInstance> returns;

			visitDepthFirstPrunable(stmt, [&](const StatementInstance& stmt) {
				// prune inner functions
				if (stmt.isa<LambdaExprInstance>()) return true;

				// visit return statements
				if (auto returnStmt = stmt.isa<ReturnStmtInstance>()) {
					returns.push_back(returnStmt);
					return true;
				}

				return false;
			});

			auto l_body = cba.getLabel(stmt);
			for(const ReturnStmtInstance& returnStmt : returns) {
				// connect value of return statement with body value
				auto l_ret = cba.getLabel(returnStmt);
				auto R_ret = cba.getVar(Rout, l_ret, ctxt);
				this->connectStateSetsIf(Reachable(), R_ret, this->Aout, l_ret, ctxt, this->Aout, l_body, ctxt, params..., constraints);
			}

		}

		void visitDeclarationStmt(const DeclarationStmtInstance& stmt, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			// link out of init expression to out of decl stmt
			this->connectSets(this->Aout, stmt->getInitialization(), ctxt, this->Aout, stmt, ctxt, params..., constraints);
		}

		void visitReturnStmt(const ReturnStmtInstance& stmt, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			// link out of return expression to out of return stmt
			this->connectSets(this->Aout, stmt->getReturnExpr(), ctxt, this->Aout, stmt, ctxt, params..., constraints);
		}

		void visitIfStmt(const IfStmtInstance& stmt, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			// link out with out of bodies
			auto l_cond = cba.getLabel(stmt->getCondition());
			auto B_cond = cba.getVar(B, l_cond, ctxt);
			this->connectSetsIf(true,  B_cond, this->Aout, stmt->getThenBody(), ctxt, this->Aout, stmt, ctxt, params..., constraints);
			this->connectSetsIf(false, B_cond, this->Aout, stmt->getElseBody(), ctxt, this->Aout, stmt, ctxt, params..., constraints);
		}

		void visitSwitchStmt(const SwitchStmtInstance& stmt, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			// link out with out of cases
			for(const auto& cur : stmt->getCases()) {
				this->connectSets(this->Aout, cur->getBody(), ctxt, this->Aout, stmt, ctxt, params..., constraints);
			}
		}

		void visitWhileStmt(const WhileStmtInstance& stmt, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			// link out of condition to out if condition may ever become false
			auto cond = stmt->getCondition();
			auto l_cond = cba.getLabel(cond);
			auto B_cond = cba.getVar(B, l_cond, ctxt);
			this->connectSetsIf(false, B_cond, this->Aout, cond, ctxt, this->Aout, stmt, ctxt, params..., constraints);
		}

		void visitForStmt(const ForStmtInstance& stmt, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			typedef std::function<bool(const std::set<Formula>&,const std::set<Formula>&,const std::set<Formula>&)> filter;
			// TODO: consider continues and breaks!

			// get values defining range
			auto Al = cba.getVar(A, stmt->getStart(), ctxt);
			auto Au = cba.getVar(A, stmt->getEnd(), ctxt);
			auto As = cba.getVar(A, stmt->getStep(), ctxt);

			// link out of body with out of for stmt
			auto body = stmt->getBody();
			this->connectSetsIf(f_trinary(filter(detail::loop_entered), Al, Au, As), this->Aout, body, ctxt, this->Aout, stmt, ctxt, params..., constraints);

			// link in-state with out-state of step expression (last parameter to be evaluated)
			this->connectSetsIf(f_trinary(filter(detail::loop_not_entered), Al, Au, As), this->Aout, stmt->getStep(), ctxt, this->Aout, stmt, ctxt, params..., constraints);
		}

		void visitMarkerStmt(const MarkerStmtInstance& stmt, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			// link out of body with out of for stmt
			auto body = stmt->getSubStatement();
			// TODO: consider continues and breaks!
			this->connectSets(this->Aout, body, ctxt, this->Aout, stmt, ctxt, params..., constraints);
		}

		void visitNode(const NodeInstance& node, const Context& ctxt, const ExtraParams& ... params, Constraints& res) {
			assert_fail() << "Unsupported Node Type encountered: " << node->getNodeType();
		}

	};


	template<
		typename InValueType,
		typename TmpValueType,
		typename OutValueType,
		typename Context,
		typename ... ExtraParams
	>
	class BasicTmpConstraintGenerator : public DataValueConstraintGenerator<Context, ExtraParams...> {

		CBA& cba;

		const InValueType& Ain;
		const TmpValueType& Atmp;
		const OutValueType& Aout;

	public:

		BasicTmpConstraintGenerator(CBA& cba, const InValueType& Ain, const TmpValueType& Atmp, const OutValueType& Aout)
			: DataValueConstraintGenerator<Context, ExtraParams...>(cba), cba(cba), Ain(Ain), Atmp(Atmp), Aout(Aout) {}

		void visitCallExpr(const CallExprInstance& call, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {

			auto A_tmp = cba.getVar(Atmp, call, ctxt, params...);

			// link it with out-state of last non-captured parameter / function
			for(int i=(call.size()-1); i>=0; i--) {
				auto cur = call[i];
				if (!isCapturedValue(cur)) {
					auto A_out = cba.getVar(Aout, cur, ctxt, params...);
					constraints.add(subset(A_out, A_tmp));
					return;
				}
			}

			// if we reach this point all arguments have been captured ...
			if (!isCapturedValue(call->getFunctionExpr())) {

				// link it with the out of the function expression
				auto A_fun = cba.getVar(Aout, call->getFunctionExpr(), ctxt, params...);
				constraints.add(subset(A_fun, A_tmp));
				return;
			}

			// so the function expression and all its arguments are captured => link it with the in state of the call
			auto A_in = cba.getVar(Ain, call, ctxt, params...);
			constraints.add(subset(A_in, A_tmp));

		}

		void visitStatement(const StatementInstance& stmt, const Context& ctxt, const ExtraParams& ... params, Constraints& constraints) {
			// simply link in with tmp
			auto A_in  = cba.getVar(Ain, stmt, ctxt, params...);
			auto A_tmp = cba.getVar(Atmp, stmt, ctxt, params...);
			constraints.add(subset(A_in, A_tmp));
		}

	};

	namespace {

		/**
		 * ----------------------------- a Constraint Connecting the start of a thread with its spawn point --------------------------------
		 */

		/**
		 * A custom constraint connecting a thread to its spawn point(s).
		 */
		template<typename ValueLattice, typename Context>
		struct SpawnPointConnector : public Constraint {

			typedef typename ValueLattice::value_type value_type;
			typedef typename ValueLattice::meet_assign_op_type meet_assign_op_type;
			typedef typename ValueLattice::less_op_type less_op_type;

			typedef std::function<TypedVariable<ValueLattice>(CBA&, const StatementInstance&, const Context&)> var_resolver_function_type;

			typedef typename lattice<thread_list_analysis,analysis_config<Context>>::type thread_list_lattice;
			typedef typename thread_list_lattice::value_type thread_list;
			typedef typename Context::thread_context thread_type;

			// the enclosing analysis instance
			CBA& cba;

			// the program point marking the start of the linked thread
			TypedVariable<ValueLattice> head;

			// the id of the nested thread
			thread_type nestedThread;

			// the variable listing all threads in the program
			TypedVariable<thread_list_lattice> threadList;

			// a function generation variables describing state of a given point
			var_resolver_function_type varGen;

			// the list of variables describing the state at potential spawn points
			mutable std::vector<TypedVariable<ValueLattice>> spawns;

			// the inputs referenced by this constraint
			mutable std::vector<Variable> inputs;

		public:

			SpawnPointConnector(
					CBA& cba, const thread_type& nestedThread, const TypedVariable<ValueLattice>& head, const TypedVariable<thread_list_lattice>& threadList, const var_resolver_function_type& varGen
				) : Constraint(toVector<Variable>(threadList), toVector<Variable>(head), true, true),
				  cba(cba), head(head), nestedThread(nestedThread), threadList(threadList), varGen(varGen)
			{
				inputs.push_back(threadList);
			}

			virtual Constraint::UpdateResult update(Assignment& ass) const {
				// the operator merging values
				meet_assign_op_type meet_assign_op;

				// update the value by merging all known sources
				auto& value = ass[head];
				bool changed = false;
				for(const auto& cur : spawns) {
					changed = meet_assign_op(value, ass[cur]) || changed;
				}

				// check whether something has changed
				return (changed) ? Constraint::Incremented : Constraint::Unchanged;
			}

			virtual bool updateDynamicDependencies(const Assignment& ass) const {

				// get list of threads
				const thread_list& threads = ass[threadList];

				bool changed = false;
				for(const auto& t : threads) {

					// check whether this thread is a potential parent thread
					if ((t >> nestedThread.front()) == nestedThread) {

						// get spawning statement
						const auto& spawnStmt = cba.getStmt(nestedThread.front().getSpawnLabel());

						// construct context of spawn call
						const auto& spawnCtxt = Context(nestedThread.front().getSpawnContext(), t);

						// get variable referencing spawn point
						auto var = varGen(cba, spawnStmt, spawnCtxt);

						// check whether this location is already linked
						if (::contains(spawns, var)) continue;

						// add to list of potential spawning locations
						spawns.push_back(var);
						inputs.push_back(var);
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
				const auto& value = ass[head];
				for(const auto& cur : spawns) {
					if (!less_op(ass[cur], value)) return false;
				}

				// everything is fine
				return true;
			}

			virtual std::ostream& writeDotEdge(std::ostream& out) const {
				for(const auto& cur : spawns) {
					out << cur << " -> " << head << "[label=\"spawns\"]\n";
				}
				return out;
//				return out << threadList << " -> " << head << "[label=\"defines\"]\n";
			}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "union({" << ::join(",", spawns) << "}) sub " << head;
			}

			virtual const std::vector<Variable>& getUsedInputs(const Assignment& ass) const {
				return inputs;
			}

		};

		template<typename Context>
		TypedVariable<typename lattice<thread_list_analysis,analysis_config<Context>>::type>
		getThreadListVariable(CBA& cba) {
			return cba.getVar<Context>(ThreadList);
		}

		/**
		 * A factory function for a constrain linking the head of a thread to its spawn point(s).
		 */
		template<typename Context, typename ValueLattice, typename TmpAnalysisType>
		ConstraintPtr createSpawnPointConnectorConstraint(CBA& cba, const typename Context::thread_context& nestedContext, const TypedVariable<ValueLattice>& startPoint, const TmpAnalysisType& tmp) {
			return std::make_shared<SpawnPointConnector<ValueLattice,Context>>(cba, nestedContext, startPoint, getThreadListVariable<Context>(cba), [=](CBA& cba, const StatementInstance& stmt, const Context& ctxt) {
				return cba.getVar(tmp, stmt, ctxt);
			});
		}

		// an overload for one extra parameter
		template<typename Context, typename ValueLattice, typename TmpAnalysisType, typename Arg0>
		ConstraintPtr createSpawnPointConnectorConstraint(CBA& cba, const typename Context::thread_context& nestedContext, const TypedVariable<ValueLattice>& startPoint, const TmpAnalysisType& tmp, const Arg0& arg0) {
			return std::make_shared<SpawnPointConnector<ValueLattice,Context>>(cba, nestedContext, startPoint, getThreadListVariable<Context>(cba), [=](CBA& cba, const StatementInstance& stmt, const Context& ctxt) {
				return cba.getVar(tmp, stmt, ctxt, arg0);
			});
		}

		// an overload for two extra parameter
		template<typename Context, typename ValueLattice, typename TmpAnalysisType, typename Arg0, typename Arg1>
		ConstraintPtr createSpawnPointConnectorConstraint(CBA& cba, const typename Context::thread_context& nestedContext, const TypedVariable<ValueLattice>& startPoint, const TmpAnalysisType& tmp, const Arg0& arg0, const Arg1& arg1) {
			return std::make_shared<SpawnPointConnector<ValueLattice,Context>>(cba, nestedContext, startPoint, getThreadListVariable<Context>(cba), [=](CBA& cba, const StatementInstance& stmt, const Context& ctxt) {
				return cba.getVar(tmp, stmt, ctxt, arg0, arg1);
			});
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

