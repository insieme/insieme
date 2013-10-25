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

#include "insieme/analysis/cba/framework/constraint_generator.h"
#include "insieme/analysis/cba/framework/analysis_type.h"

#include "insieme/analysis/cba/framework/analysis.h"
#include "insieme/analysis/cba/framework/cba.h"

#include "insieme/analysis/cba/analysis/reachability.h"
#include "insieme/analysis/cba/analysis/callables.h"
#include "insieme/analysis/cba/analysis/references.h"
#include "insieme/analysis/cba/analysis/functions.h"
#include "insieme/analysis/cba/analysis/call_context.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/lang/basic.h"

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

	}


	template<
		typename T,
		typename AnalysisType,
		typename Context
	>
	class BasicDataFlowConstraintGenerator : public ConstraintGenerator<Context> {

		typedef ConstraintGenerator<Context> super;


		typedef typename AnalysisType::lattice_type lattice_type;
		typedef typename lattice_type::manager_type mgr_type;

	public:

		typedef typename lattice_type::base_lattice::value_type base_value_type;
		typedef typename lattice_type::value_type value_type;

	private:

		// the two set types to deal with
		const AnalysisType& A;		// the value set (labels -> values)
		const AnalysisType& a;		// the variable set (variables -> values)

		CBA& cba;

		// the manager/factory instance for handling data value instances
		mgr_type valueMgr;

	public:

		BasicDataFlowConstraintGenerator(CBA& cba, const AnalysisType& A, const AnalysisType& a)
			: super(cba), A(A), a(a), cba(cba) { };

		mgr_type& getValueManager() {
			return valueMgr;
		}

		template<typename V>
		value_type atomic(const V& set) {
			return valueMgr.atomic(set);
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
			// nothing to do by default => should be overloaded by sub-classes
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
				addConstraints(def, ctxt, constraints);
				return;
			}

			// ok - this is the definition point
			// => check type of variable (determined by parent)

			// no parent: free variable, nothing to do
			if (def.isRoot()) return;

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
									auto C_fun_bind_call = cba.getSet(C<Context>(), l_fun_bind_call, bindCallCtxt);

									// add constraints for all potential contexts the closure could be created
									for(const auto& bindCtxt : cba.getValidContexts<Context>()) {

										// get value of argument within bind context
										auto A_bind_arg = cba.getSet(A, l_arg, bindCtxt);

										// add constraint:
										//   [b,bindCtxt] \in C[bind_call,call_ctxt] => A[arg,bindCtxt] \sub a[param,ctxt]
										constraints.add(subsetIf(Callable<Context>(bind, bindCtxt), C_fun_bind_call, A_bind_arg, a_var));
									}
								}


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
						auto R_trg = this->cba.getSet(R<Context>(), l_trg, ctxt);
						for(auto loc : this->cba.template getLocations<Context>()) {

							// if loc is in R(target) then add Sin[A,trg] to A[call]
							auto S_in = this->cba.getSet(Sin, l_call, ctxt, loc, A);
							constraints.add(subsetIf(loc, R_trg, S_in, A_call));
						}
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
			return packer<value_type,fun_type,mgr_type>(fun, valueMgr);
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


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
