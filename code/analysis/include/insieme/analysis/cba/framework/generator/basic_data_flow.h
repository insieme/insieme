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
#include "insieme/analysis/cba/framework/set_type.h"

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

#include "insieme/utils/set_constraint/solver2.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * TODO: document
	 * 	  - it is assumed that the default constructed value type is the "unknown" value
	 */

	using namespace core;
	using namespace insieme::utils::set_constraint_2;

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


	template<typename T, typename SetType, typename Context>
	class BasicDataFlowConstraintGenerator : public ConstraintGenerator<Context> {


		typedef ConstraintGenerator<Context> super;

	protected:

		// the two set types to deal with
		const SetType& A;		// the value set (labels -> values)
		const SetType& a;		// the variable set (variables -> values)

		CBA& cba;

	public:

		BasicDataFlowConstraintGenerator(CBA& cba, const SetType& A, const SetType& a)
			: super(cba), A(A), a(a), cba(cba) { };

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

		void visitDeclarationStmt(const DeclarationStmtAddress& decl, const Context& ctxt, Constraints& constraints) {

			// there is nothing to do since a declaration stmt has no value

			// add constraint r(var) \subset C(init)
//				auto var = cba.getVariable(decl->getVariable());
//				auto l_init = cba.getLabel(decl->getInitialization());
//
//				// TODO: distinguish between control and data flow!
//				auto a_var = cba.getSet(a, var, ctxt);
//				auto A_init = cba.getSet(A, l_init, ctxt);
//				constraints.add(subset(A_init, a_var));		// TODO: add cba (passed by argument)
//
//				// finally, add constraints for init expression
//				visit(decl->getInitialization(), ctxt, constraints);
		}

		void visitIfStmt(const IfStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

//				// decent into sub-expressions
//				visit(stmt->getCondition(), ctxt, constraints);
//				visit(stmt->getThenBody(), ctxt, constraints);
//				visit(stmt->getElseBody(), ctxt, constraints);

		}

		void visitWhileStmt(const WhileStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

//				// decent into sub-expressions
//				visit(stmt->getCondition(), ctxt, constraints);
//				visit(stmt->getBody(), ctxt, constraints);
		}

		void visitReturnStmt(const ReturnStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

			// there is nothing to do since a return stmt has no value

//				// link the value of the result set to lambda body
//
//				// find lambda body
//				LambdaAddress lambda = getEnclosingLambda(stmt);
//				if (!lambda) {
//					std::cout << "Encountered free return!!\n";
//					return;		// return is not bound
//				}
//
//				// and add constraints for return value
////				visit(stmt->getReturnExpr(), ctxt, constraints);
//
//				auto l_retVal = cba.getLabel(stmt->getReturnExpr());
//				auto l_body = cba.getLabel(lambda->getBody());
//
//				auto A_retVal = cba.getSet(A, l_retVal, ctxt);
//				auto A_body = cba.getSet(A, l_body, ctxt);
//
//				// add constraint - forward in case end of return expression is reachable
//				auto R_ret = cba.getSet(Rout, l_retVal, ctxt);
//				constraints.add(subsetIf(Reachable(), R_ret, A_retVal, A_body));

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

					// obtain the set containing all the potential predecessor of the current call in the cba
					auto predecessor_ctxt = cba.getSet(pred, ctxt.callContext.back());

					// get containing callee (lambda or bind)
					Callee callee(parent.getParentAddress());
					assert(callee.isLambda() || callee.isBind());

					// get all callers
					const vector<Caller>& callers = cba.getCallSiteManager().getCaller(callee);

					// extract index of parameter within parameter list
					auto param_index = variable.getIndex();

					// connect all input parameters
					for(const Caller& cur : callers) {

						const auto& call = cur.getCall();
						auto l_call = cba.getLabel(call);
						auto l_cur_call_fun = cba.getLabel(call->getFunctionExpr());
						auto l_arg = cba.getLabel(call[param_index]);

						// ------------- handle bound values ------------

std::cout << "\n";
std::cout << "Variable: " << variable << "\n";
std::cout << "Call:         " << call << " = " << *call << "\n";
std::cout << "Context:      " << ctxt << "\n";

						// check whether call is a bind call and parameter is bound
						if (!call.isRoot() && call.getParentAddress().isa<BindExprAddress>()) {

							// obtain bind
							auto bind = call.getParentAddress().as<BindExprAddress>();
							assert_eq(call, bind->getCall());

							// check whether value is bound
							if (bind->isBoundExpression(call[param_index])) {
std::cout << " - it is a bound value!\n";
std::cout << " - valid contexts: " << cba.getValidContexts<Context>() << "\n";
								// consider all potential contexts of bind expression creating closure
								for (const auto& bindCtxt : cba.getValidContexts<Context>()) {

									// get value of function targeted by current call within given src ctxt
									auto C_cur_call = cba.getSet(C<Context>(), l_cur_call_fun, ctxt);

									// get value of parameter
									auto A_bind = cba.getSet(A, l_arg, bindCtxt);

									// forward parameter value
									assert(!callee.isBind()); // if this is violated => the following needs to be updated
									constraints.add(subsetIf(Callable<Context>(callee), C_cur_call, A_bind, a_var));
								}

								// done
								continue;
							}

						}

						// ----------- compute list of call contexts -----------

						// TODO: move computation of src-contexts to some shared location
						vector<Context> contexts;
						if (causesContextShift(call)) {

							// check whether current context is ending with current call
							// => otherwise this point isn't even reachable
							if (ctxt.callContext.back() != l_call) continue;

							// test all potential contexts
							for(auto l : cba.getDynamicCallLabels()) {
								Context callCtxt = ctxt;
								callCtxt.callContext >>= l;

								// check whether it is valid, if so, take it
								if (cba.isValid(callCtxt)) {
									contexts.push_back(callCtxt);
								}
							}

						} else {
							// we will just stay within the current context
							contexts.push_back(ctxt);
						}

std::cout << "Src-Contexts: " << contexts << "\n";


						// ----------- handle parameter passing ------------

						// handle target being a function
						if (callee.isLambda()) {

							// link all potential caller contexts to parameter
							for (const auto& callCtxt : contexts) {

								// get value of function targeted by current call within given src ctxt
								auto C_cur_call = cba.getSet(C<Context>(), l_cur_call_fun, callCtxt);

								// get value of parameter
								auto A_arg = cba.getSet(A, l_arg, callCtxt);

								// forward parameter value
								if (ctxt.callContext.back() != 0) {
									constraints.add(subsetIf(callCtxt.callContext.back(), predecessor_ctxt, Callable<Context>(callee), C_cur_call, A_arg, a_var));
								} else {
									constraints.add(subsetIf(Callable<Context>(callee), C_cur_call, A_arg, a_var));
								}

							}

						} else {
							assert(callee.isBind());

							// link all caller contexts with all contexts of the bind
							for (const auto& callCtxt : contexts) {

								// get value of function targeted by current call within given src ctxt
								auto C_cur_call = cba.getSet(C<Context>(), l_cur_call_fun, callCtxt);

								// get value of parameter
								auto A_arg = cba.getSet(A, l_arg, callCtxt);

								// get contexts of bind expression creating closure
								for (const auto& bindCtxt : cba.getValidContexts<Context>()) {

									// forward parameter value
									if (ctxt.callContext.back() != 0) {
										constraints.add(subsetIf(callCtxt.callContext.back(), predecessor_ctxt, Callable<Context>(callee, bindCtxt), C_cur_call, A_arg, a_var));
									} else {
										constraints.add(subsetIf(Callable<Context>(callee, bindCtxt), C_cur_call, A_arg, a_var));
									}

								}

							}
						}

					}


//					// obtain the set containing all the potential predecessor of the current call in the cba
////					auto predecessor_ctxt = cba.getSet(pred, ctxt.callContext.back());
//std::cout << "Handling parameter: " << *variable << "\n";
//					// distinguish two cases: parameter of a lambda or parameter of a bind
//					if (auto lambda = parent.getParentAddress().isa<LambdaAddress>()) {
//						// deal with a lambda parameter
//std::cout << " - it is a lambda parameter\n";
//						assert_lt(5, parent.getDepth());
//						auto lambdaExpr = parent.getParentAddress(4);
//						auto user = parent.getParentAddress(5);
//
//						auto call = user.isa<CallExprAddress>();
//						if (call && call->getFunctionExpr() == lambdaExpr) {
//std::cout << " - it is a direct call\n";
//							// TODO: consider case in which argument is bound value within a bind!
//
//							// get label of argument
//							auto l_arg = cba.getLabel(call[variable.getIndex()]);
//
//							// check whether use is a call within a bind
//							if (!call.isRoot() && call.getParentNode().isa<BindExprPtr>()) {
//
//								// check whether parameter receives a bound expression
//								auto bind = call.getParentAddress().as<BindExprAddress>();
//								if (bind.isBoundExpression(call[variable.getIndex()])) {
//
//									// check whether cba needs to be switched
//									// (bind is not created for a direct use)
//									if (!bind.isRoot()
//											&& (!bind.getParentNode().isa<CallExprPtr>() ||
//													bind.getParentAddress().as<CallExprAddress>()->getFunctionExpr() != bind)) {
//
//											for (auto l : cba.getDynamicCallLabels()) {
//
//												// nobody calls context 0
//												if (l != 0 && ctxt.callContext.startsWith(0)) continue;
//
//												// get call-site of current context
//												auto l_cur_call = ctxt.callContext.back();
//												auto l_cur_call_fun = cba.getLabel(cba.getStmt(l_cur_call).template as<CallExprAddress>()->getFunctionExpr());
//												Context curCallCtxt = ctxt;
//												curCallCtxt.callContext >>= l;
//												auto C_cur_call = cba.getSet(C<Context>(), l_cur_call_fun, curCallCtxt);
//
//												// load bound values of all potential contexts
//												for(const Context& cur : cba.getValidContexts<Context>()) {
//
//													// get bound value in this context
//													auto A_arg = cba.getSet(A, l_arg, cur);
//
//													// access parameter in src-ctxt if src-context is actually a potental predecessor
//													constraints.add(subsetIf(curCallCtxt.callContext.back(), predecessor_ctxt, Callable<Context>(bind, cur), C_cur_call, A_arg, a_var));
//												}
//
//											}
//
//											// done
//											return;
//									}
//								}
//							}
//
//							// ---- standard lambda call -----
//
//							// this is a direct call to the function => no context switch
//							auto A_arg = cba.getSet(A, l_arg, ctxt);
//
//							// pass value of argument to parameter
//							constraints.add(subset(A_arg, a_var));
//
//						} else {
//
//							// TODO: limit call-contexts to actual possible once
//std::cout << " - it is a indirect call\n";
//							// this function might be called indirectly => link in all potential call sites
//							Callee callee(lambda);
//							std::cout << "Potential call-sites: " << cba.getCallSiteManager().getCaller(callee) << "\n";
//							for(const CallExprAddress& site : cba.getCallSiteManager().getCaller(callee)) {
//
//								// filter out incorrect number of parameters
//								auto l_site = cba.getLabel(site);
//								if (!ctxt.callContext.endsWith(l_site)) continue;
//
//								for(const auto& l : cba.getDynamicCallLabels()) {
//
//									// nobody calls context 0
//									if (l != 0 && ctxt.callContext.startsWith(0)) continue;
//
//									// compute potential caller context
//									Context srcCtxt = ctxt;
//									srcCtxt.callContext >>= l;
//
//									// get value of argument
//									auto A_arg = cba.getSet(A, cba.getLabel(site[variable.getIndex()]), srcCtxt);
//									auto C_fun = cba.getSet(C<Context>(), cba.getLabel(site->getFunctionExpr()), srcCtxt);
//
//									// add constraint for function call
//									constraints.add(subsetIf(srcCtxt.callContext.back(), predecessor_ctxt, Callable<Context>(callee), C_fun, A_arg, a_var));
//								}
//							}
//						}
//
//					} else {
//
//						// deal with a bind parameter
//						assert(parent.getParentNode().isa<BindExprPtr>());
//						auto bind = parent.getParentAddress().as<BindExprAddress>();
//
//						// link variable value with call-site value
//						if (bind.isRoot()) return;
//
//						auto user = bind.getParentAddress();
//						auto call = user.isa<CallExprAddress>();
//						if (call && call->getFunctionExpr() == bind) {
//
//							// direct call - no context switch
//							auto l_arg = cba.getLabel(call[variable.getIndex()]);
//							auto A_arg = cba.getSet(A, l_arg, ctxt);
//
//							// pass value of argument to parameter
//							constraints.add(subset(A_arg, a_var));
//
//						} else {
//
//							// indirect call - context switch
//
//							// this bind might be called indirectly => link in all potential call sites
//							Callee callee(bind);
//							for(const CallExprAddress& site : cba.getCallSiteManager().getCaller(bind)) {
//
//								auto l_site = cba.getLabel(site);
//								if (!ctxt.callContext.endsWith(l_site)) continue;
//
//								for(const auto& l : cba.getDynamicCallLabels()) {
//
//									// nobody calls context 0
//									if (l != 0 && ctxt.callContext.startsWith(0)) continue;
//
//									// compute potential caller context
//									Context srcCtxt = ctxt;
//									srcCtxt.callContext >>= l;
//
//									// get value of argument
//									auto A_arg = cba.getSet(A, cba.getLabel(site[variable.getIndex()]), srcCtxt);
//									auto C_fun = cba.getSet(C<Context>(), cba.getLabel(site->getFunctionExpr()), srcCtxt);
//
//									// pass value of argument to parameter for any potential callable
//									for(const Context& ctxt : cba.getValidContexts<Context>()) {
//										constraints.add(subsetIf(srcCtxt.callContext.back(), predecessor_ctxt, Callable<Context>(bind, ctxt), C_fun, A_arg, a_var));
//									}
//
////									for(const Callable<Context>& target : cba.getCallables<Context>(num_args)) {
////										if (target.getDefinition() != bind) continue;
////										constraints.add(subsetIf(srcCtxt.callContext.back(), predecessor_ctxt, target, C_fun, A_arg, a_var));
////									}
//								}
//							}
//						}
//
//					}

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

//				// process bound arguments recursively
//				for (auto cur : bind->getBoundExpressions()) {
//					visit(cur, ctxt, constraints);
//				}

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

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
