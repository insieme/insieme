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

#include "insieme/analysis/cba/prototype.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * For the docu:
	 * 		- the constraint generation is lazy
	 * 		- it is based on: target is listing all sources (bringschuld)
	 */

	using std::set;
	using std::vector;

	using namespace core;

	VariableAddress getDefinitionPoint(const VariableAddress& varAddress) {

		// extract the variable
		VariablePtr var = varAddress.getAddressedNode();

		// start walking up the address
		NodeAddress cur = varAddress;

		// check the parent
		while (!cur.isRoot()) {
			auto pos = cur.getIndex();
			cur = cur.getParentAddress();
			switch(cur->getNodeType()) {

			case NT_Parameters: {
				return varAddress;	// this variable is a parameter definition
			}

			case NT_Lambda: {

				// check parameters
				for(auto param : cur.as<LambdaAddress>()->getParameters()) {
					if (param.as<VariablePtr>() == var) {
						return param;		// found it
					}
				}

				// otherwise continue with parent
				break;
			}

			case NT_LambdaBinding: {
				// check the bound variable
				auto boundVar = cur.as<LambdaBindingAddress>()->getVariable();
				if (boundVar.as<VariablePtr>() == var) {
					return boundVar;
				}

				// keep on searching
				break;
			}

			case NT_BindExpr: {
				// check parameters
				for(auto param : cur.as<BindExprAddress>()->getParameters()) {
					if (param.as<VariablePtr>() == var) {
						return param;		// found it
					}
				}

				// not here
				break;
			}

			case NT_CompoundStmt: {

				// check whether there is an earlier declaration
				auto compound = cur.as<CompoundStmtAddress>();
				for(int i = pos; i >= 0; i--) {
					if (auto decl = compound[i].isa<DeclarationStmtAddress>()) {
						if (decl->getVariable().as<VariablePtr>() == var) {
							return decl->getVariable();
						}
					}
				}

				// otherwise continue with parent
				break;
			}

			default: break;
			}
		}

		// the variable is a free variable in this context
		return VariableAddress(var);
	}


	bool isMemoryConstructor(const StatementAddress& address) {
		StatementPtr stmt = address;

		// literals of a reference type are memory locations
		if (auto lit = stmt.isa<LiteralPtr>()) {
			return lit->getType().isa<RefTypePtr>();
		}

		// memory allocation calls are
		return core::analysis::isCallOf(stmt, stmt->getNodeManager().getLangBasic().getRefAlloc());
	}

	ExpressionAddress getLocationDefinitionPoint(const core::StatementAddress& stmt) {
		assert(isMemoryConstructor(stmt));

		// globals are globals => always the same
		if (auto lit = stmt.isa<LiteralPtr>()) {
			return LiteralAddress(lit);
		}

		// locations created by ref.alloc calls are created at the call side
		assert(stmt.isa<CallExprAddress>());
		return stmt.as<CallExprAddress>();
	}


	namespace {

		LambdaAddress getEnclosingLambda(const NodeAddress& addr) {
			// find lambda body
			NodeAddress cur = addr;
			while(!cur.isRoot() && !cur.isa<LambdaPtr>()) {
				cur = cur.getParentAddress();
			}
			return cur.isa<LambdaAddress>();
		}

		int getParameterIndex(const ParametersPtr& params, const ExpressionPtr& expr) {
			// must be a variable
			if (!expr.isa<VariablePtr>()) return -1;

			// search for it
			for(int i = 0; i<(int)params.size(); i++) {
				if (*(params[i]) == *expr) return i;
			}

			// not found
			return -1;
		}

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


		ExpressionAddress getSurroundingFreeFunction(const NodeAddress& cur) {
			static const ExpressionAddress none;

			// move up until reaching requested function
			if (cur.isRoot()) return none; // there is none

			// stop decent at lambda or binds
			auto type = cur->getNodeType();
			if (type != NT_LambdaExpr && type != NT_BindExpr) {
				return getSurroundingFreeFunction(cur.getParentAddress());
			}

			auto fun = cur.as<ExpressionAddress>();

			// check whether function is a free function
			if (fun.isRoot()) return none;

			// if lambda is not directly called it is a free function
			auto user = fun.getParentAddress();
			auto call = user.isa<CallExprAddress>();
			if (!call || call->getFunctionExpr() != fun) return fun;

			// otherwise continue search
			return getSurroundingFreeFunction(user);
		}

		using namespace utils::set_constraint_2;

		template<typename T, int pos, int size, typename Filter>
		struct gen_context {
			void operator()(const vector<T>& values, vector<Sequence<T,size>>& res, const Filter& f, array<T,size>& data) const {
				static const gen_context<T,pos-1,size,Filter> inner;
				for(auto cur : values) {
					data[pos-1] = cur;
					inner(values, res, f, data);
				}
			}
		};

		template<typename T, int size, typename Filter>
		struct gen_context<T, 0,size, Filter> {
			void operator()(const vector<T>& values, vector<Sequence<T,size>>& res, const Filter& f, array<T,size>& data) const {
				if (f(data)) res.push_back(data);
			}
		};


		template<typename T, unsigned s, typename Filter>
		void generateSequences(const vector<T>& values, vector<Sequence<T, s>>& res, const Filter& f) {
			array<T,s> tmp;
			gen_context<T, s, s, Filter>()(values, res, f, tmp);
		}

		template<typename T, unsigned s>
		void generateSequences(const vector<T>& values, vector<Sequence<T, s>>& res) {
			return generateSequences(values, res, [](const array<T,s>& list) { return true; });
		}


		vector<Callable> getAllCallableTerms(CBA& context, const StatementAddress& root) {

			// compute list of all potential call-contexts
			vector<Label> labels;
			labels.push_back(0);		// default context
			visitDepthFirst(root, [&](const CallExprAddress& cur) {
				auto call = cur.getAddressedNode();
				auto fun = call->getFunctionExpr();

				// we can skip calls to literals
				if (fun->getNodeType() == NT_Literal) return;

				// we can also skip directly called stuff
				if (fun->getNodeType() == NT_LambdaExpr) return;
				if (fun->getNodeType() == NT_BindExpr) return;

				// this is a potential call-site creating a new context
				labels.push_back(context.getLabel(cur));
			});

			vector<Context::CallContext> callContexts;
			generateSequences(labels, callContexts, [&](const Context::CallContext& cur) {
				return context.isValid(cur);
			});

			// compute resulting set
			vector<Callable> res;

			// TODO: collect potential thread contexts
//			vector<ThreadID> threads;
//			threads.push_back(ThreadID());		// default thread
//
//			// create all thread contexts
			vector<Context::ThreadContext> threadContexts;
//			generateSequences(threads, threadContexts);
			threadContexts.push_back(Context::ThreadContext());

			// collect all terms in the code
			visitDepthFirst(root, [&](const ExpressionAddress& cur) {

				// only interested in lambdas and binds
				if (!(cur.isa<LambdaExprPtr>() || cur.isa<BindExprPtr>())) return;

				// must not be root
				if (cur.isRoot()) return;

				// it must not be the target of a call expression
				auto parent = cur.getParentAddress();
				if (auto call = parent.isa<CallExprAddress>()) {
					if (call->getFunctionExpr() == cur) {
						return;
					}
				}

				// TODO: also add all recursion variations
				if (auto lambda = cur.isa<LambdaExprAddress>()) {

					// lambdas do not need a context
					res.push_back(Callable(lambda));

				} else if (auto bind = cur.isa<BindExprAddress>()) {
					// binds do
					for(auto& callContext : callContexts) {
						for(auto& threadContext : threadContexts) {
							// TODO: add thread contexts
							res.push_back(Callable(bind, Context(callContext, threadContext)));
						}
					}

				} else {
					assert(false && "How did you get here?");
				}
			});


//			std::cout << "Sites:                " << labels << "\n";
//			std::cout << "Number of call sites:      " << labels.size() << "\n";
//			std::cout << "Number of call contexts:   " << labels.size()*labels.size() << " limited to " << callContexts.size() << "\n";
//			std::cout << "Number of threads:         " << threads.size() << "\n";
//			std::cout << "Number of thread contexts: " << threads.size()*threads.size() << " limited to " << threadContexts.size() << "\n";
//			std::cout << "Total number of contexts:  " << callContexts.size() * threadContexts.size() << "\n";
//			std::cout << "Total number of callables: " << res.size() << "\n";
//			std::cout << "Contexts:\n" << join("\n", contexts) << "\n\n";

			return res;
		}

		template<typename T>
		class BasicDataFlowConstraintCollector : public ConstraintResolver {

			typedef ConstraintResolver super;

		protected:

			// the two set types to deal with
			const TypedSetType<T>& A;		// the value set (labels -> values)
			const TypedSetType<T>& a;		// the variable set (variables -> values)

		public:

			BasicDataFlowConstraintCollector(CBA& context, const TypedSetType<T>& A, const TypedSetType<T>& a)
				: super(context), A(A), a(a) { };

			void visitCompoundStmt(const CompoundStmtAddress& compound, const Context& ctxt, Constraints& constraints) {

				// only interested in lambda bodies
				if (compound.isRoot()) return;
				if (compound.getParentNode()->getNodeType() != NT_Lambda) return;

				// TODO: identify return statements more efficiently

				auto l_body = context.getLabel(compound);
				auto A_body = context.getSet(A, l_body, ctxt);

				// since value of a compound is the value of return statements => visit those
				visitDepthFirstPrunable(compound, [&](const StatementAddress& stmt) {
					// prune inner functions
					if (stmt.isa<LambdaExprAddress>()) return true;

					// visit return statements
					if (auto returnStmt = stmt.isa<ReturnStmtAddress>()) {

						// connect value of return statement with body value
						auto l_return = context.getLabel(returnStmt->getReturnExpr());
						auto A_return = context.getSet(A, l_return, ctxt);

						// add constraint - forward in case end of return expression is reachable
						auto R_ret = context.getSet(Rout, l_return, ctxt);
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
//				auto var = context.getVariable(decl->getVariable());
//				auto l_init = context.getLabel(decl->getInitialization());
//
//				// TODO: distinguish between control and data flow!
//				auto a_var = context.getSet(a, var, ctxt);
//				auto A_init = context.getSet(A, l_init, ctxt);
//				constraints.add(subset(A_init, a_var));		// TODO: add context (passed by argument)
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
//				auto l_retVal = context.getLabel(stmt->getReturnExpr());
//				auto l_body = context.getLabel(lambda->getBody());
//
//				auto A_retVal = context.getSet(A, l_retVal, ctxt);
//				auto A_body = context.getSet(A, l_body, ctxt);
//
//				// add constraint - forward in case end of return expression is reachable
//				auto R_ret = context.getSet(Rout, l_retVal, ctxt);
//				constraints.add(subsetIf(Reachable(), R_ret, A_retVal, A_body));

			}

			void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {
				// nothing to do by default => should be overloaded by sub-classes
			}

			void visitVariable(const VariableAddress& variable, const Context& ctxt, Constraints& constraints) {

				// ----- Part I: read variable value -------

				// add constraint a(var) \subset A(var)
				auto var = context.getVariable(variable);
				auto l_var = context.getLabel(variable);

				auto a_var = context.getSet(a, var, ctxt);
				auto A_var = context.getSet(A, l_var, ctxt);

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
						// TODO: consider for-loops

						auto decl = parent.as<DeclarationStmtAddress>();

						// add constraint r(var) \subset C(init)
						auto l_init = context.getLabel(decl->getInitialization());

						// TODO: distinguish between control and data flow!
						auto A_init = context.getSet(A, l_init, ctxt);
						constraints.add(subset(A_init, a_var));		// TODO: add context (passed by argument)

						// finally, add constraints for init expression
//						visit(decl->getInitialization(), ctxt, constraints);

						break;
					}

					// the variable may be a parameter of a lambda or bind
					case NT_Parameters: {

						// this should not be the end
						assert(!parent.isRoot());

						// obtain the set containing all the potential predecessor of the current call in the context
						auto predecessor_ctxt = context.getSet(pred, ctxt.callContext.back());

						// distinguish two cases: parameter of a lambda or parameter of a bind
						if (parent.getParentNode().isa<LambdaPtr>()) {

							// deal with a lambda parameter

							assert_lt(5, parent.getDepth());
							auto lambda = parent.getParentAddress(4);
							auto user = parent.getParentAddress(5);

							auto call = user.isa<CallExprAddress>();
							if (call && call->getFunctionExpr() == lambda) {

								// TODO: consider case in which argument is bound value within a bind!

								// get label of argument
								auto l_arg = context.getLabel(call[variable.getIndex()]);

								// check whether use is a call within a bind
								if (!call.isRoot() && call.getParentNode().isa<BindExprPtr>()) {

									// check whether parameter receives a bound expression
									auto bind = call.getParentAddress().as<BindExprAddress>();
									if (bind.isBoundExpression(call[variable.getIndex()])) {

										// check whether context needs to be switched
										// (bind is not created for a direct use)
										if (!bind.isRoot()
												&& (!bind.getParentNode().isa<CallExprPtr>() ||
														bind.getParentAddress().as<CallExprAddress>()->getFunctionExpr() != bind)) {

												for (auto l : context.getDynamicCallLabels()) {

													// nobody calls context 0
													if (l != 0 && ctxt.callContext.startsWith(0)) continue;

													// get call-site of current context
													auto l_cur_call = ctxt.callContext.back();
													auto l_cur_call_fun = context.getLabel(context.getStmt(l_cur_call).as<CallExprAddress>()->getFunctionExpr());
													Context curCallCtxt = ctxt;
													curCallCtxt.callContext >>= l;
													auto C_cur_call = context.getSet(C, l_cur_call_fun, curCallCtxt);

													// load bound values of all potential contexts
													for(const auto& cur : context.getCallables()) {
														// only interested in this bind
														if (cur.definition != bind) continue;

														// get bound value in this context
														auto A_arg = context.getSet(A, l_arg, cur.context);

														// access parameter in src-ctxt if src-context is actually a potental predecessor
														constraints.add(subsetIf(curCallCtxt.callContext.back(), predecessor_ctxt, cur, C_cur_call, A_arg, a_var));
													}
												}

												// done
												return;
										}
									}
								}

								// ---- standard lambda call -----

								// this is a direct call to the function => no context switch
								auto A_arg = context.getSet(A, l_arg, ctxt);

								// pass value of argument to parameter
								constraints.add(subset(A_arg, a_var));

							} else {

								// TODO: limit call-contexts to actual possible once

								// this function might be called indirectly => link in all potential call sites
								auto num_args = parent.as<ParametersPtr>().size();
								for(const auto& site : context.getDynamicCalls()) {
									// filter out incorrect number of parameters
									if (site.size() != num_args) continue;

									auto l_site = context.getLabel(site);
									if (!ctxt.callContext.endsWith(l_site)) continue;

									for(const auto& l : context.getDynamicCallLabels()) {

										// nobody calls context 0
										if (l != 0 && ctxt.callContext.startsWith(0)) continue;

										// compute potential caller context
										Context srcCtxt = ctxt;
										srcCtxt.callContext >>= l;

										// get value of argument
										auto A_arg = context.getSet(A, context.getLabel(site[variable.getIndex()]), srcCtxt);
										auto C_fun = context.getSet(C, context.getLabel(site->getFunctionExpr()), srcCtxt);

										// pass value of argument to parameter for any potential callable
										for(const auto& target : context.getCallables()) {
											if (target.definition != lambda) continue;
											constraints.add(subsetIf(srcCtxt.callContext.back(), predecessor_ctxt, target, C_fun, A_arg, a_var));
										}
									}
								}
							}

						} else {

							// deal with a bind parameter
							assert(parent.getParentNode().isa<BindExprPtr>());
							auto bind = parent.getParentAddress().as<BindExprAddress>();

							// link variable value with call-site value
							if (bind.isRoot()) return;

							auto user = bind.getParentAddress();
							auto call = user.isa<CallExprAddress>();
							if (call && call->getFunctionExpr() == bind) {

								// direct call - no context switch
								auto l_arg = context.getLabel(call[variable.getIndex()]);
								auto A_arg = context.getSet(A, l_arg, ctxt);

								// pass value of argument to parameter
								constraints.add(subset(A_arg, a_var));

							} else {

								// indirect call - context switch

								// this bind might be called indirectly => link in all potential call sites
								auto num_args = parent.as<ParametersPtr>().size();
								for(const auto& site : context.getDynamicCalls()) {
									// filter out incorrect number of parameters
									if (site.size() != num_args) continue;

									auto l_site = context.getLabel(site);
									if (!ctxt.callContext.endsWith(l_site)) continue;

									for(const auto& l : context.getDynamicCallLabels()) {

										// nobody calls context 0
										if (l != 0 && ctxt.callContext.startsWith(0)) continue;

										// compute potential caller context
										Context srcCtxt = ctxt;
										srcCtxt.callContext >>= l;

										// get value of argument
										auto A_arg = context.getSet(A, context.getLabel(site[variable.getIndex()]), srcCtxt);
										auto C_fun = context.getSet(C, context.getLabel(site->getFunctionExpr()), srcCtxt);

										// pass value of argument to parameter for any potential callable
										for(const auto& target : context.getCallables()) {
											if (target.definition != bind) continue;
											constraints.add(subsetIf(srcCtxt.callContext.back(), predecessor_ctxt, target, C_fun, A_arg, a_var));
										}
									}
								}
							}

						}

						// this should be it
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
				auto l_call = context.getLabel(call);
				auto A_call = context.getSet(A, l_call, ctxt);

				// target may be a literal
				if (fun->getNodeType() == NT_Literal) {

					// constraints for literals ...
					const auto& base = call->getNodeManager().getLangBasic();

					// one special case: if it is a read operation
					if (base.isRefDeref(fun)) {
						// read value from memory location
						auto l_trg = this->context.getLabel(call[0]);
						auto R_trg = this->context.getSet(R, l_trg, ctxt);
						for(auto loc : this->context.getLocations()) {

							// if loc is in R(target) then add Sin[A,trg] to A[call]
							auto S_in = this->context.getSet(Sin, l_call, ctxt, loc, A);
							constraints.add(subsetIf(loc, R_trg, S_in, A_call));
						}
					}

					// no other literals supported by default - overloads may add more
					return;
				}


				// target may be a lambda (direct call)
				if (auto lambda = fun.isa<LambdaExprAddress>()) {

					// take the value of the body (no context switch for direct call)
					auto l_body = context.getLabel(lambda->getBody());
					auto A_body = context.getSet(A, l_body, ctxt);

					// take over value of function body
					constraints.add(subset(A_body, A_call));
					return;
				}

				// target may be a bind (direct call)
				if (auto bind = fun.isa<BindExprAddress>()) {

					// take the value of the body (no context switch for direct call)
					auto l_body = context.getLabel(bind->getCall());
					auto A_body = context.getSet(A, l_body, ctxt);

					// take over value of function body
					constraints.add(subset(A_body, A_call));
					return;
				}

				// target may be an indirect call => check out all callables
				Context innerCtxt = ctxt;
				innerCtxt.callContext <<= l_call;

				// NOTE: - Optimization - we only need to know the body, not the context it was created in (for binds)
				//  => we can iterate through the list of free functions, not the callables

				auto l_fun = context.getLabel(fun);
				auto F_fun = context.getSet(F, l_fun, ctxt);


				auto num_args = call.size();
				for(const auto& cur : context.getContextFreeCallableCandidate(call)) {

					// check proper number of arguments
					TypeAddress type = cur->getType();
					FunctionTypePtr funType = type.as<FunctionTypePtr>();
					auto num_params = funType->getParameterTypes().size();
					if (num_args != num_params) continue;

					// connect target body with result value
					auto l_body = context.getLabel(getBody(cur));
					auto A_body = context.getSet(A, l_body, innerCtxt);
					constraints.add(subsetIf(cur, F_fun, A_body, A_call));
				}

			}

			void visitNode(const NodeAddress& node, const Context& ctxt, Constraints& constraints) {
				std::cout << "Reached unsupported Node Type: " << node->getNodeType() << "\n";
				assert(false);
			}

		};


		class ControlFlowConstraintCollector : public BasicDataFlowConstraintCollector<Callable> {

			typedef BasicDataFlowConstraintCollector<Callable> super;

		public:

			ControlFlowConstraintCollector(CBA& context)
				: super(context, C, c) { };

			void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitLiteral(literal, ctxt, constraints);

				// only interested in functions ...
				if (!literal->getType().isa<FunctionTypePtr>()) return;

				// add constraint: literal \in C(lit)
				auto value = Callable(literal);
				auto l_lit = context.getLabel(literal);

				auto C_lit = context.getSet(C, l_lit, ctxt);
				constraints.add(elem(value, C_lit));

			}

			void visitLambdaExpr(const LambdaExprAddress& lambda, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitLambdaExpr(lambda, ctxt, constraints);

				// add constraint: lambda \in C(lambda)
				auto value = Callable(lambda);
				auto label = context.getLabel(lambda);

				constraints.add(elem(value, context.getSet(C, label, ctxt)));

				// TODO: handle recursions

			}

			void visitBindExpr(const BindExprAddress& bind, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitBindExpr(bind, ctxt, constraints);

				// add constraint: bind \in C(bind)
				auto value = Callable(bind, ctxt);
				auto label = context.getLabel(bind);

				auto C_bind = context.getSet(C, label, ctxt);
				constraints.add(elem(value, C_bind));

			}

		};

		class FunctionConstraintCollector : public BasicDataFlowConstraintCollector<ContextFreeCallable> {

			typedef BasicDataFlowConstraintCollector<ContextFreeCallable> super;

		public:

			FunctionConstraintCollector(CBA& context)
				: super(context, F, f) { };

			void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitLiteral(literal, ctxt, constraints);

				// only interested in functions ...
				if (!literal->getType().isa<FunctionTypePtr>()) return;

				// add constraint: literal \in C(lit)
				auto value = literal.as<ExpressionAddress>();
				auto l_lit = context.getLabel(literal);

				auto F_lit = context.getSet(F, l_lit, ctxt);
				constraints.add(elem(value, F_lit));

			}

			void visitLambdaExpr(const LambdaExprAddress& lambda, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitLambdaExpr(lambda, ctxt, constraints);

				// add constraint: lambda \in C(lambda)
				auto value = lambda.as<ExpressionAddress>();
				auto label = context.getLabel(lambda);

				constraints.add(elem(value, context.getSet(F, label, ctxt)));

				// TODO: handle recursions

			}

			void visitBindExpr(const BindExprAddress& bind, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitBindExpr(bind, ctxt, constraints);

				// add constraint: bind \in C(bind)
				auto value = bind.as<ExpressionAddress>();
				auto label = context.getLabel(bind);

				auto F_bind = context.getSet(F, label, ctxt);
				constraints.add(elem(value, F_bind));

			}

		};

		class ConstantConstraintCollector : public BasicDataFlowConstraintCollector<core::ExpressionPtr> {

			typedef BasicDataFlowConstraintCollector<core::ExpressionPtr> super;

		public:

			ConstantConstraintCollector(CBA& context)
				: super(context, D, d) { };

			void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitLiteral(literal, ctxt, constraints);

				// not interested in functions
				if (literal->getType().isa<FunctionTypePtr>()) return;

				// add constraint literal \in C(lit)
				auto value = literal.as<ExpressionPtr>();
				auto l_lit = context.getLabel(literal);

				auto D_lit = context.getSet(D, l_lit, ctxt);
				constraints.add(elem(value, D_lit));

			}

			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
				auto& base = call->getNodeManager().getLangBasic();

				// conduct std-procedure
				super::visitCallExpr(call, ctxt, constraints);

				// some special cases
				if (base.isIntArithOp(call->getFunctionExpr())) {

					// mark result as being unknown
					auto D_call = context.getSet(D, context.getLabel(call), ctxt);
					constraints.add(elem(ExpressionPtr(), D_call));

				}

			}

		};


		namespace {

			template<typename A, typename B, typename R>
			class total_binary_op {
				typedef std::function<R(const A&, const B&)> fun_type;
				fun_type fun;
			public:
				total_binary_op(const fun_type& fun) : fun(fun) {}

				R operator()(const A& a, const B& b) const {
					static const R fail;
					if (!a) return fail;
					if (!b) return fail;
					return fun(a,b);
				}
			};

			template<
				typename F,
				typename A = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg1_type>::type>::type,
				typename B = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg2_type>::type>::type,
				typename R = typename lambda_traits<F>::result_type
			>
			total_binary_op<A,B,R> total(const F& fun) {
				return total_binary_op<A,B,R>(fun);
			}

			template<typename A, typename B, typename R>
			class cartesion_product_binary_op {

				typedef std::function<R(const A&, const B&)> fun_type;
				fun_type fun;

			public:

				cartesion_product_binary_op(const fun_type& fun) : fun(fun) {}

				set<R> operator()(const set<A>& a, const set<B>& b) const {
					set<R> res;

					// if there is any undefined included => it is undefined
					if (any(a, [](const A& a)->bool { return !a; }) || any(b, [](const B& b)->bool { return !b; })) {
						res.insert(R());
						return res;
					}

					// compute the cross-product
					for(auto& x : a) {
						for (auto& y : b) {
							res.insert(fun(x,y));
						}
					}

					if (res.size() > 10) {
						// build a set only containing the unknown value
						set<R> res;
						res.insert(R());
						return res;
					}

					return res;
				}

			};

			template<
				typename F,
				typename A = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg1_type>::type>::type,
				typename B = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg2_type>::type>::type,
				typename R = typename lambda_traits<F>::result_type
			>
			cartesion_product_binary_op<A,B,R> cartesion_product(const F& fun) {
				return cartesion_product_binary_op<A,B,R>(fun);
			}


		}


		class ArithmeticConstraintCollector : public BasicDataFlowConstraintCollector<Formula> {

			typedef BasicDataFlowConstraintCollector<Formula> super;

			const core::lang::BasicGenerator& base;

		public:

			ArithmeticConstraintCollector(CBA& context)
				: super(context, cba::A, cba::a),
				  base(context.getRoot()->getNodeManager().getLangBasic())
			{ };

			void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitLiteral(literal, ctxt, constraints);

				// only interested in integer literals
				if (!base.isInt(literal->getType())) return;

				// add constraint literal \in A(lit)
				Formula value = core::arithmetic::toFormula(literal);
				auto l_lit = context.getLabel(literal);

				auto A_lit = context.getSet(A, l_lit, ctxt);
				constraints.add(elem(value, A_lit));

			}


			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
				static const Formula unknown;

				// conduct std-procedure
				super::visitCallExpr(call, ctxt, constraints);

				// only care for integer expressions calling literals
				if (!base.isInt(call->getType())) return;

				// check whether it is a literal => otherwise basic data flow is handling it
				auto fun = call->getFunctionExpr();
				if (!fun.isa<LiteralPtr>()) return;

				// get some labels / ids
				auto A_res = context.getSet(A, context.getLabel(call), ctxt);

				// handle unary literals
				if (call.size() == 1u) {
					if (base.isRefDeref(fun)) {
						return;		// has been handled by super!
					}
				}

				// and binary operators
				if (call.size() != 2u) {
					// this value is unknown
					constraints.add(elem(unknown, A_res));
					return;
				}

				// get sets for operators
				auto A_lhs = context.getSet(A, context.getLabel(call[0]), ctxt);
				auto A_rhs = context.getSet(A, context.getLabel(call[1]), ctxt);

				// special handling for functions
				if (base.isSignedIntAdd(fun) || base.isUnsignedIntAdd(fun)) {
					constraints.add(subsetBinary(A_lhs, A_rhs, A_res, cartesion_product(total([](const Formula& a, const Formula& b)->Formula {
						return *a.formula + *b.formula;
					}))));
					return;
				}

				if (base.isSignedIntSub(fun) || base.isUnsignedIntSub(fun)) {
					constraints.add(subsetBinary(A_lhs, A_rhs, A_res, cartesion_product(total([](const Formula& a, const Formula& b)->Formula {
						return *a.formula - *b.formula;
					}))));
					return;
				}

				if (base.isSignedIntMul(fun) || base.isUnsignedIntMul(fun)) {
					constraints.add(subsetBinary(A_lhs, A_rhs, A_res, cartesion_product(total([](const Formula& a, const Formula& b)->Formula {
						return *a.formula * *b.formula;
					}))));
					return;
				}

				// otherwise it is unknown
				constraints.add(elem(unknown, A_res));
			}

		};

		namespace {

			template<
				typename F,
				typename A = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg1_type>::type>::type,
				typename B = typename std::remove_cv<typename std::remove_reference<typename lambda_traits<F>::arg2_type>::type>::type,
				typename R = typename lambda_traits<F>::result_type
			>
			struct pair_wise {
				F f;
				pair_wise(const F& f) : f(f) {}
				set<R> operator()(const set<A>& a, const set<B>& b) const {
					set<R> res;
					for(auto& x : a) {
						for(auto& y : b) {
							res.insert(f(x,y));
						}
					}
					return res;
				}
			};

			template<typename F>
			pair_wise<F> pairwise(const F& f) {
				return pair_wise<F>(f);
			}

			template<typename Comparator>
			std::function<set<bool>(const set<Formula>&,const set<Formula>&)> compareFormula(const Comparator& fun) {
				return [=](const set<Formula>& a, const set<Formula>& b)->set<bool> {
					static const set<bool> unknown({true, false});

					set<bool> res;

					// quick check
					for(auto& x : a) if (!x) return unknown;
					for(auto& x : b) if (!x) return unknown;

					// check out pairs
					bool containsTrue = false;
					bool containsFalse = false;
					for(auto& x : a) {
						for(auto& y : b) {
							if (containsTrue && containsFalse) {
								return res;
							}

							// pair< valid, unsatisfiable >
							pair<bool,bool> validity = fun(*x.formula, *y.formula);
							if (!containsTrue && !validity.second) {
								res.insert(true);
								containsTrue = true;
							}

							if (!containsFalse && !validity.first) {
								res.insert(false);
								containsFalse = true;
							}
						}
					}
					return res;
				};
			}

			bool isBooleanSymbol(const ExpressionPtr& expr) {
				auto& gen = expr->getNodeManager().getLangBasic();
				return expr.isa<LiteralPtr>() && !gen.isTrue(expr) && !gen.isFalse(expr);
			}

		}

		class BooleanConstraintCollector : public BasicDataFlowConstraintCollector<bool> {

			typedef BasicDataFlowConstraintCollector<bool> super;

			const core::lang::BasicGenerator& base;

		public:

			BooleanConstraintCollector(CBA& context)
				: super(context, cba::B, cba::b),
				  base(context.getRoot()->getNodeManager().getLangBasic())
			{ };

			void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitLiteral(literal, ctxt, constraints);

				// only interested in boolean literals
				if (!base.isBool(literal->getType())) return;

				// add constraint literal \in A(lit)
				bool isTrue = base.isTrue(literal);
				bool isFalse = base.isFalse(literal);

				auto l_lit = context.getLabel(literal);

				if (isTrue  || (!isTrue && !isFalse)) constraints.add(elem(true, context.getSet(B, l_lit, ctxt)));
				if (isFalse || (!isTrue && !isFalse)) constraints.add(elem(false, context.getSet(B, l_lit, ctxt)));

			}


			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

				// conduct std-procedure
				super::visitCallExpr(call, ctxt, constraints);

				// only care for integer expressions calling literals
				if (!base.isBool(call->getType())) return;

				// check whether it is a literal => otherwise basic data flow is handling it
				auto fun = call->getFunctionExpr();
				if (!fun.isa<LiteralPtr>()) return;

				// get some labels / ids
				auto B_res = context.getSet(B, context.getLabel(call), ctxt);

				// handle unary literals
				if (call.size() == 1u) {

					// check whether it is a de-ref
					if (base.isRefDeref(fun)) {
						return;		// has been handled by super!
					}

					// support negation
					if (base.isBoolLNot(fun)) {
						auto B_arg = context.getSet(B, context.getLabel(call[0]), ctxt);
						constraints.add(subsetUnary(B_arg, B_res, [](const set<bool>& in)->set<bool> {
							set<bool> out;
							for(bool cur : in) out.insert(!cur);
							return out;
						}));
						return;
					}
				}

				// and binary operators
				if (call.size() != 2u) {
					// this value is unknown => might be both
					constraints.add(elem(true, B_res));
					constraints.add(elem(false, B_res));
					return;
				}


				// boolean relations
				{
					// get sets for operators
					auto B_lhs = context.getSet(B, context.getLabel(call[0]), ctxt);
					auto B_rhs = context.getSet(B, context.getLabel(call[1]), ctxt);

					if (base.isBoolEq(fun)) {
						// equality is guaranteed if symbols are identical - no matter what the value is
						if (isBooleanSymbol(call[0]) && isBooleanSymbol(call[1])) {
							constraints.add(elem(call[0].as<ExpressionPtr>() == call[1].as<ExpressionPtr>(), B_res));
						} else {
							constraints.add(subsetBinary(B_lhs, B_rhs, B_res, pairwise([](bool a, bool b) { return a == b; })));
						}
						return;
					}

					if (base.isBoolNe(fun)) {
						// equality is guaranteed if symbols are identical - no matter what the value is
						if (isBooleanSymbol(call[0]) && isBooleanSymbol(call[1])) {
							constraints.add(elem(call[0].as<ExpressionPtr>() != call[1].as<ExpressionPtr>(), B_res));
						} else {
							constraints.add(subsetBinary(B_lhs, B_rhs, B_res, pairwise([](bool a, bool b) { return a != b; })));
						}
						return;
					}
				}

				// arithmetic relations
				{
					auto A_lhs = context.getSet(cba::A, context.getLabel(call[0]), ctxt);
					auto A_rhs = context.getSet(cba::A, context.getLabel(call[1]), ctxt);

					typedef core::arithmetic::Formula F;
					typedef core::arithmetic::Inequality Inequality;		// shape: formula <= 0

					if(base.isSignedIntLt(fun) || base.isUnsignedIntLt(fun)) {
						constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b) {
							// a < b  ... if !(a >= b) = !(b <= a) = !(b-a <= 0)
							Inequality i(b-a);
							return std::make_pair(i.isUnsatisfiable(), i.isValid());
						})));
						return;
					}

					if(base.isSignedIntLe(fun) || base.isUnsignedIntLe(fun)) {
						constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b) {
							// a <= b ... if (a-b <= 0)
							Inequality i(a-b);
							return std::make_pair(i.isValid(), i.isUnsatisfiable());
						})));
						return;
					}

					if(base.isSignedIntGe(fun) || base.isUnsignedIntGe(fun)) {
						constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b){
							// a >= b ... if (b <= a) = (b-a <= 0)
							Inequality i(b-a);
							return std::make_pair(i.isValid(), i.isUnsatisfiable());
						})));
						return;
					}

					if(base.isSignedIntGt(fun) || base.isUnsignedIntGt(fun)) {
						constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b){
							// a > b ... if !(a <= b) = !(a-b <= 0)
							Inequality i(a-b);
							return std::make_pair(i.isUnsatisfiable(), i.isValid());
						})));
						return;
					}

					if(base.isSignedIntEq(fun) || base.isUnsignedIntEq(fun)) {
						constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b) {
							// just compare formulas (in normal form)
							bool equal = (a==b);
							return std::make_pair(equal, !equal && a.isConstant() && b.isConstant());
						})));
						return;
					}

					if(base.isSignedIntNe(fun) || base.isUnsignedIntNe(fun)) {
						constraints.add(subsetBinary(A_lhs, A_rhs, B_res, compareFormula([](const F& a, const F& b) {
							// just compare formulas (in normal form)
							bool equal = (a==b);
							return std::make_pair(!equal && a.isConstant() && b.isConstant(), equal);
						})));
						return;
					}
				}

				// otherwise it is unknown, hence both may be possible
				constraints.add(elem(true, B_res));
				constraints.add(elem(false, B_res));
			}

		};

		class ReferenceConstraintCollector : public BasicDataFlowConstraintCollector<Location> {

			typedef BasicDataFlowConstraintCollector<Location> super;

		public:

			ReferenceConstraintCollector(CBA& context)
				: BasicDataFlowConstraintCollector<Location>(context, R, r) { };

			void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitLiteral(literal, ctxt, constraints);

				// only interested in memory location constructors
				if (!isMemoryConstructor(literal)) return;

				// add constraint literal \in R(lit)
				auto value = context.getLocation(literal);
				auto l_lit = context.getLabel(literal);

				auto R_lit = context.getSet(R, l_lit, ctxt);
				constraints.add(elem(value, R_lit));

			}

			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

				// and default handling
				super::visitCallExpr(call, ctxt, constraints);

				// introduce memory location in some cases
				if (!isMemoryConstructor(call)) return;

				// add constraint location \in R(call)
				auto value = context.getLocation(call);
				auto l_lit = context.getLabel(call);

				auto R_lit = context.getSet(R, l_lit, ctxt);
				constraints.add(elem(value, R_lit));
			}

		};

		// a utility function extracting a list of memory location constructors from the given code fragment
		vector<Location> getAllLocations(CBA& context, const StatementAddress& root) {
			vector<Location> res;
			// collect all memory location constructors
			visitDepthFirst(root, [&](const ExpressionAddress& cur) {
				// TODO: add context info to locations
				if (isMemoryConstructor(cur)) {
					res.push_back(context.getLocation(cur));
				}
			});
			return res;
		}


		// ----------------------------------------------------------------------------------------------------------------------------
		//
		//														Imperative Constraints
		//
		// ----------------------------------------------------------------------------------------------------------------------------


		template<typename SetIDType, typename Collector>
		class BasicInOutConstraintCollector : public ConstraintResolver {

		protected:

			typedef ConstraintResolver super;

			// the sets to be used for in/out states
			const SetIDType& Ain;
			const SetIDType& Aout;

			Collector& collector;

		public:

			BasicInOutConstraintCollector(CBA& context, const SetIDType& Ain, const SetIDType& Aout, Collector& collector)
				: super(context), Ain(Ain), Aout(Aout), collector(collector) {}

		protected:

			void connectSets(const SetIDType& a, const StatementAddress& al, const Context& ac, const SetIDType& b, const StatementAddress& bl, const Context& bc, Constraints& constraints) {
				// filter out invalid contexts
				if (!this->context.isValid(ac) || !this->context.isValid(bc)) return;
				connectStateSets(a, context.getLabel(al), ac, b, context.getLabel(bl), bc, constraints);
			}

			template<typename E>
			void connectSetsIf(const E& value, const TypedSetID<E>& set, const SetIDType& a, const StatementAddress& al, const Context& ac, const SetIDType& b, const StatementAddress& bl, const Context& bc, Constraints& constraints) {
				// filter out invalid contexts
				if (!this->context.isValid(ac) || !this->context.isValid(bc)) return;
				connectStateSetsIf(value, set, a, context.getLabel(al), ac, b, context.getLabel(bl), bc, constraints);
			}

			void connectStateSets(const SetIDType& a, Label al, const Context& ac, const SetIDType& b, Label bl, const Context& bc, Constraints& constraints) {
				// filter out invalid contexts
				if (!this->context.isValid(ac) || !this->context.isValid(bc)) return;
				collector.connectStateSets(a,al,ac,b,bl,bc,constraints);
			}

			template<typename E>
			void connectStateSetsIf(const E& value, const TypedSetID<E>& set, const SetIDType& a, Label al, const Context& ac, const SetIDType& b, Label bl, const Context& bc, Constraints& constraints) {
				// filter out invalid contexts
				if (!this->context.isValid(ac) || !this->context.isValid(bc)) return;
				collector.connectStateSetsIf(value,set,a,al,ac,b,bl,bc,constraints);
			}

		};


		template<typename SetIDType, typename Collector>
		class ImperativeInConstraintCollector : public BasicInOutConstraintCollector<SetIDType, Collector> {

			typedef BasicInOutConstraintCollector<SetIDType, Collector> super;

		public:

			ImperativeInConstraintCollector(CBA& context, const SetIDType& Ain, const SetIDType& Aout, Collector& collector)
				: super(context, Ain, Aout, collector) {}


			void connectCallToBody(const CallExprAddress& call, const Context& callCtxt, const StatementAddress& body, const Context& trgCtxt, const ContextFreeCallable& callable,  Constraints& constraints) {

				// check whether given call / target context is actually valid
				auto fun = call->getFunctionExpr();
				auto l_call = this->getContext().getLabel(call);
				if (!(fun.isa<LambdaExprPtr>() || fun.isa<BindExprPtr>())) {		// it is not a direct call
					if (callCtxt.callContext << l_call != trgCtxt.callContext) return;
				} else if (callCtxt.callContext != trgCtxt.callContext) {
					return;
				}

				// check proper number of arguments
				if (callable->getType().as<FunctionTypePtr>()->getParameterTypes().size() != call.size()) return;

				// check whether call-site is within a bind
				bool isCallWithinBind = (!call.isRoot() && call.getParentNode()->getNodeType() == NT_BindExpr);
				auto bind = (isCallWithinBind) ? call.getParentAddress().as<BindExprAddress>() : BindExprAddress();

				// get label for the body expression
				auto l_body = this->getContext().getLabel(body);

				// get labels for call-site
				auto l_fun = this->getContext().getLabel(call->getFunctionExpr());
				auto F_call = this->getContext().getSet(F, l_fun, callCtxt);

				// add effect of function-expression-evaluation (except within bind calls)
				if (!isCallWithinBind) this->connectStateSetsIf(callable, F_call, this->Aout, l_fun, callCtxt, this->Ain, l_body, trgCtxt, constraints);

				// just connect the effect of the arguments of the call-site with the in of the body call statement
				for(auto arg : call) {

					// skip bound parameters
					if (bind && bind->isBoundExpression(arg)) continue;

					// add effect of argument
					auto l_arg = this->getContext().getLabel(arg);
					this->connectStateSetsIf(callable, F_call, this->Aout, l_arg, callCtxt, this->Ain, l_body, trgCtxt, constraints);
				}

				// special case: if this is a bind with no parameters
				if (bind && bind->getParameters()->empty()) {
					// connect in of call site with in of body
					this->connectStateSetsIf(callable, F_call, this->Ain, l_call, callCtxt, this->Ain, l_body, trgCtxt, constraints);
				}

			}

			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

				// special handling only for calls in bind expressions
				if (call.isRoot() || call.getParentNode()->getNodeType() != NT_BindExpr) {
					// run standard procedure
					this->visitExpression(call, ctxt, constraints);
					return;
				}

				// ----- we have a call in a bind expression ----
				auto bind = call.getParentAddress().as<BindExprAddress>();
				if (bind.isRoot()) return;	// nothing to do

				auto user = bind.getParentAddress();

				// check for direct calls ...
				if (user->getNodeType() == NT_CallExpr && user.as<CallExprAddress>()->getFunctionExpr() == bind) {

					// it is one => no change in context
					this->connectSets(this->Ain, bind, ctxt, this->Ain, call, ctxt, constraints);

				} else {
					// it is no direct call => change in context possible
					auto numParams = bind->getParameters().size();
					for(auto& dynCall : this->context.getDynamicCalls()) {
						if(numParams != dynCall.size()) continue;

						// special case: ctxt starts with 0 - root context, is not called by anybody
						if (ctxt.callContext.startsWith(0)) {
							Context srcCtxt = ctxt;
							srcCtxt.callContext >>= 0;
							connectCallToBody(dynCall, srcCtxt, call, ctxt, bind, constraints);
						} else {

							// all other contexts may be reached from any other
							for(auto& l : this->context.getDynamicCallLabels()) {

								// nobody calls context 0
								if (l != 0 && ctxt.callContext.startsWith(0)) continue;

								Context srcCtxt = ctxt;
								srcCtxt.callContext >>= l;

								// connect call site with body
								connectCallToBody(dynCall, srcCtxt, call, ctxt, bind, constraints);
							}
						}
					}
				}

			}

			void visitCompoundStmt(const CompoundStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

				// TODO: check whether it is a function body => otherwise default handling
				if (stmt.isRoot()) return;

				auto parent = stmt.getParentAddress();

				// handle lambda
				if (auto lambda = parent.isa<LambdaAddress>()) {

					// get full lambda expression
					auto lambdaExpr = parent.getParentAddress(3).as<LambdaExprAddress>();

					// get call site
					auto user = parent.getParentAddress(4);
					auto call = user.isa<CallExprAddress>();
					if (call && call->getFunctionExpr() == lambdaExpr) {

						// connect call site with body
						connectCallToBody(call, ctxt, stmt, ctxt, lambdaExpr, constraints);

					} else {

						// this function is invoked indirectly
						auto numParams = lambda.as<LambdaPtr>()->getParameters().size();
						for(auto& call : this->context.getDynamicCalls()) {
							if(numParams != call.size()) continue;

							// all other contexts may be reached from any other
							for(auto& l : this->context.getDynamicCallLabels()) {

								// nobody calls context 0
								if (l != 0 && ctxt.callContext.startsWith(0)) continue;

								Context srcCtxt = ctxt;
								srcCtxt.callContext >>= l;

								// connect call site with body
								connectCallToBody(call, srcCtxt, stmt, ctxt, lambdaExpr, constraints);
							}
						}

					}

					// done
					return;
				}

				// use default handling
				visitStatement(stmt, ctxt, constraints);

			}


			void visitStatement(const StatementAddress& stmt, const Context& ctxt, Constraints& constraints) {

				// determine predecessor based on parent
				if (stmt.isRoot()) return;		// no predecessor

				// check out parent
				auto parent = stmt.getParentAddress();

				// TODO: turn this into a visitor!

				// special case: if current expression is an argument of a bind-call expression
				if (stmt.getDepth() >=2) {
					if (auto call = parent.isa<CallExprAddress>()) {
						if (auto bind = call.getParentAddress().isa<BindExprAddress>()) {
							// if this is a bound expression predecessor is the bind, not the call
							if (bind->isBoundExpression(stmt.as<ExpressionAddress>())) {
								// connect bind with stmt - skip the call
								this->connectSets(this->Ain, bind, ctxt, this->Ain, stmt, ctxt, constraints);
								// and done
								return;
							}
						}
					}
				}

				// a simple case - it is just a nested expression
				if (auto expr = parent.isa<ExpressionAddress>()) {
					// parent is an expression => in of parent is in of current stmt
					this->connectSets(this->Ain, expr, ctxt, this->Ain, stmt, ctxt, constraints);
					return;	// done
				}

				// handle full-expressions
				if (auto compound = parent.isa<CompoundStmtAddress>()) {

					// parent is a compound, predecessor is one statement before
					auto pos = stmt.getIndex();

					// special case: first statement
					if (pos == 0) {
						this->connectSets(this->Ain, compound, ctxt, this->Ain, stmt, ctxt, constraints);
						return;	// done
					}

					// general case - link with predecessor
					auto prev = compound[pos-1];

					// do not link with previous control statements
					switch(prev->getNodeType()) {
					case NT_ReturnStmt: case NT_ContinueStmt: case NT_BreakStmt: return;
					default: break;
					}

					this->connectSets(this->Aout, prev, ctxt, this->Ain, stmt, ctxt, constraints);
					return;	// done
				}

				// handle simple statements
				if (parent.isa<ReturnStmtAddress>() || parent.isa<DeclarationStmtAddress>()) {
					// in is the in of the stmt
					this->connectSets(this->Ain, parent.as<StatementAddress>(), ctxt, this->Ain, stmt, ctxt, constraints);
					return; // done
				}

				// handle if stmt
				if (auto ifStmt = parent.isa<IfStmtAddress>()) {

					// check whether which part the current node is

					auto cond = ifStmt->getCondition();
					if (cond == stmt) {

						// connect in with if-stmt in with condition in
						this->connectSets(this->Ain, ifStmt, ctxt, this->Ain, stmt, ctxt, constraints);

					} else if (ifStmt->getThenBody() == stmt) {

						// connect out of condition with in of body if condition may be true
						auto l_cond = this->context.getLabel(cond);
						auto B_cond = this->context.getSet(B, l_cond, ctxt);
						this->connectSetsIf(true, B_cond, this->Aout, cond, ctxt, this->Ain, stmt, ctxt, constraints);

					} else if (ifStmt->getElseBody() == stmt) {

						// connect out of condition with in of body if condition may be false
						auto l_cond = this->context.getLabel(cond);
						auto B_cond = this->context.getSet(B, l_cond, ctxt);
						this->connectSetsIf(false, B_cond, this->Aout, cond, ctxt, this->Ain, stmt, ctxt, constraints);

					} else {
						assert_fail() << "No way!\n";
					}

					// dones
					return;
				}

				// handle while stmt
				if (auto whileStmt = parent.isa<WhileStmtAddress>()) {

					// check which part of a while the current node is
					auto cond = whileStmt->getCondition();
					auto l_cond = this->context.getLabel(cond);
					auto B_cond = this->context.getSet(B, l_cond, ctxt);
					if (cond == stmt) {
						// connect in of while to in of condition
						this->connectSets(this->Ain, whileStmt, ctxt, this->Ain, stmt, ctxt, constraints);

						// also, in case loop is looping, out of body is in of condition
						this->connectSetsIf(true, B_cond, this->Aout, whileStmt->getBody(), ctxt, this->Ain, stmt, ctxt, constraints);

					} else if (whileStmt->getBody() == stmt) {
						// connect out of condition with in of body
						this->connectSetsIf(true, B_cond, this->Aout, cond, ctxt, this->Ain, stmt, ctxt, constraints);
					} else {
						assert_fail() << "No way!";
					}
					return;
				}

				assert_fail() << "Unsupported parent type encountered: " << parent->getNodeType();
			}
		};

		template<typename SetIDType, typename Collector>
		class ImperativeOutConstraintCollector : public BasicInOutConstraintCollector<SetIDType, Collector> {

			typedef BasicInOutConstraintCollector<SetIDType, Collector> super;

		public:

			ImperativeOutConstraintCollector(CBA& context, const SetIDType& Ain, const SetIDType& Aout, Collector& collector)
				: super(context, Ain, Aout, collector) {}


			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

				// things to do:
				//  - link in of call with in of arguments
				//  - link out of arguments with in of function
				//  - link out of function with out of call

				auto l_call = this->context.getLabel(call);

				// create inner call context
				Context innerCallContext = ctxt;

				// get set of potential target functions
				auto l_fun = this->context.getLabel(call->getFunctionExpr());
				auto F_fun = this->context.getSet(F, l_fun, ctxt);

				// a utility resolving constraints for the called function
				auto addConstraints = [&](const ExpressionAddress& fun, bool fixed) {

					// check correct number of arguments
					if (call.size() != fun.getType().as<FunctionTypePtr>()->getParameterTypes().size()) {
						// this is not a valid target
						return;
					}

					// ---- Effect of function => out of call ---

					// get body
					StatementAddress body;
					if (auto lambda = fun.isa<LambdaExprAddress>()) {
						body = lambda->getBody();
					} else if (auto bind = fun.isa<BindExprAddress>()) {
						body = bind->getCall();
					} else {
						std::cout << "Unsupported potential target of type " << fun->getNodeType() << " encountered.";
						assert(false && "Unsupported potential call target.");
					}

					// get label for body
					auto l_body = this->context.getLabel(body);

					// link out of fun with call out
					if (fixed) {
						this->connectStateSets(this->Aout, l_body, innerCallContext, this->Aout, l_call, ctxt, constraints);
					} else {
						this->connectStateSetsIf(fun, F_fun, this->Aout, l_body, innerCallContext, this->Aout, l_call, ctxt, constraints);
					}

					// process function body
//					if(fixed) this->visit(body, innerCallContext, constraints);
				};


				// handle call target
				auto fun = call->getFunctionExpr();

				if (fun.isa<LiteralPtr>()) {

					// - here we are assuming side-effect free literals -

					// just connect out of arguments to call-out
					for (auto arg : call) {
						auto l_arg = this->context.getLabel(arg);
						this->connectStateSets(this->Aout, l_arg, ctxt, this->Aout, l_call, ctxt, constraints);
					}

					// and the function
					this->connectStateSets(this->Aout, l_fun, ctxt, this->Aout, l_call, ctxt, constraints);

				} else if (auto lambda = fun.isa<LambdaExprAddress>()) {

					// direct call => handle directly
					addConstraints(lambda, true);

				} else if (auto bind = fun.isa<BindExprAddress>()) {

					// direct call of bind => handle directly
					addConstraints(bind, true);

				} else {

					// create new call-context
					innerCallContext.callContext <<= l_call;

					// consider every potential target function
					for(const auto& cur : this->context.getFreeFunctions()) {
						addConstraints(cur, false);
					}
				}
			}

			void visitBindExpr(const BindExprAddress& bind, const Context& ctxt, Constraints& constraints) {

				// out-effects are only influenced by bound parameters
				auto l_cur = this->context.getLabel(bind);
				for(const auto& arg : bind->getBoundExpressions()) {
					auto l_arg = this->context.getLabel(arg);
					this->connectStateSets(this->Aout, l_arg, ctxt, this->Aout, l_cur, ctxt, constraints);
				}

				// and no more ! (in particular not the effects of the inner call)
			}

			void visitExpression(const ExpressionAddress& expr, const Context& ctxt, Constraints& constraints) {
				// for most expressions: just connect in and out
				auto l_cur = this->context.getLabel(expr);
				this->connectStateSets(this->Ain, l_cur, ctxt, this->Aout, l_cur, ctxt, constraints);
			}

			void visitCompoundStmt(const CompoundStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

				// special case: empty compound
				if (stmt.empty()) {
					this->connectSets(this->Ain, stmt, ctxt, this->Aout, stmt, ctxt, constraints);
					return;
				}

				// connect with last statement
				auto last = stmt[stmt.size()-1];
				auto type = last->getNodeType();
				if (!(type == NT_ReturnStmt || type == NT_ContinueStmt || type==NT_BreakStmt)) {
					this->connectSets(this->Aout, last, ctxt, this->Aout, stmt, ctxt, constraints);
				}

				// also connect stmt-out with all returns if it is a lambda body
				if (stmt.isRoot() || !stmt.getParentNode().isa<LambdaPtr>()) return;

				// TODO: locate return statements more efficiently

				auto l_body = this->context.getLabel(stmt);
				visitDepthFirstPrunable(stmt, [&](const StatementAddress& stmt) {
					// prune inner functions
					if (stmt.isa<LambdaExprAddress>()) return true;

					// visit return statements
					if (auto returnStmt = stmt.isa<ReturnStmtAddress>()) {

						// connect value of return statement with body value
						auto l_ret = this->context.getLabel(returnStmt);
						auto R_ret = this->context.getSet(Rout, l_ret, ctxt);
						this->connectStateSetsIf(Reachable(), R_ret, this->Aout, l_ret, ctxt, this->Aout, l_body, ctxt, constraints);

						// TODO: this is just a performance improvement - but for now disabled
//						visit(returnStmt, ctxt, constraints);
						return true;
					}

					return false;
				});

			}

			void visitDeclarationStmt(const DeclarationStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {
				// link out of init expression to out of decl stmt
				this->connectSets(this->Aout, stmt->getInitialization(), ctxt, this->Aout, stmt, ctxt, constraints);
			}

			void visitReturnStmt(const ReturnStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {
				// link out of return expression to out of return stmt
				this->connectSets(this->Aout, stmt->getReturnExpr(), ctxt, this->Aout, stmt, ctxt, constraints);
			}

			void visitIfStmt(const IfStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {
				// link out with out of bodies
				auto l_cond = this->context.getLabel(stmt->getCondition());
				auto B_cond = this->context.getSet(B, l_cond, ctxt);
				this->connectSetsIf(true,  B_cond, this->Aout, stmt->getThenBody(), ctxt, this->Aout, stmt, ctxt, constraints);
				this->connectSetsIf(false, B_cond, this->Aout, stmt->getElseBody(), ctxt, this->Aout, stmt, ctxt, constraints);
			}

			void visitWhileStmt(const WhileStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {
				// link out of condition to out if condition may ever become false
				auto cond = stmt->getCondition();
				auto l_cond = this->context.getLabel(cond);
				auto B_cond = this->context.getSet(B, l_cond, ctxt);
				this->connectSetsIf(false, B_cond, this->Aout, cond, ctxt, this->Aout, stmt, ctxt, constraints);
			}

			void visitNode(const NodeAddress& node, const Context& ctxt, Constraints& res) {
				assert_fail() << "Unsupported Node Type encountered: " << node->getNodeType();
			}
		};



		class ReachableInConstraintCollector : public ImperativeInConstraintCollector<TypedSetType<Reachable>, ReachableInConstraintCollector> {

			typedef ImperativeInConstraintCollector<TypedSetType<Reachable>, ReachableInConstraintCollector> super;

			StatementAddress root;

			bool initSet;

		public:

			ReachableInConstraintCollector(CBA& context)
				: super(context, Rin, Rout, *this), root(context.getRoot()), initSet(false) { }

			virtual void visit(const NodeAddress& node, const Context& ctxt, Constraints& constraints) {

				// make sure root is reachable
				if (!initSet && node == root && ctxt == Context()) {
					auto l = context.getLabel(root);
					auto R = context.getSet(Rin, l, ctxt);
					constraints.add(elem(Reachable(), R));
					initSet = true;
				}

				// and all the other constraints
				super::visit(node, ctxt, constraints);
			}

			void connectStateSets (
						const TypedSetType<Reachable>& a, Label al, const Context& ac,
						const TypedSetType<Reachable>& b, Label bl, const Context& bc,
						Constraints& constraints
					) const {

				auto A = context.getSet(a, al, ac);
				auto B = context.getSet(b, bl, bc);
				constraints.add(subset(A, B));
			}

			template<typename E>
			void connectStateSetsIf (
						const E& value, const TypedSetID<E>& set,
						const TypedSetType<Reachable>& a, Label al, const Context& ac,
						const TypedSetType<Reachable>& b, Label bl, const Context& bc,
						Constraints& constraints
					) const {

				if (ac != bc) {
					auto pre = context.getSet(pred, bc.callContext.back());
					auto A = context.getSet(a, al, ac);
					auto B = context.getSet(b, bl, bc);
					constraints.add(subsetIf(ac.callContext.back(), pre, value, set, A, B));
				} else {
					auto A = context.getSet(a, al, ac);
					auto B = context.getSet(b, bl, bc);
					constraints.add(subsetIf(value, set, A, B));
				}
			}

		};

		class ReachableOutConstraintCollector : public ImperativeOutConstraintCollector<TypedSetType<Reachable>, ReachableOutConstraintCollector> {

			typedef ImperativeOutConstraintCollector<TypedSetType<Reachable>, ReachableOutConstraintCollector> super;

		public:

			ReachableOutConstraintCollector(CBA& context)
				: super(context, Rin, Rout, *this) { }

			void connectStateSets (
						const TypedSetType<Reachable>& a, Label al, const Context& ac,
						const TypedSetType<Reachable>& b, Label bl, const Context& bc,
						Constraints& constraints
					) const {

				auto A = context.getSet(a, al, ac);
				auto B = context.getSet(b, bl, bc);
				constraints.add(subset(A, B));
			}

			template<typename E>
			void connectStateSetsIf (
						const E& value, const TypedSetID<E>& set,
						const TypedSetType<Reachable>& a, Label al, const Context& ac,
						const TypedSetType<Reachable>& b, Label bl, const Context& bc,
						Constraints& constraints
					) const {

				auto A = context.getSet(a, al, ac);
				auto B = context.getSet(b, bl, bc);
				constraints.add(subsetIf(value, set, A, B));
			}

		};


		template<typename T>
		class ImperativeInStateConstraintCollector : public ImperativeInConstraintCollector<StateSetType, ImperativeInStateConstraintCollector<T>> {

			typedef ImperativeInConstraintCollector<StateSetType, ImperativeInStateConstraintCollector<T>> super;

			const TypedSetType<T>& dataSet;

			// the one location this instance is working for
			Location location;

		public:

			ImperativeInStateConstraintCollector(CBA& context, const TypedSetType<T>& dataSet, const Location& location)
				: super(context, Sin, Sout, *this), dataSet(dataSet), location(location) {}


			void connectStateSets(const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

				// general handling - Sin = Sout

				// get Sin set		TODO: add context to locations
				auto s_in = this->context.getSet(a, al, ac, location, dataSet);
				auto s_out = this->context.getSet(b, bl, bc, location, dataSet);

				// state information entering the set is also leaving it
				constraints.add(subset(s_in, s_out));

			}

			template<typename E>
			void connectStateSetsIf(const E& value, const TypedSetID<E>& set, const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

				// general handling - Sin = Sout

				// get Sin set		TODO: add context to locations
				auto s_in = this->context.getSet(a, al, ac, location, dataSet);
				auto s_out = this->context.getSet(b, bl, bc, location, dataSet);

				// state information entering the set is also leaving it
				if (ac == bc) {
					constraints.add(subsetIf(value, set, s_in, s_out));
				} else {
					auto pre = this->context.getSet(pred, bc.callContext.back());
					constraints.add(subsetIf(ac.callContext.back(), pre, value, set, s_in, s_out));
				}
			}
		};


		template<typename T>
		class ImperativeOutStateConstraintCollector : public ImperativeOutConstraintCollector<StateSetType, ImperativeOutStateConstraintCollector<T>> {

			typedef ImperativeOutConstraintCollector<StateSetType, ImperativeOutStateConstraintCollector<T>> super;

			const TypedSetType<T>& dataSet;

			// the one location this instance is working for
			Location location;

		public:

			ImperativeOutStateConstraintCollector(CBA& context, const TypedSetType<T>& dataSet, const Location& location)
				: super(context, Sin, Sout, *this), dataSet(dataSet), location(location) {
			}

			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
				const auto& base = call->getNodeManager().getLangBasic();

				// one special case: assignments
				auto fun = call.as<CallExprPtr>()->getFunctionExpr();
				if (base.isRefAssign(fun)) {

					// get some labels
					auto l_call = this->context.getLabel(call);
					auto l_rhs = this->context.getLabel(call[0]);
					auto l_lhs = this->context.getLabel(call[1]);

					// ---- S_out of args => S_tmp of call (only if other location is possible)

					auto R_rhs = this->context.getSet(R, l_rhs, ctxt);
					auto S_out_rhs = this->context.getSet(Sout, l_rhs, ctxt, location, dataSet);
					auto S_out_lhs = this->context.getSet(Sout, l_lhs, ctxt, location, dataSet);
					auto S_tmp = this->context.getSet(Stmp, l_call, ctxt, location, dataSet);
					constraints.add(subsetIfReducedBigger(R_rhs, location, 0, S_out_rhs, S_tmp));
					constraints.add(subsetIfReducedBigger(R_rhs, location, 0, S_out_lhs, S_tmp));

					// ---- combine S_tmp to S_out ...

					// add rule: loc \in R[rhs] => A[lhs] \sub Sout[call]
					auto A_value = this->context.getSet(dataSet, l_lhs, ctxt);
					auto S_out = this->context.getSet(Sout, l_call, ctxt, location, dataSet);
					constraints.add(subsetIf(location, R_rhs, A_value, S_out));

					// add rule: |R[rhs]\{loc}| > 0 => Stmp[call] \sub Sout[call]
					constraints.add(subsetIfReducedBigger(R_rhs, location, 0, S_tmp, S_out));

					// done
					return;
				}

				// everything else is treated using the default procedure
				super::visitCallExpr(call, ctxt, constraints);
			}


			void connectStateSets(const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

				// general handling - Sin = Sout

				// get Sin set		TODO: add context to locations
				auto s_in = this->context.getSet(a, al, ac, location, dataSet);
				auto s_out = this->context.getSet(b, bl, bc, location, dataSet);

				// state information entering the set is also leaving it
				constraints.add(subset(s_in, s_out));

			}

			template<typename E>
			void connectStateSetsIf(const E& value, const TypedSetID<E>& set, const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

				// general handling - Sin = Sout

				// get Sin set		TODO: add context to locations
				auto s_in = this->context.getSet(a, al, ac, location, dataSet);
				auto s_out = this->context.getSet(b, bl, bc, location, dataSet);

				// state information entering the set is also leaving it
				constraints.add(subsetIf(value, set, s_in, s_out));
			}
		};


		// -------------------------------------- Context Predecessor Constraints -----------------------------

		class ContextPredecessor : public ConstraintResolver {


		public:

			ContextPredecessor(CBA& cba) : ConstraintResolver(cba) {}


			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
				// restrict context
				assert_true(ctxt == Context()) << "This resolver only operates on the default context - given: " << ctxt;

				// check that call is a dynamic call
				auto funType = call->getFunctionExpr()->getNodeType();
				assert(funType != NT_LambdaExpr && funType != NT_BindExpr);
				if (funType == NT_LambdaExpr || funType == NT_BindExpr) return;		// not interested

				// fill predecessor set
				auto pred_res = context.getSet(pred, context.getLabel(call));

				// get surrounding free function
				auto fun = getSurroundingFreeFunction(call);

				// check whether there is a surrounding free function
				if (!fun) {
					// => this function can only reached statically
					constraints.add(elem(0, pred_res));
					return;
				}

				// ----- check whether function forwarding can be traced statically ----

				if (auto staticUses = context.getAllStaticUses(fun)) {
					// if uses can be determined statically, we can just consider them
					for(Label l_call : *staticUses) {
						constraints.add(elem(l_call, pred_res));
					}
					return;
				}

				// ----- fallback, the function might reach any point in the code -----

				// uses have to be determined dynamically
				vector<Context::CallContext> callContexts;
				generateSequences(context.getDynamicCallLabels(), callContexts);

				// compute all contexts this function may be called at
				auto num_params = fun->getType().as<FunctionTypePtr>()->getParameterTypes().size();
				for(const auto& dynCall : context.getDynamicCalls()) {
					// check number of parameters
					if (dynCall.size() != num_params) continue;

					auto l_call = context.getLabel(dynCall);
					auto l_fun = context.getLabel(dynCall->getFunctionExpr());

					for(const auto& callCtxt : callContexts) {
						auto F_dynCall = context.getSet(F, l_fun, callCtxt);
						constraints.add(elemIf(fun.as<ExpressionAddress>(), F_dynCall, l_call, pred_res));
					}
				}
			}

		};

	}

	namespace {

		template<typename T>
		void registerImperativeCollector(CBA& context, const TypedSetType<T>& type) {
			for(auto loc : context.getLocations()) {
				context.registerLocationResolver<ImperativeInStateConstraintCollector<T>>(Sin, type, loc);
				context.registerLocationResolver<ImperativeOutStateConstraintCollector<T>>(Sout, type, loc);
			}
		}

		template<typename R, typename E>
		struct ResolverFactory {
			const TypedSetType<E>& setType;
			ResolverFactory(const TypedSetType<E>& setType) : setType(setType) {}
			ConstraintResolverPtr operator()(CBA& cba) const {
				// register imperative constraint resolver
				registerImperativeCollector(cba, setType);
				// and the actual resolver for this type
				return cba.getResolver<R>(setType);
			}
		};

		template<typename R, typename E>
		ResolverFactory<R,E> createFactory(const TypedSetType<E>& setType) {
			return ResolverFactory<R,E>(setType);
		}

	}

	const TypedSetType<Callable> C("C", createFactory<ControlFlowConstraintCollector>(C));
	const TypedSetType<Callable> c("c", createFactory<ControlFlowConstraintCollector>(C));

	const TypedSetType<ContextFreeCallable> F("F", createFactory<FunctionConstraintCollector>(F));
	const TypedSetType<ContextFreeCallable> f("f", createFactory<FunctionConstraintCollector>(F));


	const TypedSetType<Location> R("R", createFactory<ReferenceConstraintCollector>(R));
	const TypedSetType<Location> r("r", createFactory<ReferenceConstraintCollector>(R));

	const TypedSetType<core::ExpressionPtr> D("D", createFactory<ConstantConstraintCollector>(D));
	const TypedSetType<core::ExpressionPtr> d("d", createFactory<ConstantConstraintCollector>(D));

	const TypedSetType<Formula> A("A", createFactory<ArithmeticConstraintCollector>(A));
	const TypedSetType<Formula> a("a", createFactory<ArithmeticConstraintCollector>(A));

	const TypedSetType<bool> B("B", createFactory<BooleanConstraintCollector>(B));
	const TypedSetType<bool> b("b", createFactory<BooleanConstraintCollector>(B));

	const TypedSetType<Reachable> Rin("Rin", createFactory<ReachableInConstraintCollector>(Rin));			// the associated term is reached
	const TypedSetType<Reachable> Rout("Rout", createFactory<ReachableOutConstraintCollector>(Rout));		// the associated term is left

	const TypedSetType<Label> pred("pred", createFactory<ContextPredecessor>(pred));

	const StateSetType Sin("Sin");		// in-state of statements
	const StateSetType Sout("Sout");	// out-state of statements
	const StateSetType Stmp("Stmp");	// temporary states of statements (assignment only)

	using namespace utils::set_constraint_2;

	namespace {

	}

	CBA::CBA(const core::StatementAddress& root)
		: root(root),
		  solver([&](const set<SetID>& sets) {
				Constraints res;
				for (auto set : sets) {
					this->addConstraintsFor(set, res);
//					std::cout << "Resolving set " << set << "...\n";
//					Constraints tmp;
//					this->addConstraintsFor(set, tmp);
//					std::cout << "Constraints: " << tmp << "\n";
//					if(!all(tmp, [&](const ConstraintPtr& cur) { return set == cur->getOutputs()[0]; })) { std::cout << "WARNING\n"; }
//					res.add(tmp);
				}
				return res;
		  }),
		  resolver(), setResolver(), locationResolver(),
		  setCounter(0), idCounter(0),
		  set2key(), set2statekey() {

		// fill dynamicCalls
		core::visitDepthFirst(root, [&](const CallExprAddress& call) {
			auto fun = call->getFunctionExpr();
			if (fun.isa<LiteralPtr>() || fun.isa<LambdaExprPtr>() || fun.isa<BindExprPtr>()) return;
			this->dynamicCalls.push_back(call);
		});

		// fill dynamic call labels
		dynamicCallLabels = ::transform(dynamicCalls, [&](const CallExprAddress& cur) { return getLabel(cur); });
		dynamicCallLabels.push_back(0);

		// obtain list of callable functions
		callables = getAllCallableTerms(*this, root);

		// and a list of all free functions
		freeFunctions.clear();
		for(const auto& cur : callables) {
			if (!contains(freeFunctions, cur.definition)) {
				freeFunctions.push_back(cur.definition);
			}
		}

		// and a list of all memory locations
		locations = getAllLocations(*this, root);

	};

	void CBA::addConstraintsFor(const SetID& set, Constraints& res) {

		// TODO: split this up into several functions

		// check standard set keys
		{
			auto pos = set2key.find(set);
			if (pos != set2key.end()) {

				const SetKey& key = pos->second;

				int id = std::get<1>(key);
				if (id == 0) return;		// nothing to do here
				const SetType& type = *std::get<0>(key);
				const Context& context = std::get<2>(key);

				auto resolver = getResolver(type);

				// get targeted node
				core::StatementAddress trg = getStmt(id);
				if (!trg) {
					// it is a variable
					trg = getVariable(id);
				}

				// this should have worked
				assert(trg && "Unable to obtain target!");

				// run resolution
				resolver->addConstraints(trg, context, res);

				// done
				return;
			}
		}

		// try a state formula
		{
			auto pos = set2statekey.find(set);
			if (pos != set2statekey.end()) {
				const StateSetKey& key = pos->second;

				// get targeted node
				core::StatementAddress trg = getStmt(std::get<1>(key));

				// run resolution
				if (trg) {
					auto type = std::get<0>(key);
//					assert_true(locationResolver.find(type) != locationResolver.end())
//						<< "Unknown resolver for: " << std::get<0>(std::get<0>(key))->getName()
//						<< "," << std::get<1>(std::get<0>(key))->getName() << "\n";
					if (!locationResolver[type]) return;
					locationResolver[type]->addConstraints(trg, std::get<2>(key), res);
				}

				// done
				return;
			}
		}

		// an unknown set?
		assert_true(false) << "Unknown set encountered: " << set << "\n";
	}

	namespace {

		bool collectUsesOfVariable(CBA& context, const VariableAddress& var, vector<Label>& res) {
			assert(getDefinitionPoint(var) == var);

			NodeAddress root;
			if (auto decl = var.getParentAddress().isa<DeclarationStmtAddress>()) {
				root = decl.getParentAddress();
			} else if (auto params = var.getParentAddress().isa<ParametersAddress>()) {
				if (auto lambda = params.getParentAddress().isa<LambdaAddress>()) {
					root = lambda->getBody();
				} else if (auto bind = params.getParentAddress().isa<BindExprAddress>()) {
					root = bind->getCall();
				} else {
					assert_fail() << "Unknown parent type for Parameters: " << params.getParentAddress()->getNodeType();
				}
			} else {
				assert_fail() << "Unknown parent of variable definition: " << var.getParentAddress()->getNodeType();
			}

			// there should be a root now
			assert(root);

			bool allFine = true;
			visitDepthFirstPrunable(root, [&](const ExpressionAddress& cur) {
				// stop if already failed
				if (!allFine) return true;

				// only process local scope
				if (cur->getNodeType() == NT_LambdaExpr) return true;

				// for the rest, only interested in variables
				if (*cur != *var) return false;

				// if variable is used as a function => found a call
				auto call = cur.getParentAddress().isa<CallExprAddress>();

				// if it is not a call, we don't care
				if (!call) return false;

				// check out whether it is a call to the function or passed as an argument
				if (call->getFunctionExpr() == cur) {
					// it is the target function => collect this one
					res.push_back(context.getLabel(call));
				} else {
					// it is an argument
					if (auto fun = call->getFunctionExpr().isa<LambdaExprAddress>()) {
						assert(call[cur.getIndex()-2] == cur);
						// ok - it is a static call => we may follow the parameter
						allFine = allFine && collectUsesOfVariable(context, fun->getParameterList()[cur.getIndex()-2], res);
					}
				}

				return false;

			});

			return allFine;

		}

		CBA::OptCallSiteList getUsesOfVariable(CBA& context, const VariableAddress& def) {
			static const CBA::OptCallSiteList fail;
			vector<Label> res;
			bool success = collectUsesOfVariable(context, def, res);
			return success ? res : fail;
		}

		CBA::OptCallSiteList getStaticUses(CBA& context, const ExpressionAddress& function) {
			static const CBA::OptCallSiteList unknown;

			// there is nothing we can do for the root
			if (function.isRoot()) return unknown;

			// option A: the lambda is created as an argument of a call expression
			auto parent = function.getParentAddress();
			if (auto call = parent.isa<CallExprAddress>()) {
				assert(call->getFunctionExpr() != function);

				// check whether target function is fixed
				if (auto fun = call->getFunctionExpr().isa<LambdaExprAddress>()) {
					// collect all uses of corresponding function parameter
					assert(call[function.getIndex()-2] == function);
					return getUsesOfVariable(context, fun->getParameterList()[function.getIndex()-2]);
				} else {
					return unknown;
				}
			}

			// option B: the lambda is created as the value of a definition
			if (auto decl = parent.isa<DeclarationStmtAddress>()) {
				// simply collect all uses of the variable
				return getUsesOfVariable(context, decl->getVariable());
			}

			return unknown;
		}

		bool isFunction(const core::ExpressionAddress& expr) {
			return expr->getNodeType() == NT_LambdaExpr || expr->getNodeType() == NT_BindExpr;
		}

		ExpressionAddress tryObtainingFunction(const core::ExpressionAddress& expr) {
			static const ExpressionAddress unknown;

			// check for null
			if (!expr) return unknown;

			// check whether we already have one
			if (isFunction(expr)) return expr;

			// otherwise we are only supporting variables
			auto var = expr.isa<VariableAddress>();
			if (!var) return unknown;

			// get definition of variable
			auto def = getDefinitionPoint(var);

			// if it is a free variable => there is nothing we can do
			if (def.isRoot()) return unknown;

			// if variable is declared => consider declaration
			auto parent = def.getParentAddress();
			if (auto decl = parent.isa<DeclarationStmtAddress>()) {
				return tryObtainingFunction(decl->getInitialization());
			}

			// if it is an lambda parameter => follow argument
			if (auto param = parent.isa<ParametersAddress>()) {

				if (param.isRoot()) return unknown;

				auto userOffset = param.getParentNode().isa<LambdaPtr>() ? 5 : 2;
				auto user = param.getParentAddress(userOffset);

				// continue with proper argument
				auto call = user.isa<CallExprAddress>();
				if (call) return tryObtainingFunction(call[param.getIndex()]);
			}

			// otherwise there is nothing we can do
			return unknown;
		}

	}


	const CBA::OptCallSiteList& CBA::getAllStaticUses(const core::ExpressionAddress& fun) {

		// check the cache
		auto pos = callSiteCache.find(fun);
		if (pos != callSiteCache.end()) {
			return pos->second;
		}

		// compute call-site list
		return callSiteCache[fun] = getStaticUses(*this, fun);
	}

	const CBA::OptCallSiteList& CBA::getAllStaticPredecessors(const core::StatementAddress& stmt) {
		static const CBA::OptCallSiteList root = toVector<Label>(0);
		auto fun = getSurroundingFreeFunction(stmt);
		if (!fun) {
			return root;
		}

		return getAllStaticUses(fun);
	}

	const CBA::CallableList& CBA::getCallableCandidates(const core::CallExprAddress& call) {

		// special handling for null-pointer (0 context)
		static const CBA::CallableList empty;
		if (!call) return empty;

		// all the rest should by known dynamic calls
		assert(contains(dynamicCalls, call));

		auto pos = callableCandidateCache.find(call);
		if (pos != callableCandidateCache.end()) {
			// if option is not set, the all callables may be targeted
			return (pos->second) ? *pos->second : callables;
		}

		// compute set of callables

		// filter out callables depending on the function that may reach the call via forwarding
		auto& res = callableCandidateCache[call];
		if (auto fun = tryObtainingFunction(call->getFunctionExpr())) {
			// filter out callables
			res = CBA::CallableList();
			for(const auto& cur : getCallables()) {
				if (cur.definition == fun && isValid(cur.context)) {
					res->push_back(cur);
				}
			}
		}

		// return collected result
		return (res) ? *res : callables;
	}

	const CBA::ContextFreeCallableList& CBA::getContextFreeCallableCandidate(const core::CallExprAddress& call) {
		// only supported for known dynamic calls
		assert(contains(dynamicCalls, call));

		auto pos = contextFreeCallableCandidateCache.find(call);
		if (pos != contextFreeCallableCandidateCache.end()) {
			return (pos->second) ? *pos->second : freeFunctions;
		}

		// compute free functions
		auto& res = contextFreeCallableCandidateCache[call];
		if (auto fun = tryObtainingFunction(call->getFunctionExpr())) {
			res = toVector(fun);
		}
		return (res) ? *res : freeFunctions;
	}


	void CBA::plot(std::ostream& out) const {
		const Constraints& constraints = solver.getConstraints();
		const Solution& ass = solver.getAssignment();

		auto getAddress = [&](const Label l)->StatementAddress {
			if (l == 0) return root;
			{
				auto pos = this->reverseLabels.find(l);
				if (pos != this->reverseLabels.end()) {
					return pos->second;
				}
			}
			{
				auto pos = this->reverseVars.find(l);
				if (pos != this->reverseVars.end()) {
					return pos->second;
				}
			}
			return StatementAddress();
		};

		// get solutions as strings
		auto solutions = ass.toStringMap();

		out << "digraph G {";

		// name sets
		for(auto cur : sets) {
			string setName = std::get<0>(cur.first)->getName();
			auto pos = getAddress(std::get<1>(cur.first));
			out << "\n\t" << cur.second
					<< " [label=\"" << cur.second << " = " << setName
						<< "[l" << std::get<1>(cur.first) << " = " << pos->getNodeType() << " : " << pos << " : " << std::get<2>(cur.first) << "]"
					<< " = " << solutions[cur.second] << "\""
					<< ((solver.isResolved(cur.second)) ? " shape=box" : "") << "];";
		}

		for(auto cur : stateSets) {
			auto& loc = std::get<0>(cur.first);
			string setName = std::get<0>(loc)->getName();
			string dataName = std::get<1>(loc)->getName();
			auto pos = getAddress(std::get<1>(cur.first));
			out << "\n\t" << cur.second
					<< " [label=\"" << cur.second << " = " << setName << "-" << dataName << "@" << std::get<2>(loc)
						<< "[l" << std::get<1>(cur.first) << " = " << pos->getNodeType() << " : " << pos << " : " << std::get<2>(cur.first) << "]"
					<< " = " << solutions[cur.second] << "\""
					<< ((solver.isResolved(cur.second)) ? " shape=box" : "") << "];";
		}

		// link sets
		for(auto cur : constraints) {
			out << "\n\t";
			cur->writeDotEdge(out, ass);
		}

		out << "\n}\n";
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
