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

#include "insieme/analysis/cba/framework/constraint_resolver.h"
#include "insieme/analysis/cba/framework/set_type.h"

#include "insieme/analysis/cba/framework/cba.h"

#include "insieme/analysis/cba/analysis/analysis.h"
#include "insieme/analysis/cba/analysis/reachability.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/set_constraint/solver2.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace insieme::utils::set_constraint_2;

	// ----------------------------------------------------------------------------------------------------------------------------
	//
	//														Imperative Constraints
	//
	// ----------------------------------------------------------------------------------------------------------------------------

	// TODO: rewrite Collector => Derived


	template<typename SetIDType, typename Collector, typename Context>
	class BasicInOutConstraintResolver : public ConstraintResolver<Context> {

	protected:

		typedef ConstraintResolver<Context> super;

		// the sets to be used for in/out states
		const SetIDType& Ain;
		const SetIDType& Aout;

		Collector& collector;

	public:

		BasicInOutConstraintResolver(CBA& cba, const SetIDType& Ain, const SetIDType& Aout, Collector& collector)
			: super(cba), Ain(Ain), Aout(Aout), collector(collector) {}

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


	template<typename SetIDType, typename Collector, typename Context>
	class BasicInConstraintResolver : public BasicInOutConstraintResolver<SetIDType, Collector, Context> {

		typedef BasicInOutConstraintResolver<SetIDType, Collector, Context> super;

	public:

		BasicInConstraintResolver(CBA& cba, const SetIDType& Ain, const SetIDType& Aout, Collector& collector)
			: super(cba, Ain, Aout, collector) {}


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
					auto B_cond = this->context.getSet(B(), l_cond, ctxt);
					this->connectSetsIf(true, B_cond, this->Aout, cond, ctxt, this->Ain, stmt, ctxt, constraints);

				} else if (ifStmt->getElseBody() == stmt) {

					// connect out of condition with in of body if condition may be false
					auto l_cond = this->context.getLabel(cond);
					auto B_cond = this->context.getSet(B(), l_cond, ctxt);
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
				auto B_cond = this->context.getSet(B(), l_cond, ctxt);
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

	template<typename SetIDType, typename Collector, typename Context>
	class BasicOutConstraintResolver : public BasicInOutConstraintResolver<SetIDType, Collector, Context> {

		typedef BasicInOutConstraintResolver<SetIDType, Collector, Context> super;

	public:

		BasicOutConstraintResolver(CBA& cba, const SetIDType& Ain, const SetIDType& Aout, Collector& collector)
			: super(cba, Ain, Aout, collector) {}


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
					auto R_ret = this->context.getSet(Rout(), l_ret, ctxt);
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
			auto B_cond = this->context.getSet(B(), l_cond, ctxt);
			this->connectSetsIf(true,  B_cond, this->Aout, stmt->getThenBody(), ctxt, this->Aout, stmt, ctxt, constraints);
			this->connectSetsIf(false, B_cond, this->Aout, stmt->getElseBody(), ctxt, this->Aout, stmt, ctxt, constraints);
		}

		void visitWhileStmt(const WhileStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {
			// link out of condition to out if condition may ever become false
			auto cond = stmt->getCondition();
			auto l_cond = this->context.getLabel(cond);
			auto B_cond = this->context.getSet(B(), l_cond, ctxt);
			this->connectSetsIf(false, B_cond, this->Aout, cond, ctxt, this->Aout, stmt, ctxt, constraints);
		}

		void visitNode(const NodeAddress& node, const Context& ctxt, Constraints& res) {
			assert_fail() << "Unsupported Node Type encountered: " << node->getNodeType();
		}
	};



} // end namespace cba
} // end namespace analysis
} // end namespace insieme
