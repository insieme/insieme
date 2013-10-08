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

#include "insieme/analysis/cba/analysis/boolean.h"
#include "insieme/analysis/cba/analysis/reachability.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/set_constraint/solver2.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;
	using namespace insieme::utils::set_constraint_2;

	// ----------------------------------------------------------------------------------------------------------------------------
	//
	//														Imperative Constraints
	//
	// ----------------------------------------------------------------------------------------------------------------------------

	// TODO: rewrite Collector => Derived


	template<typename InSetIDType, typename OutSetIDType, typename Collector, typename Context>
	class BasicInOutConstraintGenerator : public ConstraintGenerator<Context> {

	protected:

		typedef ConstraintGenerator<Context> super;

		// the sets to be used for in/out states
		const InSetIDType& Ain;
		const OutSetIDType& Aout;

		Collector& collector;

		CBA& cba;

	public:

		BasicInOutConstraintGenerator(CBA& cba, const InSetIDType& Ain, const OutSetIDType& Aout, Collector& collector)
			: super(cba), Ain(Ain), Aout(Aout), collector(collector), cba(cba) {}

	protected:

		template<typename SetTypeA, typename SetTypeB>
		void connectSets(const SetTypeA& a, const StatementAddress& al, const Context& ac, const SetTypeB& b, const StatementAddress& bl, const Context& bc, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			connectStateSets(a, cba.getLabel(al), ac, b, cba.getLabel(bl), bc, constraints);
		}

		template<typename E, typename SetTypeA, typename SetTypeB>
		void connectSetsIf(const E& value, const TypedSetID<E>& set, const SetTypeA& a, const StatementAddress& al, const Context& ac, const SetTypeB& b, const StatementAddress& bl, const Context& bc, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			connectStateSetsIf(value, set, a, cba.getLabel(al), ac, b, cba.getLabel(bl), bc, constraints);
		}

		template<typename SetTypeA, typename SetTypeB>
		void connectStateSets(const SetTypeA& a, Label al, const Context& ac, const SetTypeB& b, Label bl, const Context& bc, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			collector.connectStateSets(a,al,ac,b,bl,bc,constraints);
		}

		template<typename E, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIf(const E& value, const TypedSetID<E>& set, const SetTypeA& a, Label al, const Context& ac, const SetTypeB& b, Label bl, const Context& bc, Constraints& constraints) {
			// filter out invalid contexts
			if (!cba.isValid(ac) || !cba.isValid(bc)) return;
			collector.connectStateSetsIf(value,set,a,al,ac,b,bl,bc,constraints);
		}

	};


	template<typename InSetIDType, typename OutSetIDType, typename Collector, typename Context>
	class BasicInConstraintGenerator : public BasicInOutConstraintGenerator<InSetIDType, OutSetIDType, Collector, Context> {

		typedef BasicInOutConstraintGenerator<InSetIDType, OutSetIDType, Collector, Context> super;

		CBA& cba;

	public:

		BasicInConstraintGenerator(CBA& cba, const InSetIDType& Ain, const OutSetIDType& Aout, Collector& collector)
			: super(cba, Ain, Aout, collector), cba(cba) {}

		void connectCallToBody(const CallExprAddress& call, const Context& callCtxt, const StatementAddress& body, const Context& trgCtxt, const Callee& callee,  Constraints& constraints) {

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

			// check whether call-site is within a bind
			bool isCallWithinBind = (!call.isRoot() && call.getParentNode()->getNodeType() == NT_BindExpr);
			auto bind = (isCallWithinBind) ? call.getParentAddress().as<BindExprAddress>() : BindExprAddress();

			// get label for the body expression
			auto l_body = cba.getLabel(body);

			// get labels for call-site
			auto l_fun = cba.getLabel(call->getFunctionExpr());
			auto F_call = cba.getSet(F, l_fun, callCtxt);

			// add effect of function-expression-evaluation (except within bind calls)
			if (!isCallWithinBind) this->connectStateSetsIf(callee, F_call, this->Aout, l_fun, callCtxt, this->Ain, l_body, trgCtxt, constraints);

			// just connect the effect of the arguments of the call-site with the in of the body call statement
			for(auto arg : call) {

				// skip bound parameters
				if (bind && bind->isBoundExpression(arg)) continue;

				// add effect of argument
				auto l_arg = cba.getLabel(arg);
				this->connectStateSetsIf(callee, F_call, this->Aout, l_arg, callCtxt, this->Ain, l_body, trgCtxt, constraints);
			}

			// special case: if this is a bind with no parameters
			if (bind && bind->getParameters()->empty()) {
				// connect in of call site with in of body
				this->connectStateSetsIf(callee, F_call, this->Ain, l_call, callCtxt, this->Ain, l_body, trgCtxt, constraints);
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
				Callee callee(bind);
				for (const Caller& cur : cba.getCallSiteManager().getCaller(callee)) {

					// may be reached from any context
					for(auto& l : cba.getDynamicCallLabels()) {

						// nobody calls context 0
						if (l != 0 && ctxt.callContext.startsWith(0)) continue;

						Context srcCtxt = ctxt;
						srcCtxt.callContext >>= l;

						// connect call site with body
						connectCallToBody(cur, srcCtxt, call, ctxt, bind, constraints);
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
							connectCallToBody(cur.getCall(), srcCtxt, stmt, ctxt, callee, constraints);
						}

					} else {

						// no context switch required
						connectCallToBody(cur.getCall(), ctxt, stmt, ctxt, callee, constraints);

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
					auto l_cond = cba.getLabel(cond);
					auto B_cond = cba.getSet(B, l_cond, ctxt);
					this->connectSetsIf(true, B_cond, this->Aout, cond, ctxt, this->Ain, stmt, ctxt, constraints);

				} else if (ifStmt->getElseBody() == stmt) {

					// connect out of condition with in of body if condition may be false
					auto l_cond = cba.getLabel(cond);
					auto B_cond = cba.getSet(B, l_cond, ctxt);
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
				auto l_cond = cba.getLabel(cond);
				auto B_cond = cba.getSet(B, l_cond, ctxt);
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

			// handle for stmt
			if (auto forStmt = parent.isa<ForStmtAddress>()) {

				// check which part of a while the current node is
				auto body = forStmt->getBody();
				if (body == stmt) {

					// connect out of declaration, end and step to in of body
					this->connectSets(this->Aout, forStmt->getDeclaration(), ctxt, this->Ain, body, ctxt, constraints);
					this->connectSets(this->Aout, forStmt->getEnd(),         ctxt, this->Ain, body, ctxt, constraints);
					this->connectSets(this->Aout, forStmt->getStep(),        ctxt, this->Ain, body, ctxt, constraints);

					// also, since it is a loop, the out of the loop body is the in of the next iteration
					// TODO: consider continues and breaks!
					this->connectSets(this->Aout, body, ctxt, this->Ain, body, ctxt, constraints);

				} else {

					// connect in of for with in of declaration, end and step (current stmt)
					this->connectSets(this->Ain, forStmt, ctxt, this->Ain, stmt, ctxt, constraints);

				}
				return;
			}

			assert_fail() << "Unsupported parent type encountered: " << parent->getNodeType();
		}
	};

	template<typename InSetIDType, typename OutSetIDType, typename Collector, typename Context>
	class BasicOutConstraintGenerator : public BasicInOutConstraintGenerator<InSetIDType, OutSetIDType, Collector, Context> {

		typedef BasicInOutConstraintGenerator<InSetIDType, OutSetIDType, Collector, Context> super;

		CBA& cba;

	public:

		BasicOutConstraintGenerator(CBA& cba, const InSetIDType& Ain, const OutSetIDType& Aout, Collector& collector)
			: super(cba, Ain, Aout, collector), cba(cba) {}


		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

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

				// skip handling of literals
				if (targets[0].isLiteral()) {
					// we assume literals have no affect
					this->connectStateSets(this->Ain, l_call, ctxt, this->Aout, l_call, ctxt, constraints);
					return;
				}

				// get label of body
				auto l_body = cba.getLabel(targets[0].getBody());

				// just connect out of body with out of call
				this->connectStateSets(this->Aout, l_body, innerCtxt, this->Aout, l_call, ctxt, constraints);

				// and done
				return;
			}

			// if there are multiple targets => check the value of the function expression

			// get set of potential target functions
			auto l_fun = cba.getLabel(call->getFunctionExpr());
			auto F_fun = cba.getSet(F, l_fun, ctxt);

			for(const Callee& cur : targets) {

				// get label of body
				auto l_body = cba.getLabel(cur.getBody());

				// just connect out of body with out of call if function fits
				this->connectStateSetsIf(cur, F_fun, this->Aout, l_body, innerCtxt, this->Aout, l_call, ctxt, constraints);
			}

		}

		void visitBindExpr(const BindExprAddress& bind, const Context& ctxt, Constraints& constraints) {

			// out-effects are only influenced by bound parameters
			auto l_cur = cba.getLabel(bind);
			for(const auto& arg : bind->getBoundExpressions()) {
				auto l_arg = cba.getLabel(arg);
				this->connectStateSets(this->Aout, l_arg, ctxt, this->Aout, l_cur, ctxt, constraints);
			}

			// and no more ! (in particular not the effects of the inner call)
		}

		void visitExpression(const ExpressionAddress& expr, const Context& ctxt, Constraints& constraints) {
			// for most expressions: just connect in and out
			auto l_cur = cba.getLabel(expr);
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

			auto l_body = cba.getLabel(stmt);
			visitDepthFirstPrunable(stmt, [&](const StatementAddress& stmt) {
				// prune inner functions
				if (stmt.isa<LambdaExprAddress>()) return true;

				// visit return statements
				if (auto returnStmt = stmt.isa<ReturnStmtAddress>()) {

					// connect value of return statement with body value
					auto l_ret = cba.getLabel(returnStmt);
					auto R_ret = cba.getSet(Rout, l_ret, ctxt);
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
			auto l_cond = cba.getLabel(stmt->getCondition());
			auto B_cond = cba.getSet(B, l_cond, ctxt);
			this->connectSetsIf(true,  B_cond, this->Aout, stmt->getThenBody(), ctxt, this->Aout, stmt, ctxt, constraints);
			this->connectSetsIf(false, B_cond, this->Aout, stmt->getElseBody(), ctxt, this->Aout, stmt, ctxt, constraints);
		}

		void visitWhileStmt(const WhileStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {
			// link out of condition to out if condition may ever become false
			auto cond = stmt->getCondition();
			auto l_cond = cba.getLabel(cond);
			auto B_cond = cba.getSet(B, l_cond, ctxt);
			this->connectSetsIf(false, B_cond, this->Aout, cond, ctxt, this->Aout, stmt, ctxt, constraints);
		}

		void visitForStmt(const ForStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {
			// link out of body with out of for stmt
			auto body = stmt->getBody();
			// TODO: consider continues and breaks!
			this->connectSets(this->Aout, body, ctxt, this->Aout, stmt, ctxt, constraints);
		}

		void visitNode(const NodeAddress& node, const Context& ctxt, Constraints& res) {
			assert_fail() << "Unsupported Node Type encountered: " << node->getNodeType();
		}
	};



} // end namespace cba
} // end namespace analysis
} // end namespace insieme
