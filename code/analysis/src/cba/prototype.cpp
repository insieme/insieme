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

namespace insieme {
namespace analysis {
namespace cba {

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

		using namespace utils::set_constraint;

		class BasicDataFlowConstraintCollector : public IRVisitor<void, Address> {

			typedef IRVisitor<void,Address> super;

		protected:

			CBAContext& context;
			Constraints& constraints;

			// the list of all terms in the targeted code
			std::vector<pair<Value, ExpressionAddress>> terms;

			// the two set types to deal with
			SetType A;		// the value set (labels -> values)
			SetType a;		// the variable set (variables -> values)

		public:

			BasicDataFlowConstraintCollector(CBAContext& context, Constraints& contraints, const StatementAddress& root, SetType A, SetType a)
				: context(context), constraints(contraints), A(A), a(a) {

				// collect all terms in the code
				visitDepthFirst(root, [&](const ExpressionAddress& cur) {
					// TODO: also add all recursion variations
					terms.push_back(std::make_pair(context.getValue(cur), cur));
				});

			};

			void visitCompoundStmt(const CompoundStmtAddress& compound) {
				// just collect constraints from elements
				// TODO: add data flow constraints
				for(auto cur : compound) visit(cur);
			}

			void visitDeclarationStmt(const DeclarationStmtAddress& decl) {

				// add constraint r(var) \subset C(init)
				auto var = context.getVariable(decl->getVariable());
				auto l_init = context.getLabel(decl->getInitialization());

				// TODO: distinguish between control and data flow!
				auto a_var = context.getSet(a, var);
				auto A_init = context.getSet(A, l_init);
				constraints.insert(subset(A_init, a_var));		// TODO: add context (passed by argument)

				// finally, add constraints for init expression
				visit(decl->getInitialization());
			}

			void visitReturnStmt(const ReturnStmtAddress& stmt) {

				// link the value of the result set to lambda body

				// find lambda body
				LambdaAddress lambda = getEnclosingLambda(stmt);
				if (!lambda) {
					std::cout << "Encountered free return!!\n";
					return;		// return is not bound
				}

				// and add constraints for return value
				visit(stmt->getReturnExpr());

				auto l_retVal = context.getLabel(stmt->getReturnExpr());
				auto l_body = context.getLabel(lambda->getBody());

				auto A_retVal = context.getSet(A, l_retVal);
				auto A_body = context.getSet(A, l_body);

				// add constraint
				constraints.insert(subset(A_retVal, A_body));

			}

			void visitVariable(const VariableAddress& variable) {

				// TODO: distinguish between control and data flow

				// add constraint r(var) \subset C(var)
				auto var = context.getVariable(variable);
				auto l_var = context.getLabel(variable);

				auto a_var = context.getSet(a, var);
				auto A_var = context.getSet(A, l_var);

				constraints.insert(subset(a_var, A_var));
			}

			void visitLambdaExpr(const LambdaExprAddress& lambda) {
				// TODO: handle recursions

				// and add constraints for the body
				visit(lambda->getBody());
			}

			void visitCallExpr(const CallExprAddress& call) {

				// add constraints for function and argument expressions
				visit(call->getFunctionExpr());
				for(auto arg : call) visit(arg);

				// get values of function
				auto fun = call->getFunctionExpr();
				auto C_fun = context.getSet(C, context.getLabel(fun));

				// value set of call
				auto A_call = context.getSet(A, context.getLabel(call));

				// a utility resolving constraints for the given expression
				auto addConstraints = [&](Value t, const ExpressionAddress& expr) {

					// only searching for actual code
					if (!expr.isa<LambdaExprPtr>() && !expr.isa<BindExprPtr>()) return;

					// check whether the term is a function with the right number of arguments
					// TODO: also check type?
					auto funType = expr.as<ExpressionPtr>()->getType().isa<FunctionTypePtr>();
					if(funType->getParameterTypes().size() != call.size()) return;		// this is not a potential function

					assert(expr.isa<LambdaExprPtr>() && "Binds not implemented yet!");

					// add constraints for arguments
					auto lambda = expr.isa<LambdaExprAddress>();
					for(std::size_t i=0; i<call.size(); i++) {

						// add constraint: t \in C(fun) => C(arg) \subset r(param)
						auto l_arg = context.getLabel(call[i]);
						auto param = context.getVariable(lambda->getParameterList()[i]);

						auto A_arg = context.getSet(A, l_arg);
						auto a_param = context.getSet(a, param);
						constraints.insert((t==0) ? subset(A_arg, a_param) : subsetIf(t, C_fun, A_arg, a_param));
					}

					// add constraint for result value
					auto l_ret = context.getLabel(lambda->getBody());
					auto A_ret = context.getSet(A, l_ret);
					constraints.insert((t==0)? subset(A_ret, A_call) : subsetIf(t, C_fun, A_ret, A_call));

				};

				// no constraints for literals ...
				if (fun.isa<LiteralPtr>()) return;

				// if function expression is a lambda or bind => do not iterate through all terms, term is fixed
				if (fun.isa<LambdaExprPtr>() || fun.isa<BindExprPtr>()) {
					addConstraints(0, fun);
					return;
				}

				// fix pass-by-value semantic - by considering all potential terms
				for(auto cur : terms) {

					auto t = cur.first;
					auto expr = cur.second;

					addConstraints(t, expr);

				}
			}

//			void visitNode(const NodeAddress& node) {
//				std::cout << "Reached unsupported Node Type: " << node->getNodeType() << "\n";
//				assert(false);
//			}

		};


		class ControlFlowConstraintCollector : public BasicDataFlowConstraintCollector {

			typedef BasicDataFlowConstraintCollector super;

		public:

			ControlFlowConstraintCollector(CBAContext& context, Constraints& constraints, const StatementAddress& root)
				: BasicDataFlowConstraintCollector(context, constraints, root, C, c) { };

			void visitLiteral(const LiteralAddress& literal) {

				// and default handling
				super::visitLiteral(literal);

				// only interrested in functions ...
				if (!literal->getType().isa<FunctionTypePtr>()) return;

				// add constraint literal \in C(lit)
				auto value = context.getValue(literal);
				auto l_lit = context.getLabel(literal);

				auto C_lit = context.getSet(C, l_lit);
				constraints.insert(elem(value, C_lit));

			}

			void visitLambdaExpr(const LambdaExprAddress& lambda) {

				// and default handling
				super::visitLambdaExpr(lambda);

				// add constraint lambda \in C(lambda)
				auto label = context.getLabel(lambda);
				auto value = context.getValue(lambda);

				constraints.insert(elem(value, context.getSet(C, label)));

				// TODO: handle recursions

			}

		};

		class ConstantConstraintCollector : public BasicDataFlowConstraintCollector {

			typedef BasicDataFlowConstraintCollector super;

		public:

			ConstantConstraintCollector(CBAContext& context, Constraints& constraints, const StatementAddress& root)
				: BasicDataFlowConstraintCollector(context, constraints, root, D, d) { };

			void visitLiteral(const LiteralAddress& literal) {

				// and default handling
				super::visitLiteral(literal);

				// not interested in functions
				if (literal->getType().isa<FunctionTypePtr>()) return;

				// add constraint literal \in C(lit)
				auto value = context.getValue(literal);
				auto l_lit = context.getLabel(literal);

				auto D_lit = context.getSet(D, l_lit);
				constraints.insert(elem(value, D_lit));

			}

		};

		class ReferenceConstraintCollector : public BasicDataFlowConstraintCollector {

			typedef BasicDataFlowConstraintCollector super;

		public:

			ReferenceConstraintCollector(CBAContext& context, Constraints& constraints, const StatementAddress& root)
				: BasicDataFlowConstraintCollector(context, constraints, root, R, r) { };

			void visitLiteral(const LiteralAddress& literal) {

				// and default handling
				super::visitLiteral(literal);

				// only interested in memory location constructors
				if (!isMemoryConstructor(literal)) return;

				// add constraint literal \in R(lit)
				auto value = context.getLocation(literal);
				auto l_lit = context.getLabel(literal);

				auto R_lit = context.getSet(R, l_lit);
				constraints.insert(elem(value, R_lit));

			}

			void visitCallExpr(const CallExprAddress& call) {

				// and default handling
				super::visitCallExpr(call);

				// introduce memory location in some cases
				if (!isMemoryConstructor(call)) return;

				// add constraint location \in R(call)
				auto value = context.getLocation(call);
				auto l_lit = context.getLabel(call);

				auto R_lit = context.getSet(R, l_lit);
				constraints.insert(elem(value, R_lit));
			}

		};

		class ImperativeConstraintCollector : public IRVisitor<void, Address> {

			typedef IRVisitor<void,Address> super;

			CBAContext& context;
			Constraints& constraints;

			std::vector<SetType> dataSets;

			// list of all memory location in the processed fragment
			std::vector<Location> locations;

		public:

			ImperativeConstraintCollector(CBAContext& context, Constraints& contraints, const StatementAddress& root, const std::vector<SetType>& dataSets)
				: context(context), constraints(contraints), dataSets(dataSets) {

				// collect all memory location constructors
				visitDepthFirst(root, [&](const ExpressionAddress& cur) {
					// TODO: add context info to locations
					if (isMemoryConstructor(cur)) {
						locations.push_back(context.getLocation(cur));
					}
				});

			};

			void visitLiteral(const LiteralAddress& lit) {

				// fix: Sin \subset Sout
				auto label = context.getLabel(lit);
				Context c;
				Thread t;
				connectStateSets(Sin, label, c, t, Sout, label, c, t);

			}

			void visitVariable(const VariableAddress& var) {

				// fix: Sin \subset Sout
				auto label = context.getLabel(var);
				Context c;
				Thread t;
				connectStateSets(Sin, label, c, t, Sout, label, c, t);

			}

			void visitCallExpr(const CallExprAddress& call) {
				const auto& base = call->getNodeManager().getLangBasic();

				// recursively process sub-expressions
				visit(call->getFunctionExpr());
				for(auto arg : call) visit(arg);

				// otherwise default handling
				//  - link in of call with in of arguments
				//  - link out of arguments with in of function
				//  - link out of function with out of call

				auto l_call = context.getLabel(call);

				Context c;
				Thread t;

				// link in of call with in of arguments
				for(auto arg : call) {
					auto l_arg = context.getLabel(arg);
					connectStateSets(Sin, l_call, c, t, Sin, l_arg, c, t);
				}

				// and the function
				auto l_fun = context.getLabel(call->getFunctionExpr());
				connectStateSets(Sin, l_call, c, t, Sin, l_fun, c, t);


				// TODO: consider dynamic dispatching
				// link out of arguments with in of function candidates
				if (!call->getFunctionExpr().isa<LambdaExprPtr>() && !call->getFunctionExpr().isa<LiteralPtr>()) {
					std::cout << "WARNING: unsupported call target of type: " << call->getFunctionExpr()->getNodeType() << "\n";
					return;
				}


				if (call->getFunctionExpr().isa<LambdaExprPtr>()){ 		// for now

					// ---- Effect of arguments => in of function ----
					auto lambda = call->getFunctionExpr().as<LambdaExprAddress>();

					auto l_fun = context.getLabel(lambda->getBody());		// here we have to go through list of functions ...
					for (auto arg : call) {
						auto l_arg = context.getLabel(arg);
						connectStateSets(Sout, l_arg, c, t, Sin, l_fun, c, t);
					}

					// also add effects of function-expression evaluation
					connectStateSets(Sout, context.getLabel(lambda), c, t, Sin, l_fun, c, t);

					// ---- Effect of function => out of call ---

					// link out of fun with call out
					connectStateSets(Sout, l_fun, c, t, Sout, l_call, c, t);

					// process function body
					visit(lambda->getBody());

					return;
				}

				// ---- side-effects ----

				// special case: assignments
				if (core::analysis::isCallOf(call.as<CallExprPtr>(), base.getRefAssign())) {

					// ---- S_out of args => S_as of call

					for (auto arg : call) {
						auto l_arg = context.getLabel(arg);
						connectStateSets(Sout, l_arg, c, t, Sas, l_call, c, t);
					}
					// and the function
					connectStateSets(Sout, l_fun, c, t, Sas, l_call, c, t);

					// ---- combine S_as to S_out ...

					// add rule: loc \in R[rhs] => A[lhs] \sub Sout[call]
					auto l_rhs = context.getLabel(call[0]);
					auto l_lhs = context.getLabel(call[1]);
					auto R_rhs = context.getSet(R, l_rhs);
					for(auto loc : locations) {

						// TODO: add context

						for (auto A : dataSets) {
							// if loc is in R(target) then add D[rhs] to Sout[loc]
							auto A_value = context.getSet(A, l_lhs);
							auto S_out = context.getSet(Sout, l_call, c, t, loc, A, c, t);
							constraints.insert(subsetIf(loc, R_rhs, A_value, S_out));
						}
					}


					// add rule: |R[rhs]\{loc}| > 0 => Sas[call] \sub Sout[call]
					for(auto loc : locations) {

						for (auto type : dataSets) {

							// get Sin set		TODO: add context to locations
							auto s_as = context.getSet(Sas, l_call, c, t, loc, type);
							auto s_out = context.getSet(Sout, l_call, c, t, loc, type);

							// if more than 1 reference may be assigned => everything that comes in goes out
							constraints.insert(subsetIfReducedBigger(R_rhs, loc, 0, s_as, s_out));
						}
					}

				} else {

					// just connect out of arguments to call-out - assume no effects on state
					for (auto arg : call) {
						auto l_arg = context.getLabel(arg);
						connectStateSets(Sout, l_arg, c, t, Sout, l_call, c, t);
					}

					// and the function
					connectStateSets(Sout, l_fun, c, t, Sout, l_call, c, t);
				}

				// special case: read
				if (core::analysis::isCallOf(call.as<CallExprPtr>(), base.getRefDeref())) {

					// read value from memory location
					auto l_trg = context.getLabel(call[0]);
					auto R_trg = context.getSet(R, l_trg);
					for(auto loc : locations) {

						// TODO: add context

						// if loc is in R(target) then add Sin[A,trg] to A[call]
						for (auto A : dataSets) {
							auto S_in = context.getSet(Sin, l_call, c, t, loc, A, c, t);
							auto A_call = context.getSet(A, l_call);
							constraints.insert(subsetIf(loc, R_trg, S_in, A_call));
						}
					}

				}

			}

			void visitLambdaExpr(const LambdaExprAddress& cur) {

				// fix: Sin \subset Sout
				auto label = context.getLabel(cur);
				Context c;
				Thread t;
				connectStateSets(Sin, label, c, t, Sout, label, c, t);


				// TODO: deal with recursion

				// + process body
				for(auto def : cur->getDefinition()) {
					visit(def->getLambda()->getBody());
				}
			}

			void visitCompoundStmt(const CompoundStmtAddress& compound) {

				Context c;
				Thread t;

				// special handling for empty compound = NoOp
				if (compound.empty()) {
					auto l = context.getLabel(compound);
					connectStateSets(Sin, l, c, t, Sout, l, c, t);
					return;
				}

				// connect contained statements
				for(std::size_t i = 0; i < compound.size()-1; i++) {
					auto la = context.getLabel(compound[i]);
					auto lb = context.getLabel(compound[i+1]);
					connectStateSets(Sout, la, c, t, Sin, lb, c, t);
				}

				// connect in-state with in of first statement
				auto l = context.getLabel(compound);
				auto la = context.getLabel(compound[0]);
				connectStateSets(Sin, l, c, t, Sin, la, c, t);

				// connect out-state of last statement with out-state
				auto lb = context.getLabel(compound[compound.size()-1]);
				connectStateSets(Sout, lb, c, t, Sout, l, c, t);

				// and add constraints of all inner statements
				for(auto cur : compound) {
					visit(cur);
				}
			}

			void visitDeclarationStmt(const DeclarationStmtAddress& decl) {

				// just connect in with init value and out of innit value with out
				auto l = context.getLabel(decl);
				auto l_init = context.getLabel(decl->getInitialization());

				Context c;
				Thread t;

				connectStateSets(Sin, l, c, t, Sin, l_init, c, t);
				connectStateSets(Sout, l_init, c, t, Sout, l, c, t);

				// and create constraints for initialization value
				visit(decl->getInitialization());
			}

			void visitReturnStmt(const ReturnStmtAddress& stmt) {
				Context c;
				Thread t;

				// connect Sin with Sin of return expression
				auto l_ret = context.getLabel(stmt);
				auto l_val = context.getLabel(stmt->getReturnExpr());
				connectStateSets(Sin, l_ret, c, t, Sin, l_val, c, t);

				// find enclosing lambda
				LambdaAddress lambda = getEnclosingLambda(stmt);
				if (!lambda) {
					std::cout << "WARNING: encountered free return!\n";
					return;
				}

				// connect Sout of value with Sout of function
				auto l_fun = context.getLabel(lambda->getBody());
				connectStateSets(Sout, l_val, c, t, Sout, l_fun, c, t);

				// fix constraints for return expr
				visit(stmt->getReturnExpr());
			}

			void visitIfStmt(const IfStmtAddress& stmt) {
				Context c;
				Thread t;

				// get some labels
				auto l_if = context.getLabel(stmt);
				auto l_cond = context.getLabel(stmt->getCondition());
				auto l_then = context.getLabel(stmt->getThenBody());
				auto l_else = context.getLabel(stmt->getElseBody());

				// -- conditional has to be always evaluated --
				connectStateSets(Sin, l_)


				// add constraints recursively
				visit(stmt->getCondition());
				visit(stmt->getThenBody());
				visit(stmt->getElseBody());
			}

			void visitNode(const NodeAddress& node) {
				std::cout << "Reached unsupported Node Type: " << node->getNodeType() << "\n";
				assert(false);
			}

		private:

			void connectStateSets(SetType a, Label al, const Context& ac, const Thread& at, SetType b, Label bl, const Context& bc, const Thread& bt) {

				// general handling - Sin = Sout
				for(auto loc : locations) {

					for (auto type : dataSets) {

						// get Sin set		TODO: add context to locations
						auto s_in = context.getSet(a, al, ac, at, loc, type);
						auto s_out = context.getSet(b, bl, bc, bt, loc, type);

						// state information entering the set is also leaving it
						constraints.insert(subset(s_in, s_out));

					}
				}
			}

		};

	}


	Constraints generateConstraints(CBAContext& context, const StatementPtr& stmt) {

		// create resulting list of constraints
		Constraints res;

		// let constraint collector do the job
		StatementAddress root(stmt);

		// TODO: resolve dependencies between collectors automatically
		ControlFlowConstraintCollector(context, res, root).visit(root);
		ConstantConstraintCollector(context, res, root).visit(root);
		ReferenceConstraintCollector(context, res, root).visit(root);
		ImperativeConstraintCollector(context, res, root, toVector(C,D,R)).visit(root);


		// done
		return res;


	}


	Solution solve(const Constraints& constraints) {
		// just use the utils solver
		return utils::set_constraint::solve(constraints);
	}

	core::ExpressionSet getValuesOf(CBAContext& context, const Solution& solution, const core::ExpressionAddress& expr, SetType set) {
		core::ExpressionSet res;

		auto label = context.getLabel(expr);
		auto pos = solution.find(context.getSet(set, label));

		// check whether there is a result
		if (pos == solution.end()) return res;

		// convert result set into expressions
		for (auto i : pos->second) {
			res.insert(context.getExpr(i));
		}

		// done
		return res;
	}

	core::ExpressionSet getValuesOf(CBAContext& context, const Solution& solution, const core::VariableAddress& varAdr, SetType set) {
		core::ExpressionSet res;

		auto var = context.getVariable(varAdr);
		auto pos = solution.find(context.getSet(set, var));

		// check whether there is a result
		if (pos == solution.end()) return res;

		// convert result set into expressions
		for (auto i : pos->second) {
			res.insert(context.getExpr(i));
		}

		// done
		return res;
	}


	namespace {


		const char* getName(SetType type) {
			switch(type) {
			case C: return "C";
			case c: return "c";
			case D: return "D";
			case d: return "d";
			case R: return "R";
			case r: return "r";
			case Sin: return "Sin";
			case Sout: return "Sout";
			case Sas: return "Sas";
			}
			return "?";
		}

	}


	void CBAContext::plot(const Constraints& constraints, std::ostream& out) const {


		auto getAddress = [&](const Label l)->StatementAddress {
			for(auto cur : this->labels) {
				if (cur.second == l) return cur.first;
			}
			for(auto cur : this->vars) {
				if (cur.second == l) return cur.first;
			}
			return StatementAddress();
		};


		out << "digraph G {";

		// name sets
		for(auto cur : sets) {
			string setName = getName(std::get<0>(cur.first));
			auto pos = getAddress(std::get<1>(cur.first));
			out << "\n\ts" << cur.second << " [label=\"s" << cur.second << " = " << setName << "[l" << std::get<1>(cur.first) << " = " << pos->getNodeType() << " : " << pos << "]\"];";
		}

		for(auto cur : stateSets) {
			string setName = getName(std::get<0>(cur.first));
			string dataName = getName(std::get<5>(cur.first));
			auto pos = getAddress(std::get<1>(cur.first));
			out << "\n\ts" << cur.second << " [label=\"s" << cur.second << " = " << setName << "-" << dataName << "[l" << std::get<1>(cur.first) << " = " << pos->getNodeType() << " : " << pos << "]\"];";
		}

		// link sets
		for(auto cur : constraints) {
			switch(cur.getKind()) {
			case utils::set_constraint::Constraint::Elem:
				out << "\n\te" << cur.getE() << " -> " << cur.getA() << " [label=\"in\"];";
				break;
			case utils::set_constraint::Constraint::Subset:
				out << "\n\t" << cur.getB() << " -> " << cur.getC() << " [label=\"sub\"];";
				break;
			case utils::set_constraint::Constraint::SubsetIfElem:
				out << "\n\t" << cur.getB() << " -> " << cur.getC() << " [label=\"if " << cur.getE() << " in " << cur.getA() << "\"];";
				break;
			case utils::set_constraint::Constraint::SubsetIfBigger:
				out << "\n\t" << cur.getB() << " -> " << cur.getC() << " [label=\"if |" << cur.getA() << "| > " << cur.getE() << "\"];";
				break;
			case utils::set_constraint::Constraint::SubsetIfReducedBigger:
				out << "\n\t" << cur.getB() << " -> " << cur.getC() << " [label=\"if |" << cur.getA() << "\\{" << cur.getF() << "}| > " << cur.getE() << "\"];";
				break;
			}
		}

		out << "\n}\n";

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
