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

	using std::set;
	using std::vector;

	const TypedSetType<Callable> C("C");
	const TypedSetType<Callable> c("c");

	const TypedSetType<Location> R("R");
	const TypedSetType<Location> r("r");

	const TypedSetType<core::ExpressionPtr> D("D");
	const TypedSetType<core::ExpressionPtr> d("d");

	const TypedSetType<Formula> A("A");
	const TypedSetType<Formula> a("a");

	const TypedSetType<bool> B("B");
	const TypedSetType<bool> b("b");

	const TypedSetType<Reachable> Rin("Rin");		// the associated term is reached
	const TypedSetType<Reachable> Rout("Rout");		// the associated term is left

	const StateSetType Sin("Sin");			// in-state of statements
	const StateSetType Sout("Sout");		// out-state of statements
	const StateSetType Stmp("Stmp");		// temporary states of statements (assignment only)

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

		using namespace utils::set_constraint_2;

		template<typename T, int pos, int size>
		struct gen_context {
			void operator()(const vector<T>& values, vector<Sequence<T,size>>& res, array<T,size>& data) const {
				static const gen_context<T,pos-1,size> inner;
				for(auto cur : values) {
					data[pos-1] = cur;
					inner(values, res, data);
				}
			}
		};

		template<typename T, int size>
		struct gen_context<T, 0,size> {
			void operator()(const vector<T>& values, vector<Sequence<T,size>>& res, array<T,size>& data) const {
				res.push_back(data);
			}
		};


		template<typename T, unsigned s>
		void generateSequences(const vector<T>& values, vector<Sequence<T, s>>& res) {
			array<T,s> tmp;
			gen_context<T, s, s>()(values, res, tmp);
		}


		vector<Callable> getAllCallableTerms(CBAContext& context, const StatementAddress& root) {

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

			vector<Sequence<Label, 2>> callContexts;
			generateSequences(labels, callContexts);

			// compute resulting set
			vector<Callable> res;

			// TODO: collect potential thread contexts
			vector<ThreadID> threads;
			threads.push_back(ThreadID());		// default thread

			// create all thread contexts
			vector<Sequence<ThreadID, 2>> threadContexts;
			generateSequences(threads, threadContexts);


//			std::cout << "Sites:                " << labels << "\n";
			std::cout << "Number of call sites:      " << labels.size() << "\n";
			std::cout << "Number of call contexts:   " << labels.size()*labels.size() << " = " << callContexts.size() << "\n";
			std::cout << "Number of threads:         " << threads.size() << "\n";
			std::cout << "Number of thread contexts: " << threads.size()*threads.size() << " = " << threadContexts.size() << "\n";
			std::cout << "Total number of contexts:  " << callContexts.size() * threadContexts.size() << "\n";
//			std::cout << "Contexts:\n" << join("\n", contexts) << "\n\n";


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
			return res;
		}

		template<typename T>
		class BasicDataFlowConstraintCollector : public ConstraintResolver {

			typedef ConstraintResolver super;

		protected:

			// the list of all terms in the targeted code
			const vector<Callable>& callables;

			// the two set types to deal with
			const TypedSetType<T>& A;		// the value set (labels -> values)
			const TypedSetType<T>& a;		// the variable set (variables -> values)

		public:

			BasicDataFlowConstraintCollector(CBAContext& context, const TypedSetType<T>& A, const TypedSetType<T>& a, const vector<Callable>& callables)
				: super(context), callables(callables), A(A), a(a) { };

			void visitCompoundStmt(const CompoundStmtAddress& compound, const Context& ctxt, Constraints& constraints) {
				// just collect constraints from elements
				// TODO: add data flow constraints
				for(auto cur : compound) visit(cur, ctxt, constraints);
			}

			void visitDeclarationStmt(const DeclarationStmtAddress& decl, const Context& ctxt, Constraints& constraints) {

				// add constraint r(var) \subset C(init)
				auto var = context.getVariable(decl->getVariable());
				auto l_init = context.getLabel(decl->getInitialization());

				// TODO: distinguish between control and data flow!
				auto a_var = context.getSet(a, var, ctxt);
				auto A_init = context.getSet(A, l_init, ctxt);
				constraints.add(subset(A_init, a_var));		// TODO: add context (passed by argument)

				// finally, add constraints for init expression
				visit(decl->getInitialization(), ctxt, constraints);
			}

			void visitIfStmt(const IfStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

				// decent into sub-expressions
				visit(stmt->getCondition(), ctxt, constraints);
				visit(stmt->getThenBody(), ctxt, constraints);
				visit(stmt->getElseBody(), ctxt, constraints);

			}

			void visitWhileStmt(const WhileStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

				// decent into sub-expressions
				visit(stmt->getCondition(), ctxt, constraints);
				visit(stmt->getBody(), ctxt, constraints);
			}

			void visitReturnStmt(const ReturnStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

				// link the value of the result set to lambda body

				// find lambda body
				LambdaAddress lambda = getEnclosingLambda(stmt);
				if (!lambda) {
					std::cout << "Encountered free return!!\n";
					return;		// return is not bound
				}

				// and add constraints for return value
				visit(stmt->getReturnExpr(), ctxt, constraints);

				auto l_retVal = context.getLabel(stmt->getReturnExpr());
				auto l_body = context.getLabel(lambda->getBody());

				auto A_retVal = context.getSet(A, l_retVal, ctxt);
				auto A_body = context.getSet(A, l_body, ctxt);

				// add constraint - forward in case end of return expression is reachable
				auto R_ret = context.getSet(Rout, l_retVal, ctxt);
				constraints.add(subsetIf(Reachable(), R_ret, A_retVal, A_body));

			}

			void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {
				// nothing to do by default => should be overloaded by sub-classes
			}

			void visitVariable(const VariableAddress& variable, const Context& ctxt, Constraints& constraints) {

				// add constraint r(var) \subset C(var)
				auto var = context.getVariable(variable);
				auto l_var = context.getLabel(variable);

				auto a_var = context.getSet(a, var, ctxt);
				auto A_var = context.getSet(A, l_var, ctxt);

				constraints.add(subset(a_var, A_var));
			}

			void visitLambdaExpr(const LambdaExprAddress& lambda, const Context& ctxt, Constraints& constraints) {
				// nothing to do here => magic happens at call site
			}

			void visitBindExpr(const BindExprAddress& bind, const Context& ctxt, Constraints& constraints) {

				// process bound arguments recursively
				for (auto cur : bind->getBoundExpressions()) {
					visit(cur, ctxt, constraints);
				}

			}

			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

				// add constraints for function and argument expressions
				visit(call->getFunctionExpr(), ctxt, constraints);
				for(auto arg : call) visit(arg, ctxt, constraints);

				// get values of function
				auto fun = call->getFunctionExpr();
				auto C_fun = context.getSet(C, context.getLabel(fun), ctxt);

				// value set of call
				auto l_call = context.getLabel(call);
				auto A_call = context.getSet(A, l_call, ctxt);

				// prepare inner call context
				Context innerCallContext = ctxt;

				// a utility resolving constraints for the given expression
				auto addConstraints = [&](const Callable& target, bool fixed) {

					// only searching for actual code
					const auto& expr = target.definition;
					assert(expr.isa<LambdaExprPtr>() || expr.isa<BindExprPtr>());

					// check whether the term is a function with the right number of arguments
					auto funType = expr->getType().isa<FunctionTypePtr>();
					if(funType->getParameterTypes().size() != call.size()) return;		// this is not a potential function

					// handle lambdas
					if (auto lambda = expr.isa<LambdaExprAddress>()) {

						// add constraints for arguments
						for(std::size_t i=0; i<call.size(); i++) {

							// add constraint: t \in C(fun) => C(arg) \subset r(param)
							auto l_arg = context.getLabel(call[i]);
							auto param = context.getVariable(lambda->getParameterList()[i]);

							auto A_arg = context.getSet(A, l_arg, ctxt);
							auto a_param = context.getSet(a, param, innerCallContext);
							constraints.add((fixed) ? subset(A_arg, a_param) : subsetIf(target, C_fun, A_arg, a_param));
						}

						// add constraint for result value
						auto l_ret = context.getLabel(lambda->getBody());
						auto A_ret = context.getSet(A, l_ret, innerCallContext);
						constraints.add((fixed)? subset(A_ret, A_call) : subsetIf(target, C_fun, A_ret, A_call));

						// add function body constraints for targeted call context
						this->visit(lambda->getBody(), innerCallContext, constraints);

					// handle bind
					} else if (auto bind = expr.isa<BindExprAddress>()) {
						auto body = bind->getCall();
						auto parameters = bind.as<BindExprPtr>()->getParameters();

						// add constraints for arguments of covered call expression
						for (auto cur : body) {

							int index = getParameterIndex(parameters, cur);

							// handle bind parameter
							if (index >= 0) {		// it is a bind parameter

								// link argument to parameter
								auto l_out = context.getLabel(call[index]);
								auto l_in  = context.getLabel(cur);

								auto A_out = context.getSet(A, l_out, ctxt);
								auto A_in  = context.getSet(A, l_in, innerCallContext);
								constraints.add((fixed) ? subset(A_out, A_in) : subsetIf(target, C_fun, A_out, A_in));

							} else {

								// handle captured parameter
								// link value of creation context to body-argument
								auto l_arg = context.getLabel(cur);

								auto A_src = context.getSet(A, l_arg, target.context);
								auto A_trg = context.getSet(A, l_arg, innerCallContext);
								constraints.add((fixed) ? subset(A_src, A_trg) : subsetIf(target, C_fun, A_src, A_trg));
							}
						}

						// add constraints for result value
						auto l_body = context.getLabel(body);
						auto A_ret = context.getSet(A, l_body, innerCallContext);
						constraints.add((fixed) ? subset(A_ret, A_call) : subsetIf(target, C_fun, A_ret, A_call));

						// add function body constraints for targeted bind expression
						this->visit(body, innerCallContext, constraints);
					}
				};

				// no constraints for literals ...
				if (fun.isa<LiteralPtr>()) return;

				// if function expression is a lambda or bind => do not iterate through all callables, callable is fixed
				if (auto lambda = fun.isa<LambdaExprAddress>()) {
					addConstraints(Callable(lambda), true);
					return;
				}

				if (auto bind = fun.isa<BindExprAddress>()) {
					addConstraints(Callable(bind, ctxt), true);
					return;
				}

				// fix pass-by-value semantic - by considering all potential terms
				innerCallContext.callContext <<= l_call;
				for(auto cur : callables) {
					addConstraints(cur, false);
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

			ControlFlowConstraintCollector(CBAContext& context, const vector<Callable>& callables)
				: super(context, C, c, callables) { };

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

		class ConstantConstraintCollector : public BasicDataFlowConstraintCollector<core::ExpressionPtr> {

			typedef BasicDataFlowConstraintCollector<core::ExpressionPtr> super;

		public:

			ConstantConstraintCollector(CBAContext& context, const vector<Callable>& callables)
				: super(context, D, d, callables) { };

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

			ArithmeticConstraintCollector(CBAContext& context, const vector<Callable>& callables, NodeManager& mgr)
				: super(context, cba::A, cba::a, callables), base(mgr.getLangBasic()) { };

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

			BooleanConstraintCollector(CBAContext& context, const vector<Callable>& callables, NodeManager& mgr)
				: super(context, cba::B, cba::b, callables), base(mgr.getLangBasic()) { };

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

			ReferenceConstraintCollector(CBAContext& context, const vector<Callable>& callables)
				: BasicDataFlowConstraintCollector<Location>(context, R, r, callables) { };

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
		vector<Location> getAllLocations(CBAContext& context, const StatementAddress& root) {
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


		template<typename SetIDType, typename Derived>
		class BaseImperativeConstraintCollector : public ConstraintResolver {

			typedef ConstraintResolver super;

		protected:

			// the list of all function terms in the processed fragment
			const vector<Callable>& callables;

		private:

			// the sets to be used for in/out states
			const SetIDType& Ain;
			const SetIDType& Aout;

		public:

			BaseImperativeConstraintCollector(CBAContext& context, const vector<Callable>& callables, const SetIDType& Ain, const SetIDType& Aout)
				: ConstraintResolver(context), callables(callables), Ain(Ain), Aout(Aout) {};

			// ----------- Expressions -----------------------------------------------------------------------------------------------------

			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

				// recursively process sub-expressions
				visit(call->getFunctionExpr(), ctxt, constraints);
				for(auto arg : call) visit(arg, ctxt, constraints);

				// otherwise default handling
				//  - link in of call with in of arguments
				//  - link out of arguments with in of function
				//  - link out of function with out of call

				auto l_call = context.getLabel(call);

				// link in of call with in of arguments
				for(auto arg : call) {
					auto l_arg = context.getLabel(arg);
					connectStateSets(Ain, l_call, ctxt, Ain, l_arg, ctxt, constraints);
				}

				// and the function
				auto l_fun = context.getLabel(call->getFunctionExpr());
				connectStateSets(Ain, l_call, ctxt, Ain, l_fun, ctxt, constraints);


				// create inner call context
				Context innerCallContext = ctxt;

				// get set of potential target functions
				auto C_fun = context.getSet(C, l_fun, ctxt);

				// a utility resolving constraints for the called function
				auto addConstraints = [&](const Callable& target, bool fixed) {
					auto expr = target.definition;

					// check correct number of arguments
					if (call.size() != expr.getType().as<FunctionTypePtr>()->getParameterTypes().size()) {
						// this is not a valid target
						return;
					}

					// ---- Effect of arguments => in of function ----

					// get body
					StatementAddress body;
					if (auto lambda = expr.isa<LambdaExprAddress>()) {
						body = lambda->getBody();
					} else if (auto bind = expr.isa<BindExprAddress>()) {
						body = bind->getCall();
					} else {
						std::cout << "Unsupported potential target of type " << expr->getNodeType() << " encountered.";
						assert(false && "Unsupported potential call target.");
					}

					// get label for body
					auto l_fun = context.getLabel(body);

					// forward effects of argument evaluation
					for (auto arg : call) {
						auto l_arg = context.getLabel(arg);
						if (fixed) {
							this->connectStateSets(Aout, l_arg, ctxt, Ain, l_fun, innerCallContext, constraints);
						} else {
							this->connectStateSetsIf(target, C_fun, Aout, l_arg, ctxt, Ain, l_fun, innerCallContext, constraints);
						}
					}

					// also add effects of function-expression evaluation
					auto l_call_fun = context.getLabel(call->getFunctionExpr());
					if (fixed) {
						this->connectStateSets(Aout, l_call_fun, ctxt, Ain, l_fun, innerCallContext, constraints);
					} else {
						this->connectStateSetsIf(target, C_fun, Aout, l_call_fun, ctxt, Ain, l_fun, innerCallContext, constraints);
					}

					// ---- Effect of function => out of call ---

					// link out of fun with call out
					if (fixed) {
						this->connectStateSets(Aout, l_fun, innerCallContext, Aout, l_call, ctxt, constraints);
					} else {
						this->connectStateSetsIf(target, C_fun, Aout, l_fun, innerCallContext, Aout, l_call, ctxt, constraints);
					}

					// process function body
					this->visit(body, innerCallContext, constraints);

				};


				// handle call target
				auto fun = call->getFunctionExpr();

				if (fun.isa<LiteralPtr>()) {

					// - here we are assuming side-effect free literals -

					// just connect out of arguments to call-out
					for (auto arg : call) {
						auto l_arg = context.getLabel(arg);
						connectStateSets(Aout, l_arg, ctxt, Aout, l_call, ctxt, constraints);
					}

					// and the function
					connectStateSets(Aout, l_fun, ctxt, Aout, l_call, ctxt, constraints);

				} else if (auto lambda = fun.isa<LambdaExprAddress>()) {

					// direct call => handle directly
					addConstraints(Callable(lambda), true);

				} else if (auto bind = fun.isa<BindExprAddress>()) {

					// direct call of bind => handle directly
					addConstraints(Callable(bind, ctxt), true);

				} else {

					// create new call-context
					innerCallContext.callContext <<= l_call;

					// indirect call => dynamic dispatching required
					for(auto cur : callables) {
						addConstraints(cur,false);
					}

				}

			}

			void visitBindExpr(const BindExprAddress& bind, const Context& ctxt, Constraints& constraints) {

				auto boundExprs = bind->getBoundExpressions();

				// special case: no bound expressions
				if (boundExprs.empty()) {
					// forward in to out and be done
					visitExpression(bind, ctxt, constraints);
					return;
				}

				auto l_bind = context.getLabel(bind);

				// evaluate bound expressions
				for(auto& cur : boundExprs) {
					auto l_arg = context.getLabel(cur);

					//   - forward in to expression in
					connectStateSets(Ain, l_bind, ctxt, Ain, l_arg, ctxt, constraints);

					//   - process bound expressions
					visit(cur, ctxt, constraints);

					//   - forward out of expression to out of bind
					connectStateSets(Aout, l_arg, ctxt, Aout, l_bind, ctxt, constraints);
				}
			}

			void visitExpression(const ExpressionAddress& expr, const Context& ctxt, Constraints& constraints) {
				// in the general case, not much happening => connect in and out
				auto label = context.getLabel(expr);
				connectStateSets(Ain, label, ctxt, Aout, label, ctxt, constraints);
			}


			// ----------- Statements -----------------------------------------------------------------------------------------------------

			void visitCompoundStmt(const CompoundStmtAddress& compound, const Context& ctxt, Constraints& constraints) {

				// special handling for empty compound = NoOp
				if (compound.empty()) {
					auto l = context.getLabel(compound);
					connectStateSets(Ain, l, ctxt, Aout, l, ctxt, constraints);
					return;
				}

				// connect contained statements
				for(std::size_t i = 0; i < compound.size()-1; i++) {
					// skip connection if current stmt is a return / break / continue statement
					switch(compound[i]->getNodeType()) {
					case NT_ReturnStmt: case NT_BreakStmt: case NT_ContinueStmt: continue;
					default: break;
					}

					// connect those
					auto la = context.getLabel(compound[i]);
					auto lb = context.getLabel(compound[i+1]);
					connectStateSets(Aout, la, ctxt, Ain, lb, ctxt, constraints);
				}

				// connect in-state with in of first statement
				auto l = context.getLabel(compound);
				auto la = context.getLabel(compound[0]);
				connectStateSets(Ain, l, ctxt, Ain, la, ctxt, constraints);

				// connect out-state of last statement with out-state
				auto lb = context.getLabel(compound[compound.size()-1]);
				connectStateSets(Aout, lb, ctxt, Aout, l, ctxt, constraints);

				// and add constraints of all inner statements
				for(auto cur : compound) {
					visit(cur, ctxt, constraints);
				}
			}

			void visitDeclarationStmt(const DeclarationStmtAddress& decl, const Context& ctxt, Constraints& constraints) {

				// just connect in with init value and out of innit value with out
				auto l = context.getLabel(decl);
				auto l_init = context.getLabel(decl->getInitialization());

				connectStateSets(Ain, l, ctxt, Ain, l_init, ctxt, constraints);
				connectStateSets(Aout, l_init, ctxt, Aout, l, ctxt, constraints);

				// and create constraints for initialization value
				visit(decl->getInitialization(), ctxt, constraints);
			}

			void visitReturnStmt(const ReturnStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

				// connect Ain with Ain of return expression
				auto l_ret = context.getLabel(stmt);
				auto l_val = context.getLabel(stmt->getReturnExpr());
				connectStateSets(Ain, l_ret, ctxt, Ain, l_val, ctxt, constraints);

				// find enclosing lambda
				LambdaAddress lambda = getEnclosingLambda(stmt);
				if (!lambda) {
					std::cout << "WARNING: encountered free return!\n";
					return;
				}

				// connect Aout of value with Aout of function
				auto l_fun = context.getLabel(lambda->getBody());
				connectStateSets(Aout, l_val, ctxt, Aout, l_fun, ctxt, constraints);

				// fix constraints for return expr
				visit(stmt->getReturnExpr(), ctxt, constraints);
			}


			void visitContinueStmt(const ContinueStmtAddress& cur, const Context& ctxt, Constraints& constraints) {
				// do not connect in with out
			}

			void visitBreakStmt(const BreakStmtAddress& cur, const Context& ctxt, Constraints& constraints) {
				// do not connect in with out
			}

			void visitIfStmt(const IfStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

				// get some labels
				auto l_if = context.getLabel(stmt);
				auto l_cond = context.getLabel(stmt->getCondition());
				auto l_then = context.getLabel(stmt->getThenBody());
				auto l_else = context.getLabel(stmt->getElseBody());

				auto B_cond = context.getSet(B, l_cond, ctxt);

				// -- conditional has to be always evaluated --
				connectStateSets(Ain, l_if, ctxt, Ain, l_cond, ctxt, constraints);

				// connect Aout of condition to then and else branch
				connectStateSetsIf(true,  B_cond, Aout, l_cond, ctxt, Ain, l_then, ctxt, constraints);
				connectStateSetsIf(false, B_cond, Aout, l_cond, ctxt, Ain, l_else, ctxt, constraints);

				// connect Aout of then and else branch with Aout of if
				connectStateSetsIf(true,  B_cond, Aout, l_then, ctxt, Aout, l_if, ctxt, constraints);
				connectStateSetsIf(false, B_cond, Aout, l_else, ctxt, Aout, l_if, ctxt, constraints);

				// add constraints recursively
				visit(stmt->getCondition(), ctxt, constraints);
				visit(stmt->getThenBody(), ctxt, constraints);
				visit(stmt->getElseBody(), ctxt, constraints);
			}

			void visitWhileStmt(const WhileStmtAddress& stmt, const Context& ctxt, Constraints& constraints) {

				// get some labels
				auto l_while = context.getLabel(stmt);
				auto l_cond = context.getLabel(stmt->getCondition());
				auto l_body = context.getLabel(stmt->getBody());

				auto B_cond = context.getSet(B, l_cond, ctxt);

				// do the wiring
				connectStateSets(Ain, l_while, ctxt, Ain, l_cond, ctxt, constraints);
				connectStateSetsIf(true, B_cond, Aout, l_cond, ctxt, Ain, l_body, ctxt, constraints);
				connectStateSets(Aout, l_body, ctxt, Ain, l_cond, ctxt, constraints);
				connectStateSetsIf(false, B_cond, Aout, l_cond, ctxt, Aout, l_while, ctxt, constraints);

				// add constraints recursively
				visit(stmt->getCondition(), ctxt, constraints);
				visit(stmt->getBody(), ctxt, constraints);
			}

			void visitNode(const NodeAddress& node, const Context& ctxt, Constraints& constraints) {
				std::cout << "Reached unsupported Node Type: " << node->getNodeType() << "\n";
				assert(false);
			}

		private:

			void connectStateSets(const SetIDType& a, Label al, const Context& ac, const SetIDType& b, Label bl, const Context& bc, Constraints& constraints) {
				static_cast<Derived*>(this)->connectStateSets(a,al,ac,b,bl,bc,constraints);
			}

			template<typename E>
			void connectStateSetsIf(const E& value, const TypedSetID<E>& set, const SetIDType& a, Label al, const Context& ac, const SetIDType& b, Label bl, const Context& bc, Constraints& constraints) {
				static_cast<Derived*>(this)->connectStateSetsIf(value,set,a,al,ac,b,bl,bc,constraints);
			}

		};


		class ReachableConstraintCollector : public BaseImperativeConstraintCollector<TypedSetType<Reachable>, ReachableConstraintCollector> {

			typedef BaseImperativeConstraintCollector<TypedSetType<Reachable>, ReachableConstraintCollector> super;

			StatementAddress root;

		public:

			ReachableConstraintCollector(CBAContext& context, const vector<Callable>& callables, const StatementAddress& root)
				: super(context, callables, Rin, Rout), root(root) { }

			virtual void visit(const StatementAddress& node, const Context& ctxt, Constraints& constraints) {

				// make sure root is reachable
				if (node == root && ctxt == Context()) {
					auto l = context.getLabel(node);
					auto R = context.getSet(Rin, l, ctxt);
					constraints.add(elem(Reachable(), R));
				}

				// and all the other constraints
				super::visit(node, ctxt, constraints);
			}

			void connectStateSets(
						const TypedSetType<Reachable>& a, Label al, const Context& ac,
						const TypedSetType<Reachable>& b, Label bl, const Context& bc,
						Constraints& constraints
					) {

				auto A = context.getSet(a, al, ac);
				auto B = context.getSet(b, bl, bc);
				constraints.add(subset(A,B));
			}

			template<typename E>
			void connectStateSetsIf(
						const E& value, const TypedSetID<E>& set,
						const TypedSetType<Reachable>& a, Label al, const Context& ac,
						const TypedSetType<Reachable>& b, Label bl, const Context& bc,
						Constraints& constraints
					) {

				auto A = context.getSet(a, al, ac);
				auto B = context.getSet(b, bl, bc);
				constraints.add(subsetIf(value, set, A, B));
			}

		};


		template<typename T>
		class ImperativeStateConstraintCollector : public BaseImperativeConstraintCollector<StateSetType, ImperativeStateConstraintCollector<T>> {

			typedef BaseImperativeConstraintCollector<StateSetType, ImperativeStateConstraintCollector<T>> super;

			const TypedSetType<T>& dataSet;

			// list of all memory location in the processed fragment
			const vector<Location>& locations;

		public:

			ImperativeStateConstraintCollector(CBAContext& context, const TypedSetType<T>& dataSet,
					const vector<Location>& locations, const vector<Callable>& callables)
				: super(context, callables, Sin, Sout), dataSet(dataSet), locations(locations) {};


			void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
				const auto& base = call->getNodeManager().getLangBasic();

				// two special cases:
				auto fun = call.as<CallExprPtr>()->getFunctionExpr();

				//  A) - assignment operations (ref.assign)
				if (base.isRefAssign(fun)) {

					// ---- link S_in to S_in of arguments
					auto l_call = this->context.getLabel(call);
					auto l_rhs = this->context.getLabel(call[0]);
					auto l_lhs = this->context.getLabel(call[1]);
					connectStateSets(Sin, l_call, ctxt, Sin, l_rhs, ctxt, constraints);
					connectStateSets(Sin, l_call, ctxt, Sin, l_lhs, ctxt, constraints);


					// ---- S_out of args => S_tmp of call
					connectStateSets(Sout, l_rhs, ctxt, Stmp, l_call, ctxt, constraints);
					connectStateSets(Sout, l_lhs, ctxt, Stmp, l_call, ctxt, constraints);

					// ---- combine S_tmp to S_out ...

					// add rule: loc \in R[rhs] => A[lhs] \sub Sout[call]
					auto R_rhs = this->context.getSet(R, l_rhs, ctxt);
					for(auto loc : locations) {

						// TODO: add context

						// if loc is in R(target) then add D[rhs] to Sout[loc]
						auto A_value = this->context.getSet(dataSet, l_lhs, ctxt);
						auto S_out = this->context.getSet(Sout, l_call, ctxt, loc, dataSet);
						constraints.add(subsetIf(loc, R_rhs, A_value, S_out));
					}


					// add rule: |R[rhs]\{loc}| > 0 => Stmp[call] \sub Sout[call]
					for(auto loc : locations) {

						// get Sin set		TODO: add context to locations
						auto s_tmp = this->context.getSet(Stmp, l_call, ctxt, loc, dataSet);
						auto s_out = this->context.getSet(Sout, l_call, ctxt, loc, dataSet);

						// if more than 1 reference may be assigned => everything that comes in goes out
						constraints.add(subsetIfReducedBigger(R_rhs, loc, 0, s_tmp, s_out));
					}

					// process arguments
					this->visit(call[0], ctxt, constraints);
					this->visit(call[1], ctxt, constraints);

					// done
					return;
				}

				//  B) - read operation (ref.deref)
				if (base.isRefDeref(fun)) {

					// read value from memory location
					auto l_call = this->context.getLabel(call);
					auto l_trg = this->context.getLabel(call[0]);
					auto R_trg = this->context.getSet(R, l_trg, ctxt);
					for(auto loc : locations) {

						// TODO: add context

						// if loc is in R(target) then add Sin[A,trg] to A[call]
						auto S_in = this->context.getSet(Sin, l_call, ctxt, loc, dataSet);
						auto A_call = this->context.getSet(dataSet, l_call, ctxt);
						constraints.add(subsetIf(loc, R_trg, S_in, A_call));
					}

					// and process default procedure (no return here)
				}

				// everything else is treated using the default procedure
				super::visitCallExpr(call, ctxt, constraints);
			}

			void connectStateSets(const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) {

				// general handling - Sin = Sout
				for(auto loc : locations) {

					// get Sin set		TODO: add context to locations
					auto s_in = this->context.getSet(a, al, ac, loc, dataSet);
					auto s_out = this->context.getSet(b, bl, bc, loc, dataSet);

					// state information entering the set is also leaving it
					constraints.add(subset(s_in, s_out));

				}
			}

			template<typename E>
			void connectStateSetsIf(const E& value, const TypedSetID<E>& set, const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) {

				// general handling - Sin = Sout
				for(auto loc : locations) {

					// get Sin set		TODO: add context to locations
					auto s_in = this->context.getSet(a, al, ac, loc, dataSet);
					auto s_out = this->context.getSet(b, bl, bc, loc, dataSet);

					// state information entering the set is also leaving it
					constraints.add(subsetIf(value, set, s_in, s_out));

				}
			}

		};

		template<typename T>
		void addImperativeConstraints(CBAContext& context, const StatementAddress& root,
				const TypedSetType<T>& type, const vector<Location>& locations, const vector<Callable>& funs,
				const Context& ctxt, Constraints& res) {
			ImperativeStateConstraintCollector<T>(context, type, locations, funs).visit(root, ctxt, res);
		}
	}


	Constraints generateConstraints(CBAContext& context, const StatementPtr& stmt) {
		NodeManager& mgr = stmt->getNodeManager();

		// create resulting list of constraints
		Constraints res;

		// let constraint collector do the job
		StatementAddress root(stmt);

		Context initContext;

		// TODO: resolve dependencies between collectors automatically
		vector<Callable> funs = getAllCallableTerms(context, root);

		// TODO: can also be used to collect all call and thread contexts
		ReachableConstraintCollector(context, funs, root).visit(root, initContext, res);

		// add other constraints
		ControlFlowConstraintCollector(context, funs).visit(root, initContext, res);
		ConstantConstraintCollector(context, funs).visit(root, initContext, res);
		ReferenceConstraintCollector(context, funs).visit(root, initContext, res);
		ArithmeticConstraintCollector(context, funs, mgr).visit(root, initContext, res);
		BooleanConstraintCollector(context, funs, mgr).visit(root, initContext, res);

		// and the imperative constraints
		auto locations = getAllLocations(context, root);
		addImperativeConstraints(context, root, C, locations, funs, initContext, res);
		addImperativeConstraints(context, root, D, locations, funs, initContext, res);
		addImperativeConstraints(context, root, R, locations, funs, initContext, res);
		addImperativeConstraints(context, root, A, locations, funs, initContext, res);
		addImperativeConstraints(context, root, B, locations, funs, initContext, res);


		// done
		return res;


	}


	Solution solve(const Constraints& constraints) {
		// just use the utils solver
		return utils::set_constraint_2::solve(constraints);
	}

	using namespace utils::set_constraint_2;

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
			string setName = std::get<0>(cur.first)->getName();
			auto pos = getAddress(std::get<1>(cur.first));
			out << "\n\t" << cur.second
					<< " [label=\"" << cur.second << " = " << setName
							<< "[l" << std::get<1>(cur.first) << " = " << pos->getNodeType() << " : " << pos << " : " << std::get<2>(cur.first) << "]\""
					<< "];";
		}

		for(auto cur : stateSets) {
			string setName = std::get<0>(cur.first)->getName();
			string dataName = std::get<4>(cur.first)->getName();
			auto pos = getAddress(std::get<1>(cur.first));
			out << "\n\t" << cur.second
					<< " [label=\"" << cur.second << " = " << setName << "-" << dataName << "@" << std::get<4>(cur.first)
						<< "[l" << std::get<1>(cur.first) << " = " << pos->getNodeType() << " : " << pos << " : " << std::get<2>(cur.first) << "]\""
					<< "];";
		}

		// link sets
		for(auto cur : constraints) {
			out << "\n\t";
			cur->writeDotEdge(out);
		}

		out << "\n}\n";

	}

	void CBAContext::plot(const Constraints& constraints, const Solution& ass, std::ostream& out) const {

		auto getAddress = [&](const Label l)->StatementAddress {
			for(auto cur : this->labels) {
				if (cur.second == l) return cur.first;
			}
			for(auto cur : this->vars) {
				if (cur.second == l) return cur.first;
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
					<< " = " << solutions[cur.second] << "\"];";
		}

		for(auto cur : stateSets) {
			string setName = std::get<0>(cur.first)->getName();
			string dataName = std::get<4>(cur.first)->getName();
			auto pos = getAddress(std::get<1>(cur.first));
			out << "\n\t" << cur.second
					<< " [label=\"" << cur.second << " = " << setName << "-" << dataName << "@" << std::get<4>(cur.first)
						<< "[l" << std::get<1>(cur.first) << " = " << pos->getNodeType() << " : " << pos << " : " << std::get<2>(cur.first) << "]"
					<< " = " << solutions[cur.second] << "\"];";
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
