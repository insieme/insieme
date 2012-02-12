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

#include "insieme/analysis/features/feature_tryout.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/lua/lua.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/printer/lua_printer.h"

#include "insieme/analysis/features/type_features.h"

namespace insieme {
namespace analysis {
namespace features {


	namespace {

		namespace arith=core::arithmetic;


		class SimulationContext : private core::IRVisitor<int64_t> {

			utils::map::PointerMap<core::VariablePtr, int64_t> values;
			//std::map<core::VariablePtr, int64_t, compare_target<core::VariablePtr>> values;

			utils::map::PointerMap<core::VariablePtr, core::LambdaPtr> recLambdas;

		public:

			/**
			 * Create a new, empty simulation context.
			 */
			SimulationContext() {}

			/**
			 * Creates a copy of the given context.
			 */
			SimulationContext(const SimulationContext& other)
				: values(other.values), recLambdas(recLambdas) {}

			/**
			 * Initializes a estimated context for the given node.
			 */
			SimulationContext(const core::NodePtr& node) {

				const auto& basic = node->getNodeManager().getLangBasic();

				// init context for the given node
				uint64_t memLocation = 0;
				for_each(core::analysis::getFreeVariables(node), [&](const core::VariablePtr& var) {
					if (var->getType()->getNodeType() == core::NT_RefType) {
						// for reference variables the memory location is stored
						values[var] = memLocation;
						// big enough to be far away + some alignment offset
						memLocation += (1l<<30) + 64;
					} else if (basic.isInt(var->getType())) {
						// for other variables, the value is stored
						values[var] = 100;
					} else {
						// no initialization is required in this case
					}
				});
			}

			void setValue(const core::VariablePtr& var, int64_t value) {
				values[var] = value;
			}

			void unsetValue(const core::VariablePtr& var) {
				values.erase(var);
			}


			int64_t evalExpr(const core::ExpressionPtr& expr) {
				return visit(expr);
			}

		public:

			int64_t visitVariable(const core::VariablePtr& var) {
				auto pos = values.find(var);
				if (pos != values.end()) {
					return pos->second;
				}

				// ensure that this var is not a reference variable
				assert(var->getType()->getNodeType() != core::NT_RefType && "All reference variables have to be known!");
				assert(false && "Trying to read undefined variable!");
				return 0;
			}

			int64_t visitLiteral(const core::LiteralPtr& lit) {
				auto& basic = lit->getNodeManager().getLangBasic();
				const core::TypePtr& type = lit->getType();
				const string& value = lit->getStringValue();
				if (basic.isInt(type)) {
					return utils::numeric_cast<int64_t>(value);
				}
				if (basic.isBool(type)) {
					return basic.isTrue(lit);
				}

				// unable to process other literals
				std::cerr << "Unsupported literal: " << *lit << " of type " << *type << "\n";
				assert(false && "Unsupported literal encountered!");
				return 0;
			}


			int64_t visitCallExpr(const core::CallExprPtr& call) {
				const auto& basic = call->getNodeManager().getLangBasic();
				const auto& fun = call->getFunctionExpr();

				std::size_t numOps = call->getArgumentList().size();

				// deal with standard arithmetic expressions
				if (numOps == 2 && basic.isArithOp(fun)) {

					int64_t lhs = visit(call->getArgument(0));
					int64_t rhs = visit(call->getArgument(1));

					switch(basic.getOperator(static_pointer_cast<core::LiteralPtr>(fun))) {
					case core::lang::BasicGenerator::Add: return lhs + rhs;
					case core::lang::BasicGenerator::Sub: return lhs - rhs;
					case core::lang::BasicGenerator::Mul: return lhs * rhs;
					case core::lang::BasicGenerator::Div: return lhs / rhs;
					case core::lang::BasicGenerator::And: return lhs & rhs;
					case core::lang::BasicGenerator::Or:  return lhs | rhs;
					case core::lang::BasicGenerator::Xor: return lhs ^ rhs;
					case core::lang::BasicGenerator::LShift: return lhs << rhs;
					case core::lang::BasicGenerator::RShift: return lhs >> rhs;
					default: break;
					}

					assert(false && "Unsupported arithmetic operator!");
				}

				// deal with increment / decrement operators
				if (numOps == 1 && basic.isArithOp(fun)) {

					assert(call->getArgument(0)->getNodeType() != core::NT_Variable
							&& "Can only increment variables!");

					core::VariablePtr var = static_pointer_cast<core::VariablePtr>(call->getArgument(0));

					switch(basic.getOperator(static_pointer_cast<core::LiteralPtr>(fun))) {
					case core::lang::BasicGenerator::PreInc: 	return ++values[var];
					case core::lang::BasicGenerator::PostInc: 	return values[var]++;
					case core::lang::BasicGenerator::PreDec: 	return --values[var];
					case core::lang::BasicGenerator::PostDec: 	return values[var]--;
					default: break;
					}

					assert(false && "Unsupported arithmetic operator!");
				}

				// deal with comparison operators
				if (numOps == 2 && basic.isCompOp(fun)) {

					int64_t lhs = visit(call->getArgument(0));
					int64_t rhs = visit(call->getArgument(1));

					switch(basic.getOperator(static_pointer_cast<core::LiteralPtr>(fun))) {
					case core::lang::BasicGenerator::Eq: 	return lhs == rhs;
					case core::lang::BasicGenerator::Ne: 	return lhs != rhs;
					case core::lang::BasicGenerator::Lt: 	return lhs < rhs;
					case core::lang::BasicGenerator::Le: 	return lhs <= rhs;
					case core::lang::BasicGenerator::Gt: 	return lhs > rhs;
					case core::lang::BasicGenerator::Ge: 	return lhs >= rhs;
					default: break;
					}
				}


				// boolean operators
				if (numOps == 1 && basic.isBoolLNot(fun)) {
					return !visit(call->getArgument(0));
				}

				if (numOps == 2 && basic.isLogicOp(fun)) {

					int64_t lhs = visit(call->getArgument(0));
					int64_t rhs = 0;
					if (basic.isBoolLAnd(fun) || basic.isBoolLOr(fun)) {
						rhs = visit(core::transform::evalLazy(call->getNodeManager(), call->getArgument(0)));
					} else {
						rhs = visit(call->getArgument(1));
					}

					switch(basic.getOperator(static_pointer_cast<core::LiteralPtr>(fun))) {
					case core::lang::BasicGenerator::LAnd: 	return lhs && rhs;
					case core::lang::BasicGenerator::LOr: 	return lhs || rhs;
					case core::lang::BasicGenerator::Eq: 	return lhs == rhs;
					case core::lang::BasicGenerator::Ne: 	return lhs != rhs;
					default: break;
					}

				}

				std::cerr << "Unsupported operator encountered: " << *call << " of type " << *call->getType() << "\n";
				assert(false && "Unsupported operator encountered!");
				return 0;
			}

			int64_t visitCastExpr(const core::CastExprPtr& cast) {
				return visit(cast->getSubExpression());
			}

		};

		enum ReturnStatus {
			Normal, Break, Continue, Return
		};


		class ExecutionSimulator : public core::IRVisitor<ReturnStatus, core::Pointer, SimulationContext&> {

			const core::lang::BasicGenerator& basic;
			CacheModel& model;

		public:

			ExecutionSimulator(const core::lang::BasicGenerator& basic, CacheModel& model)
				: basic(basic), model(model) {}

			int64_t getAddress(const core::ExpressionPtr& target, SimulationContext& context) {

				if (target->getNodeType() == core::NT_Variable) {
					return context.evalExpr(static_pointer_cast<core::VariablePtr>(target));
				}

				if (target->getNodeType() == core::NT_CallExpr) {
					const core::CallExprPtr& call = static_pointer_cast<core::CallExprPtr>(target);

					const core::ExpressionPtr& fun = call->getFunctionExpr();

					if (basic.isArrayRefElem1D(fun) || basic.isVectorRefElem(fun)) {
						// compute position of accessed array element
						int size = getSizeInBytes(core::analysis::getReferencedType(target->getType()));
						int64_t base = getAddress(call->getArgument(0), context);
						int64_t index = context.evalExpr(call->getArgument(1));
						return base + size * index;
					}

					// TODO: add support for structs!
				}
std::cout << "Unsupported target: " << *target << "\n";
				// unknown location
				return -1;
			}

			void processAccess(const core::ExpressionPtr& target, SimulationContext& context) {

				assert(target->getType()->getNodeType() == core::NT_RefType && "Cannot access non-reference type!");

				// simulate memory access
				int64_t location = getAddress(target, context);
				if (location >= 0) {
					model.access(location,
							getSizeInBytes(core::analysis::getReferencedType(target->getType()))
							); // TODO: deduce real size of access
				}
			}


			ReturnStatus visitCallExpr(const core::CallExprPtr& call, SimulationContext& context) {

				// now, deal with called function
				const auto& fun = call->getFunctionExpr();

//	std::cout << "Processing: " << *call << "\n";

				// deal with memory accesses
				if (basic.isRefDeref(fun) || basic.isRefAssign(fun)) {
					// simulate access
					processAccess(call->getArgument(0), context);
					return Normal;
				}

				// start by processing argument list
				visitAllGen(call->getArguments(), context);

				// deal with external literals
				if (fun->getNodeType() == core::NT_Literal) {
					// TODO: add clear-cache option
					return Normal;	// nothing to estimate her ...
				}

				// TODO: deal with recursive function calls

				return Normal;
			}

			ReturnStatus visitReturnStmt(const core::ReturnStmtPtr& cur, SimulationContext& context) {
				return Return;
			}

			ReturnStatus visitBreakStmt(const core::BreakStmtPtr& cur, SimulationContext& context) {
				return Break;
			}

			ReturnStatus visitContinueStmt(const core::ContinueStmtPtr& cur, SimulationContext& context) {
				return Continue;
			}

			ReturnStatus visitCompoundStmt(const core::CompoundStmtPtr& cur, SimulationContext& context) {

				std::size_t numStmts = cur->size();
				ReturnStatus status = Normal;

				for (std::size_t i=0; i<numStmts && status == Normal; i++) {
					status = visit(cur->getStatement(i), context);
				}

				return status;
			}

			ReturnStatus visitIfStmt(const core::IfStmtPtr& cur, SimulationContext& context) {

				if (context.evalExpr(cur->getCondition())) {
					return visit(cur->getThenBody(), context);
				}
				return visit(cur->getElseBody(), context);
			}


			ReturnStatus visitForStmt(const core::ForStmtPtr& cur, SimulationContext& context) {

				int start = context.evalExpr(cur->getStart());
				int end = context.evalExpr(cur->getEnd());
				int step = context.evalExpr(cur->getStep());

				core::VariablePtr iter = cur->getIterator();
				core::StatementPtr body = cur->getBody();

				ReturnStatus status = Normal;
				for (int i=start; i<end && status != Return && status != Break; i+=step) {
					context.setValue(iter,i);
					status = visit(body, context);
				}
				context.unsetValue(iter);
				return (status==Return)?Return:status;
			}

			ReturnStatus visitWhileStmt(const core::WhileStmtPtr& cur, SimulationContext& context) {

				ReturnStatus status = Normal;
				while(status != Return && status != Break && context.evalExpr(cur->getCondition())) {
					status = visit(cur->getBody(), context);
				}
				return (status==Return)?Return:status;
			}

//			AST_TERMINAL(BreakStmt, Statement)
//			AST_TERMINAL(ContinueStmt, Statement)
//			AST_TERMINAL(ReturnStmt, Statement)
//			AST_TERMINAL(DeclarationStmt, Statement)
//			AST_TERMINAL(CompoundStmt, Statement)
//			AST_TERMINAL(WhileStmt, Statement)
//			AST_TERMINAL(ForStmt, Statement)
//			AST_TERMINAL(IfStmt, Statement)
//			AST_TERMINAL(SwitchStmt, Statement)


			// TODO:
			//	- support declarations
			//	- support for, while, if, switch, ...
			//  - record all integer operations (optional improvement)

		};


		class MemorySkeletonExtractor :
			public core::transform::CachedNodeMapping,
			public core::IRVisitor<core::preserve_node_type> {


			core::NodeManager& manager;
			core::IRBuilder builder;
			const core::lang::BasicGenerator& basic;

			// the literal implanted when accessing a variable
			core::LiteralPtr access;


		public:

			MemorySkeletonExtractor(core::NodeManager& manager)
				: manager(manager), builder(manager), basic(manager.getLangBasic()) {

				auto intType = basic.getUInt8();
				auto unit = basic.getUnit();
				auto accessType = builder.functionType(intType, intType, unit);
				access = builder.literal("access", accessType);

			}

			virtual const core::NodePtr resolveElement(const core::NodePtr& ptr) {
				// use internal visitor for resolving the replacement
				core::NodePtr res = visit(ptr);
				return (res)?res:builder.getNoOp();
			}

//			virtual core::NodePtr visit(const core::NodePtr& ptr) {
//				core::NodePtr res = core::IRVisitor<core::preserve_node_type>::visit(ptr);
//				std::cout << "\nConverted: " << *ptr << "\n";
//				std::cout << "To:        " << res << "\n";
//				return res;
//			}
//
//			template<typename T>
//			typename core::Pointer<const T> visit(const core::Pointer<const T>& element) {
//				return core::IRVisitor<core::preserve_node_type>::visit(element);
//			}


		private:

			/**
			 * Tests whether a expression is referencing some memory-access relevant target.
			 * A relevant target is everything within a scalar data structure, hence
			 * an element within a vector or array.
			 */
			bool isRelevantTarget(const core::ExpressionPtr& expr) {
				// rule out variables, literals and constructors
				if (expr->getNodeType() != core::NT_CallExpr) {
					return false;
				}

				const core::CallExprPtr& call = static_pointer_cast<core::CallExprPtr>(expr);
				const auto& fun = call->getFunctionExpr();

				// handle clear cases
				if (basic.isArrayRefElem1D(fun) || basic.isVectorRefElem(fun)) {
					return true;
				}

				// handle accesses to compounds
				if (basic.isCompositeRefElem(fun) || basic.isTupleRefElem(fun)) {
					// check source
					return isRelevantTarget(call->getArgument(0));
				}

				// otherwise it is not a relevant target
				return false;
			}

			core::CallExprPtr createAccess(const core::ExpressionPtr& target) {
				auto pos = builder.intLit(50);
				auto size = builder.intLit(4);
				return builder.callExpr(basic.getUnit(), access, pos, size );
			}


			core::CompoundStmtPtr visitCompoundStmt(const core::CompoundStmtPtr& cur) {

				// build a new compound statement aggregating the skeleton of the current one

				vector<core::StatementPtr> stmts;
				for_each(cur->getStatements(), [&](const core::StatementPtr& stmt) {
					core::StatementPtr mod = this->visit(stmt);
					if (mod) { stmts.push_back(mod); }
				});

				// check whether the compound stmt is empty ...
				if (stmts.empty()) {
					return core::CompoundStmtPtr();
				}

				return builder.compoundStmt(stmts);
			}


			core::ForStmtPtr visitForStmt(const core::ForStmtPtr& stmt) {

				// obtain skeleton of body
				core::CompoundStmtPtr body = visit(stmt->getBody());
				if (!body || body->empty()) {
					// there is nothing left
					return core::ForStmtPtr();
				}

				// start, end and step should be affine anyway => no conversion
				core::VariablePtr iter = stmt->getIterator();
				core::ExpressionPtr start = stmt->getStart();
				core::ExpressionPtr end = stmt->getEnd();
				core::ExpressionPtr step = stmt->getStep();

				return builder.forStmt(iter, start, end , step, body);
			}

			core::WhileStmtPtr visitWhileStmt(const core::WhileStmtPtr& stmt) {

				// obtain skeleton of body
				core::CompoundStmtPtr body = visit(stmt->getBody());
				if (!body || body->empty()) {
					// there is nothing left
					return core::WhileStmtPtr();
				}

				// add condition and re-assemble while loop
				core::ExpressionPtr condition = stmt->getCondition();
				return builder.whileStmt(condition, body);
			}

			core::StatementPtr visitStatement(const core::StatementPtr& stmt) {
				// for all other statements => get list of memory accesses in order
				vector<core::StatementPtr> skeleton;

				// collect all memory accesses
				core::visitDepthFirstPrunable(stmt, [&](const core::CallExprPtr& node)->bool {
					const bool prune = true;

					const auto& fun = node->getFunctionExpr();

					// handle actual memory accesses
					if (basic.isRefDeref(fun) && isRelevantTarget(node->getArgument(0))) {
						// add access to variable
						skeleton.push_back(createAccess(node->getArgument(0)));
						return prune;
					}

					// handle assignment operation
					if (basic.isRefAssign(fun) && isRelevantTarget(node->getArgument(0))) {

						// add access to variable
						skeleton.push_back(createAccess(node->getArgument(0)));

						// still process argument list
						return !prune;
					}

					// TODO: handle unary increment / decrement operators

					// handle function calls
					if (fun->getNodeType() == core::NT_LambdaExpr) {

						// compute skeleton of function
						core::LambdaExprPtr lambda = this->visit(static_pointer_cast<core::LambdaExprPtr>(fun));

						if (!lambda->getBody().empty()) {
							// add function call to skeleton
							skeleton.push_back(builder.callExpr(node->getType(), lambda, node->getArguments()));
						}

						// arguments are processed while processing the call
						return prune;
					}

					// further decent is required
					return !prune;
				});

				// build compound listing all the accesses in order
				if (skeleton.empty()) { return core::StatementPtr(); }

				// reverse execution order since visitor is running in preorder
				std::reverse(skeleton.begin(), skeleton.end());

				// build resulting compound statement containing all accesses
				return builder.compoundStmt(skeleton);
			}


			core::LambdaPtr visitLambdaPtr(const core::LambdaPtr& lambda) {
				// convert body
				core::CompoundStmtPtr body = visit(lambda->getBody());
				if (!body) {
					body = builder.compoundStmt();
				}
				return builder.lambda(lambda->getType(), lambda->getParameters(), body);
			}

			core::LambdaDefinitionPtr visitLambdaDefinition(const core::LambdaDefinitionPtr& def) {

				// build a new definition consisting of skeletons only
				vector<core::LambdaBindingPtr> bindings;
				for_each(def->getDefinitions(), [&](const core::LambdaBindingPtr& cur) {
					bindings.push_back(builder.lambdaBinding(cur->getVariable(), this->visit(cur->getLambda())));
				});
				return builder.lambdaDefinition(bindings);
			}

			core::LambdaExprPtr visitLambdaExpr(const core::LambdaExprPtr& lambda) {

				// convert definition
				core::LambdaDefinitionPtr def = visit(lambda->getDefinition());
				return builder.lambdaExpr(lambda->getVariable(), def);

			}

			core::NodePtr visitNode(const core::NodePtr& node) {
				return node;
			}

		};


	}


	void evalModel(const core::NodePtr& code, CacheModel& model) {

		core::StatementPtr stmt = dynamic_pointer_cast<core::StatementPtr>(code);
		assert(stmt && "Cannot evaluate non-stmt code section!");

		// simulate execution
		core::StatementPtr mod = static_pointer_cast<core::StatementPtr>(MemorySkeletonExtractor(code->getNodeManager()).map(code));

		std::cout << "Original code: \n" << core::printer::PrettyPrinter(code) << "\n";
		std::cout << "\n";
		std::cout << "Skeleton of code: \n" << core::printer::PrettyPrinter(mod) << "\n";

		string script = core::printer::toLuaScript(mod);
		std::cout << "\n";
		std::cout << "Lua Script: \n" << script << "\n";

		// create access wrapper
		auto accessFun = [&](long location, int size) {
			model.access(location, size);
		};

		// run script
		utils::lua::Lua lua;
		lua.registerFunction("access", &accessFun);
		lua.run(script);

//		SimulationContext context(mod);
//		ExecutionSimulator(code->getNodeManager().getLangBasic(), model).visit(mod, context);
	}


} // end namespace features
} // end namespace analysis
} // end namespace insieme
