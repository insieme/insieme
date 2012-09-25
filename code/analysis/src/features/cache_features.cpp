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

#include "insieme/analysis/features/cache_features.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/lua/lua.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/sequentialize.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/printer/lua_printer.h"

#include "insieme/analysis/features/type_features.h"

namespace insieme {
namespace analysis {
namespace features {

	TypePtr HitMissModel::getFeatureType() const {
		return tuple("Cache Usage",
				atom<long>("Cache Misses"),
				atom<long>("Cache Hits")
		);
	}

	Value HitMissModel::getFeatureValue() const {
		return combineValues(getMisses(), getHits());
	}

	Value CacheModel::eval(const core::NodePtr& code) {
		reset();
		evalModel(code, *this);
		return getFeatureValue();
	}


	namespace {

//		namespace arith=core::arithmetic;
//
//
//		class SimulationContext : private core::IRVisitor<int64_t> {
//
//			utils::map::PointerMap<core::VariablePtr, int64_t> values;
//			//std::map<core::VariablePtr, int64_t, compare_target<core::VariablePtr>> values;
//
//			utils::map::PointerMap<core::VariablePtr, core::LambdaPtr> recLambdas;
//
//		public:
//
//			/**
//			 * Create a new, empty simulation context.
//			 */
//			SimulationContext() {}
//
//			/**
//			 * Creates a copy of the given context.
//			 */
//			SimulationContext(const SimulationContext& other)
//				: values(other.values), recLambdas(recLambdas) {}
//
//			/**
//			 * Initializes a estimated context for the given node.
//			 */
//			SimulationContext(const core::NodePtr& node) {
//
//				const auto& basic = node->getNodeManager().getLangBasic();
//
//				// init context for the given node
//				uint64_t memLocation = 0;
//				for_each(core::analysis::getFreeVariables(node), [&](const core::VariablePtr& var) {
//					if (var->getType()->getNodeType() == core::NT_RefType) {
//						// for reference variables the memory location is stored
//						values[var] = memLocation;
//						// big enough to be far away + some alignment offset
//						memLocation += (1l<<30) + 64;
//					} else if (basic.isInt(var->getType())) {
//						// for other variables, the value is stored
//						values[var] = 100;
//					} else {
//						// no initialization is required in this case
//					}
//				});
//			}
//
//			void setValue(const core::VariablePtr& var, int64_t value) {
//				values[var] = value;
//			}
//
//			void unsetValue(const core::VariablePtr& var) {
//				values.erase(var);
//			}
//
//
//			int64_t evalExpr(const core::ExpressionPtr& expr) {
//				return visit(expr);
//			}
//
//		public:
//
//			int64_t visitVariable(const core::VariablePtr& var) {
//				auto pos = values.find(var);
//				if (pos != values.end()) {
//					return pos->second;
//				}
//
//				// ensure that this var is not a reference variable
//				assert(var->getType()->getNodeType() != core::NT_RefType && "All reference variables have to be known!");
//				assert(false && "Trying to read undefined variable!");
//				return 0;
//			}
//
//			int64_t visitLiteral(const core::LiteralPtr& lit) {
//				auto& basic = lit->getNodeManager().getLangBasic();
//				const core::TypePtr& type = lit->getType();
//				const string& value = lit->getStringValue();
//				if (basic.isInt(type)) {
//					return utils::numeric_cast<int64_t>(value);
//				}
//				if (basic.isBool(type)) {
//					return basic.isTrue(lit);
//				}
//
//				// unable to process other literals
//				std::cerr << "Unsupported literal: " << *lit << " of type " << *type << "\n";
//				assert(false && "Unsupported literal encountered!");
//				return 0;
//			}
//
//
//			int64_t visitCallExpr(const core::CallExprPtr& call) {
//				const auto& basic = call->getNodeManager().getLangBasic();
//				const auto& fun = call->getFunctionExpr();
//
//				std::size_t numOps = call->getArgumentList().size();
//
//				// deal with standard arithmetic expressions
//				if (numOps == 2 && basic.isArithOp(fun)) {
//
//					int64_t lhs = visit(call->getArgument(0));
//					int64_t rhs = visit(call->getArgument(1));
//
//					switch(basic.getOperator(static_pointer_cast<core::LiteralPtr>(fun))) {
//					case core::lang::BasicGenerator::Add: return lhs + rhs;
//					case core::lang::BasicGenerator::Sub: return lhs - rhs;
//					case core::lang::BasicGenerator::Mul: return lhs * rhs;
//					case core::lang::BasicGenerator::Div: return lhs / rhs;
//					case core::lang::BasicGenerator::And: return lhs & rhs;
//					case core::lang::BasicGenerator::Or:  return lhs | rhs;
//					case core::lang::BasicGenerator::Xor: return lhs ^ rhs;
//					case core::lang::BasicGenerator::LShift: return lhs << rhs;
//					case core::lang::BasicGenerator::RShift: return lhs >> rhs;
//					default: break;
//					}
//
//					assert(false && "Unsupported arithmetic operator!");
//				}
//
//				// deal with increment / decrement operators
//				if (numOps == 1 && basic.isArithOp(fun)) {
//
//					assert(call->getArgument(0)->getNodeType() != core::NT_Variable
//							&& "Can only increment variables!");
//
//					core::VariablePtr var = static_pointer_cast<core::VariablePtr>(call->getArgument(0));
//
//					switch(basic.getOperator(static_pointer_cast<core::LiteralPtr>(fun))) {
//					case core::lang::BasicGenerator::PreInc: 	return ++values[var];
//					case core::lang::BasicGenerator::PostInc: 	return values[var]++;
//					case core::lang::BasicGenerator::PreDec: 	return --values[var];
//					case core::lang::BasicGenerator::PostDec: 	return values[var]--;
//					default: break;
//					}
//
//					assert(false && "Unsupported arithmetic operator!");
//				}
//
//				// deal with comparison operators
//				if (numOps == 2 && basic.isCompOp(fun)) {
//
//					int64_t lhs = visit(call->getArgument(0));
//					int64_t rhs = visit(call->getArgument(1));
//
//					switch(basic.getOperator(static_pointer_cast<core::LiteralPtr>(fun))) {
//					case core::lang::BasicGenerator::Eq: 	return lhs == rhs;
//					case core::lang::BasicGenerator::Ne: 	return lhs != rhs;
//					case core::lang::BasicGenerator::Lt: 	return lhs < rhs;
//					case core::lang::BasicGenerator::Le: 	return lhs <= rhs;
//					case core::lang::BasicGenerator::Gt: 	return lhs > rhs;
//					case core::lang::BasicGenerator::Ge: 	return lhs >= rhs;
//					default: break;
//					}
//				}
//
//
//				// boolean operators
//				if (numOps == 1 && basic.isBoolLNot(fun)) {
//					return !visit(call->getArgument(0));
//				}
//
//				if (numOps == 2 && basic.isLogicOp(fun)) {
//
//					int64_t lhs = visit(call->getArgument(0));
//					int64_t rhs = 0;
//					if (basic.isBoolLAnd(fun) || basic.isBoolLOr(fun)) {
//						rhs = visit(core::transform::evalLazy(call->getNodeManager(), call->getArgument(0)));
//					} else {
//						rhs = visit(call->getArgument(1));
//					}
//
//					switch(basic.getOperator(static_pointer_cast<core::LiteralPtr>(fun))) {
//					case core::lang::BasicGenerator::LAnd: 	return lhs && rhs;
//					case core::lang::BasicGenerator::LOr: 	return lhs || rhs;
//					case core::lang::BasicGenerator::Eq: 	return lhs == rhs;
//					case core::lang::BasicGenerator::Ne: 	return lhs != rhs;
//					default: break;
//					}
//
//				}
//
//				std::cerr << "Unsupported operator encountered: " << *call << " of type " << *call->getType() << "\n";
//				assert(false && "Unsupported operator encountered!");
//				return 0;
//			}
//
//			int64_t visitCastExpr(const core::CastExprPtr& cast) {
//				return visit(cast->getSubExpression());
//			}
//
//		};
//
//		enum ReturnStatus {
//			Normal, Break, Continue, Return
//		};
//
//
//		class ExecutionSimulator : public core::IRVisitor<ReturnStatus, core::Pointer, SimulationContext&> {
//
//			const core::lang::BasicGenerator& basic;
//			CacheModel& model;
//
//		public:
//
//			ExecutionSimulator(const core::lang::BasicGenerator& basic, CacheModel& model)
//				: basic(basic), model(model) {}
//
//			int64_t getAddress(const core::ExpressionPtr& target, SimulationContext& context) {
//
//				if (target->getNodeType() == core::NT_Variable) {
//					return context.evalExpr(static_pointer_cast<core::VariablePtr>(target));
//				}
//
//				if (target->getNodeType() == core::NT_CallExpr) {
//					const core::CallExprPtr& call = static_pointer_cast<core::CallExprPtr>(target);
//
//					const core::ExpressionPtr& fun = call->getFunctionExpr();
//
//					if (basic.isArrayRefElem1D(fun) || basic.isVectorRefElem(fun)) {
//						// compute position of accessed array element
//						int size = getSizeInBytes(core::analysis::getReferencedType(target->getType()));
//						int64_t base = getAddress(call->getArgument(0), context);
//						int64_t index = context.evalExpr(call->getArgument(1));
//						return base + size * index;
//					}
//
//					// TODO: add support for structs!
//				}
//std::cout << "Unsupported target: " << *target << "\n";
//				// unknown location
//				return -1;
//			}
//
//			void processAccess(const core::ExpressionPtr& target, SimulationContext& context) {
//
//				assert(target->getType()->getNodeType() == core::NT_RefType && "Cannot access non-reference type!");
//
//				// simulate memory access
//				int64_t location = getAddress(target, context);
//				if (location >= 0) {
//					model.access(location,
//							getSizeInBytes(core::analysis::getReferencedType(target->getType()))
//							); // TODO: deduce real size of access
//				}
//			}
//
//
//			ReturnStatus visitCallExpr(const core::CallExprPtr& call, SimulationContext& context) {
//
//				// now, deal with called function
//				const auto& fun = call->getFunctionExpr();
//
////	std::cout << "Processing: " << *call << "\n";
//
//				// deal with memory accesses
//				if (basic.isRefDeref(fun) || basic.isRefAssign(fun)) {
//					// simulate access
//					processAccess(call->getArgument(0), context);
//					return Normal;
//				}
//
//				// start by processing argument list
//				visitAllGen(call->getArguments(), context);
//
//				// deal with external literals
//				if (fun->getNodeType() == core::NT_Literal) {
//					// TODO: add clear-cache option
//					return Normal;	// nothing to estimate her ...
//				}
//
//				// TODO: deal with recursive function calls
//
//				return Normal;
//			}
//
//			ReturnStatus visitReturnStmt(const core::ReturnStmtPtr& cur, SimulationContext& context) {
//				return Return;
//			}
//
//			ReturnStatus visitBreakStmt(const core::BreakStmtPtr& cur, SimulationContext& context) {
//				return Break;
//			}
//
//			ReturnStatus visitContinueStmt(const core::ContinueStmtPtr& cur, SimulationContext& context) {
//				return Continue;
//			}
//
//			ReturnStatus visitCompoundStmt(const core::CompoundStmtPtr& cur, SimulationContext& context) {
//
//				std::size_t numStmts = cur->size();
//				ReturnStatus status = Normal;
//
//				for (std::size_t i=0; i<numStmts && status == Normal; i++) {
//					status = visit(cur->getStatement(i), context);
//				}
//
//				return status;
//			}
//
//			ReturnStatus visitIfStmt(const core::IfStmtPtr& cur, SimulationContext& context) {
//
//				if (context.evalExpr(cur->getCondition())) {
//					return visit(cur->getThenBody(), context);
//				}
//				return visit(cur->getElseBody(), context);
//			}
//
//
//			ReturnStatus visitForStmt(const core::ForStmtPtr& cur, SimulationContext& context) {
//
//				int start = context.evalExpr(cur->getStart());
//				int end = context.evalExpr(cur->getEnd());
//				int step = context.evalExpr(cur->getStep());
//
//				core::VariablePtr iter = cur->getIterator();
//				core::StatementPtr body = cur->getBody();
//
//				ReturnStatus status = Normal;
//				for (int i=start; i<end && status != Return && status != Break; i+=step) {
//					context.setValue(iter,i);
//					status = visit(body, context);
//				}
//				context.unsetValue(iter);
//				return (status==Return)?Return:status;
//			}
//
//			ReturnStatus visitWhileStmt(const core::WhileStmtPtr& cur, SimulationContext& context) {
//
//				ReturnStatus status = Normal;
//				while(status != Return && status != Break && context.evalExpr(cur->getCondition())) {
//					status = visit(cur->getBody(), context);
//				}
//				return (status==Return)?Return:status;
//			}
//
//
//			// TODO:
//			//	- support declarations
//			//	- support for, while, if, switch, ...
//			//  - record all integer operations (optional improvement)
//
//		};


		class StructMemberAccessEliminator : public core::transform::CachedNodeMapping {

			core::NodeManager& manager;
			core::IRBuilder builder;
			const core::lang::BasicGenerator& basic;

		public:

			StructMemberAccessEliminator(core::NodeManager& manager)
				: manager(manager), builder(manager), basic(manager.getLangBasic()) {}


			virtual const core::NodePtr resolveElement(const core::NodePtr& ptr) {

				// skip types
				if (ptr->getNodeCategory() == core::NC_Type ) {
					return ptr;
				}

				// resolve recursively bottom up
				core::NodePtr res = ptr.substitute(manager, *this);
				// rest is only important for int values
				if (res->getNodeCategory() != core::NC_Expression) {
					return res;
				}


				core::ExpressionPtr expr = res.as<core::ExpressionPtr>();
				if (!basic.isInt(expr->getType())) {
					return res;
				}

				// skip cast expressions
				while (res->getNodeType() == core::NT_CastExpr) {
					res = res.as<core::CastExprPtr>()->getSubExpression();
				}

				// fix call expression if necessary
				if (core::analysis::isCallOf(res, basic.getRefDeref())) {
					core::CallExprPtr call = res.as<core::CallExprPtr>();
					if (core::analysis::isCallOf(call->getArgument(0), basic.getCompositeRefElem())) {
						return builder.literal(expr->getType(), "100");
					}
				}

				// otherwise, all is fine
				return res;
			}
		};


		class MemorySkeletonExtractor :
			public core::transform::CachedNodeMapping,
			public core::IRVisitor<core::preserve_node_type> {


			core::NodeManager& manager;
			core::IRBuilder builder;
			const core::lang::BasicGenerator& basic;

			// the literal implanted when accessing a variable
			core::LiteralPtr access;

			// the literal implanted when de-referencing a pointer
			core::LiteralPtr read_ptr;

			// the literal implanted whenever a new memory block is allocated (ref-new)
			core::LiteralPtr fresh_mem_location;

		public:

			MemorySkeletonExtractor(core::NodeManager& manager)
				: manager(manager), builder(manager), basic(manager.getLangBasic()) {

				core::TypePtr intType = basic.getUInt8();
				auto unit = basic.getUnit();
				auto accessType = builder.functionType(toVector(intType, intType), unit);
				access = builder.literal("access", accessType);

				auto readPtrType = builder.functionType(toVector(intType), unit);
				read_ptr = builder.literal("read_ptr", readPtrType);
				core::arithmetic::markAsValueConstructor(read_ptr);

				auto freshMemLocationType = builder.functionType(core::TypeList(), intType);
				fresh_mem_location = builder.literal("fresh_mem_location", freshMemLocationType);
			}

			virtual const core::NodePtr resolveElement(const core::NodePtr& ptr) {
				// use internal visitor for resolving the replacement
				core::NodePtr res = visit(ptr);
				return (res)?res:builder.getNoOp();
			}

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

			core::arithmetic::Formula getMemoryLocation(const core::ExpressionPtr& target) {

				// variables => just access there location
				core::NodeType nodeType = target->getNodeType();
				if (nodeType == core::NT_Variable ) {
					// re-write variables to be integers referencing the location
					return builder.variable(basic.getUInt8(),
							static_pointer_cast<core::VariablePtr>(target)->getId());
				}

				assert(nodeType == core::NT_CallExpr && "Need to be some derived value!");

				const auto& call = static_pointer_cast<core::CallExprPtr>(target);
				const auto& fun = call->getFunctionExpr();

				// check array of vector accesses
				if (basic.isArrayRefElem1D(fun) || basic.isVectorRefElem(fun)) {
					const auto& elementType = core::analysis::getReferencedType(target->getType());
					unsigned elementSize = getEstimatedSizeInBytes(elementType);
					return getMemoryLocation(call->getArgument(0))
							+ core::arithmetic::toFormula(call->getArgument(1)) * elementSize;
				}

				// check for member accesses
				if (basic.isCompositeRefElem(fun)) {

					// get base of component
					auto base = getMemoryLocation(call->getArgument(0));

					// add offset - for unions it is 0
					if (core::analysis::getReferencedType(target->getType())->getNodeType() == core::NT_UnionType) {
						return base;
					}

					// compute offset of struct member
					core::StructTypePtr structType = static_pointer_cast<core::StructTypePtr>(
							core::analysis::getReferencedType(call->getArgument(0)->getType()));
					core::StringValuePtr member = call->getArgument(1).as<core::LiteralPtr>()->getValue();
					return base + getMemberOffsetInBytes(structType, member);
				}

				// check vector-to-array-conversion
				if (basic.isRefVectorToRefArray(fun)) {
					// can just be ignored
					return getMemoryLocation(call->getArgument(0));
				}

				// check for scalar-to-array conversions
				if (basic.isScalarToArray(fun)) {
					const auto& arg = call->getArgument(0);

					// check whether it is a local variable being expanded
					if (arg->getNodeType() == core::NT_Variable) {
						// scalars are considered to be at a fixed position within the memory
						// to simulate this, the ID of the variable is exploited
						return 128 + 8 * arg.as<core::VariablePtr>()->getId();
					}
				}

				// handle string-to-char pointer stuff
				if (basic.isStringToCharPointer(fun)) {
					// strings are at a fixed position => 0
					return 0;
				}

				// add code for pointer chasing
				if (basic.isRefDeref(fun)) {
					// the read operation referencing the pointer value is handled independently
					auto ptrPos = getMemoryLocation(call->getArgument(0));
					auto pos = builder.callExpr(basic.getUInt8(), read_ptr,
							core::arithmetic::toIR(manager, ptrPos));
					return core::arithmetic::Value(pos);
				}


				// TODO: add exception for this
				std::stringstream msg;
				msg << "Unsupported memory location: " << *target << " of type " << target->getType();
				std::cout << "Encountered problem: " << msg.str() << "\n";
				assert(false);
				return 0;
			}

			core::CallExprPtr createAccess(const core::ExpressionPtr& target) {
				// compute memory location
				core::arithmetic::Piecewise location = getMemoryLocation(target);

				// create call to access function
				auto pos = core::arithmetic::toIR(manager, location);
				auto size = builder.intLit(getEstimatedSizeInBytes(core::analysis::getReferencedType(target->getType())));
				return builder.callExpr(basic.getUnit(), access, pos, size );
			}


			core::ExpressionPtr processExpression(const core::ExpressionPtr& cur) {

				// at this point we only care for pointers
				if (cur->getType()->getNodeType() == core::NT_RefType) {
					// it is a memory access => determine position and read the corresponding value
					return core::arithmetic::toIR(manager, getMemoryLocation(cur));
				}

				// remove type extensions form integer literals
				if (cur->getNodeType() == core::NT_Literal && basic.isInt(cur->getType())) {
					std::cout << "Converting " << cur << " into " << builder.literal(cur->getType(), format("%d", utils::numeric_cast<int>(cur.as<core::LiteralPtr>()->getStringValue()))) << "\n";
					return builder.literal(cur->getType(), format("%d", utils::numeric_cast<int>(cur.as<core::LiteralPtr>()->getStringValue())));
				}

				// all the rest is what it is
				return cur;

//				// forward value of variables and literals
//				if (cur->getNodeType() == core::NT_Variable || cur->getNodeType() == core::NT_Literal) {
//					// variables can simply be forwarded
//					return cur;
//				}
//
//				// some derived values => use default value
//				return builder.intLit(100);
			}

			vector<core::ExpressionPtr> processArguments(const vector<core::ExpressionPtr>& args) {

				/**
				 * Before arguments are passed to functions, pointers might need to be resolved such
				 * that the actual value being passed is a an unsigned integer containing the address
				 * of the referenced element.
				 */

				return ::transform(args, fun(*this, &MemorySkeletonExtractor::processExpression));
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

				// TODO: consider possibility of memory access within conditions

				// obtain skeleton of body
				core::CompoundStmtPtr body = map(stmt->getBody());
				if (!body || body->empty()) {
					// there is nothing left
					return core::ForStmtPtr();
				}

				// start, end and step should be affine anyway => no conversion
				core::VariablePtr iter = stmt->getIterator();
				core::ExpressionPtr start = processExpression(stmt->getStart());
				core::ExpressionPtr end = processExpression(stmt->getEnd());
				core::ExpressionPtr step = processExpression(stmt->getStep());

				return builder.forStmt(iter, start, end , step, body);
			}

			core::WhileStmtPtr visitWhileStmt(const core::WhileStmtPtr& stmt) {

				// TODO: consider possibility of memory access within conditions

				// obtain skeleton of body
				core::CompoundStmtPtr body = map(stmt->getBody());
				if (!body || body->empty()) {
					// there is nothing left
					return core::WhileStmtPtr();
				}

				// add condition and re-assemble while loop
				core::ExpressionPtr condition = processExpression(stmt->getCondition());
				return builder.whileStmt(condition, body);
			}

			core::IfStmtPtr visitIfStmt(const core::IfStmtPtr& stmt) {

				// TODO: consider possibility of memory access within conditions

				// obtain skeleton of left and right bodies
				core::CompoundStmtPtr thenBody = map(stmt->getThenBody());
				core::CompoundStmtPtr elseBody = map(stmt->getElseBody());

				if (!(thenBody && !thenBody->empty() && elseBody && !elseBody->empty())) {
					// there is no skeleton left
					return core::IfStmtPtr();
				}

				// add condition and re-assemble conditional statement
				core::ExpressionPtr condition = processExpression(stmt->getCondition());
				return builder.ifStmt(condition, thenBody, elseBody);
			}

			core::SwitchStmtPtr visitSwitchStmt(const core::SwitchStmtPtr& stmt) {
				assert(false && "Sorry, not implemented!");
				return core::SwitchStmtPtr();
			}

			core::StatementPtr visitStatement(const core::StatementPtr& stmt) {

				// for all other statements => get list of ordered memory accesses
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

					// handle unary increment / decrement operators
					if (basic.isIncrementOp(fun)) {
						// add access to target
						skeleton.push_back(createAccess(node->getArgument(0)));
					}

					// handle function calls
					if (fun->getNodeType() == core::NT_LambdaExpr) {

						// compute skeleton of function
						core::LambdaExprPtr lambda = this->map(static_pointer_cast<core::LambdaExprPtr>(fun));

						if (!lambda->getBody().empty()) {
							// add function call to skeleton
							skeleton.push_back(builder.callExpr(node->getType(), lambda, processArguments(node->getArguments())));
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

			core::DeclarationStmtPtr visitDeclarationStmt(const core::DeclarationStmtPtr& decl) {

				core::ExpressionPtr init = decl->getInitialization();
				if (core::analysis::isCallOf(init, basic.getRefNew())) {
					// create a substitute
					auto intType = basic.getUInt8();
					core::VariablePtr var = builder.variable(intType, decl->getVariable()->getID());
					return builder.declarationStmt(var, builder.callExpr(intType, fresh_mem_location));
				}

				return builder.declarationStmt(decl->getVariable(), processExpression(init));
			}


			core::LambdaPtr visitLambda(const core::LambdaPtr& lambda) {
				// convert body
				core::CompoundStmtPtr body = map(lambda->getBody());
				if (!body) {
					body = builder.compoundStmt();
				}
				return builder.lambda(lambda->getType(), lambda->getParameters(), body);
			}

			core::LambdaDefinitionPtr visitLambdaDefinition(const core::LambdaDefinitionPtr& def) {

				// build a new definition consisting of skeletons only
				vector<core::LambdaBindingPtr> bindings;
				for_each(def->getDefinitions(), [&](const core::LambdaBindingPtr& cur) {
					bindings.push_back(builder.lambdaBinding(cur->getVariable(), this->map(cur->getLambda())));
				});
				return builder.lambdaDefinition(bindings);
			}

			core::LambdaExprPtr visitLambdaExpr(const core::LambdaExprPtr& lambda) {

				// convert definition
				core::LambdaDefinitionPtr def = map(lambda->getDefinition());
				return builder.lambdaExpr(lambda->getVariable(), def);

			}

			core::NodePtr visitNode(const core::NodePtr& node) {
				std::cout << "Error - visiting " << node->getNodeType() << "\n";
				assert(false);
				return node;
			}

		};

		core::StatementPtr toSkeleton(const core::StatementPtr& stmt) {
			core::NodeManager& manager = stmt->getNodeManager();
			return MemorySkeletonExtractor(manager).map(
					StructMemberAccessEliminator(manager).map(stmt)
			);
		}


		string toLuaScript(const core::StatementPtr& stmt) {
			std::stringstream res;

			// start by eliminating parallel constructs
			core::StatementPtr seq = core::transform::trySequentialize(stmt->getNodeManager(), stmt);
			if (!seq) { return ""; }

			// start by setting up free variables
			res << "-- Adding some utility functions\n";
			res << "local mem_pointer = 0\n";

			res << "function fresh_mem_location() \n"
					"	mem_pointer = mem_pointer + (2^27) + 64\n"
					"	return mem_pointer\n"
					"end\n";

			res << "\n";
			res << "local ptr = {}\n";
			res << "function read_ptr( pos ) \n"
					"	if ptr[pos] == nil then \n"
					"		ptr[pos] = fresh_mem_location()\n"
					"	end \n"
					"	return ptr[pos] \n"
					"end \n";

			res << "\n\n";

			res << "-- Setting up free variables\n";
			const auto& basic = stmt->getNodeManager().getLangBasic();

			// init context for the given node
			for_each(core::analysis::getFreeVariables(seq), [&](const core::VariablePtr& var) {
				// res << "-- Free Variable " << *var << " of type " << *var->getType() << "\n";
				if (var->getType()->getNodeType() == core::NT_RefType) {
					// add setup for memory location
					res << *var << " = fresh_mem_location()\n";
				} else if (basic.isInt(var->getType())) {
					// for other variables the default value is used
					res << *var << " = 100\n";
				} else {
					res << "-- other variable: " << *var << " of type " << *var->getType() << "\n";
					// no initialization is required in this case
				}
			});


			// add script for code skeleton
			res << "\n-- code skeleton\n";
			res << core::printer::LuaPrinter(toSkeleton(seq));

			return res.str();
		}

	}


	bool evalModel(const core::NodePtr& code, CacheModel& model) {

		core::StatementPtr stmt = dynamic_pointer_cast<core::StatementPtr>(code);
		if (!stmt) {
			core::NodeType type = code->getNodeType();
			if (type == core::NT_LambdaExpr) {
				stmt = static_pointer_cast<core::LambdaExprPtr>(code)->getBody();
			} else if (type == core::NT_Lambda) {
				stmt = static_pointer_cast<core::LambdaPtr>(code)->getBody();
			} else if (type == core::NT_Program) {
				core::ProgramPtr program = static_pointer_cast<core::ProgramPtr>(code);
				if (program.size() == 1u && program[0]->getNodeType() == core::NT_LambdaExpr) {
					stmt = static_pointer_cast<core::LambdaExprPtr>(program[0])->getBody();
				} else {
					return false;
				}
			} else {
				return false; 	// can not be processed
			}
		}

		try {

			// simulate execution
//			std::cout << "Original code: \n" << core::printer::PrettyPrinter(code, core::printer::PrettyPrinter::OPTIONS_DETAIL) << "\n";
//			std::cout << "\n";
//			std::cout << "Removing member accesses: \n" << core::printer::PrettyPrinter(StructMemberAccessEliminator(stmt->getNodeManager()).map(stmt), core::printer::PrettyPrinter::OPTIONS_DETAIL) << "\n";
//			std::cout << "\n";
//			std::cout << "Skeleton of code: \n" << core::printer::PrettyPrinter(toSkeleton(stmt), core::printer::PrettyPrinter::OPTIONS_DETAIL) << "\n";
//			std::cout << "\n";
//			std::cout << "Lua Script: \n" << toLuaScript(stmt) << "\n";

			// create access wrapper
			auto accessFun = [&](uint64_t location, unsigned size) {
				model.access(location, size);
			};

			// run script
			utils::lua::Lua lua;

			// register model
			lua.registerFunction("access", &accessFun);


			// run script and be happy
			lua.run(toLuaScript(stmt));

			// successfully completed
			return true;

		} catch (const utils::lua::LuaException& le) {
			// TODO: define failure state or throw feature exception
			if (le.getMessage() == "C++ exception") {
				return true; 	// all fine in this case
			}
			if (!containsSubString(le.getMessage(), "control structure too long")) {
				throw le;
			}
		} catch (const core::arithmetic::NotAFormulaException& nfe) {
			// there was an error during the conversion
			// TODO: add support for indirect access or other failed cases
			LOG(WARNING) << "Unable to simulate cache usage: " << nfe.what();
		}

		// extraction failed
		return false;

	}


} // end namespace features
} // end namespace analysis
} // end namespace insieme
