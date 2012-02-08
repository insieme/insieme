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

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_node.h"

#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

namespace insieme {
namespace analysis {
namespace features {


	namespace {

		namespace arith=core::arithmetic;


		class SimulationContext {

			utils::map::PointerMap<core::VariablePtr, int64_t> locations;

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
				: locations(other.locations) {}

			/**
			 * Initializes a estimated context for the given node.
			 */
			SimulationContext(const core::NodePtr& node) {
				// init context for the given node
				uint64_t memLocation = 0;
				for_each(core::analysis::getFreeVariables(node), [&](const core::VariablePtr& var) {
					if (var->getType()->getNodeType() == core::NT_RefType) {
						locations[var] = memLocation;
						memLocation += (1l<<30) + 64;		// big enough to be far away + some alignment offset
					} else {
						locations[var] = 100;			// default value for all other values
					}
				});
			}

			void setValue(const core::VariablePtr& var, int64_t value) {
				locations[var] = value;
			}

			void unsetValue(const core::VariablePtr& var) {
				locations.erase(var);
			}

			long evalVar(const core::VariablePtr& var) {

				// check cache first
				auto pos = locations.find(var);
				if (pos != locations.end()) {
					return pos->second;
				}

				// ensure that this var is not a reference variable
				assert(var->getType()->getNodeType() != core::NT_RefType && "All reference variables have to be known!");

				// all others have the constant value 100
				return 100;
			}


			long evalExpr(const core::CallExprPtr& call) {
				// TODO: deal with all kind of de-referencing operations and displacements

				// TODO: implement evaluation using visitor

				return 0;
			}

			long evalExpr(const core::ExpressionPtr& expr) {

				// TODO: eval recursively not using the formulas
				// 	=> all values are known, should be faster to eval directly

				// check whether it is a formula
				try {
					arith::Formula f = arith::toFormula(expr);

					if (f.isConstant()) {
						return (int)f.getConstantValue();
					}
				} catch (const arith::NotAFormulaException& e) {}

				// process call expressions
				if (expr->getNodeType() == core::NT_CallExpr) {
					return evalExpr(static_pointer_cast<core::CallExprPtr>(expr));
				}

				// process variables
				if (expr->getNodeType() == core::NT_Variable) {
					return evalExpr(static_pointer_cast<core::VariablePtr>(expr));
				}

				// the rest is not important
				return 100;
			}
		};

		enum ReturnStatus {
			Normal, Break, Continue, Return
		};


		class ExecutionSimulator : public core::IRVisitor<ReturnStatus, core::Pointer, SimulationContext&, CacheUsage&> {

			const core::lang::BasicGenerator& basic;
			const CacheModel& model;

		public:

			ExecutionSimulator(const core::lang::BasicGenerator& basic, const CacheModel& model)
				: basic(basic), model(model) {}


			ReturnStatus visitCallExpr(const core::CallExprPtr& call, SimulationContext& context, CacheUsage& usage) {

				// start by processing argument list
				visitAllGen(call->getArguments(), context, usage);

//				std::cout << "Processing: " << *call << "\n";

				// now, deal with called function
				const auto& fun = call->getFunctionExpr();

				// deal with memory accesses
				if (basic.isRefDeref(fun)) {
					// simulate access
					return Normal;
				}

				// deal with assignments
				if (basic.isRefAssign(fun)) {
					// TODO: update value of variable within context

					return Normal;
				}

				// deal with external literals
				if (fun->getNodeType() == core::NT_Literal) {
					// TODO: add clear-cache option
					return Normal;	// nothing to estimate her ...
				}

				// deal with actual function calls
				if (fun->getNodeType() == core::NT_LambdaExpr) {

					// create a new context for the call


				}

				// deal with recursive function calls

				return Normal;
			}

			ReturnStatus visitReturnStmt(const core::ReturnStmtPtr& cur, SimulationContext& context, CacheUsage& usage) {
				return Return;
			}

			ReturnStatus visitBreakStmt(const core::BreakStmtPtr& cur, SimulationContext& context, CacheUsage& usage) {
				return Break;
			}

			ReturnStatus visitContinueStmt(const core::ContinueStmtPtr& cur, SimulationContext& context, CacheUsage& usage) {
				return Continue;
			}

			ReturnStatus visitCompoundStmt(const core::CompoundStmtPtr& cur, SimulationContext& context, CacheUsage& usage) {

				std::size_t numStmts = cur->size();
				ReturnStatus status = Normal;

				for (std::size_t i=0; i<numStmts && status == Normal; i++) {
					status = visit(cur->getStatement(i), context, usage);
				}

				return status;
			}

			ReturnStatus visitIfStmt(const core::IfStmtPtr& cur, SimulationContext& context, CacheUsage& usage) {

				if (context.evalExpr(cur->getCondition())) {
					return visit(cur->getThenBody(), context, usage);
				}
				return visit(cur->getElseBody(), context, usage);
			}


			ReturnStatus visitForStmt(const core::ForStmtPtr& cur, SimulationContext& context, CacheUsage& usage) {

				int start = context.evalExpr(cur->getStart());
				int end = context.evalExpr(cur->getEnd());
				int step = context.evalExpr(cur->getStep());

				core::VariablePtr iter = cur->getIterator();
				core::StatementPtr body = cur->getBody();

				ReturnStatus status = Normal;
				for (int i=start; i<end && status != Return && status != Break; i+=step) {
					context.setValue(iter,i);
					status = visit(body, context, usage);
				}
				context.unsetValue(iter);
				return (status==Return)?Return:status;
			}

			ReturnStatus visitWhileStmt(const core::WhileStmtPtr& cur, SimulationContext& context, CacheUsage& usage) {

				ReturnStatus status = Normal;
				while(status != Return && status != Break && context.evalExpr(cur->getCondition())) {
					status = visit(cur->getBody(), context, usage);
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


	}



	CacheUsage evalModel(const core::NodePtr& code, const CacheModel& model) {
		CacheUsage usage;

		// simulate execution
		SimulationContext context(code);
		ExecutionSimulator(code->getNodeManager().getLangBasic(), model).visit(code, context, usage);

		return usage;
	}


} // end namespace features
} // end namespace analysis
} // end namespace insieme
