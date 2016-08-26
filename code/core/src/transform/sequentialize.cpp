/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/transform/sequentialize.h"

#include "insieme/utils/logging.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/compound_operators.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/transform/simplify.h"

namespace insieme {
namespace core {
namespace transform {


	namespace {

		template <typename T>
		T lowerThreadIDs(const T& code) {
			// TODO: consider nested job expressions and do not lower contained getThreadID calls
			return makeCachedLambdaMapper([](const NodePtr& cur) -> NodePtr {
				       if(cur->getNodeType() != NT_CallExpr) return cur;

				       // some preparation
				       NodeManager& mgr = cur->getNodeManager();
				       auto& parExt = mgr.getLangExtension<lang::ParallelExtension>();
				       IRBuilder builder(mgr);
				       auto call = cur.as<CallExprPtr>();

				       // a function reducing the level expression by 1
				       auto decLevel = [](const ExpressionPtr& level) -> ExpressionPtr {
					       auto formula = arithmetic::toFormula(level);
					       assert_true(formula.isConstant()) << "Accessing thread-group using non-constant level index not supported!";
					       if(formula.isZero()) return ExpressionPtr();
					       return arithmetic::toIR(level->getNodeManager(), formula - 1);
					   };

				       // handle getThreadID
				       if(analysis::isCallOf(call, parExt.getGetThreadId())) {
					       if(ExpressionPtr newLevel = decLevel(call->getArgument(0))) { return builder.getThreadId(newLevel); }
					       return builder.literal("0", call->getType());
				       }

				       // handle group size
				       if(analysis::isCallOf(call, parExt.getGetGroupSize())) {
					       if(ExpressionPtr newLevel = decLevel(call->getArgument(0))) { return builder.getThreadGroupSize(newLevel); }
					       return builder.literal("1", call->getType());
				       }

				       return cur;
				   })
			    .map(code);
		}


		class Sequentializer : public transform::CachedNodeMapping, private IRVisitor<preserve_node_type> {
			NodeManager& manager;
			IRBuilder builder;
			const lang::BasicGenerator& basic;
			const lang::ParallelExtension& parExt;
			const lang::CompoundOpsExtension& compExt;

			bool removeSyncOps;

		  public:
			Sequentializer(NodeManager& manager, bool removeSyncOps)
				: manager(manager), builder(manager), basic(manager.getLangBasic()), parExt(manager.getLangExtension<lang::ParallelExtension>()),
				  compExt(manager.getLangExtension<lang::CompoundOpsExtension>()), removeSyncOps(removeSyncOps) {}

			virtual const NodePtr resolveElement(const NodePtr& ptr) {
				// skip types
				if(ptr->getNodeCategory() == NC_Type) { return ptr; }
				return visit(ptr);
			}

		  protected:
			StatementPtr handleCall(const CallExprPtr& call) {
				const auto& fun = call->getFunctionExpr();
				auto args = core::transform::extractArgExprsFromCall(call);

				// skip merge expressions if possible
				if(parExt.isMerge(fun)) {
					// eval argument if necessary
					if(args[0]->getNodeType() == NT_CallExpr) { return args[0]; }
					return builder.getNoOp();
				}

				// ignore merge-all calls
				if(parExt.isMergeAll(fun)) { return builder.getNoOp(); }

				// handle parallel expression
				if(parExt.isParallel(fun)) {
					// invoke recursively resolved argument (should be a lazy function after conversion)
					return evalLazy(manager, args[0]);
				}

				// handle pfor calls
				if(parExt.isPFor(fun)) {
					core::ExpressionPtr start = args[1];
					core::ExpressionPtr end = args[2];
					core::ExpressionPtr step = args[3];
					return core::transform::tryInlineToStmt(manager, builder.callExpr(basic.getUnit(), args[4], start, end, step));
				}

				// check whether synchronization operations should be eliminated
				if(!removeSyncOps) { return call; }

				// handle barrier
				if(parExt.isBarrier(fun)) {
					// => can be ignored
					return builder.getNoOp();
				}

				// handle flush
				if(basic.isFlush(fun)) {
					// => can be ignored
					return builder.getNoOp();
				}

				// and locks
				if(parExt.isLockAcquire(fun)) { return builder.getNoOp(); }

				if(parExt.isLockTryAcquire(fun)) { return builder.boolLit(true); }

				if(parExt.isLockRelease(fun)) { return builder.getNoOp(); }

				if(parExt.isLockInit(fun)) { return builder.getNoOp(); }

				// and atomics
				auto rT = builder.refType(call->getType());
				if(parExt.isAtomicAddAndFetch(fun)) return builder.deref(builder.callExpr(rT, compExt.getCompAssignAdd(), args[0], args[1]));
				if(parExt.isAtomicSubAndFetch(fun)) return builder.deref(builder.callExpr(rT, compExt.getCompAssignSubtract(), args[0], args[1]));
				if(parExt.isAtomicAndAndFetch(fun)) return builder.deref(builder.callExpr(rT, compExt.getCompAssignBitwiseAnd(), args[0], args[1]));
				if(parExt.isAtomicOrAndFetch(fun)) return builder.deref(builder.callExpr(rT, compExt.getCompAssignBitwiseOr(), args[0], args[1]));
				if(parExt.isAtomicXorAndFetch(fun)) return builder.deref(builder.callExpr(rT, compExt.getCompAssignBitwiseXor(), args[0], args[1]));

				if(parExt.isAtomic(fun)) {
					// fix generic parameters within atomic body definition
					core::CallExprPtr newCall = call;

					// push binds into the call
					assert_eq(newCall->getArgument(1)->getNodeType(), NT_BindExpr) << "Expected atomic argument 1 to be a bind!";
					assert_eq(newCall->getArgument(2)->getNodeType(), NT_BindExpr) << "Expected atomic argument 2 to be a bind!";

					newCall = pushBindIntoLambda(manager, newCall, 2); // start with 2 to keep argument 1 at correct position
					newCall = pushBindIntoLambda(manager, newCall, 1);

					// simplify call by eliminating all kind of bind constructs
					return simplify(manager, newCall, true);
				}

				// otherwise, don't touch it
				return call;
			}


			ExpressionPtr handleJobExpr(const JobExprPtr& job) {
				// check whether 1 is within job range
				ExpressionPtr range = job->getThreadNumRange();

				assert_eq(range->getNodeType(), NT_CallExpr) << "Range is not formed by call expression!";

				// resolve first argument (lower bound)
				ExpressionPtr lowerBound = analysis::getArgument(range, 0);

				try {
					// check lower boundary
					arithmetic::Formula f = arithmetic::toFormula(lowerBound);

					if(!f.isConstant() || !f.getConstantValue().isOne()) {
						if(ExpressionPtr pfor = tryToPFor(job)) { return map(pfor); }
						throw NotSequentializableException("Job requires more than one thread!");
					}

					if(f.getConstantValue().isZero()) {
						// job can be completely eliminated => return empty function
						return builder.lambdaExpr(basic.getUnit(), VariableList(), builder.getNoOp());
					}

				} catch(const arithmetic::NotAFormulaException& nfe) {
					throw NotSequentializableException("Unable to parse lower boundary of job expression!");
				}


				ExpressionPtr branch = job->getBody();

				// convert selected branch into a lazy expression
				//  - handle getThreadNum / getThreadGroupSize
				//	- inline local definitions into bind expression
				//	- return bind expression

				// reduce level in get-thread-group-size by 1, replace expressions representing 0
				LOG(DEBUG) << "Branch pre-lowerThreadIDs:\n" << dumpColor(branch);
				branch = lowerThreadIDs(branch);
				LOG(DEBUG) << "Branch post-lowerThreadIDs:\n" << dumpColor(branch);

				return branch;
			}

			StatementPtr visitStatement(const StatementPtr& curStmt) {
				// start with current statement
				StatementPtr stmt = curStmt;

				// start by resolve stmt recursively
				stmt = stmt->substitute(manager, *this);

				// eliminate parallel constructs if necessary
				if(stmt->getNodeType() == NT_CallExpr) { return handleCall(static_pointer_cast<CallExprPtr>(stmt)); }

				if(stmt->getNodeType() == NT_JobExpr) { return handleJobExpr(static_pointer_cast<JobExprPtr>(stmt)); }

				// otherwise take it as it has been resolved
				return stmt;
			}

			LambdaExprPtr visitLambdaExpr(const LambdaExprPtr& lambda) {
				return correctRecursiveLambdaVariableUsage(manager, visitNode(lambda).as<LambdaExprPtr>());
			}

			NodePtr visitNode(const NodePtr& node) {
				return node->substitute(manager, *this);
			}
		};
	}

	NodePtr sequentialize(NodeManager& manager, const NodePtr& stmt, bool removeSyncOps) {
		LOG(DEBUG) << "Sequentialize input:\n" << dumpColor(stmt);
		auto ret = Sequentializer(manager, removeSyncOps).map(lowerThreadIDs(stmt));
		LOG(DEBUG) << "Sequentialize output:\n" << dumpColor(ret);
		return ret;
	}

	NodePtr trySequentialize(NodeManager& manager, const NodePtr& stmt, bool removeSyncOps) {
		try {
			return sequentialize(manager, stmt, removeSyncOps);
		} catch(const NotSequentializableException& nse) { LOG(INFO) << "Unable to sequentialize: " << nse.what(); }
		return NodePtr();
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
