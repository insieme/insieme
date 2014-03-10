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

#include "insieme/analysis/cba/framework/generator/killed_definitions.h"

namespace insieme {
namespace analysis {
namespace cba {

	namespace detail {

		struct AssignmentFreeTag {
			bool value;
			AssignmentFreeTag(bool value) : value(value) {}
			bool operator==(const AssignmentFreeTag& other) const {
				return value == other.value;
			}
		};

		class AssignmentCheck : public IRVisitor<bool> {

		public:

			virtual bool visitVariable(const VariablePtr& var) {
				return true;		// no sync points here
			}

			virtual bool visitLiteral(const LiteralPtr& var) {
				return true;		// the literal itself is never a sync point, only the call to a synchronizing function
			}

			virtual bool visitCallExpr(const CallExprPtr& call) {
				auto fun = call->getFunctionExpr();
				const auto& base = fun->getNodeManager().getLangBasic();

				// if this is a call to a spawn => not free of spawn points
				if (base.isRefAssign(fun)) return false;
				if (base.isMerge(fun)) return false;

				// if one of the arguments contains a sync point we are done
				if (!all(call, isAssignmentFree)) {
					return false;		// not sync point free
				}

				// if any of the arguments is a job or closure => return false (conservative)
				for(const auto& cur : call) {
					if (cur->getType().isa<FunctionTypePtr>()) return false;
					if (cur.isa<JobExprPtr>()) return false;
				}

				// if it is a call to a literal
				if (fun.isa<LiteralPtr>()) {
					// this is fine
					return true;
				}

				// if the target is a variable => fail (conservative)
				if (fun.isa<VariablePtr>()) return false;

				// if the target is anything else => check whether synchronizing expressions are included
				return !visitDepthFirstOnceInterruptible(fun, [&](const LiteralPtr& lit){
					return base.isRefAssign(lit) || base.isMerge(lit);
				});
			}

			virtual bool visitLambdaExpr(const LambdaExprPtr& expr) {
				return true;		// the evaluation of the lambda expression is not causing sync points
			}

			virtual bool visitJobExpr(const JobExprPtr& job) {
				return isAssignmentFree(job->getThreadNumRange());
			}

			virtual bool visitTupleExpr(const TupleExprPtr& tuple) {
				for(const auto& cur : tuple->getExpressions()) {
					if (!isAssignmentFree(cur)) return false;
				}
				return true;
			}

			virtual bool visitVectorExpr(const VectorExprPtr& vec) {
				for(const auto& cur : vec->getExpressions()) {
					if (!isAssignmentFree(cur)) return false;
				}
				return true;
			}

			virtual bool visitStructExpr(const StructExprPtr& structExpr) {
				for(const auto& cur : structExpr->getMembers()) {
					if (!isAssignmentFree(cur->getValue())) return false;
				}
				return true;
			}

			virtual bool visitUnionExpr(const UnionExprPtr& unionExpr) {
				return isAssignmentFree(unionExpr->getMember());
			}

			virtual bool visitNode(const NodePtr& node) {
				assert_fail() << "Unsupported Node Type encountered: " << node->getNodeType();
				return false;
			}

		};


		bool isAssignmentFree(const NodePtr& node) {

			static AssignmentCheck isAssignmentFreeInternal;

			// check whether there is an attached sync-free tag
			if (node->hasAttachedValue<AssignmentFreeTag>()) {
				return node->getAttachedValue<AssignmentFreeTag>().value;
			}

			// compute state
			bool res = isAssignmentFreeInternal(node);

			// attach resulting annotation
			node->attachValue<AssignmentFreeTag>(res);

			// done
			return res;
		}

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
