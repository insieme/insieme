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

#include "insieme/core/ir_visitor.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace analysis {
namespace cba {

	namespace detail {

		template<typename Filter>
		class StaticReachabilityTag;

		template<typename Filter>
		class StaticReachabilityCheck;

	}


	template<typename Filter>
	bool mayReachCallTo(const core::NodePtr& node, const Filter& filter) {
		typedef detail::StaticReachabilityTag<Filter> Tag;

		detail::StaticReachabilityCheck<Filter> checkInternal(filter);

		// check whether there is an attached sync-free tag
		if (node->hasAttachedValue<Tag>()) {
			return node->getAttachedValue<Tag>().value;
		}

		// compute state
		bool res = checkInternal(node);

		// attach resulting annotation
		node->attachValue<Tag>(res);

		// done
		return res;
	}


	namespace detail {

		template<typename Filter>
		struct StaticReachabilityTag {
			bool value;
			StaticReachabilityTag(bool value) : value(value) {}
			bool operator==(const StaticReachabilityTag<Filter>& other) const {
				return value == other.value;
			}
		};

		template<typename Filter>
		class StaticReachabilityCheck : public core::IRVisitor<bool> {

			const Filter& filter;

		public:

			StaticReachabilityCheck(const Filter& filter) : core::IRVisitor<bool>(false), filter(filter) {}


			bool visitVariable(const core::VariablePtr& var) {
				return false;		// requested function can not be reached
			}

			bool visitLiteral(const core::LiteralPtr& var) {
				return false;		// the literal itself is not an execution of and function it has to be the target of a call
			}

			bool visitCallExpr(const core::CallExprPtr& call) {
				auto fun = call->getFunctionExpr();

				// if this is a call to the requested target => done
				if (filter(fun)) return true;

				// if one of the arguments contains a sync point we are done
				for(const auto& arg : call) {
					if (mayReachCallTo(arg, filter)) return true;
				}

				// if any of the arguments is a job or closure => return true (conservative)
				for(const auto& cur : call) {
					if (cur->getType().template isa<core::FunctionTypePtr>()) return true;
					if (cur.template isa<core::JobExprPtr>()) return true;
				}

				// if it is a call to any other literal
				if (fun.isa<core::LiteralPtr>()) {
					// in this case the targeted function is not reached in this sub-branch
					return false;
				}

				// if the target is a variable => fail (conservative)
				if (fun.isa<core::VariablePtr>()) return true;

				// if the target is anything else => check whether synchronizing expressions are included
				return visitDepthFirstOnceInterruptible(fun, [&](const core::CallExprPtr& call) {
					return mayReachCallTo(call, filter);
				});
			}

			bool visitLambdaExpr(const core::LambdaExprPtr& expr) {
				return false;		// the evaluation of the lambda expression is not causing the evaluation of a targeted function
			}

			bool visitJobExpr(const core::JobExprPtr& job) {
				return mayReachCallTo(job->getThreadNumRange(), filter);
			}

			bool visitTupleExpr(const core::TupleExprPtr& tuple) {
				for(const auto& cur : tuple->getExpressions()) {
					if (mayReachCallTo(cur, filter)) return true;
				}
				return false;
			}

			bool visitVectorExpr(const core::VectorExprPtr& vec) {
				for(const auto& cur : vec->getExpressions()) {
					if (mayReachCallTo(cur, filter)) return true;
				}
				return false;
			}

			bool visitStructExpr(const core::StructExprPtr& structExpr) {
				for(const auto& cur : structExpr->getMembers()) {
					if (mayReachCallTo(cur->getValue(), filter)) return true;
				}
				return false;
			}

			bool visitUnionExpr(const core::UnionExprPtr& unionExpr) {
				return mayReachCallTo(unionExpr->getMember(), filter);
			}


			bool visitBindExpr(const BindExprPtr& bindExpr) {
				for(const auto& cur : bindExpr->getBoundExpressions()) {
					if (mayReachCallTo(cur, filter)) return true;
				}
				return false;
			}

			bool visitCastExpr(const CastExprPtr& castExpr) {
				return mayReachCallTo(castExpr->getSubExpression(), filter);
			}

			bool visitCompoundStmt(const CompoundStmtPtr& stmt) {
				// just check all sub-statements
				for(const auto& cur : stmt) {
					if (mayReachCallTo(cur, filter)) return true;
				}
				return false;
			}

			bool visitDeclarationStmt(const DeclarationStmtPtr& stmt) {
				return mayReachCallTo(stmt->getInitialization(), filter);
			}

			bool visitIfStmt(const IfStmtPtr& stmt) {
				return  mayReachCallTo(stmt->getCondition(), filter) ||
						mayReachCallTo(stmt->getThenBody(), filter) ||
						mayReachCallTo(stmt->getElseBody(), filter);
			}

			bool visitForStmt(const ForStmtPtr& stmt) {
				return  mayReachCallTo(stmt->getDeclaration(), filter) ||
						mayReachCallTo(stmt->getEnd(), filter) ||
						mayReachCallTo(stmt->getStep(), filter) ||
						mayReachCallTo(stmt->getBody(), filter);
			}

			bool visitWhileStmt(const WhileStmtPtr& stmt) {
				return  mayReachCallTo(stmt->getCondition(), filter) ||
						mayReachCallTo(stmt->getBody(), filter);
			}

			bool visitReturnStmt(const ReturnStmtPtr& stmt) {
				return mayReachCallTo(stmt->getReturnExpr(), filter);
			}

            bool visitMarkerStmt(const MarkerStmtPtr& stmt) {
                if (mayReachCallTo(stmt->getSubStatement(), filter)) return true;
                return false;
            }

            bool visitStatement(const StatementPtr& stmt) {
            	assert_fail() << "Unsupported Statement Type encountered: " << stmt->getNodeType();
				return false;
            }

			bool visitNode(const core::NodePtr& node) {
            	// the rest we ignore
            	return false;
			}
		};

	}



} // end namespace cba
} // end namespace analysis
} // end namespace insieme
