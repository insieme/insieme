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

#include "insieme/core/transform/simplify.h"

#include "insieme/utils/logging.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace core {
namespace transform {


	namespace {


		/**
		 * A Node mapper conducting the actual simplifications.
		 */
		class Simplifier : public CachedNodeMapping {

			NodeManager& manager;

		public:

			/**
			 * Default constructor for the simplification.
			 */
			Simplifier(NodeManager& manager)
				: manager(manager) {}


			NodePtr simplifyCall(const NodePtr& ptr) {
				// it has to be a call expression ...
				if (ptr->getNodeType() != core::NT_CallExpr) return ptr;
				// try in-lining of call expression
				return tryInlineToExpr(manager, ptr.as<CallExprPtr>());
			}

			NodePtr simplifyIf(const NodePtr& ptr) {
				// it has to be an if ...
				if (ptr->getNodeType() != core::NT_IfStmt) return ptr;

				// check condition
				IfStmtPtr ifStmt = ptr.as<IfStmtPtr>();

				try {
					// evaluate constraint
					arithmetic::Constraint cond = arithmetic::toConstraint(ifStmt->getCondition());

					// if condition is always valid
					if (cond.isValid()) {
						return ifStmt->getThenBody();
					}

					// if condition is not satisfiable
					if (cond.isUnsatisfiable()) {
						return ifStmt->getElseBody();
					}

				} catch (const arithmetic::NotAConstraintException& nce) {
					// condition can not be statically evaluated
					// => just ignore, return unmodified
				}

				// if condition is undecided => keep if stmt
				return ifStmt;
			}

			NodePtr simplifyWhile(const NodePtr& ptr) {
				// it has to be a while loop ...
				if (ptr->getNodeType() != core::NT_WhileStmt) return ptr;

				// check condition
				WhileStmtPtr whileStmt = ptr.as<WhileStmtPtr>();

				try {
					// evaluate constraint
					arithmetic::Constraint cond = arithmetic::toConstraint(whileStmt->getCondition());

					// if condition is always valid
					if (cond.isUnsatisfiable()) {
						// replace while-loop with no-op
						return IRBuilder(manager).getNoOp();
					}

				} catch (const arithmetic::NotAConstraintException& nce) {
					// condition can not be statically evaluated
					// => just ignore, return unmodified
				}

				// if condition is undecided => keep stmt
				return whileStmt;
			}

			NodePtr simplifyFor(const NodePtr& ptr) {
				// TODO: check iterator range ... if empty, drop loop
				return ptr;
			}

			NodePtr simplifyCompound(const NodePtr& ptr) {
				// it has to be a compound statement
				if (ptr->getNodeType() != core::NT_CompoundStmt) return ptr;

				// process compound statement
				CompoundStmtPtr stmt = ptr.as<CompoundStmtPtr>();

				// re-build stmt using filtered list of statements
				bool changed = false;
				IRBuilder builder(manager);
				vector<StatementPtr> newStmts;
				for(const StatementPtr& cur : stmt) {

					// skip no-ops
					if (builder.isNoOp(cur)) {
						changed = true;
						continue;
					}

					// inline compound statements without any sub-declarations
					if (cur->getNodeType() == NT_CompoundStmt) {
						// check whether there are sub-declarations
						CompoundStmtPtr inner = cur.as<CompoundStmtPtr>();
						if (!any(inner.getStatements(), [](const StatementPtr& cur) { return cur->getNodeType() == NT_DeclarationStmt; })) {
							copy(inner, std::back_inserter(newStmts));
							copy(inner, newStmts.end());
							changed = true;
							continue;
						}
					}

					// keep statement within new compound statement
					newStmts.push_back(cur);
				}

				// do not build a new node if there was no change
				if (!changed) return stmt;

				// build resulting compound statement
				return CompoundStmt::get(manager, newStmts);
			}

			NodePtr simplifyExpr(const NodePtr& ptr) {
				// TODO: remove general identities
				//	e.g. 1+1 => 2, ref.deref(ref.var(x)) => x
				return ptr;
			}


			/**
			 * Conducts the actual simplification.
			 */
			virtual const NodePtr resolveElement(const NodePtr& ptr) {

				// skip types
				if (ptr->getNodeCategory() == NC_Type) {
					return ptr;
				}

				NodePtr res = ptr;

				// special treatment for bind-expression since inner call must not be premature simplified
				if (ptr->getNodeType() == NT_BindExpr) {
					// simplify function of inner call expression

					BindExprPtr bind = ptr.as<BindExprPtr>();
					CallExprPtr call = bind->getCall();

					// improve function part and parameters, yet preserve call
					ExpressionPtr newFun = call->getFunctionExpr()->substitute(manager, *this);
					vector<ExpressionPtr> args;
					for(auto& cur : call->getArguments()) {
						args.push_back(cur->substitute(manager, *this));
					}

					// construct bind with substituted call
					CallExprPtr newCall = CallExpr::get(manager, call->getType(), newFun, args);
					res = BindExpr::get(manager, bind->getType().as<FunctionTypePtr>(), bind->getParameters(), newCall);

				} else {
					// for all other nodes run simplifier recursively bottom up
					res = ptr->substitute(manager, *this);
				}

				// investigate current node
				NodePtr old;
				while(old != res) {
					old = res;

					// apply all kind of simplifications
					res = simplifyCall(res);
					res = simplifyIf(res);
					res = simplifyWhile(res);
					res = simplifyFor(res);
					res = simplifyExpr(res);
					res = simplifyCompound(res);
				}

				// no more modifications possible => done
				return res;
			}

		};


	}



	NodePtr simplify(NodeManager& manager, const NodePtr& code) {
		// use the simplify-mapper for the actual operation operation
		return Simplifier(manager).map(code);
	}


} // end namespace transform
} // end namespace core
} // end namespace insieme
