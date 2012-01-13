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

#include "insieme/analysis/features/code_features.h"

#include "insieme/core/ir_visitor.h"

#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/utils/set_utils.h"

namespace insieme {
namespace analysis {
namespace features {


namespace {


		class EstimatedFeatureExtractor : public core::IRVisitor<int> {

			core::IRVisitor<int>& counter;

			const unsigned numForLoopIterations;

			const unsigned numWhileLoopIterations;

			const unsigned numRecFunDecendent;

			utils::set::PointerSet<core::VariablePtr> recVars;


		public:

			EstimatedFeatureExtractor(core::IRVisitor<int>& counter, unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50)
				: core::IRVisitor<int>(false), counter(counter), numForLoopIterations(numFor), numWhileLoopIterations(numWhile), numRecFunDecendent(numRec) {}

		protected:


		//		AST_TERMINAL(CompoundStmt, Statement)
		//		AST_TERMINAL(WhileStmt, Statement)
		//		AST_TERMINAL(ForStmt, Statement)
		//		AST_TERMINAL(IfStmt, Statement)
		//		AST_TERMINAL(SwitchStmt, Statement)


			virtual int visitCompoundStmt(const core::CompoundStmtPtr& ptr) {
				int res = 0;

				// just sum up the results of the individual statements
				for_each(ptr->getStatements(), [&](const core::StatementPtr& cur) {
					res += this->visit(cur);
				});

				return res;
			}

			virtual int visitForStmt(const core::ForStmtPtr& ptr) {

				// TODO: consider init and checks

				// obtain step size
				int stepSize = 1;
				core::arithmetic::Formula formula = core::arithmetic::toFormula(ptr->getStep());
				if (formula.isInteger()) {
					stepSize = formula.getConstantValue();
				}


				// in case the for loop should be accurately counted
//				int low = 0;
//				int high = numForLoopIterations;
//
//				core::arithmetic::Formula start = core::arithmetic::toFormula(ptr->getStart());
//				core::arithmetic::Formula end = core::arithmetic::toFormula(ptr->getEnd());
//
//				if (start.isInteger() && end.isInteger()) {
//					low = start.getConstantValue();
//					high = end.getConstantValue();
//
//					return visit(ptr->getBody()) * ((high - low) / stepSize);
//				}


				// compute cost of entire loop based on cost of body * iterations
				return visit(ptr->getBody()) * (numForLoopIterations * (1.0/stepSize));
			}

			virtual int visitWhileStmt(const core::WhileStmtPtr& ptr) {

				// TODO: add cost for condition evaluation

				return visit(ptr->getBody()) * numWhileLoopIterations;
			}

			virtual int visitIfStmt(const core::IfStmtPtr& ptr) {
				// split the likelihood of following the condition
				return visit(ptr->getThenBody()) * 0.5 + visit(ptr->getElseBody()) * 0.5;
			}

			virtual int visitSwitchStmt(const core::SwitchStmtPtr& ptr) {

				// compute probability for selecting a special case
				double p = (1.0/(ptr->getCases()->size() + 1));

				int res = 0;
				for_each(ptr->getCases()->getElements(), [&](const core::SwitchCasePtr& cur) {
					res += this->visit(cur->getBody()) * p;
				});

				res += visit(ptr->getDefaultCase()) * p;
				return res;
			}


			virtual int visitLambdaExpr(const core::LambdaExprPtr& ptr) {
				int res = ptr->getBody();
				if (ptr->isRecursive()) {
					res = res * numRecFunDecendent;
				}
				return res;
			}

			virtual int visitNode(const core::NodePtr& ptr) {
				int res = 0;
				for_each(ptr->getChildList(), [&](const core::NodePtr& cur) {
					res += this->visit(cur);
				});
				return res + counter.visit(ptr);
			}

	//	public:
	//
	//		virtual int visit(const core::NodePtr& ptr) {
	//			int res = IRVisitor<int>::visit(ptr);
	//			if (ptr->getNodeCategory() == core::NC_Expression) {
	//				std::cout << *ptr << " - " << res << "\n";
	//			}
	//			return res;
	//		}

		};


		int count(const core::NodePtr& node, core::IRVisitor<int>& counter) {
			return EstimatedFeatureExtractor(counter).visit(node);
		}



		class ExpressionCounter : public core::IRVisitor<int> {

		public:

			ExpressionCounter() : core::IRVisitor<int>(false) {}

		protected:

			virtual int visitExpression(const core::ExpressionPtr& ptr) {
				return 1;
			}

			virtual int visitCallExpr(const core::CallExprPtr& ptr) {
				return ((ptr->getFunctionExpr()->getNodeType() == core::NT_Literal)?0:1);
			}
		};


		class OpCounter : public core::IRVisitor<int> {

			const core::LiteralPtr op;

		public:

			OpCounter(const core::LiteralPtr& op) : core::IRVisitor<int>(false), op(op) {}

		protected:

			virtual int visitCallExpr(const core::CallExprPtr& ptr) {
				return (*ptr->getFunctionExpr() == *op)?1:0;
			}
		};

	}


	int countOps(const core::NodePtr& node) {
		ExpressionCounter counter;
		return count(node, counter);
	}

	int countOps(const core::NodePtr& root, const core::LiteralPtr& op) {
		OpCounter counter(op);
		return count(root, counter);
	}

} // end namespace features
} // end namespace analysis
} // end namesapce insieme
