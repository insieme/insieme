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


#include <memory>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/utils/set_utils.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme {
namespace analysis {
namespace features {


namespace {


		// The code feature extraction infrastructure consists of two
		// types of components:
		//		- CodeFeatureExtractors: an abstract base class for actual
		//			feature extraction implementations focusing on local aspects only
		//		- FeatureAggregator: an abstract base class using an instance of
		//			a code feature extractor to obtain the actual features which are
		//			then aggregated according to some kind of policy.




		template<typename Value>
		class FeatureAggregator : public core::IRVisitor<Value> {

			/**
			 * The extractor owned and used by this feature aggregator.
			 */
			core::IRVisitor<Value>& extractor;

		public:

			FeatureAggregator(core::IRVisitor<Value>& extractor)
				: core::IRVisitor<Value>(extractor.isVisitingTypes()), extractor(extractor) {}

		protected:

			Value extractFrom(const core::NodePtr& node) const {
				return extractor.visit(node);
			}

		};


		template<typename Value>
		class StaticFeatureAggregator : public FeatureAggregator<Value> {

		public:

			StaticFeatureAggregator(core::IRVisitor<Value>& extractor)
				: FeatureAggregator<Value>(extractor) {}

			virtual ~StaticFeatureAggregator() {}

		protected:

			virtual Value visitNode(const core::NodePtr& ptr) {
				// just sum up metric of child nodes ...
				int res = 0;
				for_each(ptr->getChildList(), [&](const core::NodePtr& cur) {
					res += this->visit(cur);
				});

				// ... metric of current node
				return res + this->extractFrom(ptr);
			}


//		public:
//
//			virtual int visit(const core::NodePtr& ptr) {
//				int res = IRVisitor<int>::visit(ptr);
//				if (ptr->getNodeCategory() == core::NC_Expression) {
//					std::cout << *ptr << " - " << res << "\n";
//				}
//				return res;
//			}


		};



		template<typename Value>
		class EstimatedFeatureAggregator : public StaticFeatureAggregator<Value> {

			const unsigned numForLoopIterations;

			const unsigned numWhileLoopIterations;

			const unsigned numRecFunDecendent;

		public:

			EstimatedFeatureAggregator(core::IRVisitor<Value>& extractor, unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50)
				: StaticFeatureAggregator<Value>(extractor), numForLoopIterations(numFor), numWhileLoopIterations(numWhile), numRecFunDecendent(numRec) {}

			virtual ~EstimatedFeatureAggregator() {}

		protected:


		//		AST_TERMINAL(CompoundStmt, Statement)
		//		AST_TERMINAL(WhileStmt, Statement)
		//		AST_TERMINAL(ForStmt, Statement)
		//		AST_TERMINAL(IfStmt, Statement)
		//		AST_TERMINAL(SwitchStmt, Statement)


			virtual Value visitCompoundStmt(const core::CompoundStmtPtr& ptr) {
				Value res = 0;

				// just sum up the results of the individual statements
				for_each(ptr->getStatements(), [&](const core::StatementPtr& cur) {
					res += this->visit(cur);
				});

				return res + this->extractFrom(ptr);
			}

			virtual Value visitForStmt(const core::ForStmtPtr& ptr) {

				// TODO: consider init and checks

				// obtain step size
				int stepSize = 1;
				core::arithmetic::Formula formula = core::arithmetic::toFormula(ptr->getStep());
				if (formula.isInteger()) {
					stepSize = formula.getConstantValue();
				}

				// compute cost of entire loop based on cost of body * iterations
				return this->visit(ptr->getBody()) * (numForLoopIterations * (1.0/stepSize));
			}

			virtual Value visitWhileStmt(const core::WhileStmtPtr& ptr) {

				// TODO: add cost for condition evaluation

				return this->visit(ptr->getBody()) * numWhileLoopIterations;
			}

			virtual Value visitIfStmt(const core::IfStmtPtr& ptr) {
				// split the likelihood of following the condition
				return this->visit(ptr->getThenBody()) * 0.5 + this->visit(ptr->getElseBody()) * 0.5;
			}

			virtual Value visitSwitchStmt(const core::SwitchStmtPtr& ptr) {

				// compute probability for selecting a special case
				double p = (1.0/(ptr->getCases()->size() + 1));

				int res = 0;
				for_each(ptr->getCases()->getElements(), [&](const core::SwitchCasePtr& cur) {
					res += this->visit(cur->getBody()) * p;
				});

				res += this->visit(ptr->getDefaultCase()) * p;
				return res;
			}


			virtual Value visitLambdaExpr(const core::LambdaExprPtr& ptr) {
				int res = ptr->getBody();
				if (ptr->isRecursive()) {
					res = res * numRecFunDecendent;
				}
				return res;
			}

		};

		template<typename Value>
		class RealFeatureAggregator : public EstimatedFeatureAggregator<Value> {

		public:

			RealFeatureAggregator(core::IRVisitor<Value>& extractor, unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50)
				: EstimatedFeatureAggregator<Value>(extractor, numFor, numWhile, numRec) {}

			virtual ~RealFeatureAggregator() {}

		protected:

			virtual Value visitForStmt(const core::ForStmtPtr& ptr) {

				// TODO: consider init and checks

				// try collecting start, end and stepsize
				try {

					// extract formulas
					core::arithmetic::Formula start = core::arithmetic::toFormula(ptr->getStart());
					core::arithmetic::Formula end   = core::arithmetic::toFormula(ptr->getEnd());
					core::arithmetic::Formula step  = core::arithmetic::toFormula(ptr->getStep());

					// if there are constant loop boundaries => use them
					if (start.isInteger() && end.isInteger() && step.isInteger()) {
						int a = start.getConstantValue();
						int b = end.getConstantValue();
						int c = step.getConstantValue();

						return this->visit(ptr->getBody()) * ((b-a)/c);
					}

				} catch (const core::arithmetic::NotAFormulaException& nafe) {
					// accurate boundaries can not be obtained => use estimated fallback
				}

				// just use backup-solution
				return EstimatedFeatureAggregator<Value>::visitForStmt(ptr);
			}

		};

		template<typename Value>
		class PolyhedralFeatureAggregator : public RealFeatureAggregator<Value> {

		public:

			PolyhedralFeatureAggregator(core::IRVisitor<Value>& extractor, unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50)
				: RealFeatureAggregator<Value>(extractor, numFor, numWhile, numRec) {}

			virtual ~PolyhedralFeatureAggregator() {}


			virtual Value visit(const core::NodePtr& ptr) {

				// check whether it is a SCoP
				auto scop = scop::ScopRegion::toScop(ptr);

				if (!scop) {
					// => use the backup solution
					std::cout << "Not a SCoP: \n" << *ptr << "\n\n";
					return RealFeatureAggregator<Value>::visit(ptr);
				}

				// use SCoPs
				Value res = 0;
				for_each(*scop, [&](const poly::StmtPtr& cur) {

					// obtain cardinality of the current statement
					utils::Piecewise<core::arithmetic::Formula> cardinality = poly::cardinality(ptr->getNodeManager(), cur->getDomain());

					// fix parameters (if there are any)
					core::arithmetic::ValueReplacementMap replacements;
					for_each(core::arithmetic::extract(cardinality), [&](const core::arithmetic::Value& cur) {
						replacements[cur] = 100;
					});

					// TODO: fix parameters ...
					// cardinality = core::arithmetic::replace(cardinality);

					// now it should be a formula
					assert(core::arithmetic::isFormula(cardinality)
					 	 && "Without variables, the cardinality should be a constant formula!");


					// get formula ..
					core::arithmetic::Formula formula = core::arithmetic::toFormula(cardinality);

					assert(formula.isConstant() && "Without variables, the formula should be constant!");

					// get number of executions
					int numExecutions = formula.getConstantValue();

					// multiply metric within the statement with the number of executions
					res += this->extractFrom(cur->getAddr().getAddressedNode()) * numExecutions;
				});

				return res;
			}

		};


		// --- User level functions ---

		template<typename T>
		T aggregateStatic(const core::NodePtr& node, core::IRVisitor<T>& extractor) {
			return StaticFeatureAggregator<T>(extractor).visit(node);
		}

		template<typename T>
		T aggregateWeighted(const core::NodePtr& node, core::IRVisitor<T>& extractor) {
			return EstimatedFeatureAggregator<T>(extractor).visit(node);
		}

		template<typename T>
		T aggregateReal(const core::NodePtr& node, core::IRVisitor<T>& extractor) {
			return RealFeatureAggregator<T>(extractor).visit(node);
		}

		template<typename T>
		T aggregatePolyhdral(const core::NodePtr& node, core::IRVisitor<T>& extractor) {
			return PolyhedralFeatureAggregator<T>(extractor).visit(node);
		}

		template<typename T>
		T aggregate(const core::NodePtr& node, core::IRVisitor<T>& extractor, FeatureAggregationMode mode) {
			switch(mode) {
			case FA_Static: 		return aggregateStatic(node, extractor);
			case FA_Weighted: 		return aggregateWeighted(node, extractor);
			case FA_Real: 			return aggregateReal(node, extractor);
			case FA_Polyhedral: 	return aggregatePolyhdral(node, extractor);
			}
			assert(false && "Invalid mode selected!");
			return 0;
		}

	}

	int countOps(const core::NodePtr& root, const core::LiteralPtr& op, FeatureAggregationMode mode) {
		auto extractor = core::makeLambdaVisitor([&](const core::CallExprPtr& ptr){
			return (*ptr->getFunctionExpr() == *op)?1:0;
		}, false);
		return aggregate(root, extractor, mode);
	}

} // end namespace features
} // end namespace analysis
} // end namesapce insieme
