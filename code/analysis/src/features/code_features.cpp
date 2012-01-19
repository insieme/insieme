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
#include <functional>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/cache_utils.h"
#include "insieme/utils/functional_utils.h"

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

			utils::cache::PointerCache<core::NodePtr, Value> cache;

			/**
			 * The extractor owned and used by this feature aggregator.
			 */
			core::IRVisitor<Value>& extractor;

		public:

			FeatureAggregator(core::IRVisitor<Value>& extractor)
				: core::IRVisitor<Value>(extractor.isVisitingTypes()), cache(fun(*this, &FeatureAggregator<Value>::visitInternal)), extractor(extractor) {}

			virtual Value visit(const core::NodePtr& cur) {
				return cache.get(cur);
			}

			virtual Value visitInternal(const core::NodePtr& cur) {
				return core::IRVisitor<Value>::visit(cur);
			}

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
				Value res = Value();
				for_each(ptr->getChildList(), [&](const core::NodePtr& cur) {
					res += this->visit(cur);
				});

				// ... metric of current node
				return res + this->extractFrom(ptr);
			}

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


			virtual Value visitCompoundStmt(const core::CompoundStmtPtr& ptr) {
				Value res = Value();

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

				Value res = Value();
				for_each(ptr->getCases()->getElements(), [&](const core::SwitchCasePtr& cur) {
					res += this->visit(cur->getBody()) * p;
				});

				res += this->visit(ptr->getDefaultCase()) * p;
				return res;
			}


			virtual Value visitLambdaExpr(const core::LambdaExprPtr& ptr) {
				Value res = this->visit(ptr->getBody());
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


			virtual Value visitInternal(const core::NodePtr& ptr) {

				// check whether it is a SCoP
				auto scop = scop::ScopRegion::toScop(ptr);

				// check whether current node is the root of a SCoP
				if (!scop) {
					// => use the backup solution
					return RealFeatureAggregator<Value>::visitInternal(ptr);
				}

				// use SCoPs
				Value res = Value();
				for_each(*scop, [&](const poly::StmtPtr& cur) {

//std::cout << "Processing statement " << *cur->getAddr().getAddressedNode() << "\n";

					// obtain cardinality of the current statement
					utils::Piecewise<core::arithmetic::Formula> cardinality = poly::cardinality(ptr->getNodeManager(), cur->getDomain());
//std::cout << "Cardinality: " << cardinality << "\n";
//std::cout << "IsFormula: " << core::arithmetic::isFormula(cardinality) << "\n";

					// fix parameters (if there are any)
					core::arithmetic::ValueReplacementMap replacements;
					for_each(core::arithmetic::extract(cardinality), [&](const core::arithmetic::Value& cur) {
						replacements[cur] = 100;
					});

					// fix parameters ...
//					cardinality = core::arithmetic::replace(ptr->getNodeManager(), cardinality, replacements);

					// now it should be a formula
					assert(core::arithmetic::isFormula(cardinality)
					 	 && "Without variables, the cardinality should be a constant formula!");


					// get formula ..
					core::arithmetic::Formula formula = core::arithmetic::toFormula(cardinality);

					assert(formula.isConstant() && "Without variables, the formula should be constant!");

					// get number of executions
					int numExecutions = formula.getConstantValue();
//std::cout << "Num Executions: " << numExecutions << "\n";
//std::cout << "Metric: " << this->extractFrom(cur->getAddr().getAddressedNode()) << "\n";

					// multiply metric within the statement with the number of executions
					res += this->RealFeatureAggregator<Value>::visitInternal(cur->getAddr().getAddressedNode()) * numExecutions;
				});
//std::cout << "\n";
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
			return T();
		}


	}

	unsigned countOps(const core::NodePtr& root, const core::LiteralPtr& op, FeatureAggregationMode mode) {
		auto extractor = core::makeLambdaVisitor([&](const core::CallExprPtr& ptr){
			return (*ptr->getFunctionExpr() == *op)?1u:0u;
		}, false);
		return aggregate(root, extractor, mode);
	}


	namespace {

		struct LiteralOnlySimpleCodeFeatureSpec : public SimpleCodeFeatureSpec {

			const core::ExpressionPtr lit;

			LiteralOnlySimpleCodeFeatureSpec(const core::ExpressionPtr& lit, FeatureAggregationMode mode)
				: SimpleCodeFeatureSpec(mode), lit(lit) {}

			virtual unsigned visitCallExpr(const core::CallExprPtr& ptr) {
				return *lit == *ptr->getFunctionExpr();
			}

		};

		struct TypedLiteralSimpleCodeFeatureSpec : public SimpleCodeFeatureSpec {

			const core::TypePtr type;
			const core::ExpressionPtr lit;

			TypedLiteralSimpleCodeFeatureSpec(const core::TypePtr& type, const core::ExpressionPtr& lit, FeatureAggregationMode mode)
				: SimpleCodeFeatureSpec(mode), type(type), lit(lit) {}

			virtual unsigned visitCallExpr(const core::CallExprPtr& ptr) {
				return *type == *ptr->getType() && *lit == *ptr->getFunctionExpr();
			}

		};

		struct GenericSimpleCodeFeatureSpec : public SimpleCodeFeatureSpec {

			const TypeFilter typeFilter;
			const OperationFilter opFilter;

			GenericSimpleCodeFeatureSpec(const TypeFilter& type, const OperationFilter& op, FeatureAggregationMode mode)
				: SimpleCodeFeatureSpec(mode), typeFilter(type), opFilter(op) {}

			virtual unsigned visitCallExpr(const core::CallExprPtr& ptr) {
				return typeFilter(ptr->getType()) && opFilter(ptr->getFunctionExpr());
			}
		};
	}


	SimpleCodeFeatureSpecPtr countCalls(const core::ExpressionPtr& op, FeatureAggregationMode mode) {
		return std::make_shared<LiteralOnlySimpleCodeFeatureSpec>(op, mode);
	}

	SimpleCodeFeatureSpecPtr countCalls(const core::TypePtr& type, const core::ExpressionPtr& op, FeatureAggregationMode mode) {
		return std::make_shared<TypedLiteralSimpleCodeFeatureSpec>(type, op, mode);
	}

	SimpleCodeFeatureSpecPtr countCalls(const TypeFilter& typeFilter, const OperationFilter& filter, FeatureAggregationMode mode) {
		return std::make_shared<GenericSimpleCodeFeatureSpec>(typeFilter, filter, mode);
	}





	namespace {

		typedef std::function<unsigned(const core::CallExprPtr&)> FeatureExtractor;

		FeatureExtractor getExtractor(const core::lang::BasicGenerator& basic, SimpleFeature feature) {
			switch(feature) {
			case FT_NUM_INT_ARITHMETIC_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isIntArithOp(ptr->getFunctionExpr());
				};
			case FT_NUM_INT_COMPARISON_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isIntCompOp(ptr->getFunctionExpr());
				};
			case FT_NUM_INT_BITWISE_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isBitwiseIntOp(ptr->getFunctionExpr());
				};
			case FT_NUM_UINT_ARITHMETIC_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isUIntArithOp(ptr->getFunctionExpr());
				};
			case FT_NUM_UINT_COMPARISON_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isUIntCompOp(ptr->getFunctionExpr());
				};
			case FT_NUM_UINT_BITWISE_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isBitwiseUIntOp(ptr->getFunctionExpr());
				};
			case FT_NUM_FLOAT_ARITHMETIC_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isFloat(ptr->getType()) && basic.isRealArithOp(ptr->getFunctionExpr());
				};
			case FT_NUM_FLOAT_COMPARISON_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isFloat(ptr->getType()) && basic.isRealCompOp(ptr->getFunctionExpr());
				};
			case FT_NUM_DOUBLE_ARITHMETIC_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isDouble(ptr->getType()) && basic.isRealArithOp(ptr->getFunctionExpr());
				};
			case FT_NUM_DOUBLE_COMPARISON_OPs:
				return [&](const core::CallExprPtr& ptr)->unsigned {
					return basic.isDouble(ptr->getType()) && basic.isRealCompOp(ptr->getFunctionExpr());
				};
			}

			assert(false && "Unsupported feature specified!");
			return [](const core::CallExprPtr& ptr) { return 0u; };
		}

	}



	unsigned evalFeature(const core::NodePtr& root, SimpleFeature feature, FeatureAggregationMode mode) {
		// just wrap extractor in a visitor and extract the feature
		auto visitor = core::makeLambdaVisitor(getExtractor(root->getNodeManager().getLangBasic(), feature), false);
		return aggregate(root, visitor, mode);
	}


	FeatureValues FeatureValues::operator+(const FeatureValues& other) const {
		FeatureValues res = *this;
		res += other;
		return res;
	}

	FeatureValues FeatureValues::operator*(double factor) const {
		FeatureValues res = *this;
		res *= factor;
		return res;
	}

	FeatureValues& FeatureValues::operator+=(const FeatureValues& other) {
		// sum up the common sub-range

		auto itA = begin();
		auto itB = other.begin();
		auto endA = end();
		auto endB = other.end();

		// sum up common part
		while(itA != endA && itB != endB) {
			*itA += *itB;
			++itA; ++itB;
		}

		// add tail
		while(itB != endB) {
			push_back(*itB);
			++itB;
		}

		return *this;
	}

	FeatureValues& FeatureValues::operator*=(double factor) {
		for_each(*this, [&](unsigned& cur) { cur *= factor; });
		return *this;
	}


	FeatureValues evalFeatures(const core::NodePtr& root, const vector<SimpleFeature>& features, FeatureAggregationMode mode) {
		auto& mgr = root->getNodeManager();
		auto& basic = mgr.getLangBasic();

		vector<FeatureExtractor> extractors;
		for_each(features, [&](const SimpleFeature& cur) {
			extractors.push_back(getExtractor(basic, cur));
		});

		auto visitor = core::makeLambdaVisitor(
				[&](const core::CallExprPtr& call)->FeatureValues {
					FeatureValues res;
					for_each(extractors, [&](const FeatureExtractor& cur) {
						res.push_back(cur(call));
					});
					return res;
		}, false);

		return aggregate(root, visitor, mode);
	}


	// -- Operator statistics --


	OperatorStatistic OperatorStatistic::operator+(const OperatorStatistic& other) const {
		OperatorStatistic res = *this;
		res += other;
		return res;
	}

	OperatorStatistic OperatorStatistic::operator*(double factor) const {
		OperatorStatistic res = *this;
		res *= factor;
		return res;
	}


	OperatorStatistic& OperatorStatistic::operator+=(const OperatorStatistic& other) {
		for_each(other, [&](const value_type& cur){
			(*this)[cur.first] += cur.second;
		});
		return *this;
	}

	OperatorStatistic& OperatorStatistic::operator*=(double factor) {
		for_each(*this, [&](const value_type& cur){
			(*this)[cur.first] *= factor;
		});
		return *this;
	}

	OperatorStatistic getOpStats(const core::NodePtr& root, FeatureAggregationMode mode) {
		auto extractor = core::makeLambdaVisitor([&](const core::CallExprPtr& ptr){
			OperatorStatistic res;
			if (ptr->getFunctionExpr()->getNodeType() == core::NT_Literal) {
				res[static_pointer_cast<core::LiteralPtr>(ptr->getFunctionExpr())] = 1;
			}
			return res;
		}, false);
		return aggregate(root, extractor, mode);
	}


} // end namespace features
} // end namespace analysis
} // end namesapce insieme
