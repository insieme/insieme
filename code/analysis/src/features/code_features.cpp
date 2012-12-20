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


#include <array>
#include <memory>
#include <functional>

#include <boost/type_traits/remove_reference.hpp>
#include <boost/type_traits/remove_const.hpp>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/ir_node_traits.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/features/type_features.h"

#include "insieme/annotations/loop_annotations.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/cache_utils.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/transform/pattern/pattern.h"

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


		template<
			typename Extractor,
			typename Value = typename lambda_traits<Extractor>::result_type
		>
		class FeatureAggregator : public core::IRVisitor<Value> {

		public:

			typedef Extractor value_extractor;

		private:

			utils::cache::PointerCache<core::NodePtr, Value> cache;

			/**
			 * The extractor owned and used by this feature aggregator.
			 */
			const Extractor& extractor;

		public:

			FeatureAggregator(const Extractor& extractor)
				: core::IRVisitor<Value>(false), cache(fun(*this, &FeatureAggregator<Extractor, Value>::visitInternal)), extractor(extractor) {}

			virtual Value visit(const core::NodePtr& cur) {
				return cache.get(cur);
			}

			virtual Value visitInternal(const core::NodePtr& cur) {
				return core::IRVisitor<Value>::visit(cur);
			}

		protected:

			Value extractFrom(const core::NodePtr& node) const {
				return extractor(node);
			}

		};


		template<
			typename Extractor,
			typename Value = typename lambda_traits<Extractor>::result_type
		>
		class StaticFeatureAggregator : public FeatureAggregator<Extractor, Value> {

		public:

			StaticFeatureAggregator(const Extractor& extractor)
				: FeatureAggregator<Extractor, Value>(extractor) {}

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



		template<
			typename Extractor,
			typename Value = typename lambda_traits<Extractor>::result_type
		>
		class EstimatedFeatureAggregator : public StaticFeatureAggregator<Extractor, Value> {

			const unsigned numForLoopIterations;

			const unsigned numWhileLoopIterations;

			const unsigned numRecFunDecendent;

		public:

			EstimatedFeatureAggregator(const Extractor& extractor, unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50)
				: StaticFeatureAggregator<Extractor, Value>(extractor), numForLoopIterations(numFor), numWhileLoopIterations(numWhile), numRecFunDecendent(numRec) {}

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

				size_t iterations = 0;

				// check if there is a loop annotation
				if(ptr->hasAnnotation(insieme::annotations::LoopAnnotation::KEY)) {
					// if so, use the value in the annotation as estimation for the number of loops
					iterations = ptr->getAnnotation(annotations::LoopAnnotation::KEY)->getIterations();
				} else {
					// try to estimate the number of iterations
					int64_t stepSize = 1;
					// obtain step size
					core::arithmetic::Formula formula = core::arithmetic::toFormula(ptr->getStep());
					if (formula.isInteger()) {
						stepSize = formula.getConstantValue();
					}
					iterations = (numForLoopIterations * (1.0/stepSize));
				}
				// compute cost of entire loop based on cost of body * iterations
				return this->visit(ptr->getBody()) * iterations + this->extractFrom(ptr);
			}

			virtual Value visitWhileStmt(const core::WhileStmtPtr& ptr) {

				// TODO: add cost for condition evaluation

				size_t iterations = 0;

				// check if there is a loop annotation
				if(ptr->hasAnnotation(annotations::LoopAnnotation::KEY)) {
					// if so, use the value in the annotation as estimation for the number of loops
					iterations = ptr->getAnnotation(annotations::LoopAnnotation::KEY)->getIterations();
				} else {
					iterations = numWhileLoopIterations;
				}

				return this->visit(ptr->getBody()) * iterations + this->extractFrom(ptr);
			}

			virtual Value visitIfStmt(const core::IfStmtPtr& ptr) {
				// split the likelihood of following the condition
				return this->visit(ptr->getThenBody()) * 0.5 + this->visit(ptr->getElseBody()) * 0.5 + this->extractFrom(ptr);
			}

			virtual Value visitSwitchStmt(const core::SwitchStmtPtr& ptr) {

				// compute probability for selecting a special case
				double p = (1.0/(ptr->getCases()->size() + 1));

				Value res = Value();
				for_each(ptr->getCases()->getElements(), [&](const core::SwitchCasePtr& cur) {
					res += this->visit(cur->getBody()) * p;
				});

				res += this->visit(ptr->getDefaultCase()) * p;
				return res + this->extractFrom(ptr);
			}

			virtual Value visitLambdaExpr(const core::LambdaExprPtr& ptr) {
				Value res = this->visit(ptr->getBody());
				if (ptr->isRecursive()) {
					res = res * numRecFunDecendent;
				}
				return res + this->extractFrom(ptr);
			}

		};

		template<
			typename Extractor,
			typename Value = typename lambda_traits<Extractor>::result_type
		>
		class RealFeatureAggregator : public EstimatedFeatureAggregator<Extractor, Value> {

		public:

			RealFeatureAggregator(const Extractor& extractor, unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50)
				: EstimatedFeatureAggregator<Extractor, Value>(extractor, numFor, numWhile, numRec) {}

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
						int64_t a = start.getConstantValue();
						int64_t b = end.getConstantValue();
						int64_t c = step.getConstantValue();

						return this->visit(ptr->getBody()) * ((b-a)/c) + this->extractFrom(ptr);
					}

				} catch (const core::arithmetic::NotAFormulaException& nafe) {
					// accurate boundaries can not be obtained => use estimated fallback
				}

				// just use backup-solution
				return EstimatedFeatureAggregator<Extractor, Value>::visitForStmt(ptr);
			}

		};

		template<
			typename Extractor,
			typename Value = typename lambda_traits<Extractor>::result_type
		>
		class PolyhedralFeatureAggregator : public RealFeatureAggregator<Extractor, Value> {

		public:

			PolyhedralFeatureAggregator(const Extractor& extractor, unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50)
				: RealFeatureAggregator<Extractor, Value>(extractor, numFor, numWhile, numRec) {}

			virtual ~PolyhedralFeatureAggregator() {}


			virtual Value visitInternal(const core::NodePtr& ptr) {
				
				using namespace insieme::analysis::polyhedral;

				// check whether it is a SCoP
				auto scop = scop::ScopRegion::toScop(ptr);

				// check whether current node is the root of a SCoP
				if (!scop) {
					// => use the backup solution
					return RealFeatureAggregator<Extractor, Value>::visitInternal(ptr);
				}

				// use SCoPs
				Value res = Value();
				for_each(*scop, [&](const StmtPtr& cur) {

					// obtain cardinality of the current statement
					core::arithmetic::Piecewise cardinality = polyhedral::cardinality(ptr->getNodeManager(), cur->getDomain());

					// fix parameters (if there are any)
					core::arithmetic::ValueReplacementMap replacements;
					for_each(cardinality.extractValues(), [&](const core::arithmetic::Value& cur) {
						replacements[cur] = 100;
					});

					// fix parameters ...
					cardinality = cardinality.replace(replacements);

					// now it should be a formula
					assert(cardinality.isFormula()
					 	 && "Without variables, the cardinality should be a constant formula!");


					// get formula ..
					core::arithmetic::Formula formula = cardinality.toFormula();

					assert(formula.isConstant() && "Without variables, the formula should be constant!");

					// get number of executions
					int64_t numExecutions = formula.getConstantValue();

					// multiply metric within the statement with the number of executions
					res += this->RealFeatureAggregator<Extractor, Value>::visitInternal(cur->getAddr().getAddressedNode()) * numExecutions;
				});
				return res;
			}

		};


		// --- User level functions ---

		template<typename Extractor, typename Value = typename lambda_traits<Extractor>::result_type>
		Value aggregateStatic(const core::NodePtr& node, const Extractor& extractor) {
			return StaticFeatureAggregator<Extractor, Value>(extractor).visit(node);
		}

		template<typename Extractor, typename Value = typename lambda_traits<Extractor>::result_type>
		Value aggregateWeighted(const core::NodePtr& node, const Extractor& extractor,
				unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50) {
			return EstimatedFeatureAggregator<Extractor, Value>(extractor, numFor, numWhile, numRec).visit(node);
		}

		template<typename Extractor, typename Value = typename lambda_traits<Extractor>::result_type>
		Value aggregateReal(const core::NodePtr& node, const Extractor& extractor,
				unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50) {
			return RealFeatureAggregator<Extractor, Value>(extractor, numFor, numWhile, numRec).visit(node);
		}

		template<typename Extractor, typename Value = typename lambda_traits<Extractor>::result_type>
		Value aggregatePolyhdral(const core::NodePtr& node, const Extractor& extractor,
				unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50) {
			return PolyhedralFeatureAggregator<Extractor, Value>(extractor, numFor, numWhile, numRec).visit(node);
		}

		template<typename Extractor, typename Value = typename lambda_traits<Extractor>::result_type>
		Value aggregate(const core::NodePtr& node, const Extractor& extractor, FeatureAggregationMode mode,
				unsigned numFor = 100, unsigned numWhile = 100, unsigned numRec = 50) {
			switch(mode) {
			case FA_Static: 		return aggregateStatic(node, extractor);
			case FA_Weighted: 		return aggregateWeighted(node, extractor, numFor, numWhile, numRec);
			case FA_Real: 			return aggregateReal(node, extractor, numFor, numWhile, numRec);
			case FA_Polyhedral: 	return aggregatePolyhdral(node, extractor, numFor, numWhile, numRec);
			}
			assert(false && "Invalid mode selected!");
			return Value();
		}



		// -- functional utilities --

		template<typename Ptr> struct node_type;

		template<typename N>
		struct node_type<const core::Pointer<const N>&> {
			BOOST_STATIC_CONSTANT(core::NodeType, nt_value=core::concrete_node_type<N>::nt_value);
		};


		template<typename Extractor, typename Value = typename lambda_traits<Extractor>::result_type>
		struct SpecialNodeTypeWrapper {
			const Extractor extractor;
			SpecialNodeTypeWrapper(const Extractor& extractor) : extractor(extractor) {}
			Value operator()(const core::NodePtr& node) const {
				typedef typename lambda_traits<Extractor>::arg1_type ArgumentType;
				if (node->getNodeType() != node_type<ArgumentType>::nt_value) {
					return Value();
				}
				return extractor(static_pointer_cast<core::CallExprPtr>(node));
			}
		};

		template<typename Extractor>
		SpecialNodeTypeWrapper<Extractor> generalizeNodeType(const Extractor& extractor) {
			return SpecialNodeTypeWrapper<Extractor>(extractor);
		}

	}

	simple_feature_value_type countOps(const core::NodePtr& root, const core::LiteralPtr& op, FeatureAggregationMode mode) {
		return aggregate(root, generalizeNodeType([&](const core::CallExprPtr& ptr)->simple_feature_value_type {
			return (*ptr->getFunctionExpr() == *op)?1:0;
		}), mode);
	}


	SimpleCodeFeatureSpec::SimpleCodeFeatureSpec(const core::ExpressionPtr& op, FeatureAggregationMode mode)
		: CodeFeatureSpec(extractor_function(
				generalizeNodeType([=](const core::CallExprPtr& call)->simple_feature_value_type {
					return *call->getFunctionExpr() == *op;
				})
		  ), mode) {}

	SimpleCodeFeatureSpec::SimpleCodeFeatureSpec(const vector<core::ExpressionPtr>& ops, FeatureAggregationMode mode)
		: CodeFeatureSpec(extractor_function(
				generalizeNodeType([=](const core::CallExprPtr& call)->simple_feature_value_type {
					return ops.empty() || containsPtrToTarget(ops, call->getFunctionExpr());
				})
		  ), mode) {}

	SimpleCodeFeatureSpec::SimpleCodeFeatureSpec(const core::TypePtr& type, const core::ExpressionPtr& op, FeatureAggregationMode mode)
		: CodeFeatureSpec(extractor_function(
				generalizeNodeType([=](const core::CallExprPtr& call)->simple_feature_value_type {
					return *call->getType() == *type && *call->getFunctionExpr() == *op;
				})
		  ), mode) {}

	SimpleCodeFeatureSpec::SimpleCodeFeatureSpec(const vector<core::TypePtr>& types, const vector<core::ExpressionPtr>& ops, FeatureAggregationMode mode)
		: CodeFeatureSpec(extractor_function(
				generalizeNodeType([=](const core::CallExprPtr& call)->simple_feature_value_type {
					return (types.empty() || containsPtrToTarget(types, call->getType())) &&
							(ops.empty() || containsPtrToTarget(ops, call->getFunctionExpr()));
				})
		  ), mode) {}


	SimpleCodeFeaturePtr createSimpleCodeFeature(const string& name, const string& desc, const SimpleCodeFeatureSpec& spec) {
		return std::make_shared<SimpleCodeFeature>(name, desc, spec);
	}

	SimpleCodeFeaturePtr createSimpleCodeFeature(const string& name, const string& desc, const core::ExpressionPtr& op, FeatureAggregationMode mode) {
		return std::make_shared<SimpleCodeFeature>(name, desc, SimpleCodeFeatureSpec(op, mode));
	}

	namespace {

		struct VectorOpCounter {

			const vector<core::TypePtr> elementTypes;
			const vector<core::ExpressionPtr> elementOps;
			const bool considerOpWidth;
//			const unsigned vectorSizeInByte;

			VectorOpCounter(const core::ExpressionPtr& op, bool considerOpWidth = false)
				: elementOps(toVector(op)), considerOpWidth(considerOpWidth) {}

			VectorOpCounter(const vector<core::ExpressionPtr>& ops, bool considerOpWidth = false)
				: elementOps(ops), considerOpWidth(considerOpWidth) {}

			VectorOpCounter(const vector<core::TypePtr>& elementTypes, const vector<core::ExpressionPtr>& ops, bool considerOpWidth = false)
				: elementTypes(elementTypes), elementOps(ops), considerOpWidth(considerOpWidth) {}

			simple_feature_value_type operator()(const core::NodePtr& ptr) {
				if (ptr->getNodeType() != core::NT_CallExpr) {
					return 0;
				}

				core::CallExprPtr call = static_pointer_cast<core::CallExprPtr>(ptr);
				core::ExpressionPtr pointwise = call->getNodeManager().getLangBasic().getVectorPointwise();

				// check whether it is a pointwise operation
				if (!core::analysis::isCallOf(call->getFunctionExpr(), pointwise)) {
					return 0;
				}

				// check whether the element operation is right
				if (!elementOps.empty() && !containsPtrToTarget(elementOps, core::analysis::getArgument(call->getFunctionExpr(), 0))) {
					return 0;
				}

				// check element type
				if (!elementTypes.empty() && call->getType()->getNodeType() == core::NT_VectorType) {
					core::VectorTypePtr type = static_pointer_cast<core::VectorTypePtr>(call->getType());
					if (!containsPtrToTarget(elementTypes, type->getElementType())) {
						return 0;	// not valid since element type does not match
					}
				}

				// check whether the operator width should be considered
				if (!considerOpWidth) {
					return 1;
				}

				// get size of resulting vector
				if (call->getType() == core::NT_VectorType) {
					core::VectorTypePtr type = static_pointer_cast<core::VectorTypePtr>(call->getType());
					auto sizeParam = type->getSize();
					if (sizeParam->getNodeType() == core::NT_ConcreteIntTypeParam) {

						// determine vector size
						unsigned size = static_pointer_cast<core::ConcreteIntTypeParamPtr>(sizeParam)->getValue();

						// determine element size
						unsigned bytes = getEstimatedSizeInBytes(type->getElementType());

						// assuming a 128 bit = 16 byte wide SIMD unit
						return size / (16/bytes);
					}
				}

				// unable to determine the vector size / element width => use default
				return 100;
			}

		};

	}



	SimpleCodeFeatureSpec createVectorOpSpec(const core::ExpressionPtr& elementOp, FeatureAggregationMode mode) {
		return SimpleCodeFeatureSpec(VectorOpCounter(elementOp), mode);
	}

	SimpleCodeFeatureSpec createVectorOpSpec(const vector<core::ExpressionPtr>& elementOps, FeatureAggregationMode mode) {
		return SimpleCodeFeatureSpec(VectorOpCounter(elementOps), mode);
	}

	SimpleCodeFeatureSpec createVectorOpSpec(const core::ExpressionPtr& elementOp, bool considerOpWidth, FeatureAggregationMode mode) {
		return SimpleCodeFeatureSpec(VectorOpCounter(elementOp, considerOpWidth), mode);
	}

	SimpleCodeFeatureSpec createVectorOpSpec(const vector<core::ExpressionPtr>& elementOps, bool considerOpWidth, FeatureAggregationMode mode) {
		return SimpleCodeFeatureSpec(VectorOpCounter(elementOps, considerOpWidth), mode);
	}

	SimpleCodeFeatureSpec createVectorOpSpec(const vector<core::TypePtr>& elementTypes, const vector<core::ExpressionPtr>& elementOps, bool considerOpWidth, FeatureAggregationMode mode) {
		return SimpleCodeFeatureSpec(VectorOpCounter(elementTypes, elementOps, considerOpWidth), mode);
	}

	SimpleCodeFeatureSpec createNumForLoopSpec(FeatureAggregationMode mode) {
		return SimpleCodeFeatureSpec(
				[](const core::NodePtr& cur)->simple_feature_value_type {
			return (cur->getNodeType() == core::NT_ForStmt)?1:0;
		}, mode);
	}


	namespace {

		struct MemoryAccessCounter {

			const MemoryAccessMode mode;
			const MemoryAccessTarget target;

			MemoryAccessCounter(MemoryAccessMode mode, MemoryAccessTarget target)
				: mode(mode), target(target) {}

			simple_feature_value_type operator()(const core::NodePtr& node) const {

				// check for call => only calls are interesting
				if (node->getNodeType() != core::NT_CallExpr) {
					return 0;
				}

				auto& basic = node->getNodeManager().getLangBasic();

				core::CallExprPtr call = static_pointer_cast<core::CallExprPtr>(node);

				// check access mode
				bool match = false;
				switch(mode) {
				case READ:
					match = basic.isRefDeref(call->getFunctionExpr()); break;
				case WRITE:
					match = basic.isRefAssign(call->getFunctionExpr()); break;
				case READ_WRITE:
					match = basic.isRefDeref(call->getFunctionExpr()) ||
							basic.isRefAssign(call->getFunctionExpr());
					break;
				}

				if (!match) {
					return 0;
				}

				// check access target
				if (target == ANY) {
					return 1;	// we don't care about the target
				}

				auto reference = call->getArgument(0);
				if (target == ARRAY) {
					return (core::analysis::isCallOf(reference, basic.getArrayRefElem1D()) ||
							core::analysis::isCallOf(reference, basic.getArrayRefElemND())) ? 1 : 0;
				}

				if (target == VECTOR) {
					return core::analysis::isCallOf(reference, basic.getVectorRefElem()) ? 1 : 0;
				}

				assert(target == SCALAR);

				return (!(core::analysis::isCallOf(reference, basic.getArrayRefElem1D()) ||
						  core::analysis::isCallOf(reference, basic.getArrayRefElemND()) ||
						  core::analysis::isCallOf(reference, basic.getVectorRefElem())))? 1 : 0;
			}

		};
	}


	SimpleCodeFeatureSpec createMemoryAccessSpec(MemoryAccessMode mode, MemoryAccessTarget target, FeatureAggregationMode aggregation) {
		return SimpleCodeFeatureSpec(MemoryAccessCounter(mode, target), aggregation);

	}

	simple_feature_value_type evalFeature(const core::NodePtr& root, const CodeFeatureSpec& feature) {
		// just wrap extractor in a visitor and extract the feature
		return aggregate(root, fun(feature, &CodeFeatureSpec::extract), feature.getMode());
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
		for_each(*this, [&](simple_feature_value_type& cur) { cur *= factor; });
		return *this;
	}


	FeatureValues evalFeatures(const core::NodePtr& root, const vector<SimpleCodeFeatureSpec>& features) {
		vector<const SimpleCodeFeatureSpec*> pointers;
		for_each(features, [&](const SimpleCodeFeatureSpec& cur) {
			pointers.push_back(&cur);
		});
		return evalFeatures(root, pointers);
	}

	FeatureValues evalFeatures(const core::NodePtr& root, const vector<const SimpleCodeFeatureSpec*>& features) {

		typedef std::map<FeatureAggregationMode, vector<std::pair<const SimpleCodeFeatureSpec*, unsigned>>> ModeMap;

		// sort simple code features according to aggregation mode
		int counter = 0;
		ModeMap sorted;
		for_each(features, [&](const SimpleCodeFeatureSpec* cur) {
			sorted[cur->getMode()].push_back(std::make_pair(cur, counter++));
		});

		// resolve features mode by mode
		FeatureValues res(features.size());
		for_each(sorted, [&](const ModeMap::value_type& cur) {

			// resolve current aggregation mode
			FeatureValues cur_res = aggregate(root, [&](const core::NodePtr& node)->FeatureValues {
				FeatureValues res;
				for_each(cur.second, [&](const std::pair<const SimpleCodeFeatureSpec*, unsigned>& cur) {
					res.push_back(cur.first->extract(node));
				});
				return res;
			}, cur.first);

			// copy results to result-vector
			int i=0;
			for_each(cur.second, [&](const std::pair<const SimpleCodeFeatureSpec*, unsigned>& cur) {
				res[cur.second] = cur_res[i++];
			});
		});

		// now all the fields within the resulting value vector should be filled
		return res;
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
		return aggregate(root, generalizeNodeType([&](const core::CallExprPtr& ptr){
			OperatorStatistic res;
			if (ptr->getFunctionExpr()->getNodeType() == core::NT_Literal) {
				res[static_pointer_cast<core::LiteralPtr>(ptr->getFunctionExpr())] = 1;
			}
			return res;
		}), mode);
	}


	// ---------------------------------------- pattern features ------------------------------------

	PatternCodeFeaturePtr createPatternCodeFeature(const string& name, const string& desc, const PatternCodeFeatureSpec& spec) {
		return std::make_shared<PatternCodeFeature>(name, desc, spec);
	}

	PatternCodeFeatureSpec::PatternCodeFeatureSpec(const transform::pattern::TreePatternPtr& pattern, FeatureAggregationMode mode) :
		CodeFeatureSpec(extractor_function(
			generalizeNodeType([=](const core::CallExprPtr& node)->simple_feature_value_type {
				insieme::transform::pattern::MatchOpt&& match = pattern->matchPointer(node);
				return !!match;
			})
	  ), mode) {}

	// ---------------------------------------- lambda features ------------------------------------

	LambdaCodeFeaturePtr createLambdaCodeFeature(const string& name, const string& desc, const LambdaCodeFeatureSpec& spec) {
		return std::make_shared<LambdaCodeFeature>(name, desc, spec);
	}


	// ---------------------------------------- composed features ------------------------------------

	ComposedFeatureSpec::ComposedFeatureSpec(const composingFctTy composingFct, const std::vector<FeaturePtr>& components)
			: composingFct(composingFct), components(components) {
		assert(components.size() > 0 && "Composed features cannot have no composing features");
	}

	Value ComposedFeatureSpec::extract(const core::NodePtr& node) const {
		const auto componentAccess = [&](size_t idx) -> double { return getValue<double>(components.at(idx)->extractFrom(node)); } ;

		return composingFct(node, components, componentAccess);
	}

	ComposedFeaturePtr createComposedFeature(const string& name, const string& desc, const ComposedFeature::composingFctTy composingFct,
			const std::vector<FeaturePtr>& components){
		return std::make_shared<ComposedFeature>(name, desc, ComposedFeatureSpec(composingFct, components));
	}


} // end namespace features
} // end namespace analysis
} // end namesapce insieme
