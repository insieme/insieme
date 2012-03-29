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

#pragma once

#include <limits>

namespace insieme {
namespace backend {
namespace runtime {


	// ------------------------------------------------------------
	//   A data infrastructure to handle runtime items
	// ------------------------------------------------------------

	struct WorkItemRange {
		core::ExpressionPtr min;
		core::ExpressionPtr max;
		core::ExpressionPtr mod;

	public:

		WorkItemRange(const core::ExpressionPtr& min, const core::ExpressionPtr& max, const core::ExpressionPtr& mod)
			: min(min), max(max), mod(mod) {}
	};

	class DataItem {


	public:

		static core::TypePtr toDataItemType(const core::TypePtr& type);
		static core::TypePtr toLWDataItemType(const core::TupleTypePtr& type);

		static bool isDataItemType(const core::TypePtr& type);
		static bool isLWDataItemType(const core::TypePtr& type);

		static core::TypePtr extractItemType(const core::TypePtr& type);

		static core::TupleTypePtr getUnfoldedLWDataItemType(const core::TupleTypePtr& tupleType);
		static core::TupleExprPtr getLWDataItemValue(unsigned typeID, const core::TupleExprPtr& tupleValue);

	};


	struct WorkItemVariantFeatures {
		uint64_t effort;
		bool opencl;
		int64_t implicitRegionId;
		int64_t suggestedThreadNum;

		WorkItemVariantFeatures()
			: effort(0), opencl(false), implicitRegionId(-1), suggestedThreadNum(-1) {}
	};

	class WorkItemVariant {

		core::LambdaExprPtr implementation;
		core::LambdaExprPtr effortEstimator;
		WorkItemVariantFeatures features;

	public:

		WorkItemVariant(const core::LambdaExprPtr& impl, 
				const core::LambdaExprPtr& effortEstimator = core::LambdaExprPtr(), 
				const WorkItemVariantFeatures& features = WorkItemVariantFeatures())
			: implementation(impl), effortEstimator(effortEstimator), features(features) { };

		const core::LambdaExprPtr& getImplementation() const {
			return implementation;
		}
		
		const core::LambdaExprPtr& getEffortEstimator() const {
			return effortEstimator;
		}

		WorkItemVariantFeatures& getFeatures() {
			return features;
		}

		const WorkItemVariantFeatures& getFeatures() const {
			return features;
		}

		bool operator==(const WorkItemVariant& other) const {
			return equalTarget(implementation, other.implementation);
		}

	};

	class WorkItemImpl {

		vector<WorkItemVariant> variants;

	public:

		WorkItemImpl(const vector<WorkItemVariant>& variants) : variants(variants) {};

		vector<WorkItemVariant>& getVariants() {
			return variants;
		}

		const vector<WorkItemVariant>& getVariants() const {
			return variants;
		}

		bool operator==(const WorkItemImpl& other) const {
			return variants == other.variants;
		}

		static WorkItemImpl decode(const core::ExpressionPtr& expr) {
			assert(core::encoder::isEncodingOf<WorkItemImpl>(expr) && "Not an encoding of a matching value!");
			return core::encoder::toValue<WorkItemImpl>(expr);
		}

		static core::ExpressionPtr encode(core::NodeManager& manager, const WorkItemImpl& impl) {
			return core::encoder::toIR<WorkItemImpl>(manager, impl);
		}

	};

} // end namespace runtime
} // end namespace backend

namespace core {
namespace encoder {

	namespace rbe = backend::runtime;

	// ------------------------------------------------------------
	//   Implementations to fit into the data encoding framework
	// ------------------------------------------------------------


	// -- Ranges ---------------------------------

	template<>
	struct type_factory<rbe::WorkItemRange> {
		core::TypePtr operator()(core::NodeManager& manager) const {
			return manager.getLangBasic().getJobRange();
		}
	};

	template<>
	struct value_to_ir_converter<rbe::WorkItemRange> {
		core::ExpressionPtr operator()(core::NodeManager& manager, const rbe::WorkItemRange& value) const {
			IRBuilder builder(manager);
			const core::lang::BasicGenerator& basic = manager.getLangBasic();

			// create a call to the range constructor using the given values
			return builder.callExpr(basic.getJobRange(), basic.getCreateBoundRangeMod(), toVector(value.min, value.max, value.mod));
		}
	};

	template<>
	struct ir_to_value_converter<rbe::WorkItemRange> {
		rbe::WorkItemRange operator()(const core::ExpressionPtr& expr) const {
			core::NodeManager& manager = expr->getNodeManager();
			const lang::BasicGenerator& basic = manager.getLangBasic();

			// check for call
			if (expr->getNodeType() != core::NT_CallExpr) {
				throw InvalidExpression(expr);
			}

			const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(expr);
			const core::ExpressionPtr& fun = call->getFunctionExpr();
			const core::ExpressionList& args = call->getArguments();

			core::ExpressionPtr min;;
			core::ExpressionPtr max = toIR<uint64_t>(manager, std::numeric_limits<uint32_t>::max()); // target filed is 32 bit only ...
			core::ExpressionPtr mod = toIR<uint64_t>(manager, 1);
			if (basic.isCreateMinRange(fun)) {
				min = args[0];
			} else if (basic.isCreateBoundRange(fun)) {
				min = args[0];
				max = args[1];
			} else if (basic.isCreateBoundRangeMod(fun)) {
				min = args[0];
				max = args[1];
				mod = args[2];
			} else {
				throw InvalidExpression(expr);
			}

			// construct result
			return rbe::WorkItemRange(min, max, mod);
		}
	};

	template<>
	struct is_encoding_of<rbe::WorkItemRange> {
		bool operator()(const core::ExpressionPtr& expr) const {

			// check call expr
			if (!expr || expr->getNodeType() != core::NT_CallExpr) {
				return false;
			}

			// check call target and arguments
			const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(expr);
			const core::lang::BasicGenerator& basic = expr->getNodeManager().getLangBasic();

			const core::ExpressionPtr& fun = call->getFunctionExpr();

			std::size_t numArgs = 0;
			if (basic.isCreateMinRange(fun)) {
				numArgs = 1;
			} else if (basic.isCreateBoundRange(fun)) {
				numArgs = 2;
			} else if (basic.isCreateBoundRangeMod(fun)) {
				numArgs = 3;
			} else {
				return false;
			}

			// check number and encoding of arguments
			const core::ExpressionList& args = call->getArguments();
			return args.size() == numArgs;
		}
	};


	// -- Work Item Variant features ------------------
	
	template<>
	struct type_factory<rbe::WorkItemVariantFeatures> {
		core::TypePtr operator()(core::NodeManager& manager) const {
			return manager.getLangExtension<rbe::Extensions>().workItemVariantFeaturesType;
		}
	};

	template<>
	struct value_to_ir_converter<rbe::WorkItemVariantFeatures> {
		core::ExpressionPtr operator()(core::NodeManager& manager, const rbe::WorkItemVariantFeatures& value) const {
			IRBuilder builder(manager);
			const rbe::Extensions& ext = manager.getLangExtension<rbe::Extensions>();

			return builder.callExpr(ext.workItemVariantFeaturesType, ext.workItemVariantFeaturesCtr, 
				toVector(toIR(manager, value.effort), toIR(manager, value.opencl), 
				         toIR(manager, value.implicitRegionId), toIR(manager, value.suggestedThreadNum)));
		}
	};

	template<>
	struct ir_to_value_converter<rbe::WorkItemVariantFeatures> {
		rbe::WorkItemVariantFeatures operator()(const core::ExpressionPtr& expr) const {
			const rbe::Extensions& ext = expr->getNodeManager().getLangExtension<rbe::Extensions>();

			// check constructor format
			if (!core::analysis::isCallOf(expr, ext.workItemVariantFeaturesCtr)) {
				throw InvalidExpression(expr);
			}

			rbe::WorkItemVariantFeatures features;
			features.effort = toValue<uint64_t>(core::analysis::getArgument(expr,0));
			features.opencl = toValue<bool>(core::analysis::getArgument(expr,1));
			features.implicitRegionId = toValue<int64_t>(core::analysis::getArgument(expr,2));
			features.suggestedThreadNum = toValue<int64_t>(core::analysis::getArgument(expr,3));
			
			return features;
		}
	};
	
	template<>
	struct is_encoding_of<rbe::WorkItemVariantFeatures> {
		bool operator()(const core::ExpressionPtr& expr) const {
			// check call expr
			if (!expr || expr->getNodeType() != core::NT_CallExpr) {
				return false;
			}

			// check call target and arguments
			const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(expr);
			const rbe::Extensions& ext = expr->getNodeManager().getLangExtension<rbe::Extensions>();

			bool res = true;
			res = res && *call->getFunctionExpr() == *ext.workItemVariantFeaturesCtr;
			res = res && isEncodingOf<uint64_t>(call->getArgument(0));
			res = res && isEncodingOf<bool>(call->getArgument(1));
			res = res && isEncodingOf<int64_t>(call->getArgument(2));
			res = res && isEncodingOf<int64_t>(call->getArgument(3));

			return res;
		}
	};



	// -- Work Item Impls ---------------------------

	template<>
	struct type_factory<rbe::WorkItemVariant> {
		core::TypePtr operator()(core::NodeManager& manager) const {
			return manager.getLangExtension<rbe::Extensions>().workItemVariantType;
		}
	};

	template<>
	struct value_to_ir_converter<rbe::WorkItemVariant> {
		core::ExpressionPtr operator()(core::NodeManager& manager, const rbe::WorkItemVariant& value) const {
			IRBuilder builder(manager);
			const rbe::Extensions& ext = manager.getLangExtension<rbe::Extensions>();

			// encode the effort function ...
			core::ExpressionPtr effortExpr = value.getEffortEstimator();
			if (!effortExpr) {
				effortExpr = ext.unknownEffort;
			}

			// ... and call the variant constructor
			return builder.callExpr(ext.workItemVariantType, ext.workItemVariantCtr, value.getImplementation(), 
				effortExpr, toIR(manager, value.getFeatures()));
		}
	};

	template<>
	struct ir_to_value_converter<rbe::WorkItemVariant> {
		rbe::WorkItemVariant operator()(const core::ExpressionPtr& expr) const {
			const rbe::Extensions& ext = expr->getNodeManager().getLangExtension<rbe::Extensions>();

			// check constructor format
			if (!core::analysis::isCallOf(expr, ext.workItemVariantCtr)) {
				throw InvalidExpression(expr);
			}

			core::LambdaExprPtr impl = static_pointer_cast<const core::LambdaExpr>(core::analysis::getArgument(expr, 0));
			core::ExpressionPtr effortExpr = core::analysis::getArgument(expr,1);

			core::LambdaExprPtr effort =
					(effortExpr->getNodeType() == core::NT_LambdaExpr)		?
					static_pointer_cast<core::LambdaExprPtr>(effortExpr)	:
					core::LambdaExprPtr();

			auto features = toValue<rbe::WorkItemVariantFeatures>(core::analysis::getArgument(expr,2));

			return rbe::WorkItemVariant(impl, effort, features);
		}
	};

	template<>
	struct is_encoding_of<rbe::WorkItemVariant> {
		bool operator()(const core::ExpressionPtr& expr) const {

			// check call expr
			if (!expr || expr->getNodeType() != core::NT_CallExpr) {
				return false;
			}

			// check call target and arguments
			const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(expr);
			const rbe::Extensions& ext = expr->getNodeManager().getLangExtension<rbe::Extensions>();

			bool res = true;
			res = res && call->getArguments().size() == static_cast<std::size_t>(3);
			res = res && *call->getFunctionExpr() == *ext.workItemVariantCtr;

			const auto& fun = call->getArgument(0);
			res = res && fun->getNodeType() == core::NT_LambdaExpr;
			res = res && *fun->getType() == *ext.workItemVariantImplType;

			const auto& effort = call->getArgument(1);
			res = res && (
					*effort == *ext.unknownEffort ||
					(effort->getNodeType() == core::NT_LambdaExpr && *effort->getType() == *ext.workItemVariantEffortFunType)
				);

			res = res && isEncodingOf<rbe::WorkItemVariantFeatures>(call->getArgument(2));

			return res;
		}
	};


	// -- Work Items ------------------------------

	/**
	 * A type factory creating the IR type used to represent work items.
	 */
	template<>
	struct type_factory<rbe::WorkItemImpl> {
		core::TypePtr operator()(core::NodeManager& manager) const {
			return manager.getLangExtension<rbe::Extensions>().workItemImplType;
		}
	};

	/**
	 * A encoder for work items.
	 */
	template<>
	struct value_to_ir_converter<rbe::WorkItemImpl> {
		core::ExpressionPtr operator()(core::NodeManager& manager, const rbe::WorkItemImpl& value) const {
			IRBuilder builder(manager);
			const rbe::Extensions& ext = manager.getLangExtension<rbe::Extensions>();
			return builder.callExpr(ext.workItemImplType, ext.workItemImplCtr, toVector(toIR(manager, value.getVariants())));
		}
	};

	/**
	 * A decoder for work items.
	 */
	template<>
	struct ir_to_value_converter<rbe::WorkItemImpl> {
		rbe::WorkItemImpl operator()(const core::ExpressionPtr& expr) const {
			const rbe::Extensions& ext = expr->getNodeManager().getLangExtension<rbe::Extensions>();

			// check constructor format
			if (!core::analysis::isCallOf(expr, ext.workItemImplCtr)) {
				throw InvalidExpression(expr);
			}

			return rbe::WorkItemImpl(toValue<vector<rbe::WorkItemVariant>>(core::analysis::getArgument(expr, 0)));
		}
	};

	/**
	 * A membership test for work item encodings.
	 */
	template<>
	struct is_encoding_of<rbe::WorkItemImpl> {
		bool operator()(const core::ExpressionPtr& expr) const {

			// check call expr
			if (!expr || expr->getNodeType() != core::NT_CallExpr) {
				return false;
			}

			// check call target and arguments
			const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(expr);
			const rbe::Extensions& ext = expr->getNodeManager().getLangExtension<rbe::Extensions>();

			bool res = true;
			res = res && call->getArguments().size() == static_cast<std::size_t>(1);
			res = res && *call->getFunctionExpr() == *ext.workItemImplCtr;
			res = res && isEncodingOf<vector<rbe::WorkItemVariant>>(call->getArgument(0));
			return res;
		}
	};

} // end namespace encoder
} // end namespace core
} // end namespace insieme
