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

#pragma once

#include <limits>

#include "insieme/core/lang/parallel.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/backend/runtime/runtime_extension.h"

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
		WorkItemRange(const core::ExpressionPtr& min, const core::ExpressionPtr& max, const core::ExpressionPtr& mod) : min(min), max(max), mod(mod) {}
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

	class WorkItemVariant {
		core::LambdaExprPtr implementation;

	  public:
		WorkItemVariant(const core::LambdaExprPtr& impl) : implementation(impl){};

		const core::LambdaExprPtr& getImplementation() const {
			return implementation;
		}

		bool operator==(const WorkItemVariant& other) const {
			return equalTarget(implementation, other.implementation);
		}
	};

	class WorkItemImpl {
		vector<WorkItemVariant> variants;

	  public:
		WorkItemImpl(const vector<WorkItemVariant>& variants = vector<WorkItemVariant>()) : variants(variants){};

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

template <>
struct type_factory<rbe::WorkItemRange> {
	core::TypePtr operator()(core::NodeManager& manager) const {
		return manager.getLangExtension<core::lang::ParallelExtension>().getJobRange();
	}
};

template <>
struct value_to_ir_converter<rbe::WorkItemRange> {
	core::ExpressionPtr operator()(core::NodeManager& manager, const rbe::WorkItemRange& value) const {
		IRBuilder builder(manager);
		auto& parallelExtension = manager.getLangExtension<core::lang::ParallelExtension>();

		// create a call to the range constructor using the given values
		return builder.callExpr(parallelExtension.getJobRange(), parallelExtension.getCreateBoundRangeMod(),
		                        toVector(value.min, value.max, value.mod));
	}
};

template <>
struct ir_to_value_converter<rbe::WorkItemRange> {
	rbe::WorkItemRange operator()(const core::ExpressionPtr& expr) const {
		core::NodeManager& manager = expr->getNodeManager();
		auto& parallelExtension = manager.getLangExtension<core::lang::ParallelExtension>();

		// check for call
		if(expr->getNodeType() != core::NT_CallExpr) { throw InvalidExpression(expr); }

		const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(expr);
		const core::ExpressionPtr& fun = call->getFunctionExpr();
		const core::ExpressionList& args = core::transform::extractArgExprsFromCall(call);

		core::ExpressionPtr min;
		;
		core::ExpressionPtr max = toIR<uint64_t>(manager, std::numeric_limits<uint32_t>::max()); // target filed is 32 bit only ...
		core::ExpressionPtr mod = toIR<uint64_t>(manager, 1);
		if(parallelExtension.isCreateMinRange(fun)) {
			min = args[0];
		} else if(parallelExtension.isCreateBoundRange(fun)) {
			min = args[0];
			max = args[1];
		} else if(parallelExtension.isCreateBoundRangeMod(fun)) {
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

template <>
struct is_encoding_of<rbe::WorkItemRange> {
	bool operator()(const core::ExpressionPtr& expr) const {
		// check call expr
		if(!expr || expr->getNodeType() != core::NT_CallExpr) { return false; }

		// check call target and arguments
		const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(expr);
		auto& parallelExtension = expr->getNodeManager().getLangExtension<core::lang::ParallelExtension>();

		const core::ExpressionPtr& fun = call->getFunctionExpr();

		std::size_t numArgs = 0;
		if(parallelExtension.isCreateMinRange(fun)) {
			numArgs = 1;
		} else if(parallelExtension.isCreateBoundRange(fun)) {
			numArgs = 2;
		} else if(parallelExtension.isCreateBoundRangeMod(fun)) {
			numArgs = 3;
		} else {
			return false;
		}

		// check number and encoding of arguments
		return call->getArgumentDeclarations().size() == numArgs;
	}
};

// -- Work Item Impls ---------------------------

template <>
struct type_factory<rbe::WorkItemVariant> {
	core::TypePtr operator()(core::NodeManager& manager) const {
		return manager.getLangExtension<rbe::RuntimeExtension>().getWorkItemVariantType();
	}
};

template <>
struct value_to_ir_converter<rbe::WorkItemVariant> {
	core::ExpressionPtr operator()(core::NodeManager& manager, const rbe::WorkItemVariant& value) const {
		IRBuilder builder(manager);
		const rbe::RuntimeExtension& ext = manager.getLangExtension<rbe::RuntimeExtension>();

		// ... and call the variant constructor
		return builder.callExpr(ext.getWorkItemVariantType(), ext.getWorkItemVariantCtr(), value.getImplementation());
	}
};

template <>
struct ir_to_value_converter<rbe::WorkItemVariant> {
	rbe::WorkItemVariant operator()(const core::ExpressionPtr& expr) const {
		const rbe::RuntimeExtension& ext = expr->getNodeManager().getLangExtension<rbe::RuntimeExtension>();

		// check constructor format
		if(!core::analysis::isCallOf(expr, ext.getWorkItemVariantCtr())) { throw InvalidExpression(expr); }

		return rbe::WorkItemVariant(static_pointer_cast<const core::LambdaExpr>(core::analysis::getArgument(expr, 0)));
	}
};

template <>
struct is_encoding_of<rbe::WorkItemVariant> {
	bool operator()(const core::ExpressionPtr& expr) const {

		// check call expr
		if(!expr || expr->getNodeType() != core::NT_CallExpr) { return false; }

		// check call target and arguments
		const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(expr);
		const rbe::RuntimeExtension& ext = expr->getNodeManager().getLangExtension<rbe::RuntimeExtension>();

		bool res = true;
		res = res && call->getArgumentDeclarations().size() == static_cast<std::size_t>(1);
		res = res && *call->getFunctionExpr() == *ext.getWorkItemVariantCtr();

		if(!res) { return res; }

		const auto& fun = call->getArgument(0);
		res = res && fun->getNodeType() == core::NT_LambdaExpr;
		res = res && *fun->getType() == *ext.getWorkItemVariantImplType();
		return res;
	}
};


// -- Work Items ------------------------------

/**
 * A type factory creating the IR type used to represent work items.
 */
template <>
struct type_factory<rbe::WorkItemImpl> {
	core::TypePtr operator()(core::NodeManager& manager) const {
		return manager.getLangExtension<rbe::RuntimeExtension>().getWorkItemImplType();
	}
};

/**
 * A encoder for work items.
 */
template <>
struct value_to_ir_converter<rbe::WorkItemImpl> {
	core::ExpressionPtr operator()(core::NodeManager& manager, const rbe::WorkItemImpl& value) const {
		IRBuilder builder(manager);
		const rbe::RuntimeExtension& ext = manager.getLangExtension<rbe::RuntimeExtension>();
		return builder.callExpr(ext.getWorkItemImplType(), ext.getWorkItemImplCtr(), toVector(toIR(manager, value.getVariants())));
	}
};

/**
 * A decoder for work items.
 */
template <>
struct ir_to_value_converter<rbe::WorkItemImpl> {
	rbe::WorkItemImpl operator()(const core::ExpressionPtr& expr) const {
		const rbe::RuntimeExtension& ext = expr->getNodeManager().getLangExtension<rbe::RuntimeExtension>();

		// check constructor format
		if(!core::analysis::isCallOf(expr, ext.getWorkItemImplCtr())) { throw InvalidExpression(expr); }

		return rbe::WorkItemImpl(toValue<vector<rbe::WorkItemVariant>>(core::analysis::getArgument(expr, 0)));
	}
};

/**
 * A membership test for work item encodings.
 */
template <>
struct is_encoding_of<rbe::WorkItemImpl> {
	bool operator()(const core::ExpressionPtr& expr) const {
		// check call expr
		if(!expr || expr->getNodeType() != core::NT_CallExpr) { return false; }

		// check call target and arguments
		const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(expr);
		const rbe::RuntimeExtension& ext = expr->getNodeManager().getLangExtension<rbe::RuntimeExtension>();

		bool res = true;
		res = res && call->getArgumentDeclarations().size() == static_cast<std::size_t>(1);
		res = res && *call->getFunctionExpr() == *ext.getWorkItemImplCtr();

		if(!res) { return res; }

		res = res && isEncodingOf<vector<rbe::WorkItemVariant>>(call->getArgument(0));
		return res;
	}
};

} // end namespace encoder
} // end namespace core
} // end namespace insieme
