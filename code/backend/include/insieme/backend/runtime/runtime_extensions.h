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

#include "insieme/core/expressions.h"
#include "insieme/core/lang/extension.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"

namespace insieme {
namespace backend {
namespace runtime {

	/**
	 * This class offers a list of IR extensions required to model concepts within the
	 * Insieme Runtime. The extensions include literals and types to model work items,
	 * data items and additional runtime functionality.
	 */
	class Extensions : public core::lang::Extension {
	public:

		/**
		 * The function used to trigger the execution of the first work item
		 * within a stand-alone runtime instances.
		 */
		const core::LiteralPtr runStandalone;

		/**
		 * The type used to represent a runtime context within the IR.
		 */
		const core::TypePtr contextType;

		/**
		 * The type used internally to represent work items. The type is treated in an abstract
		 * way and its actual implementation is imported via a runtime-include file.
		 */
		const core::TypePtr workItemType;


		const core::TypePtr workItemImplType;

		const core::LiteralPtr workItemImplCtr;

		const core::TypePtr workItemVariantType;

		const core::LiteralPtr workItemVariantCtr;

		const core::TypePtr workItemVariantImplType;

		// --- Work Item Handling --------------------------------------------

		/**
		 * A marker literal instructing the backend to register the given entry point as a work
		 * item. A call to this literal will not produce any expression or statement. However, it
		 * will add a new entry point to the work-item/implementation table.
		 */
		const core::LiteralPtr registerWorkItemImpl;

		/**
		 * The literal used as a wrapper for the work-item creation function within the runtime.
		 */
		const core::LiteralPtr createWorkItem;

		/**
		 * The literal used as a wrapper for the work-item submission function within the runtime.
		 */
		const core::LiteralPtr submitWorkItem;

		/**
		 * The literal used as a wrapper for the work-item join function offered by the runtime.
		 */
		const core::LiteralPtr joinWorkItem;

		/**
		 * The literal representing the function used for terminating a work item.
		 */
		const core::LiteralPtr exitWorkItem;


		// --- Data Item Handling --------------------------------------------

		const core::TypePtr typeID;

//		/**
//		 * The type used internally to represent data items. The type is treated in an abstract
//		 * way and its actual implementation is imported via a runtime-include file.
//		 *
//		 * The type is DataItem<'a> where 'a is the actual data structure to be represented.
//		 */
//		const core::TypePtr dataItemType;
//
//		const core::LiteralPtr wrapData;
//
//		const core::LiteralPtr unwrapData;

		// --- Light Weight Data Item Handling ----------------------------------

		const core::TypePtr LWDataItemType;

		const core::LiteralPtr wrapLWData;

		const core::LiteralPtr unwrapLWData;

	private:

		friend class core::NodeManager;

		/**
		 * Creates a new instance of this extension set. The given manager is used to construct
		 * the contained literals and types.
		 *
		 * @param manager the manager to be used to construct the required types and literals
		 */
		Extensions(core::NodeManager& manager);

	};


	// ------------------------------------------------------------
	//   A data infrastructure to handle runtime items
	// ------------------------------------------------------------


	class DataItem {


	public:

		static core::TypePtr toDataItemType(const core::TypePtr& type);
		static core::TypePtr toLWDataItemType(const core::StructTypePtr& type);

		static bool isDataItemType(const core::TypePtr& type);
		static bool isLWDataItemType(const core::TypePtr& type);

		static core::TypePtr extractItemType(const core::TypePtr& type);

		static core::StructTypePtr getLWDataItemStruct(const core::StructTypePtr& structType);
		static core::StructExprPtr getLWDataItemValue(unsigned typeID, const core::StructExprPtr& structValue);

	};


	class WorkItemVariant {

		core::LambdaExprPtr implementation;

	public:

		WorkItemVariant(const core::LambdaExprPtr& impl) : implementation(impl) {};

		const core::LambdaExprPtr& getImplementation() const {
			return implementation;
		}

		bool operator==(const WorkItemVariant& other) const {
			return *implementation == *other.implementation;
		}

	};

	class WorkItemImpl {

		vector<WorkItemVariant> variants;

	public:

		WorkItemImpl(const vector<WorkItemVariant>& variants) : variants(variants) {};

		const vector<WorkItemVariant>& getVariants() const {
			return variants;
		}

		bool operator==(const WorkItemImpl& other) const {
			return variants == other.variants;
		}

		static WorkItemImpl decode(const core::ExpressionPtr& expr) {
			assert(core::encoder::isEncodingOf<WorkItemImpl>(expr) && "No an encoding of a matching value!");
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
			ASTBuilder builder(manager);
			const rbe::Extensions& ext = manager.getLangExtension<rbe::Extensions>();

			// just call the variant constructor
			return builder.callExpr(ext.workItemVariantType, ext.workItemVariantCtr, value.getImplementation());
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
			return rbe::WorkItemVariant(impl);
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
			res = res && call->getArguments().size() == static_cast<std::size_t>(1);
			res = res && *call->getFunctionExpr() == *ext.workItemVariantCtr;

			const auto& fun = call->getArgument(0);
			res = res && fun->getNodeType() == core::NT_LambdaExpr;
			res = res && *fun->getType() == *ext.workItemVariantImplType;
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
			ASTBuilder builder(manager);
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
