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

#include "insieme/backend/runtime/runtime_extensions.h"

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/encoder/lists.h"

namespace insieme {
namespace backend {
namespace runtime {

	const string DATA_ITEM_TYPE_NAME = "DataItem";
	const string LW_DATA_ITEM_TYPE_NAME = "LWDataItem";

	namespace {

		const core::TypePtr getContextType(core::NodeManager& manager) {
			return core::GenericType::get(manager, "Context");
		}

		const core::LiteralPtr getRunStandalone(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// create type + literal
			core::NamedCompositeType::Entries members;
			members.push_back(core::NamedCompositeType::Entry(builder.identifier("argc"), basic.getInt4()));
			members.push_back(core::NamedCompositeType::Entry(builder.identifier("argv"),
					builder.refType(builder.arrayType(builder.refType(builder.arrayType(basic.getChar()))))));

			core::TypePtr args = DataItem::toLWDataItemType(builder.structType(members));
			core::TypePtr type = builder.functionType(toVector(args), basic.getUnit());
			return builder.literal(type, "runStandalone");
		}


		// -- Runtime Types ---------------------------------------------------------

		const core::TypePtr getWorkItemType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the work item type as a generic type
			return builder.genericType("WorkItem");
		}

		const core::TypePtr getWorkItemImplType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create a generic type representing the work item implementation
			return builder.genericType("WorkItemImpl");
		}

		const core::TypePtr getWorkItemVariantType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// crate a generic type representing a work item variant
			return builder.genericType("WorkItemVariant");
		}

		const core::TypePtr getWorkItemVariantImplType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			core::TypePtr argType = builder.refType(getWorkItemType(manager));
			return builder.functionType(toVector(argType), basic.getUnit());
		}

		const core::TypePtr getDataItemType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the work item type as a generic type
			return builder.genericType(DATA_ITEM_TYPE_NAME, toVector<core::TypePtr>(builder.typeVariable("a")));
		}

		const core::TypePtr getLWDataItemType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the work item type as a generic type
			return builder.genericType(LW_DATA_ITEM_TYPE_NAME, toVector<core::TypePtr>(builder.typeVariable("a")));
		}

		const core::TypePtr getTypeIDType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			return builder.genericType("irt_type_id");
		}


		// -- Runtime Constructors ------------------------------------------------------

		const core::LiteralPtr getWorkItemImplCtr(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the work item constructor token
			core::TypePtr variants = core::encoder::getListType(getWorkItemVariantType(manager));
			core::TypePtr funType = builder.functionType(toVector(variants), getWorkItemImplType(manager));
			return builder.literal(funType, "WorkItemImpl");
		}

		const core::LiteralPtr getWorkItemVariantCtr(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the work item constructor token
			core::TypePtr variants = core::encoder::getListType(getWorkItemImplType(manager));
			core::TypePtr implFunType = getWorkItemVariantImplType(manager);
			core::TypePtr funType = builder.functionType(toVector(implFunType), getWorkItemVariantType(manager));
			return builder.literal(funType, "WorkItemVariant");
		}


		const core::LiteralPtr getRegisterWorkItemImpl(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// parameter list:
			// 	- the lambdas implementing this work item
			//  - the work-item parameter type
			//  - the data item requirements
			//  - the resource requirements
			// => all encoded within list of variants

			core::TypePtr unit = basic.getUnit();
			core::TypePtr resType = builder.functionType(toVector(getWorkItemImplType(manager)), unit);

			return builder.literal(resType, "registerWorkItemImpl");
		}

		const core::LiteralPtr getCreateWorkItem(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// parameter list:
			//	- start/end/step-size of range
			//  - work item entry
			//  - parameter data item

			core::TypePtr intType = basic.getUIntGen();
			core::TypePtr workItemImplType = getWorkItemImplType(manager);
			core::TypePtr dataItemType = getDataItemType(manager);
			core::TypePtr workItemType = getWorkItemType(manager);

			// the create-work-item function
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(intType, intType, intType, workItemImplType, dataItemType), workItemType);

			return builder.literal(type, "createWorkItem");
		}

		const core::LiteralPtr getSubmitWorkItem(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// simply (WorkItem)->unit
			core::TypePtr unit = basic.getUnit();
			core::TypePtr workItemType = getWorkItemType(manager);

			// the create-work-item function
			core::TypePtr type = builder.functionType(toVector(workItemType), unit);

			return builder.literal(type, "submitWorkItem");
		}

		const core::LiteralPtr getJoinWorkItem(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// simply (WorkItem)->unit
			core::TypePtr unit = basic.getUnit();
			core::TypePtr workItemType = getWorkItemType(manager);

			// the create-work-item function
			core::TypePtr type = builder.functionType(toVector(workItemType), unit);

			return builder.literal(type, "joinWorkItem");
		}

		const core::LiteralPtr getExitWorkItem(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// simply (WorkItem)->unit
			core::TypePtr unit = basic.getUnit();
			core::TypePtr workItemType = builder.refType(getWorkItemType(manager));

			// the create-work-item function
			core::TypePtr type = builder.functionType(toVector(workItemType), unit);

			return builder.literal(type, "irt_wi_end");
		}

		const core::LiteralPtr getWrapData(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			core::FunctionTypePtr type = builder.functionType(toVector<core::TypePtr>(builder.typeVariable("a")), getDataItemType(manager));
			return builder.literal(type, "wrap");
		}

		const core::LiteralPtr getUnwrapData(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			core::FunctionTypePtr type = builder.functionType(toVector(getDataItemType(manager)), builder.typeVariable("a"));
			return builder.literal(type, "unwrap");
		}

		const core::LiteralPtr getWrapLWData(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			core::FunctionTypePtr type = builder.functionType(toVector<core::TypePtr>(builder.typeVariable("a")), getLWDataItemType(manager));
			return builder.literal(type, "wrap");
		}

		const core::LiteralPtr getUnwrapLWData(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			core::FunctionTypePtr type = builder.functionType(toVector(getLWDataItemType(manager)), builder.typeVariable("a"));
			return builder.literal(type, "unwrap");
		}
	}

	Extensions::Extensions(core::NodeManager& manager)
		: runStandalone(getRunStandalone(manager)),
		  contextType(getContextType(manager)),
		  workItemType(getWorkItemType(manager)),
		  workItemImplType(getWorkItemImplType(manager)),
		  workItemImplCtr(getWorkItemImplCtr(manager)),
		  workItemVariantType(getWorkItemVariantType(manager)),
		  workItemVariantCtr(getWorkItemVariantCtr(manager)),
		  workItemVariantImplType(getWorkItemVariantImplType(manager)),
		  registerWorkItemImpl(getRegisterWorkItemImpl(manager)),
		  createWorkItem(getCreateWorkItem(manager)),
		  submitWorkItem(getSubmitWorkItem(manager)),
		  joinWorkItem(getJoinWorkItem(manager)),
		  exitWorkItem(getExitWorkItem(manager)),
//		  dataItemType(getDataItemType(manager)),
//		  wrapData(getWrapData(manager)),
//		  unwrapData(getUnwrapData(manager)),
		  typeID(getTypeIDType(manager)),
		  LWDataItemType(getLWDataItemType(manager)),
		  wrapLWData(getWrapLWData(manager)),
		  unwrapLWData(getUnwrapLWData(manager)) {}





	core::TypePtr DataItem::toDataItemType(const core::TypePtr& type) {
		core::ASTBuilder builder(type->getNodeManager());
		return builder.genericType(DATA_ITEM_TYPE_NAME, toVector(type));
	}

	core::TypePtr DataItem::toLWDataItemType(const core::StructTypePtr& type) {
		core::ASTBuilder builder(type->getNodeManager());
		return builder.genericType(LW_DATA_ITEM_TYPE_NAME, toVector<core::TypePtr>(type));
	}

	core::TypePtr DataItem::extractItemType(const core::TypePtr& type) {
		assert((isDataItemType(type) || isLWDataItemType(type))&& "Only works for data item types!");
		return static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0];
	}

	namespace {

		bool isItemType(const core::TypePtr& type, const string& familyName) {
			// check node type
			if (type->getNodeType() != core::NT_GenericType) {
				return false;
			}

			// check remaining properties
			const core::GenericTypePtr& cur = static_pointer_cast<const core::GenericType>(type);

			bool res = true;
			res = res && cur->getFamilyName() == familyName;
			res = res && cur->getTypeParameter().size() == static_cast<std::size_t>(1);
			res = res && cur->getIntTypeParameter().size() == static_cast<std::size_t>(0);
			return res;
		}

	}

	bool DataItem::isDataItemType(const core::TypePtr& type) {
		return isItemType(type, DATA_ITEM_TYPE_NAME);
	}

	bool DataItem::isLWDataItemType(const core::TypePtr& type) {
		return isItemType(type, LW_DATA_ITEM_TYPE_NAME) &&
				static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0]->getNodeType() == core::NT_StructType;
	}

	core::StructTypePtr DataItem::getLWDataItemStruct(const core::StructTypePtr& structType) {
		// obtain some utilities
		core::NodeManager& manager = structType->getNodeManager();
		core::ASTBuilder builder(manager);
		const Extensions& ext = manager.getLangExtension<Extensions>();

		// create resulting struct - same struct + the type id field
		core::StructType::Entries entries;
		entries.push_back(core::StructType::Entry(builder.identifier("type"), ext.typeID));
		copy(structType->getEntries(), std::back_inserter(entries));
		return core::StructType::get(manager, entries);
	}

	core::StructExprPtr DataItem::getLWDataItemValue(unsigned typeID, const core::StructExprPtr& structValue) {
		// obtain some utilities
		core::NodeManager& manager = structValue->getNodeManager();
		core::ASTBuilder builder(manager);
		const Extensions& ext = manager.getLangExtension<Extensions>();

		core::StructExpr::Members members;
		members.push_back(core::StructExpr::Member(builder.identifier("type"), builder.literal(ext.typeID, toString(typeID))));
		copy(structValue->getMembers(), std::back_inserter(members));
		return core::StructExpr::get(manager, members);
	}

} // end namespace runtime
} // end namespace backend
} // end namespace insieme
