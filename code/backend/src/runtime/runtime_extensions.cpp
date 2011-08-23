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

#include "insieme/annotations/c/naming.h"

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/backend/runtime/runtime_entities.h"

namespace insieme {
namespace backend {
namespace runtime {

	const string DATA_ITEM_TYPE_NAME = "irt_di";
	const string LW_DATA_ITEM_TYPE_NAME = "irt_lwdi";

	namespace {

		template<typename T>
		const T& attachName(const T& node, const string& name) {
			const core::NodePtr& cur = node;
			cur->addAnnotation<annotations::c::CNameAnnotation>(name);
			return node;
		}
	}

	namespace lang = core::lang;

	Extensions::Extensions(core::NodeManager& manager)
		:
		  runStandalone(lang::getLiteral(manager, 			"(irt_wi_implementation_id, irt_lwdi<'a>)->unit", 	"runStandalone")),

		  contextType(lang::getType(manager, 				"irt_context")),
		  workItemType(lang::getType(manager, 				"irt_wi")),
		  workItemImplType(lang::getType(manager, 			"irt_wi_implementation_id")),
		  workItemImplCtr(lang::getLiteral(manager, 		"(list<irt_wi_variant>)->irt_wi_implementation_id", "WorkItemImpl")),
		  workItemVariantType(lang::getType(manager, 		"irt_wi_variant")),
		  workItemVariantCtr(lang::getLiteral(manager, 		"((ref<irt_wi>)->unit)->irt_wi_variant",			"WorkItemVariant")),
		  workItemVariantImplType(lang::getType(manager, 	"(ref<irt_wi>)->unit")),

		  registerWorkItemImpl(lang::getLiteral(manager, 	"(irt_wi_implementation_id)->unit", 				"registerImpl")),
		  createWorkItem(lang::getLiteral(manager, 			"(uint<#a>,uint<#a>,uint<#a>,irt_wi_implementation_id,irt_di<'a>)->ref<irt_wi>", "createWI")),
		  submitWorkItem(lang::getLiteral(manager, 			"(ref<irt_wi>)->unit", 								"submitWI")),
		  joinWorkItem(lang::getLiteral(manager, 			"(ref<irt_wi>)->unit", 								"joinWI")),
		  exitWorkItem(lang::getLiteral(manager, 			"(ref<irt_wi>)->unit", 								"irt_wi_end")),

		  typeID(lang::getType(manager, 					"irt_type_id")),
		  LWDataItemType(lang::getType(manager, 			"irt_lwdi<'a>")),
		  wrapLWData(lang::getLiteral(manager, 				"('a)->irt_lwdi<'a>", 								"wrap")),
		  unwrapLWData(lang::getLiteral(manager, 			"(irt_lwdi<'a>)->'a", 								"unwrap")),

		  jobType(lang::getType(manager, 					"irt_parallel_job")),

		  createJob(lang::getLiteral(manager, 				"(uint<8>,uint<8>,uint<8>,irt_wi_implementation_id,irt_lwdi<'a>)->irt_parallel_job" , "createJob")),
		  parallel(lang::getLiteral(manager, 				"(irt_parallel_job)->ref<irt_wi>", 					"irt_parallel")),

		  pfor(lang::getLiteral(manager, 					"(threadgroup,int<4>,int<4>,int<4>,irt_wi_implementation_id,irt_lwdi<'a>)->ref<irt_wi>", "irt_pfor")),
		  merge(lang::getLiteral(manager, 					"(ref<irt_wi>)->unit", 								"irt_merge")),

		  getWorkItemArgument(lang::getLiteral(manager, 	"(ref<irt_wi>, uint<#a>, type<irt_lwdi<'p>>, type<'a>)->'a", "getArg")),
		  workItemRange(attachName(lang::getType(manager, 	"struct<begin:int<4>,end:int<4>,step:int<4>>"), "irt_work_item_range")),
		  getWorkItemRange(lang::getLiteral(manager, 		"(ref<irt_wi>)->struct<begin:int<4>,end:int<4>,step:int<4>>", "getRange")) {}





	core::TypePtr DataItem::toDataItemType(const core::TypePtr& type) {
		core::ASTBuilder builder(type->getNodeManager());
		return builder.genericType(DATA_ITEM_TYPE_NAME, toVector(type));
	}

	core::TypePtr DataItem::toLWDataItemType(const core::TupleTypePtr& type) {
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
				static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0]->getNodeType() == core::NT_TupleType;
	}

	core::TupleTypePtr DataItem::getUnfoldedLWDataItemType(const core::TupleTypePtr& tupleType) {
		// obtain some utilities
		core::NodeManager& manager = tupleType->getNodeManager();
		core::ASTBuilder builder(manager);
		const Extensions& ext = manager.getLangExtension<Extensions>();

		// create resulting tuple - same components + the type id field
		core::TypeList components;
		components.push_back(ext.typeID);
		copy(tupleType->getElementTypes(), std::back_inserter(components));
		return core::TupleType::get(manager, components);
	}

	core::TupleExprPtr DataItem::getLWDataItemValue(unsigned typeID, const core::TupleExprPtr& tupleValue) {
		// obtain some utilities
		core::NodeManager& manager = tupleValue->getNodeManager();
		core::ASTBuilder builder(manager);
		const Extensions& ext = manager.getLangExtension<Extensions>();

		core::ExpressionList values;
		values.push_back(builder.literal(ext.typeID, toString(typeID)));
		copy(tupleValue->getExpressions(), std::back_inserter(values));
		return core::TupleExpr::get(manager, values);
	}

} // end namespace runtime
} // end namespace backend
} // end namespace insieme
