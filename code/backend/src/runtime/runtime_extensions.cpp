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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/backend/runtime/runtime_entities.h"

namespace insieme {
namespace backend {
namespace runtime {

	const string DATA_ITEM_TYPE_NAME = "irt_di";
	const string LW_DATA_ITEM_TYPE_NAME = "irt_lwdi";

	Extensions::Extensions(core::NodeManager& manager)
		: core::lang::Extension(manager),

		// members are initialized using the content of the macro file
		#define TYPE(_name, _type) 				_name(core::lang::getType(manager, _type)),
		#define NTYPE(_name, _type, _cname)    	_name(core::annotations::attachNameWithReturn(core::lang::getType(manager, _type), _cname)),
		#define LITERAL(_name, _value, _type) 	_name(core::lang::getLiteral(manager, _type, _value)),
		#include "insieme/backend/runtime/ir_extensions.def"
		#undef TYPE
		#undef NTYPE
		#undef LITERAL

		dummy(0) // required since sequence must not end with a ,
	{}


	core::TypePtr DataItem::toDataItemType(const core::TypePtr& type) {
		core::IRBuilder builder(type->getNodeManager());
		return builder.genericType(DATA_ITEM_TYPE_NAME, toVector(type));
	}

	core::TypePtr DataItem::toLWDataItemType(const core::TupleTypePtr& type) {
		core::IRBuilder builder(type->getNodeManager());
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
		core::IRBuilder builder(manager);
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
		core::IRBuilder builder(manager);
		const Extensions& ext = manager.getLangExtension<Extensions>();

		core::ExpressionList values;
		values.push_back(builder.literal(ext.typeID, toString(typeID)));
		copy(tupleValue->getExpressions(), std::back_inserter(values));
		return builder.tupleExpr(values);
	}

} // end namespace runtime
} // end namespace backend
} // end namespace insieme
