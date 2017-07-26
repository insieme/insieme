/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/backend/runtime/runtime_entities.h"

namespace insieme {
namespace backend {
namespace runtime {

	const string DATA_ITEM_TYPE_NAME = "irt_di";
	const string LW_DATA_ITEM_TYPE_NAME = "irt_lwdi";

	core::TypePtr DataItem::toDataItemType(const core::TypePtr& type) {
		core::IRBuilder builder(type->getNodeManager());
		return builder.genericType(DATA_ITEM_TYPE_NAME, toVector(type));
	}

	core::TypePtr DataItem::toLWDataItemType(const core::TupleTypePtr& type) {
		core::IRBuilder builder(type->getNodeManager());
		return builder.genericType(LW_DATA_ITEM_TYPE_NAME, toVector<core::TypePtr>(type));
	}

	core::TypePtr DataItem::extractItemType(const core::TypePtr& type) {
		assert_true((isDataItemType(type) || isLWDataItemType(type))) << "Only works for data item types!";
		return static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0];
	}

	namespace {

		bool isItemType(const core::TypePtr& type, const string& familyName) {
			// check node type
			if(type->getNodeType() != core::NT_GenericType) { return false; }

			// check remaining properties
			const core::GenericTypePtr& cur = static_pointer_cast<const core::GenericType>(type);

			bool res = true;
			res = res && cur->getFamilyName() == familyName;
			res = res && cur->getParents()->empty();
			res = res && cur->getTypeParameter().size() == static_cast<std::size_t>(1);
			return res;
		}
	}

	bool DataItem::isDataItemType(const core::TypePtr& type) {
		return isItemType(type, DATA_ITEM_TYPE_NAME);
	}

	bool DataItem::isLWDataItemType(const core::TypePtr& type) {
		return isItemType(type, LW_DATA_ITEM_TYPE_NAME)
			   && static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0]->getNodeType() == core::NT_TupleType;
	}

	core::TupleTypePtr DataItem::getUnfoldedLWDataItemType(const core::TupleTypePtr& tupleType) {
		// obtain some utilities
		core::NodeManager& manager = tupleType->getNodeManager();
		core::IRBuilder builder(manager);
		const RuntimeExtension& ext = manager.getLangExtension<RuntimeExtension>();

		// create resulting tuple - same components + the type id field
		core::TypeList components;
		components.push_back(ext.getTypeID());
		copy(tupleType->getElementTypes(), std::back_inserter(components));
		return core::TupleType::get(manager, components);
	}

	core::TupleExprPtr DataItem::getLWDataItemValue(unsigned typeID, const core::TupleExprPtr& tupleValue) {
		// obtain some utilities
		core::NodeManager& manager = tupleValue->getNodeManager();
		core::IRBuilder builder(manager);
		const RuntimeExtension& ext = manager.getLangExtension<RuntimeExtension>();

		core::ExpressionList values;
		values.push_back(builder.literal(ext.getTypeID(), toString(typeID)));
		copy(tupleValue->getExpressions(), std::back_inserter(values));
		return builder.tupleExpr(values);
	}

} // end namespace runtime
} // end namespace backend
} // end namespace insieme
