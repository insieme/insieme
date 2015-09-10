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
