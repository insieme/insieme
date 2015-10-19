/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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


#include "insieme/core/lang/enum.h"
#include "insieme/core/ir_builder.h"


namespace insieme {
namespace core {
namespace lang {

	// Enum definition
	EnumDefinition::EnumDefinition(const NodePtr& node) {
		assert_true(node) << "Given node is null!";

		// support expressions as input
		auto type = node.isa<GenericTypePtr>();
		if (auto expr = node.isa<ExpressionPtr>()) type = expr->getType().isa<GenericTypePtr>();

		// if we have a struct expression that is of type (e.g. struct { enum_def, value_type })
		if(isEnumType(node)) {
			type = node.as<core::TagTypePtr>()->getFields()[0]->getType().as<core::GenericTypePtr>();
		}

		// check given node type (performs all needed checks)
		assert_true(isEnumDefinition(type)) << "Given node " << *node << " is not an enum type!";

		// process node type
		std::vector<GenericTypePtr> entries;
		for(unsigned i=1; i<type->getTypeParameter()->size(); ++i) {
			entries.push_back(type->getTypeParameter(i).as<core::GenericTypePtr>());
		}
		*this = EnumDefinition(type->getTypeParameter(0).as<GenericTypePtr>(), entries);
	}

	bool EnumDefinition::isEnumDefinition(const NodePtr& node) {
		//generic type ptr?!
		if(!node.isa<GenericTypePtr>())
			return false;
		GenericTypePtr gt = node.as<core::GenericTypePtr>();
		//name has to be enum_entry
		if(gt->getName()->getValue().find("enum_def") == std::string::npos)
			return false;
		//first field has to be a string lit or empty string
		if(GenericTypePtr gt2 = gt->getTypeParameter(0).isa<GenericTypePtr>()) {
			if(gt2->getTypeParameter()->size())
				return false;
		} else {
			return false;
		}
		//all following fields must be enum entries
		for(unsigned i=1; i<gt->getTypeParameter()->size(); ++i) {
			if(!EnumEntry::isEnumEntry(gt->getTypeParameter(i)))
				return false;
		}
		return true;
	}

	EnumDefinition::operator GenericTypePtr() const {
		NodeManager& nm = enumName.getNodeManager();
		IRBuilder builder(nm);
		TypeList tl;
		tl.push_back(enumName);
		for(auto& ty : domain)
			tl.push_back(static_cast<GenericTypePtr>(ty));
		return GenericType::get(nm, "enum_def", ParentList(), tl);
	}

	//Enum entry
	EnumEntry::EnumEntry(const NodePtr& node) {
		assert_true(node) << "Given node is null!";

		// support expressions as input
		auto type = node.isa<GenericTypePtr>();
		if (auto expr = node.isa<ExpressionPtr>()) type = expr->getType().isa<GenericTypePtr>();

		// check given node type (performs all needed checks)
		assert_true(isEnumEntry(type)) << "Given node " << *node << " is not an enum entry type!";

		// process node type
		*this = EnumEntry(type->getTypeParameter(0).as<GenericTypePtr>(), 
			type->getTypeParameter(1).as<NumericTypePtr>()->getValue().as<core::LiteralPtr>()->getValueAs<int64_t>());
	}

	bool EnumEntry::isEnumEntry(const NodePtr& node) {
		//generic type ptr?!
		if(!node.isa<GenericTypePtr>())
			return false;
		GenericTypePtr gt = node.as<core::GenericTypePtr>();
		//type param size == 2
		if(gt->getTypeParameter()->size() != 2)
			return false;
		//name has to be enum_entry
		if(gt->getName()->getValue().find("enum_entry") == std::string::npos)
			return false;
		//first field has to be a string lit or empty string
		if(GenericTypePtr gt2 = gt->getTypeParameter(0).isa<GenericTypePtr>()) {
			if(gt2->getTypeParameter()->size())
				return false;
		} else {
			return false;
		}
		//second field has to be numerictypeptr
		if(!gt->getTypeParameter(1).isa<NumericTypePtr>())
			return false;
		return true;
	}

	EnumEntry::operator GenericTypePtr() const {
		NodeManager& nm = entryName.getNodeManager();
		IRBuilder builder(nm);
		auto val = builder.numericType(builder.literal(builder.getLangBasic().getUIntInf(), toString(value)));
		return GenericType::get(nm, "enum_entry", toVector(entryName.as<core::TypePtr>(), val));
	}

	bool isEnumType(const core::NodePtr& node) {
		core::TypePtr type;
		if(node.isa<core::TypePtr>())
			type = node.as<core::TypePtr>();
		if(node.isa<core::ExpressionPtr>())
			type = node.as<core::ExpressionPtr>()->getType();
		//type must be a tag type (struct type)
		if(!type.isa<core::TagTypePtr>())
			return false;
		const core::TagTypePtr tt = type.as<core::TagTypePtr>();
		//name of struct has to be enum
		if(tt->getName()->getValue() != "enum")
			return false;
		//with exactly 2 fields (value and enum_type)
		if(tt->getFields().size() != 2)
			return false;
		const core::FieldPtr t1 = tt->getFields()[0];
		const core::FieldPtr t2 = tt->getFields()[1];
		if(t1->getName()->getValue().find("enum_type") == std::string::npos)
			return false;
		if(t2->getName()->getValue().find("value") == std::string::npos)
			return false;
		return true;
	}


	ExpressionPtr getEnumInit(const ExpressionPtr& initVal, const TagTypePtr& enumType) {
		IRBuilder builder(enumType->getNodeManager());
		return builder.structExpr(enumType, { builder.namedValue("value", initVal) });
	}

	TagTypePtr getEnumType(const TypePtr& valueType, const GenericTypePtr& enumDef) {
		IRBuilder builder(enumDef->getNodeManager());
		// build struct {enumty t; val v;}
		std::vector<core::FieldPtr> elements {
			builder.field(builder.stringValue("enum_type"), enumDef),
			builder.field(builder.stringValue("value"), valueType)
		};
		return builder.structType("enum", elements);
	}

	GenericTypePtr getEnumElement(const GenericTypePtr& name, const ExpressionPtr& val) {
		return EnumEntry::create(name, val);
	}

	GenericTypePtr getEnumDef(const GenericTypePtr& name, const std::vector<GenericTypePtr>& entries) {
		return EnumDefinition::create(name, entries);
	}

}
}
}