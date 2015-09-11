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

#pragma once

#include "insieme/core/lang/extension.h"

#include <boost/algorithm/string/predicate.hpp>

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace lang {

	/**
	 */
	class EnumExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		EnumExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}


	  public:
		LANG_EXT_LITERAL(EnumElementAsInt, "enum_to_int", "('a) -> int<4>");

		LANG_EXT_LITERAL(EnumElementAsUInt, "enum_to_uint", "('a) -> uint<4>");

		LANG_EXT_DERIVED(EnumElementAsBool, "lambda ('a i) -> bool { return lit(\"enum_to_int\":('a)->int<4>)(i) != 0; }");

		LANG_EXT_LITERAL(IntAsEnum, "int_to_enum", "(int<4>, type<'a> ) -> 'a");
		LANG_EXT_LITERAL(UIntAsEnum, "uint_to_enum", "(uint<4>, type<'a> ) -> 'a");


		/**
		 * Creates an enum type out of a literal.
		 * @param lit The name of the enum
		 * @param elements vector of elements
		 * @return the enum type
		 */
		TypePtr getEnumType(const string& lit, const std::vector<TypePtr> elements = std::vector<TypePtr>()) const {
			IRBuilder builder(getNodeManager());
			TypeList typeList;
			typeList.insert(typeList.end(), builder.genericType(lit));
			for(TypePtr gt : elements) {
				assert(gt.isa<GenericTypePtr>());
				typeList.insert(typeList.end(), gt);
			}
			return builder.genericType("__insieme_enum", typeList);
		}
		/**
		 * Check if a type is an enumeration type
		 * @param type TypePtr that should be checked
		 * @return boolean value
		 */
		bool isEnumType(const TypePtr& type) const {
			core::GenericTypePtr gt;
			if(!type.isa<core::GenericTypePtr>()) { return false; }
			gt = static_pointer_cast<const core::GenericType>(type);
			return (gt->getName()->getValue() == "__insieme_enum");
		}

		bool isEnumConstant(const NodePtr& node) const {
			if(const LiteralPtr lit = node.isa<LiteralPtr>()) {
				if(!isEnumType(lit->getType())) { return false; }
				return (boost::starts_with(lit->getStringValue(), "__insieme_enum_constant__"));
			}
			return false;
		}

		/**
		 * Retrieve the name of an enumeration.
		 * @param type Enumeration type pointer
		 * @return TypePtr that contains the enumeration name literal
		 */
		std::string getEnumName(const TypePtr& type) const {
			assert_true(isEnumType(type)) << "this is no enumeration type";
			core::GenericTypePtr gt = static_pointer_cast<const core::GenericType>(type);
			return gt->getFamilyName();
		}

		TypePtr getEnumConstantType(const string& lit) const {
			IRBuilder builder(getNodeManager());
			TypeList typeList;
			return builder.genericType(lit, typeList);
		}

		TypePtr getEnumConstantType(const string& lit, const string& val) const {
			IRBuilder builder(getNodeManager());
			TypeList typeList;
			typeList.push_back(builder.genericType(lit));
			typeList.push_back(builder.genericType(val));
			return builder.genericType("__insieme_enum_constant__", typeList);
		}
	};
} // lang
} // core
} // insieme
