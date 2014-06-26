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

#include "insieme/core/lang/extension.h" 

#include <boost/algorithm/string/predicate.hpp>

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
		EnumExtension(core::NodeManager& manager)
				: core::lang::Extension(manager) {}


	public:

        LANG_EXT_LITERAL(EnumElementAsInt,      "enum.to.int",          "('a) -> int<4>");

        LANG_EXT_LITERAL(EnumElementAsUInt,      "enum.to.uint",          "('a) -> uint<4>");

        LANG_EXT_DERIVED(EnumElementAsBool,      "('a i) -> bool { return lit(\"enum.to.int\":('a)->int<4>)(i) != 0; }");


// STEFAN: 
//
//	for constants
//    builder.genericType(genType(CTANT_NAME), intParam(val))
//
//	For the enum type:
//		builder.genericType("enum", { genType(ENUM_NAE) : Ctants_types  }, emptyIntPAram)
//
//
//		//// TypePtr getEnumCtantType (string name, int val=0);
//		//// typePtr getEnumType (String name, ctantTypeList());

        /**
         * Creates an enum type out a literal. 
         * @param lit The name (co
         * @return the enum type
         */
		TypePtr getEnumType(const string& lit) const {
		    IRBuilder builder(getNodeManager());
		    TypeList typeList;
		    typeList.insert(typeList.end(), builder.genericType(lit));
            return builder.genericType("enum", typeList, insieme::core::IntParamList());
		}

		/**
         * Creates an enum type out of a genericType.
         * @param gt The generic type representing the enumeration
         * @return TypePtr that contains an enum type (e.g. enum<Colors>)
         */
		TypePtr getEnumType(const GenericTypePtr& gt) const {
		    IRBuilder builder(getNodeManager());
		    TypeList typeList;
		    typeList.insert(typeList.end(), gt);
            return builder.genericType("enum", typeList, insieme::core::IntParamList());
		}
        /**
         * Retrieve the name of an enumeration.
         * @param type Enumeration type pointer
         * @return TypePtr that contains the enumeration name literal
         */
		std::string getEnumName(const TypePtr& type) const {
            assert(isEnumType(type) && "this is no enumeration type");
            core::GenericTypePtr gt = static_pointer_cast<const core::GenericType>(type);
            auto name = gt->getTypeParameter()[0].as<GenericTypePtr>()->getName();
			return name->getValue();
		}

        /**
         * Check if a type is an enumeration type
         * @param type TypePtr that should be checked
         * @return boolean value
         */
		bool isEnumType(const TypePtr& type) const {
            core::GenericTypePtr gt;
            if(!type.isa<core::GenericTypePtr>())
                return false;
            gt = static_pointer_cast<const core::GenericType>(type);
            return  (gt->getName()->getValue() == "enum" &&
                     gt->getTypeParameter().size() == 1u &&
                     gt->getIntTypeParameter().empty()
                    );
		}

		bool isEnumConstant (const NodePtr& node) const{
			if (const LiteralPtr lit= node.isa<LiteralPtr>()){
				if (!isEnumType(lit->getType())) return false;
				return  (boost::starts_with(lit->getStringValue(), "__insieme_enum_constant__"));
			}
			return false;
		}

	};
}
}
}
