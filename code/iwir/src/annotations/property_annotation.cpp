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

#include "insieme/iwir/annotations/property_annotation.h"

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/maps.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace annotations {
namespace iwir {

	using namespace insieme::core;

	typedef std::map<std::string, std::string> PropertyMap;

	struct PropertyMapTag : public insieme::core::value_annotation::copy_on_migration  {
		PropertyMap propertyMap;
		PropertyMapTag(const PropertyMap& pm) : propertyMap(pm) { }
		bool operator==(const PropertyMapTag& other) const { return propertyMap == other.propertyMap; }
	};
	
	// ---------------- Support Dump ----------------------

	
	VALUE_ANNOTATION_CONVERTER(PropertyMapTag)

		typedef insieme::core::value_node_annotation<PropertyMapTag>::type annotation_type;

		virtual insieme::core::ExpressionPtr toIR(NodeManager& manager, const insieme::core::NodeAnnotationPtr& annotation) const {
			//assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only constraintMapTag annotations supported!");
			return encoder::toIR(manager, static_pointer_cast<annotation_type>(annotation)->getValue().propertyMap);
		}

		virtual insieme::core::NodeAnnotationPtr toAnnotation(const insieme::core::ExpressionPtr& node) const {
			//assert(encoder::isEncodingOf<std::map<std::string, std::string>>(node.as<insieme::core::ExpressionPtr>()) && "Invalid encoding encountered!");
			return std::make_shared<annotation_type>(PropertyMapTag(encoder::toValue<std::map<std::string, std::string>>(node)));
		}
	};

	bool hasPropertyMapAttached(const insieme::core::NodePtr& node) {
		return node->hasAttachedValue<PropertyMapTag>();
	}

	PropertyMap getPropertyMapAttached(const insieme::core::NodePtr& node) {
		assert_true(hasPropertyMapAttached(node)) << "Does not have a IWIR::PropertyMap attached";
		return node->getAttachedValue<PropertyMapTag>().propertyMap;
	}

	void attachPropertyMap(const insieme::core::NodePtr& node, const PropertyMap& propertyMap) {
		node->attachValue<PropertyMapTag>(propertyMap);
	}

} //iwir 
} //annotations
} //insieme 
