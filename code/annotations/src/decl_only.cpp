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

#include "insieme/annotations/c/decl_only.h"

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/encoder.h"


namespace insieme {
namespace annotations {
namespace c {

using namespace insieme::core;

// ---------------- Support Dump ----------------------

VALUE_ANNOTATION_CONVERTER(DeclOnlyTag)

typedef core::value_node_annotation<DeclOnlyTag>::type annotation_type;

virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
	assert_true(dynamic_pointer_cast<annotation_type>(annotation)) << "Only DeclOnly annotations supported!";
	return encoder::toIR(manager, (unsigned)(static_pointer_cast<annotation_type>(annotation)->getValue().kind));
}

virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
	assert_true(encoder::isEncodingOf<unsigned>(node.as<ExpressionPtr>())) << "Invalid encoding encountered!";
	unsigned value = encoder::toValue<unsigned>(node);
	
	DeclOnlyTag::Kind kind = DeclOnlyTag::Kind::Struct;
	switch(value) {
	case 0:
		kind = DeclOnlyTag::Struct;
		break;
	case 1:
		kind = DeclOnlyTag::Class;
		break;
	case 2:
		kind = DeclOnlyTag::Enum;
		break;
	case 3:
		kind = DeclOnlyTag::Union;
		break;
	default:
		assert_fail() << "what DeclOnlyTag::Kind is it then?";
		break;
	}
	
	return std::make_shared<annotation_type>(DeclOnlyTag(kind));
}
};

// ----------------------------------------------------


bool isDeclOnly(const insieme::core::GenericTypePtr& type) {
	return type->hasAttachedValue<DeclOnlyTag>();
}
void markDeclOnlyStruct(const insieme::core::GenericTypePtr& type) {
	type->attachValue(DeclOnlyTag(DeclOnlyTag::Kind::Struct));
}
void markDeclOnlyClass(const insieme::core::GenericTypePtr& type) {
	type->attachValue(DeclOnlyTag(DeclOnlyTag::Kind::Class));
}
void markDeclOnlyEnum(const insieme::core::GenericTypePtr& type) {
	type->attachValue(DeclOnlyTag(DeclOnlyTag::Kind::Enum));
}
void markDeclOnlyUnion(const insieme::core::GenericTypePtr& type) {
	type->attachValue(DeclOnlyTag(DeclOnlyTag::Kind::Union));
}
void unmarkDeclOnly(const insieme::core::GenericTypePtr& type) {
	type->detachValue<DeclOnlyTag>();
}
DeclOnlyTag::Kind getDeclOnlyKind(const insieme::core::GenericTypePtr& type) {
	return type->getAttachedValue<DeclOnlyTag>().kind;
}


} // end namespace c
} // end namespace annotations
} // end namespace insieme
