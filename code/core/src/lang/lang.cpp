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

#include "insieme/core/annotations/naming.h"

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/encoder.h"

#include "insieme/utils/unused.h"

namespace insieme {
namespace core {
namespace lang {

	/**
	 * The value annotation type to be attached to all derived constructs.
	 */
	struct DerivedTag {
		string name;
		DerivedTag(const string& name) : name(name) {}
		bool operator==(const DerivedTag& other) const {
			return name == other.name;
		}
	};

	// ---------------- Support Dump ----------------------

	VALUE_ANNOTATION_CONVERTER(DerivedTag)

	typedef core::value_node_annotation<DerivedTag>::type annotation_type;

	virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
		assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only derived-tag annotations supported!");
		return encoder::toIR(manager, static_pointer_cast<annotation_type>(annotation)->getValue().name);
	}

	virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
		assert_true(encoder::isEncodingOf<string>(node.as<ExpressionPtr>())) << "Invalid encoding encountered!";
		return std::make_shared<annotation_type>(DerivedTag(encoder::toValue<string>(node)));
	}
};

// ----------------------------------------------------

bool isDerived(const NodePtr& node) {
	return node->hasAttachedValue<DerivedTag>();
}

const string& getConstructName(const NodePtr& node) {
	assert_true(isDerived(node)) << "Node not marked as being a derived construct!";
	return node->getAttachedValue<DerivedTag>().name;
}

NodePtr markAsDerived(const NodePtr& node, const string& name) {
	node->attachValue(DerivedTag(name));
	return node;
}

// ----------------------------------------------------

// the type of marker utilized to mark built-in literals and derived functions
struct BuiltInTag {};

namespace {

	/**
	 * A Converter ensuring that built-in-tags are preserved within binary dumps.
	 */
	struct BuiltInTagConverter : public dump::AnnotationConverter {

		BuiltInTagConverter() : AnnotationConverter("BuiltInTag") {}

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const override {
			// the actual node here is not really important ... it just must not be null
			return manager.getLangBasic().getTrue();
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const override {
			return std::make_shared<typename core::value_node_annotation<BuiltInTag>::type>(BuiltInTag());
		}

	};

	// register the converter into the central annotation converter registry
	__unused bool reg = dump::AnnotationConverterRegister::getDefault().registerConverter<BuiltInTagConverter,BuiltInTag>();

}


void markAsBuiltIn(const NodePtr& node) {
	node->attachValue<BuiltInTag>();
}

bool isBuiltIn(const core::NodePtr& node) {
	return node->hasAttachedValue<BuiltInTag>();
}

// ---------------- Support Dump ----------------------

VALUE_ANNOTATION_CONVERTER(BuiltInTag)

typedef core::value_node_annotation<BuiltInTag>::type annotation_type;

virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
	assert_true(dynamic_pointer_cast<annotation_type>(annotation)) << "Only BuiltInTag annotations supported!";
	return encoder::toIR(manager, string("builtin_tag"));
}

virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
	assert_true(encoder::isEncodingOf<string>(node.as<ExpressionPtr>())) << "Invalid encoding of BuiltInTag encountered!";
	return std::make_shared<annotation_type>(BuiltInTag());
}
};


} // end namespace lang
} // end namespace core
} // end namespace insieme
