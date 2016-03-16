/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/annotations/c/tag.h"

#include "insieme/utils/annotation.h"
#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/encoder.h"

namespace insieme {
namespace annotations {
namespace c {

	using namespace insieme::core;

	/**
	 * The value annotation type to be attached to nodes to store
	 * the actual C tag.
	 */
	struct CTagTag : public core::value_annotation::copy_on_migration {
		string tag;
		CTagTag(const string& tag) : tag(tag) {}
		bool operator==(const CTagTag& other) const {
			return tag == other.tag;
		}
	};

	// ---------------- Support Dump ----------------------

	VALUE_ANNOTATION_CONVERTER(CTagTag)

		typedef core::value_node_annotation<CTagTag>::type annotation_type;

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
			assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only tag annotations supported!");
			return encoder::toIR(manager, static_pointer_cast<annotation_type>(annotation)->getValue().tag);
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
			assert_true(encoder::isEncodingOf<string>(node.as<ExpressionPtr>())) << "Invalid encoding encountered!";
			return std::make_shared<annotation_type>(CTagTag(encoder::toValue<string>(node)));
		}

	VALUE_ANNOTATION_CONVERTER_END

	// ----------------------------------------------------

	bool hasAttachedCTag(const NodePtr& node) {
		return node->hasAttachedValue<CTagTag>();
	}

	const string& getAttachedCTag(const NodePtr& node) {
		assert_true(hasAttachedCTag(node)) << "Does not have a C tag annotation!";
		return node->getAttachedValue<CTagTag>().tag;
	}

	const NodePtr& attachCTag(const NodePtr& node, const string& tag) {
		assert_false(tag.empty()) << "Annotated tag must not be empty!";
		node->attachValue(CTagTag(tag));
		return node;
	}

} // end namespace c
} // end namespace annotations
} // end namespace insieme
