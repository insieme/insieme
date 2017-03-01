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
 *
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

	VALUE_ANNOTATION_CONVERTER_END

	// ----------------------------------------------------

	bool isDerived(const NodePtr& node) {
		return node->hasAttachedValue<DerivedTag>();
	}

	const string& getConstructName(const NodePtr& node) {
		// for derived constructs
		if(isDerived(node)) { return node->getAttachedValue<DerivedTag>().name; }
		// also: support built-in constructs
		assert_true(isBuiltIn(node) && node.isa<LiteralPtr>()) << "Error for " << *node << " of type " << node->getNodeType()
		                                                       << "\nNode is neither a derived nor built-in literal!";
		return node.as<LiteralPtr>()->getValue()->getValue();
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
		__insieme_unused bool reg = dump::AnnotationConverterRegister::getDefault().registerConverter<BuiltInTagConverter,BuiltInTag>();

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
	
	VALUE_ANNOTATION_CONVERTER_END

} // end namespace lang
} // end namespace core
} // end namespace insieme
