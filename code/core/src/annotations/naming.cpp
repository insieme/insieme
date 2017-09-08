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

#include "insieme/core/annotations/naming.h"

#include <boost/algorithm/string.hpp>

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/encoder.h"

namespace insieme {
namespace core {
namespace annotations {

	/**
	 * The value annotation type to be attached to nodes to store
	 * the actual name.
	 */
	struct NameTag : public core::value_annotation::copy_on_migration {
		string name;
		NameTag(const string& name) : name(name) {}
		bool operator==(const NameTag& other) const {
			return name == other.name;
		}
	};

	// ---------------- Support Dump ----------------------

	VALUE_ANNOTATION_CONVERTER(NameTag)

		typedef core::value_node_annotation<NameTag>::type annotation_type;

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
			assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only dummy annotations supported!");
			return encoder::toIR(manager, static_pointer_cast<annotation_type>(annotation)->getValue().name);
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
			assert_true(encoder::isEncodingOf<string>(node.as<ExpressionPtr>())) << "Invalid encoding encountered!";
			return std::make_shared<annotation_type>(NameTag(encoder::toValue<string>(node)));
		}

	VALUE_ANNOTATION_CONVERTER_END

// ----------------------------------------------------

bool hasAttachedName(const NodePtr& node) {
	return node->hasAttachedValue<NameTag>();
}

const string& getAttachedName(const NodePtr& node) {
	assert_true(hasAttachedName(node)) << "Does not have a name annotation!";
	return node->getAttachedValue<NameTag>().name;
}

void attachName(const NodePtr& node, const string& name) {
	assert_false(name.empty()) << "Annotated name must not be empty!";
	//assert_false(name.find(" ") != std::string::npos && !boost::starts_with(name, "operator ")) << "Annotated name \"" << name << "\" must not contain spaces!";
	node->attachValue(NameTag(name));
}

} // end namespace annotations
} // end namespace core
} // end namespace insieme
