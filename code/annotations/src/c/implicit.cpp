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

#include "insieme/annotations/c/implicit.h"

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/encoder.h"

namespace insieme {
namespace annotations {
namespace c {

	using namespace insieme::core;

	/**
	 * The value annotation type to be attached to nodes to mark it as implicit.
	 */
	struct ImplicitTag : public core::value_annotation::copy_on_migration {
		bool operator==(const ImplicitTag& other) const {
			return true;
		}
	};

	// ---------------- Support Dump ----------------------

	VALUE_ANNOTATION_CONVERTER(ImplicitTag)

		typedef core::value_node_annotation<ImplicitTag>::type annotation_type;

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
			assert_true(dynamic_pointer_cast<annotation_type>(annotation)) << "Only \"implicit\" annotations supported!";
			return encoder::toIR(manager, string("implicitTag"));
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
			assert_true(encoder::isEncodingOf<string>(node.as<ExpressionPtr>())) << "Invalid encoding encountered!";
			return std::make_shared<annotation_type>(ImplicitTag());
		}

	VALUE_ANNOTATION_CONVERTER_END

// ----------------------------------------------------


bool isMarkedAsImplicit(const insieme::core::NodePtr& call) {
	return call->hasAttachedValue<ImplicitTag>();
}

void markAsImplicit(const insieme::core::NodePtr& call, bool value) {
	if(value) {
		call->attachValue(ImplicitTag());
	} else {
		call->detachValue<ImplicitTag>();
	}
}

} // end namespace c
} // end namespace annotations
} // end namespace insieme
