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

#include "insieme/annotations/std_init_list.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"

namespace insieme {
namespace annotations {

	struct StdInitListAnnotation : public core::value_annotation::copy_on_migration {
		const core::TypePtr elementType;

		StdInitListAnnotation(const core::TypePtr elementType) : elementType(elementType) { }

		bool operator==(const StdInitListAnnotation& other) const {
			return elementType == other.elementType;
		}
	};

	// ---------------- Support Dump ----------------------

	VALUE_ANNOTATION_CONVERTER(StdInitListAnnotation)

		typedef core::value_node_annotation<StdInitListAnnotation>::type annotation_type;

		virtual core::ExpressionPtr toIR(core::NodeManager& manager, const core::NodeAnnotationPtr& annotation) const {
			assert_true(dynamic_pointer_cast<annotation_type>(annotation)) << "Only StdInitListAnnotation annotations supported!";
			return core::IRBuilder(manager).getTypeLiteral(static_pointer_cast<annotation_type>(annotation)->getValue().elementType);
		}

		virtual core::NodeAnnotationPtr toAnnotation(const core::ExpressionPtr& node) const {
			return std::make_shared<annotation_type>(StdInitListAnnotation(core::analysis::getRepresentedType(node)));
		}

	VALUE_ANNOTATION_CONVERTER_END

// ----------------------------------------------------


	void markStdInitList(const core::NodePtr& node, const core::TypePtr& elementType) {
		node->attachValue(StdInitListAnnotation(elementType));
	}

	bool isStdInitList(const core::NodePtr& node) {
		return node->hasAttachedValue<StdInitListAnnotation>();
	}

	core::TypePtr getStdInitListElementType(const core::NodePtr& node) {
		assert_true(isStdInitList(node)) << "No StdInitListAnnotation attached to the given node " << *node;
		return node->getAttachedValue<StdInitListAnnotation>().elementType;
	}

} // end namespace annotations
} // end namespace insieme
