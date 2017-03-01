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
/*
 * loop_annotations.cpp
 *
 *  Created on: Dec 13, 2012
 *      Author: klaus
 */

#include "insieme/annotations/loop_annotations.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/encoder/encoder.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/dump/annotations.h"


namespace insieme {
namespace annotations {

	const string LoopAnnotation::NAME = "LoopAnnotation";
	const utils::StringKey<LoopAnnotation> LoopAnnotation::KEY("Loop");

	size_t LoopAnnotation::getIterations() const {
		return iterations;
	}


	void LoopAnnotation::attach(const core::NodePtr& node, size_t iterations) {
		node->addAnnotation(std::make_shared<LoopAnnotation>(iterations));
	}

	bool LoopAnnotation::hasAttachedValue(const core::NodePtr& node) {
		return node->hasAnnotation(LoopAnnotation::KEY);
	}

	size_t LoopAnnotation::getValue(const core::NodePtr& node) {
		assert_true(hasAttachedValue(node)) << "Loop Annotation Counter has to be attached!";
		return node->getAnnotation(LoopAnnotation::KEY)->getIterations();
	}

	namespace {

		ANNOTATION_CONVERTER(LoopAnnotation)

			core::ExpressionPtr toIR(core::NodeManager& manager, const core::NodeAnnotationPtr& annotation) const {
				assert(dynamic_pointer_cast<LoopAnnotation>(annotation) && "Only supports the conversion of Loop Annotations!");
				return core::encoder::toIR<size_t>(manager, static_pointer_cast<LoopAnnotation>(annotation)->getIterations());
			};

			core::NodeAnnotationPtr toAnnotation(const core::ExpressionPtr& node) const {
				assert(core::encoder::isEncodingOf<size_t>(node) && "Invalid Encoding!");
				return std::make_shared<LoopAnnotation>(core::encoder::toValue<size_t>(node));
			};

		ANNOTATION_CONVERTER_END

	}

} // namespace annotations
} // namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::LoopAnnotation& lAnnot) {
		out << "LoopAnnotation:\n";
		out << "Iterations: " << lAnnot.getIterations() << std::endl;
		return out;
	}

} // end namespace std
