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
