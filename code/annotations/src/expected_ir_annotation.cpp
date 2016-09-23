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

#include "insieme/annotations/expected_ir_annotation.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/encoder/encoder.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/dump/annotations.h"

#include <string>


namespace insieme {
namespace annotations {

	const string ExpectedIRAnnotation::NAME = "ExpectedIRAnnotation";
	const utils::StringKey<ExpectedIRAnnotation> ExpectedIRAnnotation::KEY("ExpectedIRAnnotation");

	string ExpectedIRAnnotation::getExpected() const {
		return expected;
	}

	void ExpectedIRAnnotation::attach(const core::NodePtr& node, string expected) {
		node->addAnnotation(std::make_shared<ExpectedIRAnnotation>(expected));
	}

	bool ExpectedIRAnnotation::hasAttachedValue(const core::NodePtr& node) {
		return node->hasAnnotation(ExpectedIRAnnotation::KEY);
	}

	string ExpectedIRAnnotation::getValue(const core::NodePtr& node) {
		assert_true(hasAttachedValue(node)) << "Expected IR string has to be attached!";
		return node->getAnnotation(ExpectedIRAnnotation::KEY)->getExpected();
	}

	namespace {

		ANNOTATION_CONVERTER(ExpectedIRAnnotation)

			core::ExpressionPtr toIR(core::NodeManager& manager, const core::NodeAnnotationPtr& annotation) const {
				assert(dynamic_pointer_cast<ExpectedIRAnnotation>(annotation) && "Only supports the conversion of ExpectedIRAnnotation Annotations!");
				return core::encoder::toIR<string>(manager, static_pointer_cast<ExpectedIRAnnotation>(annotation)->getExpected());
			}

			core::NodeAnnotationPtr toAnnotation(const core::ExpressionPtr& node) const {
				assert(core::encoder::isEncodingOf<size_t>(node) && "Invalid Encoding!");
				return std::make_shared<ExpectedIRAnnotation>(core::encoder::toValue<string>(node));
			}

		ANNOTATION_CONVERTER_END
	}

} // namespace annotations
} // namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::annotations::ExpectedIRAnnotation& lAnnot) {
		out << "ExpectedIRAnnotation:\n";
		out << "Expected IR: " << lAnnot.getExpected() << std::endl;
		return out;
	}

} // end namespace std
