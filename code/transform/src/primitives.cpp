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
#include "insieme/transform/primitives.h"

#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace transform {

	NoOp::NoOp(const parameter::Value& value) : Transformation(NoOpType::getInstance(), value) {}

	bool NoOp::operator==(const Transformation& other) const {
		return this == &other || dynamic_cast<const NoOp*>(&other);
	}

	std::ostream& NoOp::printTo(std::ostream& out, const Indent& indent) const {
		return out << indent << "NoOp";
	}

	LambdaTransformation::LambdaTransformation(const TransformationFunction& fun, const string& desc)
	    : Transformation(LambdaTransformationType::getInstance(), parameter::emptyValue), fun(fun), desc(desc){};

	LambdaTransformation::LambdaTransformation(const parameter::Value& value) : Transformation(LambdaTransformationType::getInstance(), parameter::emptyValue) {
		assert_fail() << "Lambda Transformations can not be instantiated using parameters!";
		throw InvalidParametersException("Parameter-based instantiation not supported by lambda transformation!");
	};

	bool LambdaTransformation::operator==(const Transformation& transform) const {
		if(this == &transform) { return true; }

		const LambdaTransformation* other = dynamic_cast<const LambdaTransformation*>(&transform);
		return other && !desc.empty() && desc == other->desc; // description is only important if set
	}

	std::ostream& LambdaTransformation::printTo(std::ostream& out, const Indent& indent) const {
		return out << indent << "LambdaTransformation: " << ((desc.empty()) ? "no-description" : desc);
	}


	TransformationPtr makeDebugPrinter(const string& title, std::ostream& out) {
		return std::make_shared<LambdaTransformation>([=, &out](const core::NodePtr& node) -> core::NodePtr {
			if(title != "") { out << title << "\n"; }
			out << core::printer::PrettyPrinter(node, core::printer::PrettyPrinter::NAME_CONTRACTION | core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS)
			    << "\n";
			return node;
		}, "debug-print");
	}

} // end namespace transform
} // end namespace transform
