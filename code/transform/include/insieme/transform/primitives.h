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

#pragma once

#include "insieme/core/ir_expressions.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/transform/transformation.h"
#include "insieme/transform/catalog.h"


namespace insieme {
namespace transform {


	// --------------- NoOp Transformation -----------------

	/**
	 * A transformation representing the identity, hence not doing anything.
	 */
	class NoOp : public Transformation {
	  public:
		/**
		 * Creates a new instance of this NoOp transformation.
		 *
		 * @param value only valid if empty (required for Transformation Type infrastructure)
		 */
		NoOp(const parameter::Value& value = parameter::emptyValue);

		/**
		 * Applies this transformation to the given target node.
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const {
			return target; // this is just the identity!
		}

		/**
		 * Compares this connector with the given transformation. It will only be the same
		 * if it is a transformation of the same type being instantiated using the same parameters.
		 */
		virtual bool operator==(const Transformation& other) const;

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const;
	};


	/**
	 * The transformation type used as a factory for pipeline connectors.
	 */
	TRANSFORMATION_TYPE(NoOp, "A transformation representing the identity, hence not doing anything.", parameter::tuple());
	/**
	 * makeNoOp() : creates a no transformation
	 */
	inline TransformationPtr makeNoOp() {
		return std::make_shared<NoOp>();
	}


	// --------------- Lambda Transformation -----------------


	/**
	 * A lambda transformation is a simple wrapper allowing to easily create simple transformations
	 * within test cases or when composing transformations to form new transformations. These kind
	 * of transformations are not part of any catalog. They are only a utility for implementing
	 * other transformations.
	 */
	class LambdaTransformation : public Transformation {
		/**
		 * The function type internally stored for conducting the actual transformation.
		 */
		typedef std::function<core::NodePtr(const core::NodePtr&)> TransformationFunction;

		/**
		 * The function conducting the actual transformation.
		 */
		TransformationFunction fun;

		/**
		 * A description for this transformation. If there is no description associated,
		 * an empty string is used.
		 */
		const string desc;

	  public:
		/**
		 * Creates a new instance based on the given transformation function and description.
		 */
		LambdaTransformation(const TransformationFunction& fun, const string& desc);

		LambdaTransformation(const parameter::Value& value);

		/**
		 * Applies this transformation to the given target node.
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const {
			return core::transform::replaceAddress(target->getNodeManager(), target, fun(target));
		}

		/**
		 * Compares this transformation with the given transformation. It is considered equivalent
		 * in case it is the same instance or it has the same non-empty description.
		 */
		virtual bool operator==(const Transformation& other) const;

		/**
		 * Prints a string representation to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const;
	};

	/**
	 * The transformation type used as a factory for pipeline connectors.
	 */
	TRANSFORMATION_TYPE(LambdaTransformation, "A transformation hull for a lambda function realizing the actual transformation.", parameter::tuple());


	/**
	 * A factory method creating a lambda transformation based on the given lambda.
	 *
	 * @param lambda the function conducting the actual transformation
	 * @return a transformation which will invoke the given lambda when being applied
	 */
	template <typename Lambda>
	TransformationPtr makeLambdaTransformation(const Lambda& lambda) {
		return std::make_shared<LambdaTransformation>(lambda, "");
	}

	/**
	 * A factory method creating a lambda transformation based on the given lambda.
	 *
	 * @param desc a description for the resulting transformation
	 * @param lambda the function conducting the actual transformation
	 * @return a transformation which will invoke the given lambda when being applied
	 */
	template <typename Lambda>
	TransformationPtr makeLambdaTransformation(const string& desc, const Lambda& lambda) {
		return std::make_shared<LambdaTransformation>(lambda, desc);
	}


	// --- For debugging purposes ---

	/**
	 * A factory for a debugging-transformation which can be added inside a composed
	 * transformation to print the current transformed code version.
	 */
	TransformationPtr makeDebugPrinter(const string& title = "", std::ostream& out = std::cout);

} // end namespace transform
} // end namespace insieme
