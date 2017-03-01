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
#pragma once

#include "insieme/transform/catalog.h"
#include "insieme/transform/transformation.h"

namespace insieme {
namespace transform {


	/**
	 * A transformation connector creating multiple versions of the same code.
	 */
	class Versioning : public Transformation {
	  public:
		/**
		 * Creates a new versioning transformation based on the given list of transformations.
		 *
		 * @param transformations the transformations to be used for creating multiple versions of the same code.
		 */
		Versioning(const vector<TransformationPtr>& transformations);

		/**
		 * Creates a new versioning transformation based on the given encoded list of transformations.
		 *
		 * @param params an encoded list of transformations
		 */
		Versioning(const parameter::Value& params);

		/**
		 * Applies the represented list of transformations to the given code fragment. If all work out,
		 * the resulting code fragments will be combined into a code fragment dynamically selecting one
		 * of the produced versions.
		 *
		 * @param target the target to be transformed
		 * @return the transformed code segment
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const;

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
	TRANSFORMATION_TYPE(Versioning, "Creates multiple versions of the same code and let some mechanism during the runtime decide which one to chose.",
	                    parameter::list("List of transformations producing the versions.", parameter::atom<TransformationPtr>()));

	/**
	 * Creates a new versioning transformation based on the given list of transformations.
	 *
	 * @param transformations the transformations to be combined
	 * @return the combined transformation creating one version based on each given transformation
	 */
	inline TransformationPtr versioning(const vector<TransformationPtr>& transformations) {
		if(transformations.size() == 1u) { return transformations[0]; }
		return std::make_shared<Versioning>(transformations);
	}

	/**
	 * Creates a new versioning transformation based on the given list of transformations.
	 *
	 * @param transformations the transformations to be combined
	 * @return the combined transformation creating one version based on each given transformation
	 */
	template <typename... T>
	inline TransformationPtr versioning(const TransformationPtr& first, const T&... rest) {
		return versioning(toVector(first, rest...));
	}


} // end namespace transform
} // end namespace insieme
