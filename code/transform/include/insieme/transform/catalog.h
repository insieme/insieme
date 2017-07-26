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

#include <map>
#include <memory>
#include <string>

#include "insieme/transform/parameter.h"
#include "insieme/transform/transformation.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace transform {

	/**
	 * Within this header file the transformation catalog infrastructure is defined.
	 * The corresponding factory mechanisms are as well offered.
	 */

	using std::string;

	// forward declaration
	class Catalog;

	/**
	 * Obtains a catalog containing a comprehensive list of transformations.
	 */
	Catalog getStandardCatalog();

	/**
	 * The catalog provides a list of transformations annotated with additional information enabling users / code to
	 * instantiated them. The catalog should be the main interface for an optimizer when interacting with the
	 * transformation environment of the Insieme Compiler core. It should shield the optimizer from the underlying
	 * details.
	 *
	 * The Transformation catalog is an aggregation of Transformation-Meta-Information and the main
	 * utility to be used by the optimizer when selecting, instantiating and composing transformations
	 * to be applied on code within the Insieme Compiler.
	 */
	class Catalog {
		/**
		 * The container for the internally stored transformations.
		 */
		std::map<string, TransformationTypePtr> catalog;

	  public:
		/**
		 * Adds a new transformation type to this catalog.
		 *
		 * @param newType the new transformation type to be added
		 */
		void add(const TransformationType& newType) {
			assert(catalog.find(newType.getName()) == catalog.end() && "Discoverd name collision!");
			catalog.insert(std::make_pair(newType.getName(), &newType));
		}

		/**
		 * Obtains the type registered to the given name.
		 *
		 * @param name the name of the transformation looking for
		 * @return the requested type or a null pointer if there is no such type
		 */
		TransformationTypePtr getTransformationType(const string& name) const {
			auto pos = catalog.find(name);
			if(pos != catalog.end()) { return pos->second; }
			return NULL;
		}

		/**
		 * Creates a new transformation. The given name is used to determine the type of the requested
		 * transformation and the given values are used to parameterize the result.
		 *
		 * @param name the name of the transformation for which a new instance is requested
		 * @param value the values to be used for setting up transformation parameters
		 */
		TransformationPtr createTransformation(const string& name, const parameter::Value& value = parameter::emptyValue) const {
			TransformationTypePtr type = getTransformationType(name);
			assert_true(type) << "Unknown transformation type requested!";
			return type->createTransformation(value);
		}

		/**
		 * Obtains a reference to the internally maintained transformation type register.
		 */
		const std::map<string, TransformationTypePtr>& getRegister() const {
			return catalog;
		}

		/**
		 * Obtains a list of all names of the internally maintained transformations.
		 */
		vector<string> getAllTransformationNames() const {
			vector<string> res;
			projectToFirst(catalog, res);
			return res;
		}

		/**
		 * Obtains a list of all internally maintained transformations.
		 */
		vector<TransformationTypePtr> getAllTransformations() const {
			return projectToSecond(catalog);
		}
	};


} // end namespace transform
} // end namespace insieme
