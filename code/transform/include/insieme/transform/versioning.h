/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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
	TRANSFORMATION_TYPE(
			Versioning,
			"Creates multiple versions of the same code and let some mechanism during the runtime decide which one to chose.",
			parameter::list("List of transformations producing the versions.", parameter::atom<TransformationPtr>())
	);

	/**
	 * Creates a new versioning transformation based on the given list of transformations.
	 *
	 * @param transformations the transformations to be combined
	 * @return the combined transformation creating one version based on each given transformation
	 */
	inline TransformationPtr versioning(const vector<TransformationPtr>& transformations) {
		if (transformations.size() == 1u) { return transformations[0]; }
		return std::make_shared<Versioning>(transformations);
	}

	/**
	 * Creates a new versioning transformation based on the given list of transformations.
	 *
	 * @param transformations the transformations to be combined
	 * @return the combined transformation creating one version based on each given transformation
	 */
	template<typename ... T>
	inline TransformationPtr versioning(const TransformationPtr& first, const T& ... rest) {
		return versioning(toVector(first, rest ...));
	}


} // end namespace transform
} // end namespace insieme
