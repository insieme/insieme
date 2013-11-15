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

#include <vector>
#include "insieme/transform/transformation.h"

/**
 * This header file provides the definition for a set of common function-level transformation
 * including recursive function unrolling and peeling.
 */

namespace insieme {
namespace transform {
namespace functions {


	/**
	 * A transformation unrolling the recursive definition of a function for a given
	 * number of levels. The unrolling includes the recursive unrolling and successive
	 * inlining of the unrolled recursive functions.
	 */
	class RecursiveFunctionUnrolling : public Transformation {

		/**
		 * The number of times the targeted function shell be unrolled.
		 */
		int unrolling;

	public:

		/**
		 * Creates a new instance of this transformation type
		 */
		RecursiveFunctionUnrolling(const parameter::Value& value);

		/**
		 * Implements the actual transformation.
		 *
		 * @param target the node to be transformed
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const;

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << indent << "RecursiveFunctionUnrolling(" << unrolling << ")";
		}

	};

	/**
	 * Factory for the recursive-function unrolling transformation.
	 */
	TRANSFORMATION_TYPE(
		RecursiveFunctionUnrolling,
		"Implementation of the recursive function unrolling transformation.",
		parameter::atom<unsigned>("The unrolling factor to be used.")
	);

	/**
	 * A factory function allowing recursive-function unrolling transformations to be
	 * easily constructed.
	 *
	 * @param unrollingFactor the number of times the resulting transformation will unroll the
	 * 				recursive function it will be applied on
	 * @return the requested transformation instance
	 */
	inline TransformationPtr makeRecFunUnrolling(unsigned unrollingFactor) {
		return RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue<unsigned>(unrollingFactor));
	}


} // end functions
} // end transform
} // end insieme
