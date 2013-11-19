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

#include "insieme/transform/transformation.h"

namespace insieme {
namespace transform {
namespace cba_based {


	// -- Plain Functions -------------------------------------------------

	/**
	 * Conducts constant propagation on the given code fragment.
	 *
	 * @param target the targeted code fragment - all nodes within the given sub-tree
	 * 		will be checked for constants and replaced if so
	 * @return the target address pointing to the modified code fragment
	 */
	core::NodeAddress propagateConstants(const core::NodeAddress& target);



	// -- Transformation Wrapper ------------------------------------------


	/**
	 * A transformation conducting constant propagation within the given code fragment
	 * based on the constraint-based-analysis framework.
	 */
	class ConstantPropagation : public Transformation {

	public:

		/**
		 * Creates a new instance of this transformation type
		 */
		ConstantPropagation(const parameter::Value& value);

		/**
		 * Implements the actual transformation.
		 *
		 * @param target the node to be transformed
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const {
			return propagateConstants(target);
		}

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << indent << "ConstantPropagation()";
		}

	};

	/**
	 * Factory for the recursive-function unrolling transformation.
	 */
	TRANSFORMATION_TYPE(
		ConstantPropagation,
		"Applies constant propagation on a targeted sub-tree.",
		parameter::no_parameters()
	);

} // end namespace cba_based
} // end namespace transform
} // end namespace insieme
