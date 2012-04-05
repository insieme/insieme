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
namespace sequential {

	
	/**
	 * A transformation class allowing the integration of loop collapsing into a
	 * composed transformation.
	 */
	class LoopCollapsing : public Transformation {

	public:

		/**
		 * Creates a new instance of this transformation type.
		 *
		 * @param value only valid if empty (required for Transformation Type infrastructure)
		 */
		LoopCollapsing(const parameter::Value& value);

		/**
		 * Applies this transformation to the given target node.
		 */
		virtual core::NodePtr apply(const core::NodePtr& target) const;
		
		/**
		 * Compares this transformation with the given transformation. It will be considered identical
		 * if the given transformation is of the same type and has the same parameters.
		 */
		virtual bool operator==(const Transformation& other) const {
			return &getType() == &other.getType() && getParameters() == other.getParameters();
		}

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << "LoopCollapsing";
		}

	};

	TRANSFORMATION_TYPE(
		LoopCollapsing,
		"Collapses nested for loops into a single loop over the combined iteration space.",
		parameter::tuple(
				parameter::atom<unsigned>("Number of loop levels to collapse"),
				parameter::atom<bool>("Preserve parallelism")
		)
	);
	
	inline TransformationPtr makeLoopCollapsing(unsigned numLevels, bool preserveParallelism = false) {
		return std::make_shared<LoopCollapsing>( parameter::combineValues(numLevels, preserveParallelism) );
	}

} // end namespace sequential
} // end namespace transform
} // end namespace insieme
