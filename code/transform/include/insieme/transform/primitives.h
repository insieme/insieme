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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/transform/transformation.h"
#include "insieme/transform/catalog.h"


namespace insieme {
namespace transform {

	/**
	 * An abstract transformation implementation simplifying the
	 * implementation of standard transformations by providing rudimentary
	 * implementations of the requested virtual functions.
	 */
	class AbstractTransformation : public Transformation {

	public:

		virtual bool checkPreCondition(const core::NodePtr& target) const {
			// transformation may be applied by default
			return true;
		}

		virtual core::NodePtr apply(const core::NodePtr& target) const {
			// no transformation applied by default
			return target;
		}

		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const {
			//  be default, no post-condition check is carried out
			return true;
		}

	};


	/**
	 * The transformation type used as a factory for pipeline connectors.
	 */
	TRANSFORM_TYPE(
			NoOp,
			"A transformation representing the identity, hence not doing anything.",
			parameter::no_parameters
	);


	/**
	 * A transformation representing the identity, hence not doing anything.
	 */
	class NoOp : public AbstractTransformation {

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



} // end namespace transform
} // end namespace insieme
