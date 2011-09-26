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

#include "insieme/core/transform/node_replacer.h"
#include "insieme/transform/transformation.h"

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


	class NoOp : public AbstractTransformation {
		// nothing to implement
	};


	// TODO: extend and rename this class to a substitution and support consecutive execution operation
	class Replace : public AbstractTransformation {

		/**
		 * A type definition for the type used to represent target/replacement
		 * mappings.
		 */
		typedef utils::map::PointerMap<core::NodePtr, core::NodePtr> Map;

		/**
		 * The list of replacements to be conducted.
		 */
		Map replacements;

	public:

		/**
		 * A simple constructor realizing an replacement operation substituting a
		 * single target node with a given replacement.
		 *
		 * @param target the node to be replaced
		 * @param replacement the replacement to be inserted instead
		 */
		Replace(const core::NodePtr& target, const core::NodePtr& replacement)
			: replacements(utils::map::toPointerMap(target, replacement)) {}

		/**
		 * A constructor accepting a replacement map as an argument. Every element
		 * within the given map will be replaced by its associated replacement. All
		 * substitutions will thereby be applied in parallel, hence, substitutions will
		 * occure within replacements.
		 *
		 * @param replacements the replacement map describing the substitutions to be applied
		 */
		Replace(const Map& replacements) : replacements(replacements) {}

		/**
		 * Conducts the actual transformation.
		 */
		virtual core::NodePtr apply(const core::NodePtr& target) const {
			// use core utility to realize this operation
			return core::transform::replaceAll(target->getNodeManager(), target, replacements);
		}
	};

	// Further primitives:
	// 	- Variable Replacer (with limited scope)

//	class ReplaceVariable : public AbstractTransformation {
//
//		/**
//		 * A type definition for the type used to represent target/replacement
//		 * mappings.
//		 */
//		typedef utils::map::PointerMap<core::VariablePtr, core::ExpressionPtr> Map;
//
//		Map replacements;
//
//	public:
//
//		ReplaceVariable(const core::VariablePtr& var, const core::ExpressionPtr& value)
//			: replacements(utils::map::toPointerMap(var, value)) {}
//
//		ReplaceVariable(const Map& replacements) : replacements(replacements) {}
//
//		virtual core::NodePtr apply(const core::NodePtr& target) const {
//			// use core utility to replace variables
//			return core::transform::replaceVars(target->getNodeManager(), target, replacements);
//		}
//
//	};


} // end namespace transform
} // end namespace insieme
