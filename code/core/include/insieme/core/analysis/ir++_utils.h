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

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {
namespace analysis {

	/**
	 * A test which can be applied to the an arbitrary IR structure to determine
	 * whether the represented structure includes object-oriented elements.
	 *
	 * @param node the IR structure to be tested
	 * @return true if IR++ elements are included, false otherwise.
	 */
	bool isIRpp(const NodePtr& node);

	/**
	 * A test verifying whether the given type is a valid object type to
	 * be used as an object type for a ctor / dtor / member function.
	 *
	 * @param type the type to be tested
	 * @return true if type is a valid object type, false otherwise
	 */
	bool isObjectType(const TypePtr& type);

	/**
	 * A shortcut for the isObjectType function above obtaining the result
	 * statically since any tag type is a valid object type.
	 */
	static inline bool isObjectType(const TagTypePtr& type) {
		return true;
	}

	/**
	 * A shortcut for the isObjectType function above obtaining the result
	 * statically since any tag type is a valid object type.
	 */
	static inline bool isObjectType(const TagTypeReferencePtr& type) {
		return true;
	}

	/**
	 * A shortcut for the isObjectType function above obtaining the result
	 * statically since any generic type is a valid object type.
	 */
	static inline bool isObjectType(const GenericTypePtr& type) {
		return true;
	}

	/**
	 * A shortcut for the isObjectType function above obtaining the result
	 * statically since any type variable is a valid object type.
	 */
	static inline bool isObjectType(const TypeVariablePtr& type) {
		return true;
	}

	/**
	 * A test determining whether some type is a reference to a valid object
	 * type structure (hence a potential type for a this pointer).
	 *
	 * @param type the type to be tested
	 * @return true if the given type is a reference to a valid object type.
	 */
	bool isObjectReferenceType(const TypePtr& type);

	/**
	 * A test determining whether some type is a reference to a valid object
	 * type structure (hence a potential type for a this pointer).
	 *
	 * @param type the type to be tested
	 * @return true if the given type is a reference to a valid object type.
	 */
	bool isObjectReferenceType(const GenericTypePtr& type);

	// ---------------------------- Constructors --------------------------------------

	/**
	 * checks if a expression is a constructor call
	 * @param expr, the expression to be checked
	 * @return if is a call to constructor, even intercepted ones
	 */
	bool isConstructorCall(const core::ExpressionPtr& expr);

	// ---------------------------- Defaulted Members --------------------------------------

	/**
	 * Returns a new lambda which has been marked to be a default member
	 */
	LambdaExprPtr markAsDefaultMember(const LambdaExprPtr& lambda);

	/**
	 * Checks whether the given node is a lambda or member function which is marked as a default member
	 */
	bool isaDefaultMember(const NodePtr& node);

} // end namespace analysis
} // end namespace core
} // end namespace insieme
