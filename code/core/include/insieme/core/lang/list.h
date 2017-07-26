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

#include "insieme/core/lang/extension.h"

namespace insieme {
namespace core {
namespace lang {


	// --------------------- Extension ----------------------------

	/**
	 * A language extension introducing the primitives required to encode lists of elements.
	 * Lists are represented within the IR using cons and empty tokens.
	 */
	class ListExtension : public core::lang::Extension {
	  private:
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		ListExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:
		/**
		 * The list type.
		 */
		LANG_EXT_TYPE(ListTemplate, "list<'a>");
		
		// -------------------- operators ---------------------------

		/**
		 * literal creating an empty list of a given type.
		 */
		LANG_EXT_LITERAL(ListEmpty, "list_empty", "(type<'a>) -> list<'a>")

		/**
		 * The cons operator of type ('a,list<'a>)->list<'a> extending a given list
		 * by an additional head-element.
		 */
		LANG_EXT_LITERAL(ListCons, "list_cons", "('a, list<'a>) -> list<'a>")
	};

	// --------------------- Utilities ----------------------------

	/**
	 * A utility allowing to test whether the given node is an expression of list type or a list type.
	 *
	 * @param node the node to be tested
	 */
	bool isList(const NodePtr& node);

	/**
	 * A utility allowing to obtain the element type of the given list type.
	 *
	 * @param listType the type representing a list type from which the element type should be extracted
	 * @return a reference to the contained element type
	 */
	const TypePtr getListElementType(const TypePtr& listType);

	/**
	 * Creates a list type maintaining elements of the given element type.
	 *
	 * @param elementType the type to be maintained by the resulting list type.
	 * @return the requested list type maintained by the same manager as the given element type.
	 */
	const TypePtr getListType(const TypePtr& elementType);


	ExpressionPtr buildListEmpty(const TypePtr& type);
	ExpressionPtr buildListCons(const ExpressionPtr& head, const ExpressionPtr& list);
	
	/**
	 * Creates an ir list which consists of the list of expressions passed into the function.
	 * Note: all expressions should be of the same type.
	 *
	 * @param expressions list of expressions to put into returned list.
	 * @return the generated list.
	 */
	ExpressionPtr buildListOfExpressions(const ExpressionList& expressions);

	/**
	 * Converts an encoded list of expressions into a vector of expressions.
	 *
	 * @param list the encoded list of expressions
	 * @return the list of expressions as a vector of expressions
	 */
	ExpressionList parseListOfExpressions(const ExpressionPtr& list);

} // end namespace lang
} // end namespace core
} // end namespace insieme
