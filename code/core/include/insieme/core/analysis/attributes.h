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

#include <vector>

#include "insieme/core/ir_expressions.h"
#include "insieme/core/lang/extension.h"

#include "insieme/utils/set_utils.h"

namespace insieme {
namespace core {
namespace analysis {

	using std::vector;

	/**
	 * A language extension for specifying attributes being used
	 * to wrap attributes.
	 */
	class AttributeExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		AttributeExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:
		// -- literals attaching attributes --

		// literals managing the capturing module
		LANG_EXT_LITERAL(Attr, "attr", "('a, list<attribute>)->'a");

		// -- literals specifying attributes --

		// The type of all attributes
		LANG_EXT_TYPE(AttributeType, "attribute");

		// an attribute
		LANG_EXT_LITERAL(Unordered, "unordered", "attribute");
	};

	// attributes are just expressions in the IR
	typedef ExpressionPtr AttributePtr;

	// the type used to represent sets of attributes
	typedef utils::set::PointerSet<AttributePtr> AttributeSet;

	/**
	 * Checks whether the given expression has the given attribute attached to it.
	 *
	 * @param expr the expression to be tested
	 * @param attribute the attributed to be searched for
	 */
	bool hasAttribute(const ExpressionPtr& expr, const AttributePtr& attribute);

	/**
	 * Attaches the given attribute to the given expression.
	 *
	 * @param expr the expression to be extended
	 * @param attribute the attribute to be attached
	 * @return the modified version of the given expression
	 */
	ExpressionPtr addAttribute(const ExpressionPtr& expr, const AttributePtr& attribute);

	/**
	 * Removes an attribute from the given expression if present.
	 *
	 * @param expr the expression to be extended
	 * @param attribute the attribute to be attached
	 * @return the modified version of the given expression
	 */
	ExpressionPtr remAttribute(const ExpressionPtr& expr, const AttributePtr& attribute);

	/**
	 * Strips off all the attributes of the given expression and obtains the underlying
	 * un-modified expression.
	 *
	 * @param expr the expression to be processed
	 * @param the underlying, non-attributed expression
	 */
	ExpressionPtr stripAttributes(const ExpressionPtr& expr);

	/**
	 * Obtains all attributes attached to the given expression.
	 *
	 * @param expr the expression for which all attributes should be obtained
	 * @return the list of obtained attributes
	 */
	AttributeSet getAttributes(const ExpressionPtr& expr);

	/**
	 * Attaches the given list of attributes to the given expression and returns the
	 * resulting modified expression. If the given expr has already some attributes
	 * attached, those will be replaced.
	 *
	 * @param expr the expression to be attributed
	 * @param attributes the list of attributes to be attached
	 * @return the modified expression
	 */
	ExpressionPtr setAttributes(const ExpressionPtr& expr, const AttributeSet& attributes);

} // end namespace analysis
} // end namespace core
} // end namespace insieme
