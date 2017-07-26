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

#include "insieme/core/analysis/attributes.h"

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"

namespace insieme {
namespace core {
namespace analysis {

	namespace {

		/**
		 * Tests whether the given expression is a valid attribute.
		 */
		inline bool isAttribute(const ExpressionPtr& expr) {
			if(!expr) { return false; }
			const auto& ext = expr->getNodeManager().getLangExtension<AttributeExtension>();
			return expr->getType() == ext.getAttributeType();
		}
	}

	typedef encoder::ListConverter<ExpressionPtr, encoder::DirectExprConverter> AttributConverter;


	bool hasAttribute(const ExpressionPtr& expr, const AttributePtr& attribute) {
		assert_true(isAttribute(attribute)) << "Checking for non-attribute!";

		// simple "dirty" implementation
		return getAttributes(expr).contains(attribute);
	}

	ExpressionPtr addAttribute(const ExpressionPtr& expr, const AttributePtr& attribute) {
		assert_true(isAttribute(attribute)) << "Cannot add non-attribute!";

		// simple "dirty" implementation
		auto attributes = getAttributes(expr);
		attributes.insert(attribute);
		return setAttributes(expr, attributes);
	}

	ExpressionPtr remAttribute(const ExpressionPtr& expr, const AttributePtr& attribute) {
		assert_true(isAttribute(attribute)) << "Cannot remove non-attribute!";

		// simple "dirty" implementation
		auto attributes = getAttributes(expr);
		attributes.erase(attribute);
		return setAttributes(expr, attributes);
	}

	ExpressionPtr stripAttributes(const ExpressionPtr& expr) {
		if(expr->getNodeType() != NT_CallExpr) { return expr; }

		// convert to call
		const CallExprPtr& call = expr.as<CallExprPtr>();

		// strip of attribute wrapper
		const auto& ext = expr->getNodeManager().getLangExtension<AttributeExtension>();
		if(!ext.isAttr(call->getFunctionExpr())) {
			// not wrapped => return result
			return expr;
		}

		// no wrapper => return expression
		return stripAttributes(call->getArgument(0));
	}


	AttributeSet getAttributes(const ExpressionPtr& expr) {
		if(!expr || expr->getNodeType() != NT_CallExpr) { return AttributeSet(); }

		// convert to call
		const CallExprPtr& call = expr.as<CallExprPtr>();

		// strip of attribute wrapper
		const auto& ext = expr->getNodeManager().getLangExtension<AttributeExtension>();
		if(!ext.isAttr(call->getFunctionExpr())) {
			// not wrapped => return empty set
			return AttributeSet();
		}

		// collect set of attributes from wrapped expression
		auto res = getAttributes(call->getArgument(0));

		// add local attributes
		auto attributes = core::encoder::toValue<vector<ExpressionPtr>, AttributConverter>(call->getArgument(1));
		res.insert(attributes.begin(), attributes.end());

		// return united result
		return res;
	}

	ExpressionPtr setAttributes(const ExpressionPtr& expr, const AttributeSet& attributes) {
		auto& mgr = expr->getNodeManager();
		const auto& ext = expr->getNodeManager().getLangExtension<AttributeExtension>();
		IRBuilder builder(mgr);

		// build up attribute list
		std::vector<AttributePtr> list(attributes.begin(), attributes.end());
		ExpressionPtr attrExpr = core::encoder::toIR<decltype(list), AttributConverter>(mgr, list);

		// build wrapped expression
		return builder.callExpr(expr->getType(), ext.getAttr(), stripAttributes(expr), attrExpr);
	}


} // end namespace analysis
} // end namespace core
} // end namespace insieme
