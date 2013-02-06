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
		bool isAttribute(const ExpressionPtr& expr) {
			if (!expr) { return false; }
			const auto& ext = expr->getNodeManager().getLangExtension<AttributeExtension>();
			return expr->getType() == ext.getAttributeType();
		}

	}

	typedef encoder::ListConverter<ExpressionPtr, encoder::DirectExprConverter> AttributConverter;


	bool hasAttribute(const ExpressionPtr& expr, const AttributePtr& attribute) {
		assert(isAttribute(attribute) && "Checking for non-attribute!");

		// simple "dirty" implementation
		return getAttributes(expr).contains(attribute);
	}

	ExpressionPtr addAttribute(const ExpressionPtr& expr, const AttributePtr& attribute) {
		assert(isAttribute(attribute) && "Cannot add non-attribute!");

		// simple "dirty" implementation
		auto attributes = getAttributes(expr);
		attributes.insert(attribute);
		return setAttributes(expr, attributes);
	}

	ExpressionPtr remAttribute(const ExpressionPtr& expr, const AttributePtr& attribute) {
		assert(isAttribute(attribute) && "Cannot remove non-attribute!");

		// simple "dirty" implementation
		auto attributes = getAttributes(expr);
		attributes.erase(attribute);
		return setAttributes(expr, attributes);
	}

	ExpressionPtr stripAttributes(const ExpressionPtr& expr) {
		if (expr->getNodeType() != NT_CallExpr) {
			return expr;
		}

		// convert to call
		const CallExprPtr& call = expr.as<CallExprPtr>();

		// strip of attribute wrapper
		const auto& ext = expr->getNodeManager().getLangExtension<AttributeExtension>();
		if (!ext.isAttr(call->getFunctionExpr())) {
			// not wrapped => return result
			return expr;
		}

		// no wrapper => return expression
		return stripAttributes(call->getArgument(0));
	}


	AttributeSet getAttributes(const ExpressionPtr& expr) {
		if (!expr || expr->getNodeType() != NT_CallExpr) {
			return AttributeSet();
		}

		// convert to call
		const CallExprPtr& call = expr.as<CallExprPtr>();

		// strip of attribute wrapper
		const auto& ext = expr->getNodeManager().getLangExtension<AttributeExtension>();
		if (!ext.isAttr(call->getFunctionExpr())) {
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
