/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/lang/list.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/encoder/lists.h"

namespace insieme {
namespace core {
namespace lang {
	
	bool isList(const core::NodePtr& node) {
		if(auto expr = node.isa<ExpressionPtr>()) return isList(expr->getType());
		auto type = node.isa<TypePtr>();
		if(!type) return false;
		if(type->getNodeType() != NT_GenericType) { return false; }

		// check generic type properties
		auto genType = type.as<GenericTypePtr>();
		return genType->getName()->getValue() == "list" && genType->getTypeParameter().size() == static_cast<std::size_t>(1);
	}

	const TypePtr getListElementType(const core::TypePtr& listType) {
		assert_true(isList(listType)) << "Not applicable to non-list type!";
		return listType.as<GenericTypePtr>()->getTypeParameter()->getElement(0);
	}

	const TypePtr getListType(const core::TypePtr& elementType) {
		IRBuilder builder(elementType->getNodeManager());
		return builder.genericType("list", toVector(elementType));
	}

	ExpressionPtr buildListEmpty(const TypePtr& type) {
		IRBuilder builder(type->getNodeManager());
		auto& lExt = type->getNodeManager().getLangExtension<ListExtension>();
		return builder.callExpr(getListType(type), lExt.getListEmpty(), builder.getTypeLiteral(type));
	}

	ExpressionPtr buildListCons(const ExpressionPtr& head, const ExpressionPtr& list) {
		assert_pred1(core::lang::isList, list) << "Trying to add list element to non-list.";
		IRBuilder builder(list->getNodeManager());
		auto& lExt = list->getNodeManager().getLangExtension<ListExtension>();
		return builder.callExpr(list->getType(), lExt.getListCons(), head, list);		
	}
	
	ExpressionPtr buildListOfExpressions(const ExpressionList& expressions) {
		assert_gt(expressions.size(), 0) << "Need at least one element in list, or use buildListEmpty";
		return encoder::toIR<ExpressionList, encoder::DirectExprListConverter>(expressions.front().getNodeManager(), expressions);
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
