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

	ExpressionList parseListOfExpressions(const ExpressionPtr& list) {
		return encoder::toValue<ExpressionList, encoder::DirectExprListConverter>(list);
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
