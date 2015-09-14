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

#include "insieme/core/lang/array.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/types/match.h"
#include "insieme/core/lang/list.h"

namespace insieme {
namespace core {
namespace lang {

	namespace {

		bool isInf(const TypePtr& type) {
			auto gen = type.isa<GenericTypePtr>();
			return gen && gen->getFamilyName() == "inf" && gen->getParents().empty() && gen->getTypeParameter().empty();
		}

	}

	ArrayType::ArrayType(const NodePtr& node) {
		assert_true(node) << "Given node is null!";

		// support expressions as input
		auto type = node.isa<GenericTypePtr>();
		if (auto expr = node.isa<ExpressionPtr>()) type = expr->getType().isa<GenericTypePtr>();

		// check given node type
		assert_true(isArrayType(type)) << "Given node " << *node << " is not a array type!";

		// process node type
		if(isInf(type->getTypeParameter(1))) *this = ArrayType(type->getTypeParameter(0), ExpressionPtr());
		else *this = ArrayType(type->getTypeParameter(0), type->getTypeParameter(1).as<NumericTypePtr>()->getValue());
	}

	bool ArrayType::isArrayType(const NodePtr& node) {
		auto type = node.isa<GenericTypePtr>();
		if(!type) return false;

		// simple approach: use unification
		NodeManager& mgr = node.getNodeManager();
		const ArrayExtension& ext = mgr.getLangExtension<ArrayExtension>();

		// unify given type with template type
		auto ref = ext.getGenArray().as<GenericTypePtr>();
		auto sub = types::match(mgr, type, ref);
		if(!sub) return false;

		// check instantiation
		const types::Substitution& map = *sub;
		auto size = map.applyTo(ref->getTypeParameter(1));
		return size.isa<TypeVariablePtr>() || size.isa<NumericTypePtr>() || isInf(size);
	}

	bool ArrayType::isFixedSizedArrayType(const NodePtr& node) {
		// arrays that are with a constant numeric type parameter
		return isArrayType(node) &&
				node.as<GenericTypePtr>().getTypeParameter(1).isa<NumericTypePtr>() &&
				node.as<GenericTypePtr>().getTypeParameter(1).as<NumericTypePtr>().isConstant() ;
	}

	bool ArrayType::isVariableSizedArrayType(const NodePtr& node) {
		// arrays that are with a constant numeric type parameter
		return isArrayType(node) &&
				node.as<GenericTypePtr>().getTypeParameter(1).isa<NumericTypePtr>() &&
				node.as<GenericTypePtr>().getTypeParameter(1).as<NumericTypePtr>().isVariable() ;
	}

	bool ArrayType::isUnknownSizedArrayType(const NodePtr& node) {
		return isArrayType(node) &&
				isInf(node.as<GenericTypePtr>().getTypeParameter(1));
	}


	GenericTypePtr ArrayType::create(const TypePtr& elementType, const ExpressionPtr& size) {
		return static_cast<GenericTypePtr>(ArrayType(elementType, size));
	}

	GenericTypePtr ArrayType::create(const TypePtr& elementType, unsigned size) {
		auto& mgr = elementType->getNodeManager();
		ExpressionPtr s = Literal::get(mgr, mgr.getLangBasic().getUIntInf(), toString(size));
		return create(elementType, s);
	}

	ArrayType::operator GenericTypePtr() const {
		NodeManager& nm = elementType.getNodeManager();
		IRBuilder builder(nm);

		TypePtr num;
		if (!size) {
			num = GenericType::get(nm, "inf");
		} else if(auto lit = size.isa<LiteralPtr>()) {
			auto newIntLit = builder.literal(lit.getValue(), builder.getLangBasic().getIntInf());
			num = NumericType::get(nm, newIntLit);
		} else {
			assert_true(size.isa<VariablePtr>()) << "Not a variable: " << *size << "\n";
			num = NumericType::get(nm, size.as<VariablePtr>());
		}

		return GenericType::get(nm, "array", ParentList(), toVector(elementType, num));
	}

	ExpressionPtr buildArrayCreate(const TypePtr& elemType, const TypePtr& size, const ExpressionList& list) {
		NodeManager& nm = elemType.getNodeManager();
		IRBuilder builder(nm);
		auto& arrExt = nm.getLangExtension<ArrayExtension>();

		return builder.callExpr(arrExt.getArrayCreate(), builder.getTypeLiteral(elemType), builder.getTypeLiteral(size),
			                    core::lang::buildListOfExpressions(list));
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
