/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/list.h"
#include "insieme/core/types/match.h"

namespace insieme {
namespace core {
namespace lang {

	namespace {

		bool isInf(const TypePtr& type) {
			auto gen = type.isa<GenericTypePtr>();
			return gen && gen->getFamilyName() == "inf" && gen->getParents().empty() && gen->getTypeParameter().empty();
		}

		ExpressionPtr toUIntInf(const ExpressionPtr& expr) {
			// check for literals
			LiteralPtr size = expr.isa<LiteralPtr>();
			if (!size) {
				return expr;
			}

			// fix literal type
			auto& mgr = size->getNodeManager();
			auto& basic = mgr.getLangBasic();
			if (basic.isUIntInf(size->getType())) {
				return size;	// take size at it is
			}

			// fix the size
			return Literal::get(mgr, basic.getUIntInf(), size->getValue());
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
		if(isInf(type->getTypeParameter(1))) {
			// unknown sized array
			*this = ArrayType(type->getTypeParameter(0), ExpressionPtr());
		} else if (auto num = type->getTypeParameter(1).isa<NumericTypePtr>()) {
			// variable or fixed sized array
			*this = ArrayType(type->getTypeParameter(0), num->getValue());
		} else {
			// check validity
			auto size = type->getTypeParameter(1).isa<TypeVariablePtr>();
			assert_true(size) << "Invalid size parameter: " << *size << " of kind: " << size->getNodeType();
			// generic array type
			*this = ArrayType(type->getTypeParameter(0), size);
		}
	}

	bool ArrayType::operator==(const ArrayType& other) const {
		return elementType == other.elementType && size == other.size;
	}

	namespace {

		bool isArrayTypeInternal(const GenericTypePtr& arr) {

			// check properties
			if(arr->getTypeParameter().size()!=2) return false;
			auto size = arr->getTypeParameter(1);

			return arr->getParents().empty()
				&& arr->getName()->getValue() == "array"
				&& (size.isa<TypeVariablePtr>() || size.isa<NumericTypePtr>() || isInf(size));
		}

	}


	bool ArrayType::isArrayType(const NodePtr& node) {

		// a quick check
		auto type = node.isa<GenericTypePtr>();
		if(!type) return false;

		// the annotation to cache check results
		struct ArrayTypeMark {
			bool res;
			bool operator==(const ArrayTypeMark& other) const { return res == other.res; }
		};

		// check annotation
		if (type->hasAttachedValue<ArrayTypeMark>()) {
			return type->getAttachedValue<ArrayTypeMark>().res;
		}

		// compute result
		bool res = isArrayTypeInternal(type);

		// attach result
		type->attachValue(ArrayTypeMark{ res });

		// done
		return res;

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

	bool ArrayType::isGenericSizedArrayType(const NodePtr& node) {
		return isArrayType(node) && node.as<GenericTypePtr>().getTypeParameter(1).isa<TypeVariablePtr>();
	}

	GenericTypePtr ArrayType::create(const TypePtr& elementType, const ExpressionPtr& size) {
		return static_cast<GenericTypePtr>(ArrayType(elementType, toUIntInf(size)));
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
		if(!size) {
			num = GenericType::get(nm, "inf");
		} else if(auto lit = size.isa<LiteralPtr>()) {
			assert_pred1(builder.getLangBasic().isUIntInf, lit->getType());
			num = NumericType::get(nm, lit);
		} else if (auto var = size.isa<VariablePtr>()) {
			assert_pred1(builder.getLangBasic().isUIntInf, var->getType());
			num = NumericType::get(nm, var);
		} else {
			num = size.as<TypeVariablePtr>();
		}

		return GenericType::get(nm, "array", ParentList(), toVector(elementType, num));
	}

	void ArrayType::setSize(const LiteralPtr& size) {
		this->size = toUIntInf(size);
	}

	void ArrayType::setSize(unsigned size) {
		auto& mgr = elementType->getNodeManager();
		auto& basic = mgr.getLangBasic();
		this->size = Literal::get(mgr, basic.getUIntInf(), toString(size));
	}

	void ArrayType::setSize(const VariablePtr& size) {
		if(size) assert_pred1(size->getNodeManager().getLangBasic().isUIntInf, size->getType());
		this->size = size;
	}

	boost::optional<ArrayType> isArrayInit(const NodePtr& node) {
		if (!node || !node.isa<InitExprPtr>()) {
			return boost::optional<ArrayType>();
		}

		auto initExpr = node.as<InitExprPtr>();
		auto initType = analysis::getReferencedType(initExpr->getType());

		if (isArray(initType)) {
			return boost::optional<ArrayType>(initType);
		}

		return boost::optional<ArrayType>();
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
