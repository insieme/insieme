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
#include "insieme/core/lang/pointer.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/types/match.h"

#include "insieme/core/lang/boolean_marker.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/array.h"

namespace insieme {
namespace core {
namespace lang {

	namespace {

		TypePtr extractReferencedType(const TupleTypePtr& type) {
			GenericTypePtr refType = type->getElement(0).as<GenericTypePtr>();
			return ArrayType(ReferenceType(refType).getElementType()).getElementType();
		}

		TypePtr extractConstMarker(const TupleTypePtr& type) {
			GenericTypePtr refType = type->getElement(0).as<GenericTypePtr>();
			return refType->getTypeParameter(1);
		}

		TypePtr extractVolatileMarker(const TupleTypePtr& type) {
			GenericTypePtr refType = type->getElement(0).as<GenericTypePtr>();
			return refType->getTypeParameter(2);
		}

	}


	PointerType::PointerType(const NodePtr& node) {

		// check given node type
		assert_true(node) << "Invalid input node!";
		assert_true(isPointer(node)) << "Given node " << *node << " is not a pointer type!";

		// get the type
		NodePtr trg = node;
		if (auto expr = node.isa<ExpressionPtr>()) trg = expr->getType();

		// process node type
		auto tupleType = trg.as<TupleTypePtr>();
		*this = PointerType(
				extractReferencedType(tupleType),
				extractConstMarker(tupleType),
				extractVolatileMarker(tupleType)
		);
	}

	TypePtr PointerType::create(const TypePtr& elementType, bool _const, bool _volatile) {
		assert_true(elementType);

		NodeManager& mgr = elementType.getNodeManager();
		const BooleanMarkerExtension& ext = mgr.getLangExtension<BooleanMarkerExtension>();

		return PointerType(elementType, ext.getMarkerType(_const), ext.getMarkerType(_volatile)).operator TypePtr();
	}


	PointerType::operator TypePtr() const {
		NodeManager& mgr = elementType.getNodeManager();
		IRBuilder builder(mgr);

		auto& ext = mgr.getLangBasic();

		// build a tuple representing this pointer
		return builder.tupleType(toVector(ReferenceType::create(ArrayType::create(elementType), mConst, mVolatile).as<TypePtr>(), ext.getInt8().as<TypePtr>()));
	}

	const TypePtr& PointerType::getElementType() const {
		return elementType;
	}

	void PointerType::setElementType(const TypePtr& type) {
		assert_true(type) << "Element type must not be null!";
		elementType = type;
	}

	bool PointerType::isConst() const {
		return isTrueMarker(mConst);
	}

	void PointerType::setConst(bool newState) {
		const auto& ext = elementType->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		mConst = (newState) ? ext.getTrueMarker() : ext.getFalseMarker();
	}

	bool PointerType::isVolatile() const {
		return isTrueMarker(mVolatile);
	}

	void PointerType::setVolatile(bool newState) {
		const auto& ext = elementType->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		mVolatile = (newState) ? ext.getTrueMarker() : ext.getFalseMarker();
	}

	namespace {
		bool isPointerInternal(const TupleTypePtr& type) {
			NodeManager& mgr = type.getNodeManager();
			auto& basic = mgr.getLangBasic();

			// check properties directly
			return type->getElements().size() == 2
				&& basic.isInt8(type->getElement(1))
				&& isReference(type->getElement(0))
				&& isArray(core::analysis::getReferencedType(type->getElement(0)))
				&& isValidBooleanMarker(extractConstMarker(type))
				&& isValidBooleanMarker(extractVolatileMarker(type));
		}

	}


	bool isPointer(const NodePtr& node) {

		// check for null
		if (!node) return false;

		// check for an expression
		if (auto expr = node.isa<ExpressionPtr>()) return isPointer(expr->getType());

		// needs to be a tuple type
		auto type = node.isa<TupleTypePtr>();
		if(!type) return false;

		// check annotation
		struct PointerMarker {
			bool res;
			bool operator==(const PointerMarker& other) const {
				return res == other.res;
			}
		};

		// check for annotations
		if (type->hasAttachedValue<PointerMarker>()) {
			return type->getAttachedValue<PointerMarker>().res;
		}

		// compute, attach, and return result
		bool res = isPointerInternal(type);
		type->attachValue(PointerMarker{ res });
		return res;
	}

	TypePtr buildPtrType(const TypePtr& elementType, bool _const, bool _volatile) {
		return PointerType::create(elementType, _const, _volatile);
	}

	TypePtr buildPtrType(const ReferenceType& referenceType) {
		return buildPtrType(referenceType.getElementType(), referenceType.isConst(), referenceType.isVolatile());
	}

	insieme::core::ExpressionPtr buildPtrNull(const TypePtr& type) {
		assert_pred1(isPointer, type) << "Trying to build a null ptr which isn't a pointer.";
		IRBuilder builder(type->getNodeManager());
		auto& pExt = type->getNodeManager().getLangExtension<PointerExtension>();
		auto& bmExt = type->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		PointerType pt(type);
		return builder.callExpr(pExt.getPtrNull(), builder.getTypeLiteral(pt.getElementType()), bmExt.getMarkerTypeLiteral(pt.isConst()),
			                    bmExt.getMarkerTypeLiteral(pt.isVolatile()));
	}

	ExpressionPtr buildPtrFromRef(const ExpressionPtr& refExpr, bool simplify) {
		assert_pred1(isReference, refExpr) << "Trying to build ptr from non-ref.";
		IRBuilder builder(refExpr->getNodeManager());
		auto& pExt = refExpr->getNodeManager().getLangExtension<PointerExtension>();
		// if we are operating on a ptr_to_ref, strip it rather than adding a ptr_from_ref
		if(simplify && analysis::isCallOf(refExpr, pExt.getPtrToRef())) return refExpr.as<CallExprPtr>()->getArgument(0);
		return builder.callExpr(pExt.getPtrFromRef(), refExpr);
	}

	ExpressionPtr buildPtrToRef(const ExpressionPtr& ptrExpr, bool simplify) {
		assert_pred1(isPointer, ptrExpr) << "Trying to build a ref from non-ptr.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		// if we are operating on a ptr_from_ref, strip it rather than adding a ptr_to_ref
		if(simplify && analysis::isCallOf(ptrExpr, pExt.getPtrFromRef())) return ptrExpr.as<CallExprPtr>()->getArgument(0);
		return builder.callExpr(pExt.getPtrToRef(), ptrExpr);
	}

	ExpressionPtr buildPtrFromArray(const ExpressionPtr& arrExpr) {
		assert_pred1(isReference, arrExpr) << "Trying to buildPtrFromArray from non-ref.";
		assert_pred1(isArray, core::analysis::getReferencedType(arrExpr->getType())) << "Trying to buildPtrFromArray from non-array.";
		IRBuilder builder(arrExpr->getNodeManager());
		auto& pExt = arrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrFromArray(), arrExpr);
	}

	ExpressionPtr buildPtrFromIntegral(const ExpressionPtr& intExpr, const TypePtr& ptrType) {
		assert_pred1(intExpr->getNodeManager().getLangBasic().isInt, intExpr->getType()) << "Trying to build ptr from non-integral.";
		assert_pred1(isPointer, ptrType) << "Trying to build non-ptr-type from integral.";

		PointerType pointerType(ptrType);
		IRBuilder builder(intExpr->getNodeManager());
		auto& pExt = intExpr->getNodeManager().getLangExtension<PointerExtension>();
		ExpressionPtr res = builder.callExpr(pExt.getPtrFromIntegral(), intExpr, builder.getTypeLiteral(pointerType.getElementType()));

		// update pointer flags
		if (pointerType.isConst() || pointerType.isVolatile()) {
			res = buildPtrCast(res, pointerType.isConst(), pointerType.isVolatile());
		}

		// done
		return res;
	}

	ExpressionPtr buildPtrToIntegral(const ExpressionPtr& ptrExpr, const TypePtr& intType) {
		assert_pred1(isPointer, ptrExpr) << "Trying to build a ref from non-ptr.";
		assert_pred1(ptrExpr->getNodeManager().getLangBasic().isInt, intType) << "Trying to build non-integral from ptr.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrToIntegral(), ptrExpr, builder.getTypeLiteral(intType));
	}

	ExpressionPtr buildPtrOfFunction(const ExpressionPtr& funExpr) {
		assert_true(funExpr->getType().isa<FunctionTypePtr>()) << "Trying to build a ptr of a non-function type:\n" << dumpColor(funExpr)
			                                                   << "\nType: " << dumpColor(funExpr->getType());
		IRBuilder builder(funExpr->getNodeManager());
		auto& pExt = funExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrOfFunction(), funExpr);
	}

	ExpressionPtr buildPtrToArray(const ExpressionPtr& ptrExpr) {
		assert_pred1(isPointer, ptrExpr) << "Trying to build an array from non-ptr.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrToArray(), ptrExpr);
	}

	ExpressionPtr buildPtrCast(const ExpressionPtr& ptrExpr, bool newConst, bool newVolatile) {
		assert_pred1(core::lang::isPointer, ptrExpr) << "Trying to build a ptr cast from non-ptr.";
		PointerType srcTy(ptrExpr->getType());
		// early exit if there is nothing to do
		if(srcTy.isConst() == newConst && srcTy.isVolatile() == newVolatile) return ptrExpr;
		// otherwise, build cast
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		auto& bmExt = ptrExpr->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		return builder.callExpr(pExt.getPtrCast(), ptrExpr, bmExt.getMarkerTypeLiteral(newConst), bmExt.getMarkerTypeLiteral(newVolatile));
	}

	ExpressionPtr buildPtrReinterpret(const ExpressionPtr& ptrExpr, const TypePtr& newElementType) {
		assert_pred1(core::lang::isPointer, ptrExpr) << "Trying to build a ptr reinterpret from non-ptr.";
		PointerType srcTy(ptrExpr->getType());
		// early exit if there is nothing to do
		if(srcTy.getElementType() == newElementType) return ptrExpr;
		// otherwise, build reinterpret
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrReinterpret(), ptrExpr, builder.getTypeLiteral(newElementType));
	}

	ExpressionPtr buildPtrParentCast(const ExpressionPtr& ptrExpr, const TypePtr& parentType) {
		assert_pred1(core::lang::isPointer, ptrExpr) << "Trying to build a ptr parent cast from non-ptr.";
		PointerType srcTy(ptrExpr->getType());
		// early exit if there is nothing to do
		if(srcTy.getElementType() == parentType) return ptrExpr;
		// otherwise, build parent cast
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrParentCast(), ptrExpr, builder.getTypeLiteral(parentType));
	}

	ExpressionPtr buildPtrDeref(const ExpressionPtr& ptrExpr) {
		assert_pred1(isPointer, ptrExpr) << "Trying to build ptr_deref from non-ptr type.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrDeref(), ptrExpr);
	}

	namespace {

		ExpressionPtr buildInt8Cast(const ExpressionPtr& exp) {
			auto& basic = exp->getNodeManager().getLangBasic();
			IRBuilder builder(exp->getNodeManager());

			if(!core::types::isSubTypeOf(exp->getType(), basic.getInt8())) {
				return builder.numericCast(exp, basic.getInt8());
			}
			return exp;
		}
	}

	ExpressionPtr buildPtrSubscript(const ExpressionPtr& ptrExpr, const ExpressionPtr& subscriptExpr) {
		assert_pred1(isPointer, ptrExpr) << "Trying to build a ptr subscript from non-ptr.";
		assert_pred1(ptrExpr->getNodeManager().getLangBasic().isInt, subscriptExpr->getType()) << "Trying to build a ptr subscript with non-integral subscript.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrSubscript(), ptrExpr, buildInt8Cast(subscriptExpr));
	}

	ExpressionPtr buildPtrOperation(BasicGenerator::Operator op, const ExpressionPtr& lhs, const ExpressionPtr& rhs) {
		auto& pExt = lhs->getNodeManager().getLangExtension<PointerExtension>();
		IRBuilder builder(lhs->getNodeManager());

		auto assertPtr = [&](const ExpressionPtr& exp) {
			assert_pred1(isPointer, exp) << "Trying to build a ptr operation from non-ptr:"
				<< "\n lhs: " << *lhs << "\n  - of type: " << *lhs->getType()
				<< "\n rhs: " << *rhs << "\n  - of type: " << *rhs->getType()
				<< "\n op: " << op;
		};
		auto assertInt = [&](const ExpressionPtr& exp) {
			assert_pred1(builder.getLangBasic().isInt, exp->getType()) << "Trying to build a ptr add/sub with non-int"
				<< "\n lhs: " << *lhs << "\n  - of type: " << *lhs->getType()
				<< "\n rhs: " << *rhs << "\n  - of type: " << *rhs->getType()
				<< "\n op: " << op;
		};

		// arithmetic operations
		switch(op) {
		case BasicGenerator::Operator::Add: {
			if(!isPointer(lhs)) {
				assertPtr(rhs);
				assertInt(lhs);
				return builder.callExpr(pExt.getPtrAdd(), rhs, buildInt8Cast(lhs));
			}
			assertPtr(lhs);
			assertInt(rhs);
			return builder.callExpr(pExt.getPtrAdd(), lhs, buildInt8Cast(rhs));
		}
		case BasicGenerator::Operator::Sub: { // minus is only supported with ptr on the lhs
			assertPtr(lhs);
			if(!isPointer(rhs)) {
				assertInt(rhs);
				return builder.callExpr(pExt.getPtrSub(), lhs, buildInt8Cast(rhs));
			} else {
				return builder.callExpr(pExt.getPtrDiff(), lhs, rhs);
			}
		}
		default: break;
		}

		// comparison operations
		assertPtr(lhs);
		assertPtr(rhs);
		switch(op) {
		case BasicGenerator::Operator::Eq: return builder.callExpr(pExt.getPtrEqual(), lhs, rhs);
		case BasicGenerator::Operator::Ne: return builder.callExpr(pExt.getPtrNotEqual(), lhs, rhs);
		case BasicGenerator::Operator::Le: return builder.callExpr(pExt.getPtrLessEqual(), lhs, rhs);
		case BasicGenerator::Operator::Lt: return builder.callExpr(pExt.getPtrLessThan(), lhs, rhs);
		case BasicGenerator::Operator::Ge: return builder.callExpr(pExt.getPtrGreaterEqual(), lhs, rhs);
		case BasicGenerator::Operator::Gt: return builder.callExpr(pExt.getPtrGreaterThan(), lhs, rhs);
		default: break;
		}

		assert_fail() << "Unsupported binary pointer operation " << op;
		return ExpressionPtr();
	}


	ExpressionPtr buildPtrOperation(BasicGenerator::Operator op, const ExpressionPtr& ptrExpr) {
		assert_true(isReference(ptrExpr) && isPointer(core::analysis::getReferencedType(ptrExpr->getType())))
			<< "Trying to build a unary pointer operation with non-ref<ptr>.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		switch(op) {
		case BasicGenerator::Operator::PostInc: return builder.callExpr(pExt.getPtrPostInc(), ptrExpr);
		case BasicGenerator::Operator::PostDec: return builder.callExpr(pExt.getPtrPostDec(), ptrExpr);
		case BasicGenerator::Operator::PreInc: return builder.callExpr(pExt.getPtrPreInc(), ptrExpr);
		case BasicGenerator::Operator::PreDec: return builder.callExpr(pExt.getPtrPreDec(), ptrExpr);
		default: break;
		}

		assert_fail() << "Unsupported unary pointer operation " << op;
		return ExpressionPtr();
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
