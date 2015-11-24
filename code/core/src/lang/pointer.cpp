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
				isTrueMarker(extractConstMarker(tupleType)),
				isTrueMarker(extractVolatileMarker(tupleType))
		);
	}

	TypePtr PointerType::create(const TypePtr& elementType, bool _const, bool _volatile) {
		assert_true(elementType);
		return PointerType(elementType, _const, _volatile).operator TypePtr();
	}


	PointerType::operator TypePtr() const {
		NodeManager& mgr = elementType.getNodeManager();
		IRBuilder builder(mgr);

		auto& ext = mgr.getLangBasic();

		// build a tuple representing this pointer
		return builder.tupleType(toVector(ReferenceType::create(ArrayType::create(elementType), mConst, mVolatile).as<TypePtr>(), ext.getInt8().as<TypePtr>()));
	}

	bool isPointer(const NodePtr& node) {

		// check for null
		if (!node) return false;

		// check for an expression
		if (auto expr = node.isa<ExpressionPtr>()) return isPointer(expr->getType());

		// needs to be a tuple type
		auto type = node.isa<TupleTypePtr>();
		if(!type) return false;

		// simple approach: use unification
		NodeManager& mgr = node.getNodeManager();
		const PointerExtension& ext = mgr.getLangExtension<PointerExtension>();

		// unify given type with template type
		auto ref = ext.getGenPtr();
		auto sub = types::match(mgr, type, ref);
		if(!sub) return false;

		// check instantiation
		return isValidBooleanMarker(extractConstMarker(type)) && isValidBooleanMarker(extractVolatileMarker(type));
	}

	TypePtr buildPtrType(const TypePtr& elementType, bool _const, bool _volatile) {
		return PointerType::create(elementType, _const, _volatile);
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

	ExpressionPtr buildPtrFromRef(const ExpressionPtr& refExpr) {
		assert_pred1(isReference, refExpr) << "Trying to build ptr from non-ref.";
		IRBuilder builder(refExpr->getNodeManager());
		auto& pExt = refExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrFromRef(), refExpr);
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
		IRBuilder builder(intExpr->getNodeManager());
		auto& pExt = intExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrFromIntegral(), intExpr, builder.getTypeLiteral(ptrType));
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

	ExpressionPtr buildPtrToRef(const ExpressionPtr& ptrExpr) {
		assert_pred1(isPointer, ptrExpr) << "Trying to build a ref from non-ptr.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrToRef(), ptrExpr);
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

	ExpressionPtr buildPtrDeref(const ExpressionPtr& ptrExpr) {
		assert_pred1(isPointer, ptrExpr) << "Trying to build ptr_deref from non-ptr type.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrDeref(), ptrExpr);
	}

	ExpressionPtr buildPtrSubscript(const ExpressionPtr& ptrExpr, const ExpressionPtr& subscriptExpr) {
		assert_pred1(isPointer, ptrExpr) << "Trying to build a ptr subscript from non-ptr.";
		assert_pred1(ptrExpr->getNodeManager().getLangBasic().isInt, subscriptExpr->getType()) << "Trying to build a ptr subscript with non-integral subscript.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrSubscript(), ptrExpr, subscriptExpr);
	}

	ExpressionPtr buildPtrOperation(BasicGenerator::Operator op, const ExpressionPtr& lhs, const ExpressionPtr& rhs) {
		auto& basic = lhs->getNodeManager().getLangBasic();
		auto& pExt = lhs->getNodeManager().getLangExtension<PointerExtension>();
		IRBuilder builder(lhs->getNodeManager());

		auto assertPtr = [&](const ExpressionPtr& exp) {
			assert_pred1(isPointer, exp) << "Trying to build a ptr operation from non-ptr:"
				<< "\n lhs: " << *lhs << "\n  - of type: " << *lhs->getType() 
				<< "\n rhs: " << *rhs << "\n  - of type: " << *rhs->getType()
				<< "\n op: " << op;
		};
		auto assertInt = [&](const ExpressionPtr& exp) {
			assert_pred1(basic.isInt, exp->getType()) << "Trying to build a ptr add/sub with non-int"
				<< "\n lhs: " << *lhs << "\n  - of type: " << *lhs->getType() 
				<< "\n rhs: " << *rhs << "\n  - of type: " << *rhs->getType()
				<< "\n op: " << op;
		};
		auto buildInt8Cast = [&](const ExpressionPtr& exp) {
			if(!core::types::isSubTypeOf(exp->getType(), basic.getInt8())) {
				return builder.numericCast(exp, basic.getInt8());
			}
			return exp;
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
