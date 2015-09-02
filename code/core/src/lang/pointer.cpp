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

		TypePtr extractReferencedType(const StructTypePtr& type) {
			GenericTypePtr refType = type->getTypeOfMember("data").as<GenericTypePtr>();
			return ArrayType(ReferenceType(refType).getElementType()).getElementType();
		}

		TypePtr extractConstMarker(const StructTypePtr& type) {
			GenericTypePtr refType = type->getTypeOfMember("data").as<GenericTypePtr>();
			return refType->getTypeParameter(1);
		}

		TypePtr extractVolatileMarker(const StructTypePtr& type) {
			GenericTypePtr refType = type->getTypeOfMember("data").as<GenericTypePtr>();
			return refType->getTypeParameter(2);
		}

	}


	PointerType::PointerType(const NodePtr& node) {
		// check given node type
		assert_true(node) << "Given node is null!";
		assert_true(isPointerType(node)) << "Given node " << *node << " is not a pointer type!";

		// process node type
		StructTypePtr type = node.as<StructTypePtr>();
		*this = PointerType(
				extractReferencedType(type),
				isTrueMarker(extractConstMarker(type)),
				isTrueMarker(extractVolatileMarker(type))
		);
	}

	bool PointerType::isPointerType(const NodePtr& node) {
		auto type = node.isa<StructTypePtr>();
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

	StructTypePtr PointerType::create(const TypePtr& elementType, bool _const, bool _volatile) {
		assert_true(elementType);
		return PointerType(elementType, _const, _volatile);
	}

	PointerType::operator StructTypePtr() const {
		NodeManager& mgr = elementType.getNodeManager();
		IRBuilder builder(mgr);

		auto& ext = mgr.getLangBasic();

		// build a struct representing this pointer
		return StructType::get(
				mgr,
				builder.stringValue("_ir_pointer"),
				{
						builder.namedType( builder.stringValue("data"), ReferenceType::create(ArrayType::create(elementType), mConst, mVolatile) ),
						builder.namedType( builder.stringValue("offset"), ext.getInt8() )
				}
		);

	}


	bool isPointer(const NodePtr& node) {
		if(auto expr = node.isa<ExpressionPtr>()) { return isPointer(expr->getType()); }
		return node && PointerType::isPointerType(node);
	}

	bool differOnlyInQualifiers(const TypePtr& typeA, const TypePtr& typeB) {
		assert_true(core::lang::isPointer(typeA) || core::lang::isReference(typeA)) << "Can only check qualifiers on pointers or references, not on "
			                                                                        << dumpColor(typeA);
		assert_true(core::lang::isPointer(typeB) || core::lang::isReference(typeB)) << "Can only check qualifiers on pointers or references, not on "
			                                                                        << dumpColor(typeB);
		
		auto gA = typeA.as<GenericTypePtr>();
		auto gB = typeB.as<GenericTypePtr>();

		return gA->getTypeParameter(0) == gB->getTypeParameter(0)
			   && (gA->getTypeParameter(1) != gB->getTypeParameter(1) || gA->getTypeParameter(2) != gB->getTypeParameter(2));
	}


	ExpressionPtr buildPtrFromRef(const ExpressionPtr& refExpr) {
		assert_pred1(core::lang::isReference, refExpr) << "Trying to build ptr from non-ref.";
		IRBuilder builder(refExpr->getNodeManager());
		auto& pExt = refExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrFromRef(), refExpr);
	}

	ExpressionPtr buildPtrFromArray(const ExpressionPtr& arrExpr) {
		assert_pred1(core::lang::isReference, arrExpr) << "Trying to buildPtrFromArray from non-ref.";
		assert_pred1(core::lang::isArray, core::analysis::getReferencedType(arrExpr->getType())) << "Trying to buildPtrFromArray from non-array.";
		IRBuilder builder(arrExpr->getNodeManager());
		auto& pExt = arrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrFromArray(), arrExpr);
	}

	ExpressionPtr buildPtrToRef(const ExpressionPtr& ptrExpr) {
		assert_pred1(core::lang::isPointer, ptrExpr) << "Trying to build a ref from non-ptr.";
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrToRef(), ptrExpr);
	}
	
	ExpressionPtr buildPtrCast(const ExpressionPtr& ptrExpr, const TypePtr& targetTy) {
		assert_pred1(core::lang::isPointer, ptrExpr) << "Trying to build a ptr cast from non-ptr.";
		assert_pred1(core::lang::isPointer, targetTy) << "Trying to build a ptr cast to non-ptr type.";
		if(targetTy == ptrExpr->getType()) return ptrExpr;
		assert_true(differOnlyInQualifiers(ptrExpr->getType(), targetTy)) << "Ptr cast only allowed to cast between qualifiers.";
		// TODO THIS IS WHY WE NEED THE MODULAR BUILDER
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		auto& bmExt = ptrExpr->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		PointerType pointerTy(targetTy);
		return builder.callExpr(pExt.getPtrCast(), ptrExpr, bmExt.getMarkerTypeLiteral(pointerTy.isConst()),
			                    bmExt.getMarkerTypeLiteral(pointerTy.isVolatile()));
	}

	ExpressionPtr buildPtrSubscript(const ExpressionPtr& ptrExpr, const ExpressionPtr& subscriptExpr) {
		assert_pred1(core::lang::isPointer, ptrExpr) << "Trying to build a ptr subscript from non-ptr.";		
		auto& basic = ptrExpr->getNodeManager().getLangBasic();
		assert_pred1(basic.isInt, subscriptExpr->getType()) << "Trying to build a ptr subscript with non-integral subscript.";
		// TODO THIS IS WHY WE NEED THE MODULAR BUILDER
		IRBuilder builder(ptrExpr->getNodeManager());
		auto& pExt = ptrExpr->getNodeManager().getLangExtension<PointerExtension>();
		return builder.callExpr(pExt.getPtrSubscript(), ptrExpr, subscriptExpr);
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
