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

#include "insieme/core/lang/reference.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/lang/boolean_marker.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/types/match.h"
#include "insieme/core/types/type_variable_deduction.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/manipulation.h"

namespace insieme {
namespace core {
namespace lang {

	namespace {

		bool isRefMarker(const TypePtr& type) {
			if (!type) return false;
			if (type.isa<TypeVariablePtr>()) return true;
			const ReferenceExtension& refExt = type->getNodeManager().getLangExtension<ReferenceExtension>();
			return refExt.isMarkerPlain(type) || refExt.isMarkerCppReference(type) || refExt.isMarkerCppRValueReference(type) || refExt.isMarkerQualified(type);
		}

		ReferenceType::Kind parseKind(const TypePtr& type) {
			assert_pred1(isRefMarker, type);

			const ReferenceExtension& refExt = type->getNodeManager().getLangExtension<ReferenceExtension>();
			if(refExt.isMarkerPlain(type)) return ReferenceType::Kind::Plain;
			if(refExt.isMarkerCppReference(type)) return ReferenceType::Kind::CppReference;
			if(refExt.isMarkerCppRValueReference(type)) return ReferenceType::Kind::CppRValueReference;
			if(refExt.isMarkerQualified(type)) return ReferenceType::Kind::Qualified;
			if(type.isa<TypeVariablePtr>()) return ReferenceType::Kind::Undefined;

			// something went wrong
		 	assert_fail() << "Unknown reference kind: " << type;
			return ReferenceType::Kind::Plain;
		}

		TypePtr toType(NodeManager& mgr, const ReferenceType::Kind& kind) {

			const ReferenceExtension& refExt = mgr.getLangExtension<ReferenceExtension>();
			switch(kind) {
				case ReferenceType::Kind::Plain: 				return refExt.getMarkerPlain();
				case ReferenceType::Kind::CppReference: 		return refExt.getMarkerCppReference();
				case ReferenceType::Kind::CppRValueReference: 	return refExt.getMarkerCppRValueReference();
				case ReferenceType::Kind::Qualified:			return refExt.getMarkerQualified();
				case ReferenceType::Kind::Undefined:            return TypeVariable::get(mgr, "k");
			}

			// something went wrong
			assert_fail() << "Unknown reference kind.";
			return refExt.getMarkerPlain();
		}

	}


	ReferenceType::ReferenceType(const NodePtr& node) {
		// check given node type
		assert_true(node) << "Given node is null!";
		assert_true(isReference(node)) << "Given node " << *node << " is not a reference type!";

		// extract the type
		GenericTypePtr type = node.isa<GenericTypePtr>();
		if (auto expr = node.isa<ExpressionPtr>()) type = expr->getType().as<GenericTypePtr>();

		// initialize the local instance
		*this = ReferenceType(
				type->getTypeParameter(0),
				type->getTypeParameter(1),
				type->getTypeParameter(2),
				type->getTypeParameter(3)
		);
	}

	GenericTypePtr ReferenceType::create(const TypePtr& elementType, bool _const, bool _volatile, const Kind& kind) {
		assert_true(elementType);

		NodeManager& mgr = elementType.getNodeManager();
		const BooleanMarkerExtension& ext = mgr.getLangExtension<BooleanMarkerExtension>();

		return ReferenceType(elementType, ext.getMarkerType(_const), ext.getMarkerType(_volatile), lang::toType(mgr, kind)).operator GenericTypePtr();
	}

	GenericTypePtr ReferenceType::create(const TypePtr& elementType, const TypePtr& _const, const TypePtr& _volatile, const Kind& kind) {
		assert_true(elementType);
		NodeManager& mgr = elementType.getNodeManager();
		return ReferenceType(elementType, _const, _volatile, lang::toType(mgr, kind)).operator GenericTypePtr();
	}

	GenericTypePtr ReferenceType::toType() const {
		NodeManager& mgr = elementType.getNodeManager();
		return GenericType::get(mgr, "ref", ParentList(), toVector(elementType, mConst, mVolatile, kind));
	}


	bool ReferenceType::isConst() const {
		return isTrueMarker(mConst);
	}

	void ReferenceType::setConst(bool newState) {
		const auto& ext = elementType->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		mConst = (newState) ? ext.getTrue() : ext.getFalse();
	}

	bool ReferenceType::isVolatile() const {
		return isTrueMarker(mVolatile);
	}

	void ReferenceType::setVolatile(bool newState) {
		const auto& ext = elementType->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		mVolatile = (newState) ? ext.getTrue() : ext.getFalse();
	}

	ReferenceType::Kind ReferenceType::getKind() const {
		return parseKind(kind);
	}

	void ReferenceType::setKind(const Kind& kind) {
		this->kind = lang::toType(elementType.getNodeManager(), kind);
	}

	bool ReferenceType::isPlain() const {
		return isRefMarker(kind) && getKind() == Kind::Plain;
	}

	bool ReferenceType::isCppReference() const {
		return isRefMarker(kind) && getKind() == Kind::CppReference;
	}

	bool ReferenceType::isCppRValueReference() const {
		return isRefMarker(kind) && getKind() == Kind::CppRValueReference;
	}

	bool ReferenceType::isQualified() const {
		return isRefMarker(kind) && getKind() == Kind::Qualified;
	}

	namespace {

		bool isReferenceInternal(const GenericTypePtr& ref) {

			// make sure the node is an actual type
			assert_true(ref);

			// check instantiation
			return ref->getParents().empty() &&
				ref->getTypeParameter().size() == 4 &&
				ref->getName()->getValue() == "ref" &&
				isValidBooleanMarker(ref->getTypeParameter(1)) &&
				isValidBooleanMarker(ref->getTypeParameter(2)) &&
				isRefMarker(ref->getTypeParameter(3));
		}

	}



	bool isReference(const NodePtr& node) {

		// the mark annotated to cache results
		struct ReferenceMark {
			bool valid;
			bool operator==(const ReferenceMark& other) const { return valid == other.valid;  }
		};

		// check for null
		if(!node) return false;

		// check for expressions
		if (auto expr = node.isa<ExpressionPtr>()) return isReference(expr->getType());

		// now it needs to be a type
		auto type = node.isa<GenericTypePtr>();
		if (!type) return false;

		// check for a cached annotation
		if (type->hasAttachedValue<ReferenceMark>()) {
			return type->getAttachedValue<ReferenceMark>().valid;
		}

		// compute the result
		bool res = isReferenceInternal(type);

		// attach the result
		type->attachValue(ReferenceMark{ res });

		// done
		return res;
	}

	bool isReferenceTo(const NodePtr& node, const TypePtr& type) {
		// check for expressions
		if(auto expr = node.isa<ExpressionPtr>()) return isReferenceTo(expr->getType(), type);
		// check type
		auto nodeType = node.isa<GenericTypePtr>();
		if(!nodeType) return false;
		// check referenced type
		return isReference(nodeType) && ReferenceType(nodeType).getElementType() == type;
	}

	bool isPlainReference(const NodePtr& node) {
		return isReference(node) && ReferenceType(node).isPlain();
	}

	bool isCppReference(const NodePtr& node) {
		return isReference(node) && ReferenceType(node).isCppReference();
	}

	bool isCppRValueReference(const NodePtr& node) {
		return isReference(node) && ReferenceType(node).isCppRValueReference();
	}

	bool isQualifiedReference(const NodePtr& node) {
		return isReference(node) && ReferenceType(node).isQualified();
	}

	bool isAssignment(const NodePtr& node) {
		if(node->getNodeType() != NT_CallExpr) return false;
		auto& rExt = node->getNodeManager().getLangExtension<ReferenceExtension>();
		return rExt.isCallOfRefAssign(node);
	}

	ReferenceType::Kind getReferenceKind(const TypePtr& typeLitType) {
		if(core::analysis::isTypeLiteralType(typeLitType)) return parseKind(core::analysis::getRepresentedType(typeLitType));
		return parseKind(typeLitType);
	}
	ReferenceType::Kind getReferenceKind(const ExpressionPtr& expression) {
		if(isReference(expression)) return ReferenceType(expression).getKind();
		return getReferenceKind(core::analysis::getRepresentedType(expression));
	}

	TypePtr buildRefType(const TypePtr& elementType, bool _const, bool _volatile, const ReferenceType::Kind& kind) {
		return ReferenceType::create(elementType, _const, _volatile, kind);
	}


	bool doReferenceQualifiersDiffer(const TypePtr& typeA, const TypePtr& typeB) {
		assert_true(isReference(typeA)) << "Can only check qualifiers on references, not on " << dumpColor(typeA);
		assert_true(isReference(typeB)) << "Can only check qualifiers on references, not on " << dumpColor(typeB);

		ReferenceType rtA(typeA);
		ReferenceType rtB(typeB);

		return (rtA.isConst() != rtB.isConst())
				|| (rtA.isVolatile() != rtB.isVolatile())
				|| (rtA.getKind() != rtB.getKind());
	}

	bool doReferencesDifferOnlyInQualifiers(const TypePtr& typeA, const TypePtr& typeB) {
		assert_true(isReference(typeA)) << "Can only check qualifiers on references, not on " << dumpColor(typeA);
		assert_true(isReference(typeB)) << "Can only check qualifiers on references, not on " << dumpColor(typeB);

		// check that they are not equal
		if(typeA == typeB) return false;

		auto gA = typeA.as<GenericTypePtr>();
		auto gB = typeB.as<GenericTypePtr>();

		return gA->getTypeParameter(0) == gB->getTypeParameter(0)
				|| types::getTypeVariableInstantiation(typeA->getNodeManager(), gA->getTypeParameter(0), gB->getTypeParameter(0))
				|| types::getTypeVariableInstantiation(typeA->getNodeManager(), gB->getTypeParameter(0), gA->getTypeParameter(0));
	}

	ExpressionPtr buildRefDeref(const ExpressionPtr& refExpr) {
		assert_pred1(isReference, refExpr->getType());
		auto& rExt = refExpr->getNodeManager().getLangExtension<lang::ReferenceExtension>();
		IRBuilder builder(refExpr->getNodeManager());
		return builder.callExpr(analysis::getReferencedType(refExpr->getType()), rExt.getRefDeref(), refExpr);
	}

	ExpressionPtr buildRefCast(const ExpressionPtr& refExpr, const TypePtr& targetTy) {
		assert_pred1(isReference, refExpr) << "Trying to build a ref cast from non-ref.";
		assert_pred1(isReference, targetTy) << "Trying to build a ref cast to non-ref type.";
		if(targetTy == refExpr->getType()) return refExpr;

		// only create the cast if the types really do differ in their qualifiers
		if(!doReferenceQualifiersDiffer(refExpr->getType(), targetTy)) {
			return refExpr;
		}

		assert_true(doReferencesDifferOnlyInQualifiers(refExpr->getType(), targetTy)) << "Ref cast only allowed to cast between qualifiers,"
			<< "trying to cast from\n" << dumpColor(refExpr->getType()) << " - to - \n" << dumpColor(targetTy);


		IRBuilder builder(refExpr->getNodeManager());
		auto& rExt = refExpr->getNodeManager().getLangExtension<ReferenceExtension>();
		auto& bmExt = refExpr->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		ReferenceType referenceTy(targetTy);

		// for temp init exprs, simply change type being created rather than casting
		if(auto initExpr = refExpr.isa<InitExprPtr>()) {
			if(rExt.isCallOfRefTemp(initExpr->getMemoryExpr())) {
				return builder.initExprTemp(targetTy.as<GenericTypePtr>(), initExpr->getInitExprList());
			}
		}

		return builder.callExpr(rExt.getRefCast(), refExpr,
						bmExt.getMarkerTypeLiteral(referenceTy.isConst()),
			            bmExt.getMarkerTypeLiteral(referenceTy.isVolatile()),
						builder.getTypeLiteral(lang::toType(refExpr->getNodeManager(), referenceTy.getKind())));
	}

	ExpressionPtr buildRefKindCast(const ExpressionPtr& refExpr, ReferenceType::Kind newKind) {
		assert_pred1(isReference, refExpr) << "Trying to build a ref kind cast from non-ref.";
		auto rT = ReferenceType(refExpr);
		if(rT.getKind() == newKind) return refExpr;
		IRBuilder builder(refExpr->getNodeManager());
		auto& rExt = refExpr->getNodeManager().getLangExtension<ReferenceExtension>();
		return builder.callExpr(rExt.getRefKindCast(), refExpr, builder.getTypeLiteral(lang::toType(refExpr->getNodeManager(), newKind)));
	}

	ExpressionPtr buildRefParentCast(const ExpressionPtr& refExpr, const TypePtr& targetTy) {
		assert_pred1(isReference, refExpr) << "Trying to build a ref parent cast from non-ref.";
		auto rT = ReferenceType(refExpr);
		if(rT.getElementType() == targetTy || core::types::isMatchable(rT.getElementType(), targetTy)) return refExpr;
		IRBuilder builder(refExpr->getNodeManager());
		auto& rExt = refExpr->getNodeManager().getLangExtension<ReferenceExtension>();
		return builder.callExpr(rExt.getRefParentCast(), refExpr, builder.getTypeLiteral(targetTy));
	}

	ExpressionPtr buildRefReinterpret(const ExpressionPtr& refExpr, const TypePtr& targetTy) {
		assert_pred1(isReference, refExpr) << "Trying to build a ref reinterpret cast from non-ref.";
		auto rT = ReferenceType(refExpr);
		if(rT.getElementType() == targetTy || core::types::isMatchable(rT.getElementType(), targetTy)) return refExpr;
		IRBuilder builder(refExpr->getNodeManager());
		auto& rExt = refExpr->getNodeManager().getLangExtension<ReferenceExtension>();
		return builder.callExpr(rExt.getRefReinterpret(), refExpr, builder.getTypeLiteral(targetTy));
	}


	bool isAnyRefCast(const NodePtr& node) {
		auto expr = node.isa<ExpressionPtr>();
		if(!expr) return false;
		auto& refExt = expr->getNodeManager().getLangExtension<lang::ReferenceExtension>();
		auto funExpr = expr;
		if(auto call = expr.isa<CallExprPtr>()) funExpr = call->getFunctionExpr();
		return refExt.isRefCast(funExpr) || refExt.isRefConstCast(funExpr) || refExt.isRefVolatileCast(funExpr) || refExt.isRefKindCast(funExpr)
			   || refExt.isRefReinterpret(funExpr) || refExt.isRefParentCast(funExpr);
	}

	ExpressionPtr removeSurroundingRefCasts(const ExpressionPtr& node) {
		auto ret = node;
		while(isAnyRefCast(ret)) ret = core::transform::extractArg(ret, 0);
		return ret;
	}

	ExpressionPtr buildRefTemp(const TypePtr& type) {
		IRBuilder builder(type->getNodeManager());
		auto& refExt = type->getNodeManager().getLangExtension<lang::ReferenceExtension>();
		if(isReference(type)) {
			auto elementType = ReferenceType(type).getElementType();
			return buildRefCast(builder.callExpr(builder.refType(elementType), refExt.getRefTemp(), builder.getTypeLiteral(elementType)), type);
		}
		return builder.callExpr(builder.refType(type), refExt.getRefTemp(), builder.getTypeLiteral(type));
	}

	ExpressionPtr buildRefNull(const TypePtr& type) {
		assert_pred1(isReference, type) << "Trying to build a null ref which isn't a reference.";
		IRBuilder builder(type->getNodeManager());
		auto& rExt = type->getNodeManager().getLangExtension<ReferenceExtension>();
		auto& bmExt = type->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		ReferenceType rt(type);
		return builder.callExpr(rExt.getRefNull(), builder.getTypeLiteral(rt.getElementType()), bmExt.getMarkerTypeLiteral(rt.isConst()),
								bmExt.getMarkerTypeLiteral(rt.isVolatile()));
	}

	ExpressionPtr buildRefDecl(const TypePtr& type) {
		assert_pred1(isReference, type) << "Trying to build a decl ref which isn't a reference.";
		IRBuilder builder(type->getNodeManager());
		auto& refExt = type->getNodeManager().getLangExtension<lang::ReferenceExtension>();
		return builder.callExpr(type, refExt.getRefDecl(), builder.getTypeLiteral(type));
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
