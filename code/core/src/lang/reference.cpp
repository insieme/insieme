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

#include "insieme/core/lang/reference.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/lang/boolean_marker.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/types/match.h"

namespace insieme {
namespace core {
namespace lang {


	ReferenceType::ReferenceType(const NodePtr& node) {
		// check given node type
		assert_true(node) << "Given node is null!";
		assert_true(isReference(node)) << "Given node " << *node << " is not a reference type!";

		// extract the type
		GenericTypePtr type = node.isa<GenericTypePtr>();
		if (auto expr = node.isa<ExpressionPtr>()) type = expr->getType().as<GenericTypePtr>();

		// initialize the local instance
		*this = ReferenceType(type->getTypeParameter(0), isTrueMarker(type->getTypeParameter(1)), isTrueMarker(type->getTypeParameter(2)));
	}

	GenericTypePtr ReferenceType::create(const TypePtr& elementType, bool _const, bool _volatile) {
		assert_true(elementType);
		return ReferenceType(elementType, _const, _volatile).operator GenericTypePtr();
	}

	ReferenceType::operator GenericTypePtr() const {
		NodeManager& mgr = elementType.getNodeManager();
		auto& ext = mgr.getLangExtension<BooleanMarkerExtension>();

		return GenericType::get(mgr, "ref", ParentList(),
		                        toVector(elementType, (mConst ? ext.getTrue() : ext.getFalse()), (mVolatile ? ext.getTrue() : ext.getFalse())));
	}


	bool isReference(const NodePtr& node) {

		// check for null
		if (!node) return false;

		// check for expressions
		if (auto expr = node.isa<ExpressionPtr>()) return isReference(expr->getType());

		// check type
		auto type = node.isa<GenericTypePtr>();
		if(!type) return false;

		// simple approach: use unification
		NodeManager& mgr = node.getNodeManager();
		const ReferenceExtension& ext = mgr.getLangExtension<ReferenceExtension>();

		// unify given type with template type
		auto ref = ext.getGenRef().as<GenericTypePtr>();
		auto sub = types::match(mgr, type, ref);
		if(!sub) return false;

		// check instantiation
		const types::Substitution& map = *sub;
		return isValidBooleanMarker(map.applyTo(ref->getTypeParameter(1))) && isValidBooleanMarker(map.applyTo(ref->getTypeParameter(2)));
	}
	
	TypePtr buildRefType(const TypePtr& elementType, bool _const, bool _volatile) {
		return ReferenceType::create(elementType, _const, _volatile);
	}


	bool doReferencesDifferOnlyInQualifiers(const TypePtr& typeA, const TypePtr& typeB) {
		assert_true(isReference(typeA)) << "Can only check qualifiers on references, not on " << dumpColor(typeA);
		assert_true(isReference(typeB)) << "Can only check qualifiers on references, not on " << dumpColor(typeB);
		
		auto gA = typeA.as<GenericTypePtr>();
		auto gB = typeB.as<GenericTypePtr>();

		return gA->getTypeParameter(0) == gB->getTypeParameter(0)
			   && (gA->getTypeParameter(1) != gB->getTypeParameter(1) || gA->getTypeParameter(2) != gB->getTypeParameter(2));
	}
	
	ExpressionPtr buildRefCast(const ExpressionPtr& refExpr, const TypePtr& targetTy) {
		assert_pred1(isReference, refExpr) << "Trying to build a ref cast from non-ref.";
		assert_pred1(isReference, targetTy) << "Trying to build a ref cast to non-ref type.";
		if(targetTy == refExpr->getType()) return refExpr;
		assert_true(doReferencesDifferOnlyInQualifiers(refExpr->getType(), targetTy)) << "Ref cast only allowed to cast between qualifiers,"
			<< "trying to cast from\n" << dumpColor(refExpr->getType()) << " - to - \n" << dumpColor(targetTy);
		IRBuilder builder(refExpr->getNodeManager());
		auto& rExt = refExpr->getNodeManager().getLangExtension<ReferenceExtension>();
		auto& bmExt = refExpr->getNodeManager().getLangExtension<BooleanMarkerExtension>();
		ReferenceType referenceTy(targetTy);
		return builder.callExpr(rExt.getRefCast(), refExpr, bmExt.getMarkerTypeLiteral(referenceTy.isConst()),
			                    bmExt.getMarkerTypeLiteral(referenceTy.isVolatile()));
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

} // end namespace lang
} // end namespace core
} // end namespace insieme
