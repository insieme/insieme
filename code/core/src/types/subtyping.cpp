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

#include "insieme/core/types/subtyping.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/analysis/ir++_utils.h"

namespace insieme {
namespace core {
namespace types {


// -------------------------------------------------------------------------------------------------------------------------
//                                                    SubTyping
// -------------------------------------------------------------------------------------------------------------------------

namespace {

	void addParents(const ParentsPtr& parents, TypeSet& res) {
		for(auto cur : parents) res.insert(cur->getType());
	}

	void addParents(const TypePtr& type, TypeSet& res) {
		if (StructTypePtr cur = type.isa<StructTypePtr>()) addParents(cur->getParents(), res);
		if (GenericTypePtr cur = type.isa<GenericTypePtr>()) addParents(cur->getParents(), res);
	}

	const TypeSet getSuperTypes(const TypePtr& type) {
		TypeSet res = type->getNodeManager().getLangBasic().getDirectSuperTypesOf(type);
		addParents(type, res);
		return res;
	}

	const TypeSet getSubTypes(const TypePtr& type) {
		return type->getNodeManager().getLangBasic().getDirectSubTypesOf(type);
	}

	template<typename Extractor>
	inline TypeSet getAllFor(const TypeSet& set, const Extractor& src) {
		TypeSet res;
		for_each(set, [&](const TypePtr& cur) {
			utils::set::insertAll(res, src(cur));
		});
		return res;
	}

	bool isSubTypeOfInternal(const TypePtr& subType, const TypePtr& superType) {
		// start from the sub-type and work toward the top.
		// As soon as the super type is included, the procedure can stop.

		// compute the closure using the delta algorithm
		TypeSet delta = utils::set::toSet<TypeSet>(subType);
		TypeSet superTypes = delta;
		while (!utils::set::contains(superTypes, superType) && !delta.empty()) {
			// get super-types of delta types
			delta = getAllFor(delta, &getSuperTypes);
			utils::set::insertAll(superTypes, delta);
		}

		// check whether the given super type is within the closure
		return utils::set::contains(superTypes, superType);
	}

	template<typename Extractor>
	TypePtr getJoinMeetType(const GenericTypePtr& typeA, const GenericTypePtr& typeB, const Extractor& extract) {
		// from both sides, the sets of super-types are computed step by step
		// when the sets are intersecting, the JOIN type has been found

		// super-type closure is computed using the delta algorithm
		TypeSet deltaA = utils::set::toSet<TypeSet>(typeA);
		TypeSet deltaB = utils::set::toSet<TypeSet>(typeB);
		TypeSet superTypesA = deltaA;
		TypeSet superTypesB = deltaB;
		TypeSet intersect = utils::set::intersect(superTypesA, superTypesB);
		while(intersect.empty() && !(deltaA.empty() && deltaB.empty())) {

			// get super-types of delta types
			deltaA = extract(deltaA);
			deltaB = extract(deltaB);
			utils::set::insertAll(superTypesA, deltaA);
			utils::set::insertAll(superTypesB, deltaB);
			intersect = utils::set::intersect(superTypesA, superTypesB);
		}

		// check result
		assert(intersect.size() <= static_cast<std::size_t>(1) && "More than one JOIN type detected!");

		// use result
		if (intersect.empty()) {
			// no Join type detected
			return TypePtr();
		}
		return *intersect.begin();
	}

	TypePtr getJoinType(const GenericTypePtr& typeA, const GenericTypePtr& typeB) {
		return getJoinMeetType(typeA, typeB, [](const TypeSet& set) {
			return getAllFor(set, &getSuperTypes);
		});
	}

	TypePtr getMeetType(const GenericTypePtr& typeA, const GenericTypePtr& typeB) {
		return getJoinMeetType(typeA, typeB, [](const TypeSet& set) {
			return getAllFor(set, &getSubTypes);
		});
	}
}

bool isSubTypeOf(const TypePtr& subType, const TypePtr& superType) {

	// quick check - reflexivity
	if (*subType == *superType) {
		return true;
	}

	// check for recursive types
	if (subType->getNodeType() == NT_RecType || superType->getNodeType() == NT_RecType) {

		// if both are they have to be equivalent
		if (subType->getNodeType() == NT_RecType && superType->getNodeType() == NT_RecType) {
			return subType == superType;
		}

		// if only one is, it needs to be unrolled
		if (subType->getNodeType() == NT_RecType) {
			assert(superType->getNodeType() != NT_RecType);
			return isSubTypeOf(subType.as<RecTypePtr>()->unroll(), superType);
		}

		if (superType->getNodeType() == NT_RecType) {
			assert(subType->getNodeType() != NT_RecType);
			return isSubTypeOf(subType, superType.as<RecTypePtr>()->unroll());
		}

		assert(false && "How could you get here?");
	}

	// check whether the sub-type is generic
	if (subType->getNodeType() == NT_GenericType || subType->getNodeType() == NT_StructType) {
		// use the delta algorithm for computing all the super-types of the given sub-type
		return isSubTypeOfInternal(subType, superType);
	}

	// check for vector types
	if (subType.isa<VectorTypePtr>() ) {
		VectorTypePtr vector = static_pointer_cast<const VectorType>(subType);
		// potential super type is an array of the same element type
		IRBuilder builder(vector->getNodeManager());
		return *superType == *builder.arrayType(vector->getElementType());
	}

	// for all other relations, the node type has to be the same
	if (subType->getNodeType() != superType->getNodeType()) {
		return false;
	}

	// check function types
	if (subType->getNodeType() == NT_FunctionType) {
		FunctionTypePtr funTypeA = static_pointer_cast<const FunctionType>(subType);
		FunctionTypePtr funTypeB = static_pointer_cast<const FunctionType>(superType);

		// check kind of functions
		if (funTypeA->getKind() != funTypeB->getKind()) {
			// only closure to plain conversion is allowed
			if (!(funTypeB->isClosure() && funTypeA->isPlain())) {
				return false;
			}
		}

		bool res = true;
		res = res && funTypeA->getParameterTypes().size() == funTypeB->getParameterTypes().size();
		res = res && isSubTypeOf(funTypeA->getReturnType(), funTypeB->getReturnType());
		for(std::size_t i = 0; res && i<funTypeB->getParameterTypes().size(); i++) {
			res = res && isSubTypeOf(funTypeB->getParameterTypes()[i], funTypeA->getParameterTypes()[i]);
		}
		return res;
	}

	// check reference types
	if (subType->getNodeType() == NT_RefType) {
		const auto& basic = subType->getNodeManager().getLangBasic();

		auto srcElement = subType.as<RefTypePtr>()->getElementType();
		auto trgElement = superType.as<RefTypePtr>()->getElementType();

		// if sub-type is ref<any> it is ok
		if (basic.isAny(trgElement)) {
			return true;
		}

		// support nested references
		if (srcElement.isa<RefTypePtr>() && trgElement.isa<RefTypePtr>()) {
			return isSubTypeOf(srcElement, trgElement);
		}

		// a ref<vector<X,Y>> is a sub-type of a ref<array<X,1>>
		if (trgElement.isa<ArrayTypePtr>() && srcElement.isa<VectorTypePtr>()) {
			IRBuilder builder(srcElement.getNodeManager());
			auto one = builder.concreteIntTypeParam(1);

			// array needs to be 1-dimensional and both have to have the same element type
			return trgElement.as<ArrayTypePtr>()->getDimension() == one &&
				   trgElement.as<ArrayTypePtr>()->getElementType() == srcElement.as<VectorTypePtr>()->getElementType();
		}

		// also support references of derived classes being passed to base-type pointer
		if (core::analysis::isObjectType(srcElement) && core::analysis::isObjectType(trgElement)) {
			if (isSubTypeOf(srcElement, trgElement)) {
				return true;
			}
		}
	}

	// no other relations are supported
	return false;
}

/**
 * Computes a join or meet type for the given pair of types. The join flag allows to determine
 * whether the join or meet type is computed.
 */
TypePtr getJoinMeetType(const TypePtr& typeA, const TypePtr& typeB, bool join) {
	static const TypePtr fail = 0;

	// add a structure based algorithm for computing the Join-Type

	// shortcut for equal types
	if (*typeA == *typeB) {
		return typeA;
	}

	// the rest depends on the node types
	NodeType nodeTypeA = typeA->getNodeType();
	NodeType nodeTypeB = typeB->getNodeType();

	// handle generic types
	if (nodeTypeA == NT_GenericType && nodeTypeB == NT_GenericType) {
		// let the join computation handle the case
		const GenericTypePtr& genTypeA = static_pointer_cast<const GenericType>(typeA);
		const GenericTypePtr& genTypeB = static_pointer_cast<const GenericType>(typeB);
		return (join) ? getJoinType(genTypeA, genTypeB) : getMeetType(genTypeA, genTypeB);
	}

	// handle vector types (only if array super type of A is a super type of B)

	// make sure typeA is the vector
	if (nodeTypeA != NT_VectorType && nodeTypeB == NT_VectorType) {
		// switch sides
		return getJoinMeetType(typeB, typeA, join);
	}

	// handle vector-array conversion (only works for joins)
	if (join && nodeTypeA == NT_VectorType) {
		VectorTypePtr vector = static_pointer_cast<const VectorType>(typeA);

		// the only potential super type is an array of the same element type
		IRBuilder builder(vector->getNodeManager());
		ArrayTypePtr array = builder.arrayType(vector->getElementType());
		if (isSubTypeOf(typeB, array)) {
			return array;
		}
		// no common super type!
		return fail;
	}

	// the rest can only work if it is of the same kind
	if (nodeTypeA != nodeTypeB) {
		// => no common super type
		return fail;
	}

	// check for functions
	if (nodeTypeA == NT_FunctionType) {
		FunctionTypePtr funTypeA = static_pointer_cast<const FunctionType>(typeA);
		FunctionTypePtr funTypeB = static_pointer_cast<const FunctionType>(typeB);

		// check number of arguments
		auto paramsA = funTypeA->getParameterTypes();
		auto paramsB = funTypeB->getParameterTypes();
		if (paramsA.size() != paramsB.size()) {
			// not matching
			return fail;
		}

		// check function kind
		FunctionKind resKind = funTypeA->getKind();
		if (funTypeA->getKind() != funTypeB->getKind()) {
			// differences are only allowed when going from plain to closure type
			if ((funTypeA->isPlain() && funTypeB->isClosure()) || (funTypeA->isClosure() && funTypeB->isPlain())) {
				resKind = FK_CLOSURE;
			} else {
				return fail;
			}
		}

		// compute join type
		// JOIN/MEET result and argument types - if possible
		TypePtr cur = getJoinMeetType(funTypeA->getReturnType(), funTypeB->getReturnType(), join);
		TypePtr resType = cur;

		// continue with parameters
		TypeList params;
		for (std::size_t i=0; i<paramsA.size(); i++) {
			// ATTENTION: this goes in the reverse direction
			cur = getJoinMeetType(paramsA[i], paramsB[i], !join);

			// if a pair can not be matched => fail
			if (!cur) return fail;

			params.push_back(cur);
		}

		// construct resulting type
		IRBuilder builder(funTypeA->getNodeManager());
		return builder.functionType(params, resType, resKind);
	}

	// everything else does not have a common join/meet type
	return fail;
}


TypePtr getSmallestCommonSuperType(const TypePtr& typeA, const TypePtr& typeB) {
	// use common implementation for Join and Meet type computation
	return getJoinMeetType(typeA, typeB, true);
}

TypePtr getBiggestCommonSubType(const TypePtr& typeA, const TypePtr& typeB) {
	// use common implementation for Join and Meet type computation
	return getJoinMeetType(typeA, typeB, false);
}

} // end namespace types
} // end namespace core
} // end namespace insieme
