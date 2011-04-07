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

#include <cassert>

#include <boost/unordered_map.hpp>

#include "insieme/core/type_utils.h"

#include "insieme/core/ast_builder.h"
#include "insieme/core/ast_visitor.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {

namespace {

/**
 * This class provides a wrapper for a substitution to be applied to some type. This
 * wrapper is based on a node mapping, which allows this class to exploit the general node mapping
 * mechanism to perform
 */
class SubstitutionMapper : public NodeMapping {

	/**
	 * The node manager to be used for creating new type nodes.
	 */
	NodeManager& manager;

	/**
	 * The type variable mapping constituting the substitution to be wrapped by this instance.
	 */
	const Substitution::Mapping& mapping;

	/**
	 * The int type parameter mapping of the substitution to be wrapped by this instance.
	 */
	const Substitution::IntTypeParamMapping& paramMapping;

public:

	/**
	 * Creates a new instance of this class wrapping the given substitution.
	 *
	 * @param manager the node manager to be used for creating new node instances if necessary
	 * @param substitution the substitution to be wrapped by the resulting instance.
	 */
	SubstitutionMapper(NodeManager& manager, const Substitution& substitution)
		: NodeMapping(), manager(manager), mapping(substitution.getMapping()),
		  paramMapping(substitution.getIntTypeParamMapping()) {};

	/**
	 * The procedure mapping a node to its substitution.
	 *
	 * @param element the node to be resolved
	 */
	const NodePtr mapElement(unsigned, const NodePtr& element) {
		// quick check - only variables are substituted
		auto currentType = element->getNodeType();
		if (currentType != NT_TypeVariable && currentType != NT_VariableIntTypeParam) {
			return element->substitute(manager, *this);
		}

		switch (currentType) {

			case NT_TypeVariable: {
				// lookup current variable within the mapping
				auto pos = mapping.find(static_pointer_cast<const TypeVariable>(element));
				if (pos != mapping.end()) {
					// found! => replace
					return (*pos).second;
				}

				// not found => return current node
				// (since nothing within a variable node may be substituted)
				return element;
			}
			case NT_VariableIntTypeParam: {
				// lookup int type param variable ...
				auto pos = paramMapping.find(static_pointer_cast<const VariableIntTypeParam>(element));
				if (pos != paramMapping.end()) {
					// found! => replace
					return (*pos).second;
				}
				// not found => return current node
				return element;
			}

			default:
				assert(false && "Only type and parameter variables should reach this point!");
		}

		//should never be reached
		assert(false && "This point shouldn't be reachable!");
		return element;
	}

};


}

Substitution::Substitution(const TypeVariablePtr& var, const TypePtr& type) {
	mapping.insert(std::make_pair(var, type));
};

Substitution::Substitution(const VariableIntTypeParamPtr& var, const IntTypeParamPtr& param) {
	paramMapping.insert(std::make_pair(var, param));
}

TypePtr Substitution::applyTo(NodeManager& manager, const TypePtr& type) const {
	// perform substitution
	SubstitutionMapper mapper(manager, *this);
	return mapper.map(0, type);
}

IntTypeParamPtr Substitution::applyTo(const IntTypeParamPtr& param) const {
	if (param->getNodeType() != core::NT_VariableIntTypeParam) {
		return param;
	}
	auto pos = paramMapping.find(static_pointer_cast<const VariableIntTypeParam>(param));
	if (pos == paramMapping.end()) {
		return param;
	}
	return (*pos).second;
}


void Substitution::addMapping(const TypeVariablePtr& var, const TypePtr& type) {
	auto element = std::make_pair(var,type);
	auto res = mapping.insert(element);
	if (!res.second) {
		mapping.erase(var);
		res = mapping.insert(element);
		assert( res.second && "Insert was not successful!" );
	}
}

void Substitution::addMapping(const VariableIntTypeParamPtr& var, const IntTypeParamPtr& value) {

	auto element = std::make_pair(var,value);
	auto res = paramMapping.insert(element);
	if (!res.second) {
		paramMapping.erase(var);
		res = paramMapping.insert(element);
		assert( res.second && "Insert was not successful!" );
	}
}

void Substitution::remMappingOf(const TypeVariablePtr& var) {
	mapping.erase(var);
}

void Substitution::remMappingOf(const VariableIntTypeParamPtr& var) {
	paramMapping.erase(var);
}


Substitution Substitution::compose(NodeManager& manager, const Substitution& a, const Substitution& b) {

	typedef Substitution::Mapping::value_type Entry;
	typedef Substitution::IntTypeParamMapping::value_type ParamEntry;

	// copy substitution a
	Substitution res(a);

	// --- normal types ---

	// apply substitution b to all mappings in a
	for_each(res.mapping, [&manager, &b](Entry& cur) {
		cur.second = b.applyTo(manager, cur.second);
	});

	// add remaining mappings of b
	Substitution::Mapping& resMapping = res.mapping;
	for_each(b.mapping, [&resMapping](const Entry& cur) {
		if (resMapping.find(cur.first) == resMapping.end()) {
			resMapping.insert(cur);
		}
	});

	// --- int type parameter ---

	// apply substitution b to all mappings in a
	for_each(res.paramMapping, [&manager, &b](ParamEntry& cur) {
		cur.second = b.applyTo(cur.second);
	});

	// add remaining mappings of b
	Substitution::IntTypeParamMapping& resParamMapping = res.paramMapping;
	for_each(b.paramMapping, [&resParamMapping](const ParamEntry& cur) {
		if (resParamMapping.find(cur.first) == resParamMapping.end()) {
			resParamMapping.insert(cur);
		}
	});

	return res;
}


bool occurs(const NodePtr& x, const NodePtr& term) {
	// it occurs if it is the current node or one of its child contains the node
	return *x==*term || any(term->getChildList(), [&x](const NodePtr& cur) {
		return occurs(x, cur);
	});
}

namespace {


	/**
	 * Checks whether the given super type is a subType is a sub type of the given super type.
	 *
	 * @param subType the type to be checked
	 * @param superType the type to be checked against
	 * @return true if subType is a sub-type of superType
	 */
	inline bool isSubtypeOf(const GenericTypePtr& subType, const GenericTypePtr& superType) {
		const lang::BasicGenerator& basic = superType->getNodeManager().basic;

		// start by ensuring both types are integer types
		if (!(basic.isInt(superType) && basic.isInt(subType))) {
			return false;
		}

		// get int type parameter
		const IntTypeParamPtr& superSize = superType->getIntTypeParameter()[0];
		const IntTypeParamPtr& subSize = subType->getIntTypeParameter()[0];

		// check whether sizes are comparable
		if (superSize->getNodeType() == NT_VariableIntTypeParam || subSize->getNodeType() == NT_VariableIntTypeParam) {
			return false;
		}

		// get sign status
		bool superSigned = basic.isSignedInt(superType);
		bool subSigned = basic.isSignedInt(subType);

		// if same sized => just compare size
		if (superSigned == subSigned) {
			return *subSize < *superSize || *subSize == *superSize;
		}

		// sign is different => depends on super type
		if (superSigned) {
			// size has to strictly less
			return *subSize < *superSize || superSize->getNodeType()==NT_InfiniteIntTypeParam;
		}

		// sign is different and super is unsigned
		return false; // => never works
	}


	/**
	 * Pre-processes types to handle vector/array sub-type relations. Basically, when passing
	 * an vector to a parameter requesting an array, the vector will be converted into an array.
	 */
	inline void preprocessTypes(NodeManager& manager, TypePtr& parameter, TypePtr& argument) {

		// check node types
		NodeType a = parameter->getNodeType();
		NodeType b = argument->getNodeType();

		// if only the parameter is a vector, replace the vector with an array ...
		if (a == NT_ArrayType && b == NT_VectorType) {
			TypePtr elementType = static_pointer_cast<const VectorType>(argument)->getElementType();
			argument = ArrayType::get(manager, elementType, ConcreteIntTypeParam::get(manager, 1));
			return;
		}
	}

	/**
	 * Tests whether the given type int type parameter is a variable int type parameters.
	 *
	 * @param param the instances to be tested
	 * @return true if param is a variable, false otherwise
	 */
	inline bool isVariable(const IntTypeParamPtr& param) {
		return param->getNodeType() == NT_VariableIntTypeParam;
	}

	/**
	 * This method implements the actual algorithm for computing a type variable substitution to match/unify a list of types.
	 *
	 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
	 * @param list the list of pairs of types to be matched / unified. In case matching should be applied,
	 * 				the first component has to be the pattern. The list will be altered during the computation.
	 * @param unify a boolean flag to distinguish between matching and unifying the given types
	 * @param considerSubTypes allows to enable / disable the consideration of sub-type relations
	 * @return the resulting (most general) unifier or an uninitialized value if the types couldn't be matched / unified.
	 */
	boost::optional<Substitution> computeSubstitution(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list, bool unify, bool considerSubtypes) {
		typedef std::pair<TypePtr, TypePtr> Pair;
		typedef std::list<Pair> List;

		// check input parameter
		assert( !(unify && considerSubtypes) && "Cannot perform unification with subtypes!");

		// create result
		Substitution res;
		boost::optional<Substitution> unmatchable;

		while(!list.empty()) {

			// get current element
			Pair cur = list.front();
			list.pop_front();

			TypePtr a = cur.first;
			TypePtr b = cur.second;

			if (considerSubtypes) {
				// preprocess types (e.g. handle vector - array relation)
				preprocessTypes(manager, a, b);
			}

			const NodeType typeOfA = a->getNodeType();
			const NodeType typeOfB = b->getNodeType();

			// 1) if (a == b) ignore pair ..
			if (*a==*b) {
				continue;
			}

			// 2) test whether on the B side there is a variable
			if (typeOfA != NT_TypeVariable && typeOfB == NT_TypeVariable) {
				if (unify) {
					// unification: no problem => swap sides
					// add swapped pair to front of list
					list.push_front(std::make_pair(b, a));
					continue;
				} else {
					// matching => cannot be matched
					return unmatchable;
				}
			}

			// 3) handle variables on left hand side ...
			if (typeOfA == NT_TypeVariable) {
				if (occurs(a, b)) {
					// not unifiable (e.g. X = type<X> cannot be unified)
					return unmatchable;
				}

				// convert current pair into substitution
				Substitution mapping(
						static_pointer_cast<const TypeVariable>(a),
						static_pointer_cast<const Type>(b)
				);

				// apply substitution to remaining pairs
				for_each(list, [&mapping, &manager](Pair& cur) {
					cur.first = mapping.applyTo(manager, cur.first);
					cur.second = mapping.applyTo(manager, cur.second);
				});

				// combine current mapping with overall result
				res = Substitution::compose(manager, res, mapping);
				continue;
			}

			// 4) function types / generic types / tuples / structs / unions / recursive types
			if (typeOfA != typeOfB) {
				// => not unifiable
				return unmatchable;
			}

			// handle recursive types (special treatment)
			if (typeOfA == NT_RecType) {
				// TODO: implement
				assert ("RECURSIVE TYPE SUPPORT NOT IMPLEMENTED!");
			}

			// handle function types (special treatment)
			if (!unify && typeOfA == NT_FunctionType) {
				// TODO: this is kind of a hack - might work, might not ... if not => redo unification properly
				// matching functions is kind of difficult since variables are universal quantified
				// => use unification from here on ...
				std::list<std::pair<TypePtr, TypePtr>> tmpList;
				tmpList.push_front(std::make_pair(a, b));
				auto mapping = computeSubstitution(manager, tmpList, true, false);
				if (!mapping) {
					return unmatchable;
				}

				// apply substitution to remaining pairs
				for_each(list, [&mapping, &manager](Pair& cur) {
					cur.first = mapping->applyTo(manager, cur.first);
					cur.second = mapping->applyTo(manager, cur.second);
				});

				// combine current mapping with overall result
				res = Substitution::compose(manager, res, *mapping);
				continue;
			}

			// => check family of generic type
			if (dynamic_pointer_cast<const GenericType>(a)) {
				GenericTypePtr genericTypeA = static_pointer_cast<const GenericType>(a);
				GenericTypePtr genericTypeB = static_pointer_cast<const GenericType>(b);

				// handle sub-types
				if (considerSubtypes && isSubtypeOf(genericTypeB, genericTypeA)) {
					// "upgrade" B-side
					genericTypeB = genericTypeA;
				}

				// check family names
				if (genericTypeA->getFamilyName() != genericTypeB->getFamilyName()) {
					return unmatchable;
				}

				// check same number of type parameters
				if (genericTypeA->getTypeParameter().size() != genericTypeB->getTypeParameter().size()) {
					return unmatchable;
				}

				// ---- unify int type parameter ---

				// get lists
				auto paramsA = genericTypeA->getIntTypeParameter();
				auto paramsB = genericTypeB->getIntTypeParameter();

				// check number of arguments ...
				if (paramsA.size() != paramsB.size()) {
					// => not unifyable
					return unmatchable;
				}

				for(std::size_t i=0; i<paramsA.size(); i++) {
					IntTypeParamPtr paramA = paramsA[i];
					IntTypeParamPtr paramB = paramsB[i];

					// equivalent pairs can be ignored ...
					if (paramA == paramB) {
						continue;
					}

					// check for variables
					if (!isVariable(paramA) && !isVariable(paramB)) {
						// different constants => not matchable / unifyable!
						return unmatchable;
					}

					// move variable to first place
					if (!isVariable(paramA) && isVariable(paramB)) {
						if (!unify) {
							// => only allowed for unification
							return unmatchable;
						}

						// switch sides
						IntTypeParamPtr tmp = paramA;
						paramA = paramB;
						paramB = tmp;
					}

					// add mapping
					Substitution mapping(static_pointer_cast<const VariableIntTypeParam>(paramA),paramB);

					// apply substitution to remaining pairs
					for_each(list, [&mapping, &manager](Pair& cur) {
						cur.first = mapping.applyTo(manager, cur.first);
						cur.second = mapping.applyTo(manager, cur.second);
					});

					// combine current mapping with overall result
					res = Substitution::compose(manager, res, mapping);
				}
			}

			// => check all child nodes
			auto childrenA = a->getChildList();
			auto childrenB = b->getChildList();
			if (childrenA.size() != childrenB.size()) {
				// => not matchable / unifyable
				return unmatchable;
			}

			// add pairs of children to list to be processed
			std::for_each(
					make_paired_iterator(childrenA.begin(), childrenB.begin()),
					make_paired_iterator(childrenA.end(), childrenB.end()),

					[&list](const std::pair<NodePtr, NodePtr>& cur) {
						if (cur.first->getNodeCategory() == NC_Type) {
							list.push_back(std::make_pair(
									static_pointer_cast<const Type>(cur.first),
									static_pointer_cast<const Type>(cur.second)
							));
						} else {
							assert(cur.second->getNodeCategory() == cur.first->getNodeCategory()
									&& "Unexpected incompatible node pair!");
						}
			});
		}

		// done
		return boost::optional<Substitution>(res);
	}

}

// -------------------------------------------------------------------------------------------------------------------------
//                                                    Unification
// -------------------------------------------------------------------------------------------------------------------------


boost::optional<Substitution> unify(NodeManager& manager, const TypePtr& typeA, const TypePtr& typeB) {
	return unifyAll(manager, toVector<TypePtr>(typeA), toVector<TypePtr>(typeB));
}

boost::optional<Substitution> unifyAll(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list) {
	return computeSubstitution(manager, list, true, false);
}

bool isUnifyable(const TypePtr& typeA, const TypePtr& typeB) {
	if (typeA == typeB) {
		return true;
	}
	NodeManager tmp; // requires only temporary manager
	return unify(tmp, typeA, typeB);
}

// -------------------------------------------------------------------------------------------------------------------------
//                                                    Matching
// -------------------------------------------------------------------------------------------------------------------------


boost::optional<Substitution> match(NodeManager& manager, const TypePtr& pattern, const TypePtr& type, bool considerSubtypes) {
	return matchAll(manager, toVector(pattern), toVector(type));
}

boost::optional<Substitution> matchAll(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list, bool considerSubtypes) {
	return computeSubstitution(manager, list, false, true);
}

bool isMatching(const TypePtr& pattern, const TypePtr& type, bool considerSubtypes) {
	if (pattern == type) {
		return true;
	}
	NodeManager tmp; // requires only temporary manager
	return match(tmp, pattern, type);
}


// -------------------------------------------------------------------------------------------------------------------------
//                                                    Return type deduction
// -------------------------------------------------------------------------------------------------------------------------


namespace {

	class FreshVariableSubstitution : public NodeMapping {

		NodeManager& manager;

		// sets of used variables
		TypeSet& varSet;
		IntTypeParamSet& paramSet;

		// a container for "remembering" replacements
		utils::map::PointerMap<TypeVariablePtr, TypeVariablePtr> varMap;
		utils::map::PointerMap<VariableIntTypeParamPtr, VariableIntTypeParamPtr> paramMap;

		// some utilities for generating variables
		unsigned varCounter;
		unsigned paramCounter;

	public:

		FreshVariableSubstitution(NodeManager& manager, TypeSet& varSet, IntTypeParamSet& paramSet)
			: NodeMapping(), manager(manager), varSet(varSet), paramSet(paramSet), varCounter(0), paramCounter(0) {};

		virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
			// only handle type variables
			NodeType curType = ptr->getNodeType();
			if (curType != NT_TypeVariable && curType != NT_VariableIntTypeParam) {
				return ptr->substitute(manager, *this);
			}


			switch(curType) {
				case NT_TypeVariable: {

					// cast type variable
					TypeVariablePtr cur = static_pointer_cast<const TypeVariable>(ptr);

					// search for parameter ...
					auto pos = varMap.find(cur);
					if (pos != varMap.end()) {
						// found => return result
						return pos->second;
					}

					// create new variable substitution
					TypeVariablePtr res = getFreshVar();
					varMap.insert(std::make_pair(cur, res));
					varSet.insert(res);
					return res;
				}

				case NT_VariableIntTypeParam: {

					// cast int type parameter
					VariableIntTypeParamPtr cur = static_pointer_cast<const VariableIntTypeParam>(ptr);

					// search for parameter ...
					auto pos = paramMap.find(cur);
					if (pos != paramMap.end()) {
						// found => return result
						return pos->second;
					}

					// create fresh parameter ...
					VariableIntTypeParamPtr res = getFreshParam();
					paramMap.insert(std::make_pair(cur, res));
					paramSet.insert(res);
					return res;
				}
				default:
					assert(false && "Should be impossible to reach!");
			}
			return ptr;
		}

	private:

		TypeVariablePtr getFreshVar() {
			TypeVariablePtr res;
			do {
				res = TypeVariable::get(manager, "V" + toString(++varCounter));
			} while(varSet.find(res) != varSet.end());
			return res;
		}

		VariableIntTypeParamPtr getFreshParam() {
			VariableIntTypeParamPtr res;
			do {
				res = VariableIntTypeParam::get(manager, 'a' + (paramCounter++));
			} while (paramSet.find(res) != paramSet.end());
			return res;
		}

	};


	void collectAllTypeVariables(const TypeList& types, TypeSet& varSet, IntTypeParamSet& paramSet) {

		// assemble type-variable collecting visitor
		auto visitor = makeLambdaPtrVisitor([&](const NodePtr& cur) {
			// collect all type variables
			if (cur->getNodeType() == NT_TypeVariable) {
				varSet.insert(static_pointer_cast<const Type>(cur));
			}

			// collect variable int-type parameters
			if (cur->getNodeType() == NT_VariableIntTypeParam) {
				paramSet.insert(static_pointer_cast<const VariableIntTypeParam>(cur));
			}

		}, true);

		// collect type variables
		for_each(types, [&](const TypePtr& cur) {
			visitAllOnce(cur, visitor);
		});

	}

	template<typename T>
	Pointer<T> makeTypeVariablesUnique(const Pointer<T>& target, TypeSet& usedTypes, IntTypeParamSet& paramSet) {
		NodeManager& manager = target->getNodeManager();
		FreshVariableSubstitution mapper(manager, usedTypes, paramSet);
		return static_pointer_cast<T>(target->substitute(manager, mapper));
	}

}

TypePtr deduceReturnType(FunctionTypePtr funType, TypeList argumentTypes) {

	NodeManager& manager = funType->getNodeManager();

	// make type variables within function types unique
	TypeSet usedVars;
	IntTypeParamSet usedParams;
	collectAllTypeVariables(argumentTypes, usedVars, usedParams);
	FunctionTypePtr modFunType = makeTypeVariablesUnique(funType, usedVars, usedParams);

	// try unifying the argument types
	auto mgu = matchAll(manager, modFunType->getArgumentTypes(), argumentTypes);

	// check whether unification was successful
	if (!mgu) {
		// return null-pointer
		LOG(WARNING) << "Unable to deduce return type for call to function of type "
				<< toString(*funType) << " using arguments " << join(", ", argumentTypes, print<deref<TypePtr>>());

		// return unit type
		return manager.basic.getUnit();
	}

	// extract return type
	const TypePtr& resType = modFunType->getReturnType();

	// compute and return the expected return type
	return mgu->applyTo(manager, resType);
}


// -------------------------------------------------------------------------------------------------------------------------
//                                                    SubTyping
// -------------------------------------------------------------------------------------------------------------------------

namespace {

	const TypeSet getSuperTypes(const TypePtr& type) {
		return type->getNodeManager().getBasicGenerator().getDirectSuperTypesOf(type);
	}

	const TypeSet getSubTypes(const TypePtr& type) {
		return type->getNodeManager().getBasicGenerator().getDirectSubTypesOf(type);
	}

	template<typename Extractor>
	inline TypeSet getAllFor(const TypeSet& set, const Extractor& src) {
		TypeSet res;
		for_each(set, [&](const TypePtr& cur) {
			utils::set::insertAll(res, src(cur));
		});
		return res;
	}

	bool isSubTypeOf(const GenericTypePtr& subType, const TypePtr& superType) {
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

	// check whether the sub-type is generic
	if (subType->getNodeType() == NT_GenericType) {
		// use the delta algorithm for computing all the super-types of the given sub-type
		return isSubTypeOf(static_pointer_cast<const GenericType>(subType), superType);
	}

	// check for vector types
	if (subType->getNodeType() == NT_VectorType) {
		VectorTypePtr vector = static_pointer_cast<const VectorType>(subType);

		// the only potential super type is an array of the same element type
		ASTBuilder builder(vector->getNodeManager());
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

		bool res = true;
		res = res && funTypeA->getArgumentTypes().size() == funTypeB->getArgumentTypes().size();
		res = res && isSubTypeOf(funTypeA->getReturnType(), funTypeB->getReturnType());
		for(std::size_t i = 0; res && i<funTypeB->getArgumentTypes().size(); i++) {
			res = res && isSubTypeOf(funTypeB->getArgumentTypes()[i], funTypeA->getArgumentTypes()[i]);
		}
		return res;
	}

	// no other relations are supported
	return false;
}

/**
 * Computes a join or meet type for the given pair of types. The join flag allows to determine
 * whether the join or meet type is computed.
 */
TypePtr getJoinMeetType(const TypePtr& typeA, const TypePtr& typeB, bool join) {

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
		ASTBuilder builder(vector->getNodeManager());
		ArrayTypePtr array = builder.arrayType(vector->getElementType());
		if (isSubTypeOf(typeB, array)) {
			return array;
		}
		// no common super type!
		return 0;
	}

	// the rest can only work if it is of the same kind
	if (nodeTypeA != nodeTypeB) {
		// => no common super type
		return 0;
	}

	// check for functions
	if (nodeTypeA == NT_FunctionType) {
		FunctionTypePtr funTypeA = static_pointer_cast<const FunctionType>(typeA);
		FunctionTypePtr funTypeB = static_pointer_cast<const FunctionType>(typeB);

		// check number of arguments
		auto argsA = funTypeA->getArgumentTypes();
		auto argsB = funTypeB->getArgumentTypes();
		if (argsA.size() != argsB.size()) {
			// not matching
			return 0;
		}

		// compute join type
		// JOIN/MEET result and argument types - if possible
		TypePtr cur = getJoinMeetType(funTypeA->getReturnType(), funTypeB->getReturnType(), join);
		TypePtr resType = cur;

		// continue with arguments
		TypeList args;
		for (std::size_t i=0; cur && i<argsA.size(); i++) {
			// ATTENTION: this goes in the reverse direction
			cur = getJoinMeetType(argsA[i], argsB[i], !join);
			args.push_back(cur);
		}

		// check whether for all pairs a match has been found
		if (!cur) {
			// no => no result
			return 0;
		}

		// construct resulting type
		ASTBuilder builder(funTypeA->getNodeManager());
		return builder.functionType(args, resType);
	}

	// everything else does not have a common join/meet type
	return 0;
}


TypePtr getSmallestCommonSuperType(const TypePtr& typeA, const TypePtr& typeB) {
	// use common implementation for Join and Meet type computation
	return getJoinMeetType(typeA, typeB, true);
}

TypePtr getBiggestCommonSubType(const TypePtr& typeA, const TypePtr& typeB) {
	// use common implementation for Join and Meet type computation
	return getJoinMeetType(typeA, typeB, false);
}


} // end namespace core
} // end namespace insieme


std::ostream& operator<<(std::ostream& out, const insieme::core::Substitution& substitution) {
	out << "{";
	out << join(",", substitution.getMapping(), [](std::ostream& out, const insieme::core::Substitution::Mapping::value_type& cur)->std::ostream& {
		return out << *cur.first << "->" << *cur.second;
	});

	if (!substitution.getMapping().empty() && !substitution.getIntTypeParamMapping().empty()) out << "/";
	out << join(",", substitution.getIntTypeParamMapping(), [](std::ostream& out, const insieme::core::Substitution::IntTypeParamMapping::value_type& cur)->std::ostream& {
		return out << *cur.first << "->" << *cur.second;
	});
	out << "}";
	return out;
}

