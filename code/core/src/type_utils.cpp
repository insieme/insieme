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
		: NodeMapping(!substitution.getIntTypeParamMapping().empty()), manager(manager),
		  mapping(substitution.getMapping()), paramMapping(substitution.getIntTypeParamMapping()) {};

	/**
	 * The procedure mapping a node to its substitution.
	 *
	 * @param element the node to be resolved
	 */
	const NodePtr mapElement(unsigned, const NodePtr& element) {
		// quick check - only variables are substituted
		if (element->getNodeType() != NT_TypeVariable) {
			return element->substitute(manager, *this);
		}

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

	/**
	 * The procedure mapping an int-type-parameter to its substitution.
	 *
	 * @param param the parameter to be resolved
	 */
	virtual IntTypeParam mapParam(const IntTypeParam& param) {
		if (param.getType() != IntTypeParam::VARIABLE) {
			return param;
		}
		auto pos = paramMapping.find(param);
		if (pos != paramMapping.end()) {
			return (*pos).second;
		}
		return param;
	}
};


}

Substitution::Substitution(const TypeVariablePtr& var, const TypePtr& type) {
	mapping.insert(std::make_pair(var, type));
};

Substitution::Substitution(const IntTypeParam& var, const IntTypeParam& type) {
	assert( var.getType()==IntTypeParam::VARIABLE && "Cannot add mapping for non-variable parameter!");
	paramMapping.insert(std::make_pair(var,type));
}

TypePtr Substitution::applyTo(NodeManager& manager, const TypePtr& type) const {
	// perform substitution
	SubstitutionMapper mapper(manager, *this);
	return mapper.map(0, type);
}

IntTypeParam Substitution::applyTo(const IntTypeParam& param) const {
	if (param.getType() != IntTypeParam::VARIABLE) {
		return param;
	}
	auto pos = paramMapping.find(param);
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

void Substitution::addMapping(const IntTypeParam& var, const IntTypeParam& value) {

	assert( var.getType()==IntTypeParam::VARIABLE && "Cannot add mapping for non-variable parameter!");

	auto element = std::make_pair(var,value);
	auto res = paramMapping.insert(element);
	if (!res.second) {
		paramMapping.erase(var);
		res = paramMapping.insert(element);
		assert( res.second && "Insert was not successful!" );
	}
}

void Substitution::remMappingOf(const TypeVariablePtr& var) {
	if (var->getNodeType() != NT_TypeVariable) {
		return;
	}
	mapping.erase(var);
}

void Substitution::remMappingOf(const IntTypeParam& var) {
	if (var.getType() != IntTypeParam::VARIABLE) {
		return;
	}
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
		const IntTypeParam& superSize = superType->getIntTypeParameter()[0];
		const IntTypeParam& subSize = subType->getIntTypeParameter()[0];

		// check whether sizes are comparable
		if (!(superSize.isConcrete() && subSize.isConcrete())) {
			return false;
		}

		// get sign status
		bool superSigned = basic.isSignedInt(superType);
		bool subSigned = basic.isSignedInt(subType);

		// if same sized => just compare size
		if (superSigned == subSigned) {
			return subSize < superSize || subSize == superSize;
		}

		// sign is different => depends on super type
		if (superSigned) {
			// size has to strictly less
			return subSize < superSize || superSize == IntTypeParam::INF;
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
			argument = ArrayType::get(manager, elementType, IntTypeParam::getConcreteIntParam(1));
			return;
		}
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
			if (typeOfA == NT_GenericType) {
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
					IntTypeParam paramA = paramsA[i];
					IntTypeParam paramB = paramsB[i];

					// equivalent pairs can be ignored ...
					if (paramA == paramB) {
						continue;
					}

					// check for variables
					if (paramA.getType() != IntTypeParam::VARIABLE && paramB.getType() != IntTypeParam::VARIABLE) {
						// different constants => not matchable / unifyable!
						return unmatchable;
					}

					// move variable to first place
					if (paramA.getType() != IntTypeParam::VARIABLE && paramB.getType() ==IntTypeParam::VARIABLE) {
						if (!unify) {
							// => only allowed for unification
							return unmatchable;
						}

						// switch sides
						IntTypeParam tmp = paramA;
						paramA = paramB;
						paramB = tmp;
					}

					// add mapping
					Substitution mapping(paramA,paramB);

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
						list.push_back(std::make_pair(
								static_pointer_cast<const Type>(cur.first),
								static_pointer_cast<const Type>(cur.second)
						));
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
		std::set<IntTypeParam>& paramSet;

		// a container for "remembering" replacements
		utils::map::PointerMap<TypeVariablePtr, TypeVariablePtr> varMap;
		std::map<IntTypeParam, IntTypeParam> paramMap;

		// some utilities for generating variables
		unsigned varCounter;
		unsigned paramCounter;

	public:

		FreshVariableSubstitution(NodeManager& manager, TypeSet& varSet, std::set<IntTypeParam>& paramSet)
			: NodeMapping(true), manager(manager), varSet(varSet), paramSet(paramSet), varCounter(0), paramCounter(0) {};

		virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
			// only handle type variables
			if (ptr->getNodeType() != NT_TypeVariable) {
				return ptr->substitute(manager, *this);
			}

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

		virtual IntTypeParam mapParam(const IntTypeParam& param) {
			// only variables need to be considered
			if (param.getType() != IntTypeParam::VARIABLE) {
				return param;
			}

			// search for parameter ...
			auto pos = paramMap.find(param);
			if (pos != paramMap.end()) {
				// found => return result
				return pos->second;
			}

			// create fresh parameter ...
			IntTypeParam res = getFreshParam();
			paramMap.insert(std::make_pair(param, res));
			paramSet.insert(res);
			return res;
		}

	private:

		TypeVariablePtr getFreshVar() {
			TypeVariablePtr res;
			do {
				res = TypeVariable::get(manager, "V" + toString(++varCounter));
			} while(varSet.find(res) != varSet.end());
			return res;
		}

		IntTypeParam getFreshParam() {
			IntTypeParam res;
			do {
				res = IntTypeParam::getVariableIntParam('a' + (paramCounter++));
			} while (paramSet.find(res) != paramSet.end());
			return res;
		}

	};


	void collectAllTypeVariables(const TypeList& types, TypeSet& varSet, std::set<IntTypeParam>& paramSet) {

		// assemble type-variable collecting visitor
		auto visitor = makeLambdaPtrVisitor([&](const NodePtr& cur) {
			// collect all type variables
			if (cur->getNodeType() == NT_TypeVariable) {
				varSet.insert(static_pointer_cast<const Type>(cur));
			}

			// collect variable int-type parameters
			if (cur->getNodeType() == NT_GenericType) {
				const GenericTypePtr& type = static_pointer_cast<const GenericType>(cur);
				for_each(type->getIntTypeParameter(), [&](const IntTypeParam& cur) {
					if (cur.getType() == IntTypeParam::VARIABLE) {
						paramSet.insert(cur);
					}
				});
			}

		}, true);

		// collect type variables
		for_each(types, [&](const TypePtr& cur) {
			visitAllOnce(cur, visitor);
		});

	}

	template<typename T>
	Pointer<T> makeTypeVariablesUnique(const Pointer<T>& target, TypeSet& usedTypes, std::set<IntTypeParam>& paramSet) {
		NodeManager& manager = target->getNodeManager();
		FreshVariableSubstitution mapper(manager, usedTypes, paramSet);
		return static_pointer_cast<T>(target->substitute(manager, mapper));
	}

}

TypePtr deduceReturnType(FunctionTypePtr funType, TypeList argumentTypes) {

	NodeManager& manager = funType->getNodeManager();

	// make type variables within function types unique
	TypeSet usedVars;
	std::set<IntTypeParam> usedParams;
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



} // end namespace core
} // end namespace insieme


std::ostream& operator<<(std::ostream& out, const insieme::core::Substitution& substitution) {
	out << "{";
	out << join(",", substitution.getMapping(), [](std::ostream& out, const insieme::core::Substitution::Mapping::value_type& cur)->std::ostream& {
		return out << *cur.first << "->" << *cur.second;
	});
	out << "/";
	out << join(",", substitution.getIntTypeParamMapping(), [](std::ostream& out, const insieme::core::Substitution::IntTypeParamMapping::value_type& cur)->std::ostream& {
		return out << cur.first.toString() << "->" << cur.second.toString();
	});
	out << "}";
	return out;
}

