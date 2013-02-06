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

#include "insieme/core/type_utils.h"

#include <set>
#include <cassert>
#include <boost/unordered_map.hpp>

#include "insieme/core/analysis/type_variable_deduction.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"

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

bool Substitution::containsMappingFor(const TypeVariablePtr& var) const {
	return mapping.find(var) != mapping.end();
}

bool Substitution::containsMappingFor(const VariableIntTypeParamPtr& var) const {
	return paramMapping.find(var) != paramMapping.end();
}

void Substitution::remMappingOf(const TypeVariablePtr& var) {
	mapping.erase(var);
}

void Substitution::remMappingOf(const VariableIntTypeParamPtr& var) {
	paramMapping.erase(var);
}

std::ostream& Substitution::printTo(std::ostream& out) const {
	out << "{";
	out << join(",", mapping, [](std::ostream& out, const Mapping::value_type& cur)->std::ostream& {
		return out << *cur.first << "->" << *cur.second;
	});

	if (!mapping.empty() && !paramMapping.empty()) out << "/";
	out << join(",", paramMapping, [](std::ostream& out, const IntTypeParamMapping::value_type& cur)->std::ostream& {
		return out << *cur.first << "->" << *cur.second;
	});
	out << "}";
	return out;
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
	 * Tests whether the given type int type parameter is a variable int type parameters.
	 *
	 * @param param the instances to be tested
	 * @return true if param is a variable, false otherwise
	 */
	inline bool isVariable(const IntTypeParamPtr& param) {
		return param->getNodeType() == NT_VariableIntTypeParam;
	}


	/**
	 * Applies the given substitution to all types within the given list.
	 *
	 * @param manager the node manager to be used for creating new type instances if required
	 * @param mapping the substitution to be applied to the types within the list
	 * @param list the list to be updated by the substituted elements
	 */
	void applySubstitutionToList(NodeManager& manager, const Substitution& mapping, std::list<std::pair<TypePtr, TypePtr>>& list) {
		for_each(list, [&mapping, &manager](std::pair<TypePtr, TypePtr>& cur) {
			cur.first = mapping.applyTo(manager, cur.first);
			cur.second = mapping.applyTo(manager, cur.second);
		});
	};

	/**
	 * This method implements the actual algorithm for unifying a list of types.
	 *
	 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
	 * @param list the list of pairs of types to be unified. The list will be altered during the computation.
	 * @return the resulting (most general) unifier or an uninitialized value if the types couldn't be unified.
	 */
	SubstitutionOpt computeSubstitution(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list) {
		typedef std::pair<TypePtr, TypePtr> Pair;
		typedef std::list<Pair> List;

		// create result
		Substitution res;
		boost::optional<Substitution> unmatchable;

		while(!list.empty()) {

			// get current element
			Pair cur = list.front();
			list.pop_front();

			TypePtr a = cur.first;
			TypePtr b = cur.second;

			const NodeType typeOfA = a->getNodeType();
			const NodeType typeOfB = b->getNodeType();

			// 1) if (a == b) ignore pair ..
			if (*a==*b) {
				continue;
			}

			// 2) test whether on the B side there is a variable
			if (typeOfA != NT_TypeVariable && typeOfB == NT_TypeVariable) {
				// unification: no problem => swap sides
				// add swapped pair to front of list
				list.push_front(std::make_pair(b, a));
				continue;
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
				applySubstitutionToList(manager, mapping, list);

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


			// => check ref type
			if (typeOfA == NT_RefType) {
				const RefTypePtr& refTypeA = static_pointer_cast<RefTypePtr>(a);
				const RefTypePtr& refTypeB = static_pointer_cast<RefTypePtr>(b);

				// unify element type
				list.push_back(std::make_pair(refTypeA->getElementType(), refTypeB->getElementType()));
				continue;
			}

			// => handle single-element type cases
			if (dynamic_pointer_cast<SingleElementTypePtr>(a)) {
				const SingleElementTypePtr& typeA = static_pointer_cast<const SingleElementTypePtr>(a);
				const SingleElementTypePtr& typeB = static_pointer_cast<const SingleElementTypePtr>(b);

				// make sure element type is checked
				list.push_back(std::make_pair(typeA->getElementType(), typeB->getElementType()));

				// check int-type parameter
				IntTypeParamPtr paramA = typeA->getIntTypeParameter();
				IntTypeParamPtr paramB = typeB->getIntTypeParameter();

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
					// switch sides
					IntTypeParamPtr tmp = paramA;
					paramA = paramB;
					paramB = tmp;
				}

				// add mapping
				Substitution mapping(static_pointer_cast<const VariableIntTypeParam>(paramA),paramB);

				// apply substitution to remaining pairs
				applySubstitutionToList(manager, mapping, list);

				// combine current mapping with overall result
				res = Substitution::compose(manager, res, mapping);
				continue;
			}

			// => check family of generic type
			if (typeOfA == NT_GenericType) {
				const GenericTypePtr& genericTypeA = static_pointer_cast<const GenericType>(a);
				const GenericTypePtr& genericTypeB = static_pointer_cast<const GenericType>(b);

				// check family names
				if (*genericTypeA->getName() != *genericTypeB->getName()) {
					return unmatchable;
				}

				// check same number of type parameters
				if (genericTypeA->getTypeParameter().size() != genericTypeB->getTypeParameter().size()) {
					return unmatchable;
				}

				// ---- unify int type parameter ---

				// get lists
				vector<IntTypeParamPtr> paramsA = genericTypeA->getIntTypeParameter()->getParameters();
				vector<IntTypeParamPtr> paramsB = genericTypeB->getIntTypeParameter()->getParameters();

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
						// switch sides
						IntTypeParamPtr tmp = paramA;
						paramA = paramB;
						paramB = tmp;
					}

					// add mapping
					Substitution mapping(static_pointer_cast<const VariableIntTypeParam>(paramA),paramB);

					// apply substitution to remaining pairs
					applySubstitutionToList(manager, mapping, list);

					// also to remaining parameter within current pair
					for (std::size_t j=i+1; j < paramsA.size(); j++) {
						paramsA[j] = mapping.applyTo(paramsA[j]);
						paramsB[j] = mapping.applyTo(paramsB[j]);
					}

					// combine current mapping with overall result
					res = Substitution::compose(manager, res, mapping);
				}
			}

			// => check all child nodes
			auto typeParamsA = getElementTypes(a);
			auto typeParamsB = getElementTypes(b);
			if (typeParamsA.size() != typeParamsB.size()) {
				// => not matchable / unifyable
				return unmatchable;
			}

			// add pairs of children to list to be processed
			list.insert(list.end(),
					make_paired_iterator(typeParamsA.begin(), typeParamsB.begin()),
					make_paired_iterator(typeParamsA.end(), typeParamsB.end())
			);
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
	return computeSubstitution(manager, list);
}

bool isUnifyable(const TypePtr& typeA, const TypePtr& typeB) {
	if (typeA == typeB) {
		return true;
	}
	NodeManager tmp; // requires only temporary manager
	return unify(tmp, typeA, typeB);
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
		auto visitor = makeLambdaVisitor([&](const NodePtr& cur) {
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
			visitDepthFirstOnce(cur, visitor);
		});

	}

	template<typename T>
	Pointer<T> makeTypeVariablesUnique(const Pointer<T>& target, TypeSet& usedTypes, IntTypeParamSet& paramSet) {
		NodeManager& manager = target->getNodeManager();
		FreshVariableSubstitution mapper(manager, usedTypes, paramSet);
		return static_pointer_cast<T>(target->substitute(manager, mapper));
	}

}

TypePtr tryDeduceReturnType(const FunctionTypePtr& funType, const TypeList& argumentTypes) {

	NodeManager& manager = funType->getNodeManager();

	// first try to use a simple substitution
	if (auto sub = unifyAll(manager, funType->getParameterTypes()->getTypes(), argumentTypes)) {
		// substitution algorithm was sufficient => done
		return sub->applyTo(funType->getReturnType());
	}

	// try deducing variable instantiations the argument types
	auto varInstantiation = analysis::getTypeVariableInstantiation(manager, funType->getParameterTypes()->getTypes(), argumentTypes);

	// check whether derivation was successful
	if (!varInstantiation) {
		std::stringstream msg;
		msg << "Cannot match arguments ("
				<< join(", ", argumentTypes, print<deref<TypePtr>>())
				<< ") to parameters ("
				<< join(", ", funType->getParameterTypes(), print<deref<TypePtr>>())
				<< ")";
		throw ReturnTypeDeductionException(msg.str());
	}

	// extract return type
	const TypePtr& resType = funType->getReturnType();

	// compute and return the expected return type
	return varInstantiation->applyTo(manager, resType);
}

TypePtr deduceReturnType(const FunctionTypePtr& funType, const TypeList& argumentTypes, bool unitOnFail) {
	try {

		// try deducing the return type ...
		return tryDeduceReturnType(funType, argumentTypes);

	} catch (const ReturnTypeDeductionException&) {
		// didn't work => print a warning
		LOG(DEBUG) << "Unable to deduce return type for call to function of type "
				<< toString(*funType) << " using arguments " << join(", ", argumentTypes, print<deref<TypePtr>>());

	}
	// return null ptr
	return unitOnFail ? funType->getNodeManager().getLangBasic().getUnit() : TypePtr();
}


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

//	// sub-type check regarding type variables (variables are super/sub type of all other types)
//	if (subType->getNodeType() == NT_TypeVariable || superType->getNodeType() == NT_TypeVariable) {
//		return true;
//	}

	// check whether the sub-type is generic
	if (subType->getNodeType() == NT_GenericType || subType->getNodeType() == NT_StructType) {
		// use the delta algorithm for computing all the super-types of the given sub-type
		return isSubTypeOfInternal(subType, superType);
	}

	// check for vector types
	if (subType->getNodeType() == NT_VectorType) {
		VectorTypePtr vector = static_pointer_cast<const VectorType>(subType);

		// the only potential super type is an array of the same element type
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
		// check relation between reference types
		return isSubTypeOf(subType.as<RefTypePtr>()->getElementType(), superType.as<RefTypePtr>()->getElementType());
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
		IRBuilder builder(vector->getNodeManager());
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
		auto paramsA = funTypeA->getParameterTypes();
		auto paramsB = funTypeB->getParameterTypes();
		if (paramsA.size() != paramsB.size()) {
			// not matching
			return 0;
		}

		// compute join type
		// JOIN/MEET result and argument types - if possible
		TypePtr cur = getJoinMeetType(funTypeA->getReturnType(), funTypeB->getReturnType(), join);
		TypePtr resType = cur;

		// continue with arguments
		TypeList args;
		for (std::size_t i=0; cur && i<paramsA.size(); i++) {
			// ATTENTION: this goes in the reverse direction
			cur = getJoinMeetType(paramsA[i], paramsB[i], !join);
			args.push_back(cur);
		}

		// check whether for all pairs a match has been found
		if (!cur) {
			// no => no result
			return 0;
		}

		// construct resulting type
		IRBuilder builder(funTypeA->getNodeManager());
		bool plane = funTypeA->isPlain() && funTypeB->isPlain();
		return builder.functionType(args, resType, plane);
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


namespace {

	/**
	 * Defines a set of type variables required to be passed as a context
	 * through the IsGeneric visitor.
	 */
	typedef std::set<TypeVariablePtr> TypeVariableSet;

	/**
	 * A visitor searching for free type variables or variable int type parameters.
	 * The visitor is used by the isGeneric(..) function. If one of those is present,
	 * the type is generic.
	 */
	class IsGeneric : private IRVisitor<bool, Pointer, TypeVariableSet&> {

	public:

		IsGeneric() : IRVisitor<bool, Pointer, TypeVariableSet&>(true) {}

		/**
		 * An entry point for this visitor testing whether the given type
		 * is a generic type or not.
		 */
		bool test(const TypePtr& type) {
			TypeVariableSet boundVars;
			return visit(type, boundVars);
		}

	private:

		/**
		 * When encountering a variable, check whether it is bound.
		 */
		bool visitTypeVariable(const TypeVariablePtr& var, TypeVariableSet& boundVars) {
			return boundVars.find(var) == boundVars.end();	// if not, there is a free generic variable
		}

		bool visitFunctionType(const FunctionTypePtr& funType, TypeVariableSet& boundVars) {
			return any(funType->getParameterTypes(), [&](const TypePtr& type) { return visit(type, boundVars); }) ||
					visit(funType->getReturnType(), boundVars);
		}

		bool visitNamedCompositeType(const NamedCompositeTypePtr& type, TypeVariableSet& boundVars) {
			return any(type->getElements(), [&](const NamedTypePtr& element) { return visit(element->getType(), boundVars); });
		}

		bool visitTupleType(const TupleTypePtr& tuple, TypeVariableSet& boundVars) {
			return any(tuple->getElements(), [&](const TypePtr& type) { return visit(type, boundVars); });
		}

		bool visitGenericType(const GenericTypePtr& type, TypeVariableSet& boundVars) {
			// generic type is generic if one of its parameters is
			return
				any(
					type->getTypeParameter()->getTypes(),
					[&](const TypePtr& type) { return visit(type, boundVars); }
				) ||
				any(
					type->getIntTypeParameter()->getParameters(),
					[&](const IntTypeParamPtr& param) { return visit(param, boundVars); }
				);
		}

		bool visitRecType(const RecTypePtr& recType, TypeVariableSet& boundVars) {
			TypeVariableSet local = boundVars;
			for(const RecTypeBindingPtr& binding : recType->getDefinition()) {
				local.insert(binding->getVariable());
			}

			// check types within recursive bindings
			return any(recType->getDefinition(), [&](const RecTypeBindingPtr& binding) { return visit(binding->getType(), local); });
		}

		bool visitType(const TypePtr& type, TypeVariableSet& boundVars) {
			return any(type->getChildList(), [&](const NodePtr& node) { return visit(node, boundVars); });
		}


		// -- int type parameters --

		bool visitVariableIntTypeParam(const VariableIntTypeParamPtr& var, TypeVariableSet& boundVars) {
			return true;	// if there are free int type parameter => it is generic
		}

		bool visitIntTypeParam(const IntTypeParamPtr& param, TypeVariableSet& boundVars) {
			return false;	// int-type params are not causing a type to be generic
		}


		/**
		 * A terminal visit capturing all non-covered types. This one should
		 * never be reached since all cases need to be covered within specialized
		 * visit members.
		 */
		bool visitNode(const NodePtr& node, TypeVariableSet& boundVars) {
			LOG(FATAL) << "Reaching " << *node << " within IsGeneric visitor!";
			assert(false && "Should not be reached!");
			return false;
		}

	};

}


bool isGeneric(const TypePtr& type) {
	// just use the IsGeneric recursive visitor based test
	return IsGeneric().test(type);
}



// -------------------------------------------------------------------------------------------------------------------------
//                                                    Utilities
// -------------------------------------------------------------------------------------------------------------------------

TypeList getElementTypes(const TypePtr& type) {
	TypeList res;
	visitDepthFirstPrunable(type, [&](const TypePtr& cur)->bool {
		if (cur == type) { return false; } // exclude root
		res.push_back(cur);
		return true;
	}, true);
	return res;
}


bool isVariableSized(const TypePtr& cur) {
	NodeType type = cur->getNodeType();
	switch(type) {
	case NT_ArrayType: return true;
	case NT_StructType: {
		StructTypePtr structType = cur.as<StructTypePtr>();
		return !structType.empty() && isVariableSized(structType.back()->getType());
	}
	case NT_TupleType: {
		TupleTypePtr tupleType = cur.as<TupleTypePtr>();
		return !tupleType.empty() && isVariableSized(tupleType.back());
	}
	case NT_UnionType: {
		UnionTypePtr unionType = cur.as<UnionTypePtr>();
		return any(unionType, [](const NamedTypePtr& cur) { return isVariableSized(cur->getType()); });
	}
	default: return false;
	}
}

TypePtr getRepeatedType(const TypePtr& cur) {
	assert(isVariableSized(cur) && "Only variable sized types contain repeated types.");

	NodeType type = cur->getNodeType();
	switch(type) {
	case NT_ArrayType: return cur.as<ArrayTypePtr>()->getElementType();
	case NT_StructType: {
		StructTypePtr structType = cur.as<StructTypePtr>();
		return getRepeatedType(structType.back()->getType());
	}
	case NT_TupleType: {
		TupleTypePtr tupleType = cur.as<TupleTypePtr>();
		return getRepeatedType(tupleType.back());
	}
	case NT_UnionType: {
		UnionTypePtr unionType = cur.as<UnionTypePtr>();
		for(auto cur : unionType) {
			if (isVariableSized(cur->getType())) return getRepeatedType(cur->getType());
		}
		break;
	}
	default: break;
	}

	assert(false && "Invalid classification as a variable sized type!");
	return TypePtr();
}


} // end namespace core
} // end namespace insieme
