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

#pragma once

#include <queue>
#include <map>

#include <boost/optional/optional.hpp>
#include <boost/unordered_map.hpp>
#include <boost/utility/typed_in_place_factory.hpp>


#include "insieme/core/ir_types.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {

// -------------------------------------------------------------------------------------------------------------------------
//                                                    Substitution
// -------------------------------------------------------------------------------------------------------------------------

/**
 * This class represents a substitution for type variables within types.
 */
class Substitution : public utils::Printable {

public:

	/**
	 * The definition of the data structure used to maintain type variable mappings.
	 */
	typedef utils::map::PointerMap<TypeVariablePtr, TypePtr> Mapping;

	/**
	 * The definition of the data structure used to maintain int type parameter mappings.
	 */
	typedef utils::map::PointerMap<VariableIntTypeParamPtr, IntTypeParamPtr> IntTypeParamMapping;

private:

	/**
	 * The mapping this substitution is representing.
	 */
	Mapping mapping;

	/**
	 * The mapping between integer type variables and concrete values.
	 */
	IntTypeParamMapping paramMapping;

public:

	/**
	 * Creates a new, empty substitution.
	 */
	Substitution() {};

	/**
	 * Creates a single-element mapping.
	 *
	 * @param var the variable to be substituted
	 * @param type the type the variable should be substituted with
	 */
	Substitution(const TypeVariablePtr& var, const TypePtr& type);

	/**
	 * Creates a single-int-type-parameter mapping.
	 *
	 * @param var the parameter to be substituted
	 * @param parameter the int-type-parameter the parameter is substituted for
	 */
	Substitution(const VariableIntTypeParamPtr& var, const IntTypeParamPtr& parameter);

	/**
	 * Checks whether this substitution is actually mapping any variables to some type.
	 */
	bool empty() const {
		return mapping.empty() && paramMapping.empty();
	}

	/**
	 * Applies this substitution to the given type.
	 * @param manager the manager to be used for creating new type node instances
	 * @param type the type to which this substitution should be applied to
	 * @return the resulting type expression
	 */
	TypePtr applyTo(NodeManager& manager, const TypePtr& type) const;

	/**
	 * Applies this substitution to the given type using the types node manager. A call
	 * to applyTo(type) is equivalent to applyTo(type->getNodeManager(),type).
	 *
	 * @param type the type to which this substitution should be applied to
	 * @return the resulting type expression
	 */
	TypePtr applyTo(const TypePtr& type) const {
		return applyTo(type->getNodeManager(), type);
	}

	/**
	 * Applies this substitution to the given int type parameter.
	 * @param param the int type param this substitution should be applied to
	 * @return the resulting int type parameter
	 */
	IntTypeParamPtr applyTo(const IntTypeParamPtr& param) const;

	/**
	 * Extends this substitution by the given mapping. If the same variable
	 * is already mapped to some type, the current mapping will be replaced.
	 *
	 * @param the variable which should be substituted.
	 * @param the type the variable should be substituted for.
	 */
	void addMapping(const TypeVariablePtr& var, const TypePtr& type);

	/**
	 * Extends this substitution by the given int-type param mapping. If the same
	 * variable is already mapped to some value, the curren mapping will be replaced.
	 *
	 * @param var the int type parameter variable to be substituted. This has to be a int type parameter variable.
	 * @param value the value the variable should be substituted with.
	 */
	void addMapping(const VariableIntTypeParamPtr& var, const IntTypeParamPtr& value);

	/**
	 * Checks whether this substitution contains a mapping for the given variable.
	 *
	 * @param var the variable to be checked
	 * @return true if so, false otherwise
	 */
	bool containsMappingFor(const TypeVariablePtr& var) const;

	/**
	 * Checks whether this substitution contains a mapping for the given int type parameter
	 * variable.
	 *
	 * @param var the variable to be checked
	 * @return true if present, false otherwise
	 */
	bool containsMappingFor(const VariableIntTypeParamPtr& var) const;

	/**
	 * Removes the mapping stored for the given variable from this substitution.
	 * @param var the variable which's entry should be removed
	 */
	void remMappingOf(const TypeVariablePtr& var);

	/**
	 * Removes the mapping stored for the given variable from this substitution.
	 * @param var the variable which's entry should be removed
	 */
	void remMappingOf(const VariableIntTypeParamPtr& var);

	/**
	 * Obtains a constant reference to the type variable mapping constituting this substitution.
	 * @return a constant reference to the internally maintained type variable mapping
	 */
	Mapping& getMapping() {
		return mapping;
	}

	/**
	 * Obtains a constant reference to the type variable mapping constituting this substitution.
	 * @return a constant reference to the internally maintained type variable mapping
	 */
	const Mapping& getMapping() const {
		return mapping;
	}

	/**
	 * Obtains a constant reference to the int-type parameter mapping constituting this substitution.
	 * @return a constant reference to the internally maintained int type parameter mapping
	 */
	IntTypeParamMapping& getIntTypeParamMapping() {
		return paramMapping;
	}

	/**
	 * Obtains a constant reference to the int-type parameter mapping constituting this substitution.
	 * @return a constant reference to the internally maintained int type parameter mapping
	 */
	const IntTypeParamMapping& getIntTypeParamMapping() const {
		return paramMapping;
	}

	/**
	 * Prints this substitution to the given output stream.
	 *
	 * @param out the stream to be printed to
	 * @return the handed in stream
	 */
	virtual std::ostream& printTo(std::ostream& out) const;

	/**
	 * A utility function to compose two substitutions. Applying the resulting substitution will have the
	 * same effect on any type expression as applying substitution a followed by b.
	 *
	 * @param manager the manager to be used for creating new type nodes if necessary
	 * @param a the first substitution to be applied
	 * @param b the second substitution to be applied
	 * @return a substitution combining the given substitutions.
	 */
	static Substitution compose(NodeManager& manager, const Substitution& a, const Substitution& b);

};

// define a simple name for an optional substitution - which will be the result of unification and matching algorithms.
typedef boost::optional<Substitution> SubstitutionOpt;

/**
 * Creates a copy of the given substitution where all referenced types are handled by the given manger.
 *
 * @param manager the manager to be managing the nodes referenced by the resulting substitution
 * @param substitution the substitution to be copied
 * @return a copy of the given substitution instance where all nodes are maintained by the given manager
 */
inline SubstitutionOpt copyTo(NodeManager& manager, const SubstitutionOpt& substitution) {
	// check for early exit
	if (!substitution) {
		return substitution;
	}

	// copy the substitution
	SubstitutionOpt res(boost::in_place<Substitution>());
	for_each(substitution->getMapping(), [&](const std::pair<TypeVariablePtr, TypePtr>& cur){
		res->addMapping(manager.get(cur.first), manager.get(cur.second));
	});
	for_each(substitution->getIntTypeParamMapping(), [&](const std::pair<VariableIntTypeParamPtr, IntTypeParamPtr>& cur){
		res->addMapping(manager.get(cur.first), manager.get(cur.second));
	});
	return res;
}


// -------------------------------------------------------------------------------------------------------------------------
//                                                    Unification
// -------------------------------------------------------------------------------------------------------------------------

/**
 * Tries to unify the two given types.
 *
 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
 * @param typeA the first type to be unified
 * @param typeB the second type to be unified
 * @return the resulting most general unifier or an uninitialized value if the types couldn't be unified.
 */
SubstitutionOpt unify(NodeManager& manager, const TypePtr& typeA, const TypePtr& typeB);

/**
 * Tries to unify the given list of type pairs. This method actually implements the unification algorithm.
 *
 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
 * @param list the list of pairs of types to be unified
 * @return an optional containing a substitution if the pairs could be unified. The substitution will be the most general
 * unifier (mgu) for the given pairs. Applying the substitution to all pairs will result in a list of equal pairs. The optional
 * will be uninitialized if the given types are not unifyable.
 */
SubstitutionOpt unifyAll(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list);

/**
 * Tries to unify the types stored within the given lists (pairwise).
 *
 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
 * @param listA the first list of types
 * @param listB the second list of types
 * @return the resulting most general unifier or an uninitialized value if the types couldn't be unified.
 */
template<typename ContainerA, typename ContainerB>
SubstitutionOpt unifyAll(NodeManager& manager, const ContainerA& listA, const ContainerB& listB) {

	// check length of lists ...
	if (listA.size() != listB.size()) {
		// ... if not equal => not unifyable
		return boost::optional<Substitution>();
	}

	// define some types ...
	typedef std::pair<TypePtr, TypePtr> Pair;
	typedef std::list<Pair> List;

	// delegate work to non-template method within cpp file
	List list(
				make_paired_iterator(listA.begin(), listB.begin()),
				make_paired_iterator(listA.end(), listB.end())
	);
	return unifyAll(manager, list);
}

template<typename Container>
SubstitutionOpt unifyAll(NodeManager& manager, const Container& list) {
	return unifyRange(manager, list.begin(), list.end());
}

template<typename Iterator>
SubstitutionOpt unifyRange(NodeManager& manager, Iterator begin, Iterator end) {

	// just unify one after another
	Substitution res;
	if (begin == end) {
		return res;
	}

	TypePtr unified = *begin;
	++begin;
	for(;begin != end; ++begin) {
		auto cur = unify(manager, unified, res.applyTo(*begin));
		if (!cur) {
			// => not unify-able
			return 0;
		}
		unified = cur->applyTo(unified);
		res = Substitution::compose(manager, res, *cur);
	}

	// return result
	return res;
}

/**
 * Tests whether the given two types are unifyable at all.
 *
 * @param typeA the first type
 * @param typeB the second type
 * @return true if they are unifyable, false otherwise
 */
bool isUnifyable(const TypePtr& typeA, const TypePtr& typeB);

/**
 * Tests whether all the types within the given lists are (pairwise) unifyable.
 *
 * @param listA the first list of types
 * @param listB the second list of types
 * @return true if unifyable, false otherwise
 */
template<typename Container>
bool areUnifyable(const Container& listA, const Container& listB) {
	NodeManager tmp; // requires only temporary manager
	// exploits implicit boolean conversion of boost::optional
	return unify(tmp, listA, listB);
}


// -------------------------------------------------------------------------------------------------------------------------
//                                                    SubTyping
// -------------------------------------------------------------------------------------------------------------------------

/**
 * Tests whether the given sub-type is in deed a sub-type o the given super type.
 *
 * @param subType the sub-type to be tested
 * @param superType the super type to be compared with
 * @return true if subType is in deed a sub-type of the super type
 */
bool isSubTypeOf(const TypePtr& subType, const TypePtr& superType);

/**
 * Tries to obtain the smallest common super-type of the given types (Join Type in the sub-type relation).
 *
 * @param typeA the first type to be considered
 * @param typeB the second type to be considered
 * @return the smallest common super type or null, if no such type exists
 */
TypePtr getSmallestCommonSuperType(const TypePtr& typeA, const TypePtr& typeB);

/**
 * Computes the smallest common super type of the types within the given container.
 *
 * @param types the typed to be considered
 * @return the smallest common super type or null, if no such type exists
 */
template<typename Container>
TypePtr getSmallestCommonSuperType(const Container& types) {
	if (types.empty()) { return 0; }
	auto it = types.begin();
	TypePtr res = *it;
	for(++it; res && it != types.end(); ++it) {
		res = getSmallestCommonSuperType(res, *it);
	}
	return res;
}

/**
 * Tries to obtain the biggest common sub-type of the given types (Meet Type in the sub-type relation).
 *
 * @param typeA the first type to be considered
 * @param typeB the second type to be considered
 * @return the biggest common sub type or null, if no such type exists
 */
TypePtr getBiggestCommonSubType(const TypePtr& typeA, const TypePtr& typeB);

/**
 * Tries to obtain the biggest common sub-type of the given types (Meet Type in the sub-type relation).
 *
 * @param types the typed to be considered
 * @return the biggest common sub type or null, if no such type exists
 */
template<typename Container>
TypePtr getBiggestCommonSubType(const Container& types) {
	if (types.empty()) { return 0; }
	auto it = types.begin();
	TypePtr res = *it;
	for(++it; res && it != types.end(); ++it) {
		res = getBiggestCommonSubType(res, *it);
	}
	return res;
}

// -------------------------------------------------------------------------------------------------------------------------
//                                                    Utilities
// -------------------------------------------------------------------------------------------------------------------------

/**
 * Checks whether the given node x is referenced directly or indirectly
 * within the given term.
 *
 * @param x the node to be searched within the given term
 * @param term the term to be searched
 * @return true if present, false otherwise
 */
bool occurs(const NodePtr& x, const NodePtr& term);

/**
 * An exception type used if a return type could not successfully be deduced.
 */
class ReturnTypeDeductionException : public std::exception {
	// the message describing this type of exception
	const string msg;
public:
	ReturnTypeDeductionException(const string& msg = "Unable to deduce return type.") : msg(msg) {}
	virtual ~ReturnTypeDeductionException() throw() { }
	virtual const char* what() const throw() { return msg.c_str(); }
};

/**
 * This function is trying to deduce the type returned when calling a function having the
 * given type by passing parameters of the given types. If the type cannot be deduced,
 * an exception is raised.
 *
 * @param funType the type of the function to be invoked, for which the return type should be deduced
 * @param argumentTypes the types of arguments passed to this function
 * @return the deduced, most generic return type
 * @throws ReturnTypeDeductionException if the requested type cannot be deduced
 */
TypePtr tryDeduceReturnType(const FunctionTypePtr& funType, const TypeList& argumentTypes);

/**
 * This function is deducing the type returned when calling a function having the given
 * type by passing parameters of the given types.
 *
 * @param funType the type of the function to be invoked, for which the return type should be deduced
 * @param argumentTypes the types of arguments passed to this function
 * @return the deduced, most generic return type
 */
TypePtr deduceReturnType(const FunctionTypePtr& funType, const TypeList& argumentTypes, bool unitOnFail = true);

/**
 * Determines whether the given type is generic or not. A type is considered to be generic if it
 * includes type variables. Hence, a function accepting a generic input parameter or a value type
 * including a variable type parameter will be considered generic.
 *
 * @param type the type to be checked
 * @return true if the type is generic, false otherwise
 */
bool isGeneric(const TypePtr& type);


// -------------------------------------------------------------------------------------------------------------------------
//                                                    Utilities
// -------------------------------------------------------------------------------------------------------------------------

/**
 * Obtains a list of types directly referenced by the given type.
 *
 * @param type the type which's direct sub-types should be determined.
 * @return the list of identified sub-types
 */
TypeList getElementTypes(const TypePtr& type);

/**
 * Determines whether the given type is a variable sized data structure. A variable sized
 * data structure is either an array or a struct / tuple / union containing a variable sized
 * data structure as an element type (for structs / tuples it needs to be the laste element).
 *
 * @param cur the type to be checked
 * @return true if it is a variable sized type, false otherwise
 */
bool isVariableSized(const TypePtr& type);

/**
 * Every variable sized type needs to contain an array of elements covering an variable amount
 * of elements - this function obtains the type of elements stored within this array.
 *
 * @param type the type to be analysis - must be a variable sized type
 * @return the type of element forming the variable sized array within the given type
 */
TypePtr getRepeatedType(const TypePtr& type);


} // end namespace core
} // end namespace insieme

