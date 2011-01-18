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

#include "insieme/core/types.h"


namespace insieme {
namespace core {

/**
 * This class represents a substitution for type variables within types.
 */
class Substitution {

public:

	/**
	 * The definition of the data structure used to maintain type variable mappings.
	 */
	typedef utils::map::PointerMap<TypeVariablePtr, TypePtr> Mapping;

	/**
	 * The definition of the data structure used to maintain int type parameter mappings.
	 */
	typedef std::map<IntTypeParam, IntTypeParam> IntTypeParamMapping;

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
	Substitution(const IntTypeParam& var, const IntTypeParam& parameter);

	/**
	 * Applies this substitution to the given type.
	 * @param manager the manager to be used for creating new type node instances
	 * @param type the type to which this substitution should be applied to
	 * @return the resulting type expression
	 */
	TypePtr applyTo(NodeManager& manager, const TypePtr& type) const;

	/**
	 * Applies this substitution to the given int type parameter.
	 * @param param the int type param this substitution should be applied to
	 * @return the resulting int type parameter
	 */
	IntTypeParam applyTo(const IntTypeParam& param) const;

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
	void addMapping(const IntTypeParam& var, const IntTypeParam& value);

	/**
	 * Removes the mapping stored for the given variable from this substitution.
	 * @param var the variable which's entry should be removed
	 */
	void remMappingOf(const TypeVariablePtr& var);

	/**
	 * Removes the mapping stored for the given variable from this substitution.
	 * @param var the variable which's entry should be removed
	 */
	void remMappingOf(const IntTypeParam& var);

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
	const IntTypeParamMapping& getIntTypeParamMapping() const {
		return paramMapping;
	}

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
boost::optional<Substitution> unify(NodeManager& manager, const TypePtr& typeA, const TypePtr& typeB);

/**
 * Tries to unify the given list of type pairs. This method actually implements the unification algorithm.
 *
 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
 * @param list the list of pairs of types to be unified
 * @return an optional containing a substitution if the pairs could be unified. The substitution will be the most general
 * unifier (mgu) for the given pairs. Applying the substitution to all pairs will result in a list of equal pairs. The optional
 * will be uninitialized if the given types are not unifyable.
 */
boost::optional<Substitution> unifyAll(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list);

/**
 * Tries to unify the types stored within the given lists (pairwise).
 *
 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
 * @param listA the first list of types
 * @param listB the second list of types
 * @return the resulting most general unifier or an uninitialized value if the types couldn't be unified.
 */
template<typename Container>
boost::optional<Substitution> unifyAll(NodeManager& manager, const Container& listA, const Container& listB) {

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
//                                                    Matching
// -------------------------------------------------------------------------------------------------------------------------

/**
 * Tries to match the given type to the given pattern. The resulting substitution will be uninitialized in case
 * the given type does match the given pattern. Otherwise the result will describe the mapping of type variables to
 * be applied to the pattern to reach the given type.
 *
 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
 * @param pattern the pattern to be checked against (type variables represent variable regions)
 * @param type the type to be matched against the given pattern
 * @param considerSubtypes a flag allowing to enable/disable the consideration of subtypes
 * @return a initialized substitution transforming the given pattern into the given type or an uninitialized optional in case the
 * given type is not matching the pattern
 */
boost::optional<Substitution> match(NodeManager& manager, const TypePtr& pattern, const TypePtr& type, bool considerSubtypes=true);

/**
 * Tries to match the given pattern/type pairs.
 *
 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
 * @param list the list of pattern/type pairs
 * @param considerSubtypes a flag allowing to enable/disable the consideration of subtypes
 * @return a initialized substitution transforming the given patterns into the given types or an uninitialized optional in case the
 * given types are not matching the patterns
 */
boost::optional<Substitution> matchAll(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list, bool considerSubtypes=true);

/**
 * Tries to match the given types to the given list of type patterns.
 *
 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
 * @param patterns the list of patterns
 * @param types the list of types to be matched to the patterns
 * @param considerSubtypes a flag allowing to enable/disable the consideration of subtypes
 * @return a initialized substitution transforming the given patterns into the given types or an uninitialized optional in case the
 * given types are not matching the patterns
 */
template<typename Container>
boost::optional<Substitution> matchAll(NodeManager& manager, const Container& patterns, const Container& types, bool considerSubtypes=true) {

	// check length of lists ...
	if (patterns.size() != types.size()) {
		// ... if not equal => not matchable
		return boost::optional<Substitution>();
	}

	// define some types ...
	typedef std::pair<TypePtr, TypePtr> Pair;
	typedef std::list<Pair> List;

	// merge the given lists to a list of pairs
	List list(
				make_paired_iterator(patterns.begin(), types.begin()),
				make_paired_iterator(patterns.end(), types.end())
	);
	return matchAll(manager, list);
}


/**
 * Tests whether the given type can be matched to the given pattern.
 *
 * @param pattern the pattern the type should be matched to
 * @param type the type to be matched against the pattern
 * @param considerSubtypes a flag allowing to enable/disable the consideration of subtypes
 * @return true if matchable, false otherwise
 */
bool isMatching(const TypePtr& pattern, const TypePtr& type, bool considerSubtypes=true);

/**
 * Tests whether the given types can be matched to the given patterns.
 *
 * @param patterns the patterns to be matched to
 * @param types the types to be matched
 * @param considerSubtypes a flag allowing to enable/disable the consideration of subtypes
 * @return true if all types can be matched to the given patterns
 */
template<typename Container>
bool areMatching(const Container& patterns, const Container& types, bool considerSubtypes=true) {
	// use temporary manager (to avoid polluting other managers)
	NodeManager tmp;
	// exploits implicit boolean conversion of boost::optional
	return matchAll(tmp, patterns, types);
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
 * Searches for reused type variables within the given types. In case the same type variables
 * are used within multiple of the given types, they will be substituted by fresh variables.
 *
 * @param manager the manager to be used for creating new types
 * @param typeA the first type to be checked
 * @param typeB the second type to be checked
 * @return a pair containing the resulting types
 */
std::pair<TypePtr, TypePtr> makeTypeVariablesUnique(NodeManager& manager, const TypePtr& typeA, const TypePtr& typeB);

/**
 * Searches for reused type variables within the given types. In case the same type variables
 * are used within multiple of the given types, they will be substituted by fresh variables.
 *
 * @param manager the manager to be used for creating new types
 * @param a list of types which should be checked for reused type variables
 * @return the corrected list of types
 */
std::vector<TypePtr> makeTypeVariablesUnique(NodeManager& manager, const vector<TypePtr>& types);


/**
 * This function is deducing the type returned when calling a function having the given
 * type by passing parameters of the given types.
 *
 * @param funType the type of the function to be invoked, for which the return type should be deduced
 * @param argumentTypes the types of arguments passed to this function
 * @return the deduced, most generic return type
 */
TypePtr deduceReturnType(FunctionTypePtr funType, TypeList argumentTypes);

}
}

/**
 * Allows substitutions to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const insieme::core::Substitution& substitution);

