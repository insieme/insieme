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

#include "insieme/core/types/unification.h"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace core {
namespace types {


// -------------------------------------------------------------------------------------------------------------------------
//                                                    Unification
// -------------------------------------------------------------------------------------------------------------------------



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
				if (analysis::contains(a, b)) {
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
			auto typeParamsA = analysis::getElementTypes(a);
			auto typeParamsB = analysis::getElementTypes(b);
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


} // end namespace types
} // end namespace core
} // end namespace insieme
