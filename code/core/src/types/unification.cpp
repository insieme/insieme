/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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

			// create result
			Substitution res;
			boost::optional<Substitution> ununifiable;

			while(!list.empty()) {

				// get current element
				Pair cur = list.front();
				list.pop_front();

				TypePtr a = cur.first;
				TypePtr b = cur.second;

				const NodeType typeOfA = a->getNodeType();
				const NodeType typeOfB = b->getNodeType();

				// 1) if (a == b) ignore pair ..
				if(*a == *b) { continue; }

				// ------------------------------------ Type Variables -----------------------------------------------

				// 2) test whether on the B side there is a variable
				if(typeOfA != NT_TypeVariable && typeOfB == NT_TypeVariable) {
					// unification: no problem => swap sides
					// add swapped pair to front of list
					list.push_front(std::make_pair(b, a));
					continue;
				}

				// 2b) make sure the smaller variable is on the left side
				if(typeOfA == NT_TypeVariable && typeOfB == NT_TypeVariable) {
					// make sure the smaller variable is in front
					// (this does not really matter for the unification but
					//  ensures parameter variables to be on the left side
					//  for the return type deduction)
					if (a.as<TypeVariablePtr>()->getVarName()->getValue() > b.as<TypeVariablePtr>()->getVarName()->getValue()) {
						list.push_front(std::make_pair(b,a));
						continue;
					}
				}

				// 3) handle variables on left hand side ...
				if(typeOfA == NT_TypeVariable) {
					if(analysis::contains(a, b)) {
						// not unifiable (e.g. X = type<X> cannot be unified)
						return ununifiable;
					}

					// convert current pair into substitution
					Substitution mapping(static_pointer_cast<const TypeVariable>(a), static_pointer_cast<const Type>(b));

					// apply substitution to remaining pairs
					applySubstitutionToList(manager, mapping, list);

					// combine current mapping with overall result
					res = Substitution::compose(manager, res, mapping);
					continue;
				}

				// --------------------------------- Generic Type Variables --------------------------------------------

				// 4) test whether on the B side there is a variable
				if(typeOfA != NT_GenericTypeVariable && typeOfB == NT_GenericTypeVariable) {
					// unification: no problem => swap sides
					// add swapped pair to front of list
					list.push_front(std::make_pair(b, a));
					continue;
				}

				// 4b) make sure the smaller variable is on the left side
				if(typeOfA == NT_GenericTypeVariable && typeOfB == NT_GenericTypeVariable) {
					// make sure the smaller variable is in front
					// (this does not really matter for the unification but
					//  ensures parameter variables to be on the left side
					//  for the return type deduction)
					if (a.as<GenericTypeVariablePtr>()->getVarName()->getValue() > b.as<GenericTypeVariablePtr>()->getVarName()->getValue()) {
						list.push_front(std::make_pair(b,a));
						continue;
					}
				}

				// 5) handle variables on left hand side ...
				if(typeOfA == NT_GenericTypeVariable) {
					if(analysis::contains(a, b)) {
						// not unifiable (e.g. X = type<X> cannot be unified)
						return ununifiable;
					}

					// check whether the other side is a generic type or a generic variable
					if (typeOfB != NT_GenericTypeVariable && typeOfB != NT_GenericType) {
						// => structural mismatch
						return ununifiable;
					}

					// check length of parameters
					const auto& aParams = a.as<GenericTypeVariablePtr>()->getTypeParameterList();
					const auto& bParams = (typeOfB == NT_GenericTypeVariable)
							? b.as<GenericTypeVariablePtr>()->getTypeParameterList()
							: b.as<GenericTypePtr>()->getTypeParameterList();

					if (aParams.size() != bParams.size()) {
						// => structural mismatch
						return ununifiable;
					}

					// convert current pair into substitution
					Substitution mapping(a.as<GenericTypeVariablePtr>(), b);

					// apply substitution to remaining pairs
					applySubstitutionToList(manager, mapping, list);

					// combine current mapping with overall result
					res = Substitution::compose(manager, res, mapping);

					// add parameters to list of types to be unified
					for(const auto& cur : make_paired_range(aParams, bParams)) {
						list.push_back(cur);
					}

					// keep processing
					continue;
				}


				// ------------------------------------- Other Types ----------------------------------------------

				// 6) function types / generic types / tuples / structs / unions / recursive types
				if(typeOfA != typeOfB) {
					// => not unifiable
					return ununifiable;
				}

				// handle recursive types (special treatment)
				if(typeOfA == NT_TagType) {

					// peel recursive types until no longer recursive
					TagTypePtr ta = a.as<TagTypePtr>();
					TagTypePtr tb = b.as<TagTypePtr>();

					if (ta.isRecursive()) {
						list.push_back({ta.peel(), b});
						continue;
					}

					if (tb.isRecursive()) {
						list.push_back({a, tb.peel()});
						continue;
					}

					// check for same kind of record
					if (ta->getRecord()->getNodeType() != tb->getRecord()->getNodeType()) {
						// e.g. structs and unions can not be unified
						return ununifiable;
					}

					// add pairs of field types to unification
					FieldsPtr fa = ta->getFields();
					FieldsPtr fb = tb->getFields();

					// check number of fields
					if (fa.size() != fb.size()) {
						return ununifiable;
					}

					// for all pairs of fields ...
					for(const auto& cur : make_paired_range(fa,fb)) {
						// check name equivalence
						if (*cur.first->getName() != *cur.second->getName()) {
							return ununifiable;
						}
						// unify field types
						list.push_back({cur.first->getType(), cur.second->getType()});
					}

					// also consider parent types
					if (ta->isStruct()) {
						ParentsPtr pa = ta->getStruct()->getParents();
						ParentsPtr pb = tb->getStruct()->getParents();

						if (pa.size() != pb.size()) return ununifiable;

						// for all pairs of parents ...
						for(const auto& cur : make_paired_range(pa,pb)) {
							// check type of inheritance
							if (cur.first->isVirtual() != cur.second->isVirtual()) return ununifiable;
							if (cur.first->getAccessSpecifier() != cur.second->getAccessSpecifier()) return ununifiable;
							// unify parent types
							list.push_back({cur.first->getType(), cur.second->getType()});
						}
					}

					// that's it
					continue;
				}

				// => check family of generic type
				if(typeOfA == NT_GenericType) {
					const GenericTypePtr& genericTypeA = static_pointer_cast<const GenericType>(a);
					const GenericTypePtr& genericTypeB = static_pointer_cast<const GenericType>(b);

					// check family names
					if(*genericTypeA->getName() != *genericTypeB->getName()) { return ununifiable; }

					// check same number of type parameters
					if(genericTypeA->getTypeParameter().size() != genericTypeB->getTypeParameter().size()) { return ununifiable; }
				}

				// => check value of numeric types
				if(typeOfA == NT_NumericType) {
					// they need to be identical
					if (*a != *b) return ununifiable;
				}

				// => check all child nodes
				auto typeParamsA = analysis::getElementTypes(a);
				auto typeParamsB = analysis::getElementTypes(b);
				if(typeParamsA.size() != typeParamsB.size()) {
					// => not unifiable
					return ununifiable;
				}

				// add pairs of children to list to be processed
				list.insert(list.end(), make_paired_iterator(typeParamsA.begin(), typeParamsB.begin()),
				            make_paired_iterator(typeParamsA.end(), typeParamsB.end()));
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
		if(typeA == typeB) { return true; }
		NodeManager tmp; // requires only temporary manager
		return unify(tmp, typeA, typeB);
	}


} // end namespace types
} // end namespace core
} // end namespace insieme
