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
 */

#pragma once

#include "insieme/core/types/substitution.h"

namespace insieme {
namespace core {
namespace types {

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
	template <typename ContainerA, typename ContainerB>
	SubstitutionOpt unifyAll(NodeManager& manager, const ContainerA& listA, const ContainerB& listB) {
		// check length of lists ...
		if(listA.size() != listB.size()) {
			// ... if not equal => not unifyable
			return boost::optional<Substitution>();
		}

		// define some types ...
		typedef std::pair<TypePtr, TypePtr> Pair;
		typedef std::list<Pair> List;

		// delegate work to non-template method within cpp file
		List list(make_paired_iterator(listA.begin(), listB.begin()), make_paired_iterator(listA.end(), listB.end()));
		return unifyAll(manager, list);
	}

	template <typename Iterator>
	SubstitutionOpt unifyRange(NodeManager& manager, Iterator begin, Iterator end) {
		// just unify one after another
		Substitution res;
		if(begin == end) { return res; }

		TypePtr unified = *begin;
		++begin;
		for(; begin != end; ++begin) {
			auto cur = unify(manager, unified, res.applyTo(*begin));
			if(!cur) {
				// => not unify-able
				return boost::none;
			}
			unified = cur->applyTo(unified);
			res = Substitution::compose(manager, res, *cur);
		}

		// return result
		return res;
	}

	template <typename Container>
	SubstitutionOpt unifyAll(NodeManager& manager, const Container& list) {
		return unifyRange(manager, list.begin(), list.end());
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
	template <typename Container>
	bool areUnifyable(const Container& listA, const Container& listB) {
		NodeManager tmp; // requires only temporary manager
		// exploits implicit boolean conversion of boost::optional
		return unify(tmp, listA, listB);
	}


} // end namespace types
} // end namespace core
} // end namespace insieme
