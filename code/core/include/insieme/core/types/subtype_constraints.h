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

#include <unordered_set>

#include "insieme/core/ir_node.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/map_utils.h"

#include "insieme/core/types/subtyping.h"
#include "insieme/core/types/substitution.h"
#include "insieme/core/types/unification.h"

namespace insieme {
namespace core {
namespace types {

	/**
	 * This class is forming a collection of sub-type constraints.
	 */
	class SubTypeConstraints : public utils::Printable {
		/**
		 * A pair of types representing a sub-type constraint. The first
		 * type is constraint to be a sub-type of the second type.
		 */
		typedef std::pair<TypePtr, TypePtr> Constraint;

		/**
		 * A flag marking this constraints as unsatisfiable. The internally stored constraints
		 * may still be satisfiable. However, during their inference a unsatisfiable constrain
		 * has been encountered => no valid substitution can be derived.
		 */
		bool unsatisfiable;

		/**
		 * The set of type constraints to be considered.
		 */
		std::unordered_set<Constraint> constraints;

	  public:
		/**
		 * Creates a new, empty set of constraints.
		 */
		SubTypeConstraints() : unsatisfiable(false) {}

		/**
		 * Adds an additional sub-type constraint to this set of constraints.
		 *
		 * @param var the variable to be constraint
		 * @param subType the type the variable instantiation has to be a sub-type of
		 */
		void addSubtypeConstraint(const TypePtr& subType, const TypePtr& superType) {
			if(!unsatisfiable) { constraints.insert(std::make_pair(subType, superType)); }
		}

		/**
		 * Adds an equality constraint to this set of constraints.
		 *
		 * @param typeA the first type to be equivalent
		 * @param typeB the second type to be equivalent
		 */
		void addEqualsConstraint(const TypePtr& typeA, const TypePtr& typeB) {
			if(!unsatisfiable) {
				// add constraints in both directions
				constraints.insert(std::make_pair(typeA, typeB));
				constraints.insert(std::make_pair(typeB, typeA));
			}
		}

		/**
		 * Marks the entire set of constraints to be unsatisfiable. This can method can be
		 * used instead of adding an unsatisfiable constraint to this set.
		 */
		void makeUnsatisfiable() {
			unsatisfiable = true;
		}

		/**
		 * Obtains the current state of the explicit unsatisfiability mark. The method will return
		 * true in case this set of constraints has been marked as being unsatisfiable.
		 */
		bool isSatisfiable() const {
			return !unsatisfiable;
		}

		/**
		 * Attempts to solve this set of constraints by deriving a variable substitution
		 * mapping each constraint type variable to a satisfying substitute.
		 *
		 * @return an optional substitution satisfying all the constraints or an
		 * 			uninitialized substitution
		 */
		SubstitutionOpt solve() const;

		/**
		 * Attempts to solve this set of constraints by deriving a variable substitution
		 * mapping each constraint type variable to a satisfying substitute.
		 *
		 * @param manager the node manager to be used for constructing substitution results
		 * @return an optional substitution satisfying all the constraints or an
		 * 			uninitialized substitution
		 */
		SubstitutionOpt solve(NodeManager& manager) const;

		/**
		 * Clears all the constraints added to this container.
		 */
		void clear() {
			// clear the various members
			unsatisfiable = false;
			constraints.clear();
		}

		/**
		 * Allows instances of this class to be printed to a stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};

} // end namespace types
} // end namespace core
} // end namespace insieme
