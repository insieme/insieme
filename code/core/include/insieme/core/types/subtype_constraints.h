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

#include <boost/unordered_set.hpp>

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
		boost::unordered_set<Constraint> constraints;

		/**
		 * The mapping for int-type parameter variables resulting from the constraints.
		 */
		utils::map::PointerMap<VariableIntTypeParamPtr, IntTypeParamPtr> intTypeParameter;

	public:

		/**
		 * Creates a new, empty set of constraints.
		 */
		SubTypeConstraints() :
			unsatisfiable(false) {
		}

		/**
		 * Adds an additional sub-type constraint to this set of constraints.
		 *
		 * @param var the variable to be constraint
		 * @param subType the type the variable instantiation has to be a sub-type of
		 */
		void addSubtypeConstraint(const TypePtr& subType, const TypePtr& superType) {
			if (!unsatisfiable)
				constraints.insert(std::make_pair(subType, superType));
		}

		/**
		 * Adds an equality constraint to this set of constraints.
		 *
		 * @param typeA the first type to be equivalent
		 * @param typeB the second type to be equivalent
		 */
		void addEqualsConstraint(const TypePtr& typeA, const TypePtr& typeB) {
			if (!unsatisfiable) {
				// add constraints in both directions
				constraints.insert(std::make_pair(typeA, typeB));
				constraints.insert(std::make_pair(typeB, typeA));
			}
		}

		/**
		 * Fixes the value assigned to an int-type parameter variable.
		 *
		 * @param var the variable to be fixed
		 * @param value the value to be assigned to it
		 */
		void fixIntTypeParameter(const VariableIntTypeParamPtr& var, const IntTypeParamPtr& value) {
			auto res = intTypeParameter.insert(std::make_pair(var, value));

			if (!res.second && *(res.first->second) != *value) {
				// someone tried to assign different values the same integer type variable
				// => makes the constraints unsatisfiable
				unsatisfiable = true;
			}
		}

		/**
		 * Obtains the value of a fixed int-type parameter.
		 *
		 * @param var the int-type parameter variable to be looking for
		 * @return the requested value if fixed before, a null-pointer otherwise
		 */
		IntTypeParamPtr getIntTypeParamValue(const VariableIntTypeParamPtr& var) const {
			auto pos = intTypeParameter.find(var);
			if (pos != intTypeParameter.end()) {
				return pos->second;
			}
			return 0;
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
			intTypeParameter.clear();
		}

		/**
		 * Allows instances of this class to be printed to a stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

	};

} // end namespace types
} // end namespace core
} // end namespace insieme
