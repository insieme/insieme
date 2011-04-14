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

#include "insieme/core/type_utils.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace analysis {

/**
 * This class is forming a collection for type variable constraints.
 */
class TypeVariableConstraints : public utils::Printable {

public:

	// An enumeration of the various supported constraints
	enum ConstraintType {
		SUB_TYPE = -1, SUPER_TYPE = 1, EQUAL_TYPE = 0
	};

	/**
	 * A utility function to inverse constrain types. This function will turn a sub-type constrain
	 * into a super-type constraint and vica verce.
	 */
	inline static ConstraintType inverse(ConstraintType type) {
		return (ConstraintType)(type * -1);
	}

private:

	/**
	 * A struct summarizing constraints on a single type variable.
	 */
	struct VariableConstraint : public utils::Printable {

		/**
		 * The set of types the constraint variable has to be a super-type of.
		 */
		TypeSet superTypes;

		/**
		 * The set of types the constraint variable has to be a sub-type of.
		 */
		TypeSet subTypes;

		/**
		 * The list of types the constraint variable has to be equivalent to.
		 * Typically only a single value within this set is useful. An exception is
		 * provided by cases where all the types within the set are equivalent within
		 * the sub-type relation.
		 */
		TypeSet equalTypes;

		/**
		 * Requests this constraint to be solved. The solution will be the largest type which
		 * is satisfying all the constraints.
		 */
		TypePtr solve() const;

		/**
		 * Allows instances of this class to be printed to a stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};

	/**
	 * A flag marking this constraints as unsatisfiable. The internally stored constraints
	 * may still be satisfiable. However, during their inference a unsatisfiable constrain
	 * has been encountered => no valid substitution can be derived.
	 */
	bool unsatisfiable;

	/**
	 * The set of type variable constraints to be represented. For each variable a set of upper and
	 * lower boundaries as well as equality constraints can be defined.
	 */
	utils::map::PointerMap<TypeVariablePtr, VariableConstraint> constraints;

	/**
	 * The mapping for int-type parameter variables resulting from the constraints.
	 */
	utils::map::PointerMap<VariableIntTypeParamPtr, IntTypeParamPtr>
			intTypeParameter;

public:

	/**
	 * Creates a new, empty set of constraints.
	 */
	TypeVariableConstraints() :
		unsatisfiable(false) {
	}

	/**
	 * Adds an additional sub-type constraint to this set of constraints.
	 *
	 * @param var the variable to be constraint
	 * @param subType the type the variable instantiation has to be a sub-type of
	 */
	void addSubtypeConstraint(const TypeVariablePtr& var,
			const TypePtr& subType) {
		if (!unsatisfiable)
			getConstraintsFor(var).subTypes.insert(subType);
	}

	/**
	 * Adds an additional super-type constraint to this set of constraints.
	 *
	 * @param var the variable to be constraint
	 * @param superType the type the variable instantiation has to be a super-type for
	 */
	void addSupertypeConstraint(const TypeVariablePtr& var,
			const TypePtr& superType) {
		if (!unsatisfiable)
			getConstraintsFor(var).superTypes.insert(superType);
	}

	/**
	 * Adds an equality constraint to the set of constraints of the given variable.
	 *
	 * @param var the variable to be constraint
	 * @param type the type this variable has to be equivalent to (super- and sub-type)
	 */
	void addEqualsConstraint(const TypeVariablePtr& var, const TypePtr& type) {
		if (!unsatisfiable)
			getConstraintsFor(var).equalTypes.insert(type);
	}

	/**
	 * Adds an additional constrain on the given variable to this set of constraints.
	 *
	 * @param var the variable to be constrained
	 * @param type the type representing the constrain
	 * @param kind the kind of constraint to be added
	 */
	void addConstraint(const TypeVariablePtr& var, const TypePtr& type, ConstraintType kind) {
		switch(kind) {
		case SUB_TYPE: addSubtypeConstraint(var, type); return;
		case SUPER_TYPE: addSupertypeConstraint(var, type); return;
		case EQUAL_TYPE: addEqualsConstraint(var, type); return;
		}
		assert(false && "Illegal constrain type encountered!");
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

private:

	/**
	 * A private method obtaining the a reference to the internally maintained variable constraints
	 * regarding the given variable.
	 *
	 * @param var the variable which's constraints should be looked up
	 * @return a reference to the internally maintained constraints
	 */
	VariableConstraint& getConstraintsFor(const TypeVariablePtr& var) {
		auto pos = constraints.find(var);
		if (pos == constraints.end()) {
			auto res = constraints.insert(
					std::make_pair(var, VariableConstraint()));
			assert(res.second);
			pos = res.first;
		}
		return pos->second;
	}

};

/**
 * Tries to compute a valid type variable substitution for a call to a function accepting the given parameter types using
 * the given argument types.
 *
 * @param manager the node manager to be used for temporary IR nodes
 * @param parameter the list of parameter types accepted by the function
 * @param arguements the list of argument types passed to the function
 * @return a type-variable substitution mapping all the variables to argument specific types
 */
SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager,
		const TypeList& parameter, const TypeList& arguments);

} // end namespace analysis
} // end namespace core
} // end namespace insieme
