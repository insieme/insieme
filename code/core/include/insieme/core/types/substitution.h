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

#include <boost/optional.hpp>

#include "insieme/core/ir_types.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/map_utils.h"


namespace insieme {
namespace core {
namespace types {


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


} // end namespace types
} // end namespace core
} // end namespace insieme
