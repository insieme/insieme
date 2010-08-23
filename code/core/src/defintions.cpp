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

#include <algorithm>
#include <iterator>

#include <boost/functional/hash.hpp>

#include "definitions.h"


// ---------------------------------------------- Function ------------------------------------


//DefinitionTypePtr getType(TypeManager& manager, const Definition::ParameterList& paramList, const TypePtr& returnType) {
//
//	// compose argument type as tuple
//	TupleType::ElementTypeList elementTypes;
//	projectToSecond(paramList, elementTypes);
//
//	// obtain argument type
//	TupleTypePtr argumentType = TupleType::get(manager, elementTypes);
//
//	// construct final result
//	return DefinitionType::get(manager, argumentType, manager.get(returnType));
//}


/**
 * Computes a hash for a definition based on its name and type. Since a definition
 * is uniquely defined by its name and type, the hash is only covering those values.
 *
 * @param name the name of an identifier
 * @param type the type of the element defined
 * @return a hash value for the corresponding definition
 */
std::size_t hash(const Identifier& name, const TypePtr& type) {
	std::size_t seed = 0;
	boost::hash_combine(seed, name.hash());
	boost::hash_combine(seed, type->hash());
	return seed;
}

Definition::Definition(const Identifier& name, const TypePtr& type, const bool& external,
		const ExprPtr& definition, const std::size_t& hashCode)
	:
		name(name), type(type), external(external), definition(definition), hashCode(hashCode) { }

Definition* Definition::clone(DefinitionManager& manager) const {
	return new Definition(name,
			manager.getTypeManager().get(type), external,
			manager.getStatementManager().get(*definition), hashCode);
}

DefinitionPtr Definition::get(DefinitionManager& manager, const Identifier& name, const TypePtr& type, const ExprPtr& definition, bool external) {
	return manager.get(Definition(name, type, external, definition, ::hash(name, type)));

}

DefinitionPtr Definition::lookup(DefinitionManager& manager, const Identifier& name, const TypePtr& type) {
	return manager.lookup(Definition(name, type, false, NULL, ::hash(name, type)));
}


bool Definition::operator==(const Definition& other) const {
	// shortcut for identical entries
	if (this == &other) {
		return true;
	}

	// shortcut for not-matching entries
	if (hashCode != other.hashCode) {
		return false;
	}

	// compare type and name - which makes a function unique
	return type == other.type && name == other.name;
}

// ---------------------------------------------- Utilities ------------------------------------

std::size_t hash_value(const Definition& definition) {
	return definition.hash();
}

std::ostream& operator<<(std::ostream& out, const Definition& definition) {

	// print this definition
	out << "define " << definition.getName() << " : " << *definition.getType() << " = ";

	// print defining body ...
	if (definition.isExternal()) {
		out << "<external>";
	} else if (definition.isAbstract()) {
		out << "<abstract>";
	} else {
		out << *(definition.getDefinition());
	}

	return out;
}

