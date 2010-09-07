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

#include "definition.h"
#include "types.h"
#include "expressions.h"

using namespace insieme::core;

// ---------------------------------------------- Function ------------------------------------


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
		const ExpressionPtr& definition, const std::size_t& hashCode)
	:
		Node(NodeType::DEFINITION, hashCode), name(name), type(type), external(external), definition(definition) { }

Node::OptionChildList Definition::getChildNodes() const {
	OptionChildList res(new ChildList());
	res->push_back(type);
	if (definition) {
		res->push_back(definition);
	}
	return res;
}

Definition* Definition::clone(NodeManager& manager) const {
	return new Definition(name, manager.get(type), external, manager.get(definition), hashCode);
}

DefinitionPtr Definition::get(NodeManager& manager, const Identifier& name, const TypePtr& type, const ExpressionPtr& definition, bool external) {
	return manager.get(Definition(name, type, external, definition, ::hash(name, type)));

}

DefinitionPtr Definition::lookup(NodeManager& manager, const Identifier& name, const TypePtr& type) {
	return manager.lookup(Definition(name, type, false, NULL, ::hash(name, type)));
}


bool Definition::equals(const Node& other) const {
	// precondition: other must be a type
	assert( dynamic_cast<const Definition*>(&other) && "Type violation by base class!" );

	// convert (statically) and check the definition properties
	const Definition& ref = static_cast<const Definition&>(other);

	// compare type and name - which makes a definition unique
	return *type == *ref.type && name == ref.name;
}

std::ostream& Definition::printTo(std::ostream& out) const {

	// print this definition
	out << ((external)?"external ":"");
	out << "define ";
	out << name;
	out << " : ";
	out << *type;
	out << " = ";

	// print defining body ...
	if (isAbstract()) {
		out << "<abstract>";
	} else {
		out << *(definition);
	}

	return out;
}

