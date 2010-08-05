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

#include <string>
#include <map>
#include <memory>
#include <stdexcept>

#include "types.h"

/**
 * The TypeLookupException indicates that a certain type cannot be found within the repository maintained by a
 * type manager. It will be thrown by <lookup>(const std::string).
 *
 * @see TypeManager
 */
class TypeLookupException: public std::runtime_error {

public:

	/**
	 * A construct for this type accepting the name of the type not found.
	 *
	 * @param name	The name of the type which could not be found.
	 */
	TypeLookupException(const std::string& name) :
		runtime_error("Type lookup failed for name \"" + name + "\".") {
	}
};

/**
 * The type manager represents a central repository for managing types. It maintains a list of known types, offers
 * means to look up types based on their names and to creating / registering new types. Thereby it ensures that the
 * type hierarchy remains consistent.
 */
class TypeManager {
	std::map<const std::string, TypeRef> types;

public:

	TypeManager() {
		// add abstract type token by default
		addType(AbstractType::getInstance());
	}

	TypeRef lookup(const std::string& name) const {
		auto it = types.find(name);
		if (it == types.end()) {
			throw TypeLookupException(name);
		}
		return it->second;
	}

	ArrayTypeRef getArrayType(const TypeRef elementType, const unsigned dim) {
		ArrayTypeRef ref(new ArrayType(elementType, dim));
		auto it = types.find(ref->getName());
		if (it != types.end()) {
			return std::dynamic_pointer_cast<ArrayType>(it->second);
		}
		addType(ref);
		return ref;
	}

private:

	/**
	 * Registers the given type within this type manager.
	 *
	 * @param type the new type to be registered.
	 */
	void addType(TypeRef type) {
		types.insert(std::make_pair(type->getName(), type));
	}

};
