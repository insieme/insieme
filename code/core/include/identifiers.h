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
#include <boost/functional/hash.hpp>

#include "annotated_ptr.h"
#include "instance_manager.h"

using std::string;


class Identifier {

	// TODO: replace with flyweight!
	string name;

	std::size_t hashCode;

public:

	Identifier(const char* name) : name(string(name)), hashCode(boost::hash_value(name)) {}
	Identifier(const string& name) : name(name), hashCode(boost::hash_value(name)) {}

	const string& getName() const { return name; }

	bool operator==(const Identifier& other) const {
		// test for identity
		if (this == &other) {
			return true;
		}

		// slow name comparison
		return name == other.name;
	}

	bool operator!=(const Identifier& other) const {
		return !(*this==other);
	}

	bool operator<(const Identifier& other) const {
		return name < other.name;
	}

	std::size_t hash() const {
		return hashCode;
	}

};


// ---------------------------------------------- Utility Functions ------------------------------------

/**
 * Allows this type to be printed to a stream (especially useful during debugging and
 * within test cases where equals values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const Identifier& type);

/**
 * Allows to compute the hash value of an identifier.
 *
 * @param identifier the identifier for which a hash value should be computed
 * @return the computed hash value
 */
std::size_t hash_value(const Identifier& identifier);

