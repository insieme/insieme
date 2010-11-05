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
#include <ostream>
#include <boost/functional/hash.hpp>

#include "insieme/utils/hash_utils.h"
#include "insieme/utils/instance_manager.h"

#include "insieme/core/annotated_ptr.h"

using std::string;

namespace insieme {
namespace core {

class Identifier : public insieme::utils::HashableImmutableData<Identifier> {

	// TODO: replace with flyweight!
	string name;

public:
	Identifier() : HashableImmutableData<Identifier>(boost::hash_value(string("UNKNOWN_IDENT"))), name(string("UNKNOWN_IDENT")) {}

	Identifier(const char* name) : HashableImmutableData<Identifier>(boost::hash_value(string(name))), name(string(name)) {}

	Identifier(const string& name) : HashableImmutableData<Identifier>(boost::hash_value(name)), name(name) {}

protected:

	virtual bool equals(const Identifier& other) const {
		// compare names
		return name == other.name;
	}

public:

	const string& getName() const {
		return name;
	}

	bool operator<(const Identifier& other) const {
		return name.compare(other.name) < 0;
	}
};

} // end namespace core
} // end namespace insieme


namespace std {
	/**
	 * Allows this type to be printed to a stream (especially useful during debugging and
	 * within test cases where equals values to be printable).
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::Identifier& identifier);
}

