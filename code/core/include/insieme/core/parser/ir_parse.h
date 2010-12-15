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

#include <exception>

#define BOOST_SPIRIT_DEBUG

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>

#include "insieme/core/types.h"

namespace insieme {
namespace core {
namespace parse {

class ParseException : std::exception {
	const char* what() const throw() {
		return "IR Parsing failed";
	}
};

/** A helper function for parsing an IR type declaration.
 ** If more than one definition should be parsed it is better to generate a parser object and call the parseType method
 ** @param nodeMan the NodeManager the generated definitions will be added to
 ** @param input the string representation of the IR definition to be parsed
 ** @return a pointer to an AST node representing the generated type
 **/
TypePtr parseType(NodeManager& nodeMan, const string& input);

namespace qi = boost::spirit::qi;
typedef std::string::const_iterator ParseIt;

class IRParser {
	NodeManager& nodeMan;

public:
	IRParser(NodeManager& nodeMan);

	TypePtr parseType(const std::string& input);
};

} // namespace parse 
} // namespace core
} // namespace insieme
