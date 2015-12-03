/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/utils/assert.h"

namespace insieme {
namespace utils {

	/*
	 * Converts a string containing a possibly escaped character to a char
	 * @param character a string holding the possibly escaped character to be converted to char
	 * @return the possibly escaped character as a char type
	 */
	char escapedStringToChar(const std::string& character);

	/*
	 * Converts a possibly escaped character to a string
	 * @param character a char holding the possibly escaped character to be converted to string
	 * @return the possibly escaped character as a string
	 */
	std::string escapedCharToString(char character);

	/// Escapes the input character if required, otherwise returns a string with the character
	std::string escapeChar(char input);
	
	/// Escapes all escape sequences in the given string
	std::string escapeString(const std::string& input);
	
	/// Replaces all escaped escape sequences in the given string with the corresponding literal
	std::string unescapeString(const std::string& escapedString);

} // end namespace utils
} // end namespace insieme
