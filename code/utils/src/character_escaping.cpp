/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/utils/character_escaping.h"

#include <utility>
#include <string>
#include <vector>
#include <sstream>
#include <boost/algorithm/string.hpp>

#include "insieme/utils/assert.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	namespace {

		std::vector<std::pair<std::string, char>> escapedCharMapping {
				{"\\0", '\0'},
				{"\\n", '\n'},
				{"\\t", '\t'},
				{"\\v", '\v'},
				{"\\b", '\b'},
				{"\\r", '\r'},
				{"\\f", '\f'},
				{"\\a", '\a'},
				{"\\\\", '\\'},
				{"\\?", '\?'},
				{"\\'", '\''},
				{"\\\"", '\"'}
			};
	}

	char escapedStringToChar(const std::string& character) {
		assert_true(character.length() > 0 && character.length() < 3) <<
				"Can only convert escaped characters in strings of length 1 or 2, encountered length " << character.length();
		if(character.length() == 1)
			return character.front();
		for(auto p : escapedCharMapping) {
			if(p.first == character) return p.second;
		}
		assert_not_implemented() << "Unsupported escaped character in string: >" << character << "<";
		return ' ';
	}

	std::string escapedCharToString(char character) {
		for(auto p : escapedCharMapping) {
			if(p.second == character) return p.first;
		}
		assert_not_implemented() << "Unsupported escaped character of value " << (int)character;
		return std::string();
	}

	std::string escapeChar(char input) {
		for(auto p : escapedCharMapping) {
			if(p.second == input) return p.first;
		}
		return toString(input);
	}

	std::string escapeString(const std::string& input) {
		std::stringstream ss;
		for(size_t i=0; i<input.length(); ++i) {
			ss << escapeChar(input[i]);
		}
		return ss.str();
	}

	std::string unescapeString(const std::string& escapedString) {
		std::stringstream ss;
		auto str = escapedString;
		if(boost::starts_with(str, "\"") && boost::ends_with(str, "\"")) { str = str.substr(1, str.size() - 2); }
		for(size_t i = 0; i < str.length(); ++i) {
			for(auto p : escapedCharMapping) {
				if(boost::starts_with(str.substr(i), p.first)) {
					ss << p.second;
					i += p.first.size();
					break;
				}
			}
			// if the last character was an escaped character, i was advanced beyond the end of the string
			if(i < str.length()) {
				ss << str[i];
			}
		}
		return ss.str();
	}

} // end namespace utils
} // end namespace insieme
