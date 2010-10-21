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

#include <ostream>
#include <vector>
#include <cassert>

//
//Declaration:
//  #define ENUM(enumname, first, ...);
//
//Using:
//  ENUM(testColor, RED, GREEN, YELLOW)
//  testColor tc = fromName<testColor>("RED");
//  for(testColor c = min(c); c < max(c); c++ )
//      std::cout << c << ord(c) << name(c);
//


/**
 * Splits a comma-seperated std::string into its trimmed parts.
 * Asserts that the count equals enumMax for debugging reasons.
 *
 * Note: Not the very best implementation
 *
 * @param str Comma-seperated std::string of your element names
 * @param enumMax The count of elements the enum should have
 * @return Vector of the elements' names
*/
std::vector<std::string> getListOutOfCommaSeperated(std::string str, std::size_t enumMax);


/**
 * Creates an enum type which is iterable and has its items as names.
 * You must not use '=' in your enum declarations. Your enums need to have
 * at least one item.
 *
 * Examples:
 *  ENUM(testColor, RED, GREEN, YELLOW)
 *  testColor tc = fromName<testColor>("RED");
 *  for(testColor c = min(c); c < max(c); c++ )
 *      std::cout << c << ord(c) << name(c);
 *
 * @param enumname The typename of the new enum
 * @param first The first element
 * @param ... The tail elements
 */

#define ENUM(enumname, first, ...) \
		enum enumname {enumname ## MIN, first = enumname ## MIN, __VA_ARGS__ , enumname ## MAX}; \
		const std::vector<std::string> enumname ## Strings \
			= getListOutOfCommaSeperated( "" #first "," #__VA_ARGS__ "", enumname ## MAX ); \
		inline enumname operator++(enumname &rs) { return rs = enumname( rs+1 ); }  \
		inline enumname operator++(enumname &rs, int) {\
			enumname old = rs; \
			rs = enumname( rs+1 ); \
			return old; \
		} \
		inline enumname operator--(enumname &rs) { return rs = enumname( rs - 1 ); }  \
		inline enumname operator--(enumname &rs, int) {\
			enumname old = rs; \
			rs = enumname( rs - 1 ); \
			return old; \
		} \
		inline enumname max(const enumname &rs) { return enumname ## MAX; } \
		inline enumname min(const enumname &rs) { return enumname ## MIN; } \
		inline bool isMember(const enumname& rs) { return enumname ## MIN < rs && rs < enumname ## MAX; } \
		inline std::string name(const enumname &rs) {\
			return (enumname ## Strings).at((std::size_t) rs);\
		} \
		inline std::size_t ord(const enumname &rs) {\
			return (std::size_t) rs;\
		} \
		inline enumname count(const enumname &rs) { \
			return (enumname) (ord(max(rs)) - ord(min(rs))); \
		} \
		inline std::ostream& operator<<(std::ostream &os, const enumname &c) { \
			return os << name(c);\
		} 


/**
 * Gets the element of the enum named "nam".
 *
 * @tparam T Enumtype to search on.
 * @param nam The name of the element to get
 * @return The enum element
 */
template <typename T>
inline const T fromName(const std::string &nam);



////////////////////////////////////////////////////////////////////////////////
//The characters to trim at enumTrimName
const std::string ENUM_BAD_CHARS(", \r\n\t");

/**
 * Trims all whitespaces and commas at the beginning and end of the given string.
 *
 * @param s The std::string to trim
 * @return Trimmed std::string
*/
std::string enumTrimName(std::string s);


template <typename T>
inline const T fromName(const std::string &nam) {
	T i;
	for(i = min(i); i < max(i); ++i) {
		if (nam == name(i))
			return i;
	}

	//TODO: exception state: throw exception or use invalid number?
	return max(i);
}
