/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include <iostream>
#include <string>

namespace insieme {
namespace core {
namespace dump {
namespace binary {
namespace utils {

	// some type definitions
	typedef uint32_t length_t;
	typedef uint16_t type_t;
	typedef uint32_t index_t;

	/**
	* Writes a value binary encoded to the given output stream.
	*
	* @param out the stream to be writing to
	* @param value the value to be written
	*/
	template <typename T>
	void write(std::ostream& out, T value) {
		out.write((char*) &value, sizeof(T));
	}

	/**
	* Reads a value binary encoded from the given input stream.
	*
	* @param in the stream to be reading from
	* @return read value
	*/
	template <typename T>
	T read(std::istream& in) {
		T value = 0;
		in.read((char*) &value, sizeof(T));
		return value;
	}

	/**
	* Writes a string binary encoded, length prefixed to the given output stream.
	*
	* @param out the stream to be writing to
	* @param str the string to be written
	*/
	void dumpString(std::ostream& out, const std::string& str);

} // end namespace utils
} // end namespace binary
} // end namespace dump
} // end namespace core
} // end namespace insieme
