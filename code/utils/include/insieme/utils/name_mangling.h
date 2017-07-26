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
 */

#pragma once

#include <string>

namespace insieme {
namespace utils {

	/// Format string "name" to be usable as an identifier, and encode file, line and column information in it
	///
	std::string mangle(std::string name, std::string file, unsigned line, unsigned column);

	/// Retrieve a mangled "name" for an anonymous identifier
	///
	std::string mangle(std::string file, unsigned line, unsigned column);

	/// Format string "name" to be usable as an identifier
	///
	std::string mangle(std::string name);

	/// Retrieve the original name from the mangled representation.
	///
	std::string demangle(std::string name, bool keepLocation = false);

	/// Checks whether the given name is a mangled name or not.
	///
	bool isMangled(std::string name);

	/// Retrieve a valid C/CPP name from the mangled representation.
	///
	std::string demangleToIdentifier(std::string name, bool keepLocation = false);

	/// Returns the mangled name for the assignment operator.
	///
	const std::string& getMangledOperatorAssignName();

	/// Returns the mangled name for the call operator.
	///
	const std::string& getMangledOperatorCallName();

	/// Returns the prefix for mangled conversion operator names.
	///
	const std::string& getMangledOperatorConversionPrefix();

	/// Returns a string which (if present) indicates that the name was generated for something anonymous.
	///
	const std::string& getMangledAnonymousIndicator();

	/// Returns a "readable" representation of name if it is mangled.
	///
	const std::string getReadableName(const std::string& name);

}
}
