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

#include <string>
#include <cassert>
#include <boost/operators.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace utils {

	class SourceLocation : public boost::less_than_comparable<SourceLocation, SourceLocation>, public utils::Printable {
		const std::string fileName;
		const size_t lineNo;
		const size_t columnNo;
		const bool valid;

	  public:
		SourceLocation() : fileName(), lineNo(0), columnNo(0), valid(false) {}

		SourceLocation(const std::string& fileName, const size_t& lineNo, const size_t& columnNo)
		    : fileName(fileName), lineNo(lineNo), columnNo(columnNo), valid(true) {}

		const std::string& getFileName() const {
			assert_true(valid) << "Source location is not valid!";
			return fileName;
		}

		bool isValid() const {
			return valid;
		}

		size_t getLine() const {
			assert_true(valid) << "Source location is not valid!";
			return lineNo;
		}

		size_t getColumn() const {
			assert_true(valid) << "Source location is not valid!";
			return columnNo;
		}

		bool operator<(const SourceLocation& other) const {
			assert_true(valid) << "Source location is not valid!";
			return fileName == other.fileName && (lineNo < other.lineNo || (lineNo == other.lineNo && columnNo < other.columnNo));
		}

		bool operator==(const SourceLocation& other) const {
			if(!isValid() || !other.isValid()) { return false; }
			return fileName == other.fileName && lineNo == other.lineNo && columnNo == other.columnNo;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << fileName << ":" << lineNo << ":" << columnNo;
		}
	};

} // end utils namespace
} // end insieme namespace
