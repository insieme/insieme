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
#include <cassert>
#include <boost/operators.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace utils {

class SourceLocation:
	public boost::less_than_comparable<SourceLocation, SourceLocation>,
	public utils::Printable {
	const std::string 	fileName;
	const size_t		lineNo;
	const size_t 		columnNo;
	const bool 			valid;
	
public:
	SourceLocation(): fileName(), lineNo(0), columnNo(0), valid(false) { }
	
	SourceLocation(const std::string& 	fileName,
	               const size_t& 		lineNo,
	               const size_t& 		columnNo)
		:  fileName(fileName), lineNo(lineNo), columnNo(columnNo), valid(true) { }
		
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
		return fileName == other.fileName &&
		       (lineNo < other.lineNo || (lineNo == other.lineNo && columnNo < other.columnNo));
	}
	
	bool operator==(const SourceLocation& other) const {
		if(!isValid() || !other.isValid()) {
			return false;
		}
		return fileName == other.fileName &&
		       lineNo == other.lineNo &&
		       columnNo == other.columnNo;
	}
	
	std::ostream& printTo(std::ostream& out) const {
		return out << fileName << ":" << lineNo << ":" << columnNo;
	}
};

} // end utils namespace
} // end insieme namespace
