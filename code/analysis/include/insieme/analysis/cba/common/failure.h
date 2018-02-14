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

#include <exception>
#include <string>
#include <vector>
#include <sstream>

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * The kind of exception thrown in case failures are detected.
	 */
	class AnalysisFailure : public std::exception {

		// the list of failures detected during the evaluation
		std::vector<std::string> failures;

		// a summary of those errors
		std::string summary;

	public:

		/**
		 * Creates a new instance wrapping up the given failure.
		 */
		AnalysisFailure(const std::string& msg) : AnalysisFailure(std::vector<std::string>{msg}) {}

		/**
		 * Creates a new instance wrapping up the given failures.
		 * The list of failures must not be empty.
		 */
		AnalysisFailure(const std::vector<std::string>& failures) : failures(failures) {
			std::stringstream s;
			s << "Encountered " << failures.size() << " failures during analysis:\n\t";
			s << join("\n\t", failures);
			summary = s.str();
		}

		/**
		 * Provides access to the internally recorded list of failure messages.
		 */
		const std::vector<std::string>& getFailureMessages() const {
			return failures;
		}

		/**
		 * Provides a readable summary for the identified errors.
		 */
		virtual const char* what() const throw() {
			return summary.c_str();
		}
	};

} //'end namespace cba
} // end namespace analysis
} // end namespace insieme
