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

#pragma once

#include <exception>
#include <string>
#include <vector>

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
		 * Creates a new instance wrapping up the given failures.
		 * The list of failures most not be empty.
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
