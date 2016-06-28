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

#include "insieme/core/forward_decls.h"

// -- forward declarations --
namespace souffle {
	class Program;
} // end namespace souffle

namespace insieme {
namespace analysis {
namespace datalog {
namespace framework {

	/**
	 * Extracts facts from the given root node and inserts them into the given program using node pointers.
	 */
	int extractFacts(souffle::Program& analysis, const core::NodePtr& root, const std::function<void(core::NodePtr,int)>& nodeIndexer = [](const core::NodePtr&,int){});

	/**
	 * Extracts facts from the given root node and inserts them into the given program using node addresses.
	 */
	int extractAddressFacts(souffle::Program& analysis, const core::NodePtr& root, const std::function<void(core::NodeAddress,int)>& nodeIndexer = [](const core::NodeAddress&,int){});

	/**
	 * Checks for failure states in the given analysis. If failures are encountered, an exception will be thrown.
	 */
	void checkForFailures(souffle::Program& analysis);


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
		AnalysisFailure(const std::vector<std::string>& failures);

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

} // end namespace framework
} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
