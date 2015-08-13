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

#ifndef PRAGMA_TEST_EXTENSION_H
#define PRAGMA_TEST_EXTENSION_H

#pragma once

#include <string>

#include "insieme/frontend/extensions/frontend_extension.h"

namespace insieme {
namespace frontend {
namespace extensions {

	/**
	 * Custom pragma used for testing purposes;
	 *
	 * #pragma test "insieme-IR"
	 * C stmt
	 *
	 * checks if the conversion of the C statement matches the one specified by the user
	 */
	class TestPragmaExtension : public FrontendExtension {
		// holds the IR that is expected for the target statement, to be used for comparison and testing
		std::string expected;
		// holds a number of dummy arguments used for pragma parsing and location testing
		std::vector<std::string> dummyArguments;

	  public:
		TestPragmaExtension();
		std::string getExpected() const {
			return expected;
		}
		std::vector<std::string> getDummyArguments() const {
			return dummyArguments;
		}
	};

} // extensions
} // frontend
} // insieme

#endif // PRAGMA_TEST_EXTENSION_H
