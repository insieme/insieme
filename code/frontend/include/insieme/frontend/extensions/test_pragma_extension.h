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

#ifndef PRAGMA_TEST_EXTENSION_H
#define PRAGMA_TEST_EXTENSION_H

#pragma once

#include <string>

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/frontend/converter.h"

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

		// handler for "expect_num_vars" pragmas
		std::function<void(conversion::Converter&, int)> expectNumVarsHandler = [](conversion::Converter&, int) {
			assert_fail() << R"(Encountered a "expect_num_vars" pragma without handler)";
		};

	  public:
		TestPragmaExtension();
		TestPragmaExtension(const std::function<void(conversion::Converter&,int)>& expectNumVarsHandler);

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
