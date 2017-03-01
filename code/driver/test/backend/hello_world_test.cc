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
#include <gtest/gtest.h>

#include "insieme/driver/integration/tests.h"

#include "insieme/utils/compiler/compiler.h"

#include "insieme/frontend/frontend.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_program.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/backend/sequential/sequential_backend.h"

namespace insieme {
namespace backend {


	TEST(FullBackend, HelloWorld) {
		core::NodeManager manager;

		// load hello world test case
		auto testCase = driver::integration::getCase("seq/c/hello_world");
		ASSERT_TRUE(testCase) << "Could not load hello world test case!";

		// convert test case into IR using the frontend
		auto code = testCase->load(manager);
		ASSERT_TRUE(code) << "Unable to load input code!";

		// create target code using real backend
		auto target = backend::sequential::SequentialBackend::getDefault()->convert(code);

		// check target code
		//	EXPECT_EQ("", toString(core::printer::PrettyPrinter(code, core::printer::PrettyPrinter::OPTIONS_DETAIL)));
		//	EXPECT_EQ("", toString(*target));

		// see whether target code can be compiled
		// TODO: compile target code => test result
		//	EXPECT_TRUE(utils::compiler::compile(*target));
	}

} // end namespace backend
} // end namespace insieme
