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

#include <gtest/gtest.h>

#include <cstdlib>
#include <boost/filesystem.hpp>

#include "insieme/frontend/frontend.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/utils/config.h"
#include "insieme/utils/compiler/compiler.h"

namespace insieme {
namespace driver {
namespace integration {

	namespace fe = insieme::frontend;
	namespace be = insieme::backend;

	namespace {

		void testReturnValueOf(
				const be::BackendPtr& backend,
				const utils::compiler::Compiler& compiler = utils::compiler::Compiler::getDefaultC99Compiler()
		) {

			core::NodeManager manager;

			// pick a random return value
			int returnValue = (rand() % 50) + 1;

			// load input file
			fe::ConversionJob job(utils::getInsiemeSourceRootDir() + "driver/test/inputs/return_value.c");
			job.registerDefaultExtensions();
			job.setDefinition("N", toString(returnValue));

			// convert to IR program
			auto program = job.execute(manager);

			// convert to C target code
			auto code = backend->convert(program);

			// build binary
			auto binary = utils::compiler::compileToBinary(*code, compiler);
			ASSERT_FALSE(binary.empty());

			// run binary
			int a = system(binary.c_str());
			auto res = WEXITSTATUS(a);

			// check exit code
			EXPECT_EQ(returnValue, res);

			// delete target file
			if(boost::filesystem::exists(binary)) { boost::filesystem::remove(binary); }
		}

	}


	TEST(MainReturnValue, SequentialBackend) {
		testReturnValueOf(
				backend::sequential::SequentialBackend::getDefault()
		);
	}

	TEST(MainReturnValue, RuntimeBackend) {
		testReturnValueOf(
				backend::runtime::RuntimeBackend::getDefault(),
				utils::compiler::Compiler::getRuntimeCompiler()
		);
	}

} // end namespace integration
} // end namespace driver
} // end namespace insieme
