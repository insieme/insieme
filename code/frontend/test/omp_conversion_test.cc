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
#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/frontend/extensions/omp_frontend_extension.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"

namespace insieme {
namespace frontend {

	namespace {
		void runOmpTestOn(const string& fn, std::function<void(ConversionJob&)> jobModifier = [](ConversionJob& job) {}) {
			utils::runConversionTestOn(fn, [&jobModifier](ConversionJob& job) {
				job.forceFrontendExtension<extensions::OmpFrontendExtension>();
				jobModifier(job);
			});
		}
	}

	TEST(OpenMPConversion, Basic) {
		runOmpTestOn(FRONTEND_TEST_DIR "/inputs/omp/omp_basic.c");
	}

	TEST(OpenMPConversion, Capture) {
		runOmpTestOn(FRONTEND_TEST_DIR "/inputs/omp/omp_capture.c");
	}

	TEST(OpenMPConversion, Functions) {
		runOmpTestOn(FRONTEND_TEST_DIR "/inputs/omp/omp_functions.c");
	}

	TEST(OpenMPConversion, Task) {
		runOmpTestOn(FRONTEND_TEST_DIR "/inputs/omp/omp_task.c");
	}

	TEST(OpenMPConversion, This) {
		runOmpTestOn(FRONTEND_TEST_DIR "/inputs/omp/omp_this.cpp");
	}

} // fe namespace
} // insieme namespace
