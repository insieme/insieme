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

#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/frontend/extensions/opencl_frontend_extension.h"
#include "insieme/frontend/extensions/variable_argument_list_extension.h"

namespace insieme {
namespace frontend {

	TEST(FragmentConversionTest, Atoi) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_atoi.c");
	}

	TEST(FragmentConversionTest, BasicTypes) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_basic_types.c");
	}

	TEST(FragmentConversionTest, Casts) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_casts.c");
	}

	TEST(FragmentConversionTest, DeclInitExpressions) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_decl_init_expressions.c");
	}

	TEST(FragmentConversionTest, Enum) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_enum.c");
	}

	TEST(FragmentConversionTest, Expressions) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_expressions.c");
	}

	TEST(FragmentConversionTest, FunCalls) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_fun_calls.c");
	}

	TEST(FragmentConversionTest, FunctionPointers) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_function_pointers.c");
	}

	TEST(FragmentConversionTest, Globals) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_globals.c");
	}

	TEST(FragmentConversionTest, GlobalsInit) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_globals_init.c");
	}

	TEST(FragmentConversionTest, Literals) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_literals.c");
	}

	TEST(FragmentConversionTest, Members) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_members.c");
	}

	TEST(FragmentConversionTest, OpenCL) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_opencl.c",
			[](ConversionJob& job) {
				job.registerFrontendExtension<extensions::VariableArgumentListExtension>();
				job.registerFrontendExtension<extensions::OpenCLFrontendExtension>(true);
			});
	}

	TEST(FragmentConversionTest, Prototypes) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_prototypes.c");
	}

	TEST(FragmentConversionTest, Statements) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_statements.c");
	}

	TEST(FragmentConversionTest, Static) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_static.c");
	}

	TEST(FragmentConversionTest, VariableScopes) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_variable_scopes.c");
	}

} // frontend namespace
} // insieme namespace
