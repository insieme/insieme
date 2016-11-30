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

#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/frontend/extensions/opencl_frontend_extension.h"
#include "insieme/frontend/extensions/variable_argument_list_extension.h"

namespace insieme {
namespace frontend {

	TEST(FragmentConversionTest, Literals) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_literals.c");
	}
	
	TEST(FragmentConversionTest, BasicTypes) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_basic_types.c");
	}

	TEST(FragmentConversionTest, Globals) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_globals.c");
	}

	TEST(FragmentConversionTest, GlobalsInit) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_globals_init.c");
	}

	TEST(FragmentConversionTest, Statements) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_statements.c");
	}

	TEST(FragmentConversionTest, VariableScopes) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_variable_scopes.c");
	}
	
	TEST(FragmentConversionTest, FunCalls) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_fun_calls.c");
	}

	TEST(FragmentConversionTest, Expressions) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_expressions.c");
	}

	TEST(FragmentConversionTest, Casts) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_casts.c");
	}

	TEST(FragmentConversionTest, DeclInitExpressions) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_decl_init_expressions.c");
	}

	TEST(FragmentConversionTest, Prototypes) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_prototypes.c");
	}

	TEST(FragmentConversionTest, FunctionPointers) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_function_pointers.c");
	}
	
	TEST(FragmentConversionTest, Static) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_static.c");
	}

	TEST(FragmentConversionTest, Enum) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_enum.c");
	}

	TEST(FragmentConversionTest, OpenCL) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/c_opencl.c",
			[](ConversionJob& job) {
				job.registerFrontendExtension<extensions::VariableArgumentListExtension>();
				job.registerFrontendExtension<extensions::OpenCLFrontendExtension>(true);
			});
	}

} // frontend namespace
} // insieme namespace
