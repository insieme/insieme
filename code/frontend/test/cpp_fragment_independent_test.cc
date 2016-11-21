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

#include "insieme/frontend/utils/independent_test_utils.h"

#include "insieme/frontend/extensions/interceptor_extension.h"

namespace insieme {
namespace frontend {

	TEST(CppIndependentTest, AdvancedClasses) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_advanced_classes.cpp"); }

	TEST(CppIndependentTest, AdvancedTemplates) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_advanced_templates.cpp"); }

	TEST(CppIndependentTest, BasicClasses) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_classes.cpp"); }

	TEST(CppIndependentTest, BasicTemplates) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_templates.cpp"); }

	TEST(CppIndependentTest, BasicTypes) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_types.cpp"); }

	TEST(CppIndependentTest, ClassOperators) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_class_operators.cpp"); }

	TEST(CppIndependentTest, Constructors) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_constructors.cpp"); }

	TEST(CppIndependentTest, DefaultArgs) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_default_args.cpp"); }

	TEST(CppIndependentTest, DefaultConstructorDeclaration) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_default_constructor_declaration.cpp"); }

	TEST(CppIndependentTest, Enum) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_enum.cpp"); }

	TEST(CppIndependentTest, Expressions) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_expressions.cpp"); }

	TEST(CppIndependentTest, ExpressionsRef) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_expressions_ref.cpp"); }

	TEST(CppIndependentTest, Globals) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_globals.cpp"); }

	TEST(CppIndependentTest, InitializerList) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_initializer_list.cpp"); }

	TEST(CppIndependentTest, Lambda) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_lambda.cpp"); }

	TEST(CppIndependentTest, MemberCalls) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_member_calls.cpp"); }

	TEST(CppIndependentTest, MemFunPtr) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_mem_fun_ptr.cpp"); }

	TEST(CppIndependentTest, Naming) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_naming.cpp"); }

	TEST(CppIndependentTest, NewDelete) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_new_delete.cpp"); }

	TEST(CppIndependentTest, ObjectPassing) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_object_passing.cpp"); }

	TEST(CppIndependentTest, RefPtr) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_ref_ptr.cpp"); }

	TEST(CppIndependentTest, Return) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_return.cpp"); }

	TEST(CppIndependentTest, ReturnStmt) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_return_stmt.cpp"); }

	TEST(CppIndependentTest, RValueXValue) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_rvalue_xvalue.cpp"); }

	TEST(CppIndependentTest, StackInitialization) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_stack_initialization.cpp"); }

	TEST(CppIndependentTest, Statements) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_statements.cpp", [](ConversionJob& job) {
		// Requires interceptor extension (test extension needs to be last)
		job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
	}); }

	TEST(CppIndependentTest, Static) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_static.cpp"); }

	TEST(CppIndependentTest, StaticDataMember) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_static_data_member.cpp"); }

	TEST(CppIndependentTest, This) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_this.cpp"); }

	TEST(CppIndependentTest, Undeclared) { utils::runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_undeclared.cpp"); }

} // fe namespace
} // insieme namespace
