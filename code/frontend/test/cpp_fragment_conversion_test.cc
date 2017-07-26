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

#include "insieme/frontend/extensions/interceptor_extension.h"

namespace insieme {
namespace frontend {

	TEST(CppConversionTest, AdvancedClasses) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_advanced_classes.cpp"); }

	TEST(CppConversionTest, AdvancedTemplates) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_advanced_templates.cpp"); }

	TEST(CppConversionTest, BasicClasses) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_classes.cpp"); }

	TEST(CppConversionTest, BasicTemplates) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_templates.cpp"); }

	TEST(CppConversionTest, BasicTypes) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_types.cpp"); }

	TEST(CppConversionTest, Casts) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_casts.cpp"); }

	TEST(CppConversionTest, ClassOperators) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_class_operators.cpp"); }

	TEST(CppConversionTest, Constructors) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_constructors.cpp"); }

	TEST(CppConversionTest, DefaultArgs) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_default_args.cpp"); }

	TEST(CppConversionTest, DefaultArgsConstructors) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_default_args_constructors.cpp"); }

	TEST(CppConversionTest, DefaultDelete) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_default_delete.cpp"); }

	TEST(CppConversionTest, DefaultConstructorDeclaration) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_default_constructor_declaration.cpp"); }

	TEST(CppConversionTest, Destructors) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_destructors.cpp"); }

	TEST(CppConversionTest, Enum) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_enum.cpp"); }

	TEST(CppConversionTest, Expressions) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_expressions.cpp"); }

	TEST(CppConversionTest, ExpressionsRef) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_expressions_ref.cpp"); }

	TEST(CppConversionTest, Globals) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_globals.cpp"); }

	TEST(CppConversionTest, GlobalVarDeclOrder) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_global_var_decl_order.cpp"); }

	TEST(CppConversionTest, InitializerList) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_initializer_list.cpp"); }

	TEST(CppConversionTest, Lambda) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_lambda.cpp"); }

	TEST(CppConversionTest, LambdaCapture) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_lambda_capture.cpp"); }

	TEST(CppConversionTest, LambdaConstCapture) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_lambda_const_capture.cpp"); }

	TEST(CppConversionTest, LambdaNaming) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_lambda_naming.cpp"); }

	TEST(CppConversionTest, MemberCalls) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_member_calls.cpp"); }

	TEST(CppConversionTest, Members) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_members.cpp"); }

	TEST(CppConversionTest, MemFunPtr) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_mem_fun_ptr.cpp"); }

	TEST(CppConversionTest, MutuallyDependentMethods) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_mutually_dependent_methods.cpp"); }

	TEST(CppConversionTest, Naming) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_naming.cpp"); }

	TEST(CppConversionTest, NewDelete) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_new_delete.cpp"); }

	TEST(CppConversionTest, ObjectPassing) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_object_passing.cpp"); }

	TEST(CppConversionTest, PlacementNew) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_placement_new.cpp", [](ConversionJob& job) {
			// Requires interceptor extension (test extension needs to be last)
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	TEST(CppConversionTest, PseudoDestructor) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_pseudo_destructor.cpp"); }

	TEST(CppConversionTest, RefPtr) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_ref_ptr.cpp"); }

	TEST(CppConversionTest, Return) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_return.cpp"); }

	TEST(CppConversionTest, ReturnStmt) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_return_stmt.cpp"); }

	TEST(CppConversionTest, RValueXValue) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_rvalue_xvalue.cpp"); }

	TEST(CppConversionTest, StackInitialization) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_stack_initialization.cpp"); }

	TEST(CppConversionTest, Statements) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_statements.cpp", [](ConversionJob& job) {
			// Requires interceptor extension (test extension needs to be last)
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	TEST(CppConversionTest, Static) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_static.cpp"); }

	TEST(CppConversionTest, StaticDataMember) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_static_data_member.cpp"); }

	TEST(CppConversionTest, TemplateDeclInstantiation) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_template_decl_instantiation.cpp"); }

	TEST(CppConversionTest, This) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_this.cpp"); }

	TEST(CppConversionTest, Undeclared) { utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_undeclared.cpp"); }

} // fe namespace
} // insieme namespace
