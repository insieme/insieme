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

#include "independent_test_utils.h"

namespace insieme {
namespace frontend {

	TEST(CppIndependentTest, BasicTypes) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_types.cpp"); }

	TEST(CppIndependentTest, Enum) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_enum.cpp"); }

	TEST(CppIndependentTest, Expressions) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_expressions.cpp"); }

	TEST(CppIndependentTest, ExpressionsRef) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_expressions_ref.cpp"); }

	TEST(CppIndependentTest, BasicClasses) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_classes.cpp"); }

	TEST(CppIndependentTest, ClassOperators) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_class_operators.cpp"); }

	TEST(CppIndependentTest, BasicTemplates) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_templates.cpp"); }

	TEST(CppIndependentTest, AdvancedTemplates) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_advanced_templates.cpp"); }

	TEST(CppIndependentTest, NewDelete) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_new_delete.cpp"); }

	TEST(CppIndependentTest, DefaultArgs) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_default_args.cpp"); }

	TEST(CppIndependentTest, ObjectPassing) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_object_passing.cpp"); }

	TEST(CppIndependentTest, This) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_this.cpp"); }

	TEST(CppIndependentTest, Return) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_return.cpp"); }

	TEST(CppIndependentTest, InitializerList) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_initializer_list.cpp"); }

	TEST(CppIndependentTest, Globals) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_globals.cpp"); }

	TEST(CppIndependentTest, Constructors) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_constructors.cpp"); }

	TEST(CppIndependentTest, RefPtr) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_ref_ptr.cpp"); }

	TEST(CppIndependentTest, Static) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_static.cpp"); }

	TEST(CppIndependentTest, StackInitialization) { runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_stack_initialization.cpp"); }

} // fe namespace
} // insieme namespace
