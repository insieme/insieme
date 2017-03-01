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
#pragma once

/**
 * This macro can be used to run backend snippet tests. Normally one would use the macro defined below which uses the sequential backend by default
 *
 * IR_PROGRAM            the IR to be converted by the backend
 * BACKEND               the backend to use (e.g. sequential::SequentialBackend::getDefault() or runtime::RuntimeBackend::getDefault() )
 * PRINT_BACKEND_CODE    a boolean flag specifying whether to print the generated backend code or not
 * COMPILER              the compiler to use (mostly either utils::compiler::Compiler::getDefaultCppCompiler() or
 *                                            utils::compiler::Compiler::getDefaultC99Compiler() )
 * COMPARISON_ACTIONS    a block of code to check the result. can use the variable 'code' which contains the generated code.
 *                       The variable 'originalCode' will contain the code as the backend generated it - including all comments
 */
#define DO_TEST_WITH_BACKEND(IR_PROGRAM, BACKEND, PRINT_BACKEND_CODE, COMPILER, COMPARISON_ACTIONS) \
	core::ProgramPtr program = builder.normalize(builder.parseProgram(IR_PROGRAM)); \
	/* check program */ \
	ASSERT_TRUE(program); \
	ASSERT_TRUE(core::checks::check(program).empty()) << core::checks::check(program); \
	/* convert program */ \
	auto converted = BACKEND->convert(program); \
	ASSERT_TRUE((bool)converted); \
	auto originalCode = toString(*converted); \
	auto code = utils::removeCppStyleComments(originalCode); \
	if(PRINT_BACKEND_CODE) std::cout << "Converted code produced by sequential backend: =======\n" << code << "\n============================\n"; \
	/* perform comparisons */ \
	COMPARISON_ACTIONS \
	/* try compiling the code fragment */ \
	utils::compiler::Compiler compiler = COMPILER; \
	compiler.addFlag("-c"); /* do not run the linker */ \
	EXPECT_TRUE(utils::compiler::compile(*converted, compiler));

#define DO_TEST(IR_PROGRAM, PRINT_BACKEND_CODE, COMPILER, COMPARISON_ACTIONS) \
	DO_TEST_WITH_BACKEND(IR_PROGRAM, sequential::SequentialBackend::getDefault(), PRINT_BACKEND_CODE, COMPILER, COMPARISON_ACTIONS)
