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

/**
 * This macro can be used to run backend snippet tests
 * IR_PROGRAM            the IR to be converted by the backend
 * PRINT_BACKEND_CODE    a boolean flag specifying whether to print the generated backend code or not
 * COMPILER              the compiler to use (mostly either utils::compiler::Compiler::getDefaultCppCompiler() or
 *                                            utils::compiler::Compiler::getDefaultC99Compiler() )
 * COMPARISON_ACTIONS    a block of code to check the result. can use the variable 'code' which contains the generated code
 */
#define DO_TEST(IR_PROGRAM, PRINT_BACKEND_CODE, COMPILER, COMPARISON_ACTIONS) \
	core::ProgramPtr program = builder.normalize(builder.parseProgram(IR_PROGRAM)); \
	/* check program */ \
	ASSERT_TRUE(program); \
	ASSERT_TRUE(core::checks::check(program).empty()) << core::checks::check(program); \
	/* convert program */ \
	auto converted = sequential::SequentialBackend::getDefault()->convert(program); \
	ASSERT_TRUE((bool)converted); \
	auto code = utils::removeCppStyleComments(toString(*converted)); \
	if(PRINT_BACKEND_CODE) std::cout << "Converted code produced by sequential backend: =======\n" << code << "\n============================\n"; \
	/* perform comparisons */ \
	COMPARISON_ACTIONS \
	/* try compiling the code fragment */ \
	utils::compiler::Compiler compiler = COMPILER; \
	compiler.addFlag("-c"); /* do not run the linker */ \
	EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
