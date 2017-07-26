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

#pragma once

///////////// FRONTEND

// print color dump of Clang AST before frontend conversion
#define INSIEME_PRINT_CLANG_AST "INSIEME_PRINT_CLANG_AST"

// print list of all pragmas parsed in input before frontend conversion
#define INSIEME_DUMP_PRAGMALIST "INSIEME_DUMP_PRAGMALIST"

// run irDiff for conversion tests
#define INSIEME_IRDIFF "INSIEME_IRDIFF"
// the context size to use (default 0)
#define INSIEME_IRDIFF_CONTEXT_SIZE "INSIEME_IRDIFF_CONTEXT_SIZE"


///////////// CORE

// disable full semantic checks
#define INSIEME_NO_SEMA "INSIEME_NO_SEMA"

// create JSON dumps for inspyer tool on semantic errors
#define INSIEME_SEMA_INSPYER "INSIEME_SEMA_INSPYER"

// create JSON dumps for inspyer tool on IRDIFF
#define INSIEME_INSPYER "INSIEME_INSPYER"

// abort on node creation matching the given IR string
#define INSIEME_ABORT_NODE "INSIEME_ABORT_NODE"


///////////// BACKEND

// set backend compilers to use in insiemecc and unit/integration testing
#define INSIEME_C_BACKEND_COMPILER "INSIEME_C_BACKEND_COMPILER"
#define INSIEME_CXX_BACKEND_COMPILER "INSIEME_CXX_BACKEND_COMPILER"

// do not translate the IR versions of std::initializer_list back to C++ std::initializer_list
#define INSIEME_BE_INIT_LIST_TESTING "INSIEME_BE_INIT_LIST_TESTING"


///////////// LOGGING

// Set the log level (DEBUG, INFO, WARNING, ERROR, FATAL)
#define LOG_LEVEL_ENV "INSIEME_LOG_LEVEL"

// Set the log verbosity (1, 2, 3 are used)
#define LOG_VERBOSITY_ENV "INSIEME_LOG_VERBOSITY"

// Set up a regular expression filtering log messages by function names.
#define LOG_FILTER_ENV "INSIEME_LOG_FILTER"
