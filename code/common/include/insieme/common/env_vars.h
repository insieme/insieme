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

#pragma once

///////////// FRONTEND

// print color dump of Clang AST before frontend conversion
#define INSIEME_PRINT_CLANG_AST "INSIEME_PRINT_CLANG_AST"

// print list of all pragmas parsed in input before frontend conversion
#define INSIEME_DUMP_PRAGMALIST "INSIEME_DUMP_PRAGMALIST"

// run irDiff for conversion tests
#define INSIEME_IRDIFF "INSIEME_IRDIFF"


///////////// CORE

// disable full semantic checks
#define INSIEME_NO_SEMA "INSIEME_NO_SEMA"

// create JSON dumps for inspyer tool on semantic errors
#define INSIEME_SEMA_INSPYER "INSIEME_SEMA_INSPYER"

// create JSON dumps for inspyer tool on IRDIFF
#define INSIEME_INSPYER "INSIEME_INSPYER"


///////////// BACKEND

// set backend compilers to use in insiemecc and unit/integration testing
#define INSIEME_C_BACKEND_COMPILER "INSIEME_C_BACKEND_COMPILER"
#define INSIEME_CXX_BACKEND_COMPILER "INSIEME_CXX_BACKEND_COMPILER"


///////////// LOGGING

// Set the log level (DEBUG, INFO, WARNING, ERROR, FATAL)
#define LOG_LEVEL_ENV "INSIEME_LOG_LEVEL"

// Set the log verbosity (1, 2, 3 are used)
#define LOG_VERBOSITY_ENV "INSIEME_LOG_VERBOSITY"

// Set up a regular expression filtering log messages by function names.
#define LOG_FILTER_ENV "INSIEME_LOG_FILTER"
