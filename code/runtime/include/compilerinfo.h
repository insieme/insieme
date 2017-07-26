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
#ifndef __GUARD_COMPILERINFO_H
#define __GUARD_COMPILERINFO_H

void irt_log_compiler_info() {
#define IRT_COMPILER_PREFIX "Compiled with: "
#if defined _MSC_VER
	irt_log_comment(IRT_COMPILER_PREFIX "Microsoft C/C++ Compiler " _MSC_FULL_VER);
	#elif defined __clang__
	irt_log_comment(IRT_COMPILER_PREFIX "clang " __clang_version__);
	#elif defined __INTEL_COMPILER
	irt_log_comment(IRT_COMPILER_PREFIX "Intel Compiler " __VERSION__);
	// check for gcc last since many compilers define __GNUC__ for compatibility reasons
	#elif defined __GNUC__
	#ifdef __cplusplus
	irt_log_comment(IRT_COMPILER_PREFIX "g++ " __VERSION__);
	#else
	irt_log_comment(IRT_COMPILER_PREFIX "gcc " __VERSION__);
	#endif
	#elif defined _GEMS_SIM
	irt_log_comment(IRT_COMPILER_PREFIX "Gemsclaim Compiler");
	#else
	irt_log_comment(IRT_COMPILER_PREFIX "unknown backend compiler " __VERSION__);
	#endif
	#undef IRT_COMPILER_PREFIX
}

#endif //__GUARD_COMPILERINFO_H
