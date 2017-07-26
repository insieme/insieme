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

#ifndef POPEN_WRAPPER
	#ifdef _MSC_VER
		#define POPEN_WRAPPER _popen
		#define PCLOSE_WRAPPER _pclose
	#else
		#define POPEN_WRAPPER popen
		#define PCLOSE_WRAPPER pclose
	#endif
#endif

#ifndef SETENV_WRAPPER
	#ifdef _MSC_VER
		// _putenv_s always overwrites the value of existing environment variables
		#define SETENV_WRAPPER(__env_name_, __value_name_, __overwrite_) _putenv_s(__env_name_, __value_name_)
	#else
		#define SETENV_WRAPPER(__env_name_, __value_name_, __overwrite_) setenv(__env_name_, __value_name_, __overwrite_)
	#endif
#endif

#ifndef FUNCTION_SIGNATURE
	#ifdef _MSC_VER
		#define FUNCTION_SIGNATURE __FUNCSIG__
	#else
		#define FUNCTION_SIGNATURE __PRETTY_FUNCTION__
	#endif
#endif
