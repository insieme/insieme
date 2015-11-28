/**
* Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
