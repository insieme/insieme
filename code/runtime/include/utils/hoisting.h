/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>


#ifndef IRT_MIN_MODE

#ifdef WIN32
	
#else
	#include <dlfcn.h>
#endif

#define DLOPEN_UNIQUE_BUFFSIZE 256

// TODO: dlopen_unique should become an irt_* function

#ifdef WIN32
	// TODO: implement this for windows
	void* dlopen_unique(const char* filename, int flag) {
		return NULL;
	}
#else

	void* dlopen_unique(const char* filename, int flag) {
		static unsigned count = 0;
		char uniquename[DLOPEN_UNIQUE_BUFFSIZE];
		unsigned cc = count++; // TODO should use atomic op for thread safety
		int retval = 0;
		retval = snprintf(uniquename, DLOPEN_UNIQUE_BUFFSIZE, "%s.%d", filename, cc);
		assert(retval < DLOPEN_UNIQUE_BUFFSIZE);
		char command[DLOPEN_UNIQUE_BUFFSIZE];
		retval = snprintf(command, DLOPEN_UNIQUE_BUFFSIZE, "cp %s %s", filename, uniquename);
		assert(retval < DLOPEN_UNIQUE_BUFFSIZE);
		retval = system(command);
		assert(retval == 0);
		return dlopen(uniquename, flag);
	}

#endif

#endif



