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

static uint32 __irt_g_chached_cpu_count = 0xFFFFFFFF;

uint32 irt_get_num_cpus() {
	if(__irt_g_chached_cpu_count!=0xFFFFFFFF) return __irt_g_chached_cpu_count;
	uint32 ret = 1;
#ifdef _SC_NPROCESSORS_ONLN
	// Linux
	ret = sysconf(_SC_NPROCESSORS_ONLN);
#elif defined(WIN32)
	// Windows
	SYSTEM_INFO sysinfo; 
	GetSystemInfo( &sysinfo ); 
	ret = sysinfo.dwNumberOfProcessors;
#elif defined(_SC_NPROC_ONLN)
	// Irix
	ret = sysconf(_SC_NPROC_ONLN);
#elif defined(MPC_GETNUMSPUS)
	// HPUX
	ret = mpctl(MPC_GETNUMSPUS, NULL, NULL);
#endif
	if(ret<1) ret = 1;
	__irt_g_chached_cpu_count = ret;
	return ret;
}

// to be used only for testing
void _irt_set_num_cpus(uint32 num) {
	__irt_g_chached_cpu_count = num;
}
