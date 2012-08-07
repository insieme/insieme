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

// in this file prototypes of platform dependent affinity functionality shall be declared

#include "abstraction/threads.h"

#ifdef _MSC_VER
	#include <io.h>
	#include <Windows.h>
	typedef DWORD_PTR irt_native_cpu_set; // DWORD_PTR: unsigned long (32bit) for 32bit app., unsigned __int64 for 64bit
#else
	#include <unistd.h>
	typedef cpu_set_t irt_native_cpu_set;
#endif


// functionality regarding setting, clearing thread affinity and more

void irt_clear_affinity();

void irt_set_affinity(irt_affinity_mask irt_mask, irt_thread thread);

void irt_affinity_init_physical_mapping(irt_affinity_physical_mapping *out_mapping);

// get the number of available cores
uint32 irt_affinity_cores_available();


