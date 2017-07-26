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
#ifndef __GUARD_ABSTRACTION_AFFINITY_OS_DEPENDENT_H
#define __GUARD_ABSTRACTION_AFFINITY_OS_DEPENDENT_H

/*
 * in this file prototypes of platform dependent affinity functionality shall be declared
 */

#include "abstraction/threads.h"

#ifdef _WIN32
#include <io.h>
#include <Windows.h>
typedef DWORD_PTR irt_native_cpu_set; // DWORD_PTR: unsigned long (32bit) for 32bit app., unsigned __int64 for 64bit
#elif defined(_GEMS_SIM)
// TODO: must still find a proper type
typedef int irt_native_cpu_set;
#else
#include <unistd.h>
typedef cpu_set_t irt_native_cpu_set;
#endif


// functionality regarding setting, clearing thread affinity and more

/** restore initial affinity as saved in irt_g_affinity_base_mask */
void irt_clear_affinity();

/** set the processor-affinity for the specified thread  */
void irt_set_affinity(irt_affinity_mask irt_mask, irt_thread thread);

/** initializes irt_g_affinity_base_mask and creates a mapping from virtual cpuids (consecutive order of ids
 starting at 0) to the real, available cpuids */
void irt_affinity_init_physical_mapping(irt_affinity_physical_mapping* out_mapping);

/** get the number of available cores with respect to the initial affinity (irt_g_affinity_base_mask) */
uint32 irt_affinity_cores_available();


#endif // ifndef __GUARD_ABSTRACTION_AFFINITY_OS_DEPENDENT_H
