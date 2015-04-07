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
#ifndef __GUARD_GLOBALS_H
#define __GUARD_GLOBALS_H

#include "config.h" // required to make the switch in threads.h work
#include "abstraction/threads.h"
#include "irt_inttypes.h"
#include "runtime.h"

#ifdef IRT_USE_HWLOC
#include <hwloc.h>
#endif

extern irt_tls_key irt_g_error_key;
extern irt_mutex_obj irt_g_error_mutex;

extern irt_tls_key irt_g_worker_key;
extern uint32 irt_g_worker_count;
extern volatile uint32 irt_g_degree_of_parallelism;
extern irt_mutex_obj irt_g_degree_of_parallelism_mutex;
extern uint32 irt_g_active_worker_count;
extern irt_mutex_obj irt_g_active_worker_mutex;
struct _irt_worker;
extern struct _irt_worker **irt_g_workers;

extern bool irt_g_rt_is_initialized;
extern irt_runtime_behaviour_flags irt_g_runtime_behaviour;

#ifdef IRT_USE_HWLOC
extern hwloc_topology_t irt_g_hwloc_topology;
#endif // IRT_USE_HWLOC

#endif // ifndef __GUARD_GLOBALS_H
