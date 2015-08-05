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
#ifndef __GUARD_ABSTRACTION_SOCKETS_H
#define __GUARD_ABSTRACTION_SOCKETS_H

#include "irt_inttypes.h"
#include "irt_globals.h"
#include "error_handling.h"
#include "worker.h"

#ifdef IRT_USE_HWLOC
#include <hwloc.h>

static inline void irt_hwloc_init() {
	hwloc_topology_init(&irt_g_hwloc_topology);
	hwloc_topology_load(irt_g_hwloc_topology);
}

static inline void irt_hwloc_cleanup() {
	hwloc_topology_destroy(irt_g_hwloc_topology);
}

static inline uint32 irt_hwloc_get_num_sockets() {
	int depth = hwloc_get_type_depth(irt_g_hwloc_topology, HWLOC_OBJ_SOCKET);
	return hwloc_get_nbobjs_by_depth(irt_g_hwloc_topology, depth);
}

static inline uint32 _irt_hwloc_get_num_entities_in_socket(uint32 s, hwloc_obj_type_t typ) {
	int socket_depth = hwloc_get_type_depth(irt_g_hwloc_topology, HWLOC_OBJ_SOCKET);
	hwloc_obj_t socket = hwloc_get_obj_by_depth(irt_g_hwloc_topology, socket_depth, s);
	int core_depth = hwloc_get_type_or_below_depth(irt_g_hwloc_topology, typ);
	int num_cores = hwloc_get_nbobjs_by_depth(irt_g_hwloc_topology, core_depth);
	uint32 ret = 0;
	for(int i=0; i<num_cores; ++i) {
		if(hwloc_obj_is_in_subtree(irt_g_hwloc_topology, hwloc_get_obj_by_depth(irt_g_hwloc_topology, core_depth, i), socket)) {
			ret++;
		}
	}
	return ret;
}

static inline uint32 irt_hwloc_get_num_cores_in_socket(uint32 s) {
	return _irt_hwloc_get_num_entities_in_socket(s, HWLOC_OBJ_CORE);
}

static inline uint32 irt_hwloc_get_num_logical_processors_in_socket(uint32 s) {
	return _irt_hwloc_get_num_entities_in_socket(s, HWLOC_OBJ_PU);
}

static inline void irt_worker_move_to_socket(irt_worker* worker, uint32 socket_num) {
	int depth = hwloc_get_type_depth(irt_g_hwloc_topology, HWLOC_OBJ_SOCKET);
	hwloc_obj_t socket = hwloc_get_obj_by_depth(irt_g_hwloc_topology, depth, socket_num);
	hwloc_set_thread_cpubind(irt_g_hwloc_topology, worker->thread, socket->cpuset, HWLOC_CPUBIND_THREAD);
}

#else // IRT_USE_HWLOC

static inline void irt_hwloc_init() {}
static inline void irt_hwloc_cleanup() {}

#define NO_HWLOC(_name) irt_throw_string_error(IRT_ERR_HW_INFO, "HWloc library not available, cannot use " #_name ".");

static inline uint32 irt_hwloc_get_num_sockets() {
	NO_HWLOC(irt_hwloc_get_num_sockets);
	return 0;
}

static inline uint32 irt_hwloc_get_num_cores_in_socket(uint32 s) {
	NO_HWLOC(irt_hwloc_get_num_cores_in_socket);
	return 0;
}

static inline uint32 irt_hwloc_get_num_logical_processors_in_socket(uint32 s) {
	NO_HWLOC(irt_hwloc_get_num_logical_processors_in_socket);
	return 0;
}

static inline void irt_worker_move_to_socket(irt_worker* worker, uint32 socket) {
	NO_HWLOC(irt_worker_move_to_socket);
}

#endif //IRT_USE_HWLOC

#endif // __GUARD_ABSTRACTION_SOCKETS_H