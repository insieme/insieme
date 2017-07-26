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

#ifndef __EXTERN
#define __EXTERN extern
#endif

__EXTERN irt_mutex_obj irt_g_error_mutex;

__EXTERN irt_tls_key irt_g_worker_key;
__EXTERN uint32 irt_g_worker_count;
__EXTERN volatile uint32 irt_g_degree_of_parallelism;
__EXTERN irt_mutex_obj irt_g_degree_of_parallelism_mutex;
__EXTERN uint32 irt_g_active_worker_count;
__EXTERN irt_mutex_obj irt_g_active_worker_mutex;
struct _irt_worker;
__EXTERN struct _irt_worker** irt_g_workers;

__EXTERN bool irt_g_rt_is_initialized;
__EXTERN bool irt_g_exit_handling_done;
__EXTERN bool irt_g_globals_initialization_done;
__EXTERN irt_runtime_behaviour_flags irt_g_runtime_behaviour;

__EXTERN irt_mutex_obj irt_g_exit_handler_mutex;

#ifdef IRT_USE_HWLOC
__EXTERN hwloc_topology_t irt_g_hwloc_topology;
#endif // IRT_USE_HWLOC

#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
__EXTERN uint64 irt_g_app_progress;
#endif // IRT_ENABLE_APP_TIME_ACCOUNTING

#ifndef IRT_MIN_MODE
__EXTERN mqd_t irt_g_message_queue;
#endif // IRT_MIN_MODE

#undef __EXTERN
