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

#ifndef __EXTERN
#define __EXTERN extern
#endif

__EXTERN irt_tls_key irt_g_error_key;
__EXTERN irt_mutex_obj irt_g_error_mutex;

__EXTERN irt_tls_key irt_g_worker_key;
__EXTERN uint32 irt_g_worker_count;
__EXTERN volatile uint32 irt_g_degree_of_parallelism;
__EXTERN irt_mutex_obj irt_g_degree_of_parallelism_mutex;
__EXTERN uint32 irt_g_active_worker_count;
__EXTERN irt_mutex_obj irt_g_active_worker_mutex;
struct _irt_worker;
__EXTERN struct _irt_worker **irt_g_workers;

__EXTERN bool irt_g_rt_is_initialized;
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
