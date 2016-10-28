/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
#ifndef __GUARD_CONFIG_H
#define __GUARD_CONFIG_H

/* ------------------------------ config options ----- */

// lookup table sizes
#define IRT_CONTEXT_LT_BUCKETS 7
#define IRT_DATA_ITEM_LT_BUCKETS 97
#ifndef IRT_EVENT_LT_BUCKETS
#define IRT_EVENT_LT_BUCKETS 97 /*1021*/ /*64567*/ /*256019*/ /*7207301*/
#endif

// scheduling policy
#ifndef IRT_SCHED_POLICY
#ifdef _GEMS
#define IRT_SCHED_POLICY IRT_SCHED_POLICY_STATIC
#else
//#define IRT_SCHED_POLICY IRT_SCHED_POLICY_STATIC
#define IRT_SCHED_POLICY IRT_SCHED_POLICY_STEALING_CIRCULAR
//#define IRT_SCHED_POLICY IRT_SCHED_POLICY_UBER
#endif
#endif

#define IRT_LOOP_SCHED_POLICY_ENV "IRT_LOOP_SCHED_POLICY"

// determines if workers should ever go to sleep
// - needs to be unset for the stealing policies!
// workers must not sleep when compiling/running a program on windows xp because condition variables are not supported there
//#define IRT_WORKER_SLEEPING

// ir interface
#ifndef IRT_SANE_PARALLEL_MAX
#define IRT_SANE_PARALLEL_MAX 2048
#endif

// affinity
#ifndef IRT_MAX_CORES
#define IRT_MAX_CORES ((uint64)2048)
#endif
#define IRT_AFFINITY_POLICY_ENV "IRT_AFFINITY_POLICY"

// maximum number of sockets (used by features such as DVFS)
#define IRT_HW_MAX_NUM_SOCKETS 128
#define IRT_HW_MAX_STRING_LENGTH 128

// maximum number of frequencies that can be stored
#define IRT_INST_MAX_CPU_FREQUENCIES 32

// message queues
#define IRT_MQUEUE_NAME "/irt_message_queue"
// Be aware that the following value often has a very low OS-dictated ceiling
#define IRT_MQUEUE_MAXMSGS 4
#define IRT_MQUEUE_MAXMSGSIZE 256

// instrumentation
#define IRT_INST_OUTPUT_PATH_ENV "IRT_INST_OUTPUT_PATH"
#define IRT_INST_OUTPUT_PATH_CHAR_SIZE 4096
#define IRT_INST_BINARY_OUTPUT_ENV "IRT_INST_BINARY_OUTPUT"
#define IRT_INST_WORKER_EVENT_LOGGING_ENV "IRT_INST_WORKER_EVENT_LOGGING"
#define IRT_INST_WORKER_EVENT_TYPES_ENV "IRT_INST_WORKER_EVENT_TYPES"
#define IRT_INST_WORKER_PD_BLOCKSIZE 512
#define IRT_INST_REGION_INSTRUMENTATION_ENV "IRT_INST_REGION_INSTRUMENTATION"
#define IRT_INST_REGION_INSTRUMENTATION_TYPES_ENV "IRT_INST_REGION_INSTRUMENTATION_TYPES"
#define IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE 256

// standalone
#define IRT_NUM_WORKERS_ENV "IRT_NUM_WORKERS"
#define IRT_CPU_FREQUENCIES "IRT_CPU_FREQUENCIES"
#define IRT_REPORT_ENV "IRT_REPORT"
#define IRT_REPORT_TO_FILE_ENV "IRT_REPORT_TO_FILE"

// for using a minimal variant of the runtime without affinity and message queues => standalone mode only
#define IRT_MIN_MODE

// define IRT_USE_PTHREADS if you want to use pthread lib for windows (has no effect under linux)
// better pass it in as as flag for the compiler
//#ifndef IRT_USE_PTHREADS
//#define IRT_USE_PTHREADS
//#endif

// work item
#ifndef IRT_WI_PARAM_BUFFER_SIZE
#define IRT_WI_PARAM_BUFFER_SIZE 128
#endif

// work group
#define IRT_WG_RING_BUFFER_SIZE 1024

// worker
#define IRT_DEFAULT_VARIANT_ENV "IRT_DEFAULT_VARIANT"

// TODO : better configurability, maybe per-wi stack size set by compiler?
// updated to 8MB due to failing test cases (quicksort, jacobi)
// don't misalign!
#ifndef IRT_WI_STACK_SIZE
#define IRT_WI_STACK_SIZE 8 * 1024 * 1024
#endif

#ifndef IRT_DEF_WORKERS
#define IRT_DEF_WORKERS 1
#endif
#ifndef IRT_MAX_WORKERS
#define IRT_MAX_WORKERS 2048
#endif
#ifndef IRT_MAX_WORK_GROUPS
#define IRT_MAX_WORK_GROUPS 1
#endif

// gemsclaim simulator
#if defined(_GEMS)
#ifdef __arm__
#define _GEMS_TODO
#define _GEMS_ODROID
#define GEMS_IRT_INST_REGION_INSTRUMENTATION_TYPES                                                                                                             \
	"cpu_time,wall_time,a15_avgpow,a07_avgpow,mem_avgpow,gpu_avgpow,cpu_avgpow,"                                                                               \
	"a15_energy,a07_energy,mem_energy,gpu_energy,cpu_energy"
#else
#define _GEMS_SIM
#define GEMS_CORE_FREQ_MHZ 100
#define GEMS_IRT_INST_REGION_INSTRUMENTATION_TYPES "energy,average_power,ticks"
#endif
#endif

// optimizer
// enables optimizations based on openmp+ (DCT excluded)
//#define IRT_ENABLE_OMPP_OPTIMIZER
// enables DCT optimizations based on openmp+
//#define IRT_ENABLE_OMPP_OPTIMIZER_DCT
// enables measurments of resource compsumption with different frequencies
//#defines IRT_ENABLE_OMPP_OPTIMIZER_DVFS_EVAL
// defines how many times a new frequency will be picked
//#define IRT_OMPP_OPTIMIZER_DVFS_EVAL_STEPS
#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DVFS_EVAL
#define IRT_ENABLE_OMPP_OPTIMIZER
#endif
#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT
#define IRT_ENABLE_OMPP_OPTIMIZER
#endif
#ifdef IRT_ENABLE_OMPP_OPTIMIZER
#define IRT_WORKER_SLEEPING
#endif
// defines how many settings must be evaluated before sticking to the best one
#define IRT_OMPP_OPTIMIZER_BEST (irt_g_worker_count * 40)
// lookup table size
#define IRT_OPTIMIZER_LT_BUCKETS 97
// the index of the frequency (among the vector of available ones) used by the rt
#define IRT_OPTIMIZER_RT_FREQ 1

#endif // ifndef __GUARD_CONFIG_H
