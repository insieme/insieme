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

/* ------------------------------ config options ----- */

#define IRT_CONTEXT_LT_BUCKETS 7
#define IRT_DATA_ITEM_LT_BUCKETS 97
#define IRT_EVENT_LT_BUCKETS 72073

// ir interface
#ifndef IRT_SANE_PARALLEL_MAX
#define IRT_SANE_PARALLEL_MAX 2048
#endif

// affinity
#ifndef IRT_MAX_CORES
#define IRT_MAX_CORES ((uint64)2048)
#endif
#define IRT_AFFINITY_POLICY_ENV "IRT_AFFINITY_POLICY"

// maximum number of frequencies that can be stored
#define IRT_INST_MAX_CPU_FREQUENCIES 32

// message queues
#define IRT_MQUEUE_NAME "/irt_message_queue"
// Be aware that the following value often has a very low OS-dictated ceiling
#define IRT_MQUEUE_MAXMSGS 4
#define IRT_MQUEUE_MAXMSGSIZE 256

// instrumentation
#define IRT_INST_OUTPUT_PATH "IRT_INST_OUTPUT_PATH"
#define IRT_INST_OUTPUT_PATH_CHAR_SIZE 4096
#define IRT_INST_WORKER_EVENT_LOGGING "IRT_INST_WORKER_EVENT_LOGGING"
#define IRT_WORKER_PD_BLOCKSIZE	512
#define IRT_REGION_LIST_SIZE 1024

// performance counters
// environment variable holding the papi events, separated by colons
#define IRT_INST_PAPI_EVENTS "IRT_INST_PAPI_EVENTS"
#define IRT_INST_PAPI_MAX_COUNTERS 16
#define IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS 512
#define NUMBER_OF_EXTENDED_PERFORMANCE_DATA_ENTRIES 19

// standalone
#define IRT_NUM_WORKERS_ENV "IRT_NUM_WORKERS"

// work group
#define IRT_WG_RING_BUFFER_SIZE 1024

// worker
#define IRT_DEFAULT_VARIANT_ENV "IRT_DEFAULT_VARIANT"

// TODO : better configurability, maybe per-wi stack size set by compiler?
// updated to 8MB due to failing test cases (quicksort, jacobi)
// don't misalign!
#define IRT_WI_STACK_SIZE 8 * 1024 * 1024

#ifndef IRT_MAX_WORKERS
#define IRT_MAX_WORKERS 2048
#endif
#ifndef IRT_MAX_WORK_GROUPS
#define IRT_MAX_WORK_GROUPS 4
#endif

