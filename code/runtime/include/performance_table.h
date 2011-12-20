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

#include "declarations.h"

typedef enum {
	WORK_ITEM_CREATED = 1000,
	WORK_ITEM_QUEUED = 1100,
	WORK_ITEM_SPLITTED = 1200,
	WORK_ITEM_STARTED = 1300,
	WORK_ITEM_SUSPENDED_IO = 1500,
	WORK_ITEM_SUSPENDED_BARRIER = 1501,
	WORK_ITEM_SUSPENDED_JOIN = 1502,
	WORK_ITEM_SUSPENDED_GROUPJOIN = 1503,
	WORK_ITEM_SUSPENDED_UNKOWN = 1599,
	WORK_ITEM_RESUMED = 1600,
	WORK_ITEM_FINISHED = 1900,
} wi_instrumentation_event;

typedef enum {
	WORK_GROUP_CREATED = 2000,
} wg_instrumentation_event;

typedef enum {
	WORKER_CREATED = 3000,
	WORKER_RUNNING = 3100,
	WORKER_SLEEP_START = 3200,
	WORKER_SLEEP_END = 3300,
	WORKER_SLEEP_BUSY_START = 3400,
	WORKER_SLEEP_BUSY_END = 3500,
	WORKER_STOP = 3800,
} worker_instrumentation_event;

typedef enum {
	DATA_ITEM_CREATED = 4000,
	DATA_ITEM_RECYCLED = 4500,
} di_instrumentation_event;

typedef enum {
	REGION_START = 5000,
	REGION_END = 5100,
} region_instrumentation_event;

typedef enum {
	OPENCL_COMMAND_NDRANGE_KERNEL = 6000,
	OPENCL_COMMAND_TASK = 6100,
	OPENCL_COMMAND_READ_BUFFER = 6200,
	OPENCL_COMMAND_WRITE_BUFFER = 6300,
	OPENCL_COMMAND_COPY_BUFFER = 6400,
	OPENCL_COMMAND_MAP_BUFFER = 6500,
	OPENCL_COMMAND_UNMAP_MEM_OBJECT = 6700,
} ocl_instrumentation_event;

typedef struct _irt_performance_data {
	uint64 timestamp;
	// just an enum, also takes event types other than wi
	wi_instrumentation_event event;
	uint64 subject_id;
} _irt_performance_data;

typedef struct _irt_pd_table {
	uint32 size;
	uint32 number_of_elements;
	uint32 blocksize;
	_irt_performance_data* data;
} _irt_pd_table;

typedef enum {
	PERFORMANCE_DATA_TYPE_ENERGY = 0,
} extended_performance_data_type;
	
typedef enum {
	ENERGY_MEASUREMENT_START = 0,
	ENERGY_MEASUREMENT_STOP = 1,
} extended_performance_event;

typedef struct _irt_extended_performance_data {
	uint64 timestamp;
	wi_instrumentation_event event;
	uint64 subject_id;
	extended_performance_data_type type;
	double data;
} _irt_extended_performance_data;

typedef struct _irt_epd_table {
	uint32 size;
	uint32 number_of_elements;
	uint32 blocksize;
	_irt_extended_performance_data* data;
} _irt_epd_table;
