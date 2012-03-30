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
#include "performance_table.h"

#ifdef USE_OPENCL
#define IRT_ENABLE_INSTRUMENTATION
#endif

#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
#define IRT_ENABLE_INSTRUMENTATION
#endif

#ifndef IRT_ENABLE_INSTRUMENTATION
#define IRT_ENABLE_INSTRUMENTATION
#endif
#ifndef IRT_ENABLE_REGION_INSTRUMENTATION
#define IRT_ENABLE_REGION_INSTRUMENTATION
#endif

//#define IRT_ENABLE_ENERGY_INSTRUMENTATION // leave deactivated, not working at the moment

#define IRT_DECLARE_PERFORMANCE_TABLE(__type__) \
	struct _irt_##__type__##_table { \
	uint32 size; \
	uint32 number_of_elements; \
	uint32 blocksize; \
	_irt_##__type__##_data* data; \
}; \
typedef struct _irt_##__type__##_table irt_##__table__##_table; \


#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
#include "papi.h"
#endif

// functions for creating and destroying performance tables

irt_pd_table* irt_create_performance_table(const unsigned blocksize);
irt_epd_table* irt_create_extended_performance_table(unsigned blocksize);

void irt_destroy_performance_table(irt_pd_table* table);
void irt_destroy_extended_performance_table(irt_epd_table* table);

// initialization functions

void irt_instrumentation_init_energy_instrumentation();

// private event handlers

void _irt_wi_instrumentation_event(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id);
void _irt_wg_instrumentation_event(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id);
void _irt_worker_instrumentation_event(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id);
void _irt_di_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id);

// debug output functions

void irt_instrumentation_output(irt_worker* worker);
void irt_extended_instrumentation_output(irt_worker* worker);
void irt_aggregated_instrumentation_output();

// instrumentation function pointer toggle functions

void irt_wi_toggle_instrumentation(bool enable);
void irt_wg_toggle_instrumentation(bool enable);
void irt_worker_toggle_instrumentation(bool enable);
void irt_di_toggle_instrumentation(bool enable);
void irt_region_toggle_instrumentation(bool enable);
void irt_all_toggle_instrumentation(bool enable);

// dummy functions to be used via function pointer to disable 
// instrumentation even if IRT_ENABLE_INSTRUMENTATION is set

void _irt_wi_no_instrumentation_event(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id);
void _irt_wg_no_instrumentation_event(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id);
void _irt_worker_no_instrumentation_event(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id);
void _irt_di_no_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id);

typedef uint64 region_id;

void _irt_instrumentation_region_start(region_id id);
void _irt_instrumentation_region_end(region_id id);

void irt_instrumentation_region_set_timestamp(irt_work_item* wi);
void irt_instrumentation_region_add_time(irt_work_item* wi);

