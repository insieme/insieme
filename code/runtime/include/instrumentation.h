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
//#define IRT_ENABLE_INSTRUMENTATION
#endif
#ifndef IRT_ENABLE_REGION_INSTRUMENTATION
//#define IRT_ENABLE_REGION_INSTRUMENTATION
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

irt_instrumentation_event_data_table* irt_inst_create_event_data_table();
irt_instrumentation_region_data_table* irt_inst_create_region_data_table();

void irt_inst_destroy_event_data_table(irt_instrumentation_event_data_table* table);
void irt_inst_destroy_region_data_table(irt_instrumentation_region_data_table* table);

// initialization functions

void irt_instrumentation_init_energy_instrumentation();

// private event handlers

void _irt_inst_insert_wi_event(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id);
void _irt_inst_insert_wg_event(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id);
void _irt_inst_insert_wo_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id);
void _irt_inst_insert_di_event(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id);
void _irt_inst_insert_db_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id);

// debug output functions

void irt_inst_event_data_output_single(irt_instrumentation_event_data data, FILE* outputfile, bool readable);
void irt_inst_event_data_output_all(bool binary_format);
void irt_inst_event_data_output(irt_worker* worker, bool binary_format);
void irt_inst_region_data_output(irt_worker* worker);
void irt_inst_aggregated_data_output();

// instrumentation function pointer toggle functions

void irt_inst_set_wi_instrumentation(bool enable);
void irt_inst_set_wg_instrumentation(bool enable);
void irt_inst_set_wo_instrumentation(bool enable);
void irt_inst_set_di_instrumentation(bool enable);
void irt_inst_set_db_instrumentation(bool enable);
void irt_inst_set_region_instrumentation(bool enable);
void irt_inst_set_all_instrumentation(bool enable);

// dummy functions to be used via function pointer to disable 
// instrumentation even if IRT_ENABLE_INSTRUMENTATION is set

void _irt_inst_insert_no_wi_event(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id);
void _irt_inst_insert_no_wg_event(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id);
void _irt_inst_insert_no_wo_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id);
void _irt_inst_insert_no_di_event(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id);
void _irt_inst_insert_no_db_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id);

typedef uint64 region_id;

void _irt_inst_region_start(region_id id);
void _irt_inst_region_end(region_id id);

void irt_inst_region_set_timestamp(irt_work_item* wi);
void irt_inst_region_add_time(irt_work_item* wi);

// some management operations
void irt_inst_init(irt_context* context);
void irt_inst_finalize(irt_context* context);

