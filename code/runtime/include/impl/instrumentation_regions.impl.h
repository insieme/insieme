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
 *
 */
#pragma once
#ifndef __GUARD_IMPL_INSTRUMENTATION_REGIONS_IMPL_H
#define __GUARD_IMPL_INSTRUMENTATION_REGIONS_IMPL_H

#include "instrumentation_regions.h"

#include <string.h>
#include <locale.h> // needed to use thousands separator
#include <stdio.h>
#ifndef _GEMS_SIM
#include <sys/stat.h>
#endif
#include <errno.h>

#include "utils/timing.h"
#include "abstraction/measurements.h"
#include "abstraction/impl/measurements.impl.h"
#include "abstraction/unused.h"
#include "utils/memory.h"
#include "impl/error_handling.impl.h"
#include "instrumentation_regions_includes.h"

// --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//
//																				Regions
//
// --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#ifdef IRT_ENABLE_REGION_INSTRUMENTATION

void _irt_inst_region_stack_push(irt_work_item* wi, irt_inst_region_context_data* region) {
	irt_inst_region_list* list = wi->inst_region_list;

	IRT_ASSERT(list->length <= list->size, IRT_ERR_INSTRUMENTATION, "WI region stack overflow, size %lu length %lu", list->size, list->length)

	if(list->length == list->size) {
		list->size *= 2;
		list->items = (irt_inst_region_context_data**)realloc(list->items, list->size * sizeof(irt_inst_region_context_data*));
	}

	list->items[list->length++] = region;
}

irt_inst_region_context_data* _irt_inst_region_stack_pop(irt_work_item* wi) {
	irt_inst_region_list* list = wi->inst_region_list;

	IRT_ASSERT(list->length > 0, IRT_ERR_INSTRUMENTATION, "Tried to remove a region from a WI that has no region!")

	irt_inst_region_context_data* retval = list->items[list->length - 1];
	// remove cur from stack
	list->items[--list->length] = NULL;
	return retval;
}

void _irt_inst_region_start_early_entry_measurements(irt_work_item* wi) {
	//	printf("current id: %u, current entries: %llu\n", irt_inst_region_get_current(wi)->id,
	// wi->wg_memberships[0].wg_id.cached->region_data_entries[irt_inst_region_get_current(wi)->id]);
	uint32 index = wi->wg_memberships[0].wg_id.cached->region_data_entries[irt_inst_region_get_current(wi)->id]++;
	volatile irt_inst_region_wi_data* rg =
	    &wi->wg_memberships[0].wg_id.cached->region_data[irt_inst_region_get_current(wi)->id][index % IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE];
	//	irt_inst_region_context_data* rg = irt_inst_region_get_current(wi);
	__irt_unused irt_context* context = irt_context_table_lookup(wi->context_id);
	#pragma GCC diagnostic push
	// ignore uninitialized variables in _local_var_decls__ that might only be used for starting measurements but not for ending
	#pragma GCC diagnostic ignored "-Wunused-variable"
	#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,              \
	              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                        \
		_local_var_decls__;                                                                                                                                    \
		if(irt_g_inst_region_metric_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */                        \
			_region_early_start_code__;                                                                                                                        \
		}
	#include "irt_metrics.def"
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		if(irt_g_inst_region_metric_measure_##_name__) { _region_early_start_code__; }
	#include "irt_metrics.def"
	//	printf("region %u %u start: last %llu at %p\n", irt_inst_region_get_current(wi)->id, index, rg->last_wall_time, (void*) &rg->last_wall_time);
}
#pragma GCC diagnostic pop // needs to be done after ending the function scope

void _irt_inst_region_end_late_exit_measurements(irt_work_item* wi) {
	irt_inst_region_list* list = wi->inst_region_list;
	uint64 length = list->length;
	IRT_ASSERT(length > 0, IRT_ERR_INSTRUMENTATION, "Tried to get region data from a WI that has no region data")
	uint32 index = wi->wg_memberships[0].wg_id.cached->region_data_exits[irt_inst_region_get_current(wi)->id]++;
	volatile irt_inst_region_wi_data* rg =
	    &wi->wg_memberships[0].wg_id.cached->region_data[irt_inst_region_get_current(wi)->id][index % IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE];
	//	printf("region %u %u end start: last %llu\n", irt_inst_region_get_current(wi)->id, index, rg->last_wall_time);
	__irt_unused irt_context* context = irt_context_table_lookup(wi->context_id);
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		_data_type__ old_aggregated_##_name__ = rg->aggregated_##_name__;
	#include "irt_metrics.def"
	#pragma GCC diagnostic push
	// ignore uninitialized variables in _local_var_decls__ that might only be used for starting measurements but not for ending
	#pragma GCC diagnostic ignored "-Wunused-variable"
	#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,              \
	              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                        \
		_local_var_decls__;                                                                                                                                    \
		if(irt_g_inst_region_metric_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */                        \
			_region_late_end_code__;                                                                                                                           \
		}
	#include "irt_metrics.def"
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		if(irt_g_inst_region_metric_measure_##_name__) {                                                                                                       \
			_region_late_end_code__;                                                                                                                           \
			rg->last_##_name__ = 0;                                                                                                                            \
		}
	#include "irt_metrics.def"
	//	printf("region %u %u end end: aggregated %llu, using last %p\n", irt_inst_region_get_current(wi)->id, index, rg->aggregated_wall_time, (void*)
	//&rg->last_wall_time);
	irt_inst_region_context_data* current_region = irt_inst_region_get_current(wi);
	irt_spin_lock(&(current_region->lock));
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		/* only propagate metrics that were touched by late exit measurements (i.e. only EE/LE metrics) */                                                     \
		if(rg->aggregated_##_name__ != old_aggregated_##_name__) {                                                                                             \
			switch(_aggregation__) {                                                                                                                           \
			case IRT_METRIC_AGGREGATOR_AVG:                                                                                                                    \
				current_region->aggregated_##_name__ =                                                                                                         \
				    (current_region->aggregated_##_name__ * current_region->num_executions + rg->aggregated_##_name__) / (current_region->num_executions + 1); \
				break;                                                                                                                                         \
			case IRT_METRIC_AGGREGATOR_NONE: current_region->aggregated_##_name__ = rg->aggregated_##_name__; break;                                           \
			case IRT_METRIC_AGGREGATOR_SUM:                                                                                                                    \
			default: current_region->aggregated_##_name__ += rg->aggregated_##_name__;                                                                         \
			}                                                                                                                                                  \
			/*current_region->aggregated_##_name__ += rg->aggregated_##_name__;*/ /* current_region->aggregated_##_name__ += rg->aggregated_##_name__ -        \
			                                                                         old_aggregated_##_name__;*/                                               \
			rg->aggregated_##_name__ = 0;                                                                                                                      \
		}
	#include "irt_metrics.def"
	irt_spin_unlock(&(current_region->lock));
}
#pragma GCC diagnostic pop // needs to be done after ending the function scope

void _irt_inst_region_metrics_init(irt_context* context) {
	// initialize IDs
	int metric_id = 0;
	int group_id = 0;
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		irt_g_region_metric_##_name__##_id = metric_id++;
	#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,              \
	              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                        \
		irt_g_region_metric_group_##_name__##_id = group_id++;
	#include "irt_metrics.def"
	irt_g_inst_region_metric_count = metric_id;
	irt_g_inst_region_metric_group_count = group_id;
	// initialize groups
	#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,              \
	              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                        \
		_init_code__;
	#include "irt_metrics.def"
}


void _irt_inst_region_metrics_init_worker(irt_worker* worker) {
// thread-specific initialization of groups
#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,                  \
              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                            \
	_init_code_worker__;
#include "irt_metrics.def"
}

void _irt_inst_region_metrics_finalize(irt_context* context) {
// finalize groups
#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,                  \
              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                            \
	_finalize_code__;
#include "irt_metrics.def"
}

void _irt_inst_region_metrics_finalize_worker(irt_worker* worker) {
// thread-specific finalization of groups
#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,                  \
              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                            \
	_finalize_code_worker__;
#include "irt_metrics.def"
}

void irt_inst_region_list_copy(irt_work_item* destination, irt_work_item* source) {
	irt_inst_region_list* src = source->inst_region_list;
	if(src->length > 0) {
		irt_inst_region_list* dst = destination->inst_region_list;

		if(dst->size < src->size) {
			dst->size = src->size;
			dst->items = (irt_inst_region_context_data**)realloc(dst->items, sizeof(irt_inst_region_context_data*) * dst->size);
		}

		dst->length = src->length;
		memcpy(dst->items, src->items, sizeof(irt_inst_region_context_data*) * dst->length);
	}
}

void irt_inst_region_wi_init(irt_work_item* wi) {
	wi->inst_region_list = (irt_inst_region_list*)malloc(sizeof(irt_inst_region_list));
	wi->inst_region_list->size = 8;
	wi->inst_region_list->length = 0;
	wi->inst_region_list->items = (irt_inst_region_context_data**)malloc(sizeof(irt_inst_region_context_data*) * wi->inst_region_list->size);
	wi->inst_region_data = (irt_inst_region_wi_data*)malloc(sizeof(irt_inst_region_wi_data));
	wi->inst_region_data->region_entries = 0;
	wi->inst_region_data->region_exits = 0;
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		wi->inst_region_data->last_##_name__ = 0;                                                                                                              \
		wi->inst_region_data->aggregated_##_name__ = 0;
	#include "irt_metrics.def"
}

void irt_inst_region_wi_finalize(irt_work_item* wi) {
	free(wi->inst_region_list->items);
	free(wi->inst_region_list);
	free(wi->inst_region_data);
}

void irt_inst_region_wg_init(irt_work_group* wg) {
	irt_context* context = irt_context_get_current();
	wg->regions_ended = 0;
	wg->regions_started = 0;
	wg->region_data = (volatile irt_inst_region_wi_data**)malloc(sizeof(irt_inst_region_wi_data*) * context->num_regions);
	wg->region_data_entries = (volatile uint64*)malloc(sizeof(uint64) * context->num_regions);
	wg->region_data_exits = (volatile uint64*)malloc(sizeof(uint64) * context->num_regions);
	memset((void*)wg->region_completions_required, 0, sizeof(uint64) * IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE);
	memset((void*)wg->region_data_entries, 0, sizeof(uint64) * context->num_regions);
	memset((void*)wg->region_data_exits, 0, sizeof(uint64) * context->num_regions);
	for(uint32 i = 0; i < context->num_regions; ++i) {
		wg->region_data[i] = (irt_inst_region_wi_data*)malloc(sizeof(irt_inst_region_wi_data) * IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE);
		memset((void*)wg->region_data[i], 0, sizeof(irt_inst_region_wi_data) * IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE);
	}
}

void irt_inst_region_wg_finalize(irt_work_group* wg) {
	free((void*)wg->region_data_entries);
	free((void*)wg->region_data_exits);
	irt_context* context = irt_context_get_current();
	for(uint32 i = 0; i < context->num_regions; ++i) {
		free((void*)wg->region_data[i]);
	}
	free(wg->region_data);
}

void irt_inst_region_init(irt_context* context) {
	context->inst_region_data = (irt_inst_region_context_data*)malloc(context->num_regions * sizeof(irt_inst_region_context_data));
	for(uint32 i = 0; i < context->num_regions; ++i) {
		context->inst_region_data[i].id = i;
		context->inst_region_data[i].num_executions = 0;
		irt_spin_init(&context->inst_region_data[i].lock);
		#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,           \
		               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                         \
			context->inst_region_data[i].last_##_name__ = 0;                                                                                                   \
			context->inst_region_data[i].aggregated_##_name__ = 0;
		#include "irt_metrics.def"
	}
	_irt_inst_region_metrics_init(context);
	irt_inst_region_select_metrics_from_env();
}

void irt_inst_region_init_worker(irt_worker* worker) {
	_irt_inst_region_metrics_init_worker(worker);
}

void irt_inst_region_finalize(irt_context* context) {
	_irt_inst_region_metrics_finalize(context);
	irt_time_ticks_per_sec_calibration_mark(); // needs to be done before any time instrumentation processing!
	irt_inst_region_output();
	// irt_inst_region_debug_output();
	for(uint32 i = 0; i < context->num_regions; ++i) {
		irt_spin_destroy(&context->inst_region_data[i].lock);
	}
	free(context->inst_region_data);
}

void irt_inst_region_finalize_worker(irt_worker* worker) {
	_irt_inst_region_metrics_finalize_worker(worker);
}

void irt_inst_region_propagate_data_from_wi_to_regions(irt_work_item* wi) {
	irt_inst_region_list* list = wi->inst_region_list;

	for(uint64 i = 0; i < list->length; ++i) {
		irt_inst_region_context_data* cur_region = list->items[i];

		IRT_ASSERT(cur_region, IRT_ERR_INSTRUMENTATION, "Tried to get region data from a WI that has no region data")

		irt_spin_lock(&(cur_region->lock));
		#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,           \
		               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                         \
			switch(_aggregation__) {                                                                                                                           \
			case IRT_METRIC_AGGREGATOR_NONE:                                                                                                                   \
				/* special case e.g. for region-only instrumentation that do not require WI measurements or aggregation */                                     \
				break;                                                                                                                                         \
			case IRT_METRIC_AGGREGATOR_AVG:                                                                                                                    \
				cur_region->aggregated_##_name__ =                                                                                                             \
				    (cur_region->aggregated_##_name__ * cur_region->num_executions + wi->inst_region_data->aggregated_##_name__)                               \
				    / (cur_region->num_executions + 1);                                                                                                        \
				break;                                                                                                                                         \
			case IRT_METRIC_AGGREGATOR_SUM:                                                                                                                    \
			default: cur_region->aggregated_##_name__ += wi->inst_region_data->aggregated_##_name__;                                                           \
			}
		#include "irt_metrics.def"
		irt_spin_unlock(&(cur_region->lock));
	}
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		wi->inst_region_data->aggregated_##_name__ = 0;
	#include "irt_metrics.def"
}

void irt_inst_region_start_measurements(irt_work_item* wi) {
	if(wi->inst_region_list->length <= 0) { return; }

	__irt_unused irt_context* context = irt_context_table_lookup(wi->context_id);
	#pragma GCC diagnostic push
	// ignore uninitialized variables in _local_var_decls__ that might only be used for starting measurements but not for ending
	#pragma GCC diagnostic ignored "-Wunused-variable"
	#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,              \
	              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                        \
		_local_var_decls__;                                                                                                                                    \
		if(irt_g_inst_region_metric_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */                        \
			_wi_start_code__;                                                                                                                                  \
		}
	#include "irt_metrics.def"
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		if(wi->inst_region_data->last_##_name__ != 0) {                                                                                                        \
			printf("Instrumentation: Encountered start measurements for region %" PRIu64 " but last_" #_name__ " was not zero\n",                              \
			       irt_inst_region_get_current(wi)->id);                                                                                                       \
			exit(1);                                                                                                                                           \
		}                                                                                                                                                      \
		IRT_ASSERT(wi->inst_region_data->last_##_name__ == 0, IRT_ERR_INSTRUMENTATION,                                                                         \
		           "Started measurement, but previous measurement still in progress (last_%s was not 0)", #_name__)                                            \
		if(irt_g_inst_region_metric_measure_##_name__) { _wi_start_code__; }
	#include "irt_metrics.def"
}
#pragma GCC diagnostic pop // needs to be done after ending the function scope

void irt_inst_region_end_measurements(irt_work_item* wi) {
	if(wi->inst_region_list->length <= 0) { return; }

	__irt_unused irt_context* context = irt_context_table_lookup(wi->context_id);

	#pragma GCC diagnostic push
	// ignore uninitialized variables in _local_var_decls__ that might only be used for starting measurements but not for ending
	#pragma GCC diagnostic ignored "-Wunused-variable"
	#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,              \
	              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                        \
		_local_var_decls__;                                                                                                                                    \
		if(irt_g_inst_region_metric_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */                        \
			wi_end_code__;                                                                                                                                     \
		}
	#include "irt_metrics.def"
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		if(irt_g_inst_region_metric_measure_##_name__) {                                                                                                       \
			wi_end_code__;                                                                                                                                     \
			wi->inst_region_data->last_##_name__ = 0;                                                                                                          \
		}
	#include "irt_metrics.def"
}
#pragma GCC diagnostic pop // needs to be done after ending the function scope

// start a region and push it to the region stack of the WI
void irt_inst_region_start(const irt_inst_region_id id) {
	irt_work_item* wi = irt_wi_get_current();
	irt_context* context = irt_context_table_lookup(wi->context_id);
	irt_inst_region_context_data* outer_region = irt_inst_region_get_current(wi);

	IRT_ASSERT(id >= 0 && id < context->num_regions, IRT_ERR_INSTRUMENTATION, "Start of region id %lu requested, but only %u region(s) present", id,
	           context->num_regions)
	IRT_ASSERT(id < IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE - 1, IRT_ERR_INSTRUMENTATION,
	           "Start of region id %lu requested, but IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE is only %d (needs to be id + 1)", id,
	           IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE)
	irt_inst_region_context_data* inner_region = &(context->inst_region_data[id]);

	if(outer_region) {
		IRT_ASSERT(outer_region != inner_region, IRT_ERR_INSTRUMENTATION, "Region %u start encountered, but this region was already started", id)
		irt_inst_region_end_measurements(wi);
		irt_inst_region_propagate_data_from_wi_to_regions(wi);
	}

	IRT_ASSERT(wi->num_groups > 0, IRT_ERR_INSTRUMENTATION, "Encountered a WI that is not member of any group")

	wi->inst_region_data->region_entries++;

	// lock while ring buffer is full
	while(wi->inst_region_data->region_entries - wi->wg_memberships[0].wg_id.cached->regions_ended >= IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE)
		;

	if(irt_atomic_bool_compare_and_swap(&wi->wg_memberships[0].wg_id.cached->regions_started, wi->inst_region_data->region_entries - 1,
	                                    wi->inst_region_data->region_entries, uint64)) {
		// first
		_irt_inst_region_stack_push(wi, inner_region);
		_irt_inst_region_start_early_entry_measurements(wi);
		wi->wg_memberships[0]
		    .wg_id.cached->region_completions_required[wi->inst_region_data->region_entries % IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE] =
		    wi->wg_memberships[0].wg_id.cached->local_member_count;
	} else {
		// all others
		_irt_inst_region_stack_push(wi, inner_region);
	}

	irt_inst_region_start_measurements(wi);
}

// stop a region and remove it from the region stack of the WI
void irt_inst_region_end(const irt_inst_region_id id) {
	irt_work_item* wi = irt_wi_get_current();
	irt_context* context = irt_context_table_lookup(wi->context_id);
	irt_inst_region_context_data* inner_region = irt_inst_region_get_current(wi);

	IRT_ASSERT(id >= 0 && id < context->num_regions, IRT_ERR_INSTRUMENTATION, "End of region id %lu requested, but only %u region(s) present", id,
	           context->num_regions)
	IRT_ASSERT(inner_region, IRT_ERR_INSTRUMENTATION, "Region end occurred while no region was started")
	IRT_ASSERT(inner_region->id == id, IRT_ERR_INSTRUMENTATION, "Region end id %lu did not match currently open region id %lu", id, inner_region->id)

	irt_inst_region_end_measurements(wi);
	irt_inst_region_propagate_data_from_wi_to_regions(wi);

	IRT_ASSERT(wi->num_groups > 0, IRT_ERR_INSTRUMENTATION, "Encountered a WI that is not member of any group")

	irt_inst_region_context_data* outer_region = NULL;
	if(wi->inst_region_list->length > 1) { outer_region = wi->inst_region_list->items[wi->inst_region_list->length - 2]; }

	// only increase count if wi is not member in any work group or has wg id == 0
	if(wi->num_groups == 0 || (wi->num_groups > 0 && wi->wg_memberships[0].num == 0)) { inner_region->num_executions++; }

	wi->inst_region_data->region_exits++;

	// lock until at least one WI has started the region
	while(wi->wg_memberships[0].wg_id.cached->region_completions_required[wi->inst_region_data->region_exits % IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE]
	      <= 0)
		;

	if(irt_atomic_sub_and_fetch(&wi->wg_memberships[0].wg_id.cached->region_completions_required[wi->inst_region_data->region_exits
	                                                                                             % IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE],
	                            1, uint64)
	   == 0) {
		// last
		_irt_inst_region_end_late_exit_measurements(wi);
		_irt_inst_region_stack_pop(wi);
		irt_atomic_inc(&wi->wg_memberships[0].wg_id.cached->regions_ended, uint64);
	} else {
		// all others
		_irt_inst_region_stack_pop(wi);
	}

	if(outer_region) { irt_inst_region_start_measurements(wi); }
}

// selectively enable region instrumentation metrics - NOTE: a NULL pointer or an empty string as an argument will enable all metrics!
void irt_inst_region_select_metrics(const char* selection) {
	char enabled_types[4096];
	uint16 enabled_types_counter = 0;

	if(!selection || strcmp(selection, "") == 0) {
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		irt_g_inst_region_metric_measure_##_name__ = true;                                                                                                     \
		irt_g_inst_region_metric_group_##_group__##membership_count++;                                                                                         \
		enabled_types_counter += sprintf(&(enabled_types[enabled_types_counter]), "%s,", #_name__);
	#include "irt_metrics.def"
	} else {
	// need to copy string since strtok requires it to be non-const
	#ifdef _GEMS_SIM
		char selection_copy[512];
		#else
		char selection_copy[strlen(selection) + 1];
		#endif
		strcpy(selection_copy, selection);
		// tokenize
		char* tok = strtok(selection_copy, ",");

		do {
		#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,           \
		               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                         \
			if(strcmp(tok, #_name__) == 0) {                                                                                                                   \
				irt_g_inst_region_metric_measure_##_name__ = true;                                                                                             \
				irt_g_inst_region_metric_group_##_group__##membership_count++;                                                                                 \
				enabled_types_counter += sprintf(&(enabled_types[enabled_types_counter]), "%s,", #_name__);                                                    \
			}
		#include "irt_metrics.def"
		} while((tok = strtok(NULL, ",")) != NULL);
	}

	// remove the last comma and replace with termination symbol
	enabled_types[enabled_types_counter - 1] = '\0';
	irt_log_setting_s(IRT_INST_REGION_INSTRUMENTATION_TYPES_ENV, enabled_types);
}

void irt_inst_region_select_metrics_from_env() {
#ifdef _GEMS
	irt_inst_region_select_metrics(GEMS_IRT_INST_REGION_INSTRUMENTATION_TYPES);
	#else
	if(getenv(IRT_INST_REGION_INSTRUMENTATION_ENV) && strcmp(getenv(IRT_INST_REGION_INSTRUMENTATION_ENV), "enabled") == 0) {
		irt_log_setting_s(IRT_INST_REGION_INSTRUMENTATION_ENV, "enabled");

		char* metrics = getenv(IRT_INST_REGION_INSTRUMENTATION_TYPES_ENV);
		irt_inst_region_select_metrics(metrics);
	} else {
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		irt_g_inst_region_metric_measure_##_name__ = false;                                                                                                    \
		irt_g_inst_region_metric_group_##_group__##membership_count = 0;
	#include "irt_metrics.def"
		irt_log_setting_s(IRT_INST_REGION_INSTRUMENTATION_ENV, "disabled");
	}
	#endif
}

void irt_inst_region_debug_output() {
	irt_context* context = irt_context_get_current();
	uint32 num_regions = context->num_regions;
	printf("%u region(s):\n", num_regions);
	for(uint32 i = 0; i < num_regions; ++i) {
		irt_inst_region_context_data* rg = &context->inst_region_data[i];
		printf("region %u\n", i);
		#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,           \
		               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                         \
			if(irt_g_inst_region_metric_measure_##_name__) {                                                                                                   \
				printf("  aggregated_" #_name__ ":\t" _format_string__ ", last_" #_name__ ":\t" _format_string__,                                              \
				       (_data_type__)((double)rg->aggregated_##_name__ * _output_conversion_code__), rg->last_##_name__);                                      \
				if(rg->last_##_name__ != 0) printf(", warning, field last_" #_name__ " was not 0");                                                            \
				printf("\n");                                                                                                                                  \
			}
		#include "irt_metrics.def"
	}
}

void irt_inst_region_output() {
	FILE* outputfile = stderr;
	char outputfilename[IRT_INST_OUTPUT_PATH_CHAR_SIZE] = "";
	char defaultoutput[] = ".";
	char* outputprefix = defaultoutput;
	if(getenv(IRT_INST_OUTPUT_PATH_ENV)) { outputprefix = getenv(IRT_INST_OUTPUT_PATH_ENV); }

	#if !defined(_GEMS_SIM) && !defined(IRT_INSTRUMENTATION_OUTPUT_TO_STDERR)
	struct stat st;
	int stat_retval = stat(outputprefix, &st);
	if(stat_retval != 0) { mkdir(outputprefix, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH); }

	IRT_ASSERT(stat(outputprefix, &st) == 0, IRT_ERR_INSTRUMENTATION, "Error using directory %s for region data: %s", outputprefix, strerror(errno));

	sprintf(outputfilename, "%s/worker_efficiency.log", outputprefix);
#ifdef IRT_USE_MPI
	sprintf(outputfilename + strlen(outputfilename), ".%s", getenv("OMPI_COMM_WORLD_RANK"));
#endif

	outputfile = fopen(outputfilename, "w");
	IRT_ASSERT(outputfile != 0, IRT_ERR_INSTRUMENTATION, "Unable to open region data file for writing: %s", strerror(errno));
	#endif

	irt_context* context = irt_context_get_current();
	uint32 num_regions = context->num_regions;
	irt_inst_region_context_data* regions = context->inst_region_data;

	// write header
	fprintf(outputfile, "#subject,id,num_executions(unit)");
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		if(irt_g_inst_region_metric_measure_##_name__) { fprintf(outputfile, "," #_name__ "(%s)", #_unit__); }
	#include "irt_metrics.def"
	fprintf(outputfile, "\n");

	// write data
	for(uint32 i = 0; i < num_regions; ++i) {
		fprintf(outputfile, "RG,%u,%" PRIu64, i, regions[i].num_executions);
		#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,           \
		               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                         \
			if(irt_g_inst_region_metric_measure_##_name__) {                                                                                                   \
				fprintf(outputfile, "," _format_string__, (_data_type__)((double)regions[i].aggregated_##_name__ * _output_conversion_code__));                \
			}
		#include "irt_metrics.def"
		fprintf(outputfile, "\n");
	}
	#if !defined(_GEMS_SIM) && !defined(IRT_INSTRUMENTATION_OUTPUT_TO_STDERR)
	fclose(outputfile);
	#endif
}

inline irt_inst_region_context_data* irt_inst_region_get_current(irt_work_item* wi) {
	if(wi->inst_region_list->length < 1) { return NULL; }
	return wi->inst_region_list->items[wi->inst_region_list->length - 1];
}

// stop a region, do not remove it from the region stack of the WI
// void irt_inst_region_suspend(irt_work_item* wi) {
//	irt_inst_region_end_measurements(wi);
//	irt_inst_region_propagate_data_from_wi_to_regions(wi);
//}
//
//// resume the current region
// void irt_inst_region_continue(irt_work_item* wi) {
//	// resume the current region (since it was not removed by the last suspend)
//	irt_inst_region_list list = wi->inst_region_list;
//	if(list.length > 0)
//		irt_inst_region_start(wi, list.items[list.length-1]);
//}

#else // if not IRT_ENABLE_REGION_INSTRUMENTATION

void irt_inst_region_list_copy(irt_work_item* destination, irt_work_item* source) {}
void irt_inst_region_wi_init(irt_work_item* wi) {}
void irt_inst_region_wi_finalize(irt_work_item* wi) {}
void irt_inst_region_wg_init(irt_work_group* wg) {}
void irt_inst_region_wg_finalize(irt_work_group* wg) {}
void irt_inst_region_init(irt_context* context) {}
void irt_inst_region_init_worker(irt_worker* worker) {}
void irt_inst_region_finalize(irt_context* context) {}
void irt_inst_region_finalize_worker(irt_worker* worker) {}
void irt_inst_region_propagate_data_from_wi_to_regions(irt_work_item* wi) {}

void irt_inst_region_start_measurements(irt_work_item* wi) {
#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	irt_worker* self = irt_worker_get_current();
	struct timespec ts;
	clock_gettime(self->clockid, &ts);
	self->app_time_last_start = ts.tv_sec * 1000000.0 + ts.tv_nsec / 1000.0;
	self->app_time_running = true;
#endif // IRT_ENABLE_APP_TIME_ACCOUNTING
}

void irt_inst_region_end_measurements(irt_work_item* wi) {
#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	irt_worker* self = irt_worker_get_current();
	struct timespec ts;
	clock_gettime(self->clockid, &ts);
	self->app_time_total += (ts.tv_sec * 1000000.0 + ts.tv_nsec / 1000.0) - self->app_time_last_start;
	self->app_time_running = false;
#endif // IRT_ENABLE_APP_TIME_ACCOUNTING
}

void irt_inst_region_start(irt_inst_region_id id) {}
void irt_inst_region_end(irt_inst_region_id id) {}
void irt_inst_region_select_metrics(const char* selection) {}
void irt_inst_region_select_metrics_from_env() {}
void irt_inst_region_debug_output() {}
void irt_inst_region_output() {}
irt_inst_region_context_data* irt_inst_region_get_current(irt_work_item* wi) {
	return NULL;
}

#endif // IRT_ENABLE_REGION_INSTRUMENTATION

#endif // ifndef __GUARD_IMPL_INSTRUMENTATION_REGIONS_IMPL_H
