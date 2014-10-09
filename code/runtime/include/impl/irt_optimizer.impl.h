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
#ifndef __GUARD_IMPL_IRT_OPTIMIZER_IMPL_H
#define __GUARD_IMPL_IRT_OPTIMIZER_IMPL_H

#include "utils/timing.h"
#include "irt_optimizer.h"
#include "impl/irt_context.impl.h"
#include "meta_information/meta_infos.h"

#include "optimizers/opencl_optimizer.h"
#include "optimizers/shared_mem_effort_estimate_external_load_optimizer.h"

void irt_optimizer_objective_init(irt_context *context);
void irt_optimizer_objective_destroy(irt_context *context);

void irt_optimizer_context_destroy(irt_context *context) {
    irt_optimizer_objective_destroy(context);
}

#ifndef IRT_RUNTIME_TUNING

void irt_optimizer_context_startup(irt_context *context) {
    irt_optimizer_objective_init(context);
}

void irt_optimizer_starting_pfor(irt_wi_implementation *impl, irt_work_item_range range, irt_work_group* group) { }
void irt_optimizer_completed_pfor(irt_wi_implementation *impl, uint64 time, irt_loop_sched_data *sched_data) { }

#else // ifndef IRT_RUNTIME_TUNING

///////////////////////////////////// Context =========================================================================

void irt_optimizer_context_startup(irt_context *context) {
    irt_optimizer_objective_init(context);
	irt_shared_mem_effort_estimate_external_load_optimizer_context_startup(context);
	irt_opencl_optimizer_context_startup(context); // OpenCL startup
}

///////////////////////////////////// Loops ===========================================================================

void irt_optimizer_starting_pfor(irt_wi_implementation *impl, irt_work_item_range range, irt_work_group* group) {

	if(irt_meta_info_is_opencl_available(impl->variants[0].meta_info) && irt_meta_info_get_opencl(impl->variants[0].meta_info)->opencl) {
		irt_opencl_optimizer_starting_pfor(impl, range, group);
	} else {
		irt_shared_mem_effort_estimate_external_load_optimizer_starting_pfor(impl, range, group);
	}
}

#ifndef IRT_RUNTIME_TUNING_EXTENDED

void irt_optimizer_completed_pfor(irt_wi_implementation *impl, uint64 walltime, irt_loop_sched_data* sched_data) { }

#else

void irt_optimizer_completed_pfor(irt_wi_implementation *impl, irt_work_item_range range, uint64 total_time, irt_loop_sched_data *sched_data) {
	if(impl->features.opencl) {
		// nothing
	} else {
		irt_shared_mem_effort_estimate_external_load_optimizer_completed_pfor(impl, range, total_time, sched_data);
	}
}

#endif

#endif // ifndef IRT_RUNTIME_TUNING

uint64_t irt_optimizer_pick_in_range(uint64_t max) {
    /* according to enclosing region goals, pick a value */

    return max -1;
}

#ifdef IRT_ENABLE_OMPP_OPTIMIZER

uint32 irt_g_available_freqs[128];
uint32 irt_g_available_freq_count = UINT_MAX;
#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DVFS_EVAL
float irt_g_dvfs_eval_energy[128];
float irt_g_dvfs_eval_time[128];
int irt_g_dvfs_eval_count[128];
#endif

void get_available_freqs() {
    if(!irt_cpu_freq_get_available_frequencies(irt_g_available_freqs, &irt_g_available_freq_count))
        return;

    // scaling_available_frequencies not available
    
    uint32 max = irt_cpu_freq_get_max_frequency_core(0);
    uint32 min = irt_cpu_freq_get_min_frequency_core(0);

    irt_g_available_freq_count = 0;
    do {
        irt_g_available_freqs[irt_g_available_freq_count++] = max;
        max -= 100;
    } while(max >= min);
}


void irt_optimizer_objective_init(irt_context *context) {
    for(int i=0; i<context->impl_table_size; i++) { 
        for(int j=0; j<context->impl_table[i].num_variants; j++) { 
            irt_spin_init(&context->impl_table[i].variants[j].rt_data.optimizer_rt_data.spinlock);
            context->impl_table[i].variants[j].rt_data.optimizer_rt_data.best.full = ULONG_MAX;

            irt_optimizer_wi_data* new_element = calloc(1, sizeof(irt_optimizer_wi_data));
            // conservative approach at the beginning
            new_element->id.frequency = 1; // let's skip freq 0 (turbo boost (?))
            new_element->id.thread_count = irt_g_worker_count;
            irt_optimizer_wi_data_table_insert_impl(context->impl_table[i].variants[j].rt_data.optimizer_rt_data.irt_g_optimizer_wi_data_table, NULL, new_element);   
            context->impl_table[i].variants[j].rt_data.optimizer_rt_data.cur.full = new_element->id.full;
            context->impl_table[i].variants[j].rt_data.optimizer_rt_data.cur_resources.wall_time = 0;
            context->impl_table[i].variants[j].rt_data.optimizer_rt_data.cur_resources.cpu_time = 0;
            context->impl_table[i].variants[j].rt_data.optimizer_rt_data.cur_resources.power = 0;
            context->impl_table[i].variants[j].rt_data.optimizer_rt_data.cur_resources.cpu_energy = 0;
        }
    }

    get_available_freqs();
}

void irt_optimizer_objective_destroy(irt_context *context) {
    for(int i=0; i<context->impl_table_size; i++) { 
        for(int j=0; j<context->impl_table[i].num_variants; j++) { 
            irt_spin_destroy(&context->impl_table[i].variants[j].rt_data.optimizer_rt_data.spinlock);
            for(int k=0; k<IRT_OPTIMIZER_LT_BUCKETS; ++k) {
                irt_optimizer_wi_data *element = context->impl_table[i].variants[j].rt_data.optimizer_rt_data.irt_g_optimizer_wi_data_table[k], *temp;
                while(element) {
                    temp = element->lookup_table_next;
                    element->lookup_table_next = NULL;
                    free(element);
                    element = temp;
                }
                context->impl_table[i].variants[j].rt_data.optimizer_rt_data.irt_g_optimizer_wi_data_table[k] = NULL;
            }
        }
    }
}

void irt_optimizer_compute_optimizations(irt_wi_implementation_variant* variant, irt_work_item * wi, bool force_computation) {
    if(!variant->meta_info || (variant->meta_info->ompp_objective.region_id == UINT_MAX && !variant->rt_data.wrapping_optimizer_rt_data))
        return;

    irt_worker* self = irt_worker_get_current();
    irt_context* context = irt_context_get_current();
    irt_inst_region_context_data* regions = context->inst_region_data;
    
    irt_spin_lock(&variant->rt_data.optimizer_rt_data.spinlock);

    /* We update our optimization values every round of #workers parallel executions of the work item 
     * This applies only to parallel construct (force_computation == false)
     * For now a ir_parallel call produces exactly #workers wi instances 
     * hence we enter this if construct only after a region execution. If not the case
     * instrumentation readings would be meaningless.
     */
    variant->rt_data.completed_wi_count ++;
    if(variant->meta_info->ompp_objective.region_id != UINT_MAX) {
#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT
        if(force_computation || variant->rt_data.completed_wi_count == irt_g_active_worker_count) {
#else
        if(force_computation || variant->rt_data.completed_wi_count == irt_g_worker_count) {
#endif
            variant->rt_data.completed_wi_count = 0;

            // collecting data                          
                                                        
            irt_optimizer_resources cur_resources;
 
            #define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	        if(irt_g_inst_region_metric_measure_cpu_energy) { \
                cur_resources.cpu_energy = (_data_type__)((double)regions[variant->meta_info->ompp_objective.region_id].aggregated_cpu_energy * _output_conversion_code__); \
                /*cpu_energy *= wi_count; */ \
                /*cpu_energy /= (irt_get_num_cores_per_socket() < irt_g_worker_count) ? irt_get_num_cores_per_socket() : irt_g_worker_count;*/ \
	        }
            #define ISOLATE_METRIC
            #define ISOLATE_CPU_ENERGY
            #include "irt_metrics.def"

            #define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	        if(irt_g_inst_region_metric_measure_cpu_time) { \
                cur_resources.cpu_time = (_data_type__)((double)regions[variant->meta_info->ompp_objective.region_id].aggregated_cpu_time * _output_conversion_code__); \
	        }
            #define ISOLATE_METRIC
            #define ISOLATE_CPU_TIME
            #include "irt_metrics.def"


            #define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	        if(irt_g_inst_region_metric_measure_wall_time) { \
                cur_resources.wall_time = (_data_type__)((double)regions[variant->meta_info->ompp_objective.region_id].aggregated_wall_time * _output_conversion_code__); \
	        }
            #define ISOLATE_METRIC
            #define ISOLATE_WALL_TIME
            #include "irt_metrics.def"

#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DVFS_EVAL
            if(variant->rt_data.optimizer_rt_data.cur_resources.cpu_energy) {
                irt_g_dvfs_eval_energy[variant->rt_data.optimizer_rt_data.cur.frequency] += cur_resources.cpu_energy - variant->rt_data.optimizer_rt_data.cur_resources.cpu_energy;
                irt_g_dvfs_eval_time[variant->rt_data.optimizer_rt_data.cur.frequency] += cur_resources.wall_time - variant->rt_data.optimizer_rt_data.cur_resources.wall_time;
                if(irt_g_dvfs_eval_count[irt_g_available_freq_count -1] == IRT_OMPP_OPTIMIZER_DVFS_EVAL_STEPS) {
                    printf("frequency: energy - time - power (num of executions)\n");
                    for(int k=0; k<irt_g_available_freq_count; k++)
                        printf("%d: %f - %f - %f (%d) ", k, irt_g_dvfs_eval_energy[k] / irt_g_dvfs_eval_count[k], irt_g_dvfs_eval_time[k] / irt_g_dvfs_eval_count[k], (irt_g_dvfs_eval_energy[k] ) / (irt_g_dvfs_eval_time[k] / 1000000000), irt_g_dvfs_eval_count[k]);
                    printf("\n");
                }
            }
            else if(irt_g_dvfs_eval_count[variant->rt_data.optimizer_rt_data.cur.frequency])
                irt_g_dvfs_eval_count[variant->rt_data.optimizer_rt_data.cur.frequency] --;
#endif
	
            // Update best
            if(variant->rt_data.optimizer_rt_data.cur_resources.cpu_energy) {
                if(variant->rt_data.optimizer_rt_data.best.full == ULONG_MAX || cur_resources.cpu_energy - variant->rt_data.optimizer_rt_data.cur_resources.cpu_energy < variant->rt_data.optimizer_rt_data.best_resources.cpu_energy) {
                    variant->rt_data.optimizer_rt_data.best_resources.cpu_energy = cur_resources.cpu_energy - variant->rt_data.optimizer_rt_data.cur_resources.cpu_energy;
                    variant->rt_data.optimizer_rt_data.best = variant->rt_data.optimizer_rt_data.cur;
                    //printf("next freq %d next thread count %d\n", variant->rt_data.optimizer_rt_data.best.frequency, variant->rt_data.optimizer_rt_data.best.thread_count);
                }
            }

            // Computing new settings

            variant->rt_data.optimizer_rt_data.cur_resources = cur_resources;

#ifndef IRT_ENABLE_OMPP_OPTIMIZER_DVFS_EVAL
            // let's just stick to the best after some iterations
            if(regions[variant->meta_info->ompp_objective.region_id].num_executions > IRT_OMPP_OPTIMIZER_BEST) {
                variant->rt_data.optimizer_rt_data.cur.full = variant->rt_data.optimizer_rt_data.best.full;
            }
            else
#endif
            {
                irt_optimizer_wi_data* new_element = calloc(1, sizeof(irt_optimizer_wi_data)); 
                new_element->id.frequency = rand() % irt_g_available_freq_count;                                                                                                                                                                                                                                      

#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DVFS_EVAL
                static int next_freq = 0;
                irt_g_dvfs_eval_count[next_freq] ++;
                if(irt_g_dvfs_eval_count[next_freq] >= IRT_OMPP_OPTIMIZER_DVFS_EVAL_STEPS) {
                    next_freq ++;
                    if(next_freq > irt_g_available_freq_count-1)
                        next_freq = irt_g_available_freq_count-1;
                }
                new_element->id.frequency = next_freq;
#endif

#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT
                new_element->id.thread_count = rand() % irt_g_worker_count + 1;
#else
                // fix the value to avoid different hashes
                new_element->id.thread_count = irt_g_worker_count;
#endif

                //new_element->id.param_value     = rand() % irt_g_available_freq_count;

                variant->rt_data.optimizer_rt_data.cur.full = new_element->id.full;
                free(new_element);
            }
        }
    }

    irt_spin_unlock(&variant->rt_data.optimizer_rt_data.spinlock);

    return;
}

void irt_optimizer_apply_dvfs(irt_wi_implementation_variant* variant) {
    irt_optimizer_runtime_data* data;

    if(variant->meta_info->ompp_objective.region_id == UINT_MAX) {
        data = variant->rt_data.wrapping_optimizer_rt_data;
        // no optimizations to apply
        if(!data)
            return;
    }
    else
        data = &(variant->rt_data.optimizer_rt_data);

    irt_spin_lock(&data->spinlock);

    irt_worker* self = irt_worker_get_current();
    irt_cpu_freq_set_frequency_worker(self, irt_g_available_freqs[data->cur.frequency]);

    irt_spin_unlock(&data->spinlock);

    //printf("%s: %d\n", __func__, irt_g_available_freqs[data->cur.frequency]);

    return;
}

void irt_optimizer_remove_dvfs(irt_wi_implementation_variant* variant) {
    if(variant->meta_info->ompp_objective.region_id == UINT_MAX && !variant->rt_data.wrapping_optimizer_rt_data)
        return;
    
    irt_worker* self = irt_worker_get_current();
    irt_cpu_freq_set_frequency_worker(self, irt_g_available_freqs[IRT_OPTIMIZER_RT_FREQ]);

    //printf("%s: %d\n", __func__, irt_g_available_freqs[IRT_OPTIMIZER_RT_FREQ]);

    return;
}

#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT

uint32 irt_optimizer_apply_dct(irt_wi_implementation_variant* variant) {
    if(!variant->meta_info)
        return 0;

    if(variant->meta_info->ompp_objective.region_id != UINT_MAX)
        return variant->rt_data.optimizer_rt_data.cur.thread_count; 

    if(variant->rt_data.wrapping_optimizer_rt_data)
        return variant->rt_data.wrapping_optimizer_rt_data->cur.thread_count; 

    return 0;
}

#endif

irt_optimizer_runtime_data* irt_optimizer_set_wrapping_optimizations(irt_wi_implementation_variant* variant, irt_wi_implementation_variant* parent_var) {
    if(!parent_var || !variant) {
        return NULL;
    }

    irt_optimizer_runtime_data* old_data = variant->rt_data.wrapping_optimizer_rt_data;

    if(parent_var->meta_info->ompp_objective.region_id == UINT_MAX) {
        variant->rt_data.wrapping_optimizer_rt_data = parent_var->rt_data.wrapping_optimizer_rt_data;
    }
    else {
        variant->rt_data.wrapping_optimizer_rt_data = &(parent_var->rt_data.optimizer_rt_data);
    }

    return old_data;
}

void irt_optimizer_reset_wrapping_optimizations(irt_wi_implementation_variant* variant, irt_optimizer_runtime_data* data) {
    variant->rt_data.wrapping_optimizer_rt_data = data;
}

#else

void irt_optimizer_objective_init(irt_context *context) {}
void irt_optimizer_objective_destroy(irt_context *context) {}
void irt_optimizer_compute_optimizations(irt_wi_implementation_variant* variant, irt_work_item* wi, bool force_computation) {}
void irt_optimizer_apply_dvfs(irt_wi_implementation_variant* variant) {}
void irt_optimizer_remove_dvfs(irt_wi_implementation_variant* variant) {}
irt_optimizer_runtime_data* irt_optimizer_set_wrapping_optimizations(irt_wi_implementation_variant* variant, irt_wi_implementation_variant* parent_var) {return NULL;}
void irt_optimizer_reset_wrapping_optimizations(irt_wi_implementation_variant* variant, irt_optimizer_runtime_data* data) {}

#endif // IRT_ENABLE_OMPP_OPTIMIZER

#ifndef IRT_ENABLE_OMPP_OPTIMIZER_DCT

uint32 irt_optimizer_apply_dct(irt_wi_implementation_variant* variant) { return 0; }

#endif // IRT_ENABLE_OMPP_OPTIMIZER_DCT

#endif // ifndef __GUARD_IMPL_IRT_OPTIMIZER_IMPL_H
