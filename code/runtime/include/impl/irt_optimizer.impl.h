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

void irt_optimizer_starting_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, irt_work_group* group) { }
void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 time, irt_loop_sched_data *sched_data) { }

#else // ifndef IRT_RUNTIME_TUNING

///////////////////////////////////// Context =========================================================================

void irt_optimizer_context_startup(irt_context *context) {
    irt_optimizer_objective_init(context);
	irt_shared_mem_effort_estimate_external_load_optimizer_context_startup(context);
	irt_opencl_optimizer_context_startup(context); // OpenCL startup
}

///////////////////////////////////// Loops ===========================================================================

void irt_optimizer_starting_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, irt_work_group* group) {
	irt_wi_implementation *impl = &irt_context_get_current()->impl_table[impl_id];

	if(irt_meta_info_is_opencl_available(impl->variants[0].meta_info) && irt_meta_info_get_opencl(impl->variants[0].meta_info)->opencl) {
		irt_opencl_optimizer_starting_pfor(impl, range, group);
	} else {
		irt_shared_mem_effort_estimate_external_load_optimizer_starting_pfor(impl, range, group);
	}
}

#ifndef IRT_RUNTIME_TUNING_EXTENDED

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 walltime, irt_loop_sched_data* sched_data) { }

#else

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, uint64 total_time, irt_loop_sched_data *sched_data) {
	if(impl[0].features.opencl) {
		// nothing
	} else {
		irt_shared_mem_effort_estimate_external_load_optimizer_completed_pfor(impl_id, range, total_time, sched_data);
	}
}

#endif

#endif // ifndef IRT_RUNTIME_TUNING

uint64_t irt_optimizer_pick_in_range(uint64_t max) {
    /* according to enclosing region goals, pick a value */

    return max -1;
}

uint32 irt_g_available_freqs[128];
uint32 irt_g_available_freqs_count = -1;    

void irt_optimizer_objective_init(irt_context *context) {
    for(int i=0; i<context->impl_table_size; i++) { 
        for(int j=0; j<context->impl_table[i].num_variants; j++) { 
            context->impl_table[i].variants[j].rt_data.optimizer_rt_data.data_last = -1;
            context->impl_table[i].variants[j].rt_data.optimizer_rt_data.completed_wi_count = -1;
            irt_spin_init(&context->impl_table[i].variants[j].rt_data.optimizer_rt_data.spinlock);
        }
    }

    irt_cpu_freq_get_available_frequencies_worker(irt_worker_get_current(), irt_g_available_freqs, &irt_g_available_freqs_count);
}

void irt_optimizer_objective_destroy(irt_context *context) {
    for(int i=0; i<context->impl_table_size; i++) { 
        for(int j=0; j<context->impl_table[i].num_variants; j++) { 
            irt_spin_destroy(&context->impl_table[i].variants[j].rt_data.optimizer_rt_data.spinlock);
            free(context->impl_table[i].variants[j].rt_data.optimizer_rt_data.data);
        }
    }
}

void irt_optimizer_compute_optimizations(irt_wi_implementation_variant* variant) {
    if(!variant->meta_info || variant->meta_info->ompp_objective.region_id == (unsigned)-1) {
            return;
    }

    irt_worker* self = irt_worker_get_current();
    
    irt_spin_lock(&variant->rt_data.optimizer_rt_data.spinlock);

    variant->rt_data.optimizer_rt_data.completed_wi_count ++;
    variant->rt_data.optimizer_rt_data.completed_wi_count %= irt_g_worker_count;

    // We update our optimization values every round of #workers parallel executions of the work item 
    if(variant->rt_data.optimizer_rt_data.completed_wi_count == 0) {

        int32 cur_freq = irt_cpu_freq_get_cur_frequency_worker(self);
        variant->rt_data.optimizer_rt_data.data_last ++;

        // allocate some memory

        if(variant->rt_data.optimizer_rt_data.data_last >= variant->rt_data.optimizer_rt_data.data_size) {
            variant->rt_data.optimizer_rt_data.data_size = (variant->rt_data.optimizer_rt_data.data_size) ? variant->rt_data.optimizer_rt_data.data_size * 2 : 2;
            variant->rt_data.optimizer_rt_data.data = (irt_optimizer_wi_data*)realloc(variant->rt_data.optimizer_rt_data.data, variant->rt_data.optimizer_rt_data.data_size * sizeof(irt_optimizer_wi_data));
            if(variant->rt_data.optimizer_rt_data.data == NULL)
                IRT_ASSERT(variant->rt_data.optimizer_rt_data.data != NULL, IRT_ERR_OMPP, "Optimizer: Could not perform realloc for optimizer data: %s", strerror(errno));
        }

        // compute optimal frequency

        if(variant->rt_data.optimizer_rt_data.data_last == 0) {
            // conservative approach at the beginning
            variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency = 0;
            variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].workers_count = 1;
            variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].resources.energy = 0;
        }
        else {
          	//irt_context* context = irt_context_get_current();
	        //irt_inst_region_context_data* regions = context->inst_region_data;
            //
            //#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
            //_data_type__ cpu_energy; \
	    	//if(irt_g_inst_region_metric_measure_cpu_energy) { \
            //    variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last -1].resources.energy = \
            //       (_data_type__)((double)regions[variant->objective.region_id].aggregated_cpu_energy * _output_conversion_code__); \
            //    /*cpu_energy *= wi_count; */ \
            //    /*cpu_energy /= (irt_get_num_cores_per_socket() < irt_g_worker_count) ? irt_get_num_cores_per_socket() : irt_g_worker_count;*/ \
	    	//}
            //#define ISOLATE_METRIC
            //#define ISOLATE_CPU_ENERGY
            //#include "irt_metrics.def"

            //cpu_energy = variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last -1].resources.energy;
            //if(variant->rt_data.optimizer_rt_data.data_last >= 2)
            //    cpu_energy -= variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last -2].resources.energy;

            //variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency  = variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last -1].frequency;
            //variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].workers_count  = variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last -1].workers_count +1;
            //if(cpu_energy < variant->objective.constraints.max.energy && variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency) {
            //    variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency --;
            //}
            //else if(cpu_energy > variant->objective.constraints.max.energy && variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency < irt_g_available_freqs_count -2) {
            //    variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency ++;
            //}
            
            if(variant->meta_info->ompp_objective.energy_weight) {
                    //printf("task %d ener\n", is_task);
                variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency =  0;// irt_g_available_freqs_count -1;
            }
            else {
                    //printf("task %d time\n", is_task);
                variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency = 1;
            }


            //printf("cur_freq = %d cpu_energy = %f  next freq %d\n", cur_freq, cpu_energy, variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency);
        }
    
            printf("cur_freq = %d next freq %d\n", cur_freq, variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency);
        variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].outer_frequency = 3900 ;//cur_freq;
    }


    irt_spin_unlock(&variant->rt_data.optimizer_rt_data.spinlock);
    return;
}

void irt_optimizer_apply_optimizations(irt_wi_implementation_variant* variant) {
    // no optimizations to apply
    if(!variant->meta_info || variant->meta_info->ompp_objective.region_id == (unsigned)-1 || !variant->rt_data.optimizer_rt_data.data)
        return;

    irt_spin_lock(&variant->rt_data.optimizer_rt_data.spinlock);

    irt_worker* self = irt_worker_get_current();
    //    //int32 cur_freq = irt_cpu_freq_get_cur_frequency_worker(self);
    //    //printf("cur freq = %d\n", cur_freq);
    //for(int i=0; i<irt_g_worker_count; i++) {
    //    irt_cpu_freq_set_frequency_worker(irt_g_workers[i], irt_g_available_freqs[variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency]);
    //}
    //    //int32 cur_freq = irt_cpu_freq_get_cur_frequency_worker(self);
    //    //printf("set cur freq = %d\n", cur_freq);

    irt_cpu_freq_set_frequency_worker(self, irt_g_available_freqs[variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency]);

    irt_spin_unlock(&variant->rt_data.optimizer_rt_data.spinlock);
    printf("%s: %d\n", __func__, irt_g_available_freqs[variant->rt_data.optimizer_rt_data.data[variant->rt_data.optimizer_rt_data.data_last].frequency]);

    return;
}

void irt_optimizer_remove_optimizations(irt_wi_implementation_variant* variant, int pos, bool wi_finalized) {
    // no optimizations to apply
    if(!variant || !variant->rt_data.optimizer_rt_data.data)
        return;
    
    irt_spin_lock(&variant->rt_data.optimizer_rt_data.spinlock);

    // Which frequency should we use to execute runtime code?
    // Does it make sense to switch to a different frequency?
    // We have to switch at the end of a work item because it could be nested inside
    // another one

    irt_worker* self = irt_worker_get_current();
    irt_cpu_freq_set_frequency_worker(self, variant->rt_data.optimizer_rt_data.data[pos].outer_frequency);

    irt_spin_unlock(&variant->rt_data.optimizer_rt_data.spinlock);
    printf("%s: %d\n", __func__, variant->rt_data.optimizer_rt_data.data[pos].outer_frequency);

    return;
}

#endif // ifndef __GUARD_IMPL_IRT_OPTIMIZER_IMPL_H
