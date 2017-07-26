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

#pragma once
#ifndef __GUARD_IMPL_IRT_OPTIMIZER_IMPL_H
#define __GUARD_IMPL_IRT_OPTIMIZER_IMPL_H

#include "utils/timing.h"
#include "irt_optimizer.h"
#include "impl/irt_context.impl.h"
#include "meta_information/meta_infos.h"

#include "optimizers/opencl_optimizer.h"
#include "optimizers/shared_mem_effort_estimate_external_load_optimizer.h"

void irt_optimizer_objective_init(irt_context* context);
void irt_optimizer_objective_destroy(irt_context* context);

void irt_optimizer_context_destroy(irt_context* context) {
	irt_optimizer_objective_destroy(context);
}

#ifndef IRT_RUNTIME_TUNING

void irt_optimizer_context_startup(irt_context* context) {
	irt_optimizer_objective_init(context);
}

void irt_optimizer_starting_pfor(irt_wi_implementation* impl, irt_work_item_range range, irt_work_group* group) {}
void irt_optimizer_completed_pfor(irt_wi_implementation* impl, uint64 time, irt_loop_sched_data* sched_data) {}

#else // ifndef IRT_RUNTIME_TUNING

///////////////////////////////////// Context =========================================================================

void irt_optimizer_context_startup(irt_context* context) {
	irt_optimizer_objective_init(context);
	irt_shared_mem_effort_estimate_external_load_optimizer_context_startup(context);
	irt_opencl_optimizer_context_startup(context); // OpenCL startup
}

///////////////////////////////////// Loops ===========================================================================

void irt_optimizer_starting_pfor(irt_wi_implementation* impl, irt_work_item_range range, irt_work_group* group) {
	// Note: As the selection of loop implementation variants may be different from WI impl selection we simply use the first implementation for now
	if(irt_meta_info_is_opencl_available(impl->variants[0].meta_info) && irt_meta_info_get_opencl(impl->variants[0].meta_info)->opencl) {
		irt_opencl_optimizer_starting_pfor(impl, range, group);
	} else {
		irt_shared_mem_effort_estimate_external_load_optimizer_starting_pfor(impl, range, group);
	}
}

#ifndef IRT_RUNTIME_TUNING_EXTENDED

void irt_optimizer_completed_pfor(irt_wi_implementation* impl, uint64 walltime, irt_loop_sched_data* sched_data) {}

#else

void irt_optimizer_completed_pfor(irt_wi_implementation* impl, irt_work_item_range range, uint64 total_time, irt_loop_sched_data* sched_data) {
	if(impl->features.opencl) {
		// nothing
	} else {
		irt_shared_mem_effort_estimate_external_load_optimizer_completed_pfor(impl, range, total_time, sched_data);
	}
}

#endif

#endif // ifndef IRT_RUNTIME_TUNING

#ifdef IRT_ENABLE_OMPP_OPTIMIZER

#define IRT_OMPP_OPTIMIZER_PRINT(...) // printf(__VA_ARGS__)

uint32 irt_g_available_freqs[128];
uint32 irt_g_available_freq_count = UINT_MAX;
#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DVFS_EVAL
float irt_g_dvfs_eval_energy[128];
uint64 irt_g_dvfs_eval_time[128];
int irt_g_dvfs_eval_count[128];
#endif

void get_available_freqs() {
	if(!irt_cpu_freq_get_available_frequencies(irt_g_available_freqs, &irt_g_available_freq_count)) { return; }

	// scaling_available_frequencies not available

	uint32 max = irt_cpu_freq_get_max_frequency_core(0);
	uint32 min = irt_cpu_freq_get_min_frequency_core(0);

	irt_g_available_freq_count = 0;
	do {
		irt_g_available_freqs[irt_g_available_freq_count++] = max;
		max -= 100;
	} while(max >= min);
}


void irt_optimizer_objective_init(irt_context* context) {
	get_available_freqs();

	for(int i = 0; i < context->impl_table_size; i++) {
		for(int j = 0; j < context->impl_table[i].num_variants; j++) {
			irt_spin_init(&context->impl_table[i].variants[j].rt_data.optimizer_rt_data.spinlock);
			context->impl_table[i].variants[j].rt_data.optimizer_rt_data.best.frequency = UINT64_MAX;
			context->impl_table[i].variants[j].rt_data.optimizer_rt_data.hc_elem = -1;
			context->impl_table[i].variants[j].rt_data.optimizer_rt_data.hc_dir = 0;
			context->impl_table[i].variants[j].rt_data.optimizer_rt_data.hc_end = false;
			memset(&(context->impl_table[i].variants[j].rt_data.optimizer_rt_data.max), UINT8_MAX,
			       sizeof(context->impl_table[i].variants[j].rt_data.optimizer_rt_data.max));
			context->impl_table[i].variants[j].rt_data.optimizer_rt_data.max.thread_count = irt_g_worker_count;
			context->impl_table[i].variants[j].rt_data.optimizer_rt_data.max.frequency = irt_g_available_freq_count;

			context->impl_table[i].variants[j].rt_data.optimizer_rt_data.cur.frequency = 1;                         // let's skip freq 0 (turbo boost (?));
			context->impl_table[i].variants[j].rt_data.optimizer_rt_data.cur.thread_count = irt_g_worker_count - 1; // 0 based as the rest
			memset(&(context->impl_table[i].variants[j].rt_data.optimizer_rt_data.cur_resources), 0, sizeof(irt_optimizer_resources));
		}
	}
}

void irt_optimizer_objective_destroy(irt_context* context) {
	// restore highest frequency
	irt_cpu_freq_set_frequency_worker(NULL, irt_g_available_freqs[IRT_OPTIMIZER_RT_FREQ]);

	for(int i = 0; i < context->impl_table_size; i++)
		for(int j = 0; j < context->impl_table[i].num_variants; j++) {
			irt_spin_destroy(&context->impl_table[i].variants[j].rt_data.optimizer_rt_data.spinlock);
		}
}

/* return 1 if res2 satisfies objective clause and it's better than res1, 0 otherwise */
int is_objective_satisfied(ompp_objective_info obj, irt_optimizer_resources res1, irt_optimizer_resources res2) {
	if(res2.cpu_energy < res1.cpu_energy && res2.wall_time < obj.time_max * 1e9 && res2.quality < obj.quality_max * obj.param_count) {
		return 1;
	} else {
		return 0;
	}
}

uint64 irt_optimizer_pick_in_range(int id, uint64 max, int qual_lb, int qual_ub, int qual_st) {
	irt_worker* self = irt_worker_get_current();
	irt_wi_implementation* impl = self->cur_wi->impl;
	irt_wi_implementation_variant* variant = &(impl->variants[self->default_variant]);

	if(!variant->meta_info || variant->meta_info->ompp_objective.region_id == UINT_MAX) { return 0; }

	// communicate max value to hill climbing algorithm
	if(variant->rt_data.optimizer_rt_data.max.param_value[id] != max + 1) {
		variant->rt_data.optimizer_rt_data.max.param_value[id] = max + 1;

		variant->rt_data.optimizer_rt_data.param_qual_range.lowb[id] = qual_lb;
		variant->rt_data.optimizer_rt_data.param_qual_range.uppb[id] = qual_ub;
		variant->rt_data.optimizer_rt_data.param_qual_range.step[id] = qual_st;
	}

	variant->rt_data.optimizer_rt_data.cur.param_value[id] %= (max + 1);

	return variant->rt_data.optimizer_rt_data.cur.param_value[id];
}

irt_optimizer_wi_data_id hill_climb(irt_optimizer_runtime_data* data, ompp_objective_info obj) {
	bool move_on = false;

	IRT_OMPP_OPTIMIZER_PRINT("%s: elem %d dir %d val %d\n", __func__, data->hc_elem, data->hc_dir, (*((uint64*)&data->best + data->hc_elem)));

	if(data->hc_elem == -1) {
		// first call
		data->hc_elem = 0; // frequency
		data->hc_dir = 1;  // go towards upper bound
	} else if(!is_objective_satisfied(obj, data->best_resources, data->cur_resources)) {
		if(data->hc_dir == 2) {
			// changhe hill climbing direction
			data->hc_dir = -1;
		} else {
			move_on = true;
		}
	} else {
		// keep climbing current element in solution (update best)
		data->best = data->cur;
		data->best_resources = data->cur_resources;
		IRT_OMPP_OPTIMIZER_PRINT("BEST: next freq %" PRIu64 " next thread count %" PRIu64 " next param %" PRIu64 " time %" PRIu64 " energy %f\n",
		                         data->best.frequency, data->best.thread_count + 1, data->best.param_value[0], data->cur_resources.wall_time,
		                         data->best_resources.cpu_energy);
	}

	do {
		irt_optimizer_wi_data_id next = data->best;

		// hill climb
		if(data->hc_dir > 0) {
			// go towards upper bound
			(*((uint64*)&next + data->hc_elem))++;
			data->hc_dir++;

			// if upper bound is reached
			if((*((uint64*)&next + data->hc_elem)) >= (*((uint64*)&(data->max) + data->hc_elem))) {
				if(data->hc_dir < 3) {
					// let's try to go towards lower bound
					data->hc_dir = -1;
					continue;
				} else {
					move_on = true;
				}
			}
		} else {
			// go towards lower bound
			(*((uint64*)&next + data->hc_elem))--;
			data->hc_dir--;

			// if lower bound is reached
			if((*((uint64*)&next + data->hc_elem)) == UINT64_MAX) { move_on = true; }
		}

		if(move_on) {
			IRT_OMPP_OPTIMIZER_PRINT("%s: moving to next tunable knob\n\n", __func__);
			move_on = false;
			// move to next element in solution
			data->hc_dir = 1;
			data->hc_elem++;

			#ifndef IRT_ENABLE_OMPP_OPTIMIZER_DCT
			if(data->hc_elem == (uint64*)&(data->best.thread_count) - (uint64*)&(data->best)) { data->hc_elem++; }
			#endif

			if(data->hc_elem >= (uint64*)&(data->best.param_value) + obj.param_count - (uint64*)&(data->best)) {
				data->hc_elem = (uint64*)&(data->best.param_value) + IRT_OPTIMIZER_PARAM_COUNT - (uint64*)&(data->best);
			}

			// if last element in solution, stop
			if(data->hc_elem == (uint64*)&(data->best.eos) - (uint64*)&(data->best)) {
				printf("BEST: frequency %" PRIu64 " threads count %" PRIu64 " param value %" PRIu64 " time %" PRIu64 " energy %f\n", data->best.frequency,
				       data->best.thread_count + 1, data->best.param_value[0], data->cur_resources.wall_time, data->best_resources.cpu_energy);
				data->hc_end = true;
				return data->best;
			}
		} else {
			return next;
		}
	} while(true);
}

void irt_optimizer_compute_optimizations(irt_wi_implementation_variant* variant, irt_work_item* wi, bool force_computation) {
	if(!variant->meta_info || (variant->meta_info->ompp_objective.region_id == UINT_MAX && !variant->rt_data.wrapping_optimizer_rt_data)
	   || variant->rt_data.optimizer_rt_data.hc_end) {
		return;
	}

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
	variant->rt_data.completed_wi_count++;
	if(variant->meta_info->ompp_objective.region_id != UINT_MAX) {
	#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT
		if(force_computation || variant->rt_data.completed_wi_count == variant->rt_data.optimizer_rt_data.cur.thread_count + 1) {
		#else
		if(force_computation || variant->rt_data.completed_wi_count == irt_g_worker_count) {
		#endif
			variant->rt_data.completed_wi_count = 0;

			if(context->inst_region_metric_group_support_data.global_rapmi_data.n == variant->rt_data.optimizer_rt_data.cur_resources.samplings) {
				irt_spin_unlock(&variant->rt_data.optimizer_rt_data.spinlock);
				return;
			}

			// collecting data

			irt_optimizer_resources cur_resources;

			cur_resources.quality = 0;
			cur_resources.samplings = context->inst_region_metric_group_support_data.global_rapmi_data.n;

			#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,       \
			               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                     \
				if(irt_g_inst_region_metric_measure_cpu_energy) {                                                                                              \
					cur_resources.cpu_energy =                                                                                                                 \
					    (_data_type__)((double)regions[variant->meta_info->ompp_objective.region_id].aggregated_cpu_energy * _output_conversion_code__);       \
				}
			#define ISOLATE_METRIC
			#define ISOLATE_CPU_ENERGY
			#include "irt_metrics.def"

			#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,       \
			               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                     \
				if(irt_g_inst_region_metric_measure_wall_time) {                                                                                               \
					cur_resources.wall_time =                                                                                                                  \
					    (_data_type__)((double)regions[variant->meta_info->ompp_objective.region_id].aggregated_wall_time * _output_conversion_code__);        \
				}
			#define ISOLATE_METRIC
			#define ISOLATE_WALL_TIME
			#include "irt_metrics.def"

			variant->rt_data.optimizer_rt_data.cur_resources.cpu_energy =
			    cur_resources.cpu_energy - variant->rt_data.optimizer_rt_data.cur_resources.cpu_energy;
			variant->rt_data.optimizer_rt_data.cur_resources.wall_time = cur_resources.wall_time - variant->rt_data.optimizer_rt_data.cur_resources.wall_time;
			variant->rt_data.optimizer_rt_data.cur_resources.quality = 0;
			for(int i = 0; i < variant->meta_info->ompp_objective.param_count; i++) {
				variant->rt_data.optimizer_rt_data.cur_resources.quality += variant->rt_data.optimizer_rt_data.cur.param_value[i];
			}

			#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DVFS_EVAL
			if(variant->rt_data.optimizer_rt_data.cur_resources.cpu_energy) {
				irt_g_dvfs_eval_energy[variant->rt_data.optimizer_rt_data.cur.frequency] += variant->rt_data.optimizer_rt_data.cur_resources.cpu_energy;
				irt_g_dvfs_eval_time[variant->rt_data.optimizer_rt_data.cur.frequency] += variant->rt_data.optimizer_rt_data.cur_resources.wall_time;
				if(irt_g_dvfs_eval_count[irt_g_available_freq_count - 1] == IRT_OMPP_OPTIMIZER_DVFS_EVAL_STEPS) {
					printf("%10s %10s %10s %20s %10s %s\n", "frequency", "energytot", "energy", "time", "power", "(num of executions)");
					for(int k = 0; k < irt_g_available_freq_count; k++) {
						printf("%10d %10f %10f %20" PRIu64 " %10f (%d)\n", irt_g_available_freqs[k], irt_g_dvfs_eval_energy[k],
						       irt_g_dvfs_eval_energy[k] / irt_g_dvfs_eval_count[k], irt_g_dvfs_eval_time[k] / irt_g_dvfs_eval_count[k],
						       (irt_g_dvfs_eval_energy[k]) / ((float)irt_g_dvfs_eval_time[k] / 1000000000), irt_g_dvfs_eval_count[k]);
					}
					printf("\n");
				}
			} else if(irt_g_dvfs_eval_count[variant->rt_data.optimizer_rt_data.cur.frequency]) {
				irt_g_dvfs_eval_count[variant->rt_data.optimizer_rt_data.cur.frequency]--;
			}

			variant->rt_data.optimizer_rt_data.cur_resources = cur_resources;

			// Computing new settings

			irt_optimizer_wi_data_id new_element = {0};

			static int next_freq = 0;
			irt_g_dvfs_eval_count[next_freq]++;
			if(irt_g_dvfs_eval_count[next_freq] >= IRT_OMPP_OPTIMIZER_DVFS_EVAL_STEPS) {
				next_freq++;
				if(next_freq > irt_g_available_freq_count - 1) { next_freq = irt_g_available_freq_count - 1; }
			}
			new_element.frequency = next_freq;
			variant->rt_data.optimizer_rt_data.cur = new_element;
			#else
			irt_optimizer_wi_data_id new_element;
			memset(&new_element, UINT8_MAX, sizeof(new_element));

			if(regions[variant->meta_info->ompp_objective.region_id].num_executions < 2000) { // IRT_OMPP_OPTIMIZER_BEST) {
				// first step: random search

				// Update best
				if(variant->rt_data.optimizer_rt_data.best.frequency == UINT64_MAX
				   || is_objective_satisfied(variant->meta_info->ompp_objective, variant->rt_data.optimizer_rt_data.best_resources,
				                             variant->rt_data.optimizer_rt_data.cur_resources)
				          == 1) {
					variant->rt_data.optimizer_rt_data.best_resources = variant->rt_data.optimizer_rt_data.cur_resources;
					variant->rt_data.optimizer_rt_data.best = variant->rt_data.optimizer_rt_data.cur;
					IRT_OMPP_OPTIMIZER_PRINT("BEST: next freq %" PRIu64 " next thread count %" PRIu64 " next param %" PRIu64 " time %" PRIu64 " energy %f\n",
					                         variant->rt_data.optimizer_rt_data.best.frequency, variant->rt_data.optimizer_rt_data.best.thread_count + 1,
					                         variant->rt_data.optimizer_rt_data.best.param_value[0], variant->rt_data.optimizer_rt_data.cur_resources.wall_time,
					                         variant->rt_data.optimizer_rt_data.best_resources.cpu_energy);
				}

				// Computing new settings

				new_element.frequency = rand() % irt_g_available_freq_count;
			#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT
				new_element.thread_count = rand() % irt_g_worker_count;
			#else
				new_element.thread_count = irt_g_worker_count - 1;
			#endif
				for(int i = 0; i < variant->meta_info->ompp_objective.param_count; i++) {
					new_element.param_value[i] = rand();
				}
			} else {
				// second step: hill climbing
				new_element = hill_climb(&(variant->rt_data.optimizer_rt_data), variant->meta_info->ompp_objective);
			}

			IRT_OMPP_OPTIMIZER_PRINT("NEXT: next freq %" PRIu64 " next thread count %" PRIu64 " next param %" PRIu64 "\n", new_element.frequency,
			                         new_element.thread_count + 1, new_element.param_value[0]);

			variant->rt_data.optimizer_rt_data.cur_resources = cur_resources;
			variant->rt_data.optimizer_rt_data.cur = new_element;
			#endif
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
		if(!data) { return; }
	} else {
		data = &(variant->rt_data.optimizer_rt_data);
	}

	irt_spin_lock(&data->spinlock);

	irt_worker* self = irt_worker_get_current();
	irt_cpu_freq_set_frequency_worker(
	    self, irt_g_available_freqs[(irt_g_available_freq_count - 1 < data->cur.frequency) ? irt_g_available_freq_count - 1 : data->cur.frequency]);

	irt_spin_unlock(&data->spinlock);

	// printf("%s: %d\n", __func__, irt_g_available_freqs[data->cur.frequency]);

	return;
}

void irt_optimizer_remove_dvfs(irt_wi_implementation_variant* variant) {
#if defined(_GEMS_ODROID)
// Resetting the frequency takes too long
#else
	if(variant->meta_info->ompp_objective.region_id == UINT_MAX && !variant->rt_data.wrapping_optimizer_rt_data) { return; }

	irt_worker* self = irt_worker_get_current();
	irt_cpu_freq_set_frequency_worker(self, irt_g_available_freqs[IRT_OPTIMIZER_RT_FREQ]);

	// printf("%s: %d\n", __func__, irt_g_available_freqs[IRT_OPTIMIZER_RT_FREQ]);
#endif

	return;
}

#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT

uint32 irt_optimizer_apply_dct(irt_wi_implementation_variant* variant) {
	if(!variant->meta_info) { return 0; }

	if(variant->meta_info->ompp_objective.region_id != UINT_MAX) {
		irt_scheduling_set_dop((irt_g_worker_count < variant->rt_data.optimizer_rt_data.cur.thread_count + 1)
		                           ? irt_g_worker_count
		                           : variant->rt_data.optimizer_rt_data.cur.thread_count + 1);
	}

	if(variant->rt_data.wrapping_optimizer_rt_data) {
		irt_scheduling_set_dop((irt_g_worker_count < variant->rt_data.wrapping_optimizer_rt_data->cur.thread_count + 1)
		                           ? irt_g_worker_count
		                           : variant->rt_data.wrapping_optimizer_rt_data->cur.thread_count + 1);
	}

	return 0;
}

#endif

irt_optimizer_runtime_data* irt_optimizer_set_wrapping_optimizations(irt_wi_implementation_variant* target_variant,
                                                                     irt_wi_implementation_variant* source_variant) {
	if(!source_variant || !target_variant) { return NULL; }

	irt_optimizer_runtime_data* old_data = target_variant->rt_data.wrapping_optimizer_rt_data;

	if(source_variant->meta_info->ompp_objective.region_id == UINT_MAX) {
		target_variant->rt_data.wrapping_optimizer_rt_data = source_variant->rt_data.wrapping_optimizer_rt_data;
	} else {
		target_variant->rt_data.wrapping_optimizer_rt_data = &(source_variant->rt_data.optimizer_rt_data);
	}

	return old_data;
}

void irt_optimizer_reset_wrapping_optimizations(irt_wi_implementation_variant* target_variant, irt_optimizer_runtime_data* data) {
	target_variant->rt_data.wrapping_optimizer_rt_data = data;
}

#else

void irt_optimizer_objective_init(irt_context* context) {}
void irt_optimizer_objective_destroy(irt_context* context) {}
uint64 irt_optimizer_pick_in_range(int id, uint64 max, int qual_lb, int qual_ub, int qual_st) {
	return 0;
}
void irt_optimizer_compute_optimizations(irt_wi_implementation_variant* variant, irt_work_item* wi, bool force_computation) {}
void irt_optimizer_apply_dvfs(irt_wi_implementation_variant* variant) {}
void irt_optimizer_remove_dvfs(irt_wi_implementation_variant* variant) {}
irt_optimizer_runtime_data* irt_optimizer_set_wrapping_optimizations(irt_wi_implementation_variant* target_variant,
                                                                     irt_wi_implementation_variant* source_variant) {
	return NULL;
}
void irt_optimizer_reset_wrapping_optimizations(irt_wi_implementation_variant* target_variant, irt_optimizer_runtime_data* data) {}

#endif // IRT_ENABLE_OMPP_OPTIMIZER

#ifndef IRT_ENABLE_OMPP_OPTIMIZER_DCT

uint32 irt_optimizer_apply_dct(irt_wi_implementation_variant* variant) {
	return 0;
}

#endif // IRT_ENABLE_OMPP_OPTIMIZER_DCT

#endif // ifndef __GUARD_IMPL_IRT_OPTIMIZER_IMPL_H
