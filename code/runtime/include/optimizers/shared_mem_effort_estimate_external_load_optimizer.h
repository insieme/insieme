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

uint64 irt_g_opt_min_effort = 1000000ull;
#include "utils/load.h"

void irt_shared_mem_effort_estimate_external_load_optimizer_context_startup(irt_context *context) {
	//const uint64 EST_RANGE = 1000000;

	//for(uint32 i=0; i < context->impl_table_size; ++i) {
	//	irt_wi_implementation_variant *variant = &context->impl_table[i].variants[0];

	//	// check if effort estimator available
	//	if(variant->effort_estimator != NULL) {
	//		// check if flat profile
	//		bool flat = true;
	//		uint64 chunk = (EST_RANGE/10);
	//		uint64 effort, last_effort;
	//		for(uint32 j=0; j<10; ++j) {
	//			effort = variant->effort_estimator(j*chunk, (j+1)*chunk);
	//			if(j>0 && effort != last_effort) flat = false;
	//			last_effort = effort;
	//		}

	//		variant->rt_data.flat_profile = flat;

	//		// if not flat profile, calculate ideal distribution
	//		if(!flat) {
	//			uint64 total_effort = variant->effort_estimator(0, EST_RANGE);
	//			uint64 ideal_effort = total_effort/irt_g_worker_count;
	//			double position = 0.0;
	//			for(uint32 c=0; c<irt_g_worker_count; ++c) {
	//				// start value
	//				double share = (1.0-position) / (double)(irt_g_worker_count-c);
	//				double max = 1.0, min = 0.0;
	//				// binary search
	//				for(uint32 k=0; k<1000; ++k) {
	//					uint64 est_effort = variant->effort_estimator(position*EST_RANGE, (position+share)*EST_RANGE);
	//					if(est_effort < ideal_effort) {
	//						min = share;
	//						share += (max - share) * 0.5;
	//					}
	//					else if(est_effort > ideal_effort) {
	//						max = share;
	//						share -= (share - min) * 0.5;
	//					}
	//					else break;
	//				}
	//				variant->rt_data.distribution[c] = share;
	//				position += share;
	//			}
	//		}
	//	}

	//	// calculate a good chunk size based on per-iteration effort
	//	uint64 effort = variant->features.effort;
	//	uint64 chunk = irt_g_opt_min_effort/MAX(effort, 1);
	//	chunk = MAX(chunk, 1);
	//	variant->rt_data.chunk_size = chunk;

	//	variant->rt_data.tested = false;
	//	variant->rt_data.force_dyn = false;

	//	// print info
	//	//if(variant->effort_estimator != NULL) {
	//	//	printf("flat profile: % 5s\n", variant->rt_data.flat_profile ? "true" : "false");
	//	//	if(!variant->rt_data.flat_profile) {
	//	//		printf("distribution: ");
	//	//		for(uint32 c=0; c<irt_g_worker_count; ++c) {
	//	//			printf("% 5.3lf, ", variant->rt_data.distribution[c]);
	//	//		}
	//	//		printf("\n");
	//	//	}
	//	//}
	//	//printf("per-iteration effort estimate: %llu\n", variant->features.effort);
	//	//printf("dynamic minimum chunk: %llu\n", variant->rt_data.chunk_size);
	//}
	//get_load_external();
}

static inline double get_cur_external_load() {
	static double load = 0.0;
	static uint64 last_ticks = 0;
	if(irt_time_ticks()-last_ticks < 1000000000ull) {
		return load;
	}
	load = get_load_external();
	if(load>1.0) load = 0.0;
	//printf("measure %lf\n", load);
	last_ticks = irt_time_ticks();
	return load;
}

void irt_shared_mem_effort_estimate_external_load_optimizer_starting_pfor(irt_wi_implementation* impl, irt_work_item_range range, irt_work_group* group) {
	//uint32 ncpus = group->local_member_count;
	//irt_wi_implementation_variant *variant = &impl->variants[0];

	//// if we have an effort estimator
	//if(variant->effort_estimator) {
	//	// check if worth parallelizing
	//	if(variant->effort_estimator(range.begin, range.end) < irt_g_opt_min_effort) {
	//		irt_wg_set_loop_scheduling_policy(group, &irt_g_loop_sched_policy_single);
	//		//printf("Haha tiny! % 3d\n", impl_id);
	//		return;
	//	}

	//	//check for external load
	//	double load = get_cur_external_load();
	//	if(load>0.03 || variant->rt_data.force_dyn) {
	//		// use dynamic distribution with educated chunk size estimation
	//		irt_loop_sched_policy dynamic_policy;
	//		dynamic_policy.type = IRT_DYNAMIC_CHUNKED;
	//		dynamic_policy.participants = ncpus;
	//		dynamic_policy.param.chunk_size = variant->rt_data.chunk_size;
	//		dynamic_policy.param.chunk_size = MAX((dynamic_policy.param.chunk_size) * (1.0 - load), 1);
	//		irt_wg_set_loop_scheduling_policy(group, &dynamic_policy);
	//		//printf("Haha load! % 3d l: %lf\n", impl_id, load);
	//		return;
	//	}

	//	// check if flat profile
	//	if(variant->rt_data.flat_profile) {
	//		irt_wg_set_loop_scheduling_policy(group, &irt_g_loop_sched_policy_default);
	//		//printf("Haha flat! % 3d\n", impl_id);
	//		return;
	//	}
	//	// otherwise use computed distribution
	//	else {
	//		irt_loop_sched_policy shares_policy;
	//		shares_policy.type = IRT_SHARES;
	//		shares_policy.participants = ncpus;
	//		memcpy(&shares_policy.param.shares, variant->rt_data.distribution, ncpus * sizeof(double));
	//		irt_wg_set_loop_scheduling_policy(group, &shares_policy);
	//		//printf("Haha zomg! % 3d\n", impl_id);
	//		return;
	//	}
	//}
	//// we don't have an effort estimator
	//else {
	//	// check if worth parallelizing
	//	if(variant->features.effort * (range.end-range.begin) < irt_g_opt_min_effort) {
	//		irt_wg_set_loop_scheduling_policy(group, &irt_g_loop_sched_policy_single);
	//		//printf("Haha tiny 2! % 3d\n", impl_id);
	//		return;
	//	}

	//	// use dynamic distribution with educated chunk size estimation
	//	irt_loop_sched_policy dynamic_policy;
	//	dynamic_policy.type = IRT_DYNAMIC_CHUNKED;
	//	dynamic_policy.participants = ncpus;
	//	dynamic_policy.param.chunk_size = variant->rt_data.chunk_size;
	//	//check for external load
	//	double load = get_cur_external_load();
	//	if(load>0.03) {
	//		dynamic_policy.param.chunk_size = MAX((dynamic_policy.param.chunk_size) * (1.0 - load), 1);
	//	}
	//	irt_wg_set_loop_scheduling_policy(group, &dynamic_policy);
	//	//printf("Haha dynamic 2! wi: % 3d  ncp: % 4d  l: %5.2lf   ch: % 8llu\n", impl_id, ncpus, load, dynamic_policy.param.chunk_size);
	//	return;
	//}
}


#ifndef IRT_RUNTIME_TUNING_EXTENDED

void irt_shared_mem_effort_estimate_external_load_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 time) {
	// nothing
}

#else

void irt_shared_mem_effort_estimate_external_load_optimizer_completed_pfor(irt_wi_implementation_id impl_id,
		irt_work_item_range range, uint64 total_time, irt_loop_sched_data *sched_data) {
	//if(sched_data->policy.type == IRT_STATIC) {
	//	irt_wi_implementation_variant *variant = &irt_context_get_current()->impl_table[impl_id].variants[0];
	//	if(!variant->rt_data.tested) {
	//		variant->rt_data.tested = true;
	//		uint64 prev = sched_data->part_times[0];
	//		for(int i = 1; i < sched_data->participants_complete; ++i) {
	//			if(abs(sched_data->part_times[i]-prev) > prev/10) {
	//				variant->rt_data.force_dyn = true;
	//				//printf("force_dyn %d\n", impl_id);
	//			}
	//		}
	//	}
	//}
}

#endif

///////////////////////////////////// Loops (old) =====================================================================
//irt_loop
//double **irt_g_opt_shares = NULL; //stores the shares for each thread in each region
//				//shares[3][4] = 0.4 means: in work_item 3, thread 4 does 40% of the work
//double **irt_g_opt_times = NULL;
//
//
//void irt_optimizer_starting_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, irt_work_group* group) {
//	uint32 ncpus = group->local_member_count;
//
//	// first call
//	if(irt_g_opt_shares == NULL) {
//		uint32 nwi = irt_context_get_current()->impl_table_size;
//		irt_g_opt_shares = (double**)calloc(nwi, sizeof(double*));
//		irt_g_opt_times = (double**)calloc(nwi, sizeof(double*));
//	} // memory allocated
//
//	// if it is the first time this loop is called
//	if(irt_g_opt_shares[impl_id] == NULL) {
//		irt_g_opt_shares[impl_id] = (double*)malloc(ncpus * sizeof(double));
//		irt_g_opt_times[impl_id] = (double*)malloc(ncpus * sizeof(double));
//		double chunk = 1.0 / ncpus;
//		for(int i = 0; i<ncpus; i++) {
//			irt_g_opt_shares[impl_id][i] = chunk;
//		}
//
//		// estimate in first iteration
//		irt_loop_sched_policy counting_policy;
//		counting_policy.type = IRT_DYNAMIC_CHUNKED_COUNTING;
//		counting_policy.participants = ncpus;
//		counting_policy.param.chunk_size = 10; // Use the compiler!!!!
//		irt_wg_set_loop_scheduling_policy(group, &counting_policy);
//		return;
//	}
//	// if it is the second time this loop is called
//	if(group->cur_sched.type == IRT_DYNAMIC_CHUNKED_COUNTING) {
//		// estimate times from counted shares
//		double min = irt_g_opt_times[impl_id][0];
//		double max = irt_g_opt_times[impl_id][0];
//		for(int i = 1; i < ncpus; ++i) {
//			if(irt_g_opt_times[impl_id][i] < min) min = irt_g_opt_times[impl_id][i];
//			if(irt_g_opt_times[impl_id][i] > max) max = irt_g_opt_times[impl_id][i];
//		}
//		double ext = max-min;
//		for(int i = 0; i < ncpus; ++i) {
//			irt_g_opt_times[impl_id][i] = (irt_g_opt_times[impl_id][i]-min)/ext * 1.8;
//		}
//	}
//
//	// re-assining the shares
//	//printf("steal: ");
//	for(int i = 0; i < ncpus; ++i) {
//		int next_i = (i<ncpus-1) ? i+1 : 0;
//		double diff_time_next = irt_g_opt_times[impl_id][i] - irt_g_opt_times[impl_id][next_i];
//
//		if(diff_time_next < -0.1) { // negative, "steal" share
//			double stolen = -diff_time_next * irt_g_opt_shares[impl_id][next_i] * 0.5;
//			// limit max steal to current chunk
//			//stolen = MIN(stolen, irt_g_opt_shares[impl_id][i]);
//			stolen = MAX(stolen, 0.0);
//			stolen = MIN(stolen, irt_g_opt_shares[impl_id][next_i]);
//			//printf("% 5.3lf", stolen);
//			irt_g_opt_shares[impl_id][i] += stolen;
//			irt_g_opt_shares[impl_id][next_i] -= stolen;
//		} else {
//			//printf("% 5.3lf", 0.0);
//		}
//	}
//	//printf("\n");
//
//
//	// create the boundaries
//	irt_loop_sched_policy shares_policy;
//	shares_policy.type = IRT_SHARES;
//	shares_policy.participants = ncpus;
//	memcpy(&shares_policy.param.shares, irt_g_opt_shares[impl_id], ncpus * sizeof(double));
//
//	irt_wg_set_loop_scheduling_policy(group, &shares_policy);
//}
//
//#ifndef IRT_RUNTIME_TUNING_EXTENDED
//
//void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 time) {
//	// TODO
//	//printf("Completed pfor % 3d, time: % 10ld\n", impl_id, time);
//}
//
//#else
//
//void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, uint64 total_time, irt_loop_sched_data *sched_data) {
//	if(sched_data->policy.type == IRT_SHARES) {
//		for(int i = 0; i < sched_data->participants_complete; ++i) {
//			irt_g_opt_times[impl_id][i] = (double)sched_data->part_times[i] / (double)total_time;
//		}
//	} else if(sched_data->policy.type == IRT_DYNAMIC_CHUNKED_COUNTING) {
//		// we were trying to estimate core efficiency using the dynamic counting policy
//		for(int i = 0; i < sched_data->participants_complete; ++i) {
//			irt_g_opt_times[impl_id][i] = - (double)sched_data->part_times[i];
//		}
//	}
//}
//
//#endif
