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

#include "irt_optimizer.h"

double **irt_g_opt_shares = NULL; //stores the shares for each thread in each region
				//shares[3][4] = 0.4 means: in work_item 3, thread 4 does 40% of the work
double **irt_g_opt_times = NULL;


void irt_optimizer_starting_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, irt_work_group* group) {
	uint32 ncpus = group->local_member_count;

	// first call
	if(irt_g_opt_shares == NULL) {
		uint32 nwi = irt_context_get_current()->impl_table_size;
		irt_g_opt_shares = (double**)calloc(nwi, sizeof(double*));
		irt_g_opt_times = (double**)calloc(nwi, sizeof(double*));
	} // memory allocated

	// if it is the first time this loop is called
	if(irt_g_opt_shares[impl_id] == NULL) {
		irt_g_opt_shares[impl_id] = (double*)malloc(ncpus * sizeof(double));
		irt_g_opt_times[impl_id] = (double*)malloc(ncpus * sizeof(double));
		double chunk = 1.0 / ncpus;
		for(int i = 0; i<ncpus; i++) {
			irt_g_opt_shares[impl_id][i] = chunk;
		}

		// estimate in first iteration
		irt_loop_sched_policy counting_policy;
		counting_policy.type = IRT_DYNAMIC_CHUNKED_COUNTING;
		counting_policy.participants = ncpus;
		counting_policy.param.chunk_size = 10; // Use the compiler!!!!
		irt_wg_set_loop_scheduling_policy(group, &counting_policy);
		return;
	}
	// if it is the second time this loop is called
	if(group->cur_sched.type == IRT_DYNAMIC_CHUNKED_COUNTING) {
		// estimate times from counted shares
		double min = irt_g_opt_times[impl_id][0];
		double max = irt_g_opt_times[impl_id][0];
		for(int i = 1; i < ncpus; ++i) {
			if(irt_g_opt_times[impl_id][i] < min) min = irt_g_opt_times[impl_id][i];
			if(irt_g_opt_times[impl_id][i] > max) max = irt_g_opt_times[impl_id][i];
		}
		double ext = max-min;
		for(int i = 0; i < ncpus; ++i) {
			irt_g_opt_times[impl_id][i] = (irt_g_opt_times[impl_id][i]-min)/ext * 1.8;
		}
	}

	// re-assining the shares
	//printf("steal: ");
	for(int i = 0; i < ncpus; ++i) {
		int next_i = (i<ncpus-1) ? i+1 : 0;
		double diff_time_next = irt_g_opt_times[impl_id][i] - irt_g_opt_times[impl_id][next_i];

		if(diff_time_next < -0.1) { // negative, "steal" share
			double stolen = -diff_time_next * irt_g_opt_shares[impl_id][next_i] * 0.5;
			// limit max steal to current chunk
			//stolen = MIN(stolen, irt_g_opt_shares[impl_id][i]);
			stolen = MAX(stolen, 0.0);
			stolen = MIN(stolen, irt_g_opt_shares[impl_id][next_i]);
			//printf("% 5.3lf", stolen);
			irt_g_opt_shares[impl_id][i] += stolen;
			irt_g_opt_shares[impl_id][next_i] -= stolen;
		} else {
			//printf("% 5.3lf", 0.0);
		}
	}
	//printf("\n");


	// create the boundaries
	irt_loop_sched_policy shares_policy; 
	shares_policy.type = IRT_SHARES;
	shares_policy.participants = ncpus;
	memcpy(&shares_policy.param.shares, irt_g_opt_shares[impl_id], ncpus * sizeof(double));
		
	irt_wg_set_loop_scheduling_policy(group, &shares_policy);
}

#ifndef IRT_RUNTIME_TUNING_EXTENDED

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 time) {
	// TODO
	//printf("Completed pfor % 3d, time: % 10ld\n", impl_id, time);
}

#else

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, uint64 total_time, irt_loop_sched_data *sched_data) {
	if(sched_data->policy.type == IRT_SHARES) {
		for(int i = 0; i < sched_data->participants_complete; ++i) {
			irt_g_opt_times[impl_id][i] = (double)sched_data->part_times[i] / (double)total_time;
		}
	} else if(sched_data->policy.type == IRT_DYNAMIC_CHUNKED_COUNTING) {
		// we were trying to estimate core efficiency using the dynamic counting policy
		for(int i = 0; i < sched_data->participants_complete; ++i) {
			irt_g_opt_times[impl_id][i] = - (double)sched_data->part_times[i];
		}
	}
}

#endif
