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

uint64 **sub_iterations = NULL; //stores the number of iterations to be computed by each thread in each region
				//sub_iterations[3][4] = 64 means: in work_item 3, thread 4 computes 64 iteratuibs

uint32 *number_of_participants = NULL; // stores the number of participants in every region
uint32 *ranges   = NULL; // stores the range to be computed for every work item
uint8  *threads_phase = NULL;
uint64 *region_times;
uint64 *region_times_default; 


void irt_optimizer_starting_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, irt_work_group* group) {


	// first time is called (allocate memory for the ranges) // update whith the number of regions
	if (sub_iterations == NULL) {
		sub_iterations = (uint64 **)malloc(10 * sizeof(uint64 *));
		for (int i=0; i < 10; i++) {
			sub_iterations[impl_id] = NULL;
		}
		number_of_participants 		= (uint32 *)malloc(10 * sizeof(uint32 *));
		ranges  			= (uint32 *)malloc(10 * sizeof(uint32));
		threads_phase 			= (uint8  *)malloc(10 * sizeof(uint8));
		region_times 			= (uint64 *)malloc(10 * sizeof(uint64));
		region_times_default 		= (uint64 *)malloc(10 * sizeof(uint64));
		
		

		for (int i=0; i < 10; i++) {
			threads_phase[i] 	=  1;
			region_times[i] 	= -1;
			region_times_default[i] = -1;
		}

		if ((sub_iterations == NULL) || (number_of_participants == NULL))
			printf("Error allocating memory!\n");

	} // memory allocated
        
	// update the range values of the new region
	ranges[impl_id] = (uint64) (range.end - range.begin) / (uint64) range.step;


	// if it is the first time the loop is called -> set the dynamic policy
        if (sub_iterations[impl_id]==NULL) {	
		if (region_times_default[impl_id] == -1) { // first time is called 
			irt_loop_sched_policy static10 = (irt_loop_sched_policy){IRT_STATIC_CHUNKED,24,{10}};
			irt_wg_set_loop_scheduling_policy(group,&static10);
			threads_phase[impl_id] = 0; // no more thread phase
		
		} else if (region_times[impl_id] <= region_times_default[impl_id]) { // try to decrease even more the number of threads
			//printf("Decreasing the number of threads");
			irt_loop_sched_policy static10 = (irt_loop_sched_policy){IRT_STATIC_CHUNKED,number_of_participants[impl_id]>1?(number_of_participants[impl_id]-1):1,{10}};
			irt_wg_set_loop_scheduling_policy(group,&static10);	
		
		} else  {
			irt_loop_sched_policy static10 = (irt_loop_sched_policy){IRT_STATIC_CHUNKED,number_of_participants[impl_id]+1,{10}}; // use the previous configuration
			irt_wg_set_loop_scheduling_policy(group,&static10);
			threads_phase[impl_id] = 0;
		}
	} else {  // keep working on the previous loop
		//printf("pintando nuevos bordes: %d \n ", number_of_participants[impl_id]);
		
		for (int i=0; i < number_of_participants[impl_id]; i++) {
		//	printf("%d\n",(int)sub_iterations[impl_id][i]);
		}
		// creating the boundaries
		uint64 *boundaries = (uint64 *)malloc((number_of_participants[impl_id] -1) * sizeof(uint64));
		boundaries[0] = sub_iterations[impl_id][0];	
		for (int i=1; i < number_of_participants[impl_id];i++) {
		   boundaries[i] = boundaries[i-1]+sub_iterations[impl_id][i];
		}
		irt_loop_sched_policy fixed_policy;
        	fixed_policy.type = IRT_FIXED;
		fixed_policy.participants = number_of_participants[impl_id];
	 	fixed_policy.param.boundaries = boundaries;
		irt_wg_set_loop_scheduling_policy(group,&fixed_policy);
        } 
	
}

#ifndef IRT_RUNTIME_TUNING_EXTENDED

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 time) {
	// TODO
	//printf("Completed pfor % 3d, time: % 10ld\n", impl_id, time);
}

#else

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 total_time, uint64 *participant_times, uint32 num_participants) {

	printf("%d\n",total_time);	
	// thread_phase (account for the time)
	if (threads_phase[impl_id] == 1) {

		printf("thread phase");	
		if (region_times_default[impl_id]==-1) {
			region_times_default[impl_id] = total_time;
		} else {
			region_times_default[impl_id] = region_times[impl_id];
		}
		number_of_participants[impl_id] = num_participants;
		region_times[impl_id] = total_time;
	
	} else {

		//printf("non thread phase");
        // first time for this region?
	if (sub_iterations[impl_id] == NULL) { // the static policy is set in that thread
		//storing the number_of_participans
		number_of_participants[impl_id] = num_participants;

		// intially all the threads should have compute the same number of iterations 
		// allocating memory and filling up with that information
		sub_iterations[impl_id] = (uint64 *)malloc(num_participants * sizeof(uint64));
		if (sub_iterations[impl_id] == NULL)
			printf("Error allocating memory in the runtime optimizer");

		for (int i = 0; i < num_participants; i++) 
			sub_iterations[impl_id][i] = (int)ranges[impl_id] / (int)num_participants;
			

 	} // memory located
	
	//for (int i = 0; i < num_participants; i++) {
		//printf("Iterations %d\n",sub_iterations[impl_id][i]);
	//}	


	// normalizing the times of the threads (need the max and mins!)
	uint64 max_time = participant_times[0];
	uint64 min_time = participant_times[0];
	for (int i = 1; i < num_participants; i++) {
		if (participant_times[i] > max_time) 
			max_time = participant_times[i];
		if (participant_times[i] < min_time) 
			min_time = participant_times[i];
	}  

	//printf("Normalizing part");
        
	// normalizing to [0-1] coefficient
	uint64 coefficient = max_time - min_time;
        double *times = (double *) malloc(num_participants * sizeof(double));
	for (int i = 0; i < num_participants; i++) {
		times[i] = (double) (participant_times[i] - min_time) / (double) coefficient;
	} // normalization end

	
        //for (int i = 0; i < num_participants; i++) {
	//	printf("%d ",participant_times[i]);
	//}		
	//printf("\n");

	//for (int i =0; i < num_participants; i++) {
	//	printf("%f ", times[i]);
	//}
	//printf("\n");


	// re-assining the iterations for every loop
	for (int i = 0; i < num_participants; i++) {
		int index_next     = (i+1 < num_participants)?i+1:0;
		int index_previous = (i-1 > 0)?i-1:num_participants-1;

		double diff_time_next = times[i] - times[index_next];
		
		//printf("%d %d %f \n",index_next,index_previous, diff_time_next);
		if (diff_time_next > 0.15) { // experimental threshold
			uint32 to_reallocate = (uint32) 1; //((1 - diff_time_next) * sub_iterations[impl_id][i]) / 2;
			sub_iterations[impl_id][i] = sub_iterations[impl_id][i] 	 - to_reallocate;
			sub_iterations[impl_id][index_next] = sub_iterations[impl_id][index_next] + to_reallocate;
		}
	}

	
	//for (int i = 0; i < num_participants; i++) {
	//	printf("Iterations after %d\n",sub_iterations[impl_id][i]);
	//}	
	}
}




#endif
