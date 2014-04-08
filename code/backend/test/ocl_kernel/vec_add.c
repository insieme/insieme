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

#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"

void subfunction(icl_kernel* kernel, size_t szGlobalWorkSize, size_t szLocalWorkSize, icl_event* wb_all, icl_event* rk,
		icl_buffer* buf_input1, icl_buffer* buf_input2, icl_buffer* buf_output, cl_int size, struct int2 si2) {

	szGlobalWorkSize = si2.a;
	si2.b = szLocalWorkSize;
	icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, wb_all, rk, 4,
										(size_t)0, (void *)buf_input1,
										(size_t)0, (void *)buf_input2,
										(size_t)0, (void *)buf_output,
										sizeof(cl_int), (void *)&size);

}

int main(int argc, char* argv[]) {
	int size = 1000;

	int* input1 = (int*)malloc(sizeof(int) * size);
	int* input2 = (int*) malloc(sizeof(int) * size);
	int* output = (int *)malloc(sizeof(int) * size);
	
	for(int i=0; i < size; ++i) {
		input1[i] = i;
		input2[i] = i*2;
	}

#ifndef INSIEME
	icl_timer* time1 = icl_init_timer(ICL_SEC);
	icl_start_timer(time1);
#endif
	icl_init_devices(ICL_CPU);
#ifndef INSIEME
	printf("TIME for initialization: %f\n", icl_stop_timer(time1));
#endif
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(0);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "vec_add.cl", "vec_add", "", ICL_SOURCE);
		
		icl_buffer* buf_input1 = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_input2 = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);

		icl_event* wb1 = icl_create_event();
		icl_event* wb2 = icl_create_event();
		icl_event* rb = icl_create_event();

		icl_write_buffer(buf_input1, CL_FALSE, sizeof(int) * size, &input1[0], NULL, wb1);
		icl_write_buffer(buf_input2, CL_FALSE, sizeof(int) * size, &input2[0], NULL, wb2);
		
		size_t szLocalWorkSize = 256;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_event* rk = icl_create_event();
		icl_event* wb_all = icl_create_event_list(2, wb1, wb2);	
		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, wb_all, rk, 4,
											(size_t)0, (void *)buf_input1,
											(size_t)0, (void *)buf_input2,
											(size_t)0, (void *)buf_output,
											sizeof(cl_int), (void *)&size);

//		subfunction(kernel, szGlobalWorkSize, szLocalWorkSize, wb_all, rk,
//											buf_input1,
//											buf_input2,
//											buf_output,
//											size, si2);

		icl_read_buffer(buf_output, CL_TRUE, sizeof(int) * size, &output[0], rk, rb);
		
		printf("Time wb1 %f\n", icl_profile_event(wb1, ICL_STARTED, ICL_FINISHED, ICL_SEC));		
		printf("Time wb2 %f\n", icl_profile_event(wb2, ICL_STARTED, ICL_FINISHED, ICL_SEC));
		printf("Time rk %f\n", icl_profile_event(rk, ICL_STARTED, ICL_FINISHED, ICL_SEC));
		printf("Time rb %f\n", icl_profile_event(rb, ICL_STARTED, ICL_FINISHED, ICL_SEC));
	
		icl_release_events(5, wb1, wb2, wb_all, rk, rb);
		icl_release_buffers(3, buf_input1, buf_input2, buf_output);
		icl_release_kernel(kernel);
	}
#ifndef INSIEME
	icl_restart_timer(time1);
#endif
	icl_release_devices();
#ifndef INSIEME
	printf("TIME for releasing the devices: %f\n", icl_stop_timer(time1));
	icl_release_timer(time1);
#endif
	
	// CHECK for output
	printf("======================\n= Vector Addition Done\n");
	unsigned int check = 1;
	for(unsigned int i = 0; i < size; ++i) {
		if(output[i] != i*3/2) {
			check = 0;
			printf("= fail at %d, expected %d / actual %d", i, i*3/2, output[i]);
			break;
		}
	}
	printf("= result check: %s\n======================\n", check ? "OK" : "FAIL");
	free(input1);
	free(input2);
	free(output);
}
