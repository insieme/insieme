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
#include "lib_ocl.h"

#define SIZE 1000

int main(int argc, char* argv[]) {
	int size = SIZE;

	int* input1 = (int*)malloc(sizeof(int) * SIZE);
	int* input2 = (int*) malloc(sizeof(int) * SIZE);
	int* output = (int *)malloc(sizeof(int) * SIZE);
	
	for(int i=0; i < SIZE; ++i) {
		input1[i] = i;
		input2[i] = i*2;
	}

	irt_ocl_init_devices();
	if (irt_ocl_get_num_devices() != 0) {
		irt_ocl_device* dev = irt_ocl_get_device(0);
		irt_ocl_kernel* kernel = irt_ocl_create_kernel(dev, "vec_add.cl", "vec_add", "", IRT_OCL_SOURCE);
		
		irt_ocl_buffer* buf_input1;
		buf_input1 = irt_ocl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * SIZE);
		irt_ocl_buffer* buf_input2 = irt_ocl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * SIZE);
		irt_ocl_buffer* buf_output = irt_ocl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * SIZE);

		irt_ocl_write_buffer(buf_input1, CL_FALSE, sizeof(int) * SIZE, &input1[0]);
		irt_ocl_write_buffer(buf_input2, CL_FALSE, sizeof(int) * SIZE, &input2[0]);

		size_t szLocalWorkSize = 256;
		float multiplier = SIZE/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;
	
		irt_ocl_set_kernel_ndrange(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize);

		irt_ocl_run_kernel(kernel, 4,   (size_t)0, (void *)buf_input1,
						(size_t)0, (void *)buf_input2,
						(size_t)0, (void *)buf_output,
						sizeof(cl_int), (void *)&size);

		irt_ocl_read_buffer(buf_output, CL_TRUE, sizeof(int) * SIZE, &output[0]);
	
		irt_ocl_release_buffer(buf_input1);
		irt_ocl_release_buffer(buf_input2);
		irt_ocl_release_buffer(buf_output);
		irt_ocl_release_kernel(kernel);
	}
	irt_ocl_release_devices();

	// CHECK for output
	printf("======================\n= Vector Addition Done\n");
	unsigned int check = 1;
	for(unsigned int i = 0; i < SIZE; ++i) {
		if(output[i] != i*3/2) {
			check = 0;
			printf("= fail at %d, expected %d / actual %d", i, i*3/2, output[i]);
			break;
		}
	}
	printf("= result check: %s\n======================\n", check ? "OK" : "FAIL");
}
