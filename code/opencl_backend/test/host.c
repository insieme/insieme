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

#include <CL/cl.h>
#include <stdio.h>

#define NUM_DATA 100

int main(int argc, char **argv)
{
        cl_int _err;
        cl_platform_id platforms[100];
        cl_uint platforms_n = 1;
        clGetPlatformIDs(100, platforms, &platforms_n);

        if (platforms_n == 0)
                return 1;

        cl_device_id devices[100];
        cl_uint devices_n = 1;
        clGetDeviceIDs(platforms[0], CL_DEVICE_TYPE_GPU, 100, devices, &devices_n);

        if (devices_n == 0)
                return 1;

        cl_context context = clCreateContext(NULL, 1, devices, NULL, NULL, &_err);

        const char *program_source = NULL;

        char* prova = NULL;

        cl_program program;
        #pragma insieme kernelFile "kernel.cl"
        program = clCreateProgramWithSource(context, sizeof(program_source)/sizeof(*program_source), &program_source, NULL, &_err);
        if (clBuildProgram(program, 1, devices, "", NULL, NULL) != CL_SUCCESS) {
                char buffer[10240];
                clGetProgramBuildInfo(program, devices[0], CL_PROGRAM_BUILD_LOG, sizeof(buffer), buffer, NULL);
                //fprintf(stderr, "CL Compilation failed:\n%s", buffer);
        }

        cl_mem input_buffer;
        input_buffer = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(int)*NUM_DATA, NULL, &_err);

        cl_mem output_buffer;
        output_buffer = clCreateBuffer(context, CL_MEM_WRITE_ONLY, sizeof(int)*NUM_DATA, NULL, &_err);

        int factor = 2;

        cl_kernel kernel = clCreateKernel(program, "fun", &_err);
        clSetKernelArg(kernel, 0, sizeof(input_buffer), &input_buffer);
        clSetKernelArg(kernel, 1, sizeof(output_buffer), &output_buffer);
        clSetKernelArg(kernel, 2, sizeof(factor), &factor);

        cl_command_queue queue;
        queue = clCreateCommandQueue(context, devices[0], 0, &_err);

//        for (int i=0; i<NUM_DATA; i++) { throws assertion, cannot get the reference of a for-loop-induction-variable
        int i = 0;
        while(i < NUM_DATA) {
            clEnqueueWriteBuffer(queue, input_buffer, CL_TRUE, 0, sizeof(int), &i, 0, NULL, NULL);
        	++i;
        }

        cl_event kernel_completion;
        size_t global_work_size[1] = { NUM_DATA };
        size_t local_work_size[1] = { 1 };

        clEnqueueNDRangeKernel(queue, kernel, 1, NULL, global_work_size, local_work_size, 0, NULL, &kernel_completion);
        clWaitForEvents(1, &kernel_completion);
        clReleaseEvent(kernel_completion);

        printf("Result:");
        for (int i=0; i<NUM_DATA; i++) {
                int data;
                clEnqueueReadBuffer(queue, output_buffer, CL_TRUE, i*sizeof(int), sizeof(int), &data, 0, NULL, NULL);
                printf(" %d", data);
        }
        printf("\n");

        clReleaseMemObject(input_buffer);
        clReleaseMemObject(output_buffer);

        clReleaseKernel(kernel);
        clReleaseProgram(program);
        clReleaseContext(context);

        return 0;

}
