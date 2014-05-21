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
#include "CL/cl.h"

struct Buffer {
	cl_mem mem;
	size_t size;
};

struct Kernel {
	cl_kernel fct;
	cl_command_queue queue;
};

cl_program program = NULL;
cl_context context = NULL;
short* short_host_ptr;
cl_mem dev_ptr1 = NULL;
cl_event event = NULL;

cl_int subfunction(cl_kernel kernel, cl_command_queue queue, size_t* globalSize, size_t* localSize, cl_context context, cl_mem buf_arg) {
	buf_arg = clCreateBuffer(context, CL_MEM_READ_WRITE , sizeof(cl_int) * 100, NULL, NULL);
	return clEnqueueNDRangeKernel(queue, kernel, 2, NULL, globalSize, localSize, 0, NULL, &event);
}

//#pragma insieme mark
int main(int argc, char **argv) {
	struct Kernel kernel;
	cl_int err;
	cl_device_id* device;// = (cl_device_id*)malloc(sizeof(cl_device_id*));
	cl_platform_id* platforms;

	cl_uint n;
	err = clGetPlatformIDs(10, platforms, NULL);
	clGetDeviceIDs(platforms[0], 0, 1, device, &n);

	context = clCreateContext(0, 1, device, NULL, NULL, &err);
	kernel.queue = clCreateCommandQueue(context, device[0], CL_QUEUE_PROFILING_ENABLE, &err);

	float* host_ptr;
	cl_mem dev_ptr2 = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR, sizeof(cl_float) * 100, host_ptr, NULL);
	cl_mem dev_ptr3 = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, sizeof(cl_float) * 100, host_ptr, &err);
	cl_mem dev_ptr4[2];
	cl_mem dev_ptr5 = clCreateBuffer(context, CL_MEM_READ_WRITE , sizeof(cl_int) * 100, NULL, NULL);
	clReleaseMemObject(dev_ptr5);

	dev_ptr1 = clCreateBuffer(context, CL_MEM_READ_ONLY, 100 * sizeof(cl_short), short_host_ptr, &err);
	dev_ptr4[0] = clCreateBuffer(context, CL_MEM_WRITE_ONLY, 100 * sizeof(cl_float), NULL, &err);
	dev_ptr4[1] = clCreateBuffer(context, CL_MEM_WRITE_ONLY, 100 * sizeof(cl_float), NULL, &err);

	clEnqueueWriteBuffer(kernel.queue, dev_ptr1, CL_TRUE, 0, sizeof(cl_float) * 100, host_ptr, 0, NULL, NULL);

	size_t kernelLength = 10;

	char* path = "hello.cl";

	char* kernelSrc;// = oclLoadProgSource(path, "", &kernelLength);

	#pragma insieme kernelFile "hello.cl"
	program = clCreateProgramWithSource(context, 1, (const char**) &kernelSrc, &kernelLength, &err);

	kernel.fct = clCreateKernel(program, "hello", &err);
	err = clSetKernelArg(kernel.fct, 0, sizeof(cl_mem), (void*) &dev_ptr1);
	for(int i = 0; i < 1; ++i)
		err = clSetKernelArg(kernel.fct, 1, sizeof(cl_mem), (void*) &(dev_ptr4[i]));
	// local memory
	clSetKernelArg(kernel.fct, 2, sizeof(float) * n, 0);
	// private memory
	cl_int ta = 7;
	clSetKernelArg(kernel.fct , 3, sizeof(cl_int), &ta);
	cl_short2 sv = {0,1};
	clSetKernelArg(kernel.fct , 4, sizeof(cl_short2), &sv);


	size_t globalSize[] = { 8, 8 };
	size_t localSize[] = { 3, 5, 6 };

	for(int i = 0; i < 1; ++i)
		err = subfunction(kernel.fct, kernel.queue, globalSize, localSize, context, dev_ptr5);
//		err = clEnqueueNDRangeKernel(kernel.queue, kernel, 2, NULL, globalSize, localSize, 0, NULL, &event);

	err = clWaitForEvents(1, &event);

	clEnqueueReadBuffer(kernel.queue, dev_ptr4[0], CL_TRUE, 0, sizeof(cl_float) * 100, host_ptr, 0, NULL, NULL);
	clFinish(kernel.queue);

	cl_ulong start, end;
	clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end, NULL);
	clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start, NULL);


	clReleaseMemObject(dev_ptr1);
	clReleaseMemObject(dev_ptr2);
	clReleaseMemObject(dev_ptr3);
	for(int i = 1; i < 2; ++i)
		clReleaseMemObject(dev_ptr4[i]);
	clReleaseMemObject(dev_ptr5);

	clReleaseCommandQueue(kernel.queue);
	clReleaseContext(context);
	clReleaseEvent(event);
	clReleaseKernel(kernel.fct);
	free(host_ptr);
//	free(device);


	return 0;
}
