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

#include "CL/cl.h"
//#include "/home/klaus/NVIDIA_GPU_Computing_SDK/OpenCL/common/inc/oclUtils.h"
cl_program program = NULL;
cl_context gcontext = NULL;
cl_command_queue gqueue = NULL;
short* short_host_ptr;
cl_mem dev_ptr1 = NULL;
cl_event event = NULL;

cl_int subfunction(cl_kernel kernel, cl_command_queue queue, size_t* globalSize, size_t* localSize) {
	return clEnqueueNDRangeKernel(queue, kernel, 2, NULL, globalSize, localSize, 0, NULL, &event);
}

//#pragma insieme mark
int main(int argc, char **argv) {
	cl_kernel kernel[2] = {NULL};
	cl_int err;
	cl_device_id* device;// = (cl_device_id*)malloc(sizeof(cl_device_id*));
	cl_platform_id* platforms;
	cl_command_queue* queue;
	cl_context context;

	cl_uint n;
	err = clGetPlatformIDs(10, platforms, NULL);
	clGetDeviceIDs(platforms[0], 0, 1, device, &n);

	context = clCreateContext(0, 1, &device[0], NULL, NULL, &err);
	gcontext = clCreateContext(0, 1, device, NULL, NULL, &err);
	queue[0] = clCreateCommandQueue(context, device[0], CL_QUEUE_PROFILING_ENABLE, &err);
	gqueue = clCreateCommandQueue(gcontext, device[0], CL_QUEUE_PROFILING_ENABLE, &err);

	float* host_ptr;
	cl_mem dev_ptr2 = clCreateBuffer(context, CL_MEM_READ_WRITE , sizeof(cl_float) * 100, host_ptr, NULL);
	cl_mem dev_ptr3 = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, sizeof(cl_float) * 100, host_ptr, &err);
	cl_mem dev_ptr4[2];

	dev_ptr1 = clCreateBuffer(gcontext, CL_MEM_READ_ONLY, 100 * sizeof(cl_short), short_host_ptr, &err);
	dev_ptr4[0] = clCreateBuffer(gcontext, CL_MEM_WRITE_ONLY, 100 * sizeof(cl_float), NULL, &err);
	dev_ptr4[1] = clCreateBuffer(gcontext, CL_MEM_WRITE_ONLY, 100 * sizeof(cl_float), NULL, &err);

	clEnqueueWriteBuffer(gqueue, dev_ptr1, CL_TRUE, 0, sizeof(cl_float) * 100, host_ptr, 0, NULL, NULL);

	size_t kernelLength = 10;

	char* path = "hello.cl";

	char* kernelSrc;// = oclLoadProgSource(path, "", &kernelLength);

	#pragma insieme kernelFile "hello.cl"
	program = clCreateProgramWithSource(context, 1, (const char**) &kernelSrc, &kernelLength, &err);

	kernel[1] = clCreateKernel(program, "hello", &err);
	err = clSetKernelArg(kernel[1], 0, sizeof(cl_mem), (void*) &dev_ptr1);
	for(int i = 0; i < 1; ++i)
		err = clSetKernelArg(kernel[i], 1, sizeof(cl_mem), (void*) &(dev_ptr4[i]));
	// local memory
	clSetKernelArg(kernel[1], 2, sizeof(float) * n, 0);
	// private memory
	cl_int ta = 7;
	clSetKernelArg(kernel[1] , 3, sizeof(cl_int), &ta);
	cl_short2 sv = {0,1};
	clSetKernelArg(kernel[1] , 4, sizeof(cl_short2), &sv);


	size_t globalSize[] = { 8, 8 };
	size_t localSize[] = { 3, 5, 6 };

	for(int i = 0; i < 1; ++i)
		err = subfunction(kernel[i], queue[0], globalSize, localSize);
//		err = clEnqueueNDRangeKernel(queue[0], kernel[i], 2, NULL, globalSize, localSize, 0, NULL, &event);

	err = clWaitForEvents(1, &event);

	clEnqueueReadBuffer(queue[0], dev_ptr4[0], CL_TRUE, 0, sizeof(cl_float) * 100, host_ptr, 0, NULL, NULL);
	clFinish(queue[0]);

	cl_ulong start, end;
	clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end, NULL);
	clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start, NULL);


	clReleaseMemObject(dev_ptr1);
	clReleaseMemObject(dev_ptr2);
	clReleaseMemObject(dev_ptr3);
	for(int i = 1; i < 2; ++i)
		clReleaseMemObject(dev_ptr4[i]);

	clReleaseCommandQueue(queue[0]);
	clReleaseCommandQueue(gqueue);
	clReleaseContext(context);
	clReleaseEvent(event);
	if(kernel[0]) clReleaseKernel(kernel[0]);
	free(host_ptr);
//	free(device);


	return 0;
}
