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

#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include "irt_ocl.h"

/*
 * =====================================================================================
 *  OpenCL Platforms Informations
 * =====================================================================================
 */

void _irt_cl_print_platform_info(cl_platform_id* id) {
	IRT_ASSERT(id != NULL, IRT_ERR_OCL, "Error: invalid platform");
	for (cl_uint i = 0; i < IRT_CL_NUM_PLATFORM_PARAMS; i++) {
		size_t cl_param_size;
		char* cl_param_value;
		IRT_ASSERT(clGetPlatformInfo(*id, _irt_cl_platform_params[i].name, 0, NULL, &cl_param_size) == CL_SUCCESS, IRT_ERR_OCL, "Error getting platform info");
		cl_param_value = alloca (cl_param_size);
		IRT_ASSERT(clGetPlatformInfo(*id, _irt_cl_platform_params[i].name, cl_param_size, cl_param_value, NULL) == CL_SUCCESS, IRT_ERR_OCL, "Error getting platform info");
		IRT_INFO("%-25s = \"%s\"\n", _irt_cl_platform_params[i].name_string, cl_param_value);
	}
	IRT_INFO("\n");
}

static cl_uint _irt_cl_get_num_platforms(){
	cl_uint cl_num_platforms;
	if (clGetPlatformIDs(0, NULL, &cl_num_platforms) != CL_SUCCESS) return 0;
	return cl_num_platforms;
}

static void _irt_cl_get_platforms(cl_uint num_platforms, cl_platform_id* platforms) {
	IRT_ASSERT(clGetPlatformIDs(num_platforms, platforms, NULL) == CL_SUCCESS, IRT_ERR_OCL, "Error getting platforms");
}

/* 
 * =====================================================================================
 *  OpenCL Devices Informations
 * =====================================================================================
 */

static void _irt_cl_print_device_info(cl_device_id* id) {
	IRT_ASSERT(id != NULL, IRT_ERR_OCL, "Error: invalid device");
	for (cl_uint i = 0; i < IRT_CL_NUM_DEVICE_PARAMS; i++) {
		size_t cl_param_size;
		char* cl_param_value;
		IRT_ASSERT(clGetDeviceInfo(*id, _irt_cl_device_params[i].name, 0, NULL, &cl_param_size) == CL_SUCCESS, IRT_ERR_OCL, "Error getting device info");
		cl_param_value = alloca (cl_param_size);
		IRT_ASSERT(clGetDeviceInfo(*id, _irt_cl_device_params[i].name, cl_param_size, cl_param_value, NULL)  == CL_SUCCESS, IRT_ERR_OCL, "Error getting device info");
		IRT_INFO("%-25s = \"%s\"\n", _irt_cl_device_params[i].name_string, cl_param_value);
	}
	IRT_INFO("\n");
}

cl_uint _irt_cl_get_num_devices(cl_platform_id* platform, cl_device_type device_type){
	cl_uint cl_num_devices;
	if (clGetDeviceIDs(*platform, device_type, 0, NULL, &cl_num_devices) != CL_SUCCESS) return 0;
	return cl_num_devices;
}

static void _irt_cl_get_devices(cl_platform_id* platform, cl_device_type device_type, cl_uint num_devices, cl_device_id* devices) {
	IRT_ASSERT(clGetDeviceIDs(*platform, device_type, num_devices, devices, NULL) == CL_SUCCESS, IRT_ERR_OCL, "Error getting devices"); 
}


/* 
* =====================================================================================
*  OpenCL Load & Save Program Utility
* =====================================================================================
*/

static char* _irt_load_program_source (const char* filename, size_t* filesize) { // remember to free the returned source
	IRT_ASSERT(filename != NULL && filesize != NULL, IRT_ERR_OCL, "Error input parameters");
	FILE* fp = fopen(filename, "rb");
	IRT_ASSERT(fp != NULL, IRT_ERR_OCL, "Error opening kernel file");
	IRT_ASSERT(fseek(fp, 0, SEEK_END) == 0, IRT_ERR_OCL, "Error seeking to end of file");
	int size = ftell(fp);
	IRT_ASSERT(size >= 0, IRT_ERR_OCL, "Error getting file position");
	IRT_ASSERT(fseek(fp, 0, SEEK_SET) == 0, IRT_ERR_OCL, "Error seeking to begin of file");
	char* source = (char*)malloc(size+1);
	IRT_ASSERT(source != NULL, IRT_ERR_OCL, "Error allocating space for program source");
	IRT_ASSERT(fread(source, 1, size, fp) == size, IRT_ERR_OCL, "Error reading file");
	source[size] = '\0';
	*filesize = size; // this is the size useful for create program from binary
	return source;
}

static void _irt_save_program_binary (cl_program program, const char* binary_filename) {
	IRT_ASSERT(binary_filename != NULL && program != NULL, IRT_ERR_OCL, "Error input parameters");
	size_t size_ret;
	IRT_ASSERT(clGetProgramInfo (program, CL_PROGRAM_BINARY_SIZES, 0, NULL, &size_ret) == CL_SUCCESS, IRT_ERR_OCL, "Error getting program info");
	size_t* binary_size = (size_t *) alloca (size_ret);
	IRT_ASSERT(binary_size != NULL, IRT_ERR_OCL, "Error allocating binary_size");
	IRT_ASSERT(clGetProgramInfo (program, CL_PROGRAM_BINARY_SIZES, size_ret, binary_size, NULL) == CL_SUCCESS,  IRT_ERR_OCL, "Error getting program info");
    	unsigned char* binary = (unsigned char *) alloca (sizeof (unsigned char) * (*binary_size));
	IRT_ASSERT(binary != NULL, IRT_ERR_OCL, "Error allocating binary");

	// get the binary
	IRT_ASSERT(clGetProgramInfo (program, CL_PROGRAM_BINARIES, sizeof (unsigned char *), &binary, NULL) == CL_SUCCESS,  IRT_ERR_OCL, "Error getting program info");

	FILE *fp = fopen (binary_filename, "w");
	IRT_ASSERT(fp != NULL, IRT_ERR_OCL, "Error opening binary file");
	IRT_ASSERT(fwrite (binary, 1, *binary_size, fp) ==  (size_t) *binary_size, IRT_ERR_OCL, "Error writing file");
	IRT_ASSERT(fclose (fp) == 0, IRT_ERR_OCL, "Error closing the file");
}

/* 
 * =====================================================================================
 *  Insieme Runtime OpenCL Devices
 * =====================================================================================
 */

cl_uint irt_ocl_get_num_devices() {
	cl_uint num_devices = 0;
	cl_platform_id* cl_platforms;
	cl_uint cl_num_platforms = _irt_cl_get_num_platforms();
	if (cl_num_platforms != 0) {
		cl_platforms = (cl_platform_id*)alloca(cl_num_platforms * sizeof(cl_platform_id));
		_irt_cl_get_platforms(cl_num_platforms, cl_platforms);
		
		for (cl_uint i = 0; i < cl_num_platforms; i++){
			cl_device_type device_type = DEVICE_TYPE;
			cl_uint cl_num_devices = _irt_cl_get_num_devices(&cl_platforms[i], device_type);
			num_devices += cl_num_devices;
		}
	}
	return num_devices;
}

void irt_ocl_get_devices(irt_ocl_device* devices) {
	cl_uint index = 0;
	cl_platform_id* cl_platforms;
	cl_uint cl_num_platforms = _irt_cl_get_num_platforms();
	if (cl_num_platforms != 0) {
		cl_platforms = (cl_platform_id*)alloca(cl_num_platforms * sizeof(cl_platform_id));
		_irt_cl_get_platforms(cl_num_platforms, cl_platforms);
		
		for (cl_uint i = 0; i < cl_num_platforms; i++){
			cl_device_type device_type = DEVICE_TYPE;
			cl_device_id* cl_devices;
			cl_uint cl_num_devices = _irt_cl_get_num_devices(&cl_platforms[i], device_type);
			if (cl_num_devices != 0) {
				cl_devices = (cl_device_id*)alloca(cl_num_devices * sizeof(cl_device_id));
				_irt_cl_get_devices(&cl_platforms[i], device_type, cl_num_devices, cl_devices);
				for (int i = 0; i < cl_num_devices; ++i, ++index){
					cl_int status;
					irt_ocl_device* dev = &devices[index];
					dev->cl_device = cl_devices[i];
					dev->cl_context = clCreateContext(NULL, 1, &dev->cl_device, NULL, NULL, &status);
					IRT_ASSERT(status == CL_SUCCESS && dev->cl_context != NULL, IRT_ERR_OCL, "Error creating context");
					dev->cl_queue = clCreateCommandQueue(dev->cl_context, dev->cl_device, CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE | CL_QUEUE_PROFILING_ENABLE, &status);
					IRT_ASSERT(status == CL_SUCCESS && dev->cl_queue != NULL, IRT_ERR_OCL, "Error creating queue");
				}
			}
		}
	}
}

void irt_ocl_release_device(irt_ocl_device* dev) {
	clReleaseContext(dev->cl_context);
	clReleaseCommandQueue(dev->cl_queue);
}

void irt_ocl_print_device_info(irt_ocl_device* dev) {
	_irt_cl_print_device_info(&(dev->cl_device));
}

float irt_ocl_profile_event(cl_event event, cl_profiling_info event_start, cl_profiling_info event_end, irt_ocl_profile_event_flag time_flag) {
	return irt_ocl_profile_events(event, event_start, event, event_end, time_flag);
}

float irt_ocl_profile_events(cl_event event_one, cl_profiling_info event_one_command, cl_event event_two, cl_profiling_info event_two_command, irt_ocl_profile_event_flag time_flag) {
	cl_ulong event_one_start, event_two_end;
	IRT_ASSERT(clGetEventProfilingInfo(event_two, event_two_command, sizeof(cl_ulong), &event_two_end, NULL) == CL_SUCCESS, IRT_ERR_OCL, "Error getting profiling info");
	IRT_ASSERT(clGetEventProfilingInfo(event_one, event_one_command, sizeof(cl_ulong), &event_one_start, NULL) == CL_SUCCESS, IRT_ERR_OCL, "Error getting profiling info");
	float time = 0.0;
	switch(time_flag){
		case IRT_OCL_NANO:
			time = (event_two_end - event_one_start);
			break;
		case IRT_OCL_MILLI:
			time = (event_two_end - event_one_start) * 1.0e-6f;
			break;
		case IRT_OCL_SEC:
			time = (event_two_end - event_one_start) * 1.0e-9f;
		 	break;
	}
	return time;
}

cl_kernel  irt_ocl_create_kernel(irt_ocl_device* dev, const char* filename, const char* kernel_name, const char* build_options, irt_ocl_create_kernel_flag flag) {
	cl_kernel kernel = NULL;
	cl_program program = NULL;
	size_t filesize = 0;	
	char* program_source = _irt_load_program_source(filename, &filesize);
	IRT_ASSERT(program_source != NULL, IRT_ERR_OCL, "Error loading kernel program source");
	if (flag == IRT_OCL_SOURCE){ // FIXME write in a better way
		cl_int status;
		program = clCreateProgramWithSource (dev->cl_context, 1, (const char **) &program_source, NULL, &status);
		IRT_ASSERT(status == CL_SUCCESS && program != NULL, IRT_ERR_OCL, "Error creating compute program");
		status = clBuildProgram(program, 1, &(dev->cl_device), build_options, NULL, NULL);
		
		// If there are build errors, print them to the screen
		if(status != CL_SUCCESS) {
			IRT_INFO("Kernel program failed to build.\n");
			char *buildLog;
			size_t buildLogSize;
			IRT_ASSERT(clGetProgramBuildInfo(program, dev->cl_device, CL_PROGRAM_BUILD_LOG, 0, NULL, &buildLogSize) == CL_SUCCESS, IRT_ERR_OCL, "Error getting program build info");
			buildLog = (char*)malloc(buildLogSize);
			IRT_ASSERT(clGetProgramBuildInfo(program, dev->cl_device, CL_PROGRAM_BUILD_LOG, buildLogSize, buildLog, NULL) == CL_SUCCESS, IRT_ERR_OCL, "Error getting program build info");
			buildLog[buildLogSize-1] = '\0';
			IRT_INFO("Device Build Log:\n%s\n", buildLog); 
			free(buildLog);
		}
		IRT_ASSERT(status == CL_SUCCESS, IRT_ERR_OCL, "Error building compute program");	
		
		// FIXME: save and load only to test...
		_irt_save_program_binary(program, "esatto.bin"); // FIXME: define a variable name

		cl_int binary_status;
		unsigned char* program_s = (unsigned char*) _irt_load_program_source("esatto.bin", &filesize); 
		program = clCreateProgramWithBinary (dev->cl_context, 1, &(dev->cl_device), &filesize, (const unsigned char **) &program_s, &binary_status, &status);
		free(program_s);
		IRT_ASSERT(status == CL_SUCCESS && binary_status == CL_SUCCESS && program != NULL, IRT_ERR_OCL, "Error creating compute program");
		IRT_ASSERT(clBuildProgram(program, 1, &(dev->cl_device), build_options, NULL, NULL) == CL_SUCCESS, IRT_ERR_OCL, "Error building compute program");
		
		kernel = clCreateKernel(program, kernel_name, &status);
		IRT_ASSERT(status == CL_SUCCESS, IRT_ERR_OCL, "Error creating kernel");
	}
	free(program_source);
	return kernel;
}
