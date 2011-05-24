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
	if (id == NULL) {
		fprintf (stderr, "OCL ERROR: function %s is called with an invalid platform id in file %s, at line %d\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);
		exit (EXIT_FAILURE);
	}
	
	for (cl_uint i = 0; i < IRT_CL_NUM_PLATFORM_PARAMS; i++) {
		size_t cl_param_size;
		char* cl_param_value;
		clGetPlatformInfo(*id, _irt_cl_platform_params[i].name, 0, NULL, &cl_param_size); // FIXME: check for errors
		cl_param_value = alloca (cl_param_size);
		if (!cl_param_value) {
			fprintf (stderr, "OCL ERROR: cannot allocate memory for string buffer in file %s, at line %d\n", __FILE__, __LINE__);
			exit (EXIT_FAILURE);
		}
		clGetPlatformInfo(*id, _irt_cl_platform_params[i].name, cl_param_size, cl_param_value, NULL); // FIXME: check for errors
		printf("%-25s = \"%s\"\n", _irt_cl_platform_params[i].name_string, cl_param_value);
	}
	printf("\n");
}

static cl_uint _irt_cl_get_num_platforms(){
	cl_uint cl_num_platforms;
	cl_int err_code = clGetPlatformIDs(0, NULL, &cl_num_platforms);
	if (err_code != CL_SUCCESS) return 0;
	return cl_num_platforms;
}

static void _irt_cl_get_platforms(cl_uint num_platforms, cl_platform_id* platforms) {
	clGetPlatformIDs(num_platforms, platforms, NULL); // FIXME: check for errors
}

/* 
 * =====================================================================================
 *  OpenCL Devices Informations
 * =====================================================================================
 */

static void _irt_cl_print_device_info(cl_device_id* id) {
	if (id == NULL) {
		fprintf (stderr, "OCL ERROR: function %s is called with an invalid device id in file %s, at line %d\n", __PRETTY_FUNCTION__, __FILE__, __LINE__);
		exit (EXIT_FAILURE);
	}
	
	for (cl_uint i = 0; i < IRT_CL_NUM_DEVICE_PARAMS; i++) {
		size_t cl_param_size;
		char* cl_param_value;
		clGetDeviceInfo(*id, _irt_cl_device_params[i].name, 0, NULL, &cl_param_size); // FIXME: check for errors
		cl_param_value = alloca (cl_param_size);
		if (!cl_param_value) {
			fprintf (stderr, "OCL ERROR: cannot allocate memory for string buffer in file %s, at line %d\n", __FILE__, __LINE__);
			exit (EXIT_FAILURE);
		}
		clGetDeviceInfo(*id, _irt_cl_device_params[i].name, cl_param_size, cl_param_value, NULL); // FIXME: check for errors
		printf("%-25s = \"%s\"\n", _irt_cl_device_params[i].name_string, cl_param_value);
	}
	printf("\n");
}

cl_uint _irt_cl_get_num_devices(cl_platform_id* platform, cl_device_type device_type){
	cl_uint cl_num_devices;
	cl_int err_code = clGetDeviceIDs(*platform, device_type, 0, NULL, &cl_num_devices);
	if (err_code == CL_DEVICE_NOT_FOUND) return 0;
	return cl_num_devices;
}

static void _irt_cl_get_devices(cl_platform_id* platform, cl_device_type device_type, cl_uint num_devices, cl_device_id* devices) {
	clGetDeviceIDs(*platform, device_type, num_devices, devices, NULL); // FIXME: check for errors
}


static char* _irt_load_program_source (const char* filename, size_t* filesize) {
	FILE *fp;
	int err, size;
	char *source;
	fp = fopen(filename, "rb");
	if(fp == NULL) {
		printf("Could not open kernel file: %s\n", filename);
		exit(-1);
	}
	err = fseek(fp, 0, SEEK_END);
	if(err != 0) {
		printf("Error seeking to end of file\n");
		exit(-1);
	}
	size = ftell(fp);
	if(size < 0) {
		printf("Error getting file position\n");
		exit(-1);
	}
	err = fseek(fp, 0, SEEK_SET);
	if(err != 0) {
		printf("Error seeking to start of file\n");
		exit(-1);
	}
	source = (char*)malloc(size+1);
	if(source == NULL) {
		printf("Error allocating %d bytes for the program source\n", size+1);
		exit(-1);
	}
	err = fread(source, 1, size, fp);
	if(err != size) {
		printf("only read %d bytes\n", err);
		exit(0);
	}
	source[size] = '\0';
	*filesize = size; // this is the size useful for create program from binary
	return source;
}

static void _irt_save_program_binary (cl_program program, const char* binary_filename) {
	/* check for input errors */
	if (program == NULL) {
  		exit (EXIT_FAILURE);
	}

	/*  Get the size of the binaries associated with the program */
	size_t err; 
	size_t size_ret;
	clGetProgramInfo (program, CL_PROGRAM_BINARY_SIZES, 0, NULL, &size_ret);
      	//printf("size_ret %u\n", size_ret);

	/*  allocate an array to store the sizes of the binaries */
	size_t* binary_size = (size_t *) alloca (size_ret);
	if (!binary_size) {
  		exit (EXIT_FAILURE);
	}

	/* query the sizes of the binaries */
	clGetProgramInfo (program, CL_PROGRAM_BINARY_SIZES, size_ret, binary_size, NULL);
      	//printf("bin_size %zu\n", *binary_size);

	/* allocate space for each binary associated with the program from the sizes queried above */
    	unsigned char* binary = (unsigned char *) alloca (sizeof (unsigned char) * (*binary_size));
    	if (!binary) {
      		fprintf (stderr, "ERROR in function %s, failed to allocate memory for storing binaries[i], file=%s, line=%d\n", 
      	 	__PRETTY_FUNCTION__, __FILE__, __LINE__);
      		exit (EXIT_FAILURE);
    	}

	/* Get the binaries */
	clGetProgramInfo (program, CL_PROGRAM_BINARIES, sizeof (unsigned char *), &binary, NULL);

	FILE *fh = fopen (binary_filename, "w");
	if (fh == NULL) {
  		fprintf (stderr, "fopen(%s,'w') failed errno=\n", binary_filename);
  		exit (EXIT_FAILURE);
	}
	
	/*  write the binary into the file */
	err = fwrite (binary, 1, *binary_size, fh);
	if (err != (size_t) *binary_size) {
  		exit (EXIT_FAILURE);
	}

	err = fclose (fh);
	if (err != 0) {
		fprintf (stderr, "close failed err= errno=\n");
 		 exit (EXIT_FAILURE);
	}	
}

/* 
 * =====================================================================================
 *  Insieme Runtime OpenCL Devices
 * =====================================================================================
 */

cl_uint irt_ocl_get_num_devices(){
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

void irt_ocl_get_devices(irt_ocl_device* devices){
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
					if (status != CL_SUCCESS || dev->cl_context == NULL){
						// FIXME check for errors
						printf("Error during creation of Context\n");
						exit(-1);
					}
					
					dev->cl_queue = clCreateCommandQueue(dev->cl_context, dev->cl_device, CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE | CL_QUEUE_PROFILING_ENABLE, &status);
					if (status != CL_SUCCESS || dev->cl_queue == NULL){
						// FIXME check for errors
						printf("Error during creation of Queue \n");
						exit(-1);
					}
				}
			}
		}
	}
}

void irt_ocl_release_device(irt_ocl_device* dev){
	clReleaseContext(dev->cl_context);
	clReleaseCommandQueue(dev->cl_queue);
}


void irt_ocl_print_device_info(irt_ocl_device* dev){
	_irt_cl_print_device_info(&(dev->cl_device));
}

float irt_ocl_profile_event(cl_event event, cl_profiling_info event_start, cl_profiling_info event_end, irt_ocl_profile_event_flag time_flag) {
	return irt_ocl_profile_events(event, event_start, event, event_end, time_flag);
}

float irt_ocl_profile_events(cl_event event_one, cl_profiling_info event_one_command, cl_event event_two, cl_profiling_info event_two_command, irt_ocl_profile_event_flag time_flag) {
	cl_ulong event_one_start, event_two_end;
	cl_int errcode;
	errcode = clGetEventProfilingInfo(event_two, event_two_command, sizeof(cl_ulong), &event_two_end, NULL);
	errcode = clGetEventProfilingInfo(event_one, event_one_command, sizeof(cl_ulong), &event_one_start, NULL);
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

cl_kernel  irt_ocl_create_kernel(irt_ocl_device* dev, const char* filename, const char* kernel_name, const char* build_options, irt_ocl_create_kernel_flag flag){
	// FIXME: assert for everythig
	cl_kernel kernel = NULL;
	cl_program program = NULL;
	size_t filesize = 0;	
	char* program_source = _irt_load_program_source(filename, &filesize);
	if (program_source == NULL) {
		exit (EXIT_FAILURE);
      	}
	//printf("Program Source: %s\n", program_source);
	if (flag == IRT_OCL_SOURCE){ // FIXME write better way
		cl_int status;
		program = clCreateProgramWithSource (dev->cl_context, 1, (const char **) &program_source, NULL, &status);
		if(status != CL_SUCCESS) {
			printf("clCreateProgramWithSource failed\n");
			exit(-1);
		}
		if (program == NULL) {
			printf("Error: Failed to create compute program!\n") ;
			exit(-1);
		}
		status = clBuildProgram(program, 1, &(dev->cl_device), build_options, NULL, NULL);
			
		// If there are build errors, print them to the screen
		if(status != CL_SUCCESS) {
			printf("Program failed to build.\n");
			cl_build_status buildStatus;
			clGetProgramBuildInfo(program, dev->cl_device, CL_PROGRAM_BUILD_STATUS,
			sizeof(cl_build_status), &buildStatus, NULL);
			char *buildLog;
			size_t buildLogSize;
			clGetProgramBuildInfo(program, dev->cl_device, CL_PROGRAM_BUILD_LOG, 0, NULL, &buildLogSize);
			buildLog = (char*)malloc(buildLogSize);
			if(buildLog == NULL) {
				exit(-1);
			}	

			clGetProgramBuildInfo(program, dev->cl_device, CL_PROGRAM_BUILD_LOG, buildLogSize, buildLog, NULL);
			buildLog[buildLogSize-1] = '\0';
			printf("Device Build Log:\n%s\n", buildLog);   
			free(buildLog);
			exit(0);
		}
		else {
			printf("No build errors\n");
		}

		_irt_save_program_binary(program, "esatto.bin");

		cl_int binary_status;
		unsigned char* program_s = (unsigned char*) _irt_load_program_source("esatto.bin", &filesize); 
		program = clCreateProgramWithBinary (dev->cl_context, 1, &(dev->cl_device), &filesize, (const unsigned char **) &program_s, &binary_status, &status);
		free(program_s);
		if((status != CL_SUCCESS) && (binary_status != CL_SUCCESS)) { // FIXME: check even for binary_status
			printf("clCreateProgramWithBinary failed\n");
			exit(-1);
		}
		if (program == NULL) {
			printf("Error: Failed to create compute program!\n") ;
			exit(-1);
		}
		clBuildProgram(program, 1, &(dev->cl_device), build_options, NULL, NULL);
		
		kernel = clCreateKernel(program, kernel_name, &status);
		if(status != CL_SUCCESS) {
			printf("clCreateKernel failed\n");
			exit(-1);
		}	
	}
	free(program_source);
	return kernel;
}
