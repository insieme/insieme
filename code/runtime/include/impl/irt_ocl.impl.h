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
#include <ctype.h>
#include "irt_ocl.h"

/*
 * =====================================================================================
 *  OpenCL Static Internal Functions Declarations
 * =====================================================================================
 */

static cl_uint _irt_cl_get_num_platforms();
static void _irt_cl_get_platforms(cl_uint num_platforms, cl_platform_id* platforms);

static cl_uint _irt_cl_get_num_devices(cl_platform_id* platform, cl_device_type device_type);
static void _irt_cl_get_devices(cl_platform_id* platform, cl_device_type device_type, cl_uint num_devices, cl_device_id* devices);

static const char* _irt_cl_get_device_type_string(cl_device_type type);
static char* _irt_cl_get_name(cl_device_id* device);
static cl_device_type _irt_cl_get_type(cl_device_id* device);
static char* _irt_cl_get_vendor(cl_device_id* device);
static char* _irt_cl_get_version(cl_device_id* device);
static char* _irt_cl_get_driver_version(cl_device_id* device);
static char* _irt_cl_get_profile(cl_device_id* device);

static cl_uint _irt_cl_get_max_compute_units(cl_device_id* device);
static cl_uint _irt_cl_get_max_clock_frequency(cl_device_id* device);
static cl_uint _irt_cl_get_max_work_item_dimensions(cl_device_id* device);
static size_t* _irt_cl_get_max_work_item_sizes(cl_device_id* device);
static size_t _irt_cl_get_max_work_group_size(cl_device_id* device);

static cl_bool _irt_cl_has_image_support(cl_device_id* device);
static cl_device_fp_config _irt_cl_get_single_fp_config(cl_device_id* device);
static cl_bool _irt_cl_is_endian_little(cl_device_id* device);
static char* _irt_cl_get_extensions(cl_device_id* device);

static cl_ulong _irt_cl_get_global_mem_size(cl_device_id* device);
static cl_ulong _irt_cl_get_max_mem_alloc_size(cl_device_id* device);
static cl_device_mem_cache_type _irt_cl_get_global_mem_cache_type(cl_device_id* device);
static cl_uint _irt_cl_get_global_mem_cacheline_size(cl_device_id* device);
static cl_ulong _irt_cl_get_global_mem_cache_size(cl_device_id* device);

static cl_ulong _irt_cl_get_max_constant_buffer_size(cl_device_id* device);

static cl_device_local_mem_type _irt_cl_get_local_mem_type(cl_device_id* device);
static cl_ulong _irt_cl_get_local_mem_size(cl_device_id* device);

static char* _irt_load_program_source (const char* filename, size_t* filesize);
static void _irt_save_program_binary (cl_program program, const char* binary_filename);
static const char* _irt_error_string (cl_int err_code);


/*
 * =====================================================================================
 *  OpenCL Device Functions
 * =====================================================================================
 */

void irt_ocl_init_devices() {
	devices = NULL;
	num_devices = 0;
	cl_platform_id* cl_platforms;
	cl_uint cl_num_platforms = _irt_cl_get_num_platforms();
	if (cl_num_platforms != 0) {
		cl_platforms = (cl_platform_id*)alloca(cl_num_platforms * sizeof(cl_platform_id));
		_irt_cl_get_platforms(cl_num_platforms, cl_platforms);
		
		for (cl_uint i = 0; i < cl_num_platforms; i++) {
			cl_device_type device_type = DEVICE_TYPE;
			cl_uint cl_num_devices = _irt_cl_get_num_devices(&cl_platforms[i], device_type);
			num_devices += cl_num_devices;
		}
	}
	if (num_devices != 0) {
		devices = (irt_ocl_device*)malloc(num_devices * sizeof(irt_ocl_device));
		cl_uint index = 0;
		cl_platform_id* cl_platforms;
		cl_uint cl_num_platforms = _irt_cl_get_num_platforms();
		if (cl_num_platforms != 0) {
			cl_platforms = (cl_platform_id*)alloca(cl_num_platforms * sizeof(cl_platform_id));
			_irt_cl_get_platforms(cl_num_platforms, cl_platforms);
		
			for (cl_uint i = 0; i < cl_num_platforms; i++) {
				cl_device_type device_type = DEVICE_TYPE;
				cl_device_id* cl_devices;
				cl_uint cl_num_devices = _irt_cl_get_num_devices(&cl_platforms[i], device_type);
				if (cl_num_devices != 0) {
					cl_devices = (cl_device_id*)alloca(cl_num_devices * sizeof(cl_device_id));
					_irt_cl_get_devices(&cl_platforms[i], device_type, cl_num_devices, cl_devices);
					for (int i = 0; i < cl_num_devices; ++i, ++index) {
						cl_int err_code;
						irt_ocl_device* dev = &devices[index];
						dev->device = cl_devices[i];
						dev->context = clCreateContext(NULL, 1, &dev->device, NULL, NULL, &err_code);
						IRT_ASSERT(err_code == CL_SUCCESS &&dev->context != NULL, IRT_ERR_OCL, "Error creating context: \"%s\"", _irt_error_string(err_code));
						dev->queue = clCreateCommandQueue(dev->context, dev->device, 0, &err_code); //FIXME: CL_QUEUE_PROFILING_ENABLE, &err_code);
						IRT_ASSERT(err_code == CL_SUCCESS && dev->queue != NULL, IRT_ERR_OCL, "Error creating queue: \"%s\"", _irt_error_string(err_code));

						// Device Info
						dev->name = _irt_cl_get_name(&dev->device);
						dev->type = _irt_cl_get_type(&dev->device);
						dev->vendor = _irt_cl_get_vendor(&dev->device);
						dev->version = _irt_cl_get_version(&dev->device);
						dev->driver_version = _irt_cl_get_driver_version(&dev->device);
						dev->profile = _irt_cl_get_profile(&dev->device);

						dev->max_compute_units = _irt_cl_get_max_compute_units(&dev->device);
						dev->max_clock_frequency = _irt_cl_get_max_clock_frequency(&dev->device);
						dev->max_work_item_dimensions = _irt_cl_get_max_work_item_dimensions(&dev->device);
						dev->max_work_item_sizes = _irt_cl_get_max_work_item_sizes(&dev->device);
						dev->max_work_group_size = _irt_cl_get_max_work_group_size(&dev->device);

						dev->image_support = _irt_cl_has_image_support(&dev->device);
						dev->single_fp_config = _irt_cl_get_single_fp_config(&dev->device);
						dev->endian_little = _irt_cl_is_endian_little(&dev->device);
						dev->extensions = _irt_cl_get_extensions(&dev->device);

						dev->mem_cache_type = _irt_cl_get_global_mem_cache_type(&dev->device);
						dev->global_mem_cacheline_size = _irt_cl_get_global_mem_cacheline_size(&dev->device);
						dev->global_mem_cache_size = _irt_cl_get_global_mem_cache_size(&dev->device);

						dev->max_constant_buffer_size = _irt_cl_get_max_constant_buffer_size(&dev->device);

						dev->local_mem_type = _irt_cl_get_local_mem_type(&dev->device);
						dev->local_mem_size = _irt_cl_get_local_mem_size(&dev->device);

						// Buffer Info
						dev->mem_size = _irt_cl_get_global_mem_size(&dev->device);
						dev->mem_available = _irt_cl_get_global_mem_size(&dev->device);
						dev->max_buffer_size = _irt_cl_get_max_mem_alloc_size(&dev->device);
						dev->buffer = NULL;
						pthread_spin_init(&(dev->buffer_lock), 0);
					}
				}
			}
		}
	}
}

void irt_ocl_release_devices() {
	cl_int err_code;
	for (int i = 0; i < num_devices; ++i) {
		irt_ocl_device* dev = &devices[i];
		// release the buffer_lock
		pthread_spin_destroy(&(dev->buffer_lock));
		// release the buffer list 
		irt_ocl_buffer* tmp = dev->buffer;
		while(tmp) {
			dev->buffer = (dev->buffer)->next;
			err_code = clReleaseMemObject(tmp->mem);
			IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error releasing cl_mem: \"%s\"", _irt_error_string(err_code));
			free(tmp);
			tmp = dev->buffer;
		}
		// release the queue
		err_code = clReleaseCommandQueue(dev->queue);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error releasing command queue: \"%s\"", _irt_error_string(err_code));
		// release the context
		err_code = clReleaseContext(dev->context);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error releasing context: \"%s\"", _irt_error_string(err_code));

		// release the info
		free(dev->name);
		free(dev->vendor);
		free(dev->version);
		free(dev->driver_version);
		free(dev->profile);
		free(dev->extensions);
		free(dev->max_work_item_sizes);
	}
	free(devices);
}

inline cl_uint irt_ocl_get_num_devices() {
	return num_devices;
}

inline irt_ocl_device* irt_ocl_get_device(cl_uint id) {
	IRT_ASSERT(id < num_devices && id >= 0, IRT_ERR_OCL, "Error accessing device with wrong ID");
	return &devices[id];
}

/*
 * =====================================================================================
 *  OpenCL Buffer Functions
 * =====================================================================================
 */

irt_ocl_buffer* irt_ocl_create_buffer(irt_ocl_device* dev, cl_mem_flags flags, size_t size) {
	IRT_ASSERT(size <= dev->max_buffer_size, IRT_ERR_OCL, "Error creating buffer: \"Buffer size is too big\"");
	//printf("Available Memory: %lu   Request Memory: %lu\n", dev->mem_available, size);
	if (size > dev->mem_available) {
		pthread_spin_lock(&(dev->buffer_lock));
		//printf(" Need to free some buffer\n");

		irt_ocl_buffer* ptr = dev->buffer;
		irt_ocl_buffer* prev = NULL;
		while (ptr) {
			if ((ptr->size > size) && (ptr->used == false)) 
				break;
			prev = ptr;
			ptr = ptr->next;
		}
		if (ptr) printf("YES %lu < %lu %u\n", ptr->size, size, ptr->used);
		IRT_ASSERT(ptr != NULL, IRT_ERR_OCL, "Error creating buffer: \"Not enough memory\""); // FIXME: assert if an element is not found
		// FIXME: maybe more than one element can be released
		if (!prev) {
			dev->buffer = NULL;
		} else {
			prev->next = ptr->next;
		}
		// free the selected buffer
		cl_int err_code = clReleaseMemObject(ptr->mem);
		dev->mem_available += ptr->size;
		//printf("Released Buffer: %lu  Available Memory:%lu\n", ptr->size, dev->mem_available);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error releasing cl_mem: \"%s\"", _irt_error_string(err_code));
		free(ptr);
		pthread_spin_unlock(&(dev->buffer_lock));
	}
	
	// create buffer
	irt_ocl_buffer* buf = (irt_ocl_buffer*)malloc(sizeof(irt_ocl_buffer));
	cl_int err_code;
	buf->mem = clCreateBuffer(dev->context, flags, size, NULL, &err_code);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error creating buffer: \"%s\"", _irt_error_string(err_code));
	buf->used = true;
	buf->size = size;
	buf->next = NULL;
	buf->queue =dev->queue;
	
	// add buffer to the list
	pthread_spin_lock(&(dev->buffer_lock));
	irt_ocl_buffer* ptr = dev->buffer;
	irt_ocl_buffer* prev = NULL;
	while(ptr && (ptr->size <= size)) {
		prev = ptr;
		ptr = ptr->next;
	}
	if (!prev) { // prev == NULL
		buf->next = ptr;
		dev->buffer = buf;
	} else {
		//insert after prev
		buf->next = prev->next;
		prev->next = buf;
	}
	//update the available memory
	dev->mem_available -= size;
	pthread_spin_unlock(&(dev->buffer_lock));

	// print buffer list status
	/*printf("LIST BEGIN\n");
	irt_ocl_buffer* test = dev->cl_buffer;
	while (test) {
		printf("Value: %lu\n", test->size);
		test = test->next;
	}
	printf("LIST END\n");*/
	//

	return buf;
}

inline void irt_ocl_release_buffer(irt_ocl_buffer* buf) {
	IRT_ASSERT(buf != NULL && buf->used == true, IRT_ERR_OCL, "Error releasing buffer");
	buf->used = false;
}

inline void irt_ocl_write_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t offset, size_t size, const void* source_ptr) {
	cl_int err_code = clEnqueueWriteBuffer(buf->queue, buf->mem, blocking, offset, size, source_ptr, 0, NULL, NULL);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error writing buffer: \"%s\"",  _irt_error_string(err_code));
}

inline void irt_ocl_read_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t offset, size_t size, void* source_ptr) {
	cl_int err_code = clEnqueueReadBuffer(buf->queue, buf->mem, blocking, offset, size, source_ptr, 0, NULL, NULL);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error reading buffer: \"%s\"",  _irt_error_string(err_code));
}

inline void* irt_ocl_map_buffer(irt_ocl_buffer* buf, cl_bool blocking, cl_map_flags map_flags, size_t size) {
	cl_int err_code;
	void* ptr = clEnqueueMapBuffer(buf->queue, buf->mem, blocking, map_flags, 0, size, 0, NULL, NULL, &err_code); 
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error mapping buffer: \"%s\"",  _irt_error_string(err_code));
	return ptr;
}

inline void irt_ocl_unmap_buffer(irt_ocl_buffer* buf, void* mapped_ptr) {
	cl_int err_code = clEnqueueUnmapMemObject(buf->queue, buf->mem, mapped_ptr, 0, NULL, NULL);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error unmapping buffer: \"%s\"",  _irt_error_string(err_code));
}

inline void irt_ocl_copy_buffer(irt_ocl_buffer* src_buf, irt_ocl_buffer* dest_buf, size_t size) {
	IRT_ASSERT(src_buf->queue  == dest_buf->queue, IRT_ERR_OCL, "Error: source and destination buffer have a different queue");
	cl_int err_code = clEnqueueCopyBuffer(src_buf->queue, src_buf->mem, dest_buf->mem, 0, 0, size, 0, NULL, NULL);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error copying buffer: \"%s\"",  _irt_error_string(err_code));
}

/*
 * =====================================================================================
 *  OpenCL Print & Profile Functions
 * =====================================================================================
 */

void irt_ocl_print_device_infos(irt_ocl_device* dev) {
		IRT_INFO("name:                   %s\n", dev->name);
		IRT_INFO("type:                   %s\n", _irt_cl_get_device_type_string(dev->type));
		IRT_INFO("vendor:                 %s\n", dev->vendor);
		IRT_INFO("version:                %s\n", dev->version);
		IRT_INFO("driver version:         %s\n", dev->driver_version);
		IRT_INFO("profile:                %s\n", dev->profile);
		IRT_INFO("\n");
		IRT_INFO("max compute-units:      %u\n", dev->max_compute_units);
		IRT_INFO("max clock frequency:    %u\n", dev->max_clock_frequency);
		IRT_INFO("max work-item dims:     %u\n", dev->max_work_item_dimensions);
		IRT_INFO("max work-item sizes:    [");
		for (cl_uint i = 0; i < dev->max_work_item_dimensions; i++) {
				IRT_INFO("%zu", dev->max_work_item_sizes[i]);
				if(i < dev->max_work_item_dimensions - 1) IRT_INFO(", ");
		}
		IRT_INFO("]\n");
		IRT_INFO("max work-group size:    %zu\n", dev->max_work_group_size);

		IRT_INFO("image support:          %s", dev->image_support ? "yes\n" : "no\n");
		IRT_INFO("single fp config:      ");
		if(dev->single_fp_config & CL_FP_DENORM) IRT_INFO(" denorm");
		if(dev->single_fp_config & CL_FP_INF_NAN) IRT_INFO(" inf_nan");
		if(dev->single_fp_config & CL_FP_ROUND_TO_NEAREST) IRT_INFO(" round_to_nearest");
		if(dev->single_fp_config & CL_FP_ROUND_TO_ZERO) IRT_INFO(" round_to_zero");
		if(dev->single_fp_config & CL_FP_ROUND_TO_INF) IRT_INFO(" round_to_inf");
		if(dev->single_fp_config & CL_FP_FMA) IRT_INFO(" fma");
		IRT_INFO("\n");
		IRT_INFO("endian little:          %s", dev->endian_little ? "yes\n" : "no\n");
		IRT_INFO("extensions:             %s\n", dev->extensions);
		IRT_INFO("\n");
		IRT_INFO("global mem size:        %lu MB\n", dev->mem_size / 1024 /1024);
		IRT_INFO("max mem alloc size:     %lu MB\n", dev->max_buffer_size / 1024 /1024);

		if(dev->mem_cache_type == CL_NONE) {
				IRT_INFO("global mem cache:      none\n");
		} else {
				IRT_INFO("global mem cache:     %s", dev->mem_cache_type == CL_READ_ONLY_CACHE ? "read only\n" : "write_only\n");
				IRT_INFO("global mem cline size:  %lu byte\n", dev->global_mem_cacheline_size);
				IRT_INFO("global mem cache size:  %lu kB\n", dev->global_mem_cache_size / 1024);
		}
		IRT_INFO("\n");
		IRT_INFO("max const buffer size:  %lu kB\n", dev->max_constant_buffer_size / 1024);
		IRT_INFO("dedicated local mem:    %s", dev->local_mem_type == CL_LOCAL ? "yes\n" : "no\n");
		IRT_INFO("local mem size:         %lu kB\n", dev->local_mem_size / 1024);
}


void irt_ocl_print_device_short_info(irt_ocl_device* dev) {
		IRT_INFO("%s: %s | %s | %s\n", _irt_cl_get_device_type_string(dev->type), dev->name, dev->vendor, dev->version);
}


inline float irt_ocl_profile_event(cl_event event, cl_profiling_info event_start, cl_profiling_info event_end, irt_ocl_profile_event_flag time_flag) {
	return irt_ocl_profile_events(event, event_start, event, event_end, time_flag);
}

float irt_ocl_profile_events(cl_event event_one, cl_profiling_info event_one_command, cl_event event_two, cl_profiling_info event_two_command, irt_ocl_profile_event_flag time_flag) {
	cl_ulong event_one_start, event_two_end;
	cl_int err_code;
	err_code = clGetEventProfilingInfo(event_two, event_two_command, sizeof(cl_ulong), &event_two_end, NULL);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting profiling info: \"%s\"",  _irt_error_string(err_code));
	err_code = clGetEventProfilingInfo(event_one, event_one_command, sizeof(cl_ulong), &event_one_start, NULL);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting profiling info: \"%s\"", _irt_error_string(err_code));
	float time = 0.0;
	switch(time_flag) {
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

/*
 * =====================================================================================
 *  OpenCL Kernel Functions
 * =====================================================================================
 */

irt_ocl_kernel*  irt_ocl_create_kernel(irt_ocl_device* dev, const char* file_name, const char* kernel_name, const char* build_options, irt_ocl_create_kernel_flag flag) {
	cl_program program = NULL;
	char* binary_name = NULL;
	size_t filesize = 0;
	cl_int err_code;
	if (flag != IRT_OCL_STRING) {
		// create the binary name
		size_t len, binary_name_size, cl_param_size;
		char* device_name;
		err_code = clGetDeviceInfo(dev->device, CL_DEVICE_NAME, 0, NULL, &cl_param_size);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting device name: \"%s\"", _irt_error_string(err_code));
		device_name = alloca (cl_param_size);
		err_code = clGetDeviceInfo(dev->device, CL_DEVICE_NAME, cl_param_size, device_name, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting device name: \"%s\"", _irt_error_string(err_code));

		const char* file_ptr = strrchr(file_name, '/'); // remove the path from the file_name
		if (file_ptr)
			file_ptr++; // remove the last '/'
		else
			file_ptr = file_name;

		len = strlen(file_ptr);
		IRT_ASSERT(len >= 0, IRT_ERR_OCL, "Error size of file_name");

		char* converted_file_name = alloca(len + 1); // +1 for the \0 in the end
		strcpy (converted_file_name, file_ptr);
		for (int i = 0; i < len; ++i) {
			if (!isalnum ((int)converted_file_name[i])) {
				converted_file_name[i] = '_';
			}
		}
		binary_name_size = len;

		len = strlen(device_name);
		char* converted_device_name = alloca(len + 1); // +1 for the \0 in the end
		strcpy (converted_device_name, device_name);
		for (int i = 0; i < len; ++i) {
			if (!isalnum ((int)converted_device_name[i])) {
				converted_device_name[i] = '_';
			}
		}
		binary_name_size += len;
		binary_name_size += 6; // file_name.device_name.bin\0

		binary_name = alloca (binary_name_size);

		sprintf (binary_name, "%s.%s.bin", converted_file_name, converted_device_name);
		//printf("%s\n", binary_name);
	}
	if ((flag == IRT_OCL_SOURCE) || (flag == IRT_OCL_STRING)) {
		if (flag == IRT_OCL_SOURCE) {
			char* program_source = _irt_load_program_source(file_name, &filesize);
			IRT_ASSERT(program_source != NULL, IRT_ERR_OCL, "Error loading kernel program source");
			program = clCreateProgramWithSource (dev->context, 1, (const char **) &program_source, NULL, &err_code);
			IRT_ASSERT(err_code == CL_SUCCESS && program != NULL, IRT_ERR_OCL, "Error creating compute program: \"%s\"", _irt_error_string(err_code));
			free(program_source);
		} else { // case of IRT_OCL_STRING we use the file_name to pass the string
			program = clCreateProgramWithSource (dev->context, 1, (const char **) &file_name, NULL, &err_code);
			IRT_ASSERT(err_code == CL_SUCCESS && program != NULL, IRT_ERR_OCL, "Error creating compute program: \"%s\"", _irt_error_string(err_code));
		}

		err_code = clBuildProgram(program, 1, &(dev->device), build_options, NULL, NULL);

		// If there are build errors, print them to the screen
		if(err_code != CL_SUCCESS) {
			IRT_INFO("Kernel program failed to build.\n");
			char *buildLog;
			size_t buildLogSize;
			err_code = clGetProgramBuildInfo(program,dev->device, CL_PROGRAM_BUILD_LOG, 0, NULL, &buildLogSize);
			IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting program build info: \"%s\"", _irt_error_string(err_code));
			buildLog = (char*)malloc(buildLogSize);
			err_code = clGetProgramBuildInfo(program,dev->device, CL_PROGRAM_BUILD_LOG, buildLogSize, buildLog, NULL);
			IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting program build info: \"%s\"", _irt_error_string(err_code));
			buildLog[buildLogSize-1] = '\0';
			IRT_INFO("Device Build Log:\n%s\n", buildLog);
			free(buildLog);
		}
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error building compute program: \"%s\"", _irt_error_string(err_code));
	}

	if (flag == IRT_OCL_SOURCE) _irt_save_program_binary(program, binary_name); // We don't save in case of IRT_OCL_STRING

	if (flag == IRT_OCL_BINARY) {
		cl_int binary_status;
		unsigned char* program_s = (unsigned char*) _irt_load_program_source(binary_name, &filesize);
		program = clCreateProgramWithBinary (dev->context, 1, &(dev->device), &filesize, (const unsigned char **) &program_s, &binary_status, &err_code);
		free(program_s);
		IRT_ASSERT(err_code == CL_SUCCESS && binary_status == CL_SUCCESS && program != NULL, IRT_ERR_OCL, "Error creating compute program: \"%s\"", _irt_error_string(err_code));
		err_code = clBuildProgram(program, 1, &(dev->device), build_options, NULL, NULL);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error building compute program: \"%s\"", _irt_error_string(err_code));
	}

	irt_ocl_kernel* kernel = NULL;
	if ((kernel_name != NULL) && (*kernel_name != 0)) {
		kernel = (irt_ocl_kernel*)malloc(sizeof(irt_ocl_kernel));
		kernel->kernel = clCreateKernel(program, kernel_name, &err_code);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error creating kernel: \"%s\"", _irt_error_string(err_code));
		kernel->type = IRT_OCL_TASK;
		kernel->work_dim = 0;
		kernel->global_work_size = 0;
		kernel->local_work_size = 0;
		kernel->dev = dev;
	}
	err_code = clReleaseProgram(program);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error releasing compute program: \"%s\"", _irt_error_string(err_code));
	return kernel; // I want only to compile the program, with kernel name = ""
}

inline void irt_ocl_release_kernel(irt_ocl_kernel* kernel) {
	cl_int err_code = clReleaseKernel(kernel->kernel);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error releasing kernel");
	free(kernel);
}

inline void irt_ocl_set_kernel_ndrange(irt_ocl_kernel* kernel, cl_uint work_dim, size_t* global_work_size, size_t* local_work_size) {
	kernel->type = IRT_OCL_NDRANGE;
	kernel->work_dim = work_dim;
	kernel->global_work_size = global_work_size;
	kernel->local_work_size = local_work_size;
}

void irt_ocl_run_kernel(irt_ocl_kernel* kernel, cl_uint num_args, ...) {
	//loop through the arguments and call clSetKernelArg for each argument
	cl_uint arg_index;
	size_t arg_size;
	const void *arg_val;
	va_list arg_list;
	cl_int err_code;
	va_start (arg_list, num_args);
	for (uint i = 0; i < num_args; i++) {
		arg_index = i;
		arg_size = va_arg (arg_list, size_t);
		arg_val = va_arg (arg_list, void *);
		if (arg_size == 0){
			irt_ocl_buffer* buf = (irt_ocl_buffer*) arg_val;
			arg_size = sizeof(cl_mem);
			arg_val = (void*) &(buf->mem);
		}
		err_code = clSetKernelArg(kernel->kernel, arg_index, arg_size, arg_val);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error setting kernel arguments: \"%s\"", _irt_error_string(err_code));    
	}
  	va_end (arg_list);

	if (kernel->type == IRT_OCL_NDRANGE) {
		err_code = clEnqueueNDRangeKernel((kernel->dev)->queue, 
						kernel->kernel, 
						kernel->work_dim,
						NULL, 
						kernel->global_work_size,
						kernel->local_work_size,
						0, NULL, NULL);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error enqueuing NDRange Kernel: \"%s\"", _irt_error_string(err_code));
	}
	else if (kernel->type == IRT_OCL_TASK) {
		err_code = clEnqueueTask((kernel->dev)->queue, kernel->kernel, 0, NULL, NULL);
		 IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error enqueuing Task Kernel: \"%s\"", _irt_error_string(err_code));
	}
	else IRT_ASSERT(false, IRT_ERR_OCL, "Kernel Type Not Valid");
}


/*
 * =====================================================================================
 *  Insieme Runtime OpenCL Functions
 * =====================================================================================
 */

void irt_ocl_rt_create_kernel(irt_ocl_device* dev, irt_ocl_kernel* kernel, const char* file_name, const char* kernel_name, const char* build_options, irt_ocl_create_kernel_flag flag) {
	cl_program program = NULL;
	cl_int err_code;

	if (flag == IRT_OCL_STRING) {
		program = clCreateProgramWithSource (dev->context, 1, (const char **) &file_name, NULL, &err_code);
		IRT_ASSERT(err_code == CL_SUCCESS && program != NULL, IRT_ERR_OCL, "Error creating compute program: \"%s\"", _irt_error_string(err_code));

		err_code = clBuildProgram(program, 1, &(dev->device), build_options, NULL, NULL);

		// If there are build errors, print them to the screen
		if(err_code != CL_SUCCESS) {
			IRT_INFO("Kernel program failed to build.\n");
			char *buildLog;
			size_t buildLogSize;
			err_code = clGetProgramBuildInfo(program,dev->device, CL_PROGRAM_BUILD_LOG, 0, NULL, &buildLogSize);
			IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting program build info: \"%s\"", _irt_error_string(err_code));
			buildLog = (char*)malloc(buildLogSize);
			err_code = clGetProgramBuildInfo(program,dev->device, CL_PROGRAM_BUILD_LOG, buildLogSize, buildLog, NULL);
			IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting program build info: \"%s\"", _irt_error_string(err_code));
			buildLog[buildLogSize-1] = '\0';
			IRT_INFO("Device Build Log:\n%s\n", buildLog);
			free(buildLog);
		}
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error building compute program: \"%s\"", _irt_error_string(err_code));
	}

	if ((kernel_name != NULL) && (*kernel_name != 0)) {
		clCreateKernel(program, kernel_name, &err_code);
		kernel->kernel = clCreateKernel(program, kernel_name, &err_code);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error creating kernel: \"%s\"", _irt_error_string(err_code));
		kernel->type = IRT_OCL_TASK;
		kernel->work_dim = 0;
		kernel->global_work_size = 0;
		kernel->local_work_size = 0;
		kernel->dev = dev;
	}
	err_code = clReleaseProgram(program);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error releasing compute program: \"%s\"", _irt_error_string(err_code));
}

void irt_ocl_rt_create_all_kernels(irt_context* context, irt_ocl_kernel_code* g_kernel_code_table, cl_uint num_kernels) {
	cl_uint num_devices = irt_ocl_get_num_devices();

	irt_ocl_kernel* tmp = (irt_ocl_kernel*)malloc(sizeof(irt_ocl_kernel) * num_devices * num_kernels);
	context->kernel_binary_table = (irt_ocl_kernel**)malloc(sizeof(irt_ocl_kernel*) * num_devices);

	for (cl_uint i = 0; i < num_devices; ++i) {
		irt_ocl_device* dev = irt_ocl_get_device(i);
		IRT_INFO("Compiling OpenCL program in \"%s\"\n", dev->name);

		context->kernel_binary_table[i] = &(tmp[i*num_kernels]);
		for (cl_uint j = 0; j < num_kernels; ++j) {
			irt_ocl_rt_create_kernel(dev, &(context->kernel_binary_table[i][j]), g_kernel_code_table[j].code, g_kernel_code_table[j].kernel_name, "", IRT_OCL_STRING);
		}
	}

	context->kernel_code_table = g_kernel_code_table;
}

void irt_ocl_rt_release_all_kernels(irt_context* context, cl_uint g_kernel_code_table_size) { // FIXME... is not called in the runtime
	printf("Releasing kernels\n");
	cl_uint num_devices = irt_ocl_get_num_devices();
	for (cl_uint i = 0; i < num_devices; ++i) {
		for (cl_uint j = 0; j < g_kernel_code_table_size; ++j) {
			printf("Releasing kernel device: %d  kernel %d\n", i, j);
			irt_ocl_release_kernel(&(context->kernel_binary_table[i][j]));
		}
	}
	free(context->kernel_binary_table[0]);
	free(context->kernel_binary_table);
	context->kernel_binary_table = NULL;
}

irt_ocl_buffer* irt_ocl_rt_create_buffer(cl_mem_flags flags, size_t size){
	irt_ocl_device* dev = irt_ocl_get_device(0);
	return irt_ocl_create_buffer(dev, flags, size);
}

void irt_ocl_rt_run_kernel(cl_uint kernel_id, cl_uint work_dim, size_t* global_work_size, size_t* local_work_size, cl_uint num_args, ...){
	irt_ocl_kernel* kernel = &irt_context_get_current()->kernel_binary_table[0][kernel_id];
	IRT_INFO("Running Opencl Kernel in \"%s\"\n", kernel->dev->name);

	irt_ocl_set_kernel_ndrange(kernel, work_dim, global_work_size, local_work_size);

	// FIXME: eliminate duplicated code
	// loop through the arguments and call clSetKernelArg for each argument
	cl_uint arg_index;
	size_t arg_size;
	const void *arg_val;
	va_list arg_list;
	cl_int err_code;
	va_start (arg_list, num_args);
	for (uint i = 0; i < num_args; i++) {
		arg_index = i;
		arg_size = va_arg (arg_list, size_t);
		arg_val = va_arg (arg_list, void *);
		if (arg_size == 0){
			irt_ocl_buffer* buf = (irt_ocl_buffer*) arg_val;
			arg_size = sizeof(cl_mem);
			arg_val = (void*) &(buf->mem);
		}
		err_code = clSetKernelArg(kernel->kernel, arg_index, arg_size, arg_val);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error setting kernel arguments: \"%s\"", _irt_error_string(err_code));
	}
	va_end (arg_list);

	if (kernel->type == IRT_OCL_NDRANGE) {
		err_code = clEnqueueNDRangeKernel((kernel->dev)->queue,
						kernel->kernel,
						kernel->work_dim,
						NULL,
						kernel->global_work_size,
						kernel->local_work_size,
						0, NULL, NULL);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error enqueuing NDRange Kernel: \"%s\"", _irt_error_string(err_code));
	}
	else if (kernel->type == IRT_OCL_TASK) {
		err_code = clEnqueueTask((kernel->dev)->queue, kernel->kernel, 0, NULL, NULL);
		 IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error enqueuing Task Kernel: \"%s\"", _irt_error_string(err_code));
	}
	else IRT_ASSERT(false, IRT_ERR_OCL, "Kernel Type Not Valid");
}


// ----------------------------------------------------------- OpenCL Internal Functions --------------------------------------------------

/*
 * =====================================================================================
 *  OpenCL Internal Platform Functions
 * =====================================================================================
 */

inline static cl_uint _irt_cl_get_num_platforms() {
	cl_uint cl_num_platforms;
	if (clGetPlatformIDs(0, NULL, &cl_num_platforms) != CL_SUCCESS) return 0;
	return cl_num_platforms;
}

inline static void _irt_cl_get_platforms(cl_uint num_platforms, cl_platform_id* platforms) {
	cl_int err_code = clGetPlatformIDs(num_platforms, platforms, NULL);
	IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting platforms: \"%s\"", _irt_error_string(err_code));
}

/*
 * =====================================================================================
 *  OpenCL Internal Device Functions
 * =====================================================================================
 */

cl_uint _irt_cl_get_num_devices(cl_platform_id* platform, cl_device_type device_type) {
	cl_uint cl_num_devices;
	if (clGetDeviceIDs(*platform, device_type, 0, NULL, &cl_num_devices) != CL_SUCCESS) return 0;
	return cl_num_devices;
}

inline static void _irt_cl_get_devices(cl_platform_id* platform, cl_device_type device_type, cl_uint num_devices, cl_device_id* devices) {
	cl_int err_code = clGetDeviceIDs(*platform, device_type, num_devices, devices, NULL);
	IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting devices: \"%s\"", _irt_error_string(err_code));
}


/*
* =====================================================================================
*  OpenCL Internal Load, Save, Error Functions
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
	IRT_ASSERT(fclose (fp) == 0, IRT_ERR_OCL, "Error closing the file");
	return source;
}

static void _irt_save_program_binary (cl_program program, const char* binary_filename) {
	IRT_ASSERT(binary_filename != NULL && program != NULL, IRT_ERR_OCL, "Error input parameters");
	size_t size_ret;
	cl_int err_code;
	err_code = clGetProgramInfo (program, CL_PROGRAM_BINARY_SIZES, 0, NULL, &size_ret);
	IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting program info: \"%s\"", _irt_error_string(err_code));
	size_t* binary_size = (size_t *) alloca (size_ret);
	IRT_ASSERT(binary_size != NULL, IRT_ERR_OCL, "Error allocating binary_size");
	err_code = clGetProgramInfo (program, CL_PROGRAM_BINARY_SIZES, size_ret, binary_size, NULL);
	IRT_ASSERT(err_code == CL_SUCCESS,  IRT_ERR_OCL, "Error getting program info: \"%s\"", _irt_error_string(err_code));
		unsigned char* binary = (unsigned char *) alloca (sizeof (unsigned char) * (*binary_size));
	IRT_ASSERT(binary != NULL, IRT_ERR_OCL, "Error allocating binary");

	// get the binary
	err_code = clGetProgramInfo (program, CL_PROGRAM_BINARIES, sizeof (unsigned char *), &binary, NULL);
	IRT_ASSERT(err_code == CL_SUCCESS,  IRT_ERR_OCL, "Error getting program info: \"%s\"", _irt_error_string(err_code));

	FILE *fp = fopen (binary_filename, "w");
	IRT_ASSERT(fp != NULL, IRT_ERR_OCL, "Error opening binary file");
	IRT_ASSERT(fwrite (binary, 1, *binary_size, fp) ==  (size_t) *binary_size, IRT_ERR_OCL, "Error writing file");
	IRT_ASSERT(fclose (fp) == 0, IRT_ERR_OCL, "Error closing the file");
}

static const char* _irt_error_string (cl_int errcode) {
	switch (errcode) {
		case CL_SUCCESS:
			return "SUCCESS";
		case CL_DEVICE_NOT_FOUND:
			return "Device not found";
		case CL_DEVICE_NOT_AVAILABLE:
			return "Device not available";
		case CL_COMPILER_NOT_AVAILABLE:
			return "Compiler not available";
		case CL_MEM_OBJECT_ALLOCATION_FAILURE:
			return "Memory Object allocation failure";
		case CL_OUT_OF_RESOURCES:
			return "Out of resources";
		case CL_OUT_OF_HOST_MEMORY:
			return "Out of host memory";
		case CL_PROFILING_INFO_NOT_AVAILABLE:
			return "Profiling info not available";
		case CL_MEM_COPY_OVERLAP:
			return "Mem copy overlap";
		case CL_IMAGE_FORMAT_MISMATCH:
			return "Image format mismatch";
		case CL_IMAGE_FORMAT_NOT_SUPPORTED:
			return "Image format not supported";
		case CL_BUILD_PROGRAM_FAILURE:
			return "Build program failure";
		case CL_MAP_FAILURE:
			return "Map failure";
		case CL_MISALIGNED_SUB_BUFFER_OFFSET:
			return "Misaligned sub buffer offset";
		case CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST:
			return "Status Error for events in wait list";
		case CL_INVALID_VALUE:
			return "Invalid value";
		case CL_INVALID_DEVICE_TYPE:
			return "Invalid device type";
		case CL_INVALID_PLATFORM:
			return "Invalid platform";
		case CL_INVALID_DEVICE:
			return "Invalid device";
		case CL_INVALID_CONTEXT:
			return "Invalid context";
		case CL_INVALID_QUEUE_PROPERTIES:
			return "Invalid queue properties";
		case CL_INVALID_COMMAND_QUEUE:
			return "Invalid command queue";
		case CL_INVALID_HOST_PTR:
			return "Invalid host pointer";
		case CL_INVALID_MEM_OBJECT:
			return "Invalid memory object";
		case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:
			return "Invalid image format descriptor";
		case CL_INVALID_IMAGE_SIZE:
			return "Invalid image size";
		case CL_INVALID_SAMPLER:
			return "Invalid sampler";
		case CL_INVALID_BINARY:
			return "Invalid Binary";
		case CL_INVALID_BUILD_OPTIONS:
			return "Invalid build options";
		case CL_INVALID_PROGRAM:
			return "Invalid program";
		case CL_INVALID_PROGRAM_EXECUTABLE:
			return "Invalid program executable";
		case CL_INVALID_KERNEL_NAME:
			return "Invalid kernel name";
		case CL_INVALID_KERNEL_DEFINITION:
			return "Invalid kernel definition";
		case CL_INVALID_KERNEL:
			return "Invalid kernel";
		case CL_INVALID_ARG_INDEX:
			return "Invalid arg index";
		case CL_INVALID_ARG_VALUE:
			return "Invalid arg value";
		case CL_INVALID_ARG_SIZE:
			return "Invalid arg size";
		case CL_INVALID_KERNEL_ARGS:
			return "Invalid kernel args";
		case CL_INVALID_WORK_DIMENSION:
			return "Invalid work dimension";
		case CL_INVALID_WORK_GROUP_SIZE:
			return "Invalid work group size";
		case CL_INVALID_WORK_ITEM_SIZE:
			return "Invalid work item size";
		case CL_INVALID_GLOBAL_OFFSET:
			return "Invalid global offset";
		case CL_INVALID_EVENT_WAIT_LIST:
			return "Invalid wait list";
		case CL_INVALID_EVENT:
			return "Invalid event";
		case CL_INVALID_OPERATION:
			return "Invalid operation";
		case CL_INVALID_GL_OBJECT:
			return "Invalid gl object";
		case CL_INVALID_BUFFER_SIZE:
			return "Invalid buffer size";
		case CL_INVALID_MIP_LEVEL:
			return "Invalid mip level";
		case CL_INVALID_GLOBAL_WORK_SIZE:
			return "Invalid global work size";
		case CL_INVALID_PROPERTY:
			return "Invalid property";
		default:
			return "Unknown";
	};
}

/*
 * =====================================================================================
 *  OpenCL Internal Device Info Functions
 * =====================================================================================
 */

const char* _irt_cl_get_device_type_string(cl_device_type type) {
		switch(type){
				case CL_DEVICE_TYPE_CPU: return "CPU"; break;
				case CL_DEVICE_TYPE_GPU: return "GPU"; break;
				case CL_DEVICE_TYPE_ACCELERATOR: return "ACL"; break;
				default: return "Default";
		}
}

char* _irt_cl_get_name(cl_device_id* device) {
		size_t size = 0;
		clGetDeviceInfo(*device, CL_DEVICE_NAME, size, NULL, &size);
		char* retval = (char*) malloc(sizeof(char) * size);
		cl_int err_code =  clGetDeviceInfo(*device, CL_DEVICE_NAME, size, retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL,"Error getting \"device name\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_device_type _irt_cl_get_type(cl_device_id* device) {
		cl_device_type retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_TYPE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device type\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

char* _irt_cl_get_vendor(cl_device_id* device) {
		size_t size = 0;
		clGetDeviceInfo(*device, CL_DEVICE_VENDOR, size, NULL, &size);
		char* retval = (char*) malloc(sizeof(char) * size);
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_VENDOR, size, retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device vendor\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

char* _irt_cl_get_version(cl_device_id* device) {
		size_t size = 0;
		clGetDeviceInfo(*device, CL_DEVICE_VERSION, size, NULL, &size);
		char* retval = (char*) malloc(sizeof(char) * size);
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_VERSION, size, retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device version\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

char* _irt_cl_get_driver_version(cl_device_id* device) {
		size_t size = 0;
		clGetDeviceInfo(*device, CL_DRIVER_VERSION, size, NULL, &size);
		char* retval = (char*) malloc(sizeof(char) * size);
		cl_int err_code = clGetDeviceInfo(*device, CL_DRIVER_VERSION, size, retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device driver version\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

char* _irt_cl_get_profile(cl_device_id* device) {
		size_t size = 0;
		clGetDeviceInfo(*device, CL_DEVICE_PROFILE, size, NULL, &size);
		char* retval = (char*) malloc(sizeof(char) * size);
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_PROFILE, size, retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device profile\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_uint _irt_cl_get_max_compute_units(cl_device_id* device) {
		cl_uint retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device max compute units\" info: \"%s\"", _irt_error_string(err_code));
		return retval;  ;
}

cl_uint _irt_cl_get_max_clock_frequency(cl_device_id* device) {
		cl_uint retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_CLOCK_FREQUENCY, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device max clock frequency\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_uint _irt_cl_get_max_work_item_dimensions(cl_device_id* device) {
		cl_uint retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device max work item dimensions\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

size_t* _irt_cl_get_max_work_item_sizes(cl_device_id* device) {
		cl_uint size = _irt_cl_get_max_work_item_dimensions(device);
		size_t *retval = (size_t*) malloc(size * sizeof(size_t));
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_WORK_ITEM_SIZES, sizeof(size_t) * size, retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device max work item sizes\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

size_t _irt_cl_get_max_work_group_size(cl_device_id* device) {
		size_t retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device max work group size\" info: \"%s\"", _irt_error_string(err_code));
		return retval;  ;
}

cl_bool _irt_cl_has_image_support(cl_device_id* device) {
		cl_bool retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_IMAGE_SUPPORT, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device image support\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_device_fp_config _irt_cl_get_single_fp_config(cl_device_id* device) {
		cl_device_fp_config retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_SINGLE_FP_CONFIG, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device single floating point configuration\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_bool _irt_cl_is_endian_little(cl_device_id* device)  {
		cl_bool retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_ENDIAN_LITTLE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device endian little\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

char* _irt_cl_get_extensions(cl_device_id* device) {
		size_t size = 0;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_EXTENSIONS, size, NULL, &size);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device extensions\" info: \"%s\"", _irt_error_string(err_code));
		char* retval = (char*) malloc(sizeof(char) * size);
		err_code = clGetDeviceInfo(*device, CL_DEVICE_EXTENSIONS, size, retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device extensions\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_ulong _irt_cl_get_global_mem_size(cl_device_id* device) {
		cl_ulong retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device global memory size\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_ulong _irt_cl_get_max_mem_alloc_size(cl_device_id* device) {
		cl_ulong retval;
		cl_int err_code =  clGetDeviceInfo(*device, CL_DEVICE_MAX_MEM_ALLOC_SIZE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device max memory alloc size\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_device_mem_cache_type _irt_cl_get_global_mem_cache_type(cl_device_id* device) {
		cl_device_mem_cache_type retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_GLOBAL_MEM_CACHE_TYPE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device global mem cache type\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_uint _irt_cl_get_global_mem_cacheline_size(cl_device_id* device) {
		cl_uint retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device global mem cache line size\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_ulong _irt_cl_get_global_mem_cache_size(cl_device_id* device) {
		cl_ulong retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_GLOBAL_MEM_CACHE_SIZE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device global mem cache size\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_ulong _irt_cl_get_max_constant_buffer_size(cl_device_id* device) {
		cl_ulong retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device max constant buffer size\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_device_local_mem_type _irt_cl_get_local_mem_type(cl_device_id* device) {
		cl_device_local_mem_type retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_LOCAL_MEM_TYPE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device local memory type\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}

cl_ulong _irt_cl_get_local_mem_size(cl_device_id* device) {
		cl_ulong retval;
		cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_LOCAL_MEM_SIZE, sizeof(retval), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting \"device local memory size\" info: \"%s\"", _irt_error_string(err_code));
		return retval;
}
