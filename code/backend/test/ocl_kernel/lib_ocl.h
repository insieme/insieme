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

#include "CL/cl.h"
#include "lib_dep.h"

#define DEVICE_TYPE (CL_DEVICE_TYPE_GPU | CL_DEVICE_TYPE_ACCELERATOR | CL_DEVICE_TYPE_CPU)

typedef struct _irt_ocl_buffer {
	cl_mem mem;
	bool used;
	size_t size;
	struct _irt_ocl_buffer* next;
	cl_command_queue queue; // derive the queue directly from the buffer
} irt_ocl_buffer;

typedef struct _irt_ocl_device {
	// generic info
	cl_device_id device;
	cl_context context;
	cl_command_queue queue;

	// buffers info
	cl_ulong mem_size; // memory of the device
	cl_ulong mem_available; // memory still available, reduced after each buffer allocation
	cl_ulong max_buffer_size; // max size of a buffer
	
	irt_ocl_buffer* buffer;
} irt_ocl_device;

typedef enum {IRT_OCL_NDRANGE, IRT_OCL_TASK} irt_ocl_kernel_type;
typedef enum {IRT_OCL_SOURCE, IRT_OCL_BINARY, IRT_OCL_STRING, IRT_OCL_NO_CACHE} irt_ocl_create_kernel_flag;

typedef struct _irt_ocl_kernel {
	cl_kernel kernel;
	irt_ocl_kernel_type type;
	cl_uint work_dim;
	size_t* global_work_size;
	size_t* local_work_size;

	irt_ocl_device* dev;
} irt_ocl_kernel;


irt_ocl_device* devices;
cl_uint num_devices;

void irt_ocl_init_devices();
cl_uint irt_ocl_get_num_devices();
irt_ocl_device* irt_ocl_get_device(cl_uint id);
void irt_ocl_release_devices();

irt_ocl_buffer* irt_ocl_create_buffer(irt_ocl_device* dev, cl_mem_flags flags, size_t size);
void irt_ocl_read_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t size, void* source_ptr);
void irt_ocl_write_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t size, const void* source_ptr);
void irt_ocl_copy_buffer(irt_ocl_buffer* src_buf, irt_ocl_buffer* dest_buf, size_t size);
void* irt_ocl_map_buffer(irt_ocl_buffer* buf, cl_bool blocking, cl_map_flags map_flags, size_t size);
void irt_ocl_unmap_buffer(irt_ocl_buffer* buf, void* mapped_ptr);
void irt_ocl_release_buffer(irt_ocl_buffer* buf);

irt_ocl_kernel* irt_ocl_create_kernel(irt_ocl_device* dev, const char* file_name, const char* kernel_name, const char* build_options, irt_ocl_create_kernel_flag flag);
void irt_ocl_set_kernel_ndrange(irt_ocl_kernel* kernel, cl_uint work_dim, size_t* global_work_size, size_t* local_work_size);
void irt_ocl_run_kernel(irt_ocl_kernel* kernel, cl_uint num_args, ...);
void irt_ocl_release_kernel(irt_ocl_kernel* kernel);

void irt_ocl_print_device_info(irt_ocl_device* dev, char* prefix, cl_device_info param_name, char* suffix);
void irt_ocl_print_device_infos(irt_ocl_device* dev);

typedef enum {IRT_OCL_SEC, IRT_OCL_MILLI, IRT_OCL_NANO} irt_ocl_profile_event_flag;
float irt_ocl_profile_event(cl_event event, cl_profiling_info event_start, cl_profiling_info event_end, irt_ocl_profile_event_flag time_flag);
float irt_ocl_profile_events(cl_event event_one, cl_profiling_info event_one_command, cl_event event_two, cl_profiling_info event_two_command, irt_ocl_profile_event_flag time_flag);
