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
#include "impl/error_handling.impl.h"
#include "abstraction/threads.h"
#define IRT_OCL_INSTR 0
//#define IRT_OCL_DEBUG 0

#define DEVICE_TYPE (CL_DEVICE_TYPE_CPU)

struct _irt_ocl_buffer;
struct _irt_ocl_device;
typedef struct _irt_ocl_buffer irt_ocl_buffer;
typedef struct _irt_ocl_device irt_ocl_device;

struct _irt_ocl_buffer {
	cl_mem mem;
	bool used;
	size_t size;
	struct _irt_ocl_buffer* next;
	irt_ocl_device* dev; // derive the device directly from the buffer
};


struct _irt_ocl_device {
	// internal info
	cl_device_id device;
	cl_context context;
	cl_command_queue queue;

	// buffers info
	cl_ulong mem_size; // memory of the device
	cl_ulong mem_available; // memory still available, reduced after each buffer allocation
	cl_ulong max_buffer_size; // max size of a buffer
	irt_spinlock buffer_lock;
	irt_ocl_buffer* buffer;

	// device info
	char *name;
	cl_device_type type;
	char *vendor;
	char *version;
	char *driver_version;
	char *profile;

	cl_uint max_compute_units;
	cl_uint max_clock_frequency;
	cl_uint max_work_item_dimensions;
	size_t* max_work_item_sizes;
	size_t max_work_group_size;


	cl_bool image_support;
	cl_device_fp_config single_fp_config;
	cl_bool endian_little;
	char *extensions;

	cl_device_mem_cache_type mem_cache_type;
	cl_ulong global_mem_cacheline_size;
	cl_ulong global_mem_cache_size;

	cl_ulong max_constant_buffer_size;

	cl_device_local_mem_type local_mem_type;
	cl_ulong local_mem_size;
};


typedef enum {IRT_OCL_NDRANGE, IRT_OCL_TASK} irt_ocl_kernel_type;
typedef enum {IRT_OCL_SOURCE, IRT_OCL_BINARY, IRT_OCL_STRING, IRT_OCL_NO_CACHE} irt_ocl_create_kernel_flag;

typedef struct _irt_ocl_kernel {
	cl_kernel kernel;
	irt_ocl_kernel_type type;
	cl_uint work_dim;
	size_t* global_work_offset;
	size_t* global_work_size;
	size_t* local_work_size;

	irt_spinlock kernel_lock;
	irt_ocl_device* dev;
} irt_ocl_kernel;

irt_ocl_device* devices;
cl_uint num_devices;
cl_uint* sorted_dev_id;

void irt_ocl_init_devices();
cl_uint irt_ocl_get_num_devices();
irt_ocl_device* irt_ocl_get_device(cl_uint id);
void irt_ocl_release_devices();

irt_ocl_buffer* irt_ocl_create_buffer(irt_ocl_device* dev, cl_mem_flags flags, size_t size);
void irt_ocl_read_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t offset, size_t size, void* source_ptr);
void irt_ocl_write_buffer(irt_ocl_buffer* buf, cl_bool blocking, size_t offset, size_t size, const void* source_ptr);
void irt_ocl_copy_buffer(irt_ocl_buffer* src_buf, irt_ocl_buffer* dest_buf, size_t size);
void* irt_ocl_map_buffer(irt_ocl_buffer* buf, cl_bool blocking, cl_map_flags map_flags, size_t size);
void irt_ocl_unmap_buffer(irt_ocl_buffer* buf, void* mapped_ptr);
void irt_ocl_release_buffer(irt_ocl_buffer* buf);

void irt_ocl_set_kernel_ndrange(irt_ocl_kernel* kernel, cl_uint work_dim, size_t* global_work_offset, size_t* global_work_size, size_t* local_work_size);
void irt_ocl_create_kernel(irt_ocl_device* dev, irt_ocl_kernel* kernel, const char* file_name,
						   const char* kernel_name, const char* build_options, irt_ocl_create_kernel_flag flag);
void irt_ocl_release_kernel(irt_ocl_kernel* kernel);

void irt_ocl_print_device_short_info(irt_ocl_device* dev);
void irt_ocl_print_device_infos(irt_ocl_device* dev);

// ------ Event Functions

#ifdef IRT_OCL_INSTR
#define IRT_OCL_EVENT_TABLE_BLOCKSIZE	5
typedef struct _irt_ocl_event {
	cl_event event;
	uint64 workitem_id;
} irt_ocl_event;

typedef struct _irt_ocl_event_table {
	uint32 size;
	uint32 num_events;
	uint32 blocksize;
	irt_ocl_event* event_array;
} irt_ocl_event_table;

irt_ocl_event_table* irt_ocl_create_event_table();
irt_ocl_event* irt_ocl_get_new_event();
void irt_ocl_release_event_table(irt_ocl_event_table* table);
#endif



// ------- Runtime Functions
typedef struct _irt_ocl_kernel_code {
	const char* kernel_name;
	const char* code;
} irt_ocl_kernel_code;

void irt_ocl_rt_create_all_kernels(irt_context* context, irt_ocl_kernel_code* g_kernel_code_table, cl_uint g_kernel_code_table_size);
void irt_ocl_rt_release_all_kernels(irt_context* context, cl_uint g_kernel_code_table_size);

irt_ocl_buffer* irt_ocl_rt_create_buffer(cl_mem_flags flags, size_t size);

void irt_ocl_rt_run_kernel(cl_uint kernel_id, cl_uint work_dim, size_t* global_work_offset, size_t* global_work_size, size_t* local_work_siz, cl_uint num_args, ...);

void irt_ocl_print_events();
