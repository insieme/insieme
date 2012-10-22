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
#include "impl/error_handling.impl.h"
#include "abstraction/threads.h"

#define IRT_OCL_INIT_DEVS	(1 << 1)
#define IRT_OCL_RELEASE_DEVS	(1 << 2)
#define IRT_OCL_PRINT_DEV_SHORT	(1 << 3)
#define IRT_OCL_PRINT_DEV_INFOS	(1 << 4)

#define IRT_OCL_CREATE_BUFFER	(1 << 5)
#define IRT_OCL_WRITE_BUFFER	(1 << 6)
#define IRT_OCL_READ_BUFFER	(1 << 7)
#define IRT_OCL_RELEASE_BUFFER	(1 << 8)

#define IRT_OCL_CREATE_KERNEL	(1 << 9)
#define IRT_OCL_RUN_KERNEL	(1 << 10)
#define IRT_OCL_RELEASE_KERNEL	(1 << 11)
// NOT USE MORE THAN 14 bits
// other bits from << 15 are used to identify threads

typedef uint64_t		irt_ocl_bitfield;
typedef irt_ocl_bitfield	irt_ocl_device_type;
#define IRT_OCL_CPU		(1 << 1)
#define IRT_OCL_GPU		(1 << 2)
#define IRT_OCL_ACL		(1 << 3)
#define IRT_OCL_ALL		0xFFFFFFFF

#define DEVICE_TYPE (IRT_OCL_CPU | IRT_OCL_GPU | IRT_OCL_ACL)

typedef irt_ocl_bitfield	irt_ocl_mem_flag;
#define IRT_OCL_MEM_READ_WRITE		(1 << 0)
#define IRT_OCL_MEM_WRITE_ONLY		(1 << 1)
#define IRT_OCL_MEM_READ_ONLY		(1 << 2)
#define IRT_OCL_MEM_USE_HOST_PTR	(1 << 3)
#define IRT_OCL_MEM_ALLOC_HOST_PTR	(1 << 4)
#define IRT_OCL_MEM_COPY_HOST_PTR	(1 << 5)
// reserved				(1 << 6)    
#define IRT_OCL_MEM_HOST_WRITE_ONLY	(1 << 7)
#define IRT_OCL_MEM_HOST_READ_ONLY	(1 << 8)
#define IRT_OCL_MEM_HOST_NO_ACCESS	(1 << 9)

typedef irt_ocl_bitfield	irt_ocl_create_kernel_flag;
#define IRT_OCL_SOURCE		(1 << 1)
#define IRT_OCL_BINARY		(1 << 2)
#define IRT_OCL_STRING		(1 << 3)
#define IRT_OCL_NO_CACHE	(1 << 4)

typedef irt_ocl_bitfield	irt_ocl_time_flag;
#define IRT_OCL_SEC		(1 << 1)
#define IRT_OCL_MILLI		(1 << 2)
#define IRT_OCL_NANO		(1 << 3)

typedef uint32_t	irt_ocl_blocking_flag;
#define IRT_OCL_BLOCKING		1
#define IRT_OCL_NON_BLOCKING		0

typedef irt_ocl_bitfield        irt_ocl_kernel_type;
#define IRT_OCL_NDRANGE		(1 << 0)
#define IRT_OCL_TASK		(1 << 1)


#ifdef LOCAL_MODE
//#define IRT_OCL_INSTR 0
#include <CL/cl.h>

struct _irt_ocl_local_buffer;
struct _irt_ocl_local_device;
typedef struct _irt_ocl_local_buffer irt_ocl_local_buffer;
typedef struct _irt_ocl_local_device irt_ocl_local_device;

typedef struct _irt_ocl_local_device {
	// internal info
	cl_device_id cl_dev;
	cl_context context;
	cl_command_queue queue;

	// buffers info
	cl_ulong mem_size; // memory of the device
	cl_ulong mem_available; // memory still available, reduced after each buffer allocation
	cl_ulong max_buffer_size; // max size of a buffer
	irt_spinlock local_buffer_lock; 
	irt_ocl_local_buffer* local_buffer;
	
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
} irt_ocl_local_device;

typedef struct _irt_ocl_local_buffer {
	cl_mem mem;
	size_t size;
	struct _irt_ocl_local_buffer* next;
	cl_bool used;
	irt_ocl_local_device* local_dev; // derive the device directly from the buffer
} irt_ocl_local_buffer;

typedef struct _irt_ocl_local_kernel {
	cl_kernel cl_ker;
	irt_ocl_kernel_type type;
	cl_uint work_dim;
	size_t* global_work_offset;
	size_t* global_work_size;
	size_t* local_work_size;

	irt_spinlock kernel_lock;
	irt_ocl_local_device* local_dev;

	char** args; // is not important the type, is used only for the free
	int num_args; 
} irt_ocl_local_kernel;

irt_ocl_local_device* local_devices;
uint32_t local_num_devices;

// ------ Event Functions

#ifdef IRT_OCL_INSTR
#define IRT_OCL_EVENT_TABLE_BLOCKSIZE	5
typedef struct _irt_ocl_local_event {
	cl_event cl_event;
	uint64_t workitem_id;
} irt_ocl_local_event;

typedef struct _irt_ocl_event_table {
	uint32 size;
	uint32 num_events;
	uint32 blocksize;
	irt_ocl_local_event* event_array;
} irt_ocl_event_table;

irt_ocl_event_table* irt_ocl_create_event_table();
irt_ocl_local_event* irt_ocl_get_new_event();
void irt_ocl_release_event_table(irt_ocl_event_table* table);
#endif

#endif

typedef struct _irt_ocl_device {
	uint32_t node_id; 	// id of the node
	uint32_t device_id;	// id of the device in that machine
} irt_ocl_device;

typedef struct _irt_ocl_buffer {
	uint64_t buffer_add;
	irt_ocl_device* device;
} irt_ocl_buffer;

typedef struct _irt_ocl_kernel {
	uint64_t kernel_add;
	irt_ocl_device* device;
} irt_ocl_kernel;

irt_ocl_device* devices;
uint32_t num_devices;

void irt_ocl_init_devices();
uint32_t irt_ocl_get_num_devices();
irt_ocl_device* irt_ocl_get_device(uint32_t id);
void irt_ocl_release_devices();

irt_ocl_buffer* irt_ocl_create_buffer(irt_ocl_device* dev, irt_ocl_mem_flag flags, size_t size);
void irt_ocl_read_buffer(irt_ocl_buffer* buffer, irt_ocl_blocking_flag blocking, size_t offset, size_t size, void* source_ptr);
void irt_ocl_write_buffer(irt_ocl_buffer* buffer, irt_ocl_blocking_flag blocking, size_t offset, size_t size, const void* source_ptr);
//void irt_ocl_copy_buffer(irt_ocl_buffer* src_buf, irt_ocl_buffer* dest_buf, size_t size);
//void* irt_ocl_map_buffer(irt_ocl_buffer* buf, cl_bool blocking, cl_map_flags map_flags, size_t size);
//void irt_ocl_unmap_buffer(irt_ocl_buffer* buf, void* mapped_ptr);
void irt_ocl_release_buffer(irt_ocl_buffer* buffer);

void irt_ocl_set_kernel_ndrange(irt_ocl_kernel* kernel, uint32_t work_dim, size_t* global_work_offset, size_t* global_work_size, size_t* local_work_size);
void irt_ocl_create_kernel(irt_ocl_device* dev, irt_ocl_kernel* kernel, const char* file_name, const char* kernel_name, const char* build_options, irt_ocl_create_kernel_flag flag);
void irt_ocl_release_kernel(irt_ocl_kernel* kernel);

void irt_ocl_print_device_short_info(irt_ocl_device* dev);
void irt_ocl_print_device_infos(irt_ocl_device* dev);

// ------- Runtime Functions
typedef struct _irt_ocl_kernel_code {
	const char* kernel_name;
	const char* code;
} irt_ocl_kernel_code;

void irt_ocl_rt_create_all_kernels(irt_context* context, irt_ocl_kernel_code* g_kernel_code_table, uint32_t g_kernel_code_table_size);
void irt_ocl_rt_release_all_kernels(irt_context* context, uint32_t g_kernel_code_table_size);

irt_ocl_buffer* irt_ocl_rt_create_buffer(irt_ocl_mem_flag flags, size_t size);

void irt_ocl_rt_run_kernel(uint32_t kernel_id, uint32_t work_dim, size_t* global_work_offset, size_t* global_work_size, size_t* local_work_siz, uint32_t num_args, ...);

void irt_ocl_print_events();
