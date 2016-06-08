/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#ifndef __GUARD_IRT_OCL_H
#define __GUARD_IRT_OCL_H

#include "error_handling.h"
#include "abstraction/threads.h"

#include <CL/cl.h>

enum irt_opencl_log_level {
	IRT_OPENCL_LOG_LEVEL_DEBUG = 0,
	IRT_OPENCL_LOG_LEVEL_INFO  = 1,
	IRT_OPENCL_LOG_LEVEL_WARN  = 2,
	IRT_OPENCL_LOG_LEVEL_ERROR = 3
};

extern void irt_opencl_lprintf(enum irt_opencl_log_level level, const char *format, ...);
extern void irt_opencl_pprintf(const char *format, ...);
#define OCL_DEBUG(format, ...) irt_opencl_lprintf(IRT_OPENCL_LOG_LEVEL_DEBUG, format, ##__VA_ARGS__)
#define OCL_INFO(format, ...)  irt_opencl_lprintf(IRT_OPENCL_LOG_LEVEL_INFO, format, ##__VA_ARGS__)
#define OCL_WARN(format, ...)  irt_opencl_lprintf(IRT_OPENCL_LOG_LEVEL_WARN, format, ##__VA_ARGS__)
#define OCL_ERROR(format, ...) irt_opencl_lprintf(IRT_OPENCL_LOG_LEVEL_ERROR, format, ##__VA_ARGS__)
#define OCL_PURE(format, ...)  irt_opencl_pprintf(format, ##__VA_ARGS__)

enum _irt_opencl_data_mode {
    IRT_OPENCL_DATA_MODE_READ_ONLY,
    IRT_OPENCL_DATA_MODE_WRITE_ONLY,
    IRT_OPENCL_DATA_MODE_READ_WRITE
};
typedef enum _irt_opencl_data_mode irt_opencl_data_mode;

/**
 * influences the interpretation of an optional argument
 */
enum _irt_opencl_optional_mode {
	IRT_OPENCL_OPTIONAL_MODE_HOST_PRIMITIVE,
	IRT_OPENCL_OPTIONAL_MODE_KRNL_BUFFER
};
typedef enum _irt_opencl_optional_mode irt_opencl_optional_mode;

#define IRT_OPENCL_MAX_DIMS 3
/**
 * this function is called to evaluate a given size_t
 */
struct _irt_opencl_ndrange {
    cl_uint work_dim;
    size_t global_offset_size[IRT_OPENCL_MAX_DIMS];
    size_t global_work_size[IRT_OPENCL_MAX_DIMS];
    size_t local_work_size[IRT_OPENCL_MAX_DIMS];
};
typedef struct _irt_opencl_ndrange irt_opencl_ndrange;
/**
 * this function is supposed to return an ndrange
 */
typedef irt_opencl_ndrange (*irt_opencl_ndrange_func)(
    irt_work_item *wi);

struct _irt_opencl_data_range {
    size_t size;
    size_t start;
    size_t end;
};
typedef struct _irt_opencl_data_range irt_opencl_data_range;
/**
 * this function is supposed to return a data_range for the given args
 */
typedef irt_opencl_data_range (*irt_opencl_data_range_func)(
    irt_work_item *wi, irt_opencl_ndrange *ndrange, unsigned arg, unsigned dim);
/**
 * represents the data requirement for one captured kernel argument.
 * in this context, element_type_id refers to the referenced type which
 * is used to build the dimensions. eg. float[N] -> float
 */
struct _irt_opencl_data_requirement {
    irt_type_id element_type_id;
    irt_opencl_data_mode mode;
    unsigned num_ranges;
    irt_opencl_data_range_func range_func;
};
typedef struct _irt_opencl_data_requirement irt_opencl_data_requirement;
/**
 * this function is supposed to return a data_requirement for the given args
 */
typedef irt_opencl_data_requirement (*irt_opencl_data_requirement_func)(
    irt_work_item *wi, irt_opencl_ndrange *ndrange, unsigned arg);
/**
 * represents a logical location of a device
 */
struct _irt_opencl_location {
	cl_uint platform;
	cl_uint device;
};
typedef struct _irt_opencl_location irt_opencl_location;
/* forward decl the device such that the platform can use it */
struct _irt_opencl_device;
/**
 * represents a platform within the context of openl
 */
struct _irt_opencl_platform {
  /* logical id of this platform */
  cl_uint id;
  /* opencl associated id */
  cl_platform_id platform_id;
  /* context which is used for all devices bound to this platform */
  cl_context context;
  /* number od irt_opencl_device structs linked into  */
  unsigned num_devices;
  /* points to the first discovered device */
  struct _irt_opencl_device *device;
  /* pointer to the next platform -- models a single linked list */
  struct _irt_opencl_platform *next;
};
typedef struct _irt_opencl_platform irt_opencl_platform;
/**
 * represents an OpenCL capable device including its properties
 */
struct _irt_opencl_device {
	cl_device_id device_id;
	cl_command_queue queue;
  /* platform which hosts this device */
  irt_opencl_platform *platform;
	/* logical device nr */
	irt_opencl_location location;
	/* lock to protect e.g. the queue from concurrent access */
	irt_spinlock lock;
	/* what follows is a list of properties */
	cl_uint address_bits;
	cl_bool available;
	cl_bool compiler_available;
	cl_device_fp_config double_fp_config;
	cl_bool endian_little;
	cl_bool error_correction_support;
	cl_device_exec_capabilities execution_capabilities;
	char *extensions;
	cl_ulong global_mem_cache_size;
	cl_device_mem_cache_type global_mem_cache_type;
	cl_uint global_mem_cacheline_size;
	cl_ulong global_mem_size;
	cl_ulong local_mem_size;
	cl_device_local_mem_type local_mem_type;
	cl_uint max_compute_units;
	cl_uint max_constant_args;
	cl_ulong max_constant_buffer_size;
	cl_ulong max_mem_alloc_size;
	size_t max_parameter_size;
	size_t max_work_group_size;
	cl_uint max_work_item_dimensions;
	size_t *max_work_item_sizes;
	cl_uint mem_base_addr_align;
	cl_uint min_data_type_align_size;
	char *name;
	cl_device_fp_config single_fp_config;
	cl_device_type device_type;

	/* pointer to the next device -- models a single linked list */
	struct _irt_opencl_device *next;
};
typedef struct _irt_opencl_device irt_opencl_device;
/**
 * a set of possible flags allowed within kernel data
 */
#define IRT_OPENCL_KERNEL_DATA_FLAG_BROKEN (1 << 0)
/**
 * models data associated with a kernel implementation. such data encapsulates
 * all OpenCL specific structures likes cl_kernel and others
 */
struct _irt_opencl_kernel_data {
	/* device for which this particular kernel has been built */
	irt_opencl_device *device;

	uint32 flags;
	cl_program program;
	cl_kernel kernel;
	/* bits of suitable devices */
	cl_device_type device_type;
	/* pointer to the next kernel -- models a single linked list */
	struct _irt_opencl_kernel_data *next;
};
typedef struct _irt_opencl_kernel_data irt_opencl_kernel_data;
/**
 * element of the kernel table which is generated by insiemecc. data is initially
 * set to 0 as it has only a meaning in IRT
 */
struct _irt_opencl_kernel_implementation {
    irt_opencl_kernel_data *data;
    const char *source;
	const char *routine;
};
typedef struct _irt_opencl_kernel_implementation irt_opencl_kernel_implementation;
/**
 * models the OpenCL context within IRT
 */
struct _irt_opencl_context {
	unsigned num_platforms;
  /* points to the first discovered platform */
  irt_opencl_platform *platform;
	/* table which holds all kernels */
	unsigned kernel_table_size;
    irt_opencl_kernel_implementation** kernel_table;
	struct {
		irt_opencl_location location;
		irt_opencl_kernel_data *(*select_kernel_data)(
			struct _irt_opencl_context *, irt_opencl_kernel_implementation *);
		enum irt_opencl_log_level log_level;
	} policy;
};
typedef struct _irt_opencl_context irt_opencl_context;
/**
 * initializes the opencl context
 *
 * @param context pointer to the default IRT context
 * @param impls pointer to an array of irt_opencl_kernel_implementation pointers
 *
 * @return void
 */
void irt_opencl_init_context(irt_context *contex, irt_opencl_kernel_implementation **impls);
/**
 * releases all resources bound to the opencl context during execution
 *
 * @param context pointer to the default IRT context
 *
 * @return void
 */
void irt_opencl_cleanup_context();
/**
 * issues the execution of an opencl kernel
 *
 * @param id id of kernel to execute
 * @param ndrange pointer to function which returns the ndrange for this execution
 * @param num_requirements number of requirements pointed to by the next argument
 * @param requirements pointer to an array of irt_opencl_requirement_func pointers
 * @param num_optionals number of optional arguments which will be passed (in-order) to the kernel
 * @param ... num_optionals tuples of form (size, arg)
 *
 * @return void, in case of an error the IRT triggers an assertion
 */
void irt_opencl_execute(unsigned id, irt_opencl_ndrange_func ndrange,
                        unsigned num_requirements, irt_opencl_data_requirement_func *requirements,
                        unsigned num_optionals, ...);

#endif // ifndef __GUARD_IRT_OPENCL_H
