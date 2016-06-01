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
#ifndef __GUARD_IMPL_OPENCL_IMPL_H
#define __GUARD_IMPL_OPENCL_IMPL_H

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "abstraction/atomic.h"
#include "irt_types.h"
#include "impl/irt_context.impl.h"

struct _irt_opencl_event_list {
	unsigned max_events;
	unsigned num_events;
	cl_event *events;
};
typedef struct _irt_opencl_event_list irt_opencl_event_list;

bool irt_opencl_init_platforms(irt_opencl_context *context);
void irt_opencl_cleanup_platforms(irt_opencl_context *context);
bool irt_opencl_init_platform(irt_opencl_platform* platform);
void irt_opencl_cleanup_platform(irt_opencl_platform *platform);
bool irt_opencl_init_devices(irt_opencl_platform *platform);
void irt_opencl_cleanup_devices(irt_opencl_platform *platform);
bool irt_opencl_init_policy(irt_opencl_context *context);
bool irt_opencl_init_logging(irt_opencl_context *context);
bool irt_opencl_init_kernel_table(irt_opencl_context *context, irt_opencl_kernel_implementation **kernel_table);
irt_opencl_context *irt_opencl_get_context(irt_context *context);
void irt_opencl_dump_build_log(irt_opencl_device *device, cl_program program);
bool irt_opencl_init_device(cl_device_id device_id, irt_opencl_platform *platform, irt_opencl_location location, irt_opencl_device *device);
void irt_opencl_cleanup_device(irt_opencl_device *device);
void irt_opencl_lock_device(irt_opencl_device *device);
void irt_opencl_unlock_device(irt_opencl_device *device);
bool irt_opencl_init_kernel_data(irt_opencl_device *device, cl_program program, const char *routine, irt_opencl_kernel_data *data);
void irt_opencl_cleanup_kernel_data(irt_opencl_kernel_data *data);
bool irt_opencl_init_kernel_implementation(irt_opencl_context *context, unsigned id, irt_opencl_kernel_implementation *impl);
void irt_opencl_cleanup_kernel_implementation(irt_opencl_kernel_implementation *impl);
irt_opencl_kernel_implementation *irt_opencl_get_kernel_implementation(irt_context *context, uint32 index);
irt_opencl_kernel_data *irt_opencl_select_kernel_data_random(irt_opencl_context *context, irt_opencl_kernel_implementation *impl);
irt_opencl_kernel_data *irt_opencl_select_kernel_data_user_defined(irt_opencl_context *context, irt_opencl_kernel_implementation *impl);
irt_opencl_kernel_data *irt_opencl_select_kernel_data(irt_opencl_context *context, irt_opencl_kernel_implementation *impl);
void irt_opencl_execute_fallback(irt_work_item *wi);
void _irt_opencl_execute(unsigned id, irt_opencl_ndrange_func ndrange,
                         unsigned num_requirements, irt_opencl_data_requirement_func *requirements,
                         unsigned num_optionals, va_list optionals);
void irt_opencl_evaluate_ndrange(irt_opencl_ndrange *ndrange);
irt_work_item *irt_opencl_wi_get_current(unsigned id);
size_t irt_opencl_round_up(size_t value, size_t multiple);
irt_opencl_event_list *irt_opencl_event_list_create(unsigned max_events);
void irt_opencl_event_list_wait_and_free(irt_opencl_event_list *list);
cl_event *irt_opencl_event_list_advance(irt_opencl_event_list *list);
bool irt_opencl_location_is_equal(irt_opencl_location *fst, irt_opencl_location *snd);
opencl_info *irt_opencl_get_meta_info(unsigned id);
opencl_info *irt_opencl_wi_get_meta_info(irt_work_item *wi, unsigned id);
void irt_opencl_set_kernel_optionals(cl_kernel kernel, unsigned num_requirements, unsigned num_optionals, va_list optionals);
cl_mem_flags irt_opencl_get_data_mode_flags(irt_opencl_data_mode mode);

/**
 * helper to iterate over all devices known to the runtime
 */
#define foreach_device(context, device) \
	for (irt_opencl_platform *__platform = context->platform; __platform != NULL; __platform = __platform->next) \
		for (irt_opencl_device *device = __platform->device; device != NULL; device = device->next)

void irt_opencl_printf(enum irt_opencl_log_level level, const char *format, ...)
{
	irt_opencl_context *context = irt_opencl_get_context(irt_context_get_current());
	/* as we have no globals, consult the context */
	if (context->policy.log_level > level) return;

	va_list args;
	va_start(args, format);
	vfprintf(stdout, format, args);
	va_end(args);
	fflush(stdout);
}

bool irt_opencl_location_is_equal(irt_opencl_location *fst, irt_opencl_location *snd)
{
	return fst->platform == snd->platform && fst->device == snd->device;
}

irt_opencl_context *irt_opencl_get_context(irt_context *context)
{
	return &(context->opencl_context);
}

opencl_info *irt_opencl_get_meta_info(unsigned id)
{
	irt_context *context = irt_context_get_current();
	for (uint32 i = 0; i < context->info_table_size; ++i) {
		irt_meta_info_table_entry *meta_info = &context->info_table[i];

		/* check if it has opencl support */
		if(!irt_meta_info_is_opencl_available(meta_info))
			continue;

		/* does it match with the requested id? */
		opencl_info *info = irt_meta_info_get_opencl(meta_info);
		if (info->kernel_id == id)
			return info;
	}

	return NULL;
}

opencl_info *irt_opencl_wi_get_meta_info(irt_work_item *wi, unsigned id)
{
	irt_wi_implementation* wimpl = wi->impl;
	for(uint32 i = 0; i < wimpl->num_variants; ++i) {
		irt_wi_implementation_variant *variant = &wimpl->variants[i];
		/* check if it has opencl support */
		if(!irt_meta_info_is_opencl_available(variant->meta_info))
			continue;

		/* does it match with the requested id? */
		opencl_info *info = irt_meta_info_get_opencl(variant->meta_info);
		if (info->kernel_id == id)
			return info;
	}

	return NULL;
}

irt_work_item *irt_opencl_wi_get_current(unsigned id)
{
	irt_work_item *wi = irt_wi_get_current();
	while (wi) {
		/* does it match with the requested id? */
		opencl_info *info = irt_opencl_wi_get_meta_info(wi, id);
		if (info != NULL)
			/* wi is well formed return it */
			return wi;

		/* hop one level up .. try to find captured env there */
		wi = wi->parent_id.cached;
	}
	/* oops! in this case the compiler has produced utter garbage */
	IRT_ASSERT(wi != 0, IRT_ERR_OCL, "kernel %d not associated with wi %p", id, irt_wi_get_current());
	return 0;
}

irt_opencl_kernel_implementation *irt_opencl_get_kernel_implementation(irt_context *context, uint32 index)
{
	irt_opencl_context *opencl_context = irt_opencl_get_context(context);

	uint32 max_index = opencl_context->kernel_table_size;
	irt_opencl_kernel_implementation **table = opencl_context->kernel_table;

	IRT_ASSERT(index < max_index, IRT_ERR_OCL, "invalid kernel implementation index %d, maximum is %d", index, max_index);
	/* try to initialize it (only required in lazy initialization) */
	irt_opencl_kernel_implementation *impl = table[index];
	if(!irt_opencl_init_kernel_implementation(opencl_context, index, impl))
		return NULL;
	return impl;
}

irt_opencl_kernel_data *irt_opencl_select_kernel_data_random(irt_opencl_context *context, irt_opencl_kernel_implementation *impl)
{
	irt_opencl_kernel_data *data = impl->data;
	for (unsigned i = 0, m = rand() % data->device->platform->num_devices; i < m && data; ++i)
		data = data->next;

	if (!data)
		return impl->data;
	else
		return data;
}

irt_opencl_kernel_data *irt_opencl_select_kernel_data_user_defined(irt_opencl_context *context, irt_opencl_kernel_implementation *impl)
{
	irt_opencl_kernel_data *data = impl->data;
	while (data != NULL) {
		if (irt_opencl_location_is_equal(&data->device->location, &context->policy.location))
			break;
		data = data->next;
	}
	return data;
}

irt_opencl_kernel_data *irt_opencl_select_kernel_data(irt_opencl_context *context, irt_opencl_kernel_implementation *impl)
{
	/* if we have no impl we have no data as well */
	if (impl == NULL)
		return NULL;
	/*
	 * if this function is called with no platforms available, the privor
	 * safety checks have not noticed and not invokend the fallback!
	 */
	IRT_ASSERT(context->platform != NULL, IRT_ERR_OCL, "no platforms available");
	return context->policy.select_kernel_data(context, impl);
}

void irt_opencl_execute_fallback(irt_work_item *wi)
{
	wi->impl->variants[0].implementation(wi);
}

void irt_opencl_evaluate_ndrange(irt_opencl_ndrange *ndrange)
{
	for (unsigned i = 0; i < ndrange->work_dim; ++i) {
		/* in case no local size is supplied we pick a default one */
		if (ndrange->local_work_size[i] == 0)
			ndrange->local_work_size[i] = 32;
		/* evaluate and round up to a multiple of the associated local */
		if (ndrange->global_work_size[i]) {
			ndrange->global_work_size[i] = irt_opencl_round_up(ndrange->global_work_size[i],
															   ndrange->local_work_size[i]);
		}
		/* and finally print some info */
		OCL_DEBUG("ndrange[%d] %zu %zu %zu\n", i, ndrange->global_offset_size[i],
			ndrange->global_work_size[i], ndrange->local_work_size[i]);
	}
}

void irt_opencl_set_kernel_optionals(cl_kernel kernel, unsigned num_requirements, unsigned num_optionals, va_list optionals)
{
	cl_int result;
	for (unsigned opt = 0; opt < num_optionals; ++opt) {
        /*
         * in order to be compatible with C99 we enforce that an optional
         * argument must have a size of at least uint8 and max uint64!
         * furthermore dealing with pointers would be broken per-se and complex
         * types can only be modeled using 'struct { vla }' which is _not_
         * supported by any compiler expect gcc.
         *
         * note: num_optionals refers the the number of tuples given (size, arg, mod)
         */
        uint64 data;
        uint32 size = va_arg(optionals, uint32);
        switch (size) {
        case sizeof(uint8):
		case sizeof(uint16):
		case sizeof(uint32):
                data = va_arg(optionals, uint32);
                break;
		case sizeof(uint64):
                data = va_arg(optionals, uint64);
                break;
        default:
                IRT_ASSERT(false, IRT_ERR_OCL, "optional argument must fit in uint64, size %d", size);
                break;
        }
		irt_opencl_optional_mode modifier = va_arg(optionals, uint32);
		switch (modifier) {
		case IRT_OPENCL_OPTIONAL_MODE_HOST_PRIMITIVE:
			result = clSetKernelArg(kernel, num_requirements + opt, size, &data);
			break;
		case IRT_OPENCL_OPTIONAL_MODE_KRNL_BUFFER:
			result = clSetKernelArg(kernel, num_requirements + opt, data, NULL);
			break;
		}
        /* iff the latter fails, the generated code has flaws and thus cannot continue */
        IRT_ASSERT(result == CL_SUCCESS, IRT_ERR_OCL, "clSetKernelArg returned with %d", result);
    }
}

cl_mem_flags irt_opencl_get_data_mode_flags(irt_opencl_data_mode mode)
{
	cl_mem_flags flags = 0;
	/* map the accessMode to flags */
	switch (mode) {
	case IRT_OPENCL_DATA_MODE_READ_ONLY:
			flags |= CL_MEM_READ_ONLY;
			break;
	case IRT_OPENCL_DATA_MODE_WRITE_ONLY:
			flags |= CL_MEM_WRITE_ONLY;
			break;
	case IRT_OPENCL_DATA_MODE_READ_WRITE:
			flags |= CL_MEM_READ_WRITE;
			break;
	/* omit a default case in order to trigger a compile warning & let clCreateBuffer fail */
 }
 return flags;
}

void _irt_opencl_execute(unsigned id, irt_opencl_ndrange_func ndrange,
                         unsigned num_requirements, irt_opencl_data_requirement_func *requirements,
                         unsigned num_optionals, va_list optionals)
{
    irt_work_item *wi = irt_opencl_wi_get_current(id);
	/* grab the current context such that we have access to tables */
	irt_context *irt_context = irt_context_get_current();
	irt_opencl_context *ocl_context = irt_opencl_get_context(irt_context);

	/* grab the current implementation */
	irt_opencl_kernel_implementation *impl = irt_opencl_get_kernel_implementation(irt_context, id);
	if (impl == NULL || (impl->data->flags & IRT_OPENCL_KERNEL_DATA_FLAG_BROKEN)) {
		irt_opencl_execute_fallback(wi);
		return;
	}

	/* grab a pointer to the kernel impl data */
	irt_opencl_kernel_data *impl_data = irt_opencl_select_kernel_data(ocl_context, impl);
	if (impl_data == NULL) {
		irt_opencl_execute_fallback(wi);
		return;
	}
	irt_opencl_device *device = impl_data->device;
	/* poiner to the captured environment */
	irt_lw_data_item *capture = wi->parameters;
	irt_type *capture_type = &irt_context->type_table[capture->type_id];
	/* the captured environment must be bound within a struct! */
	IRT_ASSERT(capture_type->kind == IRT_T_STRUCT, IRT_ERR_OCL, "expected captured environment of type %s but got %s\n",
			   irt_type_kind_get_name(IRT_T_STRUCT), irt_type_kind_get_name(capture_type->kind));

	/* print out debugging information related to the device */
	OCL_DEBUG("executing kernel %d on device %s at %u:%u\n",
		id, device->name, device->location.platform, device->location.device);

	/* this will be required to locally wait on events  related to I/O*/
	irt_opencl_event_list *event_io = NULL;

    /* initialize opencl specific locals */
	cl_int result;
    /* each requirement is backed by an OpenCL buffer */
    cl_mem buffer[num_requirements];
    /* initialize via memset as = { 0 } is not guarenteed to work with VLAs */
    memset(buffer, 0, sizeof(buffer));
	/* for later usage where we need to retrieve the buffers again */
	struct {
		char *data;
		size_t size;
	} reverse_offload[num_requirements];

	uint64 start_ns = irt_time_ns();
	event_io = irt_opencl_event_list_create(num_requirements);
    /* obtain the associated ndrange for this particular execution */
    irt_opencl_ndrange nd = ndrange(wi);
    for (unsigned arg = 0; arg < num_requirements; ++arg) {
      irt_opencl_data_requirement requirement = requirements[arg](wi, &nd, arg);
      /* create the buffer and offload to device memory */
      OCL_DEBUG("element_type_id: %d\nmode: %d\nnum_ranges: %d\nrange_func: %p\n",
			  requirement.element_type_id, requirement.mode, requirement.num_ranges, requirement.range_func);

			/* obtain all ranges which are associated with this requirement */
			size_t num_of_elements = 0;
			irt_opencl_data_range ranges[requirement.num_ranges];
			for (unsigned dim = 0; dim < requirement.num_ranges; ++dim) {
				ranges[dim] = requirement.range_func(wi, &nd, arg, dim);
				OCL_DEBUG("range: %d\nsize: %zu\nstart: %zu\nend: %zu\n",
						  dim, ranges[dim].size, ranges[dim].start, ranges[dim].end);
				/* sum up all sum sizes to compute the total object size */
				num_of_elements += ranges[dim].size;
			}

			cl_mem_flags flags = irt_opencl_get_data_mode_flags(requirement.mode);
			/* compute the total size of the buffer */
			size_t size_of_element = irt_context->type_table[requirement.element_type_id].bytes;
			OCL_DEBUG("buffer size: %zu\n", num_of_elements * size_of_element);
			/* allocate the buffers on the device */
			buffer[arg] = clCreateBuffer(device->platform->context, flags, num_of_elements * size_of_element, 0, &result);
			if (result != CL_SUCCESS) {
				/* @TODO: release all buffers we have allocated so far .. also wait for pending transferes to end */
				/* @TODO: mark this kernel as faulty */
				IRT_ASSERT(result == CL_SUCCESS, IRT_ERR_OCL, "clCreateBuffer returned with %d", result);
				return;
			}
			/*
			 * the capture looks like the following:
			 * struct { type_id; c1; c2 ...; cN }
			 *
			 * thus the index of the first argument is 'arg + 1'
			 */
			char *data = (char *) capture + capture_type->components_offset[arg + 1];
			irt_type *arg_type = &irt_context->type_table[capture_type->components[arg + 1]];
			/* in case it is a scalar set the argument just right away */
			if (arg_type->kind >= IRT_T_BOOL && arg_type->kind <= IRT_T_REAL64) {
				clSetKernelArg(impl_data->kernel, arg, size_of_element, data);
				/* mark the buffer as RO and thus do not transfere it back! */
				reverse_offload[arg].data = NULL;
				continue;
			}

			/* check if we face a pointer, at this level it must be! */
			IRT_ASSERT(arg_type->kind == IRT_T_POINTER, IRT_ERR_OCL, "expected captured indirection of type %s but got %s\n",
						irt_type_kind_get_name(IRT_T_POINTER), irt_type_kind_get_name(arg_type->kind));
			/*
			 * redirect data to actually "point" to the underlying blob.
			 * without components_offsets this would be wrong in many way
			 *
			 * note: uintptr_t is required here -- before the deref, data points to the pointer!
			 */
			data = (char *) *((uintptr_t**) data);

			OCL_DEBUG("clEnqueueWriteBuffer for requirement %d at %p\n", arg, data);
			irt_opencl_lock_device(device);
			/* @TODO: only transfere the ranges not the whole data blob! https://www.khronos.org/registry/cl/sdk/1.1/docs/man/xhtml/clEnqueueWriteBufferRect.html */
			result = clEnqueueWriteBuffer(device->queue, buffer[arg], CL_FALSE, 0, num_of_elements * size_of_element,
				data, 0, NULL, irt_opencl_event_list_advance(event_io));
			irt_opencl_unlock_device(device);
			if (result != CL_SUCCESS) {
				/* @TODO: proper error handling !!!! */
				IRT_ASSERT(result == CL_SUCCESS, IRT_ERR_OCL, "clEnqueueWriteBuffer returned with %d", result);
				return;
			}

			/* and hand it over to the kernel itself */
			clSetKernelArg(impl_data->kernel, arg, sizeof(cl_mem), &buffer[arg]);

			/* iff we are read-write .. also move it back afterwards! */
			if (requirement.mode == IRT_OPENCL_DATA_MODE_READ_WRITE || requirement.mode == IRT_OPENCL_DATA_MODE_WRITE_ONLY) {
				reverse_offload[arg].data = data;
				reverse_offload[arg].size = num_of_elements * size_of_element;
			} else {
				reverse_offload[arg].data = NULL;
			}
    }

    /* include any additional parameters */
    irt_opencl_set_kernel_optionals(impl_data->kernel, num_requirements, num_optionals, optionals);
	/* copy to device has been set up .. wait for them to complete */
    irt_opencl_event_list_wait_and_free(event_io);
	event_io = NULL;

	/* make sure everything is set up properly */
	irt_opencl_evaluate_ndrange(&nd);

	cl_event event_krnl;
    /* at this point we can finally execute the given kernel */
	irt_opencl_lock_device(device);
    result = clEnqueueNDRangeKernel(device->queue, impl_data->kernel, nd.work_dim, nd.global_offset_size,
		nd.global_work_size, nd.local_work_size, 0, NULL, &event_krnl);
	irt_opencl_unlock_device(device);
    /* check for error and see if it is correctable */
    switch (result) {
    case CL_SUCCESS:
			OCL_DEBUG("kernel execution has been scheduled\n");
			break;
    case CL_OUT_OF_HOST_MEMORY:
    case CL_OUT_OF_RESOURCES:
    case CL_MEM_OBJECT_ALLOCATION_FAILURE:
            /* cleanup and execute the fallback routine (sequential impl) */
            /* @TODO: print error */
            OCL_WARN("clEnqueueNDRangeKernel returned with %d\n", result);
            return;
    default:
            /* error is not correctable... oops! */
            OCL_WARN("clEnqueueNDRangeKernel returned with %d\n", result);
            return;
    }

    /* wait until everythin is done */
	clWaitForEvents(1, &event_krnl);
	OCL_DEBUG("kernel execution has finished\n");

	/* copy all required parts back */
	event_io = irt_opencl_event_list_create(num_requirements);
	for (unsigned i = 0; i < num_requirements; ++i) {
		if (reverse_offload[i].data == NULL)
			continue;

		OCL_DEBUG("clEnqueueReadBuffer for requirement %d at %p\n", i, reverse_offload[i].data);
		irt_opencl_lock_device(device);
		result = clEnqueueReadBuffer(device->queue, buffer[i], CL_FALSE, 0, reverse_offload[i].size,
			reverse_offload[i].data, 0, NULL, irt_opencl_event_list_advance(event_io));
		irt_opencl_unlock_device(device);
		if (result != CL_SUCCESS) {
			/* @TODO: proper error handling !!!! */
			IRT_ASSERT(result == CL_SUCCESS, IRT_ERR_OCL, "clEnqueueReadBuffer returned with %d", result);
			return;
		}
	}
	irt_opencl_event_list_wait_and_free(event_io);
	event_io = NULL;

	OCL_DEBUG("read buffer has finished\n");
	OCL_DEBUG("\ntotal time %" PRId64 " ns\n", irt_time_ns() - start_ns);
	/* everything done .. release all buffers */
	for (unsigned i = 0; i < num_requirements; ++i)
		clReleaseMemObject(buffer[i]);
}

void irt_opencl_execute(unsigned id, irt_opencl_ndrange_func ndrange,
                        unsigned num_requirements, irt_opencl_data_requirement_func *requirements,
                        unsigned num_optionals, ...)
{
    va_list optionals;
    va_start(optionals, num_optionals);
    /* at this point we can call the actual implementation */
    _irt_opencl_execute(id, ndrange, num_requirements, requirements, num_optionals, optionals);
    va_end(optionals);
}

bool irt_opencl_init_device(cl_device_id device_id, irt_opencl_platform *platform, irt_opencl_location location, irt_opencl_device *device)
{
	cl_int result;
	/* clear off anything in the target structure we want to initialize */
	memset(device, 0, sizeof(*device));
	device->device_id = device_id;
	device->location = location;
	device->platform = platform;
	/* create a command queue for this device */
	device->queue = clCreateCommandQueue(platform->context, device_id, 0, &result);
	if (result != CL_SUCCESS) {
		OCL_WARN("clCreateCommandQueue returned with %d\n", result);
		goto error;
	}
	irt_spin_init(&device->lock);

	/* query all properties */
	#define irt_opencl_get_device_info(param_name, param_value) \
		clGetDeviceInfo(device_id, param_name, sizeof(device->param_value), &device->param_value, 0)
	irt_opencl_get_device_info(CL_DEVICE_ADDRESS_BITS, address_bits);
	irt_opencl_get_device_info(CL_DEVICE_AVAILABLE, available);
	irt_opencl_get_device_info(CL_DEVICE_COMPILER_AVAILABLE, compiler_available);
	irt_opencl_get_device_info(CL_DEVICE_DOUBLE_FP_CONFIG, double_fp_config);
	irt_opencl_get_device_info(CL_DEVICE_ENDIAN_LITTLE, endian_little);
	irt_opencl_get_device_info(CL_DEVICE_ERROR_CORRECTION_SUPPORT, error_correction_support);
	irt_opencl_get_device_info(CL_DEVICE_EXECUTION_CAPABILITIES, execution_capabilities);
	irt_opencl_get_device_info(CL_DEVICE_GLOBAL_MEM_SIZE, global_mem_cache_size);
	irt_opencl_get_device_info(CL_DEVICE_GLOBAL_MEM_CACHE_TYPE, global_mem_cache_type);
	irt_opencl_get_device_info(CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE, global_mem_cacheline_size);
	irt_opencl_get_device_info(CL_DEVICE_GLOBAL_MEM_SIZE, global_mem_size);
	irt_opencl_get_device_info(CL_DEVICE_LOCAL_MEM_SIZE, local_mem_size);
	irt_opencl_get_device_info(CL_DEVICE_LOCAL_MEM_TYPE, local_mem_type);
	irt_opencl_get_device_info(CL_DEVICE_MAX_COMPUTE_UNITS, max_compute_units);
	irt_opencl_get_device_info(CL_DEVICE_MAX_CONSTANT_ARGS, max_constant_args);
	irt_opencl_get_device_info(CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, max_constant_buffer_size);
	irt_opencl_get_device_info(CL_DEVICE_MAX_MEM_ALLOC_SIZE, max_mem_alloc_size);
	irt_opencl_get_device_info(CL_DEVICE_MAX_PARAMETER_SIZE, max_parameter_size);
	irt_opencl_get_device_info(CL_DEVICE_MEM_BASE_ADDR_ALIGN, mem_base_addr_align);
	irt_opencl_get_device_info(CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE, min_data_type_align_size);
	irt_opencl_get_device_info(CL_DEVICE_SINGLE_FP_CONFIG, single_fp_config);
	irt_opencl_get_device_info(CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, max_work_item_dimensions);
	irt_opencl_get_device_info(CL_DEVICE_TYPE, device_type);
	#undef irt_opencl_get_device_info

	/* at this point only dynamically sized properties are left */
	size_t size;
	clGetDeviceInfo(device_id, CL_DEVICE_EXTENSIONS, 0, 0, &size);
	device->extensions = malloc(size);
	if (device->extensions == NULL)
		goto error;
	clGetDeviceInfo(device_id, CL_DEVICE_EXTENSIONS, size, device->extensions, 0);

	size = sizeof(*device->max_work_item_sizes) * device->max_work_item_dimensions;
	device->max_work_item_sizes = malloc(size);
	if (device->max_work_item_sizes == NULL)
		goto error;
	clGetDeviceInfo(device_id, CL_DEVICE_MAX_WORK_ITEM_SIZES, size, device->max_work_item_sizes, 0);

	clGetDeviceInfo(device_id, CL_DEVICE_NAME, 0, 0, &size);
	device->name = malloc(size);
	if (device->name == NULL)
		goto error;
	clGetDeviceInfo(device_id, CL_DEVICE_NAME, size, device->name, 0);

	/* print out the name that we have feedback in the log */
	OCL_INFO("initialized device %s at %u:%u\n", device->name, location.platform, location.device);
	return true;

error:
	irt_opencl_cleanup_device(device);
	return false;
}

void irt_opencl_lock_device(irt_opencl_device *device)
{
	irt_spin_lock(&device->lock);
}

void irt_opencl_unlock_device(irt_opencl_device *device)
{
	irt_spin_unlock(&device->lock);
}

void irt_opencl_dump_build_log(irt_opencl_device *device, cl_program program)
{
	size_t len;
	cl_int result;
	/* obtain the length of the log */
	result = clGetProgramBuildInfo(program, device->device_id, CL_PROGRAM_BUILD_LOG, 0, NULL, &len);
	if (result != CL_SUCCESS) {
		OCL_WARN("failed to obtain build log due to: %d", result);
		return;
	}

	char *log = calloc(++len, sizeof(char));
	result = clGetProgramBuildInfo(program, device->device_id, CL_PROGRAM_BUILD_LOG, len, log, NULL);
	if (result != CL_SUCCESS) {
		OCL_WARN("failed to obtain build log due to: %d", result);
		free(log);
		return;
	}
	/* finally dump the issue */
	OCL_WARN("%s\n\n", log);
	free(log);
}

void irt_opencl_cleanup_device(irt_opencl_device *device)
{
	if (device->name) {
		free(device->name);
		device->name = NULL;
	}

	if (device->max_work_item_sizes) {
		free(device->max_work_item_sizes);
		device->max_work_item_sizes = NULL;
	}

	if (device->extensions) {
		free(device->extensions);
		device->extensions = NULL;
	}

	if (device->queue) {
		clReleaseCommandQueue(device->queue);
		device->queue = NULL;
	}

	device->next = NULL;
}

bool irt_opencl_init_platforms(irt_opencl_context *context)
{
	cl_int result;
	context->platform = NULL;

	cl_uint num_platforms;
	result = clGetPlatformIDs(0, NULL, &num_platforms);
	if (result != CL_SUCCESS)
		return false;

	cl_platform_id platforms[num_platforms];
	result = clGetPlatformIDs(num_platforms, platforms, NULL);
	if (result != CL_SUCCESS)
		return false;

	OCL_DEBUG("found %d platforms\n", num_platforms);
	context->num_platforms = num_platforms;
	/* setup the 'next' pointer */
	irt_opencl_platform **next = &context->platform;
	/* initialize each platform */
	for (cl_uint i = 0; i < num_platforms; ++i) {
		irt_opencl_platform *platform = malloc(sizeof(*platform));
		memset(platform, 0, sizeof(*platform));
		/* setup basic properties */
		platform->id = i;
		platform->platform_id = platforms[i];
		if (!irt_opencl_init_platform(platform)) {
			free(platform);
			continue;
		}
		/* link it into our list */
		*next = platform;
		next = &platform->next;
	}
	return true;
}

void irt_opencl_cleanup_platforms(irt_opencl_context *context)
{
	irt_opencl_platform *platform = context->platform;
	while (platform != NULL) {
		irt_opencl_platform *next_platform = platform->next;
		irt_opencl_cleanup_platform(platform);
		free(platform);
		platform = next_platform;
	}
}

bool irt_opencl_init_platform(irt_opencl_platform *platform)
{
	cl_int result;
	/* setup the context itself */
	cl_context_properties  properties[3];
	properties[0] = CL_CONTEXT_PLATFORM;
	properties[1] = (cl_context_properties) platform->platform_id;
	properties[2] = 0;

	OCL_DEBUG("initializing platform %d\n", platform->id);
	platform->context = clCreateContextFromType(properties, CL_DEVICE_TYPE_ALL, NULL, NULL, &result);
	if (result != CL_SUCCESS) {
		OCL_WARN("clCreateContextFromType returned with %d\n", result);
		return false;
	}

	if (!irt_opencl_init_devices(platform)) {
		/* free the context and fail-fast */
		clReleaseContext(platform->context);
		return false;
	}
	return true;
}

void irt_opencl_cleanup_platform(irt_opencl_platform *platform)
{
	if (platform->context != NULL) {
		clReleaseContext(platform->context);
		platform->context = NULL;
	}

	irt_opencl_cleanup_devices(platform);
}

bool irt_opencl_init_devices(irt_opencl_platform *platform)
{
	cl_int result;
	platform->device = NULL;
	platform->num_devices = 0;
	/* setup the 'next' pointer */
	irt_opencl_device **next = &platform->device;

	cl_uint num_devices;
	result = clGetDeviceIDs(platform->platform_id, CL_DEVICE_TYPE_ALL, 0, NULL, &num_devices);
	if (result != CL_SUCCESS)
		return false;

	cl_device_id devices[num_devices];
	result = clGetDeviceIDs(platform->platform_id, CL_DEVICE_TYPE_ALL, num_devices, devices, NULL);
	if (result != CL_SUCCESS)
		return false;

	/* excellent, now we can initialize the specific device */
	for (cl_uint i = 0; i < num_devices; ++i) {
		irt_opencl_device *device = malloc(sizeof(*device));
		irt_opencl_location location = {platform->id, i};
		if (!irt_opencl_init_device(devices[i], platform, location, device)) {
			free(device);
			continue;
		}
		/* link it into our list */
		*next = device;
		next = &device->next;
	}
	platform->num_devices += num_devices;
	return platform->device != NULL;
}

void irt_opencl_cleanup_devices(irt_opencl_platform *platform)
{
	irt_opencl_device *device =  platform->device;
	while (device != NULL) {
		irt_opencl_device *next_device = device->next;
		irt_opencl_cleanup_device(device);
		free(device);
		device = next_device;
	}
}

bool irt_opencl_init_kernel_data(irt_opencl_device *device, cl_program program, const char *routine, irt_opencl_kernel_data *data)
{
	cl_int result;
	/* if the device does not have compiler support fail-fast */
	if (!device->compiler_available)
		return false;

	/* first of all clear the blob */
	memset(data, 0, sizeof(*data));
	data->device = device;
	/* set it to broken per default .. fixup in the exit path */
	data->flags |= IRT_OPENCL_KERNEL_DATA_FLAG_BROKEN;
	/* try to build the program for the associated device */
	result = clBuildProgram(program, 1, &device->device_id, "", NULL, NULL);
	if (result != CL_SUCCESS) {
    OCL_WARN("clBuildProgram returned with %d, build log follows:\n", result);
		irt_opencl_dump_build_log(device, program);
    return false;
	}
	/* associate the entry point */
	data->kernel = clCreateKernel(program, routine, &result);
	if (result != CL_SUCCESS) {
		/* built program cannot be undone -- skip that */
		OCL_WARN("clCreateKernel returned with %d\n", result);
		return false;
	}
	/* retain a reference to the program and store locally */
	clRetainProgram(program);
	data->program = program;
	/* remove the broken flag */
	data->flags &= ~IRT_OPENCL_KERNEL_DATA_FLAG_BROKEN;
	return true;
}

void irt_opencl_cleanup_kernel_data(irt_opencl_kernel_data *data)
{
	if (data->kernel != NULL)
		clReleaseKernel(data->kernel);

	if (data->program != NULL)
		clReleaseProgram(data->program);
}

bool irt_opencl_init_kernel_implementation(irt_opencl_context *context, unsigned id, irt_opencl_kernel_implementation *impl)
{
	cl_int result;
	/* if irt_private is set .. this kernel has already been initiaized */
	if (impl->data != NULL)
		return true;

	/* obtain the meta info to check for which devices this impl is suitable */
	opencl_info *meta_info = irt_opencl_get_meta_info(id);

	irt_opencl_kernel_data *data = NULL;
	/* iterate over all devices and build the program */
	foreach_device(context, device) {
		/* check if this type of device is ok for this impl */
		if ((device->device_type & meta_info->device_type) == 0)
			continue;
		/* allocate a new kernel_data structure if we cannot reuse the latter one */
		if (data == NULL)
			data = malloc(sizeof(*data));

		/* create the program only once, all kernel_data structs only reference it */
		cl_program program = clCreateProgramWithSource(device->platform->context, 1, &impl->source, NULL, &result);
		if (result != CL_SUCCESS) {
			OCL_WARN("clCreateProgramWithSource returned with %d\n", result);
			continue;
		}

		if (irt_opencl_init_kernel_data(device, program, impl->routine, data)) {
			/* link in the data */
			data->next = impl->data;
			impl->data = data;
			/* request a new blob for the next round */
			data = NULL;
		} else {
			/* kernels which do not compile are never kept in the context */
			OCL_WARN("failed to compile kernel %d on device %s at %u:%u\n", id,
				device->name, device->location.platform, device->location.device);
		}

		clReleaseProgram(program);
	}
	/* free data if there is a leftover */
	if (data != NULL)
		free(data);
	return true;
}

void irt_opencl_cleanup_kernel_implementation(irt_opencl_kernel_implementation *impl)
{
	if (impl->data == NULL)
		return;
	/* free all kernel_data structures associated with this impl */
	while (impl->data != NULL) {
		irt_opencl_kernel_data *data = impl->data;
		/* obtain pointer to next one before cleanup */
		impl->data = data->next;

		irt_opencl_cleanup_kernel_data(data);
		free(data);
	}
}

bool irt_opencl_init_kernel_table(irt_opencl_context *context, irt_opencl_kernel_implementation **kernel_table)
{
	/* first of all, we need to "register" the table within the context */
	context->kernel_table = kernel_table;
	/* pre-initialize counters */
	context->kernel_table_size = 0;
	/* go though all kernels and initialize them */
	for (uint32 index = 0; ; ++index) {
		irt_opencl_kernel_implementation *impl = kernel_table[index];
		if (impl == 0)
			break;

		OCL_DEBUG("initializing kernel %d at %p\n", index, impl);
		irt_opencl_init_kernel_implementation(context, index, impl);
		// right now, we only increase the total count
		context->kernel_table_size += 1;
	}
	return true;
}

void irt_opencl_cleanup_kernel_table(irt_opencl_context *context)
{
	for (unsigned i = 0; i < context->kernel_table_size; ++i)
		irt_opencl_cleanup_kernel_implementation(context->kernel_table[i]);
}

bool irt_opencl_init_logging(irt_opencl_context *context)
{
	char *value = NULL;
	/* check for loglevel related manners */
	if ((value = getenv("IRT_OPENCL_LOG_LEVEL")) != NULL) {
		bool fail = true;
		unsigned log_level;
		/* parse the log_level from the supplied string */
		if (sscanf(value, "%u", &log_level) == 1) {
			/* check for validity */
			fail = !(IRT_OPENCL_LOG_LEVEL_DEBUG <= log_level &&
					 IRT_OPENCL_LOG_LEVEL_ERROR >= log_level);
		}

		if (fail) {
			OCL_WARN("invalid log_level supplied: %s", value);
		} else {
			context->policy.log_level = (enum irt_opencl_log_level) log_level;
		}
	}
	return true;
}

bool irt_opencl_init_policy(irt_opencl_context *context)
{
	char *value = NULL;
	/* predefine to location to use the first one */
	context->policy.location.platform  = 0;
	context->policy.location.device = 0;
	context->policy.select_kernel_data = &irt_opencl_select_kernel_data_random;
	/* obtain the user defined selection from the environment */
	if ((value = getenv("IRT_OPENCL_DEVICE_LOCATION")) != NULL) {
		bool fail = true;
		irt_opencl_location location;
		/* parse the tuple (platform,device) */
		if (sscanf(value, "%u:%u", &location.platform, &location.device) == 2) {
			/* check for validity */
			foreach_device(context, device) {
				if (irt_opencl_location_is_equal(&location, &device->location)) {
					fail = false;
					break;
				}
			}
		}

		if (fail) {
			OCL_WARN("invalid device location supplied: %s", value);
		} else {
			context->policy.location = location;
			context->policy.select_kernel_data = &irt_opencl_select_kernel_data_user_defined;
		}
	}
	return true;
}

void irt_opencl_init_context(irt_context *context, irt_opencl_kernel_implementation **kernel_table)
{
	bool result;
	irt_opencl_context *opencl_context = irt_opencl_get_context(context);
	/* this is the default logging level */
	#ifndef IRT_VERBOSE
	opencl_context->policy.log_level = IRT_OPENCL_LOG_LEVEL_WARN;
	#else
	opencl_context->policy.log_level = IRT_OPENCL_LOG_LEVEL_DEBUG;
	#endif

	/* preinit the counters */
	opencl_context->num_platforms = 0;
	opencl_context->kernel_table_size = 0;

	OCL_DEBUG("initializing logging...\n");
	result = irt_opencl_init_logging(opencl_context);
	if (!result) {
		OCL_WARN("failed to initialize policy");
		return;
	}

	OCL_DEBUG("initializing platforms...\n");
	/* device discovery phase is the first step within our impl */
	result = irt_opencl_init_platforms(opencl_context);
	if (!result) {
		OCL_WARN("failed to initialize platforms");
		return;
	}

	OCL_DEBUG("initializing kernels...\n");
	/* device discovery is done, initialize the kernel table */
	result = irt_opencl_init_kernel_table(opencl_context, kernel_table);
	if (!result) {
		OCL_WARN("failed to initialize kernels");
		return;
	}

	result = irt_opencl_init_policy(opencl_context);
	if (!result) {
		OCL_WARN("failed to initialize policy");
		return;
	}

	OCL_DEBUG("done!\n");
}

void irt_opencl_cleanup_context(irt_context *context)
{
	OCL_DEBUG("cleanup...\n");
	irt_opencl_context *opencl_context = irt_opencl_get_context(context);

	OCL_DEBUG("cleanup kernels...\n");
	irt_opencl_cleanup_kernel_table(opencl_context);

	OCL_DEBUG("cleanup platforms...\n");
	irt_opencl_cleanup_platforms(opencl_context);

	OCL_DEBUG("done!\n");
}

size_t irt_opencl_round_up(size_t value, size_t multiple)
{
    if (multiple == 0)
        return value;

    size_t remainder = value % multiple;
    if (remainder == 0)
        return value;

    return value + multiple - remainder;
}

irt_opencl_event_list *irt_opencl_event_list_create(unsigned max_events)
{
	irt_opencl_event_list *list = malloc(sizeof(*list));
	list->max_events = max_events;
	list->num_events = 0;
	if (max_events)
		list->events = malloc(sizeof(cl_event) * max_events);
	else
		list->events = NULL;
	return list;
}

void irt_opencl_event_list_wait_and_free(irt_opencl_event_list *list)
{
	if (list == NULL)
		return;

	if (list->num_events) {
		clWaitForEvents(list->num_events, list->events);
		for (unsigned i = 0; i < list->num_events; ++i)
			clReleaseEvent(list->events[i]);
	}
	/* free(NULL) is never an error */
	free(list->events);
}

cl_event *irt_opencl_event_list_advance(irt_opencl_event_list *list)
{
	/* in this case we have free space avail. */
	IRT_ASSERT(list->num_events < list->max_events, IRT_ERR_OCL, "no event slots available");
	/* in case assertions are disabled */
	if (list->num_events >= list->max_events)
		return NULL;

	return &list->events[list->num_events++];
}

#endif // ifndef __GUARD_IMPL_OPENCL_IMPL_H
