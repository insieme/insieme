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

bool irt_opencl_init_devices(irt_opencl_context *context);
void irt_opencl_cleanup_devices(irt_opencl_context *context);
bool irt_opencl_init_kernel_table(irt_opencl_context *context, irt_opencl_kernel_implementation **kernel_table);
irt_opencl_context *irt_opencl_get_context(irt_context *context);
bool irt_opencl_init_device(cl_device_id device_id, cl_context context, irt_opencl_device *device);
void irt_opencl_cleanup_device(irt_opencl_device *device);
bool irt_opencl_init_kernel_implementation(irt_opencl_context *context, irt_opencl_kernel_implementation *impl);
void irt_opencl_cleanup_kernel_implementation(irt_opencl_kernel_implementation *impl);
irt_opencl_kernel_implementation *irt_opencl_get_kernel_implementation(irt_context *context, uint32 index);
void irt_opencl_execute_fallback(irt_work_item *wi);
void _irt_opencl_execute(unsigned id, irt_opencl_ndrange_func ndrange,
                         unsigned num_requirements, irt_opencl_data_requirement_func *requirements,
                         unsigned num_optionals, va_list optionals);
irt_work_item *irt_opencl_wi_get_current(unsigned id);

irt_opencl_context *irt_opencl_get_context(irt_context *context)
{
	return &(context->opencl_context);
}

irt_work_item *irt_opencl_wi_get_current(unsigned id)
{
	irt_work_item *wi = irt_wi_get_current();
	while (wi) {
		irt_wi_implementation* wimpl = wi->impl;
		for(uint32 i = 0; i < wimpl->num_variants; ++i) {
			irt_wi_implementation_variant *variant = &wimpl->variants[i];
			/* check if it has opencl support */
			if(!irt_meta_info_is_opencl_available(variant->meta_info))
				continue;

			/* does it match with the requested id? */
			opencl_info *info = irt_meta_info_get_opencl(variant->meta_info);
			if (info->kernel_id == id)
				/* wi is well formed return it */
				return wi;
		}
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
	if(!irt_opencl_init_kernel_implementation(opencl_context, impl)) {
		/* @TODO: mark it in a safe manner as 'broken' */
		impl->data->flags = 1;
		return NULL;
	}
	return impl;
}

void irt_opencl_execute_fallback(irt_work_item *wi)
{
	wi->impl->variants[0].implementation(wi);
}

void _irt_opencl_execute(unsigned id, irt_opencl_ndrange_func ndrange,
                         unsigned num_requirements, irt_opencl_data_requirement_func *requirements,
                         unsigned num_optionals, va_list optionals)
{    
    irt_work_item *wi = irt_opencl_wi_get_current(id);
	/* grab the current context such that we have access to tables */
	irt_context *irt_context = irt_context_get_current();
	irt_opencl_context *ocl_context = irt_opencl_get_context(irt_context);
	/* if no devices were found during lookup, we execute the fallback */
	if (ocl_context->device == NULL) {
		irt_opencl_execute_fallback(wi);
		return;
	}

	/* grab the current implementation */
	irt_opencl_kernel_implementation *impl = irt_opencl_get_kernel_implementation(irt_context, id);
	if (impl == NULL) {
		irt_opencl_execute_fallback(wi);
		return;
	}

	irt_opencl_device *device = ocl_context->device;	
	/* grab a pointer to the kernel impl data */
	irt_opencl_kernel_data *impl_data = impl->data;
	/* poiner to the captured environment */
	irt_lw_data_item *capture = wi->parameters;
	irt_type *capture_type = &irt_context->type_table[capture->type_id];
	/* the captured environment must be bound within a struct! */
	IRT_ASSERT(capture_type->kind == IRT_T_STRUCT, IRT_ERR_OCL, "expected captured environment of type %s but got %s\n",
			   irt_type_kind_get_name(IRT_T_STRUCT), irt_type_kind_get_name(capture_type->kind));
    /*
     * things to do:
     * o if irt_private has not yet been set-up, do it
     * o if a previous execution of this kernel has yield an error, execute default impl!
     * a command queue shall be present for each worker!
     */    
    
    /* ... */
    
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

	#ifdef IRT_VERBOSE
	uint64 start_ns = irt_time_ns();
	#endif // IRT_VERBOSE

    /* obtain the associated ndrange for this particular execution */
    irt_opencl_ndrange nd = ndrange(wi);
    for (unsigned arg = 0; arg < num_requirements; ++arg) {
        irt_opencl_data_requirement requirement = requirements[arg](wi, &nd, arg);
        /* create the buffer and offload to device memory */
        IRT_DEBUG("element_type_id:\t%d\nmode:\t%d\nnum_ranges:\t%d\nrange_func:\t%p\n",
				  requirement.element_type_id, requirement.mode, requirement.num_ranges, requirement.range_func);

		/* obtain all ranges which are associated with this requirement */
		size_t num_of_elements = 0;
		irt_opencl_data_range ranges[requirement.num_ranges];
		for (unsigned dim = 0; dim < requirement.num_ranges; ++dim) {
			ranges[dim] = requirement.range_func(wi, &nd, arg, dim);
			IRT_DEBUG("range:\t%d\nsize:\t%d\nstart:\t%d\nend:\t%d\n",
					  dim, ranges[dim].size(), ranges[dim].start(), ranges[dim].end());
			/* sum up all sum sizes to compute the total object size */
			num_of_elements += ranges[dim].size();
		}
		
		cl_mem_flags flags = 0;
		/* map the accessMode to flags */
		switch (requirement.mode) {
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
		/* compute the total size of the buffer */
		size_t size_of_element = irt_context->type_table[requirement.element_type_id].bytes;
		IRT_DEBUG("buffer size:\t%zu\n", num_of_elements * size_of_element);
		/* allocate the buffers on the device */
		buffer[arg] = clCreateBuffer(ocl_context->context, flags, num_of_elements * size_of_element, 0, &result);
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
		/* check if we face a pointer, at this level it must be! */
		irt_type *arg_type = &irt_context->type_table[capture_type->components[arg + 1]];
		IRT_ASSERT(arg_type->kind == IRT_T_POINTER, IRT_ERR_OCL, "expected captured indirection of type %s but got %s\n",
					irt_type_kind_get_name(IRT_T_POINTER), irt_type_kind_get_name(arg_type->kind));
		/*
		 * redirect data to actually "point" to the underlying blob.
		 * without components_offsets this would be wrong in many way
		 *
		 * note: uintptr_t is required here -- before the deref, data points to the pointer!
		 */
		data = (char *) *((uintptr_t**) data);
		/* 
		 * if the we face a double indirection like in:
		 *  struct __insieme_type_40 {
		 *		irt_type_id c0;
		 *		const int32_t* c1;
		 *		float** c2;
		 *		float** c3;
		 *		float** c4;;
		 *	};
		 *
		 * it can be detected by taking a look into num_components/components
		 */
		if (arg_type->num_components && irt_context->type_table[arg_type->components[0]].kind == IRT_T_POINTER) {
			/*
			 * lets redirect 'data' once again, this is very _important_ as points to our data blob!
			 * if the compiler generates meta-infos which are not a direct image of the actual case we will overwrite
			 * our stack and the program will blow up!!!
			 */
			data = *((char **) data);
		}
		
		IRT_DEBUG("clEnqueueWriteBuffer for requirement %d at %p\n", arg, data);
		/* @TODO: only transfere the ranges not the whole data blob! https://www.khronos.org/registry/cl/sdk/1.1/docs/man/xhtml/clEnqueueWriteBufferRect.html */
		result = clEnqueueWriteBuffer(device->queue, buffer[arg], CL_FALSE, 0, num_of_elements * size_of_element, data, 0, NULL, NULL);
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
     
    /* ... */
    for (unsigned opt = 0; opt < num_optionals; ++opt) {
        /* 
         * in order to be compatible with C99 we enforce that an optional
         * argument must have a size of at least uint8 and max uint64!
         * furthermore dealing with pointers would be broken per-se and complex
         * types can only be modeled using 'struct { vla }' which is _not_
         * supported by any compiler expect gcc.
         *
         * note: num_optionals refers the the number of tuples given (size, arg)
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
        result = clSetKernelArg(impl_data->kernel, num_requirements + opt, size, &data);
        /* iff the latter fails, the generated code has flaws and thus cannot continue */
        IRT_ASSERT(result == CL_SUCCESS, IRT_ERR_OCL, "clSetKernelArg returned with %d", result);
    }
	
    /* ... */
    clFinish(device->queue);
	size_t *global_work_size = nd.global_work_size;
	IRT_ASSERT(*global_work_size != 0, IRT_ERR_OCL, "global_work_size must be > 0");
	#ifdef IRT_VERBOSE
	for (unsigned i = 0; i < nd.work_dim; ++i)
		IRT_DEBUG("global_work_size[%d] = %d\n", i, (unsigned) global_work_size[i]);
	#endif // IRT_VERBOSE

	size_t *local_work_size = nd.local_work_size;
	/* special case if local size is 0, the implementation shall decide on how to split */
	if (*local_work_size == 0) local_work_size = NULL;
	#ifdef IRT_VERBOSE
	for (unsigned i = 0; i < nd.work_dim && local_work_size; ++i)
		IRT_DEBUG("local_work_size[%d] = %d\n", i, (unsigned) local_work_size[i]);
	#endif // IRT_VERBOSE

    /* at this point we can finally execute the given kernel */
    result = clEnqueueNDRangeKernel(device->queue, impl_data->kernel, nd.work_dim, NULL,
		global_work_size, local_work_size, 0, NULL, NULL);
    /* check for error and see if it is correctable */
    switch (result) {
    case CL_SUCCESS:
			IRT_DEBUG("kernel execution has been scheduled\n");
			break;
    case CL_OUT_OF_HOST_MEMORY:
    case CL_OUT_OF_RESOURCES:
    case CL_MEM_OBJECT_ALLOCATION_FAILURE:
            /* cleanup and execute the fallback routine (sequential impl) */
            /* @TODO: print error */
            IRT_WARN("clEnqueueNDRangeKernel returned with %d\n", result);
            return;
    default:
            /* error is not correctable... oops! */
            IRT_WARN("clEnqueueNDRangeKernel returned with %d\n", result);
            return;
    }
    
    /* wait until everythin is done */
	clFinish(device->queue);
	IRT_DEBUG("kernel execution has finished\n");
	
	/* copy all required parts back */
	for (unsigned i = 0; i < num_requirements; ++i) {
		if (reverse_offload[i].data == NULL)
			continue;

		IRT_DEBUG("clEnqueueReadBuffer for requirement %d at %p\n", i, reverse_offload[i].data);
		result = clEnqueueReadBuffer(device->queue, buffer[i], CL_FALSE, 0, reverse_offload[i].size, reverse_offload[i].data, 0, NULL, NULL);
		if (result != CL_SUCCESS) {
			/* @TODO: proper error handling !!!! */
			IRT_ASSERT(result == CL_SUCCESS, IRT_ERR_OCL, "clEnqueueReadBuffer returned with %d", result);
			return;
		}
	}
	clFinish(device->queue);
	
	IRT_DEBUG("read buffer has finished\n");
	IRT_DEBUG("\ntotal time %" PRId64 " ns\n", irt_time_ns() - start_ns);
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

bool irt_opencl_init_device(cl_device_id device_id, cl_context context, irt_opencl_device *device)
{
	cl_int result;
	/* clear off anything in the target structure we want to initialize */
	memset(device, 0, sizeof(*device));
	device->device_id = device_id;
	/* retain the context as this device uses it as well */
	device->context = context;
	clRetainContext(context);
	/* create a command queue for this device */
	device->queue = clCreateCommandQueue(context, device_id, 0, &result);
	if (result != CL_SUCCESS) {
		IRT_WARN("clCreateCommandQueue returned with %d\n", result);
		goto error;
	}
	
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
	irt_opencl_get_device_info(CL_DEVICE_PLATFORM, platform_id);
	irt_opencl_get_device_info(CL_DEVICE_SINGLE_FP_CONFIG, single_fp_config);
	irt_opencl_get_device_info(CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, max_work_item_dimensions);
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
	IRT_DEBUG("initialized device %s\n", device->name);
	return true;

error:
	irt_opencl_cleanup_device(device);
	return false;
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

	if (device->context) {
		clReleaseContext(device->context);
		device->context = NULL;
	}
	
	device->next = NULL;
}

bool irt_opencl_init_devices(irt_opencl_context *context)
{
	cl_int result;
	context->device = NULL;
	/* setup the 'next' pointer */
	irt_opencl_device **next = &context->device;
	
	cl_uint num_platforms;
	result = clGetPlatformIDs(0, NULL, &num_platforms);
	if (result != CL_SUCCESS)
		return false;
		
	cl_platform_id platforms[num_platforms];
	result = clGetPlatformIDs(num_platforms, platforms, NULL);
	if (result != CL_SUCCESS)
		return false;
	
	/* setup the context itself */
	cl_context_properties  properties[num_platforms*2 + 1];
	for (cl_uint i = 0; i < num_platforms; ++i) {
		properties[i*2+0] = CL_CONTEXT_PLATFORM;
		properties[i*2+1] = (cl_context_properties) platforms[i];
	}
	properties[num_platforms*2] = 0;
	
	context->context = clCreateContextFromType(properties, CL_DEVICE_TYPE_ALL, NULL, NULL, &result);
	if (result != CL_SUCCESS) {
		IRT_WARN("clCreateContextFromType returned with %d\n", result);
		return false;
	}
	
	IRT_DEBUG("found %d platforms\n", num_platforms);
	/* setup the devices for each platform */
	for (cl_uint i = 0; i < num_platforms; ++i) {
		cl_uint num_devices;
		result = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, 0, NULL, &num_devices);
		if (result != CL_SUCCESS)
			continue;
			
		cl_device_id devices[num_devices];
		result = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, num_devices, devices, NULL);
		if (result != CL_SUCCESS)
			continue;
		
		/* excellent, now we can initialize the specific device */
		for (cl_uint j = 0; j < num_devices; ++j) {
			irt_opencl_device *device = malloc(sizeof(*device));
			if (!irt_opencl_init_device(devices[j], context->context, device)) {
				free(device);
				continue;
			}
			/* link it into our list */
			*next = device;
			next = &device->next;
		}
	}
	
	if (context->device == NULL)
		IRT_WARN("no OpenCL capable devices found!\n");
	return true;
}

void irt_opencl_cleanup_devices(irt_opencl_context *context)
{
	irt_opencl_device *device =  context->device;
	while (device != NULL) {
		irt_opencl_device *next_device = device->next;
		irt_opencl_cleanup_device(device);
		device = next_device;
	}
}

bool irt_opencl_init_kernel_implementation(irt_opencl_context *context, irt_opencl_kernel_implementation *impl)
{
	cl_int result;
	/* if irt_private is set .. this kernel has already been initiaized */
	if (impl->data != NULL)
		/* however handle the case where it might be broken */
		return impl->data->flags == 0;

	impl->data = malloc(sizeof(*impl->data));
	memset(impl->data, 0, sizeof(*impl->data));
	/* shortcut such that we have one less indirection */
	irt_opencl_kernel_data *data = impl->data;
	
	size_t len = strlen(impl->source);
	data->program = clCreateProgramWithSource(context->context, 1, &impl->source, &len, &result);
	if (result != CL_SUCCESS) {
		IRT_WARN("clCreateProgramWithSource returned with %d\n", result);
		return false;
	}
	
	/* we have the program object, build it! */
	/* @TODO: this is a hack!!! */
	result = clBuildProgram(data->program, 1, &context->device->device_id, "", NULL, NULL);
	if (result != CL_SUCCESS) {
        // check build log
        size_t logSize;
        clGetProgramBuildInfo(data->program, context->device->device_id, 
                CL_PROGRAM_BUILD_LOG, 0, NULL, &logSize);
        char *programLog = (char*) calloc (logSize+1, sizeof(char));
        clGetProgramBuildInfo(data->program, context->device->device_id, 
                CL_PROGRAM_BUILD_LOG, logSize+1, programLog, NULL);
        IRT_WARN("Build Log:\n%s\n\n", programLog);
        free(programLog);
        IRT_WARN("clBuildProgram returned with %d\n", result);
        return false;
	}
	
	/* as we have the program now, build the kernel */
	/* @TODO: this is a hack!!! */
	data->kernel = clCreateKernel(data->program, "__insieme_fun_0", &result);
	if (result != CL_SUCCESS) {
		IRT_WARN("clCreateKernel returned with %d\n", result);
		return false;
	}
	
	return true;
}

void irt_opencl_cleanup_kernel_implementation(irt_opencl_kernel_implementation *impl)
{
	if (impl->data == NULL)
		return;
	/* shortcut such that we have one less indirection */
	irt_opencl_kernel_data *data = impl->data;
	
	if (data->kernel != NULL) {
		clReleaseKernel(data->kernel);
		data->kernel = NULL;
	}
	
	if (data->program != NULL) {
		clReleaseProgram(data->program);
		data->program = NULL;
	}
	
	free(data);
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
			
		IRT_DEBUG("registering kernel %d at %p\n", index, impl);
		irt_opencl_init_kernel_implementation(context, impl);
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

void irt_opencl_init_context(irt_context *context, irt_opencl_kernel_implementation **kernel_table)
{
	bool result;
	IRT_DEBUG("initializing...\n");
	irt_opencl_context *opencl_context = irt_opencl_get_context(context);
	
	IRT_DEBUG("initializing devices...\n");
	/* device discovery phase is the first step within our impl */
	result = irt_opencl_init_devices(opencl_context);
	if (!result) {
		IRT_WARN("failed to initialize devices");
		return;
	}
	
	IRT_DEBUG("initializing kernels...\n");
	/* device discovery is done, initialize the kernel table */
	result = irt_opencl_init_kernel_table(opencl_context, kernel_table);
	if (!result) {
		IRT_WARN("failed to initialize kernels");
		return;
	}
		
	IRT_DEBUG("done!\n");
}

void irt_opencl_cleanup_context(irt_context *context)
{
	IRT_DEBUG("cleanup...\n");
	irt_opencl_context *opencl_context = irt_opencl_get_context(context);
	
	IRT_DEBUG("cleanup kernels...\n");
	irt_opencl_cleanup_kernel_table(opencl_context);
	
	IRT_DEBUG("cleanup devices...\n");
	irt_opencl_cleanup_devices(opencl_context);
	
	IRT_DEBUG("done!\n");
}

#endif // ifndef __GUARD_IMPL_OPENCL_IMPL_H
