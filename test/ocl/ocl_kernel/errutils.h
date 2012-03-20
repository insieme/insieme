#pragma once

#include <stdio.h>
#include <CL/cl.h>

#define CLCHECK(_errcode) if((_errcode) != CL_SUCCESS) { fprintf(stderr, "OCL error, %s: %i \n #_errcode Error message: %s\n", __FILE__, __LINE__, clErrstring((_errcode))); exit(_errcode); }

const char* clErrstring(cl_int errcode) {
	switch(errcode) {
	case CL_SUCCESS:                          return "Success!"; break;
	case CL_DEVICE_NOT_FOUND:                 return "Device not found."; break;
	case CL_DEVICE_NOT_AVAILABLE:             return "Device not available"; break;
	case CL_COMPILER_NOT_AVAILABLE:           return "Compiler not available"; break;
	case CL_MEM_OBJECT_ALLOCATION_FAILURE:    return "Memory object allocation failure"; break;
	case CL_OUT_OF_RESOURCES:                 return "Out of resources"; break;
	case CL_OUT_OF_HOST_MEMORY:               return "Out of host memory"; break;
	case CL_PROFILING_INFO_NOT_AVAILABLE:     return "Profiling information not available"; break;
	case CL_MEM_COPY_OVERLAP:                 return "Memory copy overlap"; break;
	case CL_IMAGE_FORMAT_MISMATCH:            return "Image format mismatch"; break;
	case CL_IMAGE_FORMAT_NOT_SUPPORTED:       return "Image format not supported"; break;
	case CL_BUILD_PROGRAM_FAILURE:            return "Program build failure"; break;
	case CL_MAP_FAILURE:                      return "Map failure"; break;
	case CL_INVALID_VALUE:                    return "Invalid value"; break;
	case CL_INVALID_DEVICE_TYPE:              return "Invalid device type"; break;
	case CL_INVALID_PLATFORM:                 return "Invalid platform"; break;
	case CL_INVALID_DEVICE:                   return "Invalid device"; break;
	case CL_INVALID_CONTEXT:                  return "Invalid context"; break;
	case CL_INVALID_QUEUE_PROPERTIES:         return "Invalid queue properties"; break;
	case CL_INVALID_COMMAND_QUEUE:            return "Invalid command queue"; break;
	case CL_INVALID_HOST_PTR:                 return "Invalid host pointer"; break;
	case CL_INVALID_MEM_OBJECT:               return "Invalid memory object"; break;
	case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:  return "Invalid image format descriptor"; break;
	case CL_INVALID_IMAGE_SIZE:               return "Invalid image size"; break;
	case CL_INVALID_SAMPLER:                  return "Invalid sampler"; break;
	case CL_INVALID_BINARY:                   return "Invalid binary"; break;
	case CL_INVALID_BUILD_OPTIONS:            return "Invalid build options"; break;
	case CL_INVALID_PROGRAM:                  return "Invalid program"; break;
	case CL_INVALID_PROGRAM_EXECUTABLE:       return "Invalid program executable"; break;
	case CL_INVALID_KERNEL_NAME:              return "Invalid kernel name"; break;
	case CL_INVALID_KERNEL_DEFINITION:        return "Invalid kernel definition"; break;
	case CL_INVALID_KERNEL:                   return "Invalid kernel"; break;
	case CL_INVALID_ARG_INDEX:                return "Invalid argument index"; break;
	case CL_INVALID_ARG_VALUE:                return "Invalid argument value"; break;
	case CL_INVALID_ARG_SIZE:                 return "Invalid argument size"; break;
	case CL_INVALID_KERNEL_ARGS:              return "Invalid kernel arguments"; break;
	case CL_INVALID_WORK_DIMENSION:           return "Invalid work dimension"; break;
	case CL_INVALID_WORK_GROUP_SIZE:          return "Invalid work group size"; break;
	case CL_INVALID_WORK_ITEM_SIZE:           return "Invalid work item size"; break;
	case CL_INVALID_GLOBAL_OFFSET:            return "Invalid global offset"; break;
	case CL_INVALID_EVENT_WAIT_LIST:          return "Invalid event wait list"; break;
	case CL_INVALID_EVENT:                    return "Invalid event"; break;
	case CL_INVALID_OPERATION:                return "Invalid operation"; break;
	case CL_INVALID_GL_OBJECT:                return "Invalid OpenGL object"; break;
	case CL_INVALID_BUFFER_SIZE:              return "Invalid buffer size"; break;
	case CL_INVALID_MIP_LEVEL:                return "Invalid mip-map level"; break;
	default:                                  return "Unknown"; break;
	}
}
