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
#include <unistd.h>
#include "mpi.h"
#include "impl/irt_ocl.impl.h"

int main(int argc, char *argv[]) {
	cl_int size;
	MPI_Comm intercom, everyone;
	MPI_Init(&argc, &argv);
	MPI_Comm_get_parent(&intercom);
	IRT_ASSERT(intercom != MPI_COMM_NULL, IRT_ERR_OCL, "Error during MPI Worker initialization: \"No parent\"");
	
	MPI_Comm_remote_size(intercom, &size);
	IRT_ASSERT(size == 1, IRT_ERR_OCL, "Error during MPI Worker initialization: \"Something wrong with the parent\"");

	MPI_Status status;
	MPI_Intercomm_merge(intercom, 1, &everyone);

	cl_int my_rank;
	MPI_Comm_rank(everyone, &my_rank);
	
	while(1) {
		MPI_Probe(0, MPI_ANY_TAG, everyone, &status);
		int tag = status.MPI_TAG;
		int irt_ocl_tag = (tag & 32767);
		//printf("tag = %lu, irt_ocl_tag = %lu\n", tag, irt_ocl_tag);
		switch(irt_ocl_tag) {
			case IRT_OCL_INIT_DEVS: {
				cl_uint device_type;
				MPI_Recv(&device_type, 8, MPI_BYTE, 0, IRT_OCL_INIT_DEVS, everyone, NULL);
				irt_ocl_init_devices(device_type);
				cl_uint num_dev = irt_ocl_get_num_devices(); // num of devices of the node
				MPI_Send(&num_dev, 4, MPI_BYTE, 0, IRT_OCL_INIT_DEVS, everyone);
				break;
			}
			
			case IRT_OCL_RELEASE_DEVS: {
				MPI_Recv(NULL, 0, MPI_BYTE, 0, IRT_OCL_RELEASE_DEVS, everyone, NULL);
				irt_ocl_release_devices();
				goto exit_label;
				break;
			}
			
			case IRT_OCL_CREATE_KERNEL: {
				cl_uint dev_id;
				MPI_Recv(&dev_id, 4, MPI_BYTE, 0, IRT_OCL_CREATE_KERNEL, everyone, NULL);

				int len = 0;
				MPI_Probe(0, IRT_OCL_CREATE_KERNEL, everyone, &status);
				MPI_Get_count(&status, MPI_CHAR, &len);
				char* file_name = malloc(sizeof(char) * len);
				MPI_Recv(&file_name[0], len, MPI_CHAR, 0, IRT_OCL_CREATE_KERNEL, everyone, NULL);
				
				MPI_Probe(0, IRT_OCL_CREATE_KERNEL, everyone, &status);
				MPI_Get_count(&status, MPI_CHAR, &len);
				char* kernel_name = malloc(sizeof(char) * len);
				MPI_Recv(&kernel_name[0], len, MPI_CHAR, 0, IRT_OCL_CREATE_KERNEL, everyone, NULL);
				
				MPI_Probe(0, IRT_OCL_CREATE_KERNEL, everyone, &status);
				MPI_Get_count(&status, MPI_CHAR, &len);
				char* build_options = malloc(sizeof(char) * len);
				MPI_Recv(&build_options[0], len, MPI_CHAR, 0, IRT_OCL_CREATE_KERNEL, everyone, NULL);
				
				irt_ocl_create_kernel_flag flag;
				MPI_Recv(&flag, 8, MPI_BYTE, 0, IRT_OCL_CREATE_KERNEL, everyone, NULL);

				irt_ocl_kernel* kernel = (irt_ocl_kernel*)malloc(sizeof(irt_ocl_kernel));
				irt_ocl_create_kernel(irt_ocl_get_device(dev_id), kernel, file_name, kernel_name, build_options, flag);

				MPI_Send((cl_ulong[]){(cl_ulong)kernel}, 8, MPI_BYTE, 0, IRT_OCL_CREATE_KERNEL, everyone);
				free(file_name);
				free(kernel_name);
				free(build_options);
				break;
			}

			case IRT_OCL_RUN_KERNEL: {
				cl_ulong kernel_64bit;
                                MPI_Recv(&kernel_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
				irt_ocl_kernel* kernel = (irt_ocl_kernel*)kernel_64bit;
				irt_ocl_local_kernel* lkernel = (irt_ocl_local_kernel*)(kernel->kernel_add);
				//printf("Running Remote Opencl Kernel in \"%s\"\n", lkernel->local_dev->name);
				
				cl_uint work_dim;
                                MPI_Recv(&work_dim, 4, MPI_BYTE, 0, tag, everyone, NULL);

				cl_ulong* array_64bit = (cl_ulong*)malloc(sizeof(cl_ulong) * work_dim);
                                
				MPI_Recv(array_64bit, 8 * work_dim, MPI_BYTE, 0, tag, everyone, NULL);
				size_t* global_work_offset = (size_t*)malloc(sizeof(size_t) * work_dim);
				for (cl_uint i = 0; i < work_dim; ++i)
					global_work_offset[i] = (size_t)array_64bit[i];
                                
				MPI_Recv(array_64bit, 8 * work_dim, MPI_BYTE, 0, tag, everyone, NULL);
				size_t* global_work_size = (size_t*)malloc(sizeof(size_t) * work_dim);
				for (cl_uint i = 0; i < work_dim; ++i)
					global_work_size[i] = (size_t)array_64bit[i];
                                
				MPI_Recv(array_64bit, 8 * work_dim, MPI_BYTE, 0, tag, everyone, NULL);
				size_t* local_work_size = (size_t*)malloc(sizeof(size_t) * work_dim);
				for (cl_uint i = 0; i < work_dim; ++i)
					local_work_size[i] = (size_t)array_64bit[i];
 
				free(array_64bit);

				cl_uint num_args;
                                MPI_Recv(&num_args, 4, MPI_BYTE, 0, tag, everyone, NULL);
				for(cl_uint i = 0; i < lkernel->num_args; ++i) 
					free(lkernel->args[i]);
				free(lkernel->args);
				lkernel->args = malloc(sizeof(char*) * num_args);

				for (cl_uint i = 0; i < num_args; ++i) {
					cl_ulong size_64bit;
					MPI_Recv(&size_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
					size_t arg_size = (size_t)size_64bit;

					if (arg_size == 0) {
						void* tmp = malloc(sizeof(cl_ulong)); // point to a buffer present in the worker machine.
						MPI_Recv(tmp, 8, MPI_BYTE, 0, tag, everyone, NULL);
						cl_int err_code = clSetKernelArg(lkernel->cl_ker, i, sizeof(cl_mem), (void*) &(((irt_ocl_local_buffer*)(((irt_ocl_buffer*)*(cl_ulong*)tmp)->buffer_add))->mem));
						IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error setting kernel arguments");
						lkernel->args[i] = (char*)tmp;
					} else {
						void* tmp = malloc(arg_size);
						MPI_Recv(tmp, arg_size, MPI_BYTE, 0, tag, everyone, NULL);
						cl_int err_code = clSetKernelArg(lkernel->cl_ker, i, arg_size, tmp);
						IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error setting kernel arguments");
						lkernel->args[i] = (char*)tmp;
					}
				}
				
				cl_int err_code = clEnqueueNDRangeKernel((lkernel->local_dev)->queue,
                                                lkernel->cl_ker,
                                                work_dim,
                                                global_work_offset,
                                                global_work_size,
                                                local_work_size,
                                                0, NULL, NULL); // FIXME change to make event possible
				IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error enqueuing NDRange Kernel");

				free(global_work_offset);
				free(global_work_size);
				free(local_work_size);
				break;
			}

			case IRT_OCL_RELEASE_KERNEL: {
				cl_ulong kernel_64bit;
				MPI_Recv(&kernel_64bit, 8, MPI_BYTE, 0, IRT_OCL_RELEASE_KERNEL, everyone, NULL);
				irt_ocl_release_kernel((irt_ocl_kernel*)kernel_64bit);
				break;
			}

			case IRT_OCL_CREATE_BUFFER: {
				cl_uint dev_id;
				MPI_Recv(&dev_id, 4, MPI_BYTE, 0, tag, everyone, NULL);
				
				irt_ocl_mem_flag flag;
				MPI_Recv(&flag, 8, MPI_BYTE, 0, tag, everyone, NULL);
				
				cl_ulong size_64bit;
				MPI_Recv(&size_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
				
				irt_ocl_buffer* buffer = irt_ocl_create_buffer(irt_ocl_get_device(dev_id), flag, (size_t)size_64bit);
				MPI_Send((cl_ulong[]){(cl_ulong)buffer}, 8, MPI_BYTE, 0, tag, everyone);
				break;
			}

			case IRT_OCL_WRITE_BUFFER: {
				cl_ulong buffer_64bit;
				MPI_Recv(&buffer_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
                                
				irt_ocl_blocking_flag blocking;
                                MPI_Recv(&blocking, 4, MPI_BYTE, 0, tag, everyone, NULL);

				cl_ulong offset_64bit;
                                MPI_Recv(&offset_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
                                size_t offset = (size_t)offset_64bit;

				cl_ulong size_64bit;
                                MPI_Recv(&size_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
                                size_t size = (size_t)size_64bit;
                                
				void* source_ptr = malloc(size);
                                MPI_Recv(source_ptr, size, MPI_BYTE, 0, tag, everyone, NULL);

				irt_ocl_write_buffer((irt_ocl_buffer*)buffer_64bit, blocking, offset, size, source_ptr);
				break;
			}
	
			case IRT_OCL_READ_BUFFER: {
				cl_ulong buffer_64bit;
				MPI_Recv(&buffer_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
                                
				irt_ocl_blocking_flag blocking;
                                MPI_Recv(&blocking, 4, MPI_BYTE, 0, tag, everyone, NULL);

				cl_ulong offset_64bit;
                                MPI_Recv(&offset_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
                                size_t offset = (size_t)offset_64bit;

				cl_ulong size_64bit;
                                MPI_Recv(&size_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
                                size_t size = (size_t)size_64bit;
				                                
				void* source_ptr = malloc(size);
				irt_ocl_read_buffer((irt_ocl_buffer*)buffer_64bit, blocking, offset, size, source_ptr);
				
				MPI_Send(source_ptr, size, MPI_BYTE, 0, tag, everyone);
				break;
			}

			case IRT_OCL_RELEASE_BUFFER: {
				cl_ulong buffer_64bit;
				MPI_Recv(&buffer_64bit, 8, MPI_BYTE, 0, tag, everyone, NULL);
				irt_ocl_release_buffer((irt_ocl_buffer*)buffer_64bit);
				break;
			}

			case IRT_OCL_PRINT_DEV_SHORT: {
				cl_uint dev_id;
				MPI_Recv(&dev_id, 4, MPI_BYTE, 0, IRT_OCL_PRINT_DEV_SHORT, everyone, &status);
				irt_ocl_print_device_short_info(irt_ocl_get_device(dev_id));
				sleep(1); // FIXME in windows is different (milliseconds)
				MPI_Send(NULL, 0, MPI_BYTE, 0, IRT_OCL_PRINT_DEV_SHORT, everyone);
				break;
			}
			
			case IRT_OCL_PRINT_DEV_INFOS: {
				cl_uint dev_id;
				MPI_Recv(&dev_id, 4, MPI_BYTE, 0, IRT_OCL_PRINT_DEV_INFOS, everyone, &status);
				irt_ocl_print_device_infos(irt_ocl_get_device(dev_id));
				sleep(1); // FIXME in windows is different (milliseconds)
				MPI_Send(NULL, 0, MPI_BYTE, 0, IRT_OCL_PRINT_DEV_INFOS, everyone);
				break;
			}
		}
	}

exit_label:
	MPI_Finalize(); 
	return 0; 
}
