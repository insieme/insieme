#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdbool.h>
#include "CL/cl.h"
#include "vecops.hh"

#define bench 0
#define version 1

#if bench
	#include "cltime.h"
#endif

char buffer[8192];

cl_program load_kernel(const char *path, cl_context context)
{
	char *mm;
	struct stat st;
	int fd;
	int err = 0;
	cl_program p;
	
	fd = open(path, O_RDONLY);
	if(fd < 0)
		return NULL;

	memset(&st, 0, sizeof(struct stat));
	fstat(fd, &st);

	mm = (char *)mmap(0, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

	#pragma insieme kernelFile "vecops.cl"
	p = clCreateProgramWithSource(context, 1, (const char **)&mm, NULL, &err);
	if(!p){
		fprintf(stderr, "clCreateProgramWithSource() failed!\n");
		fprintf(stderr, "err: %i\n", err);
		munmap(mm, st.st_size);
		return NULL;
	}

	close(fd);
	munmap(mm, st.st_size);

	return p;
}

void gen_data(struct svm_node* node, size_t length, dtype val, int scale)
{
	for(size_t i = 0; i < length; ++i)
	{
		node->value = val;
		node->index = (int)i*scale;
		++node;
	}
	node->index = -1;
}

bool validate(struct svm_node* pxs, struct svm_node* pys, dtype* result, size_t x, size_t y)
{
	bool flag = true;
	for(unsigned i = 0; i < y; ++i)
	{
		struct svm_node* py = pys + i*(x+1);
		struct svm_node* px = pxs;
		dtype sum = 0;
		while(px->index != -1 && py->index != -1)
		{
			if(px->index == py->index)
			{
				sum += px->value * py->value;
				++px;
				++py;
			}
			else
			{
				if(px->index > py->index)
					++py;
				else
					++px;
			}			
		}
		if(sum != result[i])
		{
			printf("!!!Error in result %u: %f vs %f!!!\n", i, sum, result[i]);
			flag = false;
		}
	}
	return flag;
}

void unpack(struct svm_node* px, dtype* px_u)
{
	while(px->index >= 0)
	{
		px_u[px->index] = px->value;
		++px;
	}
}

#pragma insieme mark
int main(int argc, const char** argv)
{
	size_t x = 512, y = 250000; //y has to be a multiple of ciDeviceCount!
	struct svm_node* px = (struct svm_node*)malloc((x+1)*sizeof(struct svm_node));
	gen_data(px, x, 1, 3);
	struct svm_node* py = (struct svm_node*)malloc((x+1)*y*sizeof(struct svm_node));
	for(size_t i = 0; i < y; ++i) {
		struct svm_node* tmp = py+i*(x+1);
		gen_data(tmp, x, 3,2);
	}
	dtype* result = (dtype*)malloc(y*sizeof(dtype));
	int* pyLength = (int*)malloc(y*sizeof(int));
	
	for(size_t i = 0; i < y; ++i)
	{
		for(size_t j = 0; py[i*(x+1)+j].index >= 0; ++j)
			pyLength[i] = py[i*(x+1)+j].index;
		++pyLength[i];
	}
	
	cl_int err = CL_SUCCESS;
//	cl_platform_id platform = NULL;
//	cl_uint ciDeviceCount = 0;
//	cl_device_id *device = NULL;

	// retrieve devices
	cl_platform_id platform;
	err = clGetPlatformIDs(1, &platform, NULL);
	cl_device_id device;
	err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_CPU, 1, &device, NULL);

	size_t localDim  = 256l;
	size_t globalDim = localDim*y;
/*	
	device = (cl_device_id *)malloc(ciDeviceCount * sizeof(cl_device_id) );
	err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, ciDeviceCount, device, NULL);
	if (err != CL_SUCCESS)
	{
		printf("Failed to get devices:\n%s\n", oclErrorString(err));
		return -1;
	}
	*/
	//Create the context
	cl_context context1 = clCreateContext(0, 1, &device, NULL, NULL, &err);
	if(err != CL_SUCCESS)
	{
		printf("Context creation failed:\n%d\n", err);
		return -1;
	}										 

	// create a command queue for first device the context reported
	cl_command_queue queue = clCreateCommandQueue(context1, device, 0, 0);
	
	// load program from disk
	char *tmp = strdup(argv[0]);
	char* my_dir = dirname(tmp);

//	size_t program_length;
	char path[256];
  	snprintf(path, PATH_MAX - 1, "%s/vecops.cl", my_dir);
 
	cl_program vecops = load_kernel(path, context1);

	if(err != CL_SUCCESS)
	{
		printf("Program creation failed:\n%d\n", (err));
		return -1;
	}
 
	err = clBuildProgram(vecops, 0, NULL, "-I.", NULL, NULL);
	if(err != CL_SUCCESS)
	{
			err = clGetProgramBuildInfo(vecops, device, CL_PROGRAM_BUILD_LOG, 8192, buffer, NULL);
			if(err != CL_SUCCESS)
				printf("Cannot get build info: %d\n", (err));

			printf("Build log:\n%s\n", buffer);
	}
	
	// create kernel
	cl_kernel sparsedot_kernel;
	
#if version == 1
	sparsedot_kernel = clCreateKernel(vecops, "sparsedot1_kernel", &err);
#endif
#if version == 2
	sparsedot_kernel = clCreateKernel(vecops, "sparsedot4_kernel", &err);
#endif
#if version == 3
	sparsedot_kernel = clCreateKernel(vecops, "sparsedot3_kernel", &err);
#endif
	if (err != CL_SUCCESS)
	{
		printf("Kernel creation failed:\n%d\n", (err));
		return -1;
	}
	
	 
	// allocate memory on the devices
	cl_mem px_d, py_d, result_d, pyLength_d;
	
#if version == 1
	px_d = clCreateBuffer(context1,
							 CL_MEM_READ_ONLY,
							 (x+1) * sizeof(struct svm_node),
							 0, &err);
#endif
#if version == 2 || version == 3
	//unpack px
	int size = px[x-1].index+1;

	for(size_t i = 0; i < y; ++i)
		size = size > pyLength[i] ? size : pyLength[i];

	dtype* px_u = (dtype*)calloc(size, sizeof(dtype));

	unpack(px, px_u);
	printf("px size: %d\n", size);
#endif
#if version == 3
	size_t height, width;
	clGetDeviceInfo(device, CL_DEVICE_IMAGE2D_MAX_HEIGHT, sizeof(size_t), &height, 0);
	clGetDeviceInfo(Device, CL_DEVICE_IMAGE2D_MAX_WIDTH, sizeof(size_t), &width, 0);

	size_t region[3];
	region[2] = 1;

	region[0] = min(4, size);
	region[1] = (size+2-1) / 4;
		

	cl_image_format px_format;
	px_format.image_channel_order = CL_R;
	px_format.image_channel_data_type = CL_FLOAT;
#endif
#if version == 2
	px_d = clCreateBuffer(context1,
				 CL_MEM_READ_ONLY,
				 size * sizeof(dtype),
				 0, &err);
#endif
#if version == 3
	 px_d = clCreateImage2D(context1, CL_MEM_READ_ONLY, &px_format,
				  region[0], region[1], 0, 0, &err);

#endif
	if(err != CL_SUCCESS)
	{
		printf("Failed to allocate px:\n%d\n", (err));
		return -1;
	}
	py_d = clCreateBuffer(context1,
		 CL_MEM_READ_ONLY,
		 (x+1) * y * sizeof(struct svm_node),
		 0, &err);
	if(err != CL_SUCCESS)
	{
		printf("Failed to allocate px:\n%d\n", (err));
		return -1;
	}
	result_d = clCreateBuffer(context1,
		CL_MEM_WRITE_ONLY,
		y * sizeof(dtype),
		0, 0);
	pyLength_d = clCreateBuffer(context1,
		CL_MEM_READ_ONLY,
		y * sizeof(int),
		0, 0);
	

#if bench
	//start time measurement
	start_timer(0);
#endif

	// copy host vectors to device
	err = CL_SUCCESS;
   
    err |= clEnqueueWriteBuffer(queue, py_d, CL_FALSE, 0, 
								(x+1) * y * sizeof(struct svm_node), py, 0, NULL, NULL);
									

	err |= clEnqueueWriteBuffer(queue, pyLength_d, CL_FALSE, 0, 
								y * sizeof(int), pyLength, 0, NULL, NULL);

#if version == 1
	err |= clEnqueueWriteBuffer(queue, px_d, CL_FALSE, 0, 
								(x+1) * sizeof(struct svm_node), px, 0, NULL, NULL);
#endif
#if version == 2
	err |= clEnqueueWriteBuffer(queue, px_d, CL_FALSE, 0, 
								size * sizeof(dtype), px_u, 0, NULL, NULL);
#endif
#if version == 3
	size_t offset[] = {0,0,0};
	err |= clEnqueueWriteImage(queue, px_d, CL_TRUE, offset, region, sizeof(dtype), 0, 
							   px_u, 0, 0, NULL);
#endif
	clFinish(queue);

	 
	if(err != CL_SUCCESS)
	{
		printf("Data transfer to GPU failed:\n%d\n", (err));
		return -1;
	}

#if bench
	stop_timer(0);
	start_timer(1);
#endif
	// set kernel arguments

	clSetKernelArg(sparsedot_kernel, 0, sizeof(cl_mem), (void *) &px_d);
	clSetKernelArg(sparsedot_kernel, 1, sizeof(cl_mem), (void *) &py_d);
	clSetKernelArg(sparsedot_kernel, 2, sizeof(cl_mem), (void *) &result_d);
	clSetKernelArg(sparsedot_kernel, 3, sizeof(cl_mem), (void *) &pyLength_d);
	clSetKernelArg(sparsedot_kernel, 4, sizeof(cl_ulong), (void *) &x);
	clSetKernelArg(sparsedot_kernel, 5, sizeof(cl_ulong), (void *) &y);
//	clSetKernelArg(sparsedot_kernel, 6, sizeof(cl_float8)*localDim, 0);
#if version == 3
		clSetKernelArg(sparsedot_kernel, 7, sizeof(cl_long), (void *) &region[1]) ;		
		clSetKernelArg(sparsedot_kernel, 8, sizeof(cl_long), (void *) &region[0]) ;		
#endif
	clFlush(queue);

	// start kernel
	err = clEnqueueNDRangeKernel(queue, sparsedot_kernel, 1, 0, &globalDim, &localDim,
					   0, NULL, 0);

	if(err != CL_SUCCESS)
	{
		printf("Kernel launch failed:\n%d\n", (err));
		return -1;
	}

	clFinish(queue);
	
#if bench	
	stop_timer(1);
	start_timer(2);
#endif

	cl_event result_gather;
	 
	// Non-blocking copy of result from device to host
	err = clEnqueueReadBuffer(queue, result_d, CL_FALSE, 0, y * sizeof(dtype), 
						result, 0, NULL, &result_gather);
	
	if(err != CL_SUCCESS)
	{
		printf("Reading result failed:\n%d\n", (err));
		return -1;
	}

	// CPU sync with GPU
	clWaitForEvents(1, &result_gather);

#if bench	
	// stop GPU time measurement
	stop_timer(2);
#endif
	//check result
/*	for(size_t i = 0; i < y; ++i)
	{
		printf("%f ", result[i]);
	}
	printf("\n");
  */  

#if bench
	start_timer(3);
#endif
	bool correct = validate(px, py, result, x, y);
#if bench
	stop_timer(3);
	printf("v%i; x: %lu, y: %lu\n", version, x, y);
	printf("CPU: %f, upcpy: %f DeviceCalc: %f, downcpy: %f\n", 
		   get_secs(3), get_secs(0), get_secs(1), get_secs(2));
#endif
	
	if(correct)
		printf("SUCCESS!\n");
		
	//cleenup

	clReleaseKernel(sparsedot_kernel);
	clReleaseCommandQueue(queue);
	clReleaseEvent(result_gather);
	clReleaseMemObject(px_d);
	clReleaseMemObject(py_d);
	clReleaseMemObject(result_d);
	clReleaseMemObject(pyLength_d);
//	clReleaseDevice(device);

	free(px);
#if version == 2 || version == 3
	free(px_u);
#endif
	free(py);
	free(result);

	return 0;
}
	
	
