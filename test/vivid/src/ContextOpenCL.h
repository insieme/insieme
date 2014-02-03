#pragma once

#include <stdio.h>
#include <iostream>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
//#include <error.h>
#include <CL/cl.h>

using namespace std;

//Get an OpenCL platform
#define NUM_ENTRIES 10

struct myContexOpenCl 
{
	cl_platform_id cpPlatforms[NUM_ENTRIES]; 
	cl_device_id cdDevice; 
	cl_command_queue cqCommandQueue;
	cl_context Context;
	myContexOpenCl(int type)
	{
		cl_uint n_devices;
		cl_uint n_platforms;

		cl_int errorcode;

		if (clGetPlatformIDs(NUM_ENTRIES, cpPlatforms, &n_platforms) != CL_SUCCESS)
		{
			printf("clGetPlatformIDs error\n");
		}

		char platform_vendors[NUM_ENTRIES][256];
		size_t param_value_size_ret;
		printf("OpenCL Platforms: %d\n", n_platforms);
		for (int i = 0; i < n_platforms; i++)
		{
			clGetPlatformInfo(
				cpPlatforms[i],
				CL_PLATFORM_VENDOR,
				256,
				platform_vendors[i],
				&param_value_size_ret);
			std::cout << "Platform " << i + 1 << ": " << platform_vendors[i] << std::endl;
		}

		if(type==CL_DEVICE_TYPE_GPU)
		{
			printf("Getting GPU Device\n");
			for (int i = 0; i < n_platforms; i++)
			{
				if (clGetDeviceIDs(cpPlatforms[i], CL_DEVICE_TYPE_GPU, 1, &cdDevice, NULL) == CL_SUCCESS) 
				{
					std::cout << "Found GPU device on OpelCL platform " << i + 1 << std::endl;
					break;
				}
				else if (i == n_platforms - 1)
				{
					std::cerr << "Cannot find GPU device" << std::endl;
				}
			}
		}
		else
		{
			printf("Getting CPU device\n");
			for (int i = 0; i < n_platforms; i++)
			{
				if (clGetDeviceIDs(cpPlatforms[i], CL_DEVICE_TYPE_CPU, 1, &cdDevice, NULL) == CL_SUCCESS) 
				{
					std::cout << "Found CPU device on OpelCL Platform " << i + 1 << std::endl;
					break;
				}
				else if (i == n_platforms - 1)
				{
					std::cerr << "Cannot find CPU device" << std::endl;
				}
			}
		}

		printf("Making context\n");
		Context = clCreateContext(0, 1, &cdDevice, NULL, NULL, &errorcode); 
		if (Context == NULL) 
		{
			printf("clCreateContextFromType error: %d",errorcode);
			if (errorcode == CL_INVALID_PLATFORM) 
				printf("invalid platform\n");
			if (errorcode == CL_INVALID_VALUE) printf("invalid value\n");
			if (errorcode == CL_DEVICE_NOT_AVAILABLE) 
				printf("device not available\n");
			if (errorcode == CL_DEVICE_NOT_FOUND)
				printf("device not found\n");
			if (errorcode == CL_OUT_OF_HOST_MEMORY)
				printf("out of host memory\n");
			if (errorcode == CL_INVALID_DEVICE_TYPE) 
				printf("invalid device type\n");
			exit(1);
		}

		printf("Making Command Queue\n");
		cqCommandQueue = 
			clCreateCommandQueue(Context, cdDevice, 0, NULL);
		if (cqCommandQueue == NULL) 
		{
			printf("clCreateCommandQueue error\n");
		}
	}

	cl_device_id getDeviceCL(){

		if(cdDevice==NULL){
			if (clGetDeviceIDs(cpPlatforms[0], CL_DEVICE_TYPE_GPU, 1, 
				&cdDevice, NULL) != CL_SUCCESS) {
					printf("clGetDeviceIDs error\n");
			}
		}

		return cdDevice;
	}

	cl_context getContextCL(){
		cl_int errorcode;

		if(Context==NULL){
			Context = clCreateContext(0, 1, &cdDevice, NULL, NULL, &errorcode); 
			if (Context == NULL) 
			{
				printf("clCreateContextFromType error: ");
				if (errorcode == CL_INVALID_PLATFORM) 
					printf("invalid platform\n");
				if (errorcode == CL_INVALID_VALUE) printf("invalid value\n");
				if (errorcode == CL_DEVICE_NOT_AVAILABLE) 
					printf("device not available\n");
				if (errorcode == CL_DEVICE_NOT_FOUND)
					printf("device not found\n");
				if (errorcode == CL_OUT_OF_HOST_MEMORY)
					printf("out of host memory\n");
				if (errorcode == CL_INVALID_DEVICE_TYPE) 
					printf("invalid device type\n");
				exit(1);
			}
		}		
		return Context;
	}
};

class TheContext
{
public:
	static myContexOpenCl* The_Context_GPU;
	static myContexOpenCl* The_Context_CPU;
	static int type_gpu;
	TheContext();
	TheContext(int cpu);
	myContexOpenCl * getMyContext();
	myContexOpenCl * getMyContextCPU();
	void changeContextGPU()
	{
		type_gpu = 1;
	}

	void changeContextCPU()
	{
		type_gpu = 0;
	}

	~TheContext(){};
};

//boost::shared_ptr<myContexOpenCl> myContex  (new myContexOpenCl());
/*

cl_context getContextCL(){
cl_int errorcode;
if (theContextOpenCL.cpPlatform==NULL){
if (clGetPlatformIDs(1, &theContextOpenCL.cpPlatform, NULL) != CL_SUCCESS) {
printf("clGetPlatformIDs error\n");
}

}

if(theContextOpenCL.cdDevice==NULL){
if (clGetDeviceIDs(theContextOpenCL.cpPlatform, CL_DEVICE_TYPE_GPU, 1, 
&theContextOpenCL.cdDevice, NULL) != CL_SUCCESS) {
printf("clGetDeviceIDs error\n");
}
}

if(theContextOpenCL.GPUContext==NULL){
theContextOpenCL.GPUContext = clCreateContext(0, 1, &theContextOpenCL.cdDevice,
NULL, NULL, &errorcode); 
if (theContextOpenCL.GPUContext == NULL) {
printf("clCreateContextFromType error: ");
if (errorcode == CL_INVALID_PLATFORM) 
printf("invalid platform\n");
if (errorcode == CL_INVALID_VALUE) printf("invalid value\n");
if (errorcode == CL_DEVICE_NOT_AVAILABLE) 
printf("device not available\n");
if (errorcode == CL_DEVICE_NOT_FOUND)
printf("device not found\n");
if (errorcode == CL_OUT_OF_HOST_MEMORY)
printf("out of host memory\n");
if (errorcode == CL_INVALID_DEVICE_TYPE) 
printf("invalid device type\n");
exit(1);
}
}

if(theContextOpenCL.cqCommandQueue==NULL){
theContextOpenCL.cqCommandQueue = 
clCreateCommandQueue(theContextOpenCL.GPUContext, theContextOpenCL.cdDevice, 0, NULL);
if (theContextOpenCL.cqCommandQueue == NULL) {
printf("clCreateCommandQueue error\n");
}
}

return theContextOpenCL.GPUContext;

}


cl_device_id getDeviceCL(){

if(theContextOpenCL.cdDevice==NULL){
if (clGetDeviceIDs(theContextOpenCL.cpPlatform, CL_DEVICE_TYPE_GPU, 1, 
&theContextOpenCL.cdDevice, NULL) != CL_SUCCESS) {
printf("clGetDeviceIDs error\n");
}
}

return theContextOpenCL.cdDevice;

}
*/



/*static char * load_program_source(const char *filename) {

struct stat statbuf;
FILE *fh;
char *source;

fh = fopen(filename, "r");
if (fh == 0)
return 0;

stat(filename, &statbuf);
source = (char *) malloc(statbuf.st_size + 1);
fread(source, statbuf.st_size, 1, fh);
source[statbuf.st_size] = '\0';

return source;
}*/


