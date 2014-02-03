#pragma once
/*
*  OpenCLKernels.hpp
*  
*
*  Created by Antonio García Martín on 09/07/12.
*  Copyright 2012 __MyCompanyName__. All rights reserved.
*
*/
#include "CL/cl.h"
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

extern std::string kernelPath;
extern int device_use;

static char* load_program_source(const char *filename) {

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
}


struct theKernels {
	cl_kernel kernel_list[50];
	cl_program program_list[50]; 
	cl_context GPUContext_K;
	cl_device_id cdDevice_K;
	cl_mem  c_FilterBank;
	cl_mem constant_kernel;
	theKernels(cl_context GPUContext, cl_device_id cdDevice){
		GPUContext_K = GPUContext;
		cdDevice_K   = cdDevice;
		if(device_use)
		{
			createKernel("pairwiseDistanceKernel",(kernelPath+"E_PairwiseDistance.cl").c_str(),0);
		}
		else
			createKernel("pairwiseDistanceKernel",(kernelPath+"CPU_PairwiseDistance.cl").c_str(),0);
		createKernel("argminKernel",(kernelPath+"argminKernel.cl").c_str(),1);
		createKernel("argmaxKernel",(kernelPath+"argmaxKernel.cl").c_str(),2);
		createKernel("minKernel",(kernelPath+"minKernel.cl").c_str(),3);
		createKernel("maxKernel",(kernelPath+"maxKernel.cl").c_str(),4);
		if(device_use)
			createKernel("blockwise_distance_kernel",(kernelPath+"E_blockwise_distance_kernel.cl").c_str(),5);
		else
			createKernel("blockwise_distance_kernel",(kernelPath+"CPU_blockwise_distance_kernel.cl").c_str(),5);
		createKernel("blockwise_filter_kernel",(kernelPath+"blockwise_filter_kernel.cl").c_str(),6);
		createKernel("cell_histogram_kernel",(kernelPath+"cell_histogram_kernel.cl").c_str(),7);
		createKernel("cellHistogramKernel1",(kernelPath+"cellHistogramKernel1.cl").c_str(),8);
		createKernel("cellHistogramKernel2",(kernelPath+"cellHistogramKernel2.cl").c_str(),9);
		createKernel("cellHistogramKernel3",(kernelPath+"cellHistogramKernel3.cl").c_str(),10);
	}

	void createKernel(const char* kernel, const char* path, int indice){

		//	TheContext* tc = new TheContext();

		//	cl_context GPUContext_K = tc->getMyContext()->getContextCL();
		//	cl_device_id cdDevice_K = tc->getMyContext()->getDeviceCL();

		// Creates the program
		// Uses NVIDIA helper functions to get the code string and it's size (in bytes)
		//size_t src_size = 0;

        char full_path[256];

#ifdef _VIVID_STATIC_LIB
        sprintf(full_path, "%s", path);
#else
        sprintf(full_path, "%s", path);
#endif

		char *program_source = load_program_source(full_path);
		if (program_source == NULL) {
			printf("Error: Failed to read the OpenCL kernel: %s\n",path);
			exit(-1);
		}
		cl_int err;

		program_list[indice] = clCreateProgramWithSource(GPUContext_K, 1, (const char **) &program_source, NULL, &err);
		if (!program_list[indice]) {
			printf("Error: Failed to create compute program for device %d Kernel: (%s)!\n", indice,kernel);
			printf("************\n%s\n************\n", program_source);
		}

		// Build the program executable
		const char * options = "-cl-fast-relaxed-math";
		err = clBuildProgram(program_list[indice], 0, NULL, options, NULL, NULL);
		if (err != CL_SUCCESS) {
			size_t len;
			char buffer[10000];

			printf("Error: Failed to build program executable for device %d kernel: (%s)!\n",err,kernel);
			cl_int get_err=clGetProgramBuildInfo(program_list[indice], cdDevice_K, CL_PROGRAM_BUILD_LOG, sizeof (buffer), buffer, &len);
			printf("%d %s\n", get_err, buffer);

		}

		kernel_list[indice] = clCreateKernel(program_list[indice], kernel, &err);
		if (!kernel_list[indice] || err != CL_SUCCESS) {
			printf("Error: Failed to create compute kernel for device %d Kernel: (%s)!\n", indice, full_path);
			exit(1);
		}
	}
};

class MyKernels{
public:
	static theKernels*  My_Kernels;
	static theKernels*  My_Kernels_TMP;
	MyKernels(cl_context GPUContext_K1,
		cl_device_id cdDevice_K1);

	MyKernels(cl_context GPUContext_K1,
		cl_device_id cdDevice_K1,int cpu);

	void MyKernelsOff(){
		My_Kernels=NULL;
	}

	theKernels * getMyKernels(){
		return My_Kernels;
	}

	cl_kernel getPairwiseDistanceKernel(){
		return My_Kernels->kernel_list[0];
	}

	cl_kernel getArgminKernel(){
		return My_Kernels->kernel_list[1];
	}

	cl_kernel getArgmaxKernel(){
		return My_Kernels->kernel_list[2];
	}

	cl_kernel getMinKernel(){
		return My_Kernels->kernel_list[3];
	}

	cl_kernel getMaxKernel(){
		return My_Kernels->kernel_list[4];
	}

	cl_kernel getBlockWiseDistanceKernel(){
		return My_Kernels->kernel_list[5];
	}

	cl_kernel getBlockWiseFilterKernel(){
		return My_Kernels->kernel_list[6];
	}

	cl_kernel getCellHistogramKernel(){
		return My_Kernels->kernel_list[7];
	}

	cl_kernel getCellHistogramKernel1(){
		return My_Kernels->kernel_list[8];
	}

	cl_kernel getCellHistogramKernel2(){
		return My_Kernels->kernel_list[9];
	}

	cl_kernel getCellHistogramKernel3(){
	//	printf("ping\n");
		return My_Kernels->kernel_list[10];
	}
	/*
	cl_kernel getDoConvolution0(){
		return My_Kernels->kernel_list[10];
	}

	cl_kernel getDoConvolution1(){
		return My_Kernels->kernel_list[11];
	}

	cl_kernel getDoConvolution2_8(){
		return My_Kernels->kernel_list[12];
	}
	cl_kernel getDoConvolution2_10(){
		return My_Kernels->kernel_list[13];
	}

	cl_kernel getDoConvolution2_12(){
		return My_Kernels->kernel_list[14];
	}
	cl_kernel getDoConvolution2_14(){
		return My_Kernels->kernel_list[15];
	}
	cl_kernel getDoConvolution3(){
		return My_Kernels->kernel_list[16];
	}

	cl_kernel getDoConvolution3_7(){
		return My_Kernels->kernel_list[17];
	}
	cl_kernel getDoConvolution3_9(){
		return My_Kernels->kernel_list[18];

	}
	cl_kernel getDoConvolution3_11(){
		return My_Kernels->kernel_list[19];
	}

	cl_kernel getDoConvolution4(){
		return My_Kernels->kernel_list[20];
	}
	cl_kernel getDoConvolution4_7(){
		return My_Kernels->kernel_list[21];
	}
	cl_kernel getDoConvolution4_9(){
		return My_Kernels->kernel_list[22];
	}
	cl_kernel getDoConvolution4_11(){
		return My_Kernels->kernel_list[23];
	}
	cl_kernel getDoConvolution4_13(){
		return My_Kernels->kernel_list[24];
	}
	cl_kernel getDoConvolution4_15(){
		return My_Kernels->kernel_list[25];
	}

	cl_kernel getDoConvolution5(){
		return My_Kernels->kernel_list[26];
	}

	cl_kernel getDoConvolution5_7(){
		return My_Kernels->kernel_list[27];
	}

	cl_kernel getDoConvolution5_9(){
		return My_Kernels->kernel_list[28];
	}

	cl_kernel getDoConvolution5_11(){
		return My_Kernels->kernel_list[29];
	}

	cl_kernel getDoConvolutionComplexT0(){
		return My_Kernels->kernel_list[30];
	}

	cl_kernel getDoConvolutionComplexT1_5(){
		return My_Kernels->kernel_list[31];
	}
	cl_kernel getDoConvolutionComplexT1_7(){
		return My_Kernels->kernel_list[32];
	}

	cl_kernel getDoConvolutionComplexT1_9(){
		return My_Kernels->kernel_list[33];
	}

	cl_kernel getDoConvolutionComplexT1_11(){
		return My_Kernels->kernel_list[34];
	}
	*/
	~MyKernels(){};
};



class MyKernels_CPU{
public:
	static theKernels*  My_Kernels;
	MyKernels_CPU(cl_context GPUContext_K1,
		cl_device_id cdDevice_K1);

	theKernels * getMyKernels(){
		return My_Kernels;
	}

	cl_kernel getPairwiseDistanceKernel(){
		return My_Kernels->kernel_list[0];
	}

	cl_kernel getArgminKernel(){
		return My_Kernels->kernel_list[1];
	}

	cl_kernel getArgmaxKernel(){
		return My_Kernels->kernel_list[2];
	}

	cl_kernel getMinKernel(){
		return My_Kernels->kernel_list[3];
	}

	cl_kernel getMaxKernel(){
		return My_Kernels->kernel_list[4];
	}

	cl_kernel getBlockWiseDistanceKernel(){
		return My_Kernels->kernel_list[5];
	}

	cl_kernel getBlockWiseFilterKernel(){
		return My_Kernels->kernel_list[6];
	}

	cl_kernel getCellHistogramKernel(){
		return My_Kernels->kernel_list[7];
	}

	cl_kernel getCellHistogramKernel1(){
		return My_Kernels->kernel_list[8];
	}

	cl_kernel getCellHistogramKernel2(){
		return My_Kernels->kernel_list[9];
	}

	/*
	cl_kernel getDoConvolution0(){
		return My_Kernels->kernel_list[10];
	}

	cl_kernel getDoConvolution1(){
		return My_Kernels->kernel_list[11];
	}

	cl_kernel getDoConvolution2_8(){
		return My_Kernels->kernel_list[12];
	}
	cl_kernel getDoConvolution2_10(){
		return My_Kernels->kernel_list[13];
	}

	cl_kernel getDoConvolution2_12(){
		return My_Kernels->kernel_list[14];
	}
	cl_kernel getDoConvolution2_14(){
		return My_Kernels->kernel_list[15];
	}
	cl_kernel getDoConvolution3(){
		return My_Kernels->kernel_list[16];
	}

	cl_kernel getDoConvolution3_7(){
		return My_Kernels->kernel_list[17];
	}
	cl_kernel getDoConvolution3_9(){
		return My_Kernels->kernel_list[18];

	}
	cl_kernel getDoConvolution3_11(){
		return My_Kernels->kernel_list[19];
	}

	cl_kernel getDoConvolution4(){
		return My_Kernels->kernel_list[20];
	}
	cl_kernel getDoConvolution4_7(){
		return My_Kernels->kernel_list[21];
	}
	cl_kernel getDoConvolution4_9(){
		return My_Kernels->kernel_list[22];
	}
	cl_kernel getDoConvolution4_11(){
		return My_Kernels->kernel_list[23];
	}
	cl_kernel getDoConvolution4_13(){
		return My_Kernels->kernel_list[24];
	}
	cl_kernel getDoConvolution4_15(){
		return My_Kernels->kernel_list[25];
	}

	cl_kernel getDoConvolution5(){
		return My_Kernels->kernel_list[26];
	}

	cl_kernel getDoConvolution5_7(){
		return My_Kernels->kernel_list[27];
	}

	cl_kernel getDoConvolution5_9(){
		return My_Kernels->kernel_list[28];
	}

	cl_kernel getDoConvolution5_11(){
		return My_Kernels->kernel_list[29];
	}

	cl_kernel getDoConvolutionComplexT0(){
		return My_Kernels->kernel_list[30];
	}

	cl_kernel getDoConvolutionComplexT1_5(){
		return My_Kernels->kernel_list[31];
	}
	cl_kernel getDoConvolutionComplexT1_7(){
		return My_Kernels->kernel_list[32];
	}

	cl_kernel getDoConvolutionComplexT1_9(){
		return My_Kernels->kernel_list[33];
	}

	cl_kernel getDoConvolutionComplexT1_11(){
		return My_Kernels->kernel_list[34];
	}
	*/
	~MyKernels_CPU(){};
};
