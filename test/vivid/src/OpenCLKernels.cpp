/*
 *  ContextOpenCl.cpp
 *  
 *
 *  Created by Antonio García Martín on 30/06/12.
 *  Copyright 2012 __MyCompanyName__. All rights reserved.
 *
 */

#include "OpenCLKernels.hpp"

theKernels *  MyKernels::My_Kernels=NULL;
theKernels *  MyKernels::My_Kernels_TMP=NULL;
MyKernels::MyKernels(cl_context GPUContext1,
        cl_device_id cdDevice1){
    //	printf("Te Context()");
    if (My_Kernels==NULL){
        //	printf("Is NULL");
        My_Kernels = new theKernels(GPUContext1,cdDevice1);
    }
}

MyKernels::MyKernels(cl_context Context1,
        cl_device_id cdDevice1,int cpu){
    //	printf("Te Context()");
    if (My_Kernels==NULL){
        //	printf("Is NULL");
        My_Kernels = new theKernels(Context1,cdDevice1);
	}else{
		if(My_Kernels_TMP==NULL){
			My_Kernels_TMP = My_Kernels;
			My_Kernels = new theKernels(Context1,cdDevice1);
		}else{
			theKernels *  tmp = My_Kernels_TMP;
			My_Kernels_TMP = My_Kernels;
			My_Kernels=tmp;
		}
	}
}

theKernels *  MyKernels_CPU::My_Kernels=NULL;

MyKernels_CPU::MyKernels_CPU(cl_context GPUContext1,
        cl_device_id cdDevice1){
    //	printf("Te Context()");
    if (My_Kernels==NULL){
        //	printf("Is NULL");
        My_Kernels = new theKernels(GPUContext1,cdDevice1);
    }
}

