/*
 *  ContextOpenCl.cpp
 *  
 *
 *  Created by Antonio García Martín on 30/06/12.
 *  Copyright 2012 __MyCompanyName__. All rights reserved.
 *
 */

#include "ContextOpenCL.h"
extern int device_use;

myContexOpenCl *  TheContext::The_Context_GPU=NULL;
myContexOpenCl *  TheContext::The_Context_CPU=NULL;
int TheContext::type_gpu;

TheContext::TheContext()
{
	type_gpu = device_use;
	if (The_Context_GPU==NULL && type_gpu==1){
		The_Context_GPU = new myContexOpenCl( CL_DEVICE_TYPE_GPU );
		printf("\nGPU context done\n\n");
	}
	
	if (The_Context_CPU==NULL && type_gpu==0){
		 The_Context_CPU = new myContexOpenCl( CL_DEVICE_TYPE_CPU );
		 printf("\nCPU context done\n\n");
	}
}

TheContext::TheContext(int cpu)
{
	if (The_Context_GPU==NULL && cpu==0){
		The_Context_GPU = new myContexOpenCl( CL_DEVICE_TYPE_GPU );
		printf("\nGPU context done\n\n");
		type_gpu = 1;
	}
	
	if (The_Context_CPU==NULL && cpu==1){
		 The_Context_CPU= new myContexOpenCl( CL_DEVICE_TYPE_CPU );
		 printf("\nCPU context done\n\n");
		 type_gpu = 0;
	}
}

myContexOpenCl * TheContext::getMyContext()
{
	if(type_gpu == 0)
		return The_Context_CPU;
	else
		return The_Context_GPU;
}

myContexOpenCl * TheContext::getMyContextCPU()
{
	return  The_Context_CPU;
}
