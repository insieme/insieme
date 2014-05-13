#ifndef _LIB_ICL_EXT_
#define _LIB_ICL_EXT_

#include "lib_icl.h"
#include <stdio.h>
#include <math.h>

// Note(Biagio): just found a problem with windows alloca, it rasies a runtime error 
// http://msdn.microsoft.com/en-us/library/5471dc8s.aspx	
// the following code fixes the problem

#ifdef INSIEME
	#define ALLOCA malloc

//	#include <clang/Basic/Builtins.h>
#else
#ifdef _WIN32
	#include <alloca.h>
	#define ALLOCA alloca
#else
	#include <malloc.h>
	#define ALLOCA malloc

	// VS doen't define isnan(), this is a fix:
	#define isnan(x) ((x) != (x))
#endif
#endif


//#pragma warning ( disable : C4996 ) 

enum icl_datatype { icl_FLOAT, icl_INT, icl_DOUBLE };

typedef struct {
	size_t size;
	size_t groupSize;	
	cl_device_type deviceType;         // available device type: CL_DEVICE_TYPE_CPU, CL_DEVICE_TYPE_GPU, CL_DEVICE_TYPE_ACCELERATOR, CL_DEVICE_TYPE_ALL
	int deviceId;   	
	unsigned iter;          // iterations 	
	bool checkResults;      // results are check against a serial implementation or a precomputed solution	
	bool hostTimer;	

	// available, but not used yet, or not used everywhere
	bool deviceTimer;
	enum icl_datatype dataType;  // kernel version by data type
	unsigned kernel;        // kernel version by implementation version
} icl_arguments;

icl_arguments default_arguments = { 
	1048576, 128, 
	CL_DEVICE_TYPE_DEFAULT,	0, 
	16, true, true,
	false, icl_FLOAT, 0
};

// nan checks
void icl_isnan_float_hbuffer(const float *hostbuf, size_t bsize);
void icl_isnan_float_dbuffer(const icl_buffer *clbuf, size_t bsize);

// print an array of host buffer
void icl_out_float_hbuffer(const float *hostbuf, size_t bsize);

// print an array of device buffer
void icl_out_float_dbuffer(const icl_buffer *clbuf, size_t bsize);
void icl_out_uint_dbuffer(const icl_buffer *clbuf, size_t bsize);
void icl_in_float_dbuffer(float *buffer, icl_buffer *clbuf, size_t bsize);
void icl_in_uint_dbuffer(unsigned int *buffer, icl_buffer *clbuf, size_t bsize);

// argument parsing
icl_arguments icl_parse_argument(int argc, const char** argv);
void icl_print_arguments(const icl_arguments *a);
void icl_print_arguments_ext(const icl_arguments *a);
void icl_prompt();

////////////////////////////////////////////////////////////////////////////////////////////


/* command line argumtens parsing
	-datatype={int,float,double}
	-groupsize=<x>
	-size={128,256,...}	
	-devicetype={cpu,gpu,acc,all}
	-kernel=<x>                     version of the kernel (if more versions are available)
	-iter=<x>                       # of iterations
	-timer host
	-timer device
	-check
*/

icl_arguments icl_parse_argument(int argc, const char** argv)
{
	// init with default values
	icl_arguments ret = default_arguments;	

	for(int i = 1; i < argc; i++) { // usually argv[0] is the program name
		// params requiring an extra value
		if(i + 1 != argc) { 
			if(!strcmp(argv[i], "-groupsize")) {
				ret.groupSize = (size_t) (atoi(argv[i + 1]));
			}
			if(!strcmp(argv[i], "-datatype")) {				
				if(!strcmp(argv[i+1], "float"))		ret.dataType = icl_FLOAT;
				if(!strcmp(argv[i+1], "double"))	ret.dataType = icl_DOUBLE;
				if(!strcmp(argv[i+1], "int"))		ret.dataType = icl_INT;					
            } 
			if(!strcmp(argv[i], "-size")) {
				ret.size = (size_t) (atoi(argv[i + 1]));			
            } 
			if(!strcmp(argv[i], "-devicetype")) {
				if(!strcmp(argv[i+1], "cpu"))	ret.deviceType = CL_DEVICE_TYPE_CPU;
				if(!strcmp(argv[i+1], "gpu"))	ret.deviceType = CL_DEVICE_TYPE_GPU;
				if(!strcmp(argv[i+1], "acc"))	ret.deviceType = CL_DEVICE_TYPE_ACCELERATOR;
				if(!strcmp(argv[i+1], "all"))	ret.deviceType = CL_DEVICE_TYPE_ALL;					
            } 
			if(!strcmp(argv[i], "-timer")) {
				if(!strcmp(argv[i+1], "device"))	ret.deviceTimer = true; // also h
				if(!strcmp(argv[i+1], "host"))		ret.hostTimer = true;	// also d			
			}
			if(!strcmp(argv[i], "-kernel")) {
				ret.kernel = atoi(argv[i+1]);			
			}
			if(!strcmp(argv[i], "-iter")) {
				ret.iter = atoi(argv[i+1]);			
			}
		}
		// params requiring only one flag
		if(!strcmp(argv[i], "-check"))			ret.checkResults = true;
		
	}

	return ret;
}


///

void icl_isnan_float_hbuffer(const float *hostbuf, size_t bsize){
	size_t i;
	for(i=0; i< bsize; i++){
		if(isnan(hostbuf[i])) printf("WARNING: NAN found at index %zu\n", i);		
		//if(_isinf(hostbuf[i])) printf("WARNING: INF found at index %d ", i);		
	}
}

void icl_isnan_float_dbuffer(const icl_buffer *clbuf, size_t bsize){
	float *buffer = (float*)ALLOCA(sizeof(float)*bsize);
	icl_read_buffer(clbuf,  CL_TRUE, sizeof(float) * bsize, buffer, NULL, NULL); 	
	icl_isnan_float_hbuffer(buffer, bsize);
}

///

void icl_out_float_hbuffer(const float *hostbuf, size_t bsize){
	size_t i;
	for(i=0; i< bsize; i++){
		printf("%f ", hostbuf[i]);		
	}
	printf("\n");
}

void icl_out_float_dbuffer(const icl_buffer *clbuf, size_t bsize){
	float *buffer = (float*)ALLOCA(sizeof(float)*bsize);
	icl_read_buffer(clbuf,  CL_TRUE, sizeof(float) * bsize, buffer, NULL, NULL); 	
	icl_out_float_hbuffer(buffer, bsize);
}

void icl_out_uint_dbuffer(const icl_buffer *clbuf, size_t bsize){
	unsigned int *buffer = (unsigned int*)ALLOCA(sizeof(unsigned int)*bsize);
	icl_read_buffer(clbuf,  CL_TRUE, sizeof(unsigned int) * bsize, buffer, NULL, NULL); 	
	size_t i;
	for(i=0; i< bsize; i++){
		printf("%u ", buffer[i]);		
	}
	printf("\n");
}

void icl_out_int_dbuffer(const icl_buffer *clbuf, size_t bsize){
	int *buffer = (int*)ALLOCA(sizeof(int)*bsize);
	icl_read_buffer(clbuf,  CL_TRUE, sizeof(int) * bsize, buffer, NULL, NULL); 	
	size_t i;
	for(i=0; i< bsize; i++){
		printf("%d ", buffer[i]);		
	}
	printf("\n");
}


void icl_in_float_dbuffer(float *buffer, icl_buffer *clbuf, size_t bsize){
	size_t i;
	for(i=0; i< bsize; i++){
		scanf("%f", &buffer[i]);
	}		
	icl_write_buffer(clbuf, CL_TRUE, sizeof(float)*bsize, buffer, NULL, NULL);
}


void icl_in_uint_dbuffer(unsigned int *buffer, icl_buffer *clbuf, size_t bsize){
	size_t i;
	for(i=0; i< bsize; i++){
		scanf("%u", &buffer[i]);
	}	
	icl_write_buffer(clbuf, CL_TRUE, sizeof(unsigned int)*bsize, buffer, NULL, NULL);
}

void icl_in_int_dbuffer(int *buffer, icl_buffer *clbuf, size_t bsize){
	size_t i;
	for(i=0; i< bsize; i++){
		scanf("%d", &buffer[i]);
	}	
	icl_write_buffer(clbuf, CL_TRUE, sizeof(int)*bsize, buffer, NULL, NULL);
}

///

void icl_print_arguments(const icl_arguments *a){
	char deviceTypeString[32];
	if(a->deviceType ==  CL_DEVICE_TYPE_CPU)				sprintf(deviceTypeString, "cpu");
	else if(a->deviceType ==  CL_DEVICE_TYPE_GPU)			sprintf(deviceTypeString, "gpu");
	else if(a->deviceType ==  CL_DEVICE_TYPE_ACCELERATOR)   sprintf(deviceTypeString, "accelerator");
	else if(a->deviceType ==  CL_DEVICE_TYPE_ALL)			sprintf(deviceTypeString, "all");
	else if(a->deviceType ==  CL_DEVICE_TYPE_DEFAULT)   	sprintf(deviceTypeString, "default");	
	else sprintf(deviceTypeString, "not known");
	
	printf("Kernel arguments:\n"
		"    size %d\n    group_size %d\n    device_type %s\n    iterations %d\n    check %s\n    host_timer %s\n", 
		a->size, a->groupSize, deviceTypeString, 
		a->iter, 
		(a->checkResults)?"YES":"NO", (a->hostTimer)?"YES":"NO"
	);
}

void icl_print_arguments_ext(const icl_arguments *a){
	icl_print_arguments(a);

	char dataTypeString[32];
	if(a->dataType ==  icl_FLOAT)        sprintf(dataTypeString, "float");
	else if(a->dataType ==  icl_DOUBLE)  sprintf(dataTypeString, "double");
	else if(a->dataType ==  icl_INT)     sprintf(dataTypeString, "int");

	printf("device_timer %s\n    data_type %s\n    kernel_version %d\n", (a->deviceTimer)?"YES":"NO", dataTypeString, a->kernel);	
}

void icl_prompt(){	
	printf("press a key to continue...\n");
	getchar();	
}

#endif
