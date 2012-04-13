#include <stdio.h>
#include "CL/cl.h"
#include "clutils.h"
#include "errutils.h"
#include "futils.h"

void initHostPtr(float* arr, size_t height, size_t width, size_t depth, float val) {
    for(size_t i = 0; i < height; ++i) {
        for(size_t j = 0; j < width; ++j) 
            for(size_t k = 0; k < depth; ++k) 
                arr[i * width * depth + j * depth + k] = val ? val : (float)(i * width * depth + j * depth + k);
    }
}

void initHostPtrInt(int* arr, size_t height, size_t width, size_t depth, size_t scale) {
    size_t counter = 0;
    for(size_t i = 0; i < height; ++i) {
        for(size_t j = 0; j < width; ++j) 
            for(size_t k = 0; k < depth; ++k) {
                switch (counter%3) {
                case 0: arr[i * width * depth + j * depth + k] = width/scale; break;
                case 1: arr[i * width * depth + j * depth + k] = height/scale; break;
                case 2: arr[i * width * depth + j * depth + k] = depth/scale; break;
                }
                ++counter;
            }
    }
}

int main(int argc, char* argv[]) {
    char* kernelFile = argc > 1 ? argv[1] : "ocl_kernel.c";
    
    char* kernelNames[] = { "allMemArg", "simpleCalc" , "getId", "getSize", "branch", "access3D", "vectorTest", "barriers", "VectorAdd" };

    // set problem sizes
    size_t nGroups = 2;
    size_t width = 8 * nGroups;
    size_t height = 4 * nGroups;
    size_t depth = 2 * nGroups;
    size_t nElem = width*height*depth;
    
    const size_t localDim[] = {width/nGroups, height/nGroups, depth/nGroups};
    const size_t globalDim[] = {width, height, depth};

    // set up host pointers
    float *hc, *hga, *result;
    int* hgb;

    hc = (float*)malloc(sizeof(float) * nElem);
    hga = (float*)malloc(sizeof(float) * nElem);
    hgb = (int*)malloc(sizeof(int) * nElem);
    result = (float*)malloc(sizeof(float) * nElem);
    
    initHostPtr(hc, width, height, depth, 0.0f);
    initHostPtr(hga, height, width, depth, 1.25f);
    initHostPtrInt(hgb, height, width, depth, 1);
    
    // device pointers
    cl_mem dc, dga, dgb;

    cl_int _errCode;
    cl_int *errCode = &_errCode;

   	// get platforms & select platform
	const cl_uint numEntries = 8;
	cl_platform_id platforms[numEntries];
	cl_uint numPlatforms;
	CLCHECK(clGetPlatformIDs(numEntries, platforms,	&numPlatforms));
	cl_platform_id platform = platforms[0];
	if(argc > 2) {
		cl_uint platformNum = atoi(argv[2]);
		if(platformNum<0 || platformNum>numPlatforms) { fprintf(stderr, "Invalid platform selection: %i\n", platformNum); exit(-1); }
		platform = platforms[platformNum];
	}

	// get devices & select device
	const cl_uint numDevicesMax = 8;
	cl_device_id devices[numDevicesMax];
	cl_uint numDevices;
	CLCHECK(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, numDevicesMax,	devices, &numDevices));
	cl_device_id device = devices[0];
	if(argc > 3) {
		cl_uint deviceNum = atoi(argv[3]);
		if(deviceNum<0 || deviceNum>numDevices) { fprintf(stderr, "Invalid device selection: %i\n", deviceNum); exit(-1); }
		device = devices[deviceNum];
	}
    
//#ifndef PYTHON
	// print info
	const size_t nameBuffSize = 128;
	size_t platNameSize, deviceNameSize;
	char platformName[nameBuffSize+1], deviceName[nameBuffSize+1];
	CLCHECK(clGetPlatformInfo(platform, CL_PLATFORM_NAME, nameBuffSize, platformName, &platNameSize));
	CLCHECK(clGetDeviceInfo(device, CL_DEVICE_NAME, nameBuffSize, deviceName, &deviceNameSize));
	printf("Running on platform %s\n", platformName);
	printf("        on device   %s\n", deviceName);
//#endif
   
	// create a compute context with device
	cl_context_properties conProps[] = { CL_CONTEXT_PLATFORM, (intptr_t)NULL, (intptr_t)NULL };
	conProps[1] = (intptr_t)platform;
	cl_context context = clCreateContext(conProps, 1, &device, NULL, NULL, errCode);
	CLCHECK(_errCode);

	// create a command queue
	cl_command_queue queue = clCreateCommandQueue(context, device, (intptr_t)NULL, errCode);
	CLCHECK(_errCode);
	
	// create the compute program
	char *code = readFile(kernelFile);
//#pragma insieme kernelFile "test_kernels.c"
	cl_program program = clCreateProgramWithSource(context, 1, (void*)&code, (intptr_t)NULL, errCode);
	CLCHECK(_errCode);
	free(code);	
	
	// build the compute program executable
	buildProgram(program, device, "-DNO_INSIEME");
	
	// allocate device memory
    dc = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, sizeof(float) * nElem, hc, errCode);
    CLCHECK(_errCode);
    dga = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(float) * nElem, NULL, errCode);
    CLCHECK(_errCode);
    dgb = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int) * nElem, NULL, errCode);
    CLCHECK(_errCode);
	
	for(size_t i = 0; i < (sizeof(kernelNames)/sizeof(char*)); ++i) {

	    // copy data to mutable arrays
//	    CLCHECK(clEnqueueWriteBuffer(queue, dc, CL_TRUE, 0, sizeof(float) * nElem, hc, 0, NULL, NULL));
	    CLCHECK(clEnqueueWriteBuffer(queue, dga, CL_TRUE, 0, sizeof(float) * nElem, hga, 0, NULL, NULL));
	    CLCHECK(clEnqueueWriteBuffer(queue, dgb, CL_TRUE, 0, sizeof(int) * nElem, hgb, 0, NULL, NULL));
	    
	    // get kernel
//        printf("\nRunning %s kernel... \n", kernelNames[i]);
//   	    cl_kernel kernel = clCreateKernel(program, kernelNames[i], errCode);
        printf("\nRunning %s kernel... \n", kernelNames[i]);
        cl_kernel kernel = clCreateKernel(program, kernelNames[i], errCode);
           	    
	    CLCHECK(_errCode);
	    
	    // set up arguments
        clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *) &dc);
        clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *) &dga);
        clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *) &dgb);
        clSetKernelArg(kernel, 3, sizeof(float) * 512, NULL);
        clSetKernelArg(kernel, 4, sizeof(cl_uint), (void *) &width);
        clSetKernelArg(kernel, 5, sizeof(cl_int), (void *) &width);

        clFinish(queue);
        
        // start kernel
        CLCHECK(clEnqueueNDRangeKernel(queue, kernel, 3u, 0, globalDim, localDim, 0, NULL, 0));
        clFinish(queue);
        
        // get result
        clEnqueueReadBuffer(queue, dga, CL_TRUE, 0, sizeof(float) * nElem, result, 0, NULL, NULL);
        clFinish(queue);

	    // print results
        for(size_t i = 0; i < height; ++i) {
            for(size_t j = 0; j < width; ++j) {
                printf("\n");
                for(size_t k = 0; k < depth; ++k) 
                    printf("%f ", result[i * width * depth + j * depth + k]);
            }
        }
        printf("\n");
        
        clReleaseKernel(kernel);
    }

    // memory cleanup
    free(hc);
    free(hga);
    free(hgb);
    free(result);
    
	clReleaseCommandQueue(queue);
	clReleaseContext(context);

	clReleaseMemObject(dc);
	clReleaseMemObject(dga);
	clReleaseMemObject(dgb);
}
