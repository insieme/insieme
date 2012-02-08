/*
    Parallel reduction

	Note(Biagio): this code has been taken from NVidia examples, and modified to 
	be used in Insieme. Also, command line arguments are different.	You can use 
	-cl-fast-relaxed-math to considerably speed up perfomance by losing floating
	point precision.
	Original code: Copyright 1993-2010 NVIDIA Corporation.  All rights reserved.

	From NVidia:
    This sample shows how to perform a reduction operation on an array of values
    to produce a single value.
    Reductions are a very common computation in parallel algorithms.  Any time
    an array of values needs to be reduced to a single value using a binary 
    associative operator, a reduction can be used.  Example applications include
    statistics computaions such as mean and standard deviation, and image 
    processing applications such as finding the total luminance of an
    image.
    This code performs SUM reductions, but any associative operator such as
    min() or max() could also be used. It assumes the input size is a power of 2.    
*/

#include "lib_icl.h"
#include "lib_icl_ext.h"   // this also include alloca's def

#include "math.h" // for abs (host)

#define MIN(X, Y)  ((X) < (Y) ? (X) : (Y))
#define MAX_BLOCK_DIM_SIZE 65535

static bool isPow2(unsigned int x){ return ((x&(x-1))==0); }
void getNumBlocksAndThreads(int whichKernel, unsigned int n, int maxBlocks, int maxThreads, int *blocks, int *threads);
float reduce_float(cl_uint, int, int, float*, icl_kernel*, icl_kernel*, icl_buffer *, icl_buffer *);
float reduceCPU_float(float *data, int size);

// constant values
const int maxBlocks = 64;     // specify the maximum number of thread blocks to launch (kernel 6 only, default 64)
const int maxThreads = 128;   // 64 for small block, 128 otherwise
const int whichKernel = 6;  

int main(int argc, const char** argv) 
{    
	// parsing command-line arguments 650 58 56 053
	icl_arguments arguments = icl_parse_argument(argc, argv);
	arguments.iter = 1;
	arguments.deviceType = CL_DEVICE_TYPE_GPU;
//	arguments.size = 1048576;
	arguments.size = 16777216;
	
	icl_print_arguments(&arguments);

	// opencl init
	icl_init_devices(arguments.deviceType); 
	icl_device *device = icl_get_device(arguments.deviceId);
	icl_buffer *d_idata = icl_create_buffer(device, CL_MEM_READ_ONLY,  arguments.size * sizeof(float));
	icl_buffer *d_odata = icl_create_buffer(device, CL_MEM_READ_WRITE, arguments.size * sizeof(float));

	// kernel		
	icl_kernel *reductionKernel = icl_create_kernel(device, "reduction6.cl", "reduce6", "", ICL_SOURCE);		
	icl_kernel *finalKernel = icl_create_kernel(device, "reduction5.cl", "reduce5", "", ICL_SOURCE);		
	
	// generate pseudo-random input
	float *h_idata = (float*) ALLOCA(sizeof(float)*arguments.size);
	unsigned int bytes = arguments.size * sizeof(float);
	srand(42);
    for(size_t i=0; i<arguments.size; i++) 
		h_idata[i] = (rand() & 0xFF) / (float)RAND_MAX;
   
	// timer 
	icl_timer *timer = icl_init_timer(ICL_MILLI);	
	double deviceTime = 0;
	float gpu_result;	

	if(arguments.hostTimer) icl_start_timer(timer);
	for(size_t i=0; i<arguments.iter; i++)
	{
		int blocks, threads;
		getNumBlocksAndThreads(whichKernel, arguments.size, maxBlocks, maxThreads, &blocks, &threads);		

		// kernel call, including host-device communication
		gpu_result = reduce_float(arguments.size, threads, blocks, h_idata, reductionKernel, finalKernel, d_idata, d_odata);
		
	}  // for each test iteration
	if(arguments.hostTimer) deviceTime = icl_stop_timer(timer);			


	if(arguments.checkResults){
		// compute reference solution
        printf("\nComparing against a serial host computation\n"); 
        float cpu_result = reduceCPU_float(h_idata, arguments.size);
        printf("  GPU result = %.9f\n", gpu_result);
        printf("  CPU result = %.9f\n\n", cpu_result);

        double threshold = 1e-8 * arguments.size;
        double diff = abs((double)gpu_result - (double)cpu_result);
        printf("  %s\n\n", (diff < threshold) ? "PASSED" : "FAILED");
    }			

	
	printf("kernel_name reduce%d, avg_time %.6f, iterations %d\n", whichKernel, deviceTime/(double)arguments.iter, arguments.iter);

	icl_release_timer(timer);
	icl_release_devices();

	#ifdef _WIN32
	icl_prompt();
    #endif

}


/* 
  Compute sum reduction on CPU. It uses Kahan summation for an accurate sum of large arrays.
  http://en.wikipedia.org/wiki/Kahan_summation_algorithm
*/
float reduceCPU_float(float *data, int size)
{
    float sum = data[0];
    float c = (float)0.0;              
    for (int i = 1; i < size; i++)
    {
        float y = data[i] - c;  
        float t = sum + y;      
        c = (t - sum) - y;  
        sum = t;            
    }
    return sum;
}

unsigned int nextPow2( unsigned int x ) {
    --x;
    x |= x >> 1u;
    x |= x >> 2u;
    x |= x >> 4u;
    x |= x >> 8u;
    x |= x >> 16u;
    return ++x;
}

/*
  Compute the number of threads and blocks to use for the given reduction kernel.
  For the kernels >= 3, we set threads / block to the minimum of maxThreads and n/2. 
  For kernels < 3, we set to the minimum of maxThreads and n.  For kernel 6, we observe 
  the maximum specified number of blocks, because each thread in that kernel can process 
  a variable number of elements.
*/
void getNumBlocksAndThreads(int whichKernel, unsigned int n, int maxBlocks, int maxThreads, int *blocks, int *threads)
{
    if (whichKernel < 3) {
        *threads = (n < maxThreads) ? nextPow2(n) : maxThreads;
        *blocks = (n + *threads - 1) / *threads;
    }
    else {
        *threads = (n < maxThreads*2) ? nextPow2((n + 1)/ 2) : maxThreads;
        *blocks = (n + (*threads * 2 - 1)) / (*threads * 2);
    }
    if (whichKernel == 6)
        *blocks = MIN(maxBlocks, *blocks);
}


// kernel invocation 
// there could be two kernel used: one for the first pass, the latter for the final pass
float reduce_float(cl_uint  n, int  numThreads, int  numBlocks, 
                  float* h_idata, 
				  icl_kernel *reductionKernel, icl_kernel *finalKernel, 
				  icl_buffer *d_idata, icl_buffer *d_odata)
{
	float gpu_result = 0;
	size_t globalWorkSize = numBlocks * numThreads;
    size_t localWorkSize = numThreads;

	// move input buffer from host to device
	icl_write_buffer(d_idata, CL_TRUE, n*sizeof(float), h_idata, 0, 0);

	const bool cpuFinalReduction = false;
	const int cpuFinalThreshold = 1;
	bool needReadBack = true;
//    cl_kernel finalReductionKernel[10];
    int finalReductionIterations=0;    

/// XXX
	/*
    cl_kernel reductionKernel = getReductionKernel(datatype, whichKernel, numThreads, isPow2(n) );
    clSetKernelArg(reductionKernel, 0, sizeof(cl_mem), (void *) &d_idata);
    clSetKernelArg(reductionKernel, 1, sizeof(cl_mem), (void *) &d_odata);
    clSetKernelArg(reductionKernel, 2, sizeof(cl_int), &n);
    clSetKernelArg(reductionKernel, 3, sizeof(T) * numThreads, NULL);
	*/

//// DEBUG ////

printf("device input data\n");
icl_out_float_dbuffer(d_idata, 256);


    if( !cpuFinalReduction ) {
        int s=numBlocks;
        int threads = 0, blocks = 0;
        int kernel = (whichKernel == 6u) ? 5 : whichKernel;
        
        while(s > cpuFinalThreshold) 
        {
            getNumBlocksAndThreads(kernel, s, maxBlocks, maxThreads, &blocks, &threads);

/// XXX
/*
            finalReductionKernel[finalReductionIterations] = getReductionKernel(datatype, kernel, threads, isPow2(s) );
            clSetKernelArg(finalReductionKernel[finalReductionIterations], 0, sizeof(cl_mem), (void *) &d_odata);
            clSetKernelArg(finalReductionKernel[finalReductionIterations], 1, sizeof(cl_mem), (void *) &d_odata);
            clSetKernelArg(finalReductionKernel[finalReductionIterations], 2, sizeof(cl_int), &n);
            clSetKernelArg(finalReductionKernel[finalReductionIterations], 3, sizeof(T) * numThreads, NULL);
 */         
			printf("\n * (block %d, iter %d) \n", s, finalReductionIterations);
   
            if (kernel < 3)
                s = (s + threads - 1) / threads;
            else

                s = (s + (threads*2-1)) / (threads*2);

            finalReductionIterations++;

			printf("-%d \n", s);
        }
    }
    
//    size_t globalWorkSize[1];
//    size_t localWorkSize[1];



	// execute the first reduction kernel
    globalWorkSize = numBlocks * numThreads;
    localWorkSize = numThreads;
		
	size_t reductionLocalWorkSize = 128;
///
	icl_run_kernel(reductionKernel, 1, &globalWorkSize, &reductionLocalWorkSize , NULL, NULL, 4,   
			(size_t) 0, (void*) d_idata, 
			(size_t) 0, (void*) d_odata,					
			sizeof(cl_uint), &n, 
			sizeof(float) * numThreads, NULL); 

printf("device buffer 0.\n");
icl_out_float_dbuffer(d_odata, 256);
//icl_isnan_float_dbuffer(d_odata, n);

//        ciErrNum = clEnqueueNDRangeKernel(cqCommandQueue,reductionKernel, 1, 0, globalWorkSize, localWorkSize, 0, NULL, NULL);               
        // check if kernel execution generated an error        
//        oclCheckError(ciErrNum, CL_SUCCESS);

    if (cpuFinalReduction)
    {
		printf("1.\n");

        // sum partial sums from each block on CPU        
        // copy result from device to host
//            clEnqueueReadBuffer(cqCommandQueue, d_odata, CL_TRUE, 0, numBlocks * sizeof(T), h_odata, 0, NULL, NULL);
		float *h_odata = (float*) ALLOCA(sizeof(float)*n); ;
		icl_read_buffer(d_odata, CL_TRUE, n*sizeof(float), h_odata, 0, 0);
		
        for(int i=0; i<numBlocks; i++) 
        {
			gpu_result += h_odata[i];
        }

        needReadBack = false;
	}
    else
    {
		printf("2.\n");

        // sum partial block sums on GPU
        int s=numBlocks;
        int kernel = (whichKernel == 6u) ? 5 : whichKernel;
        int it = 0;            

        while(s > cpuFinalThreshold) {
			int threads = 0, blocks = 0;
            getNumBlocksAndThreads(kernel, s, maxBlocks, maxThreads, &blocks, &threads);
			
			globalWorkSize = threads * blocks;
            localWorkSize = threads;
///

			icl_run_kernel(finalKernel, 1, &globalWorkSize, &localWorkSize, NULL, NULL, 4,   
				(size_t) 0, (void*) d_odata, 
				(size_t) 0, (void*) d_odata,					
				sizeof(cl_uint), &n, 
				sizeof(float) * numThreads, NULL); 
//                ciErrNum = clEnqueueNDRangeKernel(cqCommandQueue, finalReductionKernel[it], 1, 0,  globalWorkSize, localWorkSize, 0, NULL, NULL);               
//                oclCheckError(ciErrNum, CL_SUCCESS);
            
            if (kernel < 3)
				s = (s + threads - 1) / threads;
            else
                s = (s + (threads*2-1)) / (threads*2);

            it++;


//// DEBUG ////
printf("device while 1\n");
icl_out_float_dbuffer(d_odata, 256);

		}

	if (s > 1) {
		printf("3.\n");
			
//// DEBUG ////
printf("device if 2\n");
icl_out_float_dbuffer(d_odata, 256);


			// copy result from device to host
			// clEnqueueReadBuffer(cqCommandQueue, d_odata, CL_TRUE, 0, s * sizeof(T), h_odata, 0, NULL, NULL);
			float *h_odata = (float*) ALLOCA(sizeof(float)*n); ;
			icl_read_buffer(d_odata, CL_TRUE, 1*sizeof(float), h_odata, 0, 0);

			for(int i=0; i < s; i++) gpu_result += h_odata[i];
			needReadBack = false;
		}
	}

	clFinish(finalKernel->dev->queue);
    

    if (needReadBack)
    {
		printf("3.\n");

        // copy final sum from device to host
		// Note(Biagio): the sum is in the first position of the array, others element have 0
//        clEnqueueReadBuffer(cqCommandQueue, d_odata, CL_TRUE, 0, sizeof(T), &gpu_result, 0, NULL, NULL);
		icl_read_buffer(d_odata, CL_TRUE, 1*sizeof(float), &gpu_result, 0, 0);
		printf("sent back %f\n", gpu_result);
    }

//// DEBUG ////
printf("final device status\n");
icl_out_float_dbuffer(d_odata, 256);


/*    clReleaseKernel(reductionKernel);
    if( !cpuFinalReduction ) {
        for(int it=0; it<finalReductionIterations; ++it) {
            clReleaseKernel(finalReductionKernel[it]);
        }
        
    }
*/

	return gpu_result;        
}
