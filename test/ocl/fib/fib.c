#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <CL/cl.h>

#define bool int
#define true 1
#define false 0


/**
 * The cut-off for the OpenCL job; It produces 2^CUT sub-jobs
 */
#ifndef CUT
	#define CUT 12
#endif

#ifndef OMP_CUT
	#define OMP_CUT CUT
#endif

#ifndef OCL_CUT
	#define OCL_CUT (CUT + 10)
#endif

double getTime();

int fibSeq(int n);
int fibOMP(int n);
int fibOCL(int n);


void main(int argc, char** argv) {

	int n = 50;
	if (argc > 1) {
		n = atoi(argv[1]);
	}

	printf("Computing fib(%d) ...\n", n);
	int res;
	double time;	

	printf("Sequential:\n");
	time = getTime();
	res = fibSeq(n);
	time = getTime() - time;
	printf("Result: %d\n", res);
	printf("Time: %f sec\n", time);
	printf("\n");

	printf("Using OpenMP:\n");
	time = getTime();
	res = fibOMP(n);
	time = getTime() - time;
	printf("Result: %d\n", res);
	printf("Time: %f sec\n", time);
	printf("\n");

	printf("Using OpenCL:\n");
	time = getTime();
	res = fibOCL(n);
	time = getTime() - time;
	printf("Result: %d\n", res);
	printf("Time: %f sec\n", time);
	printf("\n");
}

double getTime() {
	static const double micro = 1000 * 1000;
	struct timeval tv;
	gettimeofday(&tv,0);
	return tv.tv_sec + tv.tv_usec / micro;
}

// --------------------- Sequential -------------------------

int fibSeq(int n) {
	if (n < 0) { return 0; }
	return (n<=1)?1:(fibSeq(n-1)+fibSeq(n-2));	
}


// ----------------------- Utils ----------------------------

int initActivationRecord(int n, int** pos, int cut) {
	if (cut == 0) {
		(**pos) = n;
		(*pos)++;
	} else {
		initActivationRecord(n-1, pos, cut-1);
		initActivationRecord(n-2, pos, cut-1);
	}
}

// ----------------------- OpenMP ----------------------------

int fibOMP(int n) {

	// create array of activation records
	int record[1<<OMP_CUT];
	int* pos = record;

	// fill in inputs
	initActivationRecord(n, &pos, OMP_CUT);

	// process in parallel
	int res;
	#pragma omp parallel for reduction(+:res)
	for(int i=0; i<(1<<OMP_CUT); i++) {
		res += fibSeq(record[i]);	
	}

	// return result
	return res;
}

// ----------------------- OpenCL ----------------------------

void handleErrorCode(cl_int error) {
	
	if (error != CL_SUCCESS) {
		printf("ERROR: ");
	}

	switch (error) {
	case CL_SUCCESS:                            return; // printf("Success!\n"); return;
	case CL_DEVICE_NOT_FOUND:                   printf("Device not found.\n"); break;
	case CL_DEVICE_NOT_AVAILABLE:               printf("Device not available\n"); break;
	case CL_COMPILER_NOT_AVAILABLE:             printf("Compiler not available\n"); break;
	case CL_MEM_OBJECT_ALLOCATION_FAILURE:      printf("Memory object allocation failure\n"); break;
	case CL_OUT_OF_RESOURCES:                   printf("Out of resources\n"); break;
	case CL_OUT_OF_HOST_MEMORY:                 printf("Out of host memory\n"); break;
	case CL_PROFILING_INFO_NOT_AVAILABLE:       printf("Profiling information not available\n"); break;
	case CL_MEM_COPY_OVERLAP:                   printf("Memory copy overlap\n"); break;
	case CL_IMAGE_FORMAT_MISMATCH:              printf("Image format mismatch\n"); break;
	case CL_IMAGE_FORMAT_NOT_SUPPORTED:         printf("Image format not supported\n"); break;
	case CL_MAP_FAILURE:                        printf("Map failure\n"); break;
	case CL_BUILD_PROGRAM_FAILURE:              printf("Program build failure\n"); break;
	case CL_INVALID_VALUE:                      printf("Invalid value\n"); break;
	case CL_INVALID_DEVICE_TYPE:                printf("Invalid device type\n"); break;
	case CL_INVALID_PLATFORM:                   printf("Invalid platform\n"); break;
	case CL_INVALID_DEVICE:                     printf("Invalid device\n"); break;
	case CL_INVALID_CONTEXT:                    printf("Invalid context\n"); break;
	case CL_INVALID_QUEUE_PROPERTIES:           printf("Invalid queue properties\n"); break;
	case CL_INVALID_COMMAND_QUEUE:              printf("Invalid command queue\n"); break;
	case CL_INVALID_HOST_PTR:                   printf("Invalid host pointer\n"); break;
	case CL_INVALID_MEM_OBJECT:                 printf("Invalid memory object\n"); break;
	case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    printf("Invalid image format descriptor\n"); break;
	case CL_INVALID_IMAGE_SIZE:                 printf("Invalid image size\n"); break;
	case CL_INVALID_SAMPLER:                    printf("Invalid sampler\n"); break;
	case CL_INVALID_BINARY:                     printf("Invalid binary\n"); break;
	case CL_INVALID_BUILD_OPTIONS:              printf("Invalid build options\n"); break;
	case CL_INVALID_PROGRAM:                    printf("Invalid program\n"); break;
	case CL_INVALID_PROGRAM_EXECUTABLE:         printf("Invalid program executable\n"); break;
	case CL_INVALID_KERNEL_NAME:                printf("Invalid kernel name\n"); break;
	case CL_INVALID_KERNEL_DEFINITION:          printf("Invalid kernel definition\n"); break;
	case CL_INVALID_KERNEL:                     printf("Invalid kernel\n"); break;
	case CL_INVALID_ARG_INDEX:                  printf("Invalid argument index\n"); break;
	case CL_INVALID_ARG_VALUE:                  printf("Invalid argument value\n"); break;
	case CL_INVALID_ARG_SIZE:                   printf("Invalid argument size\n"); break;
	case CL_INVALID_KERNEL_ARGS:                printf("Invalid kernel arguments\n"); break;
	case CL_INVALID_WORK_DIMENSION:             printf("Invalid work dimension\n"); break;
	case CL_INVALID_WORK_GROUP_SIZE:            printf("Invalid work group size\n"); break;
	case CL_INVALID_WORK_ITEM_SIZE:             printf("Invalid work item size\n"); break;
	case CL_INVALID_GLOBAL_OFFSET:              printf("Invalid global offset\n"); break;
	case CL_INVALID_EVENT_WAIT_LIST:            printf("Invalid event wait list\n"); break;
	case CL_INVALID_EVENT:                      printf("Invalid event\n"); break;
	case CL_INVALID_OPERATION:                  printf("Invalid operation\n"); break;
	case CL_INVALID_GL_OBJECT:                  printf("Invalid OpenGL object\n"); break;
	case CL_INVALID_BUFFER_SIZE:                printf("Invalid buffer size\n"); break;
	case CL_INVALID_MIP_LEVEL:                  printf("Invalid mip-map level\n"); break;
	default: printf("Unknown\n"); break;
	}
	exit(1);
}

char* loadFile(char* name) {
	char *file_contents;
	long input_file_size;
	FILE *input_file = fopen(name, "rb");
	fseek(input_file, 0, SEEK_END);
	input_file_size = ftell(input_file);
	rewind(input_file);
	file_contents = malloc((input_file_size + 1) * (sizeof(char)));
	int size_read = fread(file_contents, sizeof(char), input_file_size, input_file);
	if (size_read != input_file_size) {
		printf("Error reading file content.\n");
		exit(1);
	}
	fclose(input_file);
	file_contents[input_file_size] = 0;
	return file_contents;
}

//#define INFO(...) printf(__VA_ARGS__); printf("\n")
#define INFO(...)

int fibOCL(int n) {

	// create array of activation records
	const int size = 1<< OCL_CUT;
	int* record = malloc(sizeof(int)*size);
	int* pos = record;

	// fill activation record array
	initActivationRecord(n, &pos, OCL_CUT);

	// ---- process in parallel ----
	cl_int error;	

	// get platform
	cl_platform_id platform;
	{
		cl_uint numPlatforms;
		handleErrorCode(clGetPlatformIDs(0,0, &numPlatforms));
		INFO("Found %d platform(s)!", numPlatforms);

		cl_platform_id ids[numPlatforms];
		handleErrorCode(clGetPlatformIDs(numPlatforms,ids, &numPlatforms));

		platform = ids[0];
	}

	// get devices
	cl_device_id device;
	{
		cl_uint numDevices;
		handleErrorCode(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 0, 0, &numDevices));
		cl_device_id ids[numDevices];
		handleErrorCode(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, numDevices, ids, &numDevices));
		device = ids[0];
	}

	// create context
	INFO("Creating context ..");
	cl_context context = clCreateContext(0, 1, &device, 0, 0, &error);
	handleErrorCode(error);

	// create queue
	INFO("Creating command queue ..");
	cl_command_queue queue = clCreateCommandQueue(context, device, 0, &error);
	handleErrorCode(error);

	// create program
	INFO("Building program ..");
	cl_program program;
	{
		const char* code = loadFile("fib.cl");
		INFO("Kernel Code:\n%s\n", code);

		size_t code_length = strlen(code);
		program = clCreateProgramWithSource(context,1, &code, &code_length, &error);
		handleErrorCode(error);

		// build program
		error = clBuildProgram(program, 1, &device, 0, 0, 0);

		if (error == CL_BUILD_PROGRAM_FAILURE) {
			printf("Program built failed!\n");

			// obtain additional build error information
			size_t logSize = 2048;
			char log[logSize];
			clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, logSize, log, &logSize);

			printf("Log:\n%s\n", log);
				
			exit(1);
		} else {
			handleErrorCode(error);
		}
	}

	// get kernel
	INFO("Building kernel ..");
	cl_kernel kernel = clCreateKernel(program, "fib", &error);
	handleErrorCode(error);

	// create buffers
	INFO("Creating Buffers ..");
	cl_mem vecA = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, sizeof(int)*size, record, &error); handleErrorCode(error);
	cl_mem vecR = clCreateBuffer(context, CL_MEM_WRITE_ONLY, sizeof(int)*size, 0, &error); handleErrorCode(error);

	// set up arguments
	INFO("Setting up Arguments ..");
	handleErrorCode(clSetKernelArg(kernel, 0, sizeof(vecA), &vecA));
	handleErrorCode(clSetKernelArg(kernel, 1, sizeof(vecR), &vecR));
	handleErrorCode(clSetKernelArg(kernel, 2, sizeof(size), &size));

	// run kernel
	INFO("Running kernel ..");
	size_t local_work_size = 64;
	size_t global_work_offset = 0;
	size_t global_work_size = size;
	cl_event kernel_done;
	double start = getTime();
	handleErrorCode(clEnqueueNDRangeKernel(queue, kernel, 1, &global_work_offset, &global_work_size, &local_work_size, 0, 0, &kernel_done));

	handleErrorCode(clWaitForEvents(1,&kernel_done));
	double time = getTime() - start;
	printf("OpenCL Computation Time: %lf sec\n", time);

	// enqueue read operation
	cl_event read_done;
	int* res = malloc(sizeof(int)*size);
	handleErrorCode(clEnqueueReadBuffer(queue, vecR, true, 0, sizeof(int)*size, res, 1, &kernel_done, &read_done));

	// wait for completion
	handleErrorCode(clWaitForEvents(1,&read_done));

	// cleanup
	handleErrorCode(clFinish(queue));
	handleErrorCode(clReleaseKernel(kernel));
	handleErrorCode(clReleaseProgram(program));
	handleErrorCode(clReleaseCommandQueue(queue));
	handleErrorCode(clReleaseContext(context));

	// aggregate results	TODO: move reduction to OpenCL kernel
	int sum = 0;
	#pragma omp parallel for reduction(+:sum)
	for(int i=0; i<size; i++) {
		sum += res[i];
	}

	// release arrays
	free(record);
	free(res);

	// done
	return sum;
}
