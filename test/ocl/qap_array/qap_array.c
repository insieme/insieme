
#include <math.h>
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
	#define CUT 3
#endif

#ifndef OMP_CUT
	#define OMP_CUT CUT
#endif

#ifndef OCL_CUT
	#define OCL_CUT (CUT + 10)
#endif

// A quadratic matrix, dynamically sized
typedef struct _qmatrix {
	int size;
	int data[];
} qmatrix;

qmatrix* qm_create(int size) {
	qmatrix* res = (qmatrix*)malloc(sizeof(qmatrix) + size * size * sizeof(int));
	res->size = size;
	return res;
}

void qm_del(qmatrix* matrix) {
	free(matrix);
}

#define get(M,I,J) M->data[I*M->size+J]


// a struct describing a QAP instance
typedef struct _problem {
	int size;		// the size of the problem
	qmatrix* A;		// the weight matrix (size x size)
	qmatrix* B;		// the distance matrix (size x size)
	int optimum;		// the value of the optimal solution
} problem;

problem* qap_load(char* file);

void qap_del(problem* problem) {
	qm_del(problem->A);
	qm_del(problem->B);
	free(problem);
}

#define getA(P,I,J) get(P->A,I,J)
#define getB(P,I,J) get(P->B,I,J)

double getTime();

int solve_seq(problem* problem);
int solve_omp(problem* problem);
int solve_ocl(problem* problem);

int main(int argc, char** argv) {
	
	char* problem_file = "problems/chr10a.dat";
	if (argc >= 2) {
		problem_file = argv[1];
	}

	// load problem
	problem* p = qap_load(problem_file);

	printf("Computing problem %s ...\n", problem_file);
	int res;
	double time;	

	printf("Sequential:\n");
	time = getTime();
	res = solve_seq(p);
	time = getTime() - time;
	printf("Result: %d => %s\n", res, ((p->optimum == res)?"successful":"failed"));
	printf("Time: %f sec\n", time);
	printf("\n");

	printf("Using OpenMP:\n");
	time = getTime();
	res = solve_omp(p);
	time = getTime() - time;
	printf("Result: %d => %s\n", res, ((p->optimum == res)?"successful":"failed"));
	printf("Time: %f sec\n", time);
	printf("\n");

	printf("Using OpenCL:\n");
	time = getTime();
	res = solve_ocl(p);
	time = getTime() - time;
	printf("Result: %d => %s\n", res, ((p->optimum == res)?"successful":"failed"));
	printf("Time: %f sec\n", time);
	printf("\n");

	// free problem
	qap_del(p);

	return 0;
}

double getTime() {
	static const double micro = 1000 * 1000;
	struct timeval tv;
	gettimeofday(&tv,0);
	return tv.tv_sec + tv.tv_usec / micro;
}

problem* qap_load(char* file) {

	FILE* fp = fopen(file, "r");
	printf("Loading Problem File %s ..\n", file);

	int dummy; // not the nice way ...

	// get problem size
	int problemSize;
	dummy = fscanf(fp, "%d", &problemSize);
	printf("  - problem size: %d\n", problemSize);
	
	// create problem instance
	problem* res = (problem*)malloc(sizeof(problem));
	res->size = problemSize;
	res->A = qm_create(problemSize);
	res->B = qm_create(problemSize);

	// load matrix A
	for(int i=0; i<problemSize; i++) {
		for(int j=0; j<problemSize; j++) {
			dummy = fscanf(fp, "%d", &getA(res,i,j));
		}
	}

	// load matrix B
	for(int i=0; i<problemSize; i++) {
		for(int j=0; j<problemSize; j++) {
			dummy = fscanf(fp, "%d", &getB(res,i,j));
		}
	}

	// load optimum
	dummy = fscanf(fp, "%d", &(res->optimum));
	printf("  - optimum: %d\n", res->optimum);

	fclose(fp);

	return res;
}


// --------------------- Sequential -------------------------

int solve_rec(problem* problem, int* partial, int plant, int used_mask, int cur_cost, volatile int* best_known) {
	// terminal case
	if (plant >= problem->size) {
		return cur_cost;
	}

	if (cur_cost >= *best_known) {
		return *best_known;
	}


	// fix current position
	for(int i=0; i<problem->size; i++) {
		// check whether current spot is a free spot
//		#pragma omp task
		if(!(1<<i & used_mask)) {
			// extend solution
			int tmp[problem->size];
			memcpy(tmp, partial, problem->size*sizeof(int));
			tmp[plant] = i;

			// compute additional cost of current assignment
			int new_cost = 0;
			
			for(int j=0; j<plant; j++) {
				int other_pos = tmp[j];

				// add costs between current pair of plants
				new_cost += getA(problem, plant, j) * getB(problem, i, other_pos);
				new_cost += getA(problem, j, plant) * getB(problem, other_pos, i);
			}

			// compute recursive rest
			int cur_best = solve_rec(problem, tmp, plant+1, used_mask | (1<<i), cur_cost + new_cost, best_known);

			// update best known solution
			if (cur_best < *best_known) {
				int best;
				//   |--- read best ---|          |--- check ---|    |------------ update if cur_best is better ------------|
				do { best = *best_known; } while (cur_best < best && __sync_bool_compare_and_swap(best_known, best, cur_best));
			}
		}
	}

//	#pragma omp taskwait

	return *best_known;
}

int solve_seq(problem* problem) {
	int res;
	int map[problem->size];	
	volatile int best = 1<<30;
//	#pragma omp parallel
	{
//		#pragma omp single
		res = solve_rec(problem, map, 0, 0, 0, &best);
	}
	return res;
}


// ----------------------- Utils ----------------------------

typedef struct {
	int partial_index;
	int plant; 
	int used_mask;
	int cur_cost;
} call_record;

void initActivationRecord(problem* problem, int* partial, int plant, int used_mask, int cur_cost, volatile int* best_known, call_record** pos, int* partial_store, int* partial_store_index, int cut) {
	
	// cut-off here
	if (cut == 0) {

		// reserve memory in partial solution store
		int partial_pos = *partial_store_index;
		*partial_store_index += problem->size;

		// copy partial solution to store
		memcpy(&(partial_store[partial_pos]), partial, problem->size * sizeof(int));
		
		// add call record
		(**pos) = (call_record){partial_pos, plant, used_mask, cur_cost};
		(*pos)++;
		return;
	}

	// standard body for the rest
	
	// terminal case
	if (plant >= problem->size) {
		return;
	}

	if (cur_cost >= *best_known) {
		return;
	}


	// fix current position
	for(int i=0; i<problem->size; i++) {
		// check whether current spot is a free spot
		if(!(1<<i & used_mask)) {
			// extend solution
			int tmp[problem->size];
			memcpy(tmp, partial, problem->size*sizeof(int));
			tmp[plant] = i;

			// compute additional cost of current assignment
			int new_cost = 0;
			
			for(int j=0; j<plant; j++) {
				int other_pos = tmp[j];

				// add costs between current pair of plants
				new_cost += getA(problem, plant, j) * getB(problem, i, other_pos);
				new_cost += getA(problem, j, plant) * getB(problem, other_pos, i);
			}

			// fill recoreds recursively
			initActivationRecord(problem, tmp, plant+1, used_mask | (1<<i), cur_cost + new_cost, best_known, pos, partial_store, partial_store_index, cut-1);

		}
	}
}

// ----------------------- OpenMP ----------------------------

int solve_omp(problem* problem) {

	// get upper boundary for number of sub-problems
	int max_problems = powl(problem->size, OMP_CUT);

	// create array of activation records
	call_record* sub_problems = malloc(sizeof(call_record) * max_problems);
	call_record* pos = sub_problems;

	// create store for partial solutions
	int* block = malloc(sizeof(int) * max_problems * problem->size);
	int used = 0;

	// fill up sub-problem array
	volatile int best = 1<<30;
	int partial[problem->size];
	initActivationRecord(problem, partial, 0, 0, 0, &best, &pos, block, &used, OMP_CUT);

	unsigned num_sub_problems = pos - sub_problems;
	printf("Sub-problem list filled %d/%d\n", num_sub_problems, max_problems);

	// solve sub-problem list within a loop
	#pragma omp parallel for schedule(dynamic, 1)
	for(int i=0; i<num_sub_problems; i++) {
		solve_rec(
			problem,
			&block[sub_problems[i].partial_index],
			sub_problems[i].plant,
			sub_problems[i].used_mask,
			sub_problems[i].cur_cost,
			&best
		);
	}

	// free temporary arrays
	free(sub_problems);
	free(block);

	// return result
	return best;
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

int solve_ocl(problem* problem) {

	// -------------- create list of sub-problems ---------------------

	// get upper boundary for number of sub-problems
	int max_problems = powl(problem->size, OMP_CUT);

	// create array of activation records
	call_record* sub_problems = malloc(sizeof(call_record) * max_problems);
	call_record* pos = sub_problems;

	// create store for partial solutions
	int* block = malloc(sizeof(int) * max_problems * problem->size);
	int used = 0;

	// fill up sub-problem array
	volatile int best = 1<<30;
	int partial[problem->size];
	initActivationRecord(problem, partial, 0, 0, 0, &best, &pos, block, &used, OMP_CUT);

	unsigned num_sub_problems = pos - sub_problems;
	printf("Sub-problem list filled %d/%d\n", num_sub_problems, max_problems);


	// -------------- process list using GPU ---------------------

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

		size_t size;
		clGetDeviceInfo(device, CL_DEVICE_VENDOR, 0, 0, &size);
		char name[size];
		clGetDeviceInfo(device, CL_DEVICE_VENDOR, size*sizeof(char), name, 0); 
		printf("Using Device %s\n", name);
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
		const char* code = loadFile("qap_array.cl");
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
	cl_kernel kernel = clCreateKernel(program, "qap", &error);
	handleErrorCode(error);

	// create buffers
	INFO("Creating Buffers ..");
	cl_mem mA = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, sizeof(int)*problem->size*problem->size, problem->A->data, &error); handleErrorCode(error);
	cl_mem mB = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, sizeof(int)*problem->size*problem->size, problem->B->data, &error); handleErrorCode(error);
	cl_mem vC = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, sizeof(call_record)*num_sub_problems, sub_problems, &error); handleErrorCode(error);
	cl_mem vP = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, sizeof(int)*problem->size*num_sub_problems, block, &error); handleErrorCode(error);
	cl_mem sR = clCreateBuffer(context, CL_MEM_READ_WRITE| CL_MEM_COPY_HOST_PTR, sizeof(int), (void*)&best, &error); handleErrorCode(error);

	// set up arguments
	INFO("Setting up Arguments ..");
	handleErrorCode(clSetKernelArg(kernel, 0, sizeof(int), &problem->size));
	handleErrorCode(clSetKernelArg(kernel, 1, sizeof(mA), &mA));
	handleErrorCode(clSetKernelArg(kernel, 2, sizeof(mB), &mB));
	handleErrorCode(clSetKernelArg(kernel, 3, sizeof(vC), &vC));
	handleErrorCode(clSetKernelArg(kernel, 4, sizeof(vP), &vP));
	handleErrorCode(clSetKernelArg(kernel, 5, sizeof(sR), &sR));
	handleErrorCode(clSetKernelArg(kernel, 6, sizeof(num_sub_problems), &num_sub_problems));

	// run kernel
	INFO("Running kernel ..");
	size_t local_work_size = 64;
	size_t global_work_offset = 0;
	size_t global_work_size = (num_sub_problems + (local_work_size - 1))/local_work_size*local_work_size;
	cl_event kernel_done;
	double start = getTime();
	handleErrorCode(clEnqueueNDRangeKernel(queue, kernel, 1, &global_work_offset, &global_work_size, &local_work_size, 0, 0, &kernel_done));

	handleErrorCode(clWaitForEvents(1,&kernel_done));
	double time = getTime() - start;
	printf("OpenCL Computation Time: %lf sec\n", time);

	// enqueue read operation
	cl_event read_done;
	int res = 0;
	handleErrorCode(clEnqueueReadBuffer(queue, sR, true, 0, sizeof(int), &res, 1, &kernel_done, &read_done));

	// wait for completion
	handleErrorCode(clWaitForEvents(1,&read_done));

	// cleanup
	handleErrorCode(clFinish(queue));
	handleErrorCode(clReleaseKernel(kernel));
	handleErrorCode(clReleaseProgram(program));
	handleErrorCode(clReleaseCommandQueue(queue));
	handleErrorCode(clReleaseContext(context));

	// -------------- free resources and be done ---------------------

	// free temporary arrays
	free(sub_problems);
	free(block);

	// return result
	return res;

/*

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
*/
}

