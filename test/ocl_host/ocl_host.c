#include <CL/cl.h>
#include <stdio.h>
#define NUM_DATA 100

int main(int argc, char **argv)
{
        cl_int _err;
        cl_platform_id platforms[100];
        cl_uint platforms_n = 1;
        _err = clGetPlatformIDs(100, platforms, &platforms_n);
	
	//printf("plat = %d\n",platforms_n);
        
	if (platforms_n == 0)
                return 1;

        cl_device_id devices[100];
        cl_uint devices_n = 1;
        _err = clGetDeviceIDs(platforms[0], CL_DEVICE_TYPE_ALL, 100, &(devices[0]), &devices_n);
	//printf("dev = %d\n", devices_n);

        if (devices_n == 0)
                return 1;

        cl_context context = clCreateContext(NULL, 1, devices, NULL, NULL, &_err);
	
	const char *program_source[] = {
		"__kernel void fun(__global int *src, __global int *dst, int factor)\n"
		"{\n"
		"	int i = get_global_id(0);\n"
		"	dst[i] = src[i] * factor;\n"
		"}\n"
	};
	
	cl_program program;	
	#pragma insieme kernelFile "kernel.cl"
        program = clCreateProgramWithSource(context, sizeof(program_source)/sizeof(*program_source), program_source, NULL, &_err);
	//if (_err != CL_SUCCESS) printf("FAIL2 %d\n", _err);
	
	if (clBuildProgram(program, 1, devices, "", NULL, NULL) != CL_SUCCESS) {
                char buffer[10240];
                clGetProgramBuildInfo(program, devices[0], CL_PROGRAM_BUILD_LOG, sizeof(buffer), buffer, NULL);
                printf("CL Compilation failed:\n%s", buffer);
        }
        
	cl_mem input_buffer = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(int)*100, NULL, &_err);

        cl_mem output_buffer;
	output_buffer  = clCreateBuffer(context, CL_MEM_WRITE_ONLY, sizeof(int)*100, NULL, &_err);

        int factor = 4;

        cl_kernel kernel = clCreateKernel(program, "fun", &_err);
	clSetKernelArg(kernel, 0, sizeof(input_buffer), &input_buffer);
        clSetKernelArg(kernel, 1, sizeof(output_buffer), &output_buffer);
        clSetKernelArg(kernel, 2, sizeof(factor), &factor);

        cl_command_queue queue;
        queue = clCreateCommandQueue(context, devices[0], 0, &_err);

        int i = 0;
        while(i < NUM_DATA) {
            clEnqueueWriteBuffer(queue, input_buffer, CL_TRUE, 0, sizeof(int), &i, 0, NULL, NULL);
        	++i;
        }

        cl_event kernel_completion;
        size_t global_work_size[1] = { NUM_DATA };
        size_t local_work_size[1] = { 1 };

        clEnqueueNDRangeKernel(queue, kernel, 1, NULL, global_work_size, local_work_size, 0, NULL, &kernel_completion);
        clWaitForEvents(1, &kernel_completion);
        clReleaseEvent(kernel_completion);

        printf("Result: ");
	int error = 0;
        for (int i=0; i<NUM_DATA; i++) {
                int data;
                clEnqueueReadBuffer(queue, output_buffer, CL_TRUE, i*sizeof(int), sizeof(int), &data, 0, NULL, NULL);
		if(data/factor != i) error = 1;
        }
        printf("%s\n", error ? "NOT CORRECT" : "CORRECT");

        clReleaseMemObject(input_buffer);
        clReleaseMemObject(output_buffer);

        clReleaseKernel(kernel);
        clReleaseProgram(program);
        clReleaseContext(context);
        return 0;

}
