#include "PairwiseDistance.hpp"
#include "PairwiseDistanceLocal.hpp"

#ifdef _WIN32
#include "omp.h"
#else
#include "omp_unix.h"
#endif

/*DeviceMatrix::Ptr pwdist_cuda( const DeviceMatrix::Ptr& features_train,
        const DeviceMatrix::Ptr& features_test){

    DeviceMatrix::Ptr out = makeDeviceMatrix(features_train->height,
            features_test->height);
    double tic = omp_get_wtime();
    pwdist_generic(features_train.get(), features_test.get(), out.get(), EUCLIDEAN);
    double toc = omp_get_wtime();

    //std::cout << "CUDA Time: " << toc - tic << std::endl;

    return out;
}

DeviceMatrix::Ptr pwdot_cuda( const DeviceMatrix::Ptr& features_train,
        const DeviceMatrix::Ptr& features_test){

    DeviceMatrix::Ptr out = makeDeviceMatrix(features_train->height,
            features_test->height);
    pwdist_generic(features_train.get(), features_test.get(), out.get(), DOTPRODUCT);
    return out;
}

DeviceMatrix::Ptr pwabsdot_cuda( const DeviceMatrix::Ptr& features_train,
        const DeviceMatrix::Ptr& features_test){

    DeviceMatrix::Ptr out = makeDeviceMatrix(features_train->height,
            features_test->height);
    pwdist_generic(features_train.get(), features_test.get(), out.get(), ABSDOTPRODUCT);
    return out;
}

DeviceMatrix::Ptr pwchisq_cuda( const DeviceMatrix::Ptr& features_train,
        const DeviceMatrix::Ptr& features_test){

    DeviceMatrix::Ptr out = makeDeviceMatrix(features_train->height,
            features_test->height);
    pwdist_generic(features_train.get(), features_test.get(), out.get(), CHISQUARED);
    return out;
}

DeviceMatrix::Ptr pwcityblock_cuda( const DeviceMatrix::Ptr& features_train,
        const DeviceMatrix::Ptr& features_test){

    DeviceMatrix::Ptr out = makeDeviceMatrix(features_train->height,
            features_test->height);
    pwdist_generic(features_train.get(), features_test.get(), out.get(), CITYBLOCK);
    return out;
}

DeviceMatrix::Ptr argmin_cuda(const DeviceMatrix::Ptr& matrix)
{
    DeviceMatrix::Ptr out = makeDeviceMatrix(matrix->height, 1);
    argmin_cuda_local(matrix.get(), out.get());
    return out;
}

DeviceMatrix::Ptr argmax_cuda(const DeviceMatrix::Ptr& matrix)
{
    DeviceMatrix::Ptr out = makeDeviceMatrix(matrix->height, 1);
    argmax_cuda_local(matrix.get(), out.get());
    return out;
}

DeviceMatrix::Ptr min_cuda(const DeviceMatrix::Ptr& matrix)
{
    DeviceMatrix::Ptr out = makeDeviceMatrix(matrix->height, 1);
    min_cuda_local(matrix.get(), out.get());
    return out;
}

DeviceMatrix::Ptr max_cuda(const DeviceMatrix::Ptr& matrix)
{
    DeviceMatrix::Ptr out = makeDeviceMatrix(matrix->height, 1);
    max_cuda_local(matrix.get(), out.get());
    return out;
}*/

/**
  OpenCL function
**/

DeviceMatrixCL::Ptr pwdist_cl( const DeviceMatrixCL::Ptr& features_train,
        const DeviceMatrixCL::Ptr& features_test){

//	double tic = omp_get_wtime();
    DeviceMatrixCL::Ptr out = makeDeviceMatrixCL(features_train->height,
            features_test->height);

    // double tic = omp_get_wtime();
    // pwdist_genericCL(features_train.get(), features_test.get(), out.get(), EUCLIDEAN);
//	for(int i=0; i<10000; i++)
	pwdist_eucCL(features_train.get(), features_test.get(), out.get());

 //   double toc = omp_get_wtime();
 //   std::cout << "OpenCL time: " << toc - tic << std::endl;
    return out;
}

DeviceMatrixCL::Ptr pwdot_cl( const DeviceMatrixCL::Ptr& features_train,
        const DeviceMatrixCL::Ptr& features_test){

    DeviceMatrixCL::Ptr out = makeDeviceMatrixCL(features_train->height,
            features_test->height);
    pwdist_genericCL(features_train.get(), features_test.get(), out.get(), DOTPRODUCT);
    return out;
}

DeviceMatrixCL::Ptr pwabsdot_cl( const DeviceMatrixCL::Ptr& features_train,
        const DeviceMatrixCL::Ptr& features_test){

    DeviceMatrixCL::Ptr out = makeDeviceMatrixCL(features_train->height,
            features_test->height);
    pwdist_genericCL(features_train.get(), features_test.get(), out.get(), ABSDOTPRODUCT);
    return out;
}

DeviceMatrixCL::Ptr pwchisq_cl( const DeviceMatrixCL::Ptr& features_train,
        const DeviceMatrixCL::Ptr& features_test){

    DeviceMatrixCL::Ptr out = makeDeviceMatrixCL(features_train->height,
            features_test->height);
    pwdist_genericCL(features_train.get(), features_test.get(), out.get(), CHISQUARED);
    return out;
}

DeviceMatrixCL::Ptr pwcityblock_cl( const DeviceMatrixCL::Ptr& features_train,
        const DeviceMatrixCL::Ptr& features_test){

    DeviceMatrixCL::Ptr out = makeDeviceMatrixCL(features_train->height,
            features_test->height);
    pwdist_genericCL(features_train.get(), features_test.get(), out.get(), CITYBLOCK);
    return out;
}

DeviceMatrixCL::Ptr argmin_cl(const DeviceMatrixCL::Ptr& matrix)
{
    DeviceMatrixCL::Ptr out = makeDeviceMatrixCL(matrix->height, 1);
    argmin_cl_local(matrix.get(), out.get());
    return out;
}

DeviceMatrixCL::Ptr argmax_cl(const DeviceMatrixCL::Ptr& matrix)
{
    DeviceMatrixCL::Ptr out = makeDeviceMatrixCL(matrix->height, 1);
    argmax_cl_local(matrix.get(), out.get());
    return out;
}

DeviceMatrixCL::Ptr min_cl(const DeviceMatrixCL::Ptr& matrix)
{
    DeviceMatrixCL::Ptr out = makeDeviceMatrixCL(matrix->height, 1);
    min_cl_local(matrix.get(), out.get());
    return out;
}

DeviceMatrixCL::Ptr max_cl(const DeviceMatrixCL::Ptr& matrix)
{
    DeviceMatrixCL::Ptr out = makeDeviceMatrixCL(matrix->height, 1);
    max_cl_local(matrix.get(), out.get());
    return out;
}


/**
  This code was in the cu file of the cuda version
 **/

static const unsigned int BLOCK_SIZE = 16;

void pwdist_genericCL(const DeviceMatrixCL* features_train,
        const DeviceMatrixCL* features_test,
        DeviceMatrixCL* output,
        int type) {

    TheContext* tc = new TheContext();

    cl_context GPUContext = tc->getMyContext()->getContextCL();
    cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();

    // Creates the program
    // Uses NVIDIA helper functions to get the code string and it's size (in bytes)
  	
	MyKernels *kernels = new MyKernels(GPUContext,cdDevice);
	
	cl_kernel theKernel= kernels->getPairwiseDistanceKernel();
	cl_int err;
	err=0;
	
    err |= clSetKernelArg(theKernel, 0, sizeof (cl_mem), &features_train->dataMatrix);
    err |= clSetKernelArg(theKernel, 1, sizeof (int), &features_train->width);
    err |= clSetKernelArg(theKernel, 2, sizeof (int), &features_train->height);
    err |= clSetKernelArg(theKernel, 3, sizeof (int), &features_train->pitch);
	
	
    err |= clSetKernelArg(theKernel, 4, sizeof (cl_mem), &features_test->dataMatrix);
    err |= clSetKernelArg(theKernel, 5, sizeof (int), &features_test->width);
    err |= clSetKernelArg(theKernel, 6, sizeof (int), &features_test->height);
    err |= clSetKernelArg(theKernel, 7, sizeof (int), &features_test->pitch);
	
    err |= clSetKernelArg(theKernel, 8, sizeof (cl_mem), &output->dataMatrix);
    err |= clSetKernelArg(theKernel, 9, sizeof (int), &output->width);
    err |= clSetKernelArg(theKernel, 10, sizeof (int), &output->height);
    err |= clSetKernelArg(theKernel, 11, sizeof (int), &output->pitch);
	
    err |= clSetKernelArg(theKernel, 12, sizeof (int), &type);
    err |= clSetKernelArg(theKernel, 13, sizeof (int), &BLOCK_SIZE);
	
    if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 3! %d\n", err);
        exit(1);
    }
	

    const int n_blocks_x = ((features_train->height - 1) / BLOCK_SIZE + 1) * BLOCK_SIZE;
    const int n_blocks_y = ((features_test->height - 1) / BLOCK_SIZE + 1) * BLOCK_SIZE;

    const size_t local_work_size[2] = {BLOCK_SIZE, BLOCK_SIZE}; 
    const size_t global_work_size[2] = {n_blocks_x, n_blocks_y};

    //std::cout << "Threads: " << local_work_size[0] << ", " << local_work_size[1] << std::endl;
    //std::cout << "Blocks: " << global_work_size[0] << ", " << global_work_size[1] << std::endl;

    err = clEnqueueNDRangeKernel(tc->getMyContext()->cqCommandQueue, 
            theKernel, 2, NULL, 
            global_work_size, local_work_size, 0, NULL, NULL);

    if (err) {
        printf("Error: Failed to execute kernel! %d\n", err);
        exit(1);
    }
}

void pwdist_eucCL(const DeviceMatrixCL* features_train,
        const DeviceMatrixCL* features_test,
        DeviceMatrixCL* output) {

    TheContext* tc = new TheContext();

    cl_context GPUContext = tc->getMyContext()->getContextCL();
    cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();

    // Creates the program
    // Uses NVIDIA helper functions to get the code string and it's size (in bytes)
  	
	MyKernels *kernels = new MyKernels(GPUContext,cdDevice);
	
	cl_kernel theKernel= kernels->getPairwiseDistanceKernel();
	cl_int err;
	err=0;
	/*
	size_t local;
	err = clGetKernelWorkGroupInfo(theKernel, cdDevice, CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE, sizeof(local), &local, NULL);
    if (err != CL_SUCCESS)
    {
        printf("Error: Failed to retrieve kernel work group info! %d\n", err);
        exit(1);
    }
	printf("simultaneous threads num:%d\n", local);
	*/

	int f_pitch=0;
    err |= clSetKernelArg(theKernel, 0, sizeof (cl_mem), &features_train->dataMatrix);
    err |= clSetKernelArg(theKernel, 1, sizeof (int), &features_train->width);
   // err |= clSetKernelArg(theKernel, 2, sizeof (int), &features_train->height);
	f_pitch = features_train->pitch/sizeof(float);
    err |= clSetKernelArg(theKernel, 2, sizeof (int), &f_pitch);
	
	
    err |= clSetKernelArg(theKernel, 3, sizeof (cl_mem), &features_test->dataMatrix);
  //  err |= clSetKernelArg(theKernel, 5, sizeof (int), &features_test->width);
  //  err |= clSetKernelArg(theKernel, 6, sizeof (int), &features_test->height);
	f_pitch = features_test->pitch/sizeof(float);
    err |= clSetKernelArg(theKernel, 4, sizeof (int), &f_pitch);
	
    err |= clSetKernelArg(theKernel, 5, sizeof (cl_mem), &output->dataMatrix);
 //   err |= clSetKernelArg(theKernel, 9, sizeof (int), &output->width);
 //   err |= clSetKernelArg(theKernel, 10, sizeof (int), &output->height);
	f_pitch = output->pitch/sizeof(float);
    err |= clSetKernelArg(theKernel, 6, sizeof (int), &f_pitch);
	
   // err |= clSetKernelArg(theKernel, 12, sizeof (int), &type);
 //   err |= clSetKernelArg(theKernel, 12, sizeof (int), &BLOCK_SIZE);
//	printf("params: %d %d %d,  %d %d %d, %d %d %d-- %d\n",features_train->width,
//		features_train->height,features_train->pitch, features_test->width, features_test->height,
//		features_test->pitch, output->width, output->height, output->pitch, BLOCK_SIZE);
    if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 3! %d\n", err);
        exit(1);
    }
	

    const int n_blocks_x = ((features_train->height - 1) / BLOCK_SIZE + 1) * BLOCK_SIZE;
    const int n_blocks_y = ((features_test->height - 1) / BLOCK_SIZE + 1) * BLOCK_SIZE;

    const size_t local_work_size[2] = {BLOCK_SIZE, BLOCK_SIZE}; 
    const size_t global_work_size[2] = {n_blocks_x, n_blocks_y};

  //  std::cout << "Threads: " << local_work_size[0] << ", " << local_work_size[1] << std::endl;
  //  std::cout << "Blocks: " << global_work_size[0] << ", " << global_work_size[1] << std::endl;

//	double tic = omp_get_wtime();
//	for(int i=0; i<6000; i++) 
	{
    err = clEnqueueNDRangeKernel(tc->getMyContext()->cqCommandQueue, 
            theKernel, 2, NULL, 
            global_work_size, local_work_size, 0, NULL, NULL);
	clFinish(tc->getMyContext()->cqCommandQueue);// to make sure the kernel completed
	}
//	double toc = omp_get_wtime();
//	std::cout << "OpenCL time: " << toc - tic << std::endl;


    if (err) {
        printf("Error: Failed to execute kernel! %d\n", err);
        exit(1);
    }
}

cl_int parameters_minmax_local(cl_kernel theKernel,const DeviceMatrixCL* matrix, DeviceMatrixCL* output){
	cl_int err=0;
	
    err |= clSetKernelArg(theKernel, 0, sizeof (cl_mem), &matrix->dataMatrix);
  
	if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 1! %d\n", err);
        exit(1);
    }
	
	err |= clSetKernelArg(theKernel, 1, sizeof (int), &matrix->width);
    
	if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 2! %d\n", err);
        exit(1);
    }
	
	err |= clSetKernelArg(theKernel, 2, sizeof (int), &matrix->height);
    
	if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 3! %d\n", err);
        exit(1);
    }
	
	err |= clSetKernelArg(theKernel, 3, sizeof (int), &matrix->pitch);
	
	if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 4! %d\n", err);
        exit(1);
    }
	
    err |= clSetKernelArg(theKernel, 4, sizeof (cl_mem), &output->dataMatrix);
    
	if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 5! %d\n", err);
        exit(1);
    }
	
	err |= clSetKernelArg(theKernel, 5, sizeof (int), &output->width);
    
	if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 6! %d\n", err);
        exit(1);
    }
	
	err |= clSetKernelArg(theKernel, 6, sizeof (int), &output->height);
    
	if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 7! %d\n", err);
        exit(1);
    }
	
	err |= clSetKernelArg(theKernel, 7, sizeof (int), &output->pitch);
	
  	
    if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 8! %d\n", err);
        exit(1);
    }
	return err;
}

void argmin_cl_local(const DeviceMatrixCL* matrix, DeviceMatrixCL* output)
{
   
	TheContext* tc = new TheContext();
	
    cl_context GPUContext = tc->getMyContext()->getContextCL();
    cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();
	
    // Creates the program
    // Uses NVIDIA helper functions to get the code string and it's size (in bytes)
  	
	MyKernels *kernels = new MyKernels(GPUContext,cdDevice);
	
	cl_kernel theKernel= kernels->getArgminKernel();
	
	
	cl_int err;
		
	err =  parameters_minmax_local(theKernel, matrix, output);
	
	const size_t local_work_size[2] = {256, 1}; 
	
	const int n_blocks_x =  ((matrix->height-1) / local_work_size[0] + 1)* local_work_size[0];
	const int n_blocks_y = 1;
	
    
    const size_t global_work_size[2] = {n_blocks_x, n_blocks_y};
	
	err = clEnqueueNDRangeKernel(tc->getMyContext()->cqCommandQueue, 
								 theKernel, 2, NULL, 
								 global_work_size, local_work_size, 0, NULL, NULL);
	
    if (err) {
        printf("Error: Failed to execute kernel! %d\n", err);
        exit(1);
    }
}

void argmax_cl_local(const DeviceMatrixCL* matrix, DeviceMatrixCL* output)
{
	
	TheContext* tc = new TheContext();
	
    cl_context GPUContext = tc->getMyContext()->getContextCL();
    cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();
	
    // Creates the program
    // Uses NVIDIA helper functions to get the code string and it's size (in bytes)
  	
	MyKernels *kernels = new MyKernels(GPUContext,cdDevice);
	
	cl_kernel theKernel= kernels->getArgmaxKernel();
	
	
	cl_int err;
	err=0;
	
   err =  parameters_minmax_local(theKernel, matrix, output);	
  	
    if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 3! %d\n", err);
        exit(1);
    }
	
	
	const size_t local_work_size[2] = {256, 1}; 
	
	const int n_blocks_x =  ((matrix->height-1) / local_work_size[0] + 1)* local_work_size[0];
	const int n_blocks_y = 1;
	
    
    const size_t global_work_size[2] = {n_blocks_x, n_blocks_y};
	
	err = clEnqueueNDRangeKernel(tc->getMyContext()->cqCommandQueue, 
								 theKernel, 2, NULL, 
								 global_work_size, local_work_size, 0, NULL, NULL);
	
    if (err) {
        printf("Error: Failed to execute kernel! %d\n", err);
        exit(1);
    }
}

void max_cl_local(const DeviceMatrixCL* matrix, DeviceMatrixCL* output)
{
	
	TheContext* tc = new TheContext();
	
    cl_context GPUContext = tc->getMyContext()->getContextCL();
    cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();
	
    // Creates the program
    // Uses NVIDIA helper functions to get the code string and it's size (in bytes)
  	
	MyKernels *kernels = new MyKernels(GPUContext,cdDevice);
	
	cl_kernel theKernel= kernels->getMaxKernel();
	
	cl_int err;
	err=0;
	
    err =  parameters_minmax_local(theKernel, matrix, output);	
  	
    if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 3! %d\n", err);
        exit(1);
    }
	
	
	const size_t local_work_size[2] = {256, 1}; 
	
	const int n_blocks_x =  ((matrix->height-1) / local_work_size[0] + 1)* local_work_size[0];
	const int n_blocks_y = 1;
	
    
    const size_t global_work_size[2] = {n_blocks_x, n_blocks_y};
	
	err = clEnqueueNDRangeKernel(tc->getMyContext()->cqCommandQueue, 
								 theKernel, 2, NULL, 
								 global_work_size, local_work_size, 0, NULL, NULL);
	
    if (err) {
        printf("Error: Failed to execute kernel! %d\n", err);
        exit(1);
    }
}

void min_cl_local(const DeviceMatrixCL* matrix, DeviceMatrixCL* output)
{
	
	TheContext* tc = new TheContext();
	
    cl_context GPUContext = tc->getMyContext()->getContextCL();
    cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();
	
    // Creates the program
    // Uses NVIDIA helper functions to get the code string and it's size (in bytes)
  	
	MyKernels *kernels = new MyKernels(GPUContext,cdDevice);
	
	cl_kernel theKernel= kernels->getMinKernel();
	
	
	cl_int err;
	err=0;
	
    err =  parameters_minmax_local(theKernel, matrix, output);	
  	
    if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 3! %d\n", err);
        exit(1);
    }
	
	
	const size_t local_work_size[2] = {256, 1}; 
	
	const int n_blocks_x =  ((matrix->height-1) / local_work_size[0] + 1)* local_work_size[0];
	const int n_blocks_y = 1;
	
    
    const size_t global_work_size[2] = {n_blocks_x, n_blocks_y};
	
	err = clEnqueueNDRangeKernel(tc->getMyContext()->cqCommandQueue, 
								 theKernel, 2, NULL, 
								 global_work_size, local_work_size, 0, NULL, NULL);
	
    if (err) {
        printf("Error: Failed to execute kernel! %d\n", err);
        exit(1);
    }
}
