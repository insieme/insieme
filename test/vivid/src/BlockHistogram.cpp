#ifdef _WIN32
#include "omp.h"
#else
#include "omp_unix.h"
#endif
#include "BlockHistogram.hpp"
#include "BlockHistogramLocal.hpp"

cl_int parameters_histogram_dense2(cl_kernel theKernel, 
	const DeviceMatrixCL* histogram,
	const DeviceMatrixCL* assignments,const DeviceMatrixCL* weights,const int max_bin,
	const int cell_size, const int n_cells_x,
	const int start_y,
	const int start_x)
{
	cl_int err = 0;

    err |= clSetKernelArg(theKernel, 0, sizeof (cl_mem), &histogram->dataMatrix);
	err |= clSetKernelArg(theKernel, 1, sizeof (int), &histogram->height);
    err |= clSetKernelArg(theKernel, 2, sizeof (int), &histogram->width);
	err |= clSetKernelArg(theKernel, 3, sizeof (int), &histogram->pitch);
	err |= clSetKernelArg(theKernel, 4, sizeof (cl_mem), &assignments->dataMatrix);
    err |= clSetKernelArg(theKernel, 5, sizeof (int), &assignments->width);
    err |= clSetKernelArg(theKernel, 6, sizeof (int), &assignments->height);
    err |= clSetKernelArg(theKernel, 7, sizeof (int), &assignments->pitch);
	err |= clSetKernelArg(theKernel, 8, sizeof (cl_mem), &weights->dataMatrix);
    err |= clSetKernelArg(theKernel, 9, sizeof (int), &weights->width);
    err |= clSetKernelArg(theKernel, 10, sizeof (int), &weights->height);
    err |= clSetKernelArg(theKernel, 11, sizeof (int), &weights->pitch);
	err |= clSetKernelArg(theKernel, 12, sizeof (const int), &max_bin);
	err |= clSetKernelArg(theKernel, 13, sizeof (const int), &cell_size);
	err |= clSetKernelArg(theKernel, 14, sizeof (const int), &n_cells_x);
	err |= clSetKernelArg(theKernel, 15, sizeof (const int), &start_y);
	err |= clSetKernelArg(theKernel, 16, sizeof (const int), &start_x);

	return err;
}

cl_int parameters_histogram_dense(
	cl_kernel theKernel,const DeviceMatrixCL3D* histogram,
	const DeviceMatrixCL* assignments,const DeviceMatrixCL* weights,
	const int max_bin, const int cell_size, const int start_y, const int start_x)
{
	cl_int err=0;
	printf("hist x:%d y:%d t:%d py:%d pt:%d assign width:%d height:%d pitch:%d\n", 
		histogram->dim_x, histogram->dim_y, histogram->dim_t, histogram->pitch_y, histogram->pitch_t,
		assignments->width, assignments->height, assignments->pitch);
	printf("weight w:%d h:%d p:%d max_bin:%d cell_size:%d start_y:%d start_x:%d\n",
		weights->width, weights->height, weights->pitch, max_bin, cell_size, start_y, start_x);
    err |= clSetKernelArg(theKernel, 0, sizeof (cl_mem), &histogram->dataMatrix);
	err |= clSetKernelArg(theKernel, 1, sizeof (int), &histogram->dim_x);
    err |= clSetKernelArg(theKernel, 2, sizeof (int), &histogram->dim_y);
	err |= clSetKernelArg(theKernel, 3, sizeof (int), &histogram->dim_t);
    err |= clSetKernelArg(theKernel, 4, sizeof (int), &histogram->pitch_y);
	err |= clSetKernelArg(theKernel, 5, sizeof (int), &histogram->pitch_t);
	err |= clSetKernelArg(theKernel, 6, sizeof (cl_mem), &assignments->dataMatrix);
    err |= clSetKernelArg(theKernel, 7, sizeof (int), &assignments->width);
    err |= clSetKernelArg(theKernel, 8, sizeof (int), &assignments->height);
    err |= clSetKernelArg(theKernel, 9, sizeof (int), &assignments->pitch);
	err |= clSetKernelArg(theKernel, 10, sizeof (cl_mem), &weights->dataMatrix);
    err |= clSetKernelArg(theKernel, 11, sizeof (int), &weights->width);
    err |= clSetKernelArg(theKernel, 12, sizeof (int), &weights->height);
    err |= clSetKernelArg(theKernel, 13, sizeof (int), &weights->pitch);
	err |= clSetKernelArg(theKernel, 14, sizeof (const int), &max_bin);
	err |= clSetKernelArg(theKernel, 15, sizeof (const int), &cell_size);
	err |= clSetKernelArg(theKernel, 16, sizeof (const int), &start_y);
	err |= clSetKernelArg(theKernel, 17, sizeof (const int), &start_x);
	
	/*	const int BS = BLOCK_SIZE;
	 const int B8 = BLOCK_8;
	 err |= clSetKernelArg(theKernel, 16, sizeof (const int), &B8);
	 err |= clSetKernelArg(theKernel, 17, sizeof (const int), &BS);
	 
	 */	
	return err;
}

#ifdef METHOD_2

#define BLOCK_SIZE 16

void cell_histogram_dense_device_cl(
	DeviceMatrixCL* histogram,
	const DeviceMatrixCL* assignments,
	const DeviceMatrixCL* weights,
	const int max_bin,
	const int cell_size,
	const int start_y,
	const int start_x,
	const int n_parts_y,
	const int n_parts_x)
{	
	histogram->zero();
	
	const size_t local_work_size[2] = {BLOCK_SIZE, BLOCK_SIZE}; 

	int grid_ry = (n_parts_y + 1) / 2;
	int grid_cx = (n_parts_x + 1) / 2;
		
	const int n_blocks_x = grid_ry* local_work_size[0];
	const int n_blocks_y = grid_cx* local_work_size[1];
    
	const size_t global_work_size[2] = {n_blocks_x, n_blocks_y};	
	
	assert(histogram->width == max_bin);
		
	TheContext* tc = new TheContext();
	cl_context GPUContext = tc->getMyContext()->getContextCL();
	cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();
	
	MyKernels *kernels = new MyKernels(GPUContext,cdDevice);
	
	//cl_kernel theKernel = kernels->getCellHistogramKernel2();
	cl_kernel theKernel = kernels->getCellHistogramKernel3();
	
	cl_int err;
	err=0;
	
    err =  parameters_histogram_dense2(
		theKernel, histogram, assignments,
		weights, max_bin, cell_size, n_parts_x, start_y, start_x);	
  	
    if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 3! %d\n", err);
        exit(1);
    }
	
	err = clEnqueueNDRangeKernel(tc->getMyContext()->cqCommandQueue, theKernel, 2, NULL, 
								 global_work_size, local_work_size, 0, NULL, NULL);
	clFinish(tc->getMyContext()->cqCommandQueue);// to make sure the kernel completed

    if (err) {
        printf("Error: Failed to execute kernel! %d\n", err);
        exit(1);
    }
}

void cell_histogram_dense_device_cl(DeviceMatrixCL3D* histogram,
                                 const DeviceMatrixCL* assignments,
                                 const DeviceMatrixCL* weights,
                                 const int max_bin,
                                 const int cell_size,
                                 const int start_y,
                                 const int start_x)
{
	
	histogram->zero();
	
	const size_t local_work_size[2] = {BLOCK_SIZE, BLOCK_SIZE}; 
	
	int grid_ry = (histogram->dim_t + 1) / 2;
    int grid_cx = (histogram->dim_y + 1) / 2;

	
	const int n_blocks_x = grid_ry* local_work_size[0];
	const int n_blocks_y = grid_cx* local_work_size[1];
    
	const size_t global_work_size[2] = {n_blocks_x, n_blocks_y};	
	
	assert(histogram->dim_x == max_bin);
	
	
	TheContext* tc = new TheContext();
	cl_context GPUContext = tc->getMyContext()->getContextCL();
	cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();
	
	MyKernels *kernels = new MyKernels(GPUContext,cdDevice);
	
	cl_kernel theKernel= kernels->getCellHistogramKernel2();
	
	
	cl_int err;
	err=0;
	
    err =  parameters_histogram_dense(theKernel, histogram, assignments,
												weights,max_bin,cell_size,start_y,
												start_x);	
  	
    if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 3! %d\n", err);
        exit(1);
    }
	
	err = clEnqueueNDRangeKernel(tc->getMyContext()->cqCommandQueue, 
								 theKernel, 2, NULL, 
								 global_work_size, local_work_size, 0, NULL, NULL);
	clFinish(tc->getMyContext()->cqCommandQueue);// to make sure the kernel completed

    if (err) {
        printf("Error: Failed to execute kernel! %d\n", err);
        exit(1);
    }
}

#endif


/*DeviceMatrix3D::Ptr cell_histogram_dense_cuda(
    const DeviceMatrix::Ptr& assignment_mat,
    const DeviceMatrix::Ptr& weight_mat,
    const int max_bin, const int cell_size, 
	const int start_y, const int start_x,
	const int stop_y, const int stop_x)
    //object& start_inds, object& stop_inds)
{
    
    //NumPyArray start_arr(start_inds);
    //NumPyArray stop_arr(stop_inds);

    //const int start_y = (int) start_arr.data()[0];
    //const int start_x = (int) start_arr.data()[1];

    //const int stop_y = (int) stop_arr.data()[0];
    //const int stop_x = (int) stop_arr.data()[1];


    int n_parts_y = (stop_y - start_y) / cell_size;

    int n_parts_x = (stop_x - start_x) / cell_size;

    #ifdef METHOD_2
    n_parts_y += (n_parts_y % 2);
    n_parts_x += (n_parts_x % 2);
    #endif

    DeviceMatrix3D::Ptr histogram = makeDeviceMatrix3D(
        n_parts_y, n_parts_x, max_bin);

    cell_histogram_dense_device(histogram.get(),
                                assignment_mat.get(),
                                weight_mat.get(),
                                max_bin,
                                cell_size,
                                start_y, start_x);
   
    return histogram;
}*/

DeviceMatrixCL::Ptr cell_histogram_dense_cl(
	const DeviceMatrixCL3D::Ptr& ff_out,
	const int max_bin, const int cell_size, 
	const int start_y, const int start_x,
	const int stop_y, const int stop_x)
{
	DeviceMatrixCL::Ptr assignment_mat = makeDeviceMatrixCL(*ff_out, 0);
	DeviceMatrixCL::Ptr weight_mat = makeDeviceMatrixCL(*ff_out, 1);
	
	int n_parts_y = (stop_y - start_y) / cell_size;
    int n_parts_x = (stop_x - start_x) / cell_size;
	
#ifdef METHOD_2
    n_parts_y += (n_parts_y % 2);
    n_parts_x += (n_parts_x % 2);
#endif
	
    DeviceMatrixCL::Ptr histogram = makeDeviceMatrixCL(n_parts_y * n_parts_x, max_bin);
//		for(int i=0; i<10000; i++)
	cell_histogram_dense_device_cl(
		histogram.get(),
		assignment_mat.get(),
		weight_mat.get(),
		max_bin,
		cell_size,
		start_y, start_x,
		n_parts_y, n_parts_x);

    return histogram;
}
DeviceMatrixCL3D::Ptr cell_histogram_dense_cl(
											const DeviceMatrixCL::Ptr& assignment_mat,
											const DeviceMatrixCL::Ptr& weight_mat,
											const int max_bin, const int cell_size, 
											const int start_y, const int start_x,
											const int stop_y, const int stop_x)
											  //object& start_inds, object& stop_inds)
{
    
    //NumPyArray start_arr(start_inds);
    //NumPyArray stop_arr(stop_inds);
	
    //const int start_y = (int) start_arr.data()[0];
    //const int start_x = (int) start_arr.data()[1];
	
    //const int stop_y = (int) stop_arr.data()[0];
    //const int stop_x = (int) stop_arr.data()[1];
	
	
    int n_parts_y = (stop_y - start_y) / cell_size;
	
    int n_parts_x = (stop_x - start_x) / cell_size;
	
#ifdef METHOD_2
    n_parts_y += (n_parts_y % 2);
    n_parts_x += (n_parts_x % 2);
#endif
	
    DeviceMatrixCL3D::Ptr histogram = makeDeviceMatrixCL3D(n_parts_y, n_parts_x, max_bin);

    cell_histogram_dense_device_cl(histogram.get(),
                                assignment_mat.get(),
                                weight_mat.get(),
                                max_bin,
                                cell_size,
                                start_y, start_x);
    return histogram;
}


#ifdef METHOD_1



void cell_histogram_dense_device_cl(DeviceMatrixCL3D* histogram,
                                 const DeviceMatrixCL* assignments,
                                 const DeviceMatrixCL* weights,
                                 const int max_bin,
                                 const int cell_size,
                                 const int start_y,
                                 const int start_x)
{
  histogram->zero();
	
  const size_t local_work_size[2] = {cell_size, cell_size}; 
	
  int grid_ry = histogram->dim_t;
  int grid_cx = histogram->dim_y;

  const int n_blocks_x = grid_ry* local_work_size[0];
  const int n_blocks_y = grid_cx* local_work_size[1];
    
  const size_t global_work_size[2] = {n_blocks_x, n_blocks_y};	

   assert(histogram->dim_x == max_bin);
	
	
	TheContext* tc = new TheContext();
	cl_context GPUContext = tc->getMyContext()->getContextCL();
	cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();
	
	MyKernels *kernels = new MyKernels(GPUContext,cdDevice);
	
	cl_kernel theKernel= kernels->getCellHistogramKernel1();
	
	cl_int err;
	err=0;
	
    err =  parameters_histogram_dense(theKernel, histogram, assignments,
									  weights,max_bin,cell_size,start_y,
									  start_x);	
  	
    if (err != CL_SUCCESS) {
        printf("Error: Failed to set kernel arguments 3! %d\n", err);
        exit(1);
    }
	
	
	err = clEnqueueNDRangeKernel(tc->getMyContext()->cqCommandQueue, 
								 theKernel, 2, NULL, 
								 global_work_size, local_work_size, 0, NULL, NULL);
	
    if (err) {
        printf("Error: Failed to execute kernel! %d\n", err);
        exit(1);
    }
/*	
    dim3 dimBlock(cell_size, cell_size);
    
    int grid_ry = histogram->dim_t;
    int grid_cx = histogram->dim_y;
    
    dim3 dimGrid(grid_cx, grid_ry);
	
    assert(histogram->dim_x == max_bin);
	
    cellHistogramKernel<<<dimGrid, dimBlock>>>(
											   *histogram, *assignments, *weights,
											   start_y, start_x,
											   max_bin);
  */
}

#endif
