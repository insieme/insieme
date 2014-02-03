#include <iostream>

//#include <cuda_runtime.h>

#include <CL/opencl.h>
#include <CL/cl.h>

#include "DeviceMatrix.hpp"

#include "exceptions.hpp"

// Cribbed (and modified) from cutil.h (can't seem to include the
// whole thing)
/*#  define CUDA_SAFE_CALL_NO_SYNC( call) do {                                 \
    cudaError_t err = call;                                                  \
    if( cudaSuccess != err) {                                                \
        fprintf(stderr, "Cuda error in file '%s' in line %i : %s.\n",        \
                __FILE__, __LINE__, cudaGetErrorString( err) );              \
        exit(EXIT_FAILURE);                                                  \
    } } while (0)

static void deleteDeviceMatrix(DeviceMatrix* mat)
{
	//printf("cudaFree: %p\n", mat->data);
	//
	CUDA_CALL(cudaFree(mat->data));
	delete mat;
}

boost::shared_ptr<DeviceMatrix> makeDeviceMatrix(size_t height,	size_t width)
{
		DeviceMatrix* mat = new DeviceMatrix();
		mat->width = width;
		mat->height = height;
		CUDA_CALL
			(cudaMallocPitch((void**)&mat->data, &mat->pitch,
			mat->width * sizeof(float),
			mat->height));

		// I can't imagine getting a pitch that's not a multiple of a float
		assert(mat->pitch % sizeof(float) == 0);
		// We want to express everything in floats
		mat->pitch /= sizeof(float);

		//printf("cudaMalloc: %p\n", mat->data);

		return boost::shared_ptr<DeviceMatrix>(mat, deleteDeviceMatrix);
}

void DeviceMatrix_copyToDevice(DeviceMatrix& self, const float* data)
{
	if ((self.width > 0) && (self.height > 0)) {
		const size_t widthInBytes = self.width * sizeof(float);
		CUDA_SAFE_CALL_NO_SYNC
			(cudaMemcpy2D(self.data, self.pitch * sizeof(float),
			data, widthInBytes,
			widthInBytes, self.height,
			cudaMemcpyHostToDevice));
	}
}

void DeviceMatrix_copyFromDevice(const DeviceMatrix& self, float* dst)
{
	if ((self.width > 0) && (self.height > 0)) {
		const size_t widthInBytes = self.width * sizeof(float);
		CUDA_SAFE_CALL_NO_SYNC
			(cudaMemcpy2D(dst, widthInBytes,
			self.data, self.pitch * sizeof(float),
			widthInBytes, self.height,
			cudaMemcpyDeviceToHost));
	}
}*/

static void deleteDeviceMatrixCL(DeviceMatrixCL* mat)
{
	cl_int err = clReleaseMemObject(mat->dataMatrix);
	if (err)
	{
		printf("Error releasing DeviceMatrixCL object (CL Error: %d", err);
	}

	delete mat;
}

boost::shared_ptr<DeviceMatrixCL> makeDeviceMatrixCL(DeviceMatrixCL3D& src, const int slice)
{
	const int height = src.dim_y;
	const int width = src.dim_x;

	DeviceMatrixCL* mat = new DeviceMatrixCL();
	mat->width = width;
	mat->height = height;

	//size_t mem_size = width * height 

	size_t buffer_region[2] = {src.pitch_t * slice, src.pitch_t};

	cl_int err;

	mat->dataMatrix = clCreateSubBuffer(src.dataMatrix, CL_MEM_READ_WRITE, CL_BUFFER_CREATE_TYPE_REGION, buffer_region, &err);
	
	mat->pitch = src.pitch_y;

	return boost::shared_ptr<DeviceMatrixCL>(mat, deleteDeviceMatrixCL);
}

boost::shared_ptr<DeviceMatrixCL> makeDeviceMatrixCL(size_t height, size_t width)
{
	DeviceMatrixCL* mat = new DeviceMatrixCL();
	mat->width = width;
	mat->height = height;

	TheContext * tc = new TheContext();

	cl_context GPUContext = tc->getMyContext()->getContextCL();
	cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();

	/*The optimal pitch is computed by (1) getting the base address alignment
	preference for your card (CL_DEVICE_MEM_BASE_ADDR_ALIGN property with
	clGetDeviceInfo: note that the returned value is in bits, so you have
	to divide by 8 to get it in bytes);*/

	int buffer;
	cl_int prueba = clGetDeviceInfo(cdDevice, CL_DEVICE_MEM_BASE_ADDR_ALIGN , sizeof(buffer), &buffer, NULL);
	buffer /= 8;

	int naturalPitch = sizeof(float) * mat->width;

	/*let's call this base (2) find the largest multiple of base
	that is no less than your natural
	data pitch (sizeof(type) times number of columns);*/

	int devicepitch = ceil(float(naturalPitch)/buffer) * buffer;

	//printf("Pitch: %d, DevicePitch: %d, Buffer: %d\n", naturalPitch, devicepitch, buffer);

	mat->pitch = devicepitch;

	//You then allocate pitch times number of rows bytes, and pass the pitch information to kernels.

	//std::cout << height << "\t" << devicepitch << std::endl;

	const int mem_size = (mat->height+16) * mat->pitch;

	//std::cout << "Mem size: " << mem_size << std::endl;

	int err;

	mat->dataMatrix = clCreateBuffer(GPUContext, CL_MEM_READ_WRITE, mem_size, NULL, &err);
	if(err!=0)
	{
		printf("Error Code create buffer: %d\n",err);
	}

	return boost::shared_ptr<DeviceMatrixCL>(mat, deleteDeviceMatrixCL);
}

void DeviceMatrixCL_copyToDevice(DeviceMatrixCL& self, const float* data)
{
	const int mem_size = self.height * self.pitch;
	TheContext * tc = new TheContext();

	size_t buffer_origin[3] = {0,0,0};
	size_t host_origin[3] = {0,0,0};	
	size_t region[3] = {
		self.width * sizeof(float),
		self.height,
		1};	

		int err = clEnqueueWriteBufferRect(
			tc->getMyContext()->cqCommandQueue,
			self.dataMatrix, CL_TRUE,
			buffer_origin, host_origin, region,
			self.pitch, 0,
			sizeof(float) * self.width, 0,
			data, 0, NULL, NULL);

		if (err != 0){
			std::cout << "Error in copyToDevice (CODE: " << err << ")" << std::endl;
		}
}

void DeviceMatrixCL3D_copyToDevice(DeviceMatrixCL3D& self, const float* data)
{
    if ((self.dim_x > 0) && (self.dim_y > 0) && (self.dim_t > 0)) {
		const int mem_size = self.dim_y *self.dim_t * self.pitch_y;
		TheContext * tc = new TheContext();
		
		size_t buffer_origin[3] = {0,0,0};
		size_t host_origin[3] = {0,0,0};	
		size_t region[3] = {
			self.dim_x * sizeof(float),
			self.dim_y,
			self.dim_t};
		
		int err = clEnqueueWriteBufferRect(
			tc->getMyContext()->cqCommandQueue,
			self.dataMatrix, CL_TRUE,
			buffer_origin, host_origin, region,
			self.pitch_y, 0,
			sizeof(float) * self.dim_x, 0,
			data,
			0, NULL, NULL);
		
		if (err != 0){
			std::cout << "Error in copyToDevice (CODE: " << err << ")" << std::endl;
		}
    }
}

void DeviceMatrixCL3D_copyFromDevice(const DeviceMatrixCL3D& self, float* dst)
{
    if ((self.dim_x > 0) && (self.dim_y > 0) && (self.dim_t > 0)) {
	
		const int mem_size = self.dim_y *self.dim_t * self.pitch_y;
	    	
        TheContext * tc = new TheContext();

	 //   printf("%d x %d\n",self.pitch_y,self.pitch_t);

		//printf("--->%d  x %d  x  %d\n",self.dim_x,self.dim_y,self.dim_t);
		
		size_t buffer_origin[3] = {0,0,0};
		size_t host_origin[3] = {0,0,0};	
        size_t region[3] = {self.dim_x * sizeof(float),
            self.dim_y,
            self.dim_t};	
		float prueba[5][2][3];
		//PyArray_DATA(retval.ptr());
        cl_int err =
		clEnqueueReadBufferRect(
			tc->getMyContext()->cqCommandQueue,
			self.dataMatrix, CL_TRUE,
			buffer_origin, host_origin, region,
			//self.pitch_y, self.dim_x * self.dim_y * sizeof(float),
			//self.pitch_y, 0,
			self.pitch_y, 0,
			self.dim_x * sizeof(float), 0,
			dst,
			0, NULL, NULL);
			//std::cout<<prueba[2][2][2]<<" "<<prueba[0][0][2]<<endl;
		
        if (err != 0){
            std::cout << "Error in copyFromDevice (CODE: " << err << ")" << std::endl;		
		}
	}
}

void DeviceMatrixCL_copyFromDevice(const DeviceMatrixCL& self, float* dst)
{
	if ((self.width > 0) && (self.height > 0)) {
		const int mem_size = self.height * self.pitch;

		TheContext * tc = new TheContext();

		size_t buffer_origin[3] = {0,0,0};
		size_t host_origin[3] = {0,0,0};	
		size_t region[3] = {self.width * sizeof(float),
			self.height,
			1};	

		cl_int err =
			clEnqueueReadBufferRect(
			tc->getMyContext()->cqCommandQueue,
			self.dataMatrix, CL_TRUE,
			buffer_origin, host_origin, region,
			self.pitch, 0,
			self.width * sizeof(float), 0,
			dst,
			0, NULL, NULL);

		if (err != 0){
			std::cout << "Error in copyFromDevice (CODE: " << err << ")" << std::endl;
		}
	}
}

/*
static void deleteDeviceMatrix3D(DeviceMatrix3D* mat)
{
	//printf("cudaFree: %p\n", mat->data);
	CUDA_CALL(cudaFree(mat->data));
	delete mat;
}

DeviceMatrix3D::Ptr makeDeviceMatrix3D(size_t dim_t, size_t dim_y, 
	size_t dim_x){
		DeviceMatrix3D* mat = new DeviceMatrix3D();
		mat->dim_x = dim_x;
		mat->dim_y = dim_y;
		mat->dim_t = dim_t;
		size_t pitch;
		CUDA_CALL
			(cudaMallocPitch((void**)&mat->data, &pitch,
			dim_x * sizeof(float),
			dim_y * dim_t));
		// I can't imagine getting a pitch that's not a multiple of a float
		assert(pitch % sizeof(float) == 0);
		// We want to express everything in floats
		pitch /= sizeof(float);

		mat->pitch_y = pitch;
		mat->pitch_t = dim_y*mat->pitch_y;

		return DeviceMatrix3D::Ptr(mat, deleteDeviceMatrix3D);
}

void DeviceMatrix3D_copyToDevice(DeviceMatrix3D& self, const float* data)
{
    if ((self.dim_x > 0) && (self.dim_y > 0) && (self.dim_t > 0)) {
        const size_t widthInBytes = self.dim_x * sizeof(float);
        CUDA_SAFE_CALL_NO_SYNC
        (cudaMemcpy2D(self.data, self.pitch_y * sizeof(float),
                      data, widthInBytes,
                      widthInBytes, self.dim_y * self.dim_t,
                      cudaMemcpyHostToDevice));
    }
}

void DeviceMatrix3D_copyFromDevice(const DeviceMatrix3D& self, float* dst)
{
    if ((self.dim_x == 0) || (self.dim_y == 0) || (self.dim_t == 0)) {
        // Bail early if there is nothing to copy
        return;
    }

    if (self.pitch_t == self.dim_y * self.pitch_y) {
        // Shortcut if we're packed in the t direction
        const size_t widthInBytes = self.dim_x * sizeof(float);
        CUDA_SAFE_CALL_NO_SYNC
            (cudaMemcpy2D(dst, widthInBytes,
                          self.data, self.pitch_y * sizeof(float),
                          widthInBytes, self.dim_y * self.dim_t,
                          cudaMemcpyDeviceToHost));

        return;
    }

    // Do a series of copies to fill in the 3D array
    for (size_t t=0; t < self.dim_t; t++) {
        const size_t widthInBytes = self.dim_x * sizeof(float);
        float* host_start = dst + t * self.dim_y * self.dim_x;
        float* device_start = self.data + t * self.pitch_t;
        CUDA_SAFE_CALL_NO_SYNC
            (cudaMemcpy2D(host_start, widthInBytes,
                          device_start, self.pitch_y * sizeof(float),
                          widthInBytes, self.dim_y,
                          cudaMemcpyDeviceToHost));
    }
}*/

/**
* This function is useful for generating matrices for use with CUFFT.
*/
/*DeviceMatrix3D::Ptr makeDeviceMatrix3DPacked(size_t dim_t, size_t dim_y,
	size_t dim_x)
{
	DeviceMatrix3D* mat = new DeviceMatrix3D();
	mat->dim_x = dim_x;
	mat->dim_y = dim_y;
	mat->dim_t = dim_t;
	mat->pitch_y = dim_x;
	mat->pitch_t = dim_y*mat->pitch_y;

	CUDA_CALL
		(cudaMalloc((void**)&mat->data, dim_x * dim_y * dim_t * sizeof(float)));

	return DeviceMatrix3D::Ptr(mat, deleteDeviceMatrix3D);

}

void DeviceMatrix::zero(){
	CUDA_CALL(
		cudaMemset(data, 0, height * width * sizeof(float));
	);
}

void DeviceMatrix3D::zero()
{
	CUDA_CALL
		(
		cudaMemset(data, 0, dim_t * pitch_t * sizeof(float));
	);
}
*/

DeviceMatrix3D::Ptr
	cropDeviceMatrix3D(const DeviceMatrix3D::Ptr self,
	size_t new_dim_t, size_t new_dim_y, size_t new_dim_x)
{
	boost::shared_ptr<DeviceMatrix3DView> retval(new DeviceMatrix3DView());
	retval->parent = self;
	retval->dim_x = new_dim_x;
	retval->dim_y = new_dim_y;
	retval->dim_t = new_dim_t;
	retval->pitch_y = self->pitch_y;
	retval->pitch_t = self->pitch_t;
	retval->data = self->data;

	return retval;
}


static void deleteMCudaMatrix3D(DeviceMatrix3D* mat)
{
	delete [] mat->data;
	delete mat;
}

MCudaMatrix3D::Ptr makeMCudaMatrix3D(size_t dim_t, size_t dim_y,
	size_t dim_x)
{
	MCudaMatrix3D* mat = new MCudaMatrix3D();
	mat->dim_x = dim_x;
	mat->dim_y = dim_y;
	mat->dim_t = dim_t;
	mat->pitch_y = dim_x;
	mat->pitch_t = dim_y*mat->pitch_y;

	mat->data = new float[dim_x * dim_y * dim_t];

	return MCudaMatrix3D::Ptr(mat, deleteMCudaMatrix3D);
}

/**
OpenCL 3d MATRIX
**/

static void deleteDeviceMatrixCL3D(DeviceMatrixCL3D* mat){}


DeviceMatrixCL3D::Ptr makeDeviceMatrixCL3D(size_t dim_t, size_t dim_y, 
	size_t dim_x){
		DeviceMatrixCL3D* mat = new DeviceMatrixCL3D();
		mat->dim_x = dim_x;
		mat->dim_y = dim_y;
		mat->dim_t = dim_t;
		//printf("%d  x %d  x  %d\n",dim_x,dim_y,dim_t);
		size_t pitch;

		TheContext * tc = new TheContext();

		cl_context GPUContext = tc->getMyContext()->getContextCL();
		cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();

		/*The optimal pitch is computed by (1) getting the base address alignment
		preference for your card (CL_DEVICE_MEM_BASE_ADDR_ALIGN property with
		clGetDeviceInfo: note that the returned value is in bits, so you have
		to divide by 8 to get it in bytes);*/

		int buffer;
		cl_int ierr = clGetDeviceInfo(cdDevice, CL_DEVICE_MEM_BASE_ADDR_ALIGN , sizeof(buffer), &buffer, NULL);
		
		buffer /= 8;

		int naturalPitch = sizeof(float) * mat->dim_x;

		/*let's call this base (2) find the largest multiple of base
		that is no less than your natural
		data pitch (sizeof(type) times number of columns);*/

		int devicepitch = ceil(float(naturalPitch)/buffer) * buffer;

		//printf("Pitch: %d, DevicePitch: %d, Buffer: %d\n", naturalPitch, devicepitch, buffer);

		mat->pitch_y = naturalPitch;//devicepitch;
		mat->pitch_t = dim_y*mat->pitch_y;

		//You then allocate pitch times number of rows bytes, and pass the pitch information to kernels.

		const int mem_size =  mat->dim_t*mat->pitch_t;

		//std::cout << "Mem size: " << mem_size << std::endl;

		int err;

		mat->dataMatrix = clCreateBuffer(GPUContext, CL_MEM_READ_WRITE, mem_size, NULL, &err);
		if(err!=0)
		{
			printf("Error Code create buffer: %d\n",err);
		}

		return DeviceMatrixCL3D::Ptr(mat, deleteDeviceMatrixCL3D);
}

/**
* This function is useful for generating matrices for use with CUFFT.
*/
DeviceMatrixCL3D::Ptr makeDeviceMatrixCL3DPacked(size_t dim_t, size_t dim_y,
	size_t dim_x)
{
	DeviceMatrixCL3D* mat = new DeviceMatrixCL3D();
	mat->dim_x = dim_x;
	mat->dim_y = dim_y;
	mat->dim_t = dim_t;
	size_t pitch;

	TheContext * tc = new TheContext();

	cl_context GPUContext = tc->getMyContext()->getContextCL();
	cl_device_id cdDevice = tc->getMyContext()->getDeviceCL();

	mat->pitch_y = dim_x;
	mat->pitch_t = dim_y*mat->pitch_y;

	const int mem_size = mat->dim_t * mat->pitch_t;

	int err;

	mat->dataMatrix = clCreateBuffer(GPUContext, CL_MEM_READ_WRITE, mem_size, NULL, &err);
	if(err!=0)
	{
		printf("Error Code create buffer: %d\n",err);
	}

	return DeviceMatrixCL3D::Ptr(mat, deleteDeviceMatrixCL3D);	
}

void DeviceMatrixCL::zero(){}

void DeviceMatrixCL3D::zero(){}

DeviceMatrixCL3D::Ptr
	cropDeviceMatrixCL3D(const DeviceMatrixCL3D::Ptr self,
	size_t new_dim_t, size_t new_dim_y, size_t new_dim_x)
{
	boost::shared_ptr<DeviceMatrixCL3DView> retval(new DeviceMatrixCL3DView());
	retval->parent = self;
	retval->dim_x = new_dim_x;
	retval->dim_y = new_dim_y;
	retval->dim_t = new_dim_t;
	retval->pitch_y = self->pitch_y;
	retval->pitch_t = self->pitch_t;
	retval->data = self->data;

	return retval;
}

static void deleteMCLMatrix3D(DeviceMatrixCL3D* mat)
{
	delete [] mat->data;
	delete mat;
}

MCLMatrix3D::Ptr makeMCLMatrix3D(size_t dim_t, size_t dim_y,
	size_t dim_x)
{
	MCLMatrix3D* mat = new MCLMatrix3D();
	mat->dim_x = dim_x;
	mat->dim_y = dim_y;
	mat->dim_t = dim_t;
	mat->pitch_y = dim_x;
	mat->pitch_t = dim_y*mat->pitch_y;

	mat->data = new float[dim_x * dim_y * dim_t];

	return MCLMatrix3D::Ptr(mat, deleteMCLMatrix3D);
}
