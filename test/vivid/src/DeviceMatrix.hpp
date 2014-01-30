// @(#)DeviceMatrix.hpp Utility functions for DeviceMatrix
//
//////////////////////////////////////////////////////////////////////

#ifndef _DEVICEMATRIX_HPP_
#define _DEVICEMATRIX_HPP_ 1

#include <boost/shared_ptr.hpp>
#include <CL/opencl.h>
#include <CL/cl.h>
#include <vector>
#include "ContextOpenCL.h"

struct DeviceMatrix {
  typedef boost::shared_ptr<DeviceMatrix> Ptr;
  typedef std::vector<Ptr> PtrList;

  unsigned int width; //!< width in floats
  unsigned int height;//!< height in floats
  //! pitch in floats
  size_t pitch;
  float* data;

  void zero();
};

DeviceMatrix::Ptr makeDeviceMatrix(size_t height, size_t width);

void DeviceMatrix_copyToDevice(DeviceMatrix& self, const float* data);
void DeviceMatrix_copyFromDevice(const DeviceMatrix& self, float* dst);

struct DeviceMatrixCL {
    typedef boost::shared_ptr<DeviceMatrixCL> Ptr;
    typedef std::vector<Ptr> PtrList;

    size_t width;
    size_t height;

    size_t pitch;
    float* data;
	
    cl_mem dataMatrix;

    void zero();
};

DeviceMatrixCL::Ptr makeDeviceMatrixCL(size_t height, size_t width);

void DeviceMatrixCL_copyToDevice(DeviceMatrixCL& self, const float* data);
void DeviceMatrixCL_copyFromDevice(const DeviceMatrixCL& self, float* dst);

struct DeviceMatrix3D {
    typedef boost::shared_ptr<DeviceMatrix3D> Ptr;
    typedef std::vector<Ptr> PtrList;

    unsigned int dim_x; //!< width in floats
    unsigned int dim_y; //!< height in floats
    unsigned int dim_t; //!< depth (length?) in floats

    // We assume that the data is tightly packed in x
    unsigned int pitch_y; //! pitch in the y direction
    unsigned int pitch_t; //! pitch in the t direction

    /**
     * @note We structure the data as data[t][y][x] (or as
     * FORTRAN-style x,y,t).
     */
    float* data;

    // Fill the matrix with zeros
    void zero();
};

DeviceMatrix3D::Ptr makeDeviceMatrix3D(size_t dim_t, size_t dim_y, 
                                       size_t dim_x);

void DeviceMatrix3D_copyToDevice(DeviceMatrix3D& self, const float* data);
void DeviceMatrix3D_copyFromDevice(const DeviceMatrix3D& self, float* dst);

//! Create a DeviceMatrix3D that has no padding
DeviceMatrix3D::Ptr makeDeviceMatrix3DPacked(size_t dim_t, size_t dim_y, 
                                             size_t dim_x);

/**
 * By using inheritence, we're definitely heading into c++ land.
 * However, we should still be mostly ok, since we have no virtual
 * functions and only one additional member.
 */
struct DeviceMatrix3DView : public DeviceMatrix3D
{
    //! Keeps the parent alive while the view is active
    boost::shared_ptr<void> parent;
};

/**
 * Returns a view of a portion of the input matrix.
 *
 * @note This function does not support arbitary subcubes -- the
 * "origin" must remain in place.
 */
DeviceMatrix3D::Ptr
cropDeviceMatrix3D(const DeviceMatrix3D::Ptr self,
                   size_t new_dim_t, size_t new_dim_y, size_t new_dim_x);

/**
 * This class has no new data memebers, but it has different
 * semantics.  A MCudaMatrix3D actually has a data pointer which
 * points to a (packed) C array on CPU memory.
 *
 * The subclassing is so that we can pass this seamlessly into MCUDA
 * code that expects a DeviceMatrix3D.
 */
struct MCudaMatrix3D : public DeviceMatrix3D
{
	typedef boost::shared_ptr<MCudaMatrix3D> Ptr;
};

MCudaMatrix3D::Ptr makeMCudaMatrix3D(size_t dim_t, size_t dim_y,
                                     size_t dim_x);


struct DeviceMatrixCL3D {
    typedef boost::shared_ptr<DeviceMatrixCL3D> Ptr;
    typedef std::vector<Ptr> PtrList;
	
    unsigned int dim_x; //!< width in floats
    unsigned int dim_y; //!< height in floats
    unsigned int dim_t; //!< depth (length?) in floats
	
    // We assume that the data is tightly packed in x
    unsigned int pitch_y; //! pitch in the y direction
    unsigned int pitch_t; //! pitch in the t direction
	
	cl_mem dataMatrix;
    /**
     * @note We structure the data as data[t][y][x] (or as
     * FORTRAN-style x,y,t).
     */
    float* data;
	
    // Fill the matrix with zeros
    void zero();
};

DeviceMatrixCL::Ptr makeDeviceMatrixCL(DeviceMatrixCL3D& src, const int slice);
DeviceMatrixCL3D::Ptr makeDeviceMatrixCL3D(size_t dim_t, size_t dim_y, size_t dim_x);

void DeviceMatrixCL3D_copyToDevice(DeviceMatrixCL3D& self, const float* data);
void DeviceMatrixCL3D_copyFromDevice(const DeviceMatrixCL3D& self, float* dst);

//! Create a DeviceMatrix3D that has no padding
DeviceMatrixCL3D::Ptr makeDeviceMatrixCL3DPacked(size_t dim_t, size_t dim_y, 
                                             size_t dim_x);

/**
 * By using inheritence, we're definitely heading into c++ land.
 * However, we should still be mostly ok, since we have no virtual
 * functions and only one additional member.
 */
struct DeviceMatrixCL3DView : public DeviceMatrixCL3D
{
    //! Keeps the parent alive while the view is active
    boost::shared_ptr<void> parent;
};

/**
 * Returns a view of a portion of the input matrix.
 *
 * @note This function does not support arbitary subcubes -- the
 * "origin" must remain in place.
 */
DeviceMatrixCL3D::Ptr
cropDeviceMatrixCL3D(const DeviceMatrixCL3D::Ptr self,
                   size_t new_dim_t, size_t new_dim_y, size_t new_dim_x);

/**
 * This class has no new data memebers, but it has different
 * semantics.  A MCudaMatrix3D actually has a data pointer which
 * points to a (packed) C array on CPU memory.
 *
 * The subclassing is so that we can pass this seamlessly into MCUDA
 * code that expects a DeviceMatrix3D.
 */
struct MCLMatrix3D : public DeviceMatrixCL3D
{
  typedef boost::shared_ptr<MCLMatrix3D> Ptr;
};

MCLMatrix3D::Ptr makeMCLMatrix3D(size_t dim_t, size_t dim_y,
                                     size_t dim_x);

#endif /* _DEVICEMATRIX_HPP_ */
