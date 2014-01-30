#ifndef _BLOCK_HISTOGRAM_LOCAL_HPP_
#define _BLOCK_HISTOGRAM_LOCAL_HPP_ 1

#include "DeviceMatrix.hpp"

#define METHOD_2

void cell_histogram_dense_device(DeviceMatrix3D* histogram,
                                 const DeviceMatrix* assignments,
                                 const DeviceMatrix* weights,
                                 const int max_bin,
                                 const int cell_size,
                                 const int start_y,
                                 const int start_x);

void cell_histogram_dense_device_cl(DeviceMatrixCL3D* histogram,
                                 const DeviceMatrixCL* assignments,
                                 const DeviceMatrixCL* weights,
                                 const int max_bin,
                                 const int cell_size,
                                 const int start_y,
                                 const int start_x);


#endif //_BLOCK_HISTOGRAM_LOCAL_HPP_
