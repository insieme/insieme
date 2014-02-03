#ifndef _FLEXIBLE_FILTER_HPP_
#define _FLEXIBLE_FILTER_HPP_ 1

#include "DeviceMatrix.hpp"
#include "OpenCLKernels.hpp"

#include <stdio.h>

#define FF_OPTYPE_EUCLIDEAN 0
#define FF_OPTYPE_COSINE 1

void cosine_filter(
	float* fr_data, float* fb_array, 
	const int height, const int width, 
	const int filter_h, const int filter_w, 
	const int n_filters, float* out_data);

/*size parameter is in terms of floats*/
int set_filter_bank_cuda(float* filter_bank, int size);
int set_filter_bank_cl(float* filter_bank, int size);

/*
DeviceMatrix3D::Ptr filter_frame_cuda(const DeviceMatrix::Ptr& frame,
                                    const int dim_t, const int dim_y, const int dim_x, const int nchannels,
                                    const int optype);
*/

DeviceMatrix3D::Ptr filter_frame_cuda_noargmin(const DeviceMatrix::Ptr& frame,
                                    const int dim_t, const int dim_y, const int dim_x, const int nchannels,
                                    const int optype);

DeviceMatrix3D::Ptr get_cell_histograms_cuda(const DeviceMatrix3D::Ptr& inds_and_weights,
                                             const int cell_size,
                                             const int offset_y, const int offset_x,
                                             const int n_bins);

DeviceMatrix3D::Ptr filter_frame_cuda_3(const DeviceMatrix::Ptr& frame,
                                    const int dim_t, const int nchannels,
                                    const int optype);

DeviceMatrix3D::Ptr filter_frame_cuda_5(const DeviceMatrix::Ptr& frame,
                                    const int dim_t, const int nchannels,
                                    const int optype);

DeviceMatrix3D::Ptr filter_frame_cuda_7(const DeviceMatrix::Ptr& frame,
                                    const int dim_t, const int nchannels,
                                    const int optype);

/**
 
 OPENCL FUNCTIONS
 
 
**/

DeviceMatrixCL3D::Ptr filter_frame_cl_noargmin(const DeviceMatrixCL::Ptr& frame,
											   const int dim_t, const int dim_y, const int dim_x, const int nchannels,
											   const int optype);

DeviceMatrixCL3D::Ptr get_cell_histograms_cl(const DeviceMatrixCL3D::Ptr& inds_and_weights,
                                             const int cell_size,
                                             const int offset_y, const int offset_x,
                                             const int n_bins);

DeviceMatrixCL3D::Ptr filter_frame_cl_3(const DeviceMatrixCL::Ptr& frame,
										const int dim_t, const int nchannels,
										const int optype);

DeviceMatrixCL3D::Ptr filter_frame_cl_5(const DeviceMatrixCL::Ptr& frame,
										const int dim_t, const int nchannels,
										const int optype);

DeviceMatrixCL3D::Ptr filter_frame_cl_7(const DeviceMatrixCL::Ptr& frame,
										const int dim_t, const int nchannels,
										const int optype);


void dist_filter2_d3_cl(const DeviceMatrixCL* frame,
						const int dim_t, const int nchannels,
						DeviceMatrixCL3D* output,
						const int optype);

/**  CONST  **/
static const unsigned int BLOCK_SIZE = 16;
static const unsigned int BLOCK_16 = 16;
static const unsigned int BLOCK_8 = 8;

#endif /* _FLEXIBLE_FILTER_HPP_ */
