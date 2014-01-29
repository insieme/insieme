#ifndef _BLOCK_HISTOGRAM_HPP_
#define _BLOCK_HISTOGRAM_HPP_ 1

#include "DeviceMatrix.hpp"
#include "OpenCLKernels.hpp"

DeviceMatrix3D::Ptr cell_histogram_dense_cuda(
    const DeviceMatrix::Ptr& assignment_mat,
    const DeviceMatrix::Ptr& weight_mat,
    const int max_bin, const int cell_size,
	const int start_y, const int start_x,
	const int stop_y, const int stop_x);
    //boost::python::object& start_inds, boost::python::object& stop_inds);

DeviceMatrixCL::Ptr cell_histogram_dense_cl(
	const DeviceMatrixCL3D::Ptr& ff_out,
	const int max_bin, const int cell_size, 
	const int start_y, const int start_x,
	const int stop_y, const int stop_x);

DeviceMatrixCL3D::Ptr cell_histogram_dense_cl(
	const DeviceMatrixCL::Ptr& assignment_mat,
	const DeviceMatrixCL::Ptr& weight_mat,
	const int max_bin, const int cell_size, 
	const int start_y, const int start_x,
	const int stop_y, const int stop_x);

	//boost::python::object& start_inds, boost::python::object& stop_inds);
#endif //_BLOCK_HISTOGRAM_HPP_
