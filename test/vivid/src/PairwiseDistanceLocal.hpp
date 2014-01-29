#ifndef _NEARESTNEIGHBORLOCAL_HPP_
#define _NEARESTNEIGHBORLOCAL_HPP_ 1

#include "DeviceMatrix.hpp"

void pwdist_generic( 
	const DeviceMatrix* features_train,
    const DeviceMatrix* features_test,
    DeviceMatrix* output,
    int type);

void pwdist_genericCL( 
	const DeviceMatrixCL* features_train,
	const DeviceMatrixCL* features_test,
	DeviceMatrixCL* output,
	int type);
void pwdist_eucCL( const DeviceMatrixCL* features_train,
                                       const DeviceMatrixCL* features_test,
                                       DeviceMatrixCL* output
                                       );
void argmin_cuda_local(const DeviceMatrix* matrix, DeviceMatrix* output);
void argmax_cuda_local(const DeviceMatrix* matrix, DeviceMatrix* output);

void min_cuda_local(const DeviceMatrix* matrix, DeviceMatrix* output);
void max_cuda_local(const DeviceMatrix* matrix, DeviceMatrix* output);

void argmin_cl_local(const DeviceMatrixCL* matrix, DeviceMatrixCL* output);

void argmax_cl_local(const DeviceMatrixCL* matrix, DeviceMatrixCL* output);

void min_cl_local(const DeviceMatrixCL* matrix, DeviceMatrixCL* output);
void max_cl_local(const DeviceMatrixCL* matrix, DeviceMatrixCL* output);

#endif /* _NEARESTNEIGHBORLOCAL_HPP_ */
