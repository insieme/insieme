#ifndef _PAIRWISE_DISTANCE_HPP_
#define _PAIRWISE_DISTANC_HPP_ 1

#define EUCLIDEAN 0
#define DOTPRODUCT 1
#define CHISQUARED 2
#define CITYBLOCK 3
#define ABSDOTPRODUCT 4

#include "DeviceMatrix.hpp"
#include "OpenCLKernels.hpp"

DeviceMatrix::Ptr pwdist_cuda( const DeviceMatrix::Ptr& features_train,
                               const DeviceMatrix::Ptr& features_test);

DeviceMatrix::Ptr pwdot_cuda( const DeviceMatrix::Ptr& features_train, 
                              const DeviceMatrix::Ptr& features_test);

DeviceMatrix::Ptr pwabsdot_cuda( const DeviceMatrix::Ptr& features_train, 
                                 const DeviceMatrix::Ptr& features_test);

DeviceMatrix::Ptr pwchisq_cuda( const DeviceMatrix::Ptr& features_train,
                                const DeviceMatrix::Ptr& features_test);

DeviceMatrix::Ptr pwcityblock_cuda( const DeviceMatrix::Ptr& features_train,
                                     const DeviceMatrix::Ptr& features_test);

DeviceMatrix::Ptr argmin_cuda(const DeviceMatrix::Ptr& matrix);
DeviceMatrix::Ptr argmax_cuda(const DeviceMatrix::Ptr& matrix);

DeviceMatrix::Ptr min_cuda(const DeviceMatrix::Ptr& matrix);
DeviceMatrix::Ptr max_cuda(const DeviceMatrix::Ptr& matrix);

/*
OpenCL
 */
DeviceMatrixCL::Ptr pwdist_cl( const DeviceMatrixCL::Ptr& features_train,
							  const DeviceMatrixCL::Ptr& features_test);

DeviceMatrixCL::Ptr pwdot_cl( const DeviceMatrixCL::Ptr& features_train, 
							 const DeviceMatrixCL::Ptr& features_test);

DeviceMatrixCL::Ptr pwabsdot_cl( const DeviceMatrixCL::Ptr& features_train, 
								const DeviceMatrixCL::Ptr& features_test);

DeviceMatrixCL::Ptr pwchisq_cl( const DeviceMatrixCL::Ptr& features_train,
							   const DeviceMatrixCL::Ptr& features_test);

DeviceMatrixCL::Ptr pwcityblock_cl( const DeviceMatrixCL::Ptr& features_train,
								   const DeviceMatrixCL::Ptr& features_test);

DeviceMatrixCL::Ptr argmin_cl(const DeviceMatrixCL::Ptr& matrix);

DeviceMatrixCL::Ptr argmax_cl(const DeviceMatrixCL::Ptr& matrix);

DeviceMatrixCL::Ptr min_cl(const DeviceMatrixCL::Ptr& matrix);
DeviceMatrixCL::Ptr max_cl(const DeviceMatrixCL::Ptr& matrix);

/**
 Additional functions
 **/

#endif /* _PAIRWISE_DISTANCE_HPP_ */
