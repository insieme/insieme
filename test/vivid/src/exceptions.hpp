#ifndef _EXCEPTIONS_HPP_
#define _EXCEPTIONS_HPP_ 1

//#include <cuda_runtime.h>

// The reason that this file exists is because we can't include
// <stdexcept> in a .cu file.  This is because <stdexcept> uses
// <string>, which isn't supported by cuda with gcc 4.3.
//
// see: http://forums.nvidia.com/index.php?showtopic=87096

//!A trivial wrapper.
void throw_runtime_error(const char* error);

/**
 * Handle of CUDA runtime errors
 */
//void throw_cuda_error(cudaError_t err, const char* file, int line);

// Loosly inspired by cutil.h
/*#define CUDA_CALL(call) do {                                                 \
    cudaError_t err = call;                                                  \
    if(cudaSuccess != err) {                                                 \
        throw_cuda_error(err, __FILE__, __LINE__);                           \
    } } while (0)
*/
#endif /* _EXCEPTIONS_HPP_ */
