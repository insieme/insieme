//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB FT code for multiple    //
//  devices. This OpenCL version is developed by the Center for Manycore   //
//  Programming at Seoul National University and derived from the MPI      //
//  Fortran versions in "NPB3.3-MPI" developed by NAS.                     //
//                                                                         //
//  Permission to use, copy, distribute and modify this software for any   //
//  purpose with or without fee is hereby granted. This software is        //
//  provided "as is" without express or implied warranty.                  //
//                                                                         //
//  Information on NPB 3.3, including the technical report, the original   //
//  specifications, source code, results and information on how to submit  //
//  new results, is available at:                                          //
//                                                                         //
//           http://www.nas.nasa.gov/Software/NPB/                         //
//                                                                         //
//  Send comments or suggestions for this OpenCL version to                //
//  cmp@aces.snu.ac.kr                                                     //
//                                                                         //
//          Center for Manycore Programming                                //
//          School of Computer Science and Engineering                     //
//          Seoul National University                                      //
//          Seoul 151-744, Korea                                           //
//                                                                         //
//          E-mail:  cmp@aces.snu.ac.kr                                    //
//                                                                         //
//-------------------------------------------------------------------------//

//-------------------------------------------------------------------------//
// Authors: Sangmin Seo, Jungwon Kim, Jun Lee, Gangwon Jo, Jeongho Nah,    //
//          and Jaejin Lee                                                 //
//-------------------------------------------------------------------------//

#ifndef __FT_H__
#define __FT_H__

#if CLASS == 'S'
#define NX             64
#define NY             64
#define NZ             64
#define MAXDIM         64
#define NITER_DEFAULT  6
#define NXP            65
#define NYP            64
#define NTOTAL         262144
#define NTOTALP        266240

#elif CLASS == 'W'
#define NX             128
#define NY             128
#define NZ             32
#define MAXDIM         128
#define NITER_DEFAULT  6
#define NXP            129
#define NYP            128
#define NTOTAL         524288
#define NTOTALP        528384

#elif CLASS == 'A'
#define NX             256
#define NY             256
#define NZ             128
#define MAXDIM         256
#define NITER_DEFAULT  6
#define NXP            257
#define NYP            256
#define NTOTAL         8388608
#define NTOTALP        8421376

#elif CLASS == 'B'
#define NX             512
#define NY             256
#define NZ             256
#define MAXDIM         512
#define NITER_DEFAULT  20
#define NXP            513
#define NYP            256
#define NTOTAL         33554432
#define NTOTALP        33619968

#elif CLASS == 'C'
#define NX             512
#define NY             512
#define NZ             512
#define MAXDIM         512
#define NITER_DEFAULT  20
#define NXP            513
#define NYP            512
#define NTOTAL         134217728
#define NTOTALP        134479872

#elif CLASS == 'D'
#define NX             2048
#define NY             1024
#define NZ             1024
#define MAXDIM         2048
#define NITER_DEFAULT  25
#define NXP            2049
#define NYP            1024
#define NTOTAL         2147483648
#define NTOTALP        2148532224

#elif CLASS == 'E'
#define NX             4096
#define NY             2048
#define NZ             2048
#define MAXDIM         4096
#define NITER_DEFAULT  25
#define NXP            4097
#define NYP            2048
#define NTOTAL         17179869184
#define NTOTALP        17184063488

#else
#error "Unknown CLASS"
#endif

#define NTOTAL_F        (1.0*NX*NY*NZ)

// basic decomposition strategy
#define layout_0D       0
#define layout_1D       1
#define layout_2D       2

#define FFTBLOCK_DEFAULT      16
#define FFTBLOCKPAD_DEFAULT   18
#define TRANSBLOCK            32
#define TRANSBLOCKPAD         34

#define SEED            314159265.0
#define A               1220703125.0
#define PI              3.141592653589793238
#define ALPHA           1.0e-6


#ifdef USE_CPU
#pragma OPENCL EXTENSION cl_amd_fp64: enable
#pragma OPENCL EXTENSION cl_amd_printf: enable
#else
#pragma OPENCL EXTENSION cl_khr_fp64: enable
#endif


//---------------------------------------------------------------------------
// double complex
//---------------------------------------------------------------------------
typedef struct { 
  double real;
  double imag;
} dcomplex;

#define dcmplx(r,i)       (dcomplex){r, i}
#define dcmplx_add(a,b)   (dcomplex){(a).real+(b).real, (a).imag+(b).imag}
#define dcmplx_sub(a,b)   (dcomplex){(a).real-(b).real, (a).imag-(b).imag}
#define dcmplx_mul(a,b)   (dcomplex){((a).real*(b).real)-((a).imag*(b).imag),\
                                     ((a).real*(b).imag)+((a).imag*(b).real)}
#define dcmplx_mul2(a,b)  (dcomplex){(a).real*(b), (a).imag*(b)}
inline dcomplex dcmplx_div(dcomplex z1, dcomplex z2) {
  double a = z1.real;
  double b = z1.imag;
  double c = z2.real;
  double d = z2.imag;

  double divisor = c*c + d*d;
  double real = (a*c + b*d) / divisor;
  double imag = (b*c - a*d) / divisor;
  dcomplex result = (dcomplex){real, imag};
  return result;
}
#define dcmplx_div2(a,b)  (dcomplex){(a).real/(b), (a).imag/(b)}
#define dcmplx_abs(x)     sqrt(((x).real*(x).real) + ((x).imag*(x).imag))

#define dconjg(x)         (dcomplex){(x).real, -1.0*(x).imag}
//---------------------------------------------------------------------------


#include "ft_dim.h"

/* Below constants should be the same as those in ft.c!! */
#ifdef USE_CPU
#define COMPUTE_IMAP_DIM            COMPUTE_IMAP_DIM_CPU
#define EVOLVE_DIM                  EVOLVE_DIM_CPU
#define TRANSPOSE2_LOCAL1_DIM       TRANSPOSE2_LOCAL1_DIM_CPU
#define TRANSPOSE2_LOCAL2_DIM       TRANSPOSE2_LOCAL2_DIM_CPU
#define TRANSPOSE2_LOCAL3_DIM       TRANSPOSE2_LOCAL3_DIM_CPU
#define TRANSPOSE2_FINISH_DIM       TRANSPOSE2_FINISH_DIM_CPU
#define TRANSPOSE_X_Z_LOCAL1_DIM    TRANSPOSE_X_Z_LOCAL1_DIM_CPU
#define TRANSPOSE_X_Z_LOCAL2_DIM    TRANSPOSE_X_Z_LOCAL2_DIM_CPU
#define TRANSPOSE_X_Z_FINISH_DIM    TRANSPOSE_X_Z_FINISH_DIM_CPU
#define TRANSPOSE_X_Y_LOCAL_DIM     TRANSPOSE_X_Y_LOCAL_DIM_CPU
#define TRANSPOSE_X_Y_FINISH_DIM    TRANSPOSE_X_Y_FINISH_DIM_CPU
#define CFFTS1_DIM                  CFFTS1_DIM_CPU
#define CFFTS2_DIM                  CFFTS2_DIM_CPU
#define CFFTS3_DIM                  CFFTS3_DIM_CPU

#else //GPU
#define COMPUTE_IMAP_DIM            COMPUTE_IMAP_DIM_GPU
#define EVOLVE_DIM                  EVOLVE_DIM_GPU
#define TRANSPOSE2_LOCAL1_DIM       TRANSPOSE2_LOCAL1_DIM_GPU
#define TRANSPOSE2_LOCAL2_DIM       TRANSPOSE2_LOCAL2_DIM_GPU
#define TRANSPOSE2_LOCAL3_DIM       TRANSPOSE2_LOCAL3_DIM_GPU
#define TRANSPOSE2_FINISH_DIM       TRANSPOSE2_FINISH_DIM_GPU
#define TRANSPOSE_X_Z_LOCAL1_DIM    TRANSPOSE_X_Z_LOCAL1_DIM_GPU
#define TRANSPOSE_X_Z_LOCAL2_DIM    TRANSPOSE_X_Z_LOCAL2_DIM_GPU
#define TRANSPOSE_X_Z_FINISH_DIM    TRANSPOSE_X_Z_FINISH_DIM_GPU
#define TRANSPOSE_X_Y_LOCAL_DIM     TRANSPOSE_X_Y_LOCAL_DIM_GPU
#define TRANSPOSE_X_Y_FINISH_DIM    TRANSPOSE_X_Y_FINISH_DIM_GPU
#define CFFTS1_DIM                  CFFTS1_DIM_GPU
#define CFFTS2_DIM                  CFFTS2_DIM_GPU
#define CFFTS3_DIM                  CFFTS3_DIM_GPU

#endif

#endif //__FT_H__

