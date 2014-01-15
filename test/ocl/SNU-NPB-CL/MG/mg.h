//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB MG code for multiple    //
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

#ifndef __MG_H__
#define __MG_H__

#ifdef USE_CPU
#pragma OPENCL EXTENSION cl_amd_fp64: enable
#pragma OPENCL EXTENSION cl_amd_printf: enable
#else
#pragma OPENCL EXTENSION cl_khr_fp64: enable
#endif


#include "mg_dim.h"

#ifdef USE_CPU
#define ZERO3_DIM           ZERO3_DIM_CPU
#define COMM1P_1_DIM        COMM1P_1_DIM_CPU
#define COMM1P_2_DIM        COMM1P_2_DIM_CPU
#define COMM1P_3_DIM        COMM1P_3_DIM_CPU
#define COMM1P_4_DIM        COMM1P_4_DIM_CPU
#define READY_1_DIM         READY_1_DIM_CPU
#define GIVE3_1_DIM         GIVE3_1_DIM_CPU
#define TAKE3_1_DIM         TAKE3_1_DIM_CPU
#define COMM1PEX_2_DIM      COMM1PEX_2_DIM_CPU
#define COMM1PEX_3_DIM      COMM1PEX_3_DIM_CPU
#define COMM1PEX_4_DIM      COMM1PEX_4_DIM_CPU
#define COMM1PEX_5_DIM      COMM1PEX_5_DIM_CPU
#define NORM2U3_DIM         NORM2U3_DIM_CPU
#define RESID_DIM           RESID_DIM_CPU
#define RPRJ3_DIM           RPRJ3_DIM_CPU
#define PSINV_DIM           PSINV_DIM_CPU
#define INTERP_1_DIM        INTERP_1_DIM_CPU
#define INTERP_2_DIM        INTERP_2_DIM_CPU
#define INTERP_3_DIM        INTERP_3_DIM_CPU
#define INTERP_4_DIM        INTERP_4_DIM_CPU
#define INTERP_5_DIM        INTERP_5_DIM_CPU
#define COPY_BUFFER_DIM     COPY_BUFFER_DIM_CPU

#else //GPU
#define ZERO3_DIM           ZERO3_DIM_GPU
#define COMM1P_1_DIM        COMM1P_1_DIM_GPU
#define COMM1P_2_DIM        COMM1P_2_DIM_GPU
#define COMM1P_3_DIM        COMM1P_3_DIM_GPU
#define COMM1P_4_DIM        COMM1P_4_DIM_GPU
#define READY_1_DIM         READY_1_DIM_GPU
#define GIVE3_1_DIM         GIVE3_1_DIM_GPU
#define TAKE3_1_DIM         TAKE3_1_DIM_GPU
#define COMM1PEX_2_DIM      COMM1PEX_2_DIM_GPU
#define COMM1PEX_3_DIM      COMM1PEX_3_DIM_GPU
#define COMM1PEX_4_DIM      COMM1PEX_4_DIM_GPU
#define COMM1PEX_5_DIM      COMM1PEX_5_DIM_GPU
#define NORM2U3_DIM         NORM2U3_DIM_GPU
#define RESID_DIM           RESID_DIM_GPU
#define RPRJ3_DIM           RPRJ3_DIM_GPU
#define PSINV_DIM           PSINV_DIM_GPU
#define INTERP_1_DIM        INTERP_1_DIM_GPU
#define INTERP_2_DIM        INTERP_2_DIM_GPU
#define INTERP_3_DIM        INTERP_3_DIM_GPU
#define INTERP_4_DIM        INTERP_4_DIM_GPU
#define INTERP_5_DIM        INTERP_5_DIM_GPU
#define COPY_BUFFER_DIM     COPY_BUFFER_DIM_GPU

#endif

#endif //__MG_H__

