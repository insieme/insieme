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

#ifndef __FT_DIM_H__
#define __FT_DIM_H__

#define COMPUTE_IMAP_DIM_CPU            2
#define EVOLVE_DIM_CPU                  2
#define TRANSPOSE2_LOCAL1_DIM_CPU       1
#define TRANSPOSE2_LOCAL2_DIM_CPU       1
#define TRANSPOSE2_LOCAL3_DIM_CPU       1
#define TRANSPOSE2_FINISH_DIM_CPU       2
#define TRANSPOSE_X_Z_LOCAL1_DIM_CPU    1
#define TRANSPOSE_X_Z_LOCAL2_DIM_CPU    1
#define TRANSPOSE_X_Z_FINISH_DIM_CPU    2
#define TRANSPOSE_X_Y_LOCAL_DIM_CPU     1
#define TRANSPOSE_X_Y_FINISH_DIM_CPU    2
#define CFFTS1_DIM_CPU                  2
#define CFFTS2_DIM_CPU                  2
#define CFFTS3_DIM_CPU                  2

#define COMPUTE_IMAP_DIM_GPU            2
#define EVOLVE_DIM_GPU                  2
#define TRANSPOSE2_LOCAL1_DIM_GPU       2
#define TRANSPOSE2_LOCAL2_DIM_GPU       2
#define TRANSPOSE2_LOCAL3_DIM_GPU       2
#define TRANSPOSE2_FINISH_DIM_GPU       2
#define TRANSPOSE_X_Z_LOCAL1_DIM_GPU    2
#define TRANSPOSE_X_Z_LOCAL2_DIM_GPU    2
#define TRANSPOSE_X_Z_FINISH_DIM_GPU    2
#define TRANSPOSE_X_Y_LOCAL_DIM_GPU     2
#define TRANSPOSE_X_Y_FINISH_DIM_GPU    2
#define CFFTS1_DIM_GPU                  2
#define CFFTS2_DIM_GPU                  2
#define CFFTS3_DIM_GPU                  2

#endif //__FT_DIM_H__

