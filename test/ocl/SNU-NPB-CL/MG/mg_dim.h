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

#ifndef __MG_DIM_H__
#define __MG_DIM_H__

#define ZERO3_DIM_CPU        2
#define COMM1P_1_DIM_CPU     1
#define COMM1P_2_DIM_CPU     1
#define COMM1P_3_DIM_CPU     1
#define COMM1P_4_DIM_CPU     1
#define READY_1_DIM_CPU      1
#define GIVE3_1_DIM_CPU      1
#define TAKE3_1_DIM_CPU      1
#define COMM1PEX_2_DIM_CPU   1
#define COMM1PEX_3_DIM_CPU   1
#define COMM1PEX_4_DIM_CPU   1
#define COMM1PEX_5_DIM_CPU   1
#define NORM2U3_DIM_CPU      1
#define RESID_DIM_CPU        2
#define RPRJ3_DIM_CPU        1
#define PSINV_DIM_CPU        2
#define INTERP_1_DIM_CPU     1
#define INTERP_2_DIM_CPU     1
#define INTERP_3_DIM_CPU     1
#define INTERP_4_DIM_CPU     1
#define INTERP_5_DIM_CPU     1
#define COPY_BUFFER_DIM_CPU  1

#define ZERO3_DIM_GPU        2
#define COMM1P_1_DIM_GPU     1
#define COMM1P_2_DIM_GPU     1
#define COMM1P_3_DIM_GPU     1
#define COMM1P_4_DIM_GPU     1
#define READY_1_DIM_GPU      1
#define GIVE3_1_DIM_GPU      1
#define TAKE3_1_DIM_GPU      1
#define COMM1PEX_2_DIM_GPU   1
#define COMM1PEX_3_DIM_GPU   1
#define COMM1PEX_4_DIM_GPU   1
#define COMM1PEX_5_DIM_GPU   1
#define NORM2U3_DIM_GPU      1
#define RESID_DIM_GPU        2
#define RPRJ3_DIM_GPU        1
#define PSINV_DIM_GPU        2
#define INTERP_1_DIM_GPU     1
#define INTERP_2_DIM_GPU     1
#define INTERP_3_DIM_GPU     1
#define INTERP_4_DIM_GPU     1
#define INTERP_5_DIM_GPU     1
#define COPY_BUFFER_DIM_GPU  1
                            
#endif //__MG_DIM_H__


//#define ZERO3_DIM_GPU        3
//#define COMM1P_1_DIM_GPU     1
//#define COMM1P_2_DIM_GPU     2
//#define COMM1P_3_DIM_GPU     1
//#define COMM1P_4_DIM_GPU     2
//#define READY_1_DIM_GPU      1
//#define GIVE3_1_DIM_GPU      2
//#define TAKE3_1_DIM_GPU      2
//#define COMM1PEX_2_DIM_GPU   1
//#define COMM1PEX_3_DIM_GPU   1
//#define COMM1PEX_4_DIM_GPU   1
//#define COMM1PEX_5_DIM_GPU   1
//#define NORM2U3_DIM_GPU      3
//#define RESID_DIM_GPU        2
//#define RPRJ3_DIM_GPU        2
//#define PSINV_DIM_GPU        2
//#define INTERP_1_DIM_GPU     2
//#define INTERP_2_DIM_GPU     2
//#define INTERP_3_DIM_GPU     2
//#define INTERP_4_DIM_GPU     2
//#define INTERP_5_DIM_GPU     2
//#define COPY_BUFFER_DIM_GPU  1
