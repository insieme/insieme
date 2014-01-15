//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB SP code for multiple    //
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

#include <math.h>
#include "header.h"

void compute_rhs()
{
  int c, i;
  cl_int ecode = 0;

  if (timeron) timer_start(t_rhs);
  //---------------------------------------------------------------------
  // loop over all cells owned by this node                           
  //---------------------------------------------------------------------
  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_compute_rhs1[i][c],
                                     COMPUTE_RHS1_DIM, NULL,
                                     compute_rhs1_gw[i][c],
                                     compute_rhs1_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for compute_rhs1");
    }

    for (i = 0; i < num_devices; i++) {
      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_compute_rhs2[i][c],
                                     COMPUTE_RHS2_DIM, NULL,
                                     compute_rhs2_gw[i][c],
                                     compute_rhs2_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for compute_rhs2");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_compute_rhs3[i][c],
                                     2, NULL,
                                     compute_rhs3_gw[i][c],
                                     compute_rhs3_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for compute_rhs3");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_compute_rhs4[i][c],
                                     2, NULL,
                                     compute_rhs4_gw[i][c],
                                     compute_rhs4_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for compute_rhs4");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_compute_rhs5[i][c],
                                     2, NULL,
                                     compute_rhs5_gw[i][c],
                                     compute_rhs5_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for compute_rhs5");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_compute_rhs6[i][c],
                                     COMPUTE_RHS6_DIM, NULL,
                                     compute_rhs6_gw[i][c],
                                     compute_rhs6_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for compute_rhs6");
    }
    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }
  }

  for (i = 0; i < num_devices; i++)
    CHECK_FINISH(i * 2);

  if (timeron) timer_stop(t_rhs);
}

