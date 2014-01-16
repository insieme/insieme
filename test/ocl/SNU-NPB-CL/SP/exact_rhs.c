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

#include "header.h"

//---------------------------------------------------------------------
// compute the right hand side based on exact solution
//---------------------------------------------------------------------
void exact_rhs()
{
  int c, i;
  int range_1, range_0;
  size_t d[3], local_ws[3], global_ws[3];
  cl_int ecode = 0;

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      ecode  = clSetKernelArg(k_exact_rhs1[i], 0, sizeof(cl_mem), &m_forcing[i]);
      ecode |= clSetKernelArg(k_exact_rhs1[i], 1, sizeof(int), &c);
      ecode |= clSetKernelArg(k_exact_rhs1[i], 2, sizeof(int), &cell_size[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs1[i], 3, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs1[i], 4, sizeof(int), &cell_size[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for exact_rhs1");

      d[0] = cell_size[i][c][1];
      d[1] = cell_size[i][c][2];
      compute_ws_dim_2(d, local_ws, global_ws);

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_exact_rhs1[i],
                                     2, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for exact_rhs1");
    }

    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }

  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_exact_rhs2[i], 0, sizeof(cl_mem), &m_forcing[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 1, sizeof(cl_mem), &m_ce[i]);
    clu_CheckError(ecode, "clSetKernelArg() for exact_rhs2");
  }

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      range_1 = cell_size[i][c][2] - cell_end[i][c][2];
      range_0 = cell_size[i][c][1] - cell_end[i][c][1];

      ecode  = clSetKernelArg(k_exact_rhs2[i], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 3, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 4, sizeof(int), &range_1);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 5, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 6, sizeof(int), &range_0);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 7, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 8, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 9, sizeof(int), &cell_end[i][c][0]);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 10, sizeof(int), &cell_low[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 11, sizeof(int), &cell_low[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs2[i], 12, sizeof(int), &cell_low[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for exact_rhs2");

      d[0] = cell_size[i][c][1] - cell_start[i][c][1] - cell_end[i][c][1];
      d[1] = cell_size[i][c][2] - cell_start[i][c][2] - cell_end[i][c][2];
      compute_ws_dim_2(d, local_ws, global_ws);

      if (c == 0) CHECK_FINISH(i * 2);

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_exact_rhs2[i],
                                     2, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for exact_rhs2");
    }
    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_exact_rhs3[i], 0, sizeof(cl_mem), &m_forcing[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 1, sizeof(cl_mem), &m_ce[i]);
    clu_CheckError(ecode, "clSetKernelArg() for exact_rhs3");
  }

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      range_1 = cell_size[i][c][2] - cell_end[i][c][2];
      range_0 = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_exact_rhs3[i], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 3, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 4, sizeof(int), &range_1);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 5, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 6, sizeof(int), &range_0);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 7, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 8, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 9, sizeof(int), &cell_end[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 10, sizeof(int), &cell_low[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 11, sizeof(int), &cell_low[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs3[i], 12, sizeof(int), &cell_low[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for exact_rhs3");

      d[0] = cell_size[i][c][0] - cell_start[i][c][0] - cell_end[i][c][0];
      d[1] = cell_size[i][c][2] - cell_start[i][c][2] - cell_end[i][c][2];
      compute_ws_dim_2(d, local_ws, global_ws);

      if (c == 0) CHECK_FINISH(i * 2);

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_exact_rhs3[i],
                                     2, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for exact_rhs3");
    }
    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_exact_rhs4[i], 0, sizeof(cl_mem), &m_forcing[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 1, sizeof(cl_mem), &m_ce[i]);
    clu_CheckError(ecode, "clSetKernelArg() for exact_rhs4");
  }

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      range_1 = cell_size[i][c][1] - cell_end[i][c][1];
      range_0 = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_exact_rhs4[i], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 3, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 4, sizeof(int), &range_1);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 5, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 6, sizeof(int), &range_0);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 7, sizeof(int), &cell_size[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 8, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 9, sizeof(int), &cell_end[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 10, sizeof(int), &cell_low[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 11, sizeof(int), &cell_low[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs4[i], 12, sizeof(int), &cell_low[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for exact_rhs4");

      d[0] = cell_size[i][c][0] - cell_start[i][c][0] - cell_end[i][c][0];
      d[1] = cell_size[i][c][1] - cell_start[i][c][1] - cell_end[i][c][1];
      compute_ws_dim_2(d, local_ws, global_ws);

      if (c == 0) CHECK_FINISH(i * 2);

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_exact_rhs4[i],
                                     2, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for exact_rhs4");
    }
    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }
  }

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      range_1 = cell_size[i][c][2] - cell_end[i][c][2];
      range_0 = cell_size[i][c][1] - cell_end[i][c][1];

      ecode  = clSetKernelArg(k_exact_rhs5[i], 0, sizeof(cl_mem), &m_forcing[i]);
      ecode |= clSetKernelArg(k_exact_rhs5[i], 1, sizeof(int), &c);
      ecode |= clSetKernelArg(k_exact_rhs5[i], 2, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_exact_rhs5[i], 3, sizeof(int), &range_1);
      ecode |= clSetKernelArg(k_exact_rhs5[i], 4, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_exact_rhs5[i], 5, sizeof(int), &range_0);
      ecode |= clSetKernelArg(k_exact_rhs5[i], 6, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_exact_rhs5[i], 7, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_exact_rhs5[i], 8, sizeof(int), &cell_end[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for exact_rhs5");

      d[0] = cell_size[i][c][1] - cell_start[i][c][1] - cell_end[i][c][1];
      d[1] = cell_size[i][c][2] - cell_start[i][c][2] - cell_end[i][c][2];
      compute_ws_dim_2(d, local_ws, global_ws);

      if (c == 0) CHECK_FINISH(i * 2);

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_exact_rhs5[i],
                                     2, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for exact_rhs5");
    }
    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }
  }

  for (i = 0; i < num_devices; i++)
    CHECK_FINISH(i * 2);
}

