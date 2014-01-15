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
// This subroutine initializes the field variable u using 
// tri-linear transfinite interpolation of the boundary values     
//---------------------------------------------------------------------
void initialize()
{
  int c, i;
  size_t d[3], local_ws[3], global_ws[3];
  cl_int ecode = 0;

  //---------------------------------------------------------------------
  // Later (in compute_rhs) we compute 1/u for every element. A few of 
  // the corner elements are not used, but it convenient (and faster) 
  // to compute the whole thing with a simple loop. Make sure those 
  // values are nonzero by initializing the whole thing here. 
  //---------------------------------------------------------------------

// initialize1

  d[0] = IMAX+2;
  d[1] = IMAX+2;
  d[2] = ncells;
  compute_ws_dim_3(d, local_ws, global_ws);

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_initialize1[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize1[i], 1, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg() for initialize1");
    ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                   k_initialize1[i],
                                   3, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for initialize1");
  }

  for (i = 0; i < num_devices; i++)
    CHECK_FINISH(i * 2);

// initialize2

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_initialize2[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize2[i], 1, sizeof(cl_mem), &m_ce[i]);
    clu_CheckError(ecode, "clSetKernelArg() for initialize2");
  }

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      ecode  = clSetKernelArg(k_initialize2[i], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_initialize2[i], 3, sizeof(int), &cell_low[i][c][2]);
      ecode |= clSetKernelArg(k_initialize2[i], 4, sizeof(int), &cell_high[i][c][2]);
      ecode |= clSetKernelArg(k_initialize2[i], 5, sizeof(int), &cell_low[i][c][1]);
      ecode |= clSetKernelArg(k_initialize2[i], 6, sizeof(int), &cell_high[i][c][1]);
      ecode |= clSetKernelArg(k_initialize2[i], 7, sizeof(int), &cell_low[i][c][0]);
      ecode |= clSetKernelArg(k_initialize2[i], 8, sizeof(int), &cell_high[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for initialize2");

      d[0] = cell_size[i][c][1];
      d[1] = cell_size[i][c][2];
      compute_ws_dim_2(d, local_ws, global_ws);

      if (c == 0) CHECK_FINISH(i * 2);
      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_initialize2[i],
                                     2, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for initialize2");
    }
    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }
  }

// initialize3
  for (i = 0; i < num_devices; i++) {
    c = slice[i][0][0];
    ecode  = clSetKernelArg(k_initialize3[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize3[i], 1, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize3[i], 2, sizeof(int), &c);
    ecode |= clSetKernelArg(k_initialize3[i], 3, sizeof(int), &cell_low[i][c][2]);
    ecode |= clSetKernelArg(k_initialize3[i], 4, sizeof(int), &cell_high[i][c][2]);
    ecode |= clSetKernelArg(k_initialize3[i], 5, sizeof(int), &cell_low[i][c][1]);
    ecode |= clSetKernelArg(k_initialize3[i], 6, sizeof(int), &cell_high[i][c][1]);
    clu_CheckError(ecode, "clSetKernelArg() for initialize3");

    d[0] = cell_size[i][c][1];
    d[1] = cell_size[i][c][2];
    compute_ws_dim_2(d, local_ws, global_ws);

    CHECK_FINISH(i * 2);
    ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                   k_initialize3[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for initialize3");
  }

// initialize4
  for (i = 0; i < num_devices; i++) {
    c  = slice[i][ncells-1][0];
    ecode  = clSetKernelArg(k_initialize4[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize4[i], 1, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize4[i], 2, sizeof(int), &c);
    ecode |= clSetKernelArg(k_initialize4[i], 3, sizeof(int), &cell_low[i][c][2]);
    ecode |= clSetKernelArg(k_initialize4[i], 4, sizeof(int), &cell_high[i][c][2]);
    ecode |= clSetKernelArg(k_initialize4[i], 5, sizeof(int), &cell_low[i][c][1]);
    ecode |= clSetKernelArg(k_initialize4[i], 6, sizeof(int), &cell_high[i][c][1]);
    ecode |= clSetKernelArg(k_initialize4[i], 7, sizeof(int), &cell_size[i][c][0]);
    clu_CheckError(ecode, "clSetKernelArg() for initialize4");

    d[0] = cell_size[i][c][1];
    d[1] = cell_size[i][c][2];
    compute_ws_dim_2(d, local_ws, global_ws);

    ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                   k_initialize4[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for initialize4");
  }

// initialize5
  for (i = 0; i < num_devices; i++) {
    c  = slice[i][0][1];
    ecode  = clSetKernelArg(k_initialize5[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize5[i], 1, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize5[i], 2, sizeof(int), &c);
    ecode |= clSetKernelArg(k_initialize5[i], 3, sizeof(int), &cell_low[i][c][2]);
    ecode |= clSetKernelArg(k_initialize5[i], 4, sizeof(int), &cell_high[i][c][2]);
    ecode |= clSetKernelArg(k_initialize5[i], 5, sizeof(int), &cell_low[i][c][0]);
    ecode |= clSetKernelArg(k_initialize5[i], 6, sizeof(int), &cell_high[i][c][0]);
    clu_CheckError(ecode, "clSetKernelArg() for initialize5");

    d[0] = cell_size[i][c][0];
    d[1] = cell_size[i][c][2];
    compute_ws_dim_2(d, local_ws, global_ws);

    CHECK_FINISH(i * 2);
    ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                   k_initialize5[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for initialize5");
  }

// initialize6
  for (i = 0; i < num_devices; i++) {
    c  = slice[i][ncells-1][1];
    ecode  = clSetKernelArg(k_initialize6[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize6[i], 1, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize6[i], 2, sizeof(int), &c);
    ecode |= clSetKernelArg(k_initialize6[i], 3, sizeof(int), &cell_low[i][c][2]);
    ecode |= clSetKernelArg(k_initialize6[i], 4, sizeof(int), &cell_high[i][c][2]);
    ecode |= clSetKernelArg(k_initialize6[i], 5, sizeof(int), &cell_low[i][c][0]);
    ecode |= clSetKernelArg(k_initialize6[i], 6, sizeof(int), &cell_high[i][c][0]);
    ecode |= clSetKernelArg(k_initialize6[i], 7, sizeof(int), &cell_size[i][c][1]);
    clu_CheckError(ecode, "clSetKernelArg() for initialize6");

    d[0] = cell_size[i][c][0];
    d[1] = cell_size[i][c][2];
    compute_ws_dim_2(d, local_ws, global_ws);

    ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                   k_initialize6[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for initialize6");
  }

// initialize7
  for (i = 0; i < num_devices; i++) {
    c  = slice[i][0][2];
    ecode  = clSetKernelArg(k_initialize7[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize7[i], 1, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize7[i], 2, sizeof(int), &c);
    ecode |= clSetKernelArg(k_initialize7[i], 3, sizeof(int), &cell_low[i][c][1]);
    ecode |= clSetKernelArg(k_initialize7[i], 4, sizeof(int), &cell_high[i][c][1]);
    ecode |= clSetKernelArg(k_initialize7[i], 5, sizeof(int), &cell_low[i][c][0]);
    ecode |= clSetKernelArg(k_initialize7[i], 6, sizeof(int), &cell_high[i][c][0]);
    clu_CheckError(ecode, "clSetKernelArg() for initialize7");

    d[0] = cell_size[i][c][0];
    d[1] = cell_size[i][c][1];
    compute_ws_dim_2(d, local_ws, global_ws);

    CHECK_FINISH(i * 2);
    ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                   k_initialize7[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for initialize7");
  }

// initialize8
  for (i = 0; i < num_devices; i++) {
    c  = slice[i][ncells-1][2];
    ecode  = clSetKernelArg(k_initialize8[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize8[i], 1, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize8[i], 2, sizeof(int), &c);
    ecode |= clSetKernelArg(k_initialize8[i], 3, sizeof(int), &cell_low[i][c][1]);
    ecode |= clSetKernelArg(k_initialize8[i], 4, sizeof(int), &cell_high[i][c][1]);
    ecode |= clSetKernelArg(k_initialize8[i], 5, sizeof(int), &cell_low[i][c][0]);
    ecode |= clSetKernelArg(k_initialize8[i], 6, sizeof(int), &cell_high[i][c][0]);
    ecode |= clSetKernelArg(k_initialize8[i], 7, sizeof(int), &cell_size[i][c][2]);
    clu_CheckError(ecode, "clSetKernelArg() for initialize8");

    d[0] = cell_size[i][c][0];
    d[1] = cell_size[i][c][1];
    compute_ws_dim_2(d, local_ws, global_ws);

    ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                   k_initialize8[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for initialize8");
  }

  for (i = 0; i < num_devices; i++) {
    CHECK_FINISH(i * 2);
  }
}


void lhsinit()
{
  int i, d, c;
  size_t dim[3], local_ws[3], global_ws[3];
  cl_int ecode = 0;


  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      ecode  = clSetKernelArg(k_lhsinit[i], 0, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_lhsinit[i], 1, sizeof(int), &c);
      ecode |= clSetKernelArg(k_lhsinit[i], 2, sizeof(int), &cell_size[i][c][2]);
      ecode |= clSetKernelArg(k_lhsinit[i], 3, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_lhsinit[i], 4, sizeof(int), &cell_size[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for lhsinit");

      dim[0] = cell_size[i][c][1];
      dim[1] = cell_size[i][c][2];
      compute_ws_dim_2(dim, local_ws, global_ws);

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_lhsinit[i],
                                     2, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for lhsinit");
    }
    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }
  }

  for (i = 0; i < num_devices; i++) {
    //---------------------------------------------------------------------
    // loop over all cells                                       
    //---------------------------------------------------------------------
    for (c = 0; c < ncells; c++) {

      //---------------------------------------------------------------------
      // first, initialize the start and end arrays
      //---------------------------------------------------------------------
      for (d = 0; d < 3; d++) {
        if (cell_coord[i][c][d] == 0) {
          cell_start[i][c][d] = 1;
        } else { 
          cell_start[i][c][d] = 0;
        }
        if (cell_coord[i][c][d] == ncells-1) {
          cell_end[i][c][d] = 1;
        } else {
          cell_end[i][c][d] = 0;
        }
      }
    }
  }

  for (i = 0; i < num_devices; i++)
    CHECK_FINISH(i * 2);
}

