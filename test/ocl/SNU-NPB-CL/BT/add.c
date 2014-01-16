//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB BT code for multiple    //
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
// addition of update to the vector u
//---------------------------------------------------------------------
void add()
{
  int i;
  size_t d0_size, d1_size, d2_size;
  cl_int ecode;

  for (i = 0; i < num_devices; i++) {
    size_t add_lws[3], add_gws[3], temp;

    if (ADD_DIM == 3) {
      d0_size = max_cell_size[i][1];
      d1_size = max_cell_size[i][2];
      d2_size = ncells;

      add_lws[0] = d0_size < work_item_sizes[0] ?
                   d0_size : work_item_sizes[0];
      temp = max_work_group_size / add_lws[0];
      add_lws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / add_lws[1];
      add_lws[2] = d2_size < temp ? d2_size : temp;
      add_gws[0] = clu_RoundWorkSize(d0_size, add_lws[0]);
      add_gws[1] = clu_RoundWorkSize(d1_size, add_lws[1]);
      add_gws[2] = clu_RoundWorkSize(d2_size, add_lws[2]);
    } else {
      d0_size = max_cell_size[i][2];
      d1_size = ncells;

      add_lws[0] = d0_size < work_item_sizes[0] ?
                   d0_size : work_item_sizes[0];
      temp = max_work_group_size / add_lws[0];
      add_lws[1] = d1_size < temp ? d1_size : temp;
      add_gws[0] = clu_RoundWorkSize(d0_size, add_lws[0]);
      add_gws[1] = clu_RoundWorkSize(d1_size, add_lws[1]);
    }

    ecode  = clSetKernelArg(k_add[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_add[i], 1, sizeof(cl_mem), &m_rhs[i]);
    ecode |= clSetKernelArg(k_add[i], 2, sizeof(cl_mem), &m_cell_size[i]);
    ecode |= clSetKernelArg(k_add[i], 3, sizeof(cl_mem), &m_start[i]);
    ecode |= clSetKernelArg(k_add[i], 4, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_add[i], 5, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_add[i],
                                   ADD_DIM, NULL,
                                   add_gws,
                                   add_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }

  CHECK_FINISH();
}

