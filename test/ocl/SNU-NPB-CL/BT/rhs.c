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
#include "timers.h"

void compute_rhs()
{
  int i;
  size_t d0_size, d1_size, d2_size;
  cl_int ecode;

  if (timeron) timer_start(t_rhs);

  //-------------------------------------------------------------------------
  // compute the reciprocal of density, and the kinetic energy, 
  // and the speed of sound. 
  //-------------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    size_t compute_rhs1_lws[3], compute_rhs1_gws[3], temp;

    if (COMPUTE_RHS1_DIM == 3) {
      d0_size = max_cell_size[i][1] + 2;
      d1_size = max_cell_size[i][2] + 2;
      d2_size = ncells;

      compute_rhs1_lws[0] = d0_size < work_item_sizes[0] ?
                            d0_size : work_item_sizes[0];
      temp = max_work_group_size / compute_rhs1_lws[0];
      compute_rhs1_lws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / compute_rhs1_lws[1];
      compute_rhs1_lws[2] = d2_size < temp ? d2_size : temp;
      compute_rhs1_gws[0] = clu_RoundWorkSize(d0_size, compute_rhs1_lws[0]);
      compute_rhs1_gws[1] = clu_RoundWorkSize(d1_size, compute_rhs1_lws[1]);
      compute_rhs1_gws[2] = clu_RoundWorkSize(d2_size, compute_rhs1_lws[2]);
    } else {
      d0_size = max_cell_size[i][2] + 2;
      d1_size = ncells;

      compute_rhs1_lws[0] = d0_size < work_item_sizes[0] ?
                            d0_size : work_item_sizes[0];
      temp = max_work_group_size / compute_rhs1_lws[0];
      compute_rhs1_lws[1] = d1_size < temp ? d1_size : temp;
      compute_rhs1_gws[0] = clu_RoundWorkSize(d0_size, compute_rhs1_lws[0]);
      compute_rhs1_gws[1] = clu_RoundWorkSize(d1_size, compute_rhs1_lws[1]);
    }

    ecode  = clSetKernelArg(k_compute_rhs1[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_compute_rhs1[i], 1, sizeof(cl_mem), &m_us[i]);
    ecode |= clSetKernelArg(k_compute_rhs1[i], 2, sizeof(cl_mem), &m_vs[i]);
    ecode |= clSetKernelArg(k_compute_rhs1[i], 3, sizeof(cl_mem), &m_ws[i]);
    ecode |= clSetKernelArg(k_compute_rhs1[i], 4, sizeof(cl_mem), &m_qs[i]);
    ecode |= clSetKernelArg(k_compute_rhs1[i], 5, sizeof(cl_mem),
                                                  &m_rho_i[i]);
    ecode |= clSetKernelArg(k_compute_rhs1[i], 6, sizeof(cl_mem),
                                                  &m_square[i]);
    ecode |= clSetKernelArg(k_compute_rhs1[i], 7, sizeof(cl_mem),
                                                  &m_cell_size[i]);
    ecode |= clSetKernelArg(k_compute_rhs1[i], 8, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_compute_rhs1[i],
                                   COMPUTE_RHS1_DIM, NULL,
                                   compute_rhs1_gws,
                                   compute_rhs1_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-------------------------------------------------------------------------

  //-------------------------------------------------------------------------
  // copy the exact forcing term to the right hand side;  because 
  // this forcing term is known, we can store it on the whole of every 
  // cell,  including the boundary                   
  //-------------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    size_t compute_rhs2_lws[3], compute_rhs2_gws[3], temp;

    if (COMPUTE_RHS2_DIM == 3) {
      d0_size = max_cell_size[i][1];
      d1_size = max_cell_size[i][2];
      d2_size = ncells;

      compute_rhs2_lws[0] = d0_size < work_item_sizes[0] ?
                            d0_size : work_item_sizes[0];
      temp = max_work_group_size / compute_rhs2_lws[0];
      compute_rhs2_lws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / compute_rhs2_lws[1];
      compute_rhs2_lws[2] = d2_size < temp ? d2_size : temp;
      compute_rhs2_gws[0] = clu_RoundWorkSize(d0_size, compute_rhs2_lws[0]);
      compute_rhs2_gws[1] = clu_RoundWorkSize(d1_size, compute_rhs2_lws[1]);
      compute_rhs2_gws[2] = clu_RoundWorkSize(d2_size, compute_rhs2_lws[2]);
    } else {
      d0_size = max_cell_size[i][2];
      d1_size = ncells;

      compute_rhs2_lws[0] = d0_size < work_item_sizes[0] ?
                            d0_size : work_item_sizes[0];
      temp = max_work_group_size / compute_rhs2_lws[0];
      compute_rhs2_lws[1] = d1_size < temp ? d1_size : temp;
      compute_rhs2_gws[0] = clu_RoundWorkSize(d0_size, compute_rhs2_lws[0]);
      compute_rhs2_gws[1] = clu_RoundWorkSize(d1_size, compute_rhs2_lws[1]);
    }

    ecode  = clSetKernelArg(k_compute_rhs2[i], 0, sizeof(cl_mem),
                                                  &m_forcing[i]);
    ecode |= clSetKernelArg(k_compute_rhs2[i], 1, sizeof(cl_mem), &m_rhs[i]);
    ecode |= clSetKernelArg(k_compute_rhs2[i], 2, sizeof(cl_mem),
                                                  &m_cell_size[i]);
    ecode |= clSetKernelArg(k_compute_rhs2[i], 3, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_compute_rhs2[i],
                                   COMPUTE_RHS2_DIM, NULL,
                                   compute_rhs2_gws,
                                   compute_rhs2_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-------------------------------------------------------------------------

  //-------------------------------------------------------------------------
  // compute xi-direction fluxes 
  //-------------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    size_t compute_rhs3_lws[3], compute_rhs3_gws[3], temp;

    d0_size = max_cell_size[i][1];
    d1_size = max_cell_size[i][2];
    d2_size = ncells;

    compute_rhs3_lws[0] = d0_size < work_item_sizes[0] ?
                          d0_size : work_item_sizes[0];
    temp = max_work_group_size / compute_rhs3_lws[0];
    compute_rhs3_lws[1] = d1_size < temp ? d1_size : temp;
    temp = temp / compute_rhs3_lws[1];
    compute_rhs3_lws[2] = d2_size < temp ? d2_size : temp;
    compute_rhs3_gws[0] = clu_RoundWorkSize(d0_size, compute_rhs3_lws[0]);
    compute_rhs3_gws[1] = clu_RoundWorkSize(d1_size, compute_rhs3_lws[1]);
    compute_rhs3_gws[2] = clu_RoundWorkSize(d2_size, compute_rhs3_lws[2]);

    ecode  = clSetKernelArg(k_compute_rhs3[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 1, sizeof(cl_mem), &m_us[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 2, sizeof(cl_mem), &m_vs[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 3, sizeof(cl_mem), &m_ws[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 4, sizeof(cl_mem), &m_qs[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 5, sizeof(cl_mem),
                                                  &m_rho_i[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 6, sizeof(cl_mem),
                                                  &m_square[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 7, sizeof(cl_mem), &m_rhs[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 8, sizeof(cl_mem),
                                                  &m_cell_size[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 9, sizeof(cl_mem),&m_start[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 10, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_compute_rhs3[i], 11, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_compute_rhs3[i],
                                   3, NULL,
                                   compute_rhs3_gws,
                                   compute_rhs3_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-------------------------------------------------------------------------

  //-------------------------------------------------------------------------
  // compute eta-direction fluxes 
  //-------------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    size_t compute_rhs4_lws[3], compute_rhs4_gws[3], temp;

    d0_size = max_cell_size[i][0];
    d1_size = max_cell_size[i][2];
    d2_size = ncells;

    compute_rhs4_lws[0] = d0_size < work_item_sizes[0] ?
                          d0_size : work_item_sizes[0];
    temp = max_work_group_size / compute_rhs4_lws[0];
    compute_rhs4_lws[1] = d1_size < temp ? d1_size : temp;
    temp = temp / compute_rhs4_lws[1];
    compute_rhs4_lws[2] = d2_size < temp ? d2_size : temp;
    compute_rhs4_gws[0] = clu_RoundWorkSize(d0_size, compute_rhs4_lws[0]);
    compute_rhs4_gws[1] = clu_RoundWorkSize(d1_size, compute_rhs4_lws[1]);
    compute_rhs4_gws[2] = clu_RoundWorkSize(d2_size, compute_rhs4_lws[2]);

    ecode  = clSetKernelArg(k_compute_rhs4[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 1, sizeof(cl_mem), &m_us[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 2, sizeof(cl_mem), &m_vs[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 3, sizeof(cl_mem), &m_ws[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 4, sizeof(cl_mem), &m_qs[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 5, sizeof(cl_mem),
                                                  &m_rho_i[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 6, sizeof(cl_mem),
                                                  &m_square[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 7, sizeof(cl_mem), &m_rhs[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 8, sizeof(cl_mem),
                                                  &m_cell_size[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 9, sizeof(cl_mem),&m_start[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 10, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_compute_rhs4[i], 11, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_compute_rhs4[i],
                                   3, NULL,
                                   compute_rhs4_gws,
                                   compute_rhs4_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-------------------------------------------------------------------------

  //-------------------------------------------------------------------------
  // compute zeta-direction fluxes 
  //-------------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    size_t compute_rhs5_lws[3], compute_rhs5_gws[3], temp;

    d0_size = max_cell_size[i][0];
    d1_size = max_cell_size[i][1];
    d2_size = ncells;

    compute_rhs5_lws[0] = d0_size < work_item_sizes[0] ?
                          d0_size : work_item_sizes[0];
    temp = max_work_group_size / compute_rhs5_lws[0];
    compute_rhs5_lws[1] = d1_size < temp ? d1_size : temp;
    temp = temp / compute_rhs5_lws[1];
    compute_rhs5_lws[2] = d2_size < temp ? d2_size : temp;
    compute_rhs5_gws[0] = clu_RoundWorkSize(d0_size, compute_rhs5_lws[0]);
    compute_rhs5_gws[1] = clu_RoundWorkSize(d1_size, compute_rhs5_lws[1]);
    compute_rhs5_gws[2] = clu_RoundWorkSize(d2_size, compute_rhs5_lws[2]);

    ecode  = clSetKernelArg(k_compute_rhs5[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 1, sizeof(cl_mem), &m_us[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 2, sizeof(cl_mem), &m_vs[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 3, sizeof(cl_mem), &m_ws[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 4, sizeof(cl_mem), &m_qs[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 5, sizeof(cl_mem),
                                                  &m_rho_i[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 6, sizeof(cl_mem),
                                                  &m_square[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 7, sizeof(cl_mem), &m_rhs[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 8, sizeof(cl_mem),
                                                  &m_cell_size[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 9, sizeof(cl_mem),&m_start[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 10, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_compute_rhs5[i], 11, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_compute_rhs5[i],
                                   3, NULL,
                                   compute_rhs5_gws,
                                   compute_rhs5_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-------------------------------------------------------------------------

  //-------------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    size_t compute_rhs6_lws[3], compute_rhs6_gws[3], temp;

    if (COMPUTE_RHS6_DIM == 3) {
      d0_size = max_cell_size[i][1];
      d1_size = max_cell_size[i][2];
      d2_size = ncells;

      compute_rhs6_lws[0] = d0_size < work_item_sizes[0] ?
                            d0_size : work_item_sizes[0];
      temp = max_work_group_size / compute_rhs6_lws[0];
      compute_rhs6_lws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / compute_rhs6_lws[1];
      compute_rhs6_lws[2] = d2_size < temp ? d2_size : temp;
      compute_rhs6_gws[0] = clu_RoundWorkSize(d0_size, compute_rhs6_lws[0]);
      compute_rhs6_gws[1] = clu_RoundWorkSize(d1_size, compute_rhs6_lws[1]);
      compute_rhs6_gws[2] = clu_RoundWorkSize(d2_size, compute_rhs6_lws[2]);
    } else {
      d0_size = max_cell_size[i][2];
      d1_size = ncells;

      compute_rhs6_lws[0] = d0_size < work_item_sizes[0] ?
                            d0_size : work_item_sizes[0];
      temp = max_work_group_size / compute_rhs6_lws[0];
      compute_rhs6_lws[1] = d1_size < temp ? d1_size : temp;
      compute_rhs6_gws[0] = clu_RoundWorkSize(d0_size, compute_rhs6_lws[0]);
      compute_rhs6_gws[1] = clu_RoundWorkSize(d1_size, compute_rhs6_lws[1]);
    }

    ecode  = clSetKernelArg(k_compute_rhs6[i], 0, sizeof(cl_mem), &m_rhs[i]);
    ecode |= clSetKernelArg(k_compute_rhs6[i], 1, sizeof(cl_mem),
                                                  &m_cell_size[i]);
    ecode |= clSetKernelArg(k_compute_rhs6[i], 2, sizeof(cl_mem),&m_start[i]);
    ecode |= clSetKernelArg(k_compute_rhs6[i], 3, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_compute_rhs6[i], 4, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_compute_rhs6[i],
                                   COMPUTE_RHS6_DIM, NULL,
                                   compute_rhs6_gws,
                                   compute_rhs6_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-------------------------------------------------------------------------

  CHECK_FINISH();
  if (timeron) timer_stop(t_rhs);
}
