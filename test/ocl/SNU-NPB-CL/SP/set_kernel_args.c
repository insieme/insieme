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
#include "sp_dim.h"

void set_kernel_args()
{
  int i, c;
  cl_int ecode = 0;

  if (device_type == CL_DEVICE_TYPE_CPU) {
    COPY_FACES1_DIM = COPY_FACES1_DIM_CPU;
    COPY_FACES2_DIM = COPY_FACES2_DIM_CPU;
    COPY_FACES3_DIM = COPY_FACES3_DIM_CPU;
    COPY_FACES4_DIM = COPY_FACES4_DIM_CPU;
    COPY_FACES5_DIM = COPY_FACES5_DIM_CPU;
    COPY_FACES6_DIM = COPY_FACES6_DIM_CPU;
    COMPUTE_RHS1_DIM = COMPUTE_RHS1_DIM_CPU;
    COMPUTE_RHS2_DIM = COMPUTE_RHS2_DIM_CPU;
    COMPUTE_RHS6_DIM = COMPUTE_RHS6_DIM_CPU;
    TXINVR_DIM = TXINVR_DIM_CPU;
    NINVR_DIM = NINVR_DIM_CPU;
    X_SOLVE6_DIM = X_SOLVE6_DIM_CPU;
    PINVR_DIM = PINVR_DIM_CPU;
    Y_SOLVE6_DIM = Y_SOLVE6_DIM_CPU;
    TZETAR_DIM = TZETAR_DIM_CPU;
    Z_SOLVE6_DIM = Z_SOLVE6_DIM_CPU;
    ADD_DIM = ADD_DIM_CPU;
  } else {
    COPY_FACES1_DIM = COPY_FACES1_DIM_GPU;
    COPY_FACES2_DIM = COPY_FACES2_DIM_GPU;
    COPY_FACES3_DIM = COPY_FACES3_DIM_GPU;
    COPY_FACES4_DIM = COPY_FACES4_DIM_GPU;
    COPY_FACES5_DIM = COPY_FACES5_DIM_GPU;
    COPY_FACES6_DIM = COPY_FACES6_DIM_GPU;
    COMPUTE_RHS1_DIM = COMPUTE_RHS1_DIM_GPU;
    COMPUTE_RHS2_DIM = COMPUTE_RHS2_DIM_GPU;
    COMPUTE_RHS6_DIM = COMPUTE_RHS6_DIM_GPU;
    TXINVR_DIM = TXINVR_DIM_GPU;
    NINVR_DIM = NINVR_DIM_GPU;
    X_SOLVE6_DIM = X_SOLVE6_DIM_GPU;
    PINVR_DIM = PINVR_DIM_GPU;
    Y_SOLVE6_DIM = Y_SOLVE6_DIM_GPU;
    TZETAR_DIM = TZETAR_DIM_GPU;
    Z_SOLVE6_DIM = Z_SOLVE6_DIM_GPU;
    ADD_DIM = ADD_DIM_GPU;
  }

  // copy_faces

  int p0, p1, p2, p3, p4, p5;
  int range_k, range_j, range_i;
  int minus1 = -1;

  for (i = 0; i < num_devices; i++) {
    p0 = start_send_east[i];
    p1 = start_send_west[i];
    p2 = start_send_north[i];
    p3 = start_send_south[i];
    p4 = start_send_top[i];
    p5 = start_send_bottom[i];

    for (c = 0; c < ncells; c++) {
      // copy_faces1

      ecode  = clSetKernelArg(k_copy_faces1[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_copy_faces1[i][c], 1, sizeof(cl_mem), &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_copy_faces1[i][c], 2, sizeof(int), &c);
      if (cell_coord[i][c][0] != (ncells-1))
        ecode |= clSetKernelArg(k_copy_faces1[i][c], 3, sizeof(int), &p0);
      else
        ecode |= clSetKernelArg(k_copy_faces1[i][c], 3, sizeof(int), &minus1);
      if (cell_coord[i][c][0] != 0)
        ecode |= clSetKernelArg(k_copy_faces1[i][c], 4, sizeof(int), &p1);
      else
        ecode |= clSetKernelArg(k_copy_faces1[i][c], 4, sizeof(int), &minus1);
      ecode |= clSetKernelArg(k_copy_faces1[i][c], 5, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_copy_faces1[i][c], 6, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_copy_faces1[i][c], 7, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for copy_faces1");

      if (COPY_FACES1_DIM == 2) {
        copy_faces1_dim[i][c][0] = cell_size[i][c][1];
        copy_faces1_dim[i][c][1] = cell_size[i][c][2];
        compute_ws_dim_2(copy_faces1_dim[i][c],
                         copy_faces1_lw[i][c],
                         copy_faces1_gw[i][c]);
      } else {
        copy_faces1_dim[i][c][0] = cell_size[i][c][2];
        compute_ws_dim_1(copy_faces1_dim[i][c],
                         copy_faces1_lw[i][c],
                         copy_faces1_gw[i][c]);
      }

      if (cell_coord[i][c][0] != (ncells-1))
        p0 += cell_size[i][c][2] * cell_size[i][c][1] * 2 * 5;
      if (cell_coord[i][c][0] != 0)
        p1 += cell_size[i][c][2] * cell_size[i][c][1] * 2 * 5;

      // copy_faces2

      ecode  = clSetKernelArg(k_copy_faces2[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_copy_faces2[i][c], 1, sizeof(cl_mem), &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_copy_faces2[i][c], 2, sizeof(int), &c);
      if (cell_coord[i][c][1] != (ncells-1))
        ecode |= clSetKernelArg(k_copy_faces2[i][c], 3, sizeof(int), &p2);
      else
        ecode |= clSetKernelArg(k_copy_faces2[i][c], 3, sizeof(int), &minus1);
      if (cell_coord[i][c][1] != 0)
        ecode |= clSetKernelArg(k_copy_faces2[i][c], 4, sizeof(int), &p3);
      else
        ecode |= clSetKernelArg(k_copy_faces2[i][c], 4, sizeof(int), &minus1);
      ecode |= clSetKernelArg(k_copy_faces2[i][c], 5, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_copy_faces2[i][c], 6, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_copy_faces2[i][c], 7, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for copy_faces2");

      if (COPY_FACES2_DIM == 2) {
        copy_faces2_dim[i][c][0] = cell_size[i][c][0];
        copy_faces2_dim[i][c][1] = cell_size[i][c][2];
        compute_ws_dim_2(copy_faces2_dim[i][c],
                         copy_faces2_lw[i][c],
                         copy_faces2_gw[i][c]);
      } else {
        copy_faces2_dim[i][c][0] = cell_size[i][c][2];
        compute_ws_dim_1(copy_faces2_dim[i][c],
                         copy_faces2_lw[i][c],
                         copy_faces2_gw[i][c]);
      }

      if (cell_coord[i][c][1] != (ncells-1))
        p2 += cell_size[i][c][2] * cell_size[i][c][0] * 2 * 5;
      if (cell_coord[i][c][1] != 0)
        p3 += cell_size[i][c][2] * cell_size[i][c][0] * 2 * 5;

      // copy_faces3

      ecode  = clSetKernelArg(k_copy_faces3[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_copy_faces3[i][c], 1, sizeof(cl_mem), &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_copy_faces3[i][c], 2, sizeof(int), &c);
      if (cell_coord[i][c][2] != (ncells-1))
        ecode |= clSetKernelArg(k_copy_faces3[i][c], 3, sizeof(int), &p4);
      else
        ecode |= clSetKernelArg(k_copy_faces3[i][c], 3, sizeof(int), &minus1);
      if (cell_coord[i][c][2] != 0)
        ecode |= clSetKernelArg(k_copy_faces3[i][c], 4, sizeof(int), &p5);
      else
        ecode |= clSetKernelArg(k_copy_faces3[i][c], 4, sizeof(int), &minus1);
      ecode |= clSetKernelArg(k_copy_faces3[i][c], 5, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_copy_faces3[i][c], 6, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_copy_faces3[i][c], 7, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for copy_faces3");

      if (COPY_FACES3_DIM == 2) {
        copy_faces3_dim[i][c][0] = cell_size[i][c][0];
        copy_faces3_dim[i][c][1] = cell_size[i][c][1];
        compute_ws_dim_2(copy_faces3_dim[i][c],
                         copy_faces3_lw[i][c],
                         copy_faces3_gw[i][c]);
      } else {
        copy_faces3_dim[i][c][0] = cell_size[i][c][1];
        compute_ws_dim_1(copy_faces3_dim[i][c],
                         copy_faces3_lw[i][c],
                         copy_faces3_gw[i][c]);
      }

      if (cell_coord[i][c][2] != (ncells-1))
        p4 += cell_size[i][c][1] * cell_size[i][c][0] * 2 * 5;
      if (cell_coord[i][c][2] != 0)
        p5 += cell_size[i][c][1] * cell_size[i][c][0] * 2 * 5;
    }

    p0 = start_recv_west[i];
    p1 = start_recv_east[i];
    p2 = start_recv_south[i];
    p3 = start_recv_north[i];
    p4 = start_recv_bottom[i];
    p5 = start_recv_top[i];

    for (c = 0; c < ncells; c++) {
      // copy_faces4

      ecode  = clSetKernelArg(k_copy_faces4[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_copy_faces4[i][c], 1, sizeof(cl_mem), &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_copy_faces4[i][c], 2, sizeof(int), &c);
      if (cell_coord[i][c][0] != 0)
        ecode |= clSetKernelArg(k_copy_faces4[i][c], 3, sizeof(int), &p0);
      else
        ecode |= clSetKernelArg(k_copy_faces4[i][c], 3, sizeof(int), &minus1);
      if (cell_coord[i][c][0] != (ncells-1))
        ecode |= clSetKernelArg(k_copy_faces4[i][c], 4, sizeof(int), &p1);
      else
        ecode |= clSetKernelArg(k_copy_faces4[i][c], 4, sizeof(int), &minus1);
      ecode |= clSetKernelArg(k_copy_faces4[i][c], 5, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_copy_faces4[i][c], 6, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_copy_faces4[i][c], 7, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for copy_faces4");

      if (COPY_FACES4_DIM == 2) {
        copy_faces4_dim[i][c][0] = cell_size[i][c][1];
        copy_faces4_dim[i][c][1] = cell_size[i][c][2];
        compute_ws_dim_2(copy_faces4_dim[i][c],
                         copy_faces4_lw[i][c],
                         copy_faces4_gw[i][c]);
      } else {
        copy_faces4_dim[i][c][0] = cell_size[i][c][2];
        compute_ws_dim_1(copy_faces4_dim[i][c],
                         copy_faces4_lw[i][c],
                         copy_faces4_gw[i][c]);
      }

      if (cell_coord[i][c][0] != 0)
        p0 += cell_size[i][c][2] * cell_size[i][c][1] * 2 * 5;
      if (cell_coord[i][c][0] != (ncells-1))
        p1 += cell_size[i][c][2] * cell_size[i][c][1] * 2 * 5;

      // copy_faces5

      ecode  = clSetKernelArg(k_copy_faces5[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_copy_faces5[i][c], 1, sizeof(cl_mem), &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_copy_faces5[i][c], 2, sizeof(int), &c);
      if (cell_coord[i][c][1] != 0)
        ecode |= clSetKernelArg(k_copy_faces5[i][c], 3, sizeof(int), &p2);
      else
        ecode |= clSetKernelArg(k_copy_faces5[i][c], 3, sizeof(int), &minus1);
      if (cell_coord[i][c][1] != (ncells-1))
        ecode |= clSetKernelArg(k_copy_faces5[i][c], 4, sizeof(int), &p3);
      else
        ecode |= clSetKernelArg(k_copy_faces5[i][c], 4, sizeof(int), &minus1);
      ecode |= clSetKernelArg(k_copy_faces5[i][c], 5, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_copy_faces5[i][c], 6, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_copy_faces5[i][c], 7, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for copy_faces5");

      if (COPY_FACES5_DIM == 2) {
        copy_faces5_dim[i][c][0] = cell_size[i][c][0];
        copy_faces5_dim[i][c][1] = cell_size[i][c][2];
        compute_ws_dim_2(copy_faces5_dim[i][c],
                         copy_faces5_lw[i][c],
                         copy_faces5_gw[i][c]);
      } else {
        copy_faces5_dim[i][c][0] = cell_size[i][c][2];
        compute_ws_dim_1(copy_faces5_dim[i][c],
                         copy_faces5_lw[i][c],
                         copy_faces5_gw[i][c]);
      }

      if (cell_coord[i][c][1] != 0)
        p2 += cell_size[i][c][2] * cell_size[i][c][0] * 2 * 5;
      if (cell_coord[i][c][1] != (ncells-1))
        p3 += cell_size[i][c][2] * cell_size[i][c][0] * 2 * 5;

      // copy_faces6

      ecode  = clSetKernelArg(k_copy_faces6[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_copy_faces6[i][c], 1, sizeof(cl_mem), &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_copy_faces6[i][c], 2, sizeof(int), &c);
      if (cell_coord[i][c][2] != 0)
        ecode |= clSetKernelArg(k_copy_faces6[i][c], 3, sizeof(int), &p4);
      else
        ecode |= clSetKernelArg(k_copy_faces6[i][c], 3, sizeof(int), &minus1);
      if (cell_coord[i][c][2] != (ncells-1))
        ecode |= clSetKernelArg(k_copy_faces6[i][c], 4, sizeof(int), &p5);
      else
        ecode |= clSetKernelArg(k_copy_faces6[i][c], 4, sizeof(int), &minus1);
      ecode |= clSetKernelArg(k_copy_faces6[i][c], 5, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_copy_faces6[i][c], 6, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_copy_faces6[i][c], 7, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for copy_faces6");

      if (COPY_FACES6_DIM == 2) {
        copy_faces6_dim[i][c][0] = cell_size[i][c][0];
        copy_faces6_dim[i][c][1] = cell_size[i][c][1];
        compute_ws_dim_2(copy_faces6_dim[i][c],
                         copy_faces6_lw[i][c],
                         copy_faces6_gw[i][c]);
      } else {
        copy_faces6_dim[i][c][0] = cell_size[i][c][1];
        compute_ws_dim_1(copy_faces6_dim[i][c],
                         copy_faces6_lw[i][c],
                         copy_faces6_gw[i][c]);
      }

      if (cell_coord[i][c][2] != 0)
        p4 += cell_size[i][c][1] * cell_size[i][c][0] * 2 * 5;
      if (cell_coord[i][c][2] != (ncells-1))
        p5 += cell_size[i][c][1] * cell_size[i][c][0] * 2 * 5;

    }
  }

  // compute_rhs

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      // compute_rhs1

      ecode  = clSetKernelArg(k_compute_rhs1[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 1, sizeof(cl_mem), &m_us[i]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 2, sizeof(cl_mem), &m_vs[i]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 3, sizeof(cl_mem), &m_ws[i]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 4, sizeof(cl_mem), &m_qs[i]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 5, sizeof(cl_mem), &m_ainv[i]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 6, sizeof(cl_mem), &m_rho_i[i]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 7, sizeof(cl_mem), &m_speed[i]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 8, sizeof(cl_mem), &m_square[i]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 9, sizeof(int), &c);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 10, sizeof(int), &cell_size[i][c][2]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 11, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_compute_rhs1[i][c], 12, sizeof(int), &cell_size[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for compute_rhs1");

      if (COMPUTE_RHS1_DIM == 3) {
        compute_rhs1_dim[i][c][0] = cell_size[i][c][0]+2;
        compute_rhs1_dim[i][c][1] = cell_size[i][c][1]+2;
        compute_rhs1_dim[i][c][2] = cell_size[i][c][2]+2;
        compute_ws_dim_3(compute_rhs1_dim[i][c],
                         compute_rhs1_lw[i][c],
                         compute_rhs1_gw[i][c]);
      } else if (COMPUTE_RHS1_DIM == 2) {
        compute_rhs1_dim[i][c][0] = cell_size[i][c][1]+2;
        compute_rhs1_dim[i][c][1] = cell_size[i][c][2]+2;
        compute_ws_dim_2(compute_rhs1_dim[i][c],
                         compute_rhs1_lw[i][c],
                         compute_rhs1_gw[i][c]);
      } else {
        compute_rhs1_dim[i][c][0] = cell_size[i][c][2]+2;
        compute_ws_dim_1(compute_rhs1_dim[i][c],
                         compute_rhs1_lw[i][c],
                         compute_rhs1_gw[i][c]);
      }

      // compute_rhs2

      ecode  = clSetKernelArg(k_compute_rhs2[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_compute_rhs2[i][c], 1, sizeof(cl_mem), &m_forcing[i]);
      ecode |= clSetKernelArg(k_compute_rhs2[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_compute_rhs2[i][c], 3, sizeof(int), &cell_size[i][c][2]);
      ecode |= clSetKernelArg(k_compute_rhs2[i][c], 4, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_compute_rhs2[i][c], 5, sizeof(int), &cell_size[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for compute_rhs2");

      if (COMPUTE_RHS2_DIM == 3) {
        compute_rhs2_dim[i][c][0] = cell_size[i][c][0];
        compute_rhs2_dim[i][c][1] = cell_size[i][c][1];
        compute_rhs2_dim[i][c][2] = cell_size[i][c][2];
        compute_ws_dim_3(compute_rhs2_dim[i][c],
                         compute_rhs2_lw[i][c],
                         compute_rhs2_gw[i][c]);
      } else if (COMPUTE_RHS2_DIM == 2) {
        compute_rhs2_dim[i][c][0] = cell_size[i][c][1];
        compute_rhs2_dim[i][c][1] = cell_size[i][c][2];
        compute_ws_dim_2(compute_rhs2_dim[i][c],
                         compute_rhs2_lw[i][c],
                         compute_rhs2_gw[i][c]);
      } else {
        compute_rhs2_dim[i][c][0] = cell_size[i][c][2];
        compute_ws_dim_1(compute_rhs2_dim[i][c],
                         compute_rhs2_lw[i][c],
                         compute_rhs2_gw[i][c]);
      }

      // compute_rhs3

      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_j = cell_size[i][c][1] - cell_end[i][c][1];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_compute_rhs3[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 1, sizeof(cl_mem), &m_us[i]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 2, sizeof(cl_mem), &m_vs[i]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 3, sizeof(cl_mem), &m_ws[i]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 4, sizeof(cl_mem), &m_qs[i]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 5, sizeof(cl_mem), &m_rho_i[i]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 6, sizeof(cl_mem), &m_square[i]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 7, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 8, sizeof(int), &c);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 9, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 10, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 11, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 12, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 13, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 14, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_compute_rhs3[i][c], 15, sizeof(int), &cell_end[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for compute_rhs3");

      compute_rhs3_dim[i][c][0] = range_j - cell_start[i][c][1];
      compute_rhs3_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(compute_rhs3_dim[i][c],
                       compute_rhs3_lw[i][c],
                       compute_rhs3_gw[i][c]);

      // compute_rhs4

      ecode  = clSetKernelArg(k_compute_rhs4[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 1, sizeof(cl_mem), &m_us[i]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 2, sizeof(cl_mem), &m_vs[i]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 3, sizeof(cl_mem), &m_ws[i]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 4, sizeof(cl_mem), &m_qs[i]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 5, sizeof(cl_mem), &m_rho_i[i]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 6, sizeof(cl_mem), &m_square[i]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 7, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 8, sizeof(int), &c);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 9, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 10, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 11, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 12, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 13, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 14, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_compute_rhs4[i][c], 15, sizeof(int), &cell_end[i][c][1]);
      clu_CheckError(ecode, "clSetKernelArg() for compute_rhs4");

      compute_rhs4_dim[i][c][0] = range_i - cell_start[i][c][0];
      compute_rhs4_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(compute_rhs4_dim[i][c],
                       compute_rhs4_lw[i][c],
                       compute_rhs4_gw[i][c]);

      // compute_rhs5

      ecode  = clSetKernelArg(k_compute_rhs5[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 1, sizeof(cl_mem), &m_us[i]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 2, sizeof(cl_mem), &m_vs[i]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 3, sizeof(cl_mem), &m_ws[i]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 4, sizeof(cl_mem), &m_qs[i]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 5, sizeof(cl_mem), &m_rho_i[i]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 6, sizeof(cl_mem), &m_square[i]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 7, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 8, sizeof(int), &c);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 9, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 10, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 11, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 12, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 13, sizeof(int), &cell_size[i][c][2]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 14, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_compute_rhs5[i][c], 15, sizeof(int), &cell_end[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for compute_rhs5");

      compute_rhs5_dim[i][c][0] = range_i - cell_start[i][c][0];
      compute_rhs5_dim[i][c][1] = range_j - cell_start[i][c][1];
      compute_ws_dim_2(compute_rhs5_dim[i][c],
                       compute_rhs5_lw[i][c],
                       compute_rhs5_gw[i][c]);

      // compute_rhs6

      ecode  = clSetKernelArg(k_compute_rhs6[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_compute_rhs6[i][c], 1, sizeof(int), &c);
      ecode |= clSetKernelArg(k_compute_rhs6[i][c], 2, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_compute_rhs6[i][c], 3, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_compute_rhs6[i][c], 4, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_compute_rhs6[i][c], 5, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_compute_rhs6[i][c], 6, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_compute_rhs6[i][c], 7, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for compute_rhs6");

      if (COMPUTE_RHS6_DIM == 3) {
        compute_rhs6_dim[i][c][0] = range_i - cell_start[i][c][0];
        compute_rhs6_dim[i][c][1] = range_j - cell_start[i][c][1];
        compute_rhs6_dim[i][c][2] = range_k - cell_start[i][c][2];
        compute_ws_dim_3(compute_rhs6_dim[i][c],
                         compute_rhs6_lw[i][c],
                         compute_rhs6_gw[i][c]);
      } else if (COMPUTE_RHS6_DIM == 2) {
        compute_rhs6_dim[i][c][0] = range_j - cell_start[i][c][1];
        compute_rhs6_dim[i][c][1] = range_k - cell_start[i][c][2];
        compute_ws_dim_2(compute_rhs6_dim[i][c],
                         compute_rhs6_lw[i][c],
                         compute_rhs6_gw[i][c]);
      } else {
        compute_rhs6_dim[i][c][0] = range_k - cell_start[i][c][2];
        compute_ws_dim_1(compute_rhs6_dim[i][c],
                         compute_rhs6_lw[i][c],
                         compute_rhs6_gw[i][c]);
      }
    }
  }

  // txinvr

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_j = cell_size[i][c][1] - cell_end[i][c][1];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_txinvr[i][c], 0, sizeof(cl_mem), &m_us[i]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 1, sizeof(cl_mem), &m_vs[i]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 2, sizeof(cl_mem), &m_ws[i]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 3, sizeof(cl_mem), &m_qs[i]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 4, sizeof(cl_mem), &m_ainv[i]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 5, sizeof(cl_mem), &m_rho_i[i]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 6, sizeof(cl_mem), &m_speed[i]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 7, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 8, sizeof(int), &c);
      ecode |= clSetKernelArg(k_txinvr[i][c], 9, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 10, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_txinvr[i][c], 11, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 12, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_txinvr[i][c], 13, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_txinvr[i][c], 14, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for txinvr");

      if (TXINVR_DIM == 3) {
        txinvr_dim[i][c][0] = range_i - cell_start[i][c][0];
        txinvr_dim[i][c][1] = range_j - cell_start[i][c][1];
        txinvr_dim[i][c][2] = range_k - cell_start[i][c][2];
        compute_ws_dim_3(txinvr_dim[i][c],
                         txinvr_lw[i][c],
                         txinvr_gw[i][c]);
      } else if (TXINVR_DIM == 2) {
        txinvr_dim[i][c][0] = range_j - cell_start[i][c][1];
        txinvr_dim[i][c][1] = range_k - cell_start[i][c][2];
        compute_ws_dim_2(txinvr_dim[i][c],
                         txinvr_lw[i][c],
                         txinvr_gw[i][c]);
      } else {
        txinvr_dim[i][c][0] = range_k - cell_start[i][c][2];
        compute_ws_dim_1(txinvr_dim[i][c],
                         txinvr_lw[i][c],
                         txinvr_gw[i][c]);
      }
    }
  }

  // lhsx

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_j = cell_size[i][c][1] - cell_end[i][c][1];

      ecode  = clSetKernelArg(k_lhsx[i][c], 0, sizeof(cl_mem), &m_us[i]);
      ecode |= clSetKernelArg(k_lhsx[i][c], 1, sizeof(cl_mem), &m_rho_i[i]);
      ecode |= clSetKernelArg(k_lhsx[i][c], 2, sizeof(cl_mem), &m_speed[i]);
      ecode |= clSetKernelArg(k_lhsx[i][c], 3, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_lhsx[i][c], 4, sizeof(int), &c);
      ecode |= clSetKernelArg(k_lhsx[i][c], 5, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_lhsx[i][c], 6, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_lhsx[i][c], 7, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_lhsx[i][c], 8, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_lhsx[i][c], 9, sizeof(int), &cell_size[i][c][0]);
      ecode |= clSetKernelArg(k_lhsx[i][c], 10, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_lhsx[i][c], 11, sizeof(int), &cell_end[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for lhsx");

      lhsx_dim[i][c][0] = range_j - cell_start[i][c][1];
      lhsx_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(lhsx_dim[i][c],
                       lhsx_lw[i][c],
                       lhsx_gw[i][c]);
    }
  }

  // ninvr

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_j = cell_size[i][c][1] - cell_end[i][c][1];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_ninvr[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_ninvr[i][c], 1, sizeof(int), &c);
      ecode |= clSetKernelArg(k_ninvr[i][c], 2, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_ninvr[i][c], 3, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_ninvr[i][c], 4, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_ninvr[i][c], 5, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_ninvr[i][c], 6, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_ninvr[i][c], 7, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for ninvr");

      if (NINVR_DIM == 3) {
        ninvr_dim[i][c][0] = range_i - cell_start[i][c][0];
        ninvr_dim[i][c][1] = range_j - cell_start[i][c][1];
        ninvr_dim[i][c][2] = range_k - cell_start[i][c][2];
        compute_ws_dim_3(ninvr_dim[i][c],
                         ninvr_lw[i][c],
                         ninvr_gw[i][c]);
       } else if (NINVR_DIM == 2) {
        ninvr_dim[i][c][0] = range_j - cell_start[i][c][1];
        ninvr_dim[i][c][1] = range_k - cell_start[i][c][2];
        compute_ws_dim_2(ninvr_dim[i][c],
                         ninvr_lw[i][c],
                         ninvr_gw[i][c]);
       } else {
        ninvr_dim[i][c][0] = range_k - cell_start[i][c][2];
        compute_ws_dim_1(ninvr_dim[i][c],
                         ninvr_lw[i][c],
                         ninvr_gw[i][c]);
       }
    }
  }

  // x_solve

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_j = cell_size[i][c][1] - cell_end[i][c][1];

      // x_solve1

      ecode  = clSetKernelArg(k_x_solve1[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_x_solve1[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_x_solve1[i][c], 2, sizeof(cl_mem), &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_x_solve1[i][c], 3, sizeof(int), &c);
      ecode |= clSetKernelArg(k_x_solve1[i][c], 4, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_x_solve1[i][c], 5, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_x_solve1[i][c], 6, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_x_solve1[i][c], 7, sizeof(int), &range_j);
      clu_CheckError(ecode, "clSetKernelArg() for x_solve1");

      x_solve1_dim[i][c][0] = range_j - cell_start[i][c][1];
      x_solve1_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(x_solve1_dim[i][c],
                       x_solve1_lw[i][c],
                       x_solve1_gw[i][c]);

      // x_solve2

      ecode  = clSetKernelArg(k_x_solve2[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_x_solve2[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_x_solve2[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_x_solve2[i][c], 3, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_x_solve2[i][c], 4, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_x_solve2[i][c], 5, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_x_solve2[i][c], 6, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_x_solve2[i][c], 7, sizeof(int), &cell_size[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for x_solve2");

      x_solve2_dim[i][c][0] = range_j - cell_start[i][c][1];
      x_solve2_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(x_solve2_dim[i][c],
                       x_solve2_lw[i][c],
                       x_solve2_gw[i][c]);

      // x_solve3

      ecode  = clSetKernelArg(k_x_solve3[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_x_solve3[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_x_solve3[i][c], 2, sizeof(cl_mem), &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_x_solve3[i][c], 3, sizeof(int), &c);
      ecode |= clSetKernelArg(k_x_solve3[i][c], 4, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_x_solve3[i][c], 5, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_x_solve3[i][c], 6, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_x_solve3[i][c], 7, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_x_solve3[i][c], 8, sizeof(int), &cell_size[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for x_solve3");

      x_solve3_dim[i][c][0] = range_j - cell_start[i][c][1];
      x_solve3_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(x_solve3_dim[i][c],
                       x_solve3_lw[i][c],
                       x_solve3_gw[i][c]);

      // x_solve4

      ecode  = clSetKernelArg(k_x_solve4[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_x_solve4[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_x_solve4[i][c], 2, sizeof(cl_mem), &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_x_solve4[i][c], 3, sizeof(int), &c);
      ecode |= clSetKernelArg(k_x_solve4[i][c], 4, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_x_solve4[i][c], 5, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_x_solve4[i][c], 6, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_x_solve4[i][c], 7, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_x_solve4[i][c], 8, sizeof(int), &cell_size[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for x_solve4");

      x_solve4_dim[i][c][0] = range_j - cell_start[i][c][1];
      x_solve4_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(x_solve4_dim[i][c],
                       x_solve4_lw[i][c],
                       x_solve4_gw[i][c]);

      // x_solve5

      ecode  = clSetKernelArg(k_x_solve5[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_x_solve5[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_x_solve5[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_x_solve5[i][c], 3, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_x_solve5[i][c], 4, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_x_solve5[i][c], 5, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_x_solve5[i][c], 6, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_x_solve5[i][c], 7, sizeof(int), &cell_size[i][c][0]);
      clu_CheckError(ecode, "clSetKernelArg() for x_solve5");

      x_solve5_dim[i][c][0] = range_j - cell_start[i][c][1];
      x_solve5_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(x_solve5_dim[i][c],
                       x_solve5_lw[i][c],
                       x_solve5_gw[i][c]);

      // x_solve6

      ecode  = clSetKernelArg(k_x_solve6[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_x_solve6[i][c], 1, sizeof(cl_mem), &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_x_solve6[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_x_solve6[i][c], 3, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_x_solve6[i][c], 4, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_x_solve6[i][c], 5, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_x_solve6[i][c], 6, sizeof(int), &range_j);
      clu_CheckError(ecode, "clSetKernelArg() for x_solve6");

      if (X_SOLVE6_DIM == 2) {
        x_solve6_dim[i][c][0] = range_j - cell_start[i][c][1];
        x_solve6_dim[i][c][1] = range_k - cell_start[i][c][2];
        compute_ws_dim_2(x_solve6_dim[i][c],
                         x_solve6_lw[i][c],
                         x_solve6_gw[i][c]);
      } else {
        x_solve6_dim[i][c][0] = range_k - cell_start[i][c][2];
        compute_ws_dim_1(x_solve6_dim[i][c],
                         x_solve6_lw[i][c],
                         x_solve6_gw[i][c]);
      }
    }
  }


  // lhsy

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_lhsy[i][c], 0, sizeof(cl_mem), &m_vs[i]);
      ecode |= clSetKernelArg(k_lhsy[i][c], 1, sizeof(cl_mem), &m_rho_i[i]);
      ecode |= clSetKernelArg(k_lhsy[i][c], 2, sizeof(cl_mem), &m_speed[i]);
      ecode |= clSetKernelArg(k_lhsy[i][c], 3, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_lhsy[i][c], 4, sizeof(int), &c);
      ecode |= clSetKernelArg(k_lhsy[i][c], 5, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_lhsy[i][c], 6, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_lhsy[i][c], 7, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_lhsy[i][c], 8, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_lhsy[i][c], 9, sizeof(int), &cell_size[i][c][1]);
      ecode |= clSetKernelArg(k_lhsy[i][c], 10, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_lhsy[i][c], 11, sizeof(int), &cell_end[i][c][1]);
      clu_CheckError(ecode, "clSetKernelArg() for lhsy");

      lhsy_dim[i][c][0] = range_i - cell_start[i][c][0];
      lhsy_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(lhsy_dim[i][c],
                       lhsy_lw[i][c],
                       lhsy_gw[i][c]);
    }
  }

  // pinvr

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_j = cell_size[i][c][1] - cell_end[i][c][1];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_pinvr[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_pinvr[i][c], 1, sizeof(int), &c);
      ecode |= clSetKernelArg(k_pinvr[i][c], 2, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_pinvr[i][c], 3, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_pinvr[i][c], 4, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_pinvr[i][c], 5, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_pinvr[i][c], 6, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_pinvr[i][c], 7, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for pinvr");

      if (PINVR_DIM == 3) {
        pinvr_dim[i][c][0] = range_i - cell_start[i][c][0];
        pinvr_dim[i][c][1] = range_j - cell_start[i][c][1];
        pinvr_dim[i][c][2] = range_k - cell_start[i][c][2];
        compute_ws_dim_3(pinvr_dim[i][c],
                         pinvr_lw[i][c],
                         pinvr_gw[i][c]);
       } else if (PINVR_DIM == 2) {
        pinvr_dim[i][c][0] = range_j - cell_start[i][c][1];
        pinvr_dim[i][c][1] = range_k - cell_start[i][c][2];
        compute_ws_dim_2(pinvr_dim[i][c],
                         pinvr_lw[i][c],
                         pinvr_gw[i][c]);
       } else {
        pinvr_dim[i][c][0] = range_k - cell_start[i][c][2];
        compute_ws_dim_1(pinvr_dim[i][c],
                         pinvr_lw[i][c],
                         pinvr_gw[i][c]);
       }
    }
  }

  // y_solve

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      // y_solve1

      ecode  = clSetKernelArg(k_y_solve1[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_y_solve1[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_y_solve1[i][c], 2, sizeof(cl_mem), &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_y_solve1[i][c], 3, sizeof(int), &c);
      ecode |= clSetKernelArg(k_y_solve1[i][c], 4, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_y_solve1[i][c], 5, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_y_solve1[i][c], 6, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_y_solve1[i][c], 7, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for y_solve1");

      y_solve1_dim[i][c][0] = range_i - cell_start[i][c][0];
      y_solve1_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(y_solve1_dim[i][c],
                       y_solve1_lw[i][c],
                       y_solve1_gw[i][c]);

      // y_solve2

      ecode  = clSetKernelArg(k_y_solve2[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_y_solve2[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_y_solve2[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_y_solve2[i][c], 3, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_y_solve2[i][c], 4, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_y_solve2[i][c], 5, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_y_solve2[i][c], 6, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_y_solve2[i][c], 7, sizeof(int), &cell_size[i][c][1]);
      clu_CheckError(ecode, "clSetKernelArg() for y_solve2");

      y_solve2_dim[i][c][0] = range_i - cell_start[i][c][0];
      y_solve2_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(y_solve2_dim[i][c],
                       y_solve2_lw[i][c],
                       y_solve2_gw[i][c]);

      // y_solve3

      ecode  = clSetKernelArg(k_y_solve3[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_y_solve3[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_y_solve3[i][c], 2, sizeof(cl_mem), &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_y_solve3[i][c], 3, sizeof(int), &c);
      ecode |= clSetKernelArg(k_y_solve3[i][c], 4, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_y_solve3[i][c], 5, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_y_solve3[i][c], 6, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_y_solve3[i][c], 7, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_y_solve3[i][c], 8, sizeof(int), &cell_size[i][c][1]);
      clu_CheckError(ecode, "clSetKernelArg() for y_solve3");

      y_solve3_dim[i][c][0] = range_i - cell_start[i][c][0];
      y_solve3_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(y_solve3_dim[i][c],
                       y_solve3_lw[i][c],
                       y_solve3_gw[i][c]);

      // y_solve4

      ecode  = clSetKernelArg(k_y_solve4[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_y_solve4[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_y_solve4[i][c], 2, sizeof(cl_mem), &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_y_solve4[i][c], 3, sizeof(int), &c);
      ecode |= clSetKernelArg(k_y_solve4[i][c], 4, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_y_solve4[i][c], 5, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_y_solve4[i][c], 6, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_y_solve4[i][c], 7, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_y_solve4[i][c], 8, sizeof(int), &cell_size[i][c][1]);
      clu_CheckError(ecode, "clSetKernelArg() for y_solve4");

      y_solve4_dim[i][c][0] = range_i - cell_start[i][c][0];
      y_solve4_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(y_solve4_dim[i][c],
                       y_solve4_lw[i][c],
                       y_solve4_gw[i][c]);

      // y_solve5

      ecode  = clSetKernelArg(k_y_solve5[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_y_solve5[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_y_solve5[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_y_solve5[i][c], 3, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_y_solve5[i][c], 4, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_y_solve5[i][c], 5, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_y_solve5[i][c], 6, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_y_solve5[i][c], 7, sizeof(int), &cell_size[i][c][1]);
      clu_CheckError(ecode, "clSetKernelArg() for y_solve5");

      y_solve5_dim[i][c][0] = range_i - cell_start[i][c][0];
      y_solve5_dim[i][c][1] = range_k - cell_start[i][c][2];
      compute_ws_dim_2(y_solve5_dim[i][c],
                       y_solve5_lw[i][c],
                       y_solve5_gw[i][c]);

      // y_solve6

      ecode  = clSetKernelArg(k_y_solve6[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_y_solve6[i][c], 1, sizeof(cl_mem), &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_y_solve6[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_y_solve6[i][c], 3, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_y_solve6[i][c], 4, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_y_solve6[i][c], 5, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_y_solve6[i][c], 6, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for y_solve6");

      if (Y_SOLVE6_DIM == 2) {
        y_solve6_dim[i][c][0] = range_i - cell_start[i][c][0];
        y_solve6_dim[i][c][1] = range_k - cell_start[i][c][2];
        compute_ws_dim_2(y_solve6_dim[i][c],
                         y_solve6_lw[i][c],
                         y_solve6_gw[i][c]);
      } else {
        y_solve6_dim[i][c][0] = range_k - cell_start[i][c][2];
        compute_ws_dim_1(y_solve6_dim[i][c],
                         y_solve6_lw[i][c],
                         y_solve6_gw[i][c]);
      }
    }
  }


  // lhsz

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_j = cell_size[i][c][1] - cell_end[i][c][1];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_lhsz[i][c], 0, sizeof(cl_mem), &m_ws[i]);
      ecode |= clSetKernelArg(k_lhsz[i][c], 1, sizeof(cl_mem), &m_rho_i[i]);
      ecode |= clSetKernelArg(k_lhsz[i][c], 2, sizeof(cl_mem), &m_speed[i]);
      ecode |= clSetKernelArg(k_lhsz[i][c], 3, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_lhsz[i][c], 4, sizeof(int), &c);
      ecode |= clSetKernelArg(k_lhsz[i][c], 5, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_lhsz[i][c], 6, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_lhsz[i][c], 7, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_lhsz[i][c], 8, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_lhsz[i][c], 9, sizeof(int), &cell_size[i][c][2]);
      ecode |= clSetKernelArg(k_lhsz[i][c], 10, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_lhsz[i][c], 11, sizeof(int), &cell_end[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for lhsz");

      lhsz_dim[i][c][0] = range_i - cell_start[i][c][0];
      lhsz_dim[i][c][1] = range_j - cell_start[i][c][1];
      compute_ws_dim_2(lhsz_dim[i][c],
                       lhsz_lw[i][c],
                       lhsz_gw[i][c]);
    }
  }

  // tzetar

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_j = cell_size[i][c][1] - cell_end[i][c][1];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_tzetar[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 1, sizeof(cl_mem), &m_us[i]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 2, sizeof(cl_mem), &m_vs[i]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 3, sizeof(cl_mem), &m_ws[i]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 4, sizeof(cl_mem), &m_qs[i]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 5, sizeof(cl_mem), &m_ainv[i]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 6, sizeof(cl_mem), &m_speed[i]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 7, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 8, sizeof(int), &c);
      ecode |= clSetKernelArg(k_tzetar[i][c], 9, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 10, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_tzetar[i][c], 11, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 12, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_tzetar[i][c], 13, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_tzetar[i][c], 14, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for tzetar");

      if (TZETAR_DIM == 3) {
        tzetar_dim[i][c][0] = range_i - cell_start[i][c][0];
        tzetar_dim[i][c][1] = range_j - cell_start[i][c][1];
        tzetar_dim[i][c][2] = range_k - cell_start[i][c][2];
        compute_ws_dim_3(tzetar_dim[i][c],
                         tzetar_lw[i][c],
                         tzetar_gw[i][c]);
      } else if (TZETAR_DIM == 2) {
        tzetar_dim[i][c][0] = range_j - cell_start[i][c][1];
        tzetar_dim[i][c][1] = range_k - cell_start[i][c][2];
        compute_ws_dim_2(tzetar_dim[i][c],
                         tzetar_lw[i][c],
                         tzetar_gw[i][c]);
      } else {
        tzetar_dim[i][c][0] = range_k - cell_start[i][c][2];
        compute_ws_dim_1(tzetar_dim[i][c],
                         tzetar_lw[i][c],
                         tzetar_gw[i][c]);
      }
    }
  }

  // z_solve

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_j = cell_size[i][c][1] - cell_end[i][c][1];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      // z_solve1

      ecode  = clSetKernelArg(k_z_solve1[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_z_solve1[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_z_solve1[i][c], 2, sizeof(cl_mem), &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_z_solve1[i][c], 3, sizeof(int), &c);
      ecode |= clSetKernelArg(k_z_solve1[i][c], 4, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_z_solve1[i][c], 5, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_z_solve1[i][c], 6, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_z_solve1[i][c], 7, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for z_solve1");

      z_solve1_dim[i][c][0] = range_i - cell_start[i][c][0];
      z_solve1_dim[i][c][1] = range_j - cell_start[i][c][1];
      compute_ws_dim_2(z_solve1_dim[i][c],
                       z_solve1_lw[i][c],
                       z_solve1_gw[i][c]);

      // z_solve2

      ecode  = clSetKernelArg(k_z_solve2[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_z_solve2[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_z_solve2[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_z_solve2[i][c], 3, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_z_solve2[i][c], 4, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_z_solve2[i][c], 5, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_z_solve2[i][c], 6, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_z_solve2[i][c], 7, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for z_solve2");

      z_solve2_dim[i][c][0] = range_i - cell_start[i][c][0];
      z_solve2_dim[i][c][1] = range_j - cell_start[i][c][1];
      compute_ws_dim_2(z_solve2_dim[i][c],
                       z_solve2_lw[i][c],
                       z_solve2_gw[i][c]);

      // z_solve3

      ecode  = clSetKernelArg(k_z_solve3[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_z_solve3[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_z_solve3[i][c], 2, sizeof(cl_mem), &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_z_solve3[i][c], 3, sizeof(int), &c);
      ecode |= clSetKernelArg(k_z_solve3[i][c], 4, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_z_solve3[i][c], 5, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_z_solve3[i][c], 6, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_z_solve3[i][c], 7, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_z_solve3[i][c], 8, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for z_solve3");

      z_solve3_dim[i][c][0] = range_i - cell_start[i][c][0];
      z_solve3_dim[i][c][1] = range_j - cell_start[i][c][1];
      compute_ws_dim_2(z_solve3_dim[i][c],
                       z_solve3_lw[i][c],
                       z_solve3_gw[i][c]);

      // z_solve4

      ecode  = clSetKernelArg(k_z_solve4[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_z_solve4[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_z_solve4[i][c], 2, sizeof(cl_mem), &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_z_solve4[i][c], 3, sizeof(int), &c);
      ecode |= clSetKernelArg(k_z_solve4[i][c], 4, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_z_solve4[i][c], 5, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_z_solve4[i][c], 6, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_z_solve4[i][c], 7, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_z_solve4[i][c], 8, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for z_solve4");

      z_solve4_dim[i][c][0] = range_i - cell_start[i][c][0];
      z_solve4_dim[i][c][1] = range_j - cell_start[i][c][1];
      compute_ws_dim_2(z_solve4_dim[i][c],
                       z_solve4_lw[i][c],
                       z_solve4_gw[i][c]);

      // z_solve5

      ecode  = clSetKernelArg(k_z_solve5[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_z_solve5[i][c], 1, sizeof(cl_mem), &m_lhs[i]);
      ecode |= clSetKernelArg(k_z_solve5[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_z_solve5[i][c], 3, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_z_solve5[i][c], 4, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_z_solve5[i][c], 5, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_z_solve5[i][c], 6, sizeof(int), &range_i);
      ecode |= clSetKernelArg(k_z_solve5[i][c], 7, sizeof(int), &cell_size[i][c][2]);
      clu_CheckError(ecode, "clSetKernelArg() for z_solve5");

      z_solve5_dim[i][c][0] = range_i - cell_start[i][c][0];
      z_solve5_dim[i][c][1] = range_j - cell_start[i][c][1];
      compute_ws_dim_2(z_solve5_dim[i][c],
                       z_solve5_lw[i][c],
                       z_solve5_gw[i][c]);

      // z_solve6

      ecode  = clSetKernelArg(k_z_solve6[i][c], 0, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_z_solve6[i][c], 1, sizeof(cl_mem), &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_z_solve6[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_z_solve6[i][c], 3, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_z_solve6[i][c], 4, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_z_solve6[i][c], 5, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_z_solve6[i][c], 6, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for z_solve6");

      if (Z_SOLVE6_DIM == 2) {
        z_solve6_dim[i][c][0] = range_i - cell_start[i][c][0];
        z_solve6_dim[i][c][1] = range_j - cell_start[i][c][1];
        compute_ws_dim_2(z_solve6_dim[i][c],
                         z_solve6_lw[i][c],
                         z_solve6_gw[i][c]);
      } else {
        z_solve6_dim[i][c][0] = range_j - cell_start[i][c][1];
        compute_ws_dim_1(z_solve6_dim[i][c],
                         z_solve6_lw[i][c],
                         z_solve6_gw[i][c]);
      }
    }
  }


  // add

  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < ncells; c++) {
      range_k = cell_size[i][c][2] - cell_end[i][c][2];
      range_j = cell_size[i][c][1] - cell_end[i][c][1];
      range_i = cell_size[i][c][0] - cell_end[i][c][0];

      ecode  = clSetKernelArg(k_add[i][c], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_add[i][c], 1, sizeof(cl_mem), &m_rhs[i]);
      ecode |= clSetKernelArg(k_add[i][c], 2, sizeof(int), &c);
      ecode |= clSetKernelArg(k_add[i][c], 3, sizeof(int), &cell_start[i][c][2]);
      ecode |= clSetKernelArg(k_add[i][c], 4, sizeof(int), &range_k);
      ecode |= clSetKernelArg(k_add[i][c], 5, sizeof(int), &cell_start[i][c][1]);
      ecode |= clSetKernelArg(k_add[i][c], 6, sizeof(int), &range_j);
      ecode |= clSetKernelArg(k_add[i][c], 7, sizeof(int), &cell_start[i][c][0]);
      ecode |= clSetKernelArg(k_add[i][c], 8, sizeof(int), &range_i);
      clu_CheckError(ecode, "clSetKernelArg() for add");

      if (ADD_DIM == 3) {
        add_dim[i][c][0] = range_i - cell_start[i][c][0];
        add_dim[i][c][1] = range_j - cell_start[i][c][1];
        add_dim[i][c][1] = range_k - cell_start[i][c][2];
        compute_ws_dim_3(add_dim[i][c],
                         add_lw[i][c],
                         add_gw[i][c]);
      } else if (ADD_DIM == 2) {
        add_dim[i][c][0] = range_j - cell_start[i][c][1];
        add_dim[i][c][1] = range_k - cell_start[i][c][2];
        compute_ws_dim_2(add_dim[i][c],
                         add_lw[i][c],
                         add_gw[i][c]);
      } else {
        add_dim[i][c][0] = range_k - cell_start[i][c][2];
        compute_ws_dim_1(add_dim[i][c],
                         add_lw[i][c],
                         add_gw[i][c]);
      }
    }
  }
}
