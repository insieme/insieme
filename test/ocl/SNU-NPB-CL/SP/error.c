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

//---------------------------------------------------------------------
// this function computes the norm of the difference between the
// computed solution and the exact solution
//---------------------------------------------------------------------
void error_norm(double rms[5])
{
  int c, i, m, d;
  int k, j, kk, jj;
  double rms_work[5];
  size_t one = 1;
  cl_mem m_rms[MAX_DEVICE_NUM];
  cl_int ecode = 0;

  for (m = 0; m < 5; m++) {
    rms[m] = 0.0;
    rms_work[m] = 0.0;
  }

  for (i = 0; i < num_devices; i++) {
    m_rms[i] = clCreateBuffer(context, CL_MEM_READ_WRITE,
                              sizeof(double)*5, NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_rms");

    ecode = clEnqueueWriteBuffer(cmd_queue[i * 2], m_rms[i], CL_TRUE,
                                 0, sizeof(double)*5, rms_work,
                                 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_rms");
  }

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      kk = 2;
      for (k = cell_low[i][c][2]; k <= cell_high[i][c][2]; k++) {
        jj = 2;
        for (j = cell_low[i][c][1]; j <= cell_high[i][c][1]; j++) {
          ecode  = clSetKernelArg(k_error_norm[i], 0, sizeof(cl_mem), &m_u[i]);
          ecode |= clSetKernelArg(k_error_norm[i], 1, sizeof(cl_mem), &m_rms[i]);
          ecode |= clSetKernelArg(k_error_norm[i], 2, sizeof(cl_mem), &m_ce[i]);
          ecode |= clSetKernelArg(k_error_norm[i], 3, sizeof(int), &c);
          ecode |= clSetKernelArg(k_error_norm[i], 4, sizeof(int), &k);
          ecode |= clSetKernelArg(k_error_norm[i], 5, sizeof(int), &kk);
          ecode |= clSetKernelArg(k_error_norm[i], 6, sizeof(int), &j);
          ecode |= clSetKernelArg(k_error_norm[i], 7, sizeof(int), &jj);
          ecode |= clSetKernelArg(k_error_norm[i], 8, sizeof(int), &cell_low[i][c][0]);
          ecode |= clSetKernelArg(k_error_norm[i], 9, sizeof(int), &cell_high[i][c][0]);
          clu_CheckError(ecode, "clSetKernelArg() for error_norm");

          ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                         k_error_norm[i],
                                         1, NULL, &one, &one,
                                         0, NULL, NULL);
          clu_CheckError(ecode, "clEnqueueNDRangeKernel() for error_norm");
          jj = jj + 1;

          CHECK_FINISH(i * 2);
        }
        kk = kk + 1;
      }
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clEnqueueReadBuffer(cmd_queue[i * 2], m_rms[i], CL_TRUE,
                                0, sizeof(double)*5, rms_work,
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueReadBuffer() for m_rms");

    for (m = 0; m < 5; m++)
      rms[m] += rms_work[m];

    clReleaseMemObject(m_rms[i]);
  }

  for (m = 0; m < 5; m++) {
    for (d = 0; d < 3; d++) {
      rms[m] = rms[m] / (double)(grid_points[d]-2);
    }
    rms[m] = sqrt(rms[m]);
  }
}


void rhs_norm(double rms[5])
{
  int c, i, m, d;
  int k, j;
  double rms_work[5];
  int range_i;
  size_t one = 1;
  cl_mem m_rms[MAX_DEVICE_NUM];
  cl_int ecode = 0;

  for (m = 0; m < 5; m++) {
    rms[m] = 0.0;
    rms_work[m] = 0.0;
  }

  for (i = 0; i < num_devices; i++) {
    m_rms[i] = clCreateBuffer(context, CL_MEM_READ_WRITE,
                              sizeof(double)*5, NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_rms");

    ecode = clEnqueueWriteBuffer(cmd_queue[i * 2], m_rms[i], CL_TRUE,
                                 0, sizeof(double)*5, rms_work,
                                 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_rms");
  }

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      range_i = cell_size[i][c][0] - cell_end[i][c][0];
      for (k = cell_start[i][c][2]; k < cell_size[i][c][2] - cell_end[i][c][2]; k++) {
        for (j = cell_start[i][c][1]; j < cell_size[i][c][1] - cell_end[i][c][1]; j++) {

          ecode  = clSetKernelArg(k_rhs_norm[i], 0, sizeof(cl_mem), &m_rhs[i]);
          ecode |= clSetKernelArg(k_rhs_norm[i], 1, sizeof(cl_mem), &m_rms[i]);
          ecode |= clSetKernelArg(k_rhs_norm[i], 2, sizeof(int), &c);
          ecode |= clSetKernelArg(k_rhs_norm[i], 3, sizeof(int), &k);
          ecode |= clSetKernelArg(k_rhs_norm[i], 4, sizeof(int), &j);
          ecode |= clSetKernelArg(k_rhs_norm[i], 5, sizeof(int), &cell_start[i][c][0]);
          ecode |= clSetKernelArg(k_rhs_norm[i], 6, sizeof(int), &range_i);
          clu_CheckError(ecode, "clSetKernelArg() for rhs_norm");

          ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                         k_rhs_norm[i],
                                         1, NULL, &one, &one,
                                         0, NULL, NULL);
          clu_CheckError(ecode, "clEnqueueNDRangeKernel() for rhs_norm");
          CHECK_FINISH(i * 2);
        }
      }
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clEnqueueReadBuffer(cmd_queue[i * 2], m_rms[i], CL_TRUE,
                                0, sizeof(double)*5, rms_work,
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueReadBuffer() for m_rms");

    for (m = 0; m < 5; m++)
      rms[m] += rms_work[m];

    clReleaseMemObject(m_rms[i]);
  }

  for (m = 0; m < 5; m++) {
    for (d = 0; d < 3; d++) {
      rms[m] = rms[m] / (double)(grid_points[d]-2);
    }
    rms[m] = sqrt(rms[m]);
  }
}
