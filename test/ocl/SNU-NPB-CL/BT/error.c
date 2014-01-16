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

#include <math.h>
#include <stdio.h>
#include "header.h"

//---------------------------------------------------------------------
// this function computes the norm of the difference between the
// computed solution and the exact solution
//---------------------------------------------------------------------
void error_norm(double rms[5])
{
  int i, m, d;

  cl_kernel *k_en;
  cl_mem *m_rms;
  double (*g_rms)[5];
  cl_int ecode;

  g_rms = (double (*)[5])malloc(sizeof(double)*5 * num_devices);
  m_rms = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  k_en  = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);

  for (i = 0; i < num_devices; i++) {
    m_rms[i] = clCreateBuffer(context,
                              CL_MEM_READ_WRITE,
                              sizeof(double) * 5, 
                              NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    k_en[i] = clCreateKernel(p_error[i], "error_norm", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    ecode  = clSetKernelArg(k_en[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_en[i], 1, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_en[i], 2, sizeof(cl_mem), &m_rms[i]);
    ecode |= clSetKernelArg(k_en[i], 3, sizeof(cl_mem), &m_cell_low[i]);
    ecode |= clSetKernelArg(k_en[i], 4, sizeof(cl_mem), &m_cell_high[i]);
    ecode |= clSetKernelArg(k_en[i], 5, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");
    
    ecode = clEnqueueTask(cmd_queue[i],
                          k_en[i],
                          0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueTask()");

    clFinish(cmd_queue[i]);

    ecode = clEnqueueReadBuffer(cmd_queue[i],
                                m_rms[i],
                                CL_TRUE,
                                0, sizeof(double)*5,
                                &g_rms[i],
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueReadBuffer()");
  }

  for (m = 0; m < 5; m++) {
    rms[m] = 0.0;
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  // reduction
  for (i = 0; i < num_devices; i++) {
    for (m = 0; m < 5; m++) {
      rms[m] += g_rms[i][m];
    }
  }
  
  for (m = 0; m < 5; m++) {
    for (d = 0; d < 3; d++) {
      rms[m] = rms[m] / (double)(grid_points[d]-2);
    }
    rms[m] = sqrt(rms[m]);
  }

  for (i = 0; i < num_devices; i++) {
    clReleaseMemObject(m_rms[i]);
    clReleaseKernel(k_en[i]);
  }
  free(g_rms);
  free(m_rms);
  free(k_en);
}


void rhs_norm(double rms[5])
{
  int i, m, d;

  cl_kernel *k_rn;
  cl_mem *m_rms;
  double (*g_rms)[5];
  cl_int ecode;

  g_rms = (double (*)[5])malloc(sizeof(double)*5 * num_devices);
  m_rms = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  k_rn  = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);

  for (i = 0; i < num_devices; i++) {
    m_rms[i] = clCreateBuffer(context,
                              CL_MEM_READ_WRITE,
                              sizeof(double) * 5, 
                              NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    k_rn[i] = clCreateKernel(p_error[i], "rhs_norm", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    ecode  = clSetKernelArg(k_rn[i], 0, sizeof(cl_mem), &m_rhs[i]);
    ecode |= clSetKernelArg(k_rn[i], 1, sizeof(cl_mem), &m_rms[i]);
    ecode |= clSetKernelArg(k_rn[i], 2, sizeof(cl_mem), &m_cell_size[i]);
    ecode |= clSetKernelArg(k_rn[i], 3, sizeof(cl_mem), &m_start[i]);
    ecode |= clSetKernelArg(k_rn[i], 4, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_rn[i], 5, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");
    
    ecode = clEnqueueTask(cmd_queue[i],
                          k_rn[i],
                          0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueTask()");

    ecode = clFinish(cmd_queue[i]);

    ecode = clEnqueueReadBuffer(cmd_queue[i],
                                m_rms[i],
                                CL_TRUE,
                                0, sizeof(double)*5,
                                &g_rms[i],
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueReadBuffer()");
  }

  for (m = 0; m < 5; m++) {
    rms[m] = 0.0;
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  // reduction
  for (i = 0; i < num_devices; i++) {
    for (m = 0; m < 5; m++) {
      rms[m] += g_rms[i][m];
      //printf("[%d:%d] %lf, %lf\n", i, m, rms[m], g_rms[i][m]);
    }
  }
  
  for (m = 0; m < 5; m++) {
    for (d = 0; d < 3; d++) {
      rms[m] = rms[m] / (double)(grid_points[d]-2);
    }
    rms[m] = sqrt(rms[m]);
  }

  for (i = 0; i < num_devices; i++) {
    clReleaseMemObject(m_rms[i]);
    clReleaseKernel(k_rn[i]);
  }
  free(g_rms);
  free(m_rms);
  free(k_rn);
}

