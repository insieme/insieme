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

#include <stdlib.h>
#include <math.h>
#include "header.h"

//---------------------------------------------------------------------
// compute the right hand side based on exact solution
//---------------------------------------------------------------------
void exact_rhs()
{
  int i;
  size_t d0_size, d1_size, d2_size;
  size_t local_ws[3], global_ws[3], temp;
  size_t wg_size, buf_size1, buf_size2;

  cl_kernel *k_exact_rhs1;
  cl_kernel *k_exact_rhs2;
  cl_kernel *k_exact_rhs3;
  cl_kernel *k_exact_rhs4;
  cl_kernel *k_exact_rhs5;
  cl_mem *m_ue;
  cl_mem *m_buf;
  cl_mem *m_cuf;
  cl_mem *m_q;
  cl_int ecode;

  k_exact_rhs1 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_exact_rhs2 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_exact_rhs3 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_exact_rhs4 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_exact_rhs5 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);

  m_ue  = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_buf = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_cuf = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_q   = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);

  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][1];
    d1_size = max_cell_size[i][2];
    d2_size = ncells;

    wg_size   = d0_size * d1_size * d2_size;
    buf_size1 = sizeof(double)*(MAX_CELL_DIM+4)*5 * wg_size;
    buf_size2 = sizeof(double)*(MAX_CELL_DIM+4) * wg_size;

    m_ue[i] = clCreateBuffer(context,
                          CL_MEM_READ_WRITE,
                          buf_size1,
                          NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_ue");
    
    m_buf[i] = clCreateBuffer(context,
                          CL_MEM_READ_WRITE,
                          buf_size1,
                          NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_buf");

    m_cuf[i] = clCreateBuffer(context,
                          CL_MEM_READ_WRITE,
                          buf_size2,
                          NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_cuf");
    
    m_q[i] = clCreateBuffer(context,
                          CL_MEM_READ_WRITE,
                          buf_size2,
                          NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_q");
  }

  //-----------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][1];
    d1_size = max_cell_size[i][2];
    d2_size = ncells;

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    temp = temp / local_ws[1];
    local_ws[2] = d2_size < temp ? d2_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
    global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

    k_exact_rhs1[i] = clCreateKernel(p_exact_rhs[i], "exact_rhs1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    ecode  = clSetKernelArg(k_exact_rhs1[i], 0, sizeof(cl_mem),
                                                &m_forcing[i]);
    ecode |= clSetKernelArg(k_exact_rhs1[i], 1, sizeof(cl_mem),
                                                &m_cell_size[i]);
    ecode |= clSetKernelArg(k_exact_rhs1[i], 2, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_exact_rhs1[i],
                                   3, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //-----------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][1];
    d1_size = max_cell_size[i][2];
    d2_size = ncells;

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    temp = temp / local_ws[1];
    local_ws[2] = d2_size < temp ? d2_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
    global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

    k_exact_rhs2[i] = clCreateKernel(p_exact_rhs[i], "exact_rhs2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    ecode  = clSetKernelArg(k_exact_rhs2[i], 0, sizeof(cl_mem),
                                                &m_forcing[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 1, sizeof(cl_mem), &m_ue[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 2, sizeof(cl_mem), &m_buf[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 3, sizeof(cl_mem), &m_cuf[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 4, sizeof(cl_mem), &m_q[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 5, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 6, sizeof(cl_mem),
                                                &m_cell_size[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 7, sizeof(cl_mem),
                                                &m_cell_low[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 8, sizeof(cl_mem), &m_start[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 9, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_exact_rhs2[i], 10, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_exact_rhs2[i],
                                   3, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //-----------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][0];
    d1_size = max_cell_size[i][2];
    d2_size = ncells;

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    temp = temp / local_ws[1];
    local_ws[2] = d2_size < temp ? d2_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
    global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

    k_exact_rhs3[i] = clCreateKernel(p_exact_rhs[i], "exact_rhs3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    ecode  = clSetKernelArg(k_exact_rhs3[i], 0, sizeof(cl_mem),
                                                &m_forcing[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 1, sizeof(cl_mem), &m_ue[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 2, sizeof(cl_mem), &m_buf[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 3, sizeof(cl_mem), &m_cuf[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 4, sizeof(cl_mem), &m_q[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 5, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 6, sizeof(cl_mem),
                                                &m_cell_size[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 7, sizeof(cl_mem),
                                                &m_cell_low[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 8, sizeof(cl_mem), &m_start[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 9, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_exact_rhs3[i], 10, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_exact_rhs3[i],
                                   3, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //-----------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][0];
    d1_size = max_cell_size[i][1];
    d2_size = ncells;

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    temp = temp / local_ws[1];
    local_ws[2] = d2_size < temp ? d2_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
    global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

    k_exact_rhs4[i] = clCreateKernel(p_exact_rhs[i], "exact_rhs4", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    ecode  = clSetKernelArg(k_exact_rhs4[i], 0, sizeof(cl_mem),
                                                &m_forcing[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 1, sizeof(cl_mem), &m_ue[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 2, sizeof(cl_mem), &m_buf[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 3, sizeof(cl_mem), &m_cuf[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 4, sizeof(cl_mem), &m_q[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 5, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 6, sizeof(cl_mem),
                                                &m_cell_size[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 7, sizeof(cl_mem),
                                                &m_cell_low[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 8, sizeof(cl_mem), &m_start[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 9, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_exact_rhs4[i], 10, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_exact_rhs4[i],
                                   3, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //-----------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][1];
    d1_size = max_cell_size[i][2];
    d2_size = ncells;

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    temp = temp / local_ws[1];
    local_ws[2] = d2_size < temp ? d2_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
    global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

    k_exact_rhs5[i] = clCreateKernel(p_exact_rhs[i], "exact_rhs5", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    ecode  = clSetKernelArg(k_exact_rhs5[i], 0, sizeof(cl_mem),
                                                &m_forcing[i]);
    ecode |= clSetKernelArg(k_exact_rhs5[i], 1, sizeof(cl_mem),
                                                &m_cell_size[i]);
    ecode |= clSetKernelArg(k_exact_rhs5[i], 2, sizeof(cl_mem), &m_start[i]);
    ecode |= clSetKernelArg(k_exact_rhs5[i], 3, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_exact_rhs5[i], 4, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_exact_rhs5[i],
                                   3, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  for (i = 0; i < num_devices; i++) {
    clReleaseMemObject(m_ue[i]);
    clReleaseMemObject(m_buf[i]);
    clReleaseMemObject(m_cuf[i]);
    clReleaseMemObject(m_q[i]);

    clReleaseKernel(k_exact_rhs1[i]);
    clReleaseKernel(k_exact_rhs2[i]);
    clReleaseKernel(k_exact_rhs3[i]);
    clReleaseKernel(k_exact_rhs4[i]);
    clReleaseKernel(k_exact_rhs5[i]);
  }
  free(m_ue);
  free(m_buf);
  free(m_cuf);
  free(m_q);
  free(k_exact_rhs1);
  free(k_exact_rhs2);
  free(k_exact_rhs3);
  free(k_exact_rhs4);
  free(k_exact_rhs5);
}
