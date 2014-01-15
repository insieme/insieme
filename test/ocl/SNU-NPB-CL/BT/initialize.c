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
#include "header.h"

//---------------------------------------------------------------------
// This subroutine initializes the field variable u using 
// tri-linear transfinite interpolation of the boundary values     
//---------------------------------------------------------------------
void initialize()
{
  int i;
  size_t d0_size, d1_size, d2_size;
  size_t local_ws[3], global_ws[3], temp;

  cl_kernel *k_initialize1;
  cl_kernel *k_initialize2;
  cl_kernel *k_initialize3;
  cl_kernel *k_initialize4;
  cl_kernel *k_initialize5;
  cl_kernel *k_initialize6;
  cl_kernel *k_initialize7;
  cl_kernel *k_initialize8;
  cl_int ecode;

  k_initialize1 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_initialize2 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_initialize3 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_initialize4 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_initialize5 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_initialize6 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_initialize7 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_initialize8 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);

  //-----------------------------------------------------------------------
  d0_size = JMAX+2;
  d1_size = KMAX+2;
  d2_size = ncells;

  local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
  temp = max_work_group_size / local_ws[0];
  local_ws[1] = d1_size < temp ? d1_size : temp;
  temp = temp / local_ws[1];
  local_ws[2] = d2_size < temp ? d2_size : temp;

  global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
  global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
  global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

  for (i = 0; i < num_devices; i++) {
    k_initialize1[i] = clCreateKernel(p_initialize[i], "initialize1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    
    ecode  = clSetKernelArg(k_initialize1[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize1[i], 1, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_initialize1[i],
                                   3, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // first store the "interpolated" values everywhere on the grid    
  //---------------------------------------------------------------------
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

    k_initialize2[i] = clCreateKernel(p_initialize[i], "initialize2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    
    ecode  = clSetKernelArg(k_initialize2[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize2[i], 1, sizeof(cl_mem),
                                                 &m_cell_low[i]);
    ecode |= clSetKernelArg(k_initialize2[i], 2, sizeof(cl_mem),
                                                 &m_cell_high[i]);
    ecode |= clSetKernelArg(k_initialize2[i], 3, sizeof(cl_mem),
                                                 &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize2[i], 4, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");
    
    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_initialize2[i],
                                   3, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // now store the exact values on the boundaries        
  //---------------------------------------------------------------------
  //---------------------------------------------------------------------
  // west face                                                  
  //---------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][1];
    d1_size = max_cell_size[i][2];

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);

    k_initialize3[i] = clCreateKernel(p_initialize[i], "initialize3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    
    ecode  = clSetKernelArg(k_initialize3[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize3[i], 1, sizeof(cl_mem),
                                                 &m_cell_low[i]);
    ecode |= clSetKernelArg(k_initialize3[i], 2, sizeof(cl_mem),
                                                 &m_cell_high[i]);
    ecode |= clSetKernelArg(k_initialize3[i], 3, sizeof(cl_mem), &m_slice[i]);
    ecode |= clSetKernelArg(k_initialize3[i], 4, sizeof(cl_mem), &m_ce[i]);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_initialize3[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // east face                                                      
  //---------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][1];
    d1_size = max_cell_size[i][2];

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);

    k_initialize4[i] = clCreateKernel(p_initialize[i], "initialize4", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    
    ecode  = clSetKernelArg(k_initialize4[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize4[i], 1, sizeof(cl_mem),
                                                 &m_cell_low[i]);
    ecode |= clSetKernelArg(k_initialize4[i], 2, sizeof(cl_mem),
                                                 &m_cell_high[i]);
    ecode |= clSetKernelArg(k_initialize4[i], 3, sizeof(cl_mem),
                                                 &m_cell_size[i]);
    ecode |= clSetKernelArg(k_initialize4[i], 4, sizeof(cl_mem), &m_slice[i]);
    ecode  = clSetKernelArg(k_initialize4[i], 5, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize4[i], 6, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_initialize4[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // south face                                                 
  //---------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][0];
    d1_size = max_cell_size[i][2];

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);

    k_initialize5[i] = clCreateKernel(p_initialize[i], "initialize5", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    
    ecode  = clSetKernelArg(k_initialize5[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize5[i], 1, sizeof(cl_mem),
                                                 &m_cell_low[i]);
    ecode |= clSetKernelArg(k_initialize5[i], 2, sizeof(cl_mem),
                                                 &m_cell_high[i]);
    ecode |= clSetKernelArg(k_initialize5[i], 3, sizeof(cl_mem), &m_slice[i]);
    ecode |= clSetKernelArg(k_initialize5[i], 4, sizeof(cl_mem), &m_ce[i]);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_initialize5[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // north face                                    
  //-----------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][0];
    d1_size = max_cell_size[i][2];

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);

    k_initialize6[i] = clCreateKernel(p_initialize[i], "initialize6", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    
    ecode  = clSetKernelArg(k_initialize6[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize6[i], 1, sizeof(cl_mem),
                                                 &m_cell_low[i]);
    ecode |= clSetKernelArg(k_initialize6[i], 2, sizeof(cl_mem),
                                                 &m_cell_high[i]);
    ecode |= clSetKernelArg(k_initialize6[i], 3, sizeof(cl_mem),
                                                 &m_cell_size[i]);
    ecode |= clSetKernelArg(k_initialize6[i], 4, sizeof(cl_mem), &m_slice[i]);
    ecode  = clSetKernelArg(k_initialize6[i], 5, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize6[i], 6, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_initialize6[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // bottom face                                       
  //-----------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][0];
    d1_size = max_cell_size[i][1];

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);

    k_initialize7[i] = clCreateKernel(p_initialize[i], "initialize7", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    
    ecode  = clSetKernelArg(k_initialize7[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize7[i], 1, sizeof(cl_mem),
                                                 &m_cell_low[i]);
    ecode |= clSetKernelArg(k_initialize7[i], 2, sizeof(cl_mem),
                                                 &m_cell_high[i]);
    ecode |= clSetKernelArg(k_initialize7[i], 3, sizeof(cl_mem), &m_slice[i]);
    ecode |= clSetKernelArg(k_initialize7[i], 4, sizeof(cl_mem), &m_ce[i]);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_initialize7[i],
                                   2, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }
  //-----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // top face     
  //-----------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    d0_size = max_cell_size[i][0];
    d1_size = max_cell_size[i][1];

    local_ws[0] = d0_size < work_item_sizes[0] ? d0_size : work_item_sizes[0];
    temp = max_work_group_size / local_ws[0];
    local_ws[1] = d1_size < temp ? d1_size : temp;
    global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
    global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);

    k_initialize8[i] = clCreateKernel(p_initialize[i], "initialize8", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    
    ecode  = clSetKernelArg(k_initialize8[i], 0, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_initialize8[i], 1, sizeof(cl_mem),
                                                 &m_cell_low[i]);
    ecode |= clSetKernelArg(k_initialize8[i], 2, sizeof(cl_mem),
                                                 &m_cell_high[i]);
    ecode |= clSetKernelArg(k_initialize8[i], 3, sizeof(cl_mem),
                                                 &m_cell_size[i]);
    ecode |= clSetKernelArg(k_initialize8[i], 4, sizeof(cl_mem), &m_slice[i]);
    ecode  = clSetKernelArg(k_initialize8[i], 5, sizeof(cl_mem), &m_ce[i]);
    ecode |= clSetKernelArg(k_initialize8[i], 6, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_initialize8[i],
                                   2, NULL,
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
    clReleaseKernel(k_initialize1[i]);
    clReleaseKernel(k_initialize2[i]);
    clReleaseKernel(k_initialize3[i]);
    clReleaseKernel(k_initialize4[i]);
    clReleaseKernel(k_initialize5[i]);
    clReleaseKernel(k_initialize6[i]);
    clReleaseKernel(k_initialize7[i]);
    clReleaseKernel(k_initialize8[i]);
  }

  free(k_initialize1);
  free(k_initialize2);
  free(k_initialize3);
  free(k_initialize4);
  free(k_initialize5);
  free(k_initialize6);
  free(k_initialize7);
  free(k_initialize8);
}


void lhsinit()
{
  int i;
  size_t d0_size, d1_size, d2_size;
  size_t local_ws[3], global_ws[3], temp;

  cl_kernel *k_lhsinit;
  cl_int ecode;

  k_lhsinit = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);

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

    k_lhsinit[i] = clCreateKernel(p_initialize[i], "lhsinit", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    
    ecode  = clSetKernelArg(k_lhsinit[i], 0, sizeof(cl_mem), &m_lhsc[i]);
    ecode |= clSetKernelArg(k_lhsinit[i], 1, sizeof(cl_mem), &m_start[i]);
    ecode |= clSetKernelArg(k_lhsinit[i], 2, sizeof(cl_mem), &m_end[i]);
    ecode |= clSetKernelArg(k_lhsinit[i], 3, sizeof(cl_mem),&m_cell_coord[i]);
    ecode |= clSetKernelArg(k_lhsinit[i], 4, sizeof(cl_mem), &m_cell_size[i]);
    ecode |= clSetKernelArg(k_lhsinit[i], 5, sizeof(int), &ncells);
    clu_CheckError(ecode, "clSetKernelArg()");
    
    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_lhsinit[i],
                                   3, NULL,
                                   global_ws,
                                   local_ws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  for (i = 0; i < num_devices; i++) {
    clReleaseKernel(k_lhsinit[i]);
  }
  free(k_lhsinit);
}

