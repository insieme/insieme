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
// this function copies the face values of a variable defined on a set 
// of cells to the overlap locations of the adjacent sets of cells. 
// Because a set of cells interfaces in each direction with exactly one 
// other set, we only need to fill six different buffers. We could try to 
// overlap communication with computation, by computing
// some internal values while communicating boundary values, but this
// adds so much overhead that it's not clearly useful. 
//---------------------------------------------------------------------
void copy_faces()
{
  int i, c;
  size_t d0_size, d1_size, d2_size;
  size_t local_ws[3], global_ws[3], temp;
  cl_int ecode;

  //---------------------------------------------------------------------
  // exit immediately if there are no faces to be copied           
  //---------------------------------------------------------------------
  if (num_devices == 1) {
    compute_rhs();
    return;
  }

  //---------------------------------------------------------------------
  // because the difference stencil for the diagonalized scheme is 
  // orthogonal, we do not have to perform the staged copying of faces, 
  // but can send all face information simultaneously to the neighboring 
  // cells in all directions          
  //---------------------------------------------------------------------
  if (timeron) timer_start(t_bpack);

  if (COPY_FACES_OUT1_DIM == 3) {
    for (i = 0; i < num_devices; i++) {
      d0_size = max_cell_size[i][1];
      d1_size = max_cell_size[i][2];
      d2_size = ncells;

      local_ws[0] = d0_size < work_item_sizes[0] ? d0_size:work_item_sizes[0];
      temp = max_work_group_size / local_ws[0];
      local_ws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / local_ws[1];
      local_ws[2] = d2_size < temp ? d2_size : temp;
      global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
      global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
      global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

      ecode  = clSetKernelArg(k_cf_out1[i], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_cf_out1[i], 1, sizeof(cl_mem),
                                               &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_cf_out1[i], 2, sizeof(cl_mem),
                                               &m_cell_coord[i]);
      ecode |= clSetKernelArg(k_cf_out1[i], 3, sizeof(cl_mem),
                                               &m_cell_size[i]);
      ecode |= clSetKernelArg(k_cf_out1[i], 4, sizeof(cl_mem),
                                               &m_start_send[i]);
      ecode |= clSetKernelArg(k_cf_out1[i], 5, sizeof(cl_mem),
                                               &m_p0_offset[i]);
      ecode |= clSetKernelArg(k_cf_out1[i], 6, sizeof(cl_mem),
                                               &m_p1_offset[i]);
      ecode |= clSetKernelArg(k_cf_out1[i], 7, sizeof(int), &ncells);
      clu_CheckError(ecode, "clSetKernelArg()");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                     k_cf_out1[i],
                                     COPY_FACES_OUT1_DIM, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    }
  } else {
    int (*p0_offset)[ncells] = (int (*)[ncells])g_p0_offset;
    int (*p1_offset)[ncells] = (int (*)[ncells])g_p1_offset;

    for (c = 0; c < ncells; c++) {
      for (i = 0; i < num_devices; i++) {
        if (COPY_FACES_OUT1_DIM == 2) {
          d0_size = max_cell_size[i][1];
          d1_size = max_cell_size[i][2];
          local_ws[0] = d0_size < work_item_sizes[0] ?
                        d0_size : work_item_sizes[0];
          temp = max_work_group_size / local_ws[0];
          local_ws[1] = d1_size < temp ? d1_size : temp;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
          global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
        } else {
          d0_size = 1; // max_cell_size[i][2] / max_compute_units;
          local_ws[0] = d0_size == 0 ? 1 : d0_size;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
        }

        ecode  = clSetKernelArg(k_cf_out1[i], 0, sizeof(cl_mem), &m_u[i]);
        ecode |= clSetKernelArg(k_cf_out1[i], 1, sizeof(cl_mem),
                                                 &m_out_buffer[i]);
        ecode |= clSetKernelArg(k_cf_out1[i], 2, sizeof(cl_mem),
                                                 &m_cell_coord[i]);
        ecode |= clSetKernelArg(k_cf_out1[i], 3, sizeof(cl_mem),
                                                 &m_cell_size[i]);
        ecode |= clSetKernelArg(k_cf_out1[i], 4, sizeof(cl_mem),
                                                 &m_start_send[i]);
        ecode |= clSetKernelArg(k_cf_out1[i], 5, sizeof(int),
                                                 &p0_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_out1[i], 6, sizeof(int),
                                                 &p1_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_out1[i], 7, sizeof(int), &c);
        ecode |= clSetKernelArg(k_cf_out1[i], 8, sizeof(int), &ncells);
        clu_CheckError(ecode, "clSetKernelArg()");

        ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                       k_cf_out1[i],
                                       COPY_FACES_OUT1_DIM, NULL,
                                       global_ws,
                                       local_ws,
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
      }
      CHECK_FINISH();
    }
  }

  if (COPY_FACES_OUT2_DIM == 3) {
    for (i = 0; i < num_devices; i++) {
      d0_size = max_cell_size[i][0];
      d1_size = max_cell_size[i][2];
      d2_size = ncells;

      local_ws[0] = d0_size < work_item_sizes[0] ? d0_size:work_item_sizes[0];
      temp = max_work_group_size / local_ws[0];
      local_ws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / local_ws[1];
      local_ws[2] = d2_size < temp ? d2_size : temp;
      global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
      global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
      global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

      ecode  = clSetKernelArg(k_cf_out2[i], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_cf_out2[i], 1, sizeof(cl_mem),
                                               &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_cf_out2[i], 2, sizeof(cl_mem),
                                               &m_cell_coord[i]);
      ecode |= clSetKernelArg(k_cf_out2[i], 3, sizeof(cl_mem),
                                               &m_cell_size[i]);
      ecode |= clSetKernelArg(k_cf_out2[i], 4, sizeof(cl_mem),
                                               &m_start_send[i]);
      ecode |= clSetKernelArg(k_cf_out2[i], 5, sizeof(cl_mem),
                                               &m_p2_offset[i]);
      ecode |= clSetKernelArg(k_cf_out2[i], 6, sizeof(cl_mem),
                                               &m_p3_offset[i]);
      ecode |= clSetKernelArg(k_cf_out2[i], 7, sizeof(int), &ncells);
      clu_CheckError(ecode, "clSetKernelArg()");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                     k_cf_out2[i],
                                     COPY_FACES_OUT2_DIM, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    }
  } else {
    int (*p2_offset)[ncells] = (int (*)[ncells])g_p2_offset;
    int (*p3_offset)[ncells] = (int (*)[ncells])g_p3_offset;

    for (c = 0; c < ncells; c++) {
      for (i = 0; i < num_devices; i++) {
        if (COPY_FACES_OUT2_DIM == 2) {
          d0_size = max_cell_size[i][0];
          d1_size = max_cell_size[i][2];
          local_ws[0] = d0_size < work_item_sizes[0] ?
                        d0_size : work_item_sizes[0];
          temp = max_work_group_size / local_ws[0];
          local_ws[1] = d1_size < temp ? d1_size : temp;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
          global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
        } else {
          d0_size = 1; // max_cell_size[i][2] / max_compute_units;
          local_ws[0] = d0_size == 0 ? 1 : d0_size;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
        }

        ecode  = clSetKernelArg(k_cf_out2[i], 0, sizeof(cl_mem), &m_u[i]);
        ecode |= clSetKernelArg(k_cf_out2[i], 1, sizeof(cl_mem),
                                                 &m_out_buffer[i]);
        ecode |= clSetKernelArg(k_cf_out2[i], 2, sizeof(cl_mem),
                                                 &m_cell_coord[i]);
        ecode |= clSetKernelArg(k_cf_out2[i], 3, sizeof(cl_mem),
                                                 &m_cell_size[i]);
        ecode |= clSetKernelArg(k_cf_out2[i], 4, sizeof(cl_mem),
                                                 &m_start_send[i]);
        ecode |= clSetKernelArg(k_cf_out2[i], 5, sizeof(int),
                                                 &p2_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_out2[i], 6, sizeof(int),
                                                 &p3_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_out2[i], 7, sizeof(int), &c);
        ecode |= clSetKernelArg(k_cf_out2[i], 8, sizeof(int), &ncells);
        clu_CheckError(ecode, "clSetKernelArg()");

        ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                       k_cf_out2[i],
                                       COPY_FACES_OUT2_DIM, NULL,
                                       global_ws,
                                       local_ws,
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
      }
      CHECK_FINISH();
    }
  }

  if (COPY_FACES_OUT3_DIM == 3) {
    for (i = 0; i < num_devices; i++) {
      d0_size = max_cell_size[i][0];
      d1_size = max_cell_size[i][1];
      d2_size = ncells;

      local_ws[0] = d0_size < work_item_sizes[0] ? d0_size:work_item_sizes[0];
      temp = max_work_group_size / local_ws[0];
      local_ws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / local_ws[1];
      local_ws[2] = d2_size < temp ? d2_size : temp;
      global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
      global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
      global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

      ecode  = clSetKernelArg(k_cf_out3[i], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_cf_out3[i], 1, sizeof(cl_mem),
                                               &m_out_buffer[i]);
      ecode |= clSetKernelArg(k_cf_out3[i], 2, sizeof(cl_mem),
                                               &m_cell_coord[i]);
      ecode |= clSetKernelArg(k_cf_out3[i], 3, sizeof(cl_mem),
                                               &m_cell_size[i]);
      ecode |= clSetKernelArg(k_cf_out3[i], 4, sizeof(cl_mem),
                                               &m_start_send[i]);
      ecode |= clSetKernelArg(k_cf_out3[i], 5, sizeof(cl_mem),
                                               &m_p4_offset[i]);
      ecode |= clSetKernelArg(k_cf_out3[i], 6, sizeof(cl_mem),
                                               &m_p5_offset[i]);
      ecode |= clSetKernelArg(k_cf_out3[i], 7, sizeof(int), &ncells);
      clu_CheckError(ecode, "clSetKernelArg()");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                     k_cf_out3[i],
                                     COPY_FACES_OUT3_DIM, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    }
  } else {
    int (*p4_offset)[ncells] = (int (*)[ncells])g_p4_offset;
    int (*p5_offset)[ncells] = (int (*)[ncells])g_p5_offset;

    for (c = 0; c < ncells; c++) {
      for (i = 0; i < num_devices; i++) {
        if (COPY_FACES_OUT3_DIM == 2) {
          d0_size = max_cell_size[i][0];
          d1_size = max_cell_size[i][1];
          local_ws[0] = d0_size < work_item_sizes[0] ?
                        d0_size : work_item_sizes[0];
          temp = max_work_group_size / local_ws[0];
          local_ws[1] = d1_size < temp ? d1_size : temp;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
          global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
        } else {
          d0_size = 1; // max_cell_size[i][2] / max_compute_units;
          local_ws[0] = d0_size == 0 ? 1 : d0_size;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
        }

        ecode  = clSetKernelArg(k_cf_out3[i], 0, sizeof(cl_mem), &m_u[i]);
        ecode |= clSetKernelArg(k_cf_out3[i], 1, sizeof(cl_mem),
                                                 &m_out_buffer[i]);
        ecode |= clSetKernelArg(k_cf_out3[i], 2, sizeof(cl_mem),
                                                 &m_cell_coord[i]);
        ecode |= clSetKernelArg(k_cf_out3[i], 3, sizeof(cl_mem),
                                                 &m_cell_size[i]);
        ecode |= clSetKernelArg(k_cf_out3[i], 4, sizeof(cl_mem),
                                                 &m_start_send[i]);
        ecode |= clSetKernelArg(k_cf_out3[i], 5, sizeof(int),
                                                 &p4_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_out3[i], 6, sizeof(int),
                                                 &p5_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_out3[i], 7, sizeof(int), &c);
        ecode |= clSetKernelArg(k_cf_out3[i], 8, sizeof(int), &ncells);
        clu_CheckError(ecode, "clSetKernelArg()");

        ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                       k_cf_out3[i],
                                       COPY_FACES_OUT3_DIM, NULL,
                                       global_ws,
                                       local_ws,
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
      }
      CHECK_FINISH();
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timeron) timer_stop(t_bpack);

  if (timeron) timer_start(t_exch);
  int dst;
  size_t cb, src_offset, dst_offset;

  for (i = 0; i < num_devices; i++) {
    cb = sizeof(double) * box_size[i][east];
    src_offset = sizeof(double) * start_send[i][east];
    dst = successor[i][0];
    dst_offset = sizeof(double) * start_recv[dst][west];
    ecode = clEnqueueCopyBuffer(cmd_queue[dst],
                                m_out_buffer[i],
                                m_in_buffer[dst],
                                src_offset,
                                dst_offset,
                                cb,
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueCopyBuffer()");
    
    cb = sizeof(double) * box_size[i][west];
    src_offset = sizeof(double) * start_send[i][west];
    dst = predecessor[i][0];
    dst_offset = sizeof(double) * start_recv[dst][east];
    ecode = clEnqueueCopyBuffer(cmd_queue[dst],
                                m_out_buffer[i],
                                m_in_buffer[dst],
                                src_offset,
                                dst_offset,
                                cb,
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueCopyBuffer()");
    
    cb = sizeof(double) * box_size[i][north];
    src_offset = sizeof(double) * start_send[i][north];
    dst = successor[i][1];
    dst_offset = sizeof(double) * start_recv[dst][south];
    ecode = clEnqueueCopyBuffer(cmd_queue[dst],
                                m_out_buffer[i],
                                m_in_buffer[dst],
                                src_offset,
                                dst_offset,
                                cb,
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueCopyBuffer()");
    
    cb = sizeof(double) * box_size[i][south];
    src_offset = sizeof(double) * start_send[i][south];
    dst = predecessor[i][1];
    dst_offset = sizeof(double) * start_recv[dst][north];
    ecode = clEnqueueCopyBuffer(cmd_queue[dst],
                                m_out_buffer[i],
                                m_in_buffer[dst],
                                src_offset,
                                dst_offset,
                                cb,
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueCopyBuffer()");
    
    cb = sizeof(double) * box_size[i][top];
    src_offset = sizeof(double) * start_send[i][top];
    dst = successor[i][2];
    dst_offset = sizeof(double) * start_recv[dst][bottom];
    ecode = clEnqueueCopyBuffer(cmd_queue[dst],
                                m_out_buffer[i],
                                m_in_buffer[dst],
                                src_offset,
                                dst_offset,
                                cb,
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueCopyBuffer()");
    
    cb = sizeof(double) * box_size[i][bottom];
    src_offset = sizeof(double) * start_send[i][bottom];
    dst = predecessor[i][2];
    dst_offset = sizeof(double) * start_recv[dst][top];
    ecode = clEnqueueCopyBuffer(cmd_queue[dst],
                                m_out_buffer[i],
                                m_in_buffer[dst],
                                src_offset,
                                dst_offset,
                                cb,
                                0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueCopyBuffer()");
  }

  CHECK_FINISH();
  if (timeron) timer_stop(t_exch);

  //---------------------------------------------------------------------
  // unpack the data that has just been received;             
  //---------------------------------------------------------------------
  if (timeron) timer_start(t_bpack);

  if (COPY_FACES_IN1_DIM == 3) {
    for (i = 0; i < num_devices; i++) {
      d0_size = max_cell_size[i][1];
      d1_size = max_cell_size[i][2];
      d2_size = ncells;

      local_ws[0] = d0_size < work_item_sizes[0] ? d0_size:work_item_sizes[0];
      temp = max_work_group_size / local_ws[0];
      local_ws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / local_ws[1];
      local_ws[2] = d2_size < temp ? d2_size : temp;
      global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
      global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
      global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

      ecode  = clSetKernelArg(k_cf_in1[i], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_cf_in1[i], 1, sizeof(cl_mem),
                                              &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_cf_in1[i], 2, sizeof(cl_mem),
                                              &m_cell_coord[i]);
      ecode |= clSetKernelArg(k_cf_in1[i], 3, sizeof(cl_mem),
                                              &m_cell_size[i]);
      ecode |= clSetKernelArg(k_cf_in1[i], 4, sizeof(cl_mem),
                                              &m_start_recv[i]);
      ecode |= clSetKernelArg(k_cf_in1[i], 5, sizeof(cl_mem),
                                              &m_p0_offset[i]);
      ecode |= clSetKernelArg(k_cf_in1[i], 6, sizeof(cl_mem),
                                              &m_p1_offset[i]);
      ecode |= clSetKernelArg(k_cf_in1[i], 7, sizeof(int), &ncells);
      clu_CheckError(ecode, "clSetKernelArg()");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                     k_cf_in1[i],
                                     COPY_FACES_IN1_DIM, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    }
  } else {
    int (*p0_offset)[ncells] = (int (*)[ncells])g_p0_offset;
    int (*p1_offset)[ncells] = (int (*)[ncells])g_p1_offset;

    for (c = 0; c < ncells; c++) {
      for (i = 0; i < num_devices; i++) {
        if (COPY_FACES_IN1_DIM == 2) {
          d0_size = max_cell_size[i][1];
          d1_size = max_cell_size[i][2];
          local_ws[0] = d0_size < work_item_sizes[0] ?
                        d0_size : work_item_sizes[0];
          temp = max_work_group_size / local_ws[0];
          local_ws[1] = d1_size < temp ? d1_size : temp;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
          global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
        } else {
          d0_size = 1; // max_cell_size[i][2] / max_compute_units;
          local_ws[0] = d0_size == 0 ? 1 : d0_size;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
        }

        ecode  = clSetKernelArg(k_cf_in1[i], 0, sizeof(cl_mem), &m_u[i]);
        ecode |= clSetKernelArg(k_cf_in1[i], 1, sizeof(cl_mem),
                                                &m_in_buffer[i]);
        ecode |= clSetKernelArg(k_cf_in1[i], 2, sizeof(cl_mem),
                                                &m_cell_coord[i]);
        ecode |= clSetKernelArg(k_cf_in1[i], 3, sizeof(cl_mem),
                                                &m_cell_size[i]);
        ecode |= clSetKernelArg(k_cf_in1[i], 4, sizeof(cl_mem),
                                                &m_start_recv[i]);
        ecode |= clSetKernelArg(k_cf_in1[i], 5, sizeof(int),
                                                &p0_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_in1[i], 6, sizeof(int),
                                                &p1_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_in1[i], 7, sizeof(int), &c);
        ecode |= clSetKernelArg(k_cf_in1[i], 8, sizeof(int), &ncells);
        clu_CheckError(ecode, "clSetKernelArg()");

        ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                       k_cf_in1[i],
                                       COPY_FACES_IN1_DIM, NULL,
                                       global_ws,
                                       local_ws,
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
      }
      CHECK_FINISH();
    }
  }

  if (COPY_FACES_IN2_DIM == 3) {
    for (i = 0; i < num_devices; i++) {
      d0_size = max_cell_size[i][0];
      d1_size = max_cell_size[i][2];
      d2_size = ncells;

      local_ws[0] = d0_size < work_item_sizes[0] ? d0_size:work_item_sizes[0];
      temp = max_work_group_size / local_ws[0];
      local_ws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / local_ws[1];
      local_ws[2] = d2_size < temp ? d2_size : temp;
      global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
      global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
      global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

      ecode  = clSetKernelArg(k_cf_in2[i], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_cf_in2[i], 1, sizeof(cl_mem),
                                              &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_cf_in2[i], 2, sizeof(cl_mem),
                                              &m_cell_coord[i]);
      ecode |= clSetKernelArg(k_cf_in2[i], 3, sizeof(cl_mem),
                                              &m_cell_size[i]);
      ecode |= clSetKernelArg(k_cf_in2[i], 4, sizeof(cl_mem),
                                              &m_start_recv[i]);
      ecode |= clSetKernelArg(k_cf_in2[i], 5, sizeof(cl_mem),
                                              &m_p2_offset[i]);
      ecode |= clSetKernelArg(k_cf_in2[i], 6, sizeof(cl_mem),
                                              &m_p3_offset[i]);
      ecode |= clSetKernelArg(k_cf_in2[i], 7, sizeof(int), &ncells);
      clu_CheckError(ecode, "clSetKernelArg()");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                     k_cf_in2[i],
                                     COPY_FACES_IN2_DIM, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    }
  } else {
    int (*p2_offset)[ncells] = (int (*)[ncells])g_p2_offset;
    int (*p3_offset)[ncells] = (int (*)[ncells])g_p3_offset;

    for (c = 0; c < ncells; c++) {
      for (i = 0; i < num_devices; i++) {
        if (COPY_FACES_IN2_DIM == 2) {
          d0_size = max_cell_size[i][0];
          d1_size = max_cell_size[i][2];
          local_ws[0] = d0_size < work_item_sizes[0] ?
                        d0_size : work_item_sizes[0];
          temp = max_work_group_size / local_ws[0];
          local_ws[1] = d1_size < temp ? d1_size : temp;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
          global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
        } else {
          d0_size = 1; // max_cell_size[i][2] / max_compute_units;
          local_ws[0] = d0_size == 0 ? 1 : d0_size;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
        }

        ecode  = clSetKernelArg(k_cf_in2[i], 0, sizeof(cl_mem), &m_u[i]);
        ecode |= clSetKernelArg(k_cf_in2[i], 1, sizeof(cl_mem),
                                                &m_in_buffer[i]);
        ecode |= clSetKernelArg(k_cf_in2[i], 2, sizeof(cl_mem),
                                                &m_cell_coord[i]);
        ecode |= clSetKernelArg(k_cf_in2[i], 3, sizeof(cl_mem),
                                                &m_cell_size[i]);
        ecode |= clSetKernelArg(k_cf_in2[i], 4, sizeof(cl_mem),
                                                &m_start_recv[i]);
        ecode |= clSetKernelArg(k_cf_in2[i], 5, sizeof(int),
                                                &p2_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_in2[i], 6, sizeof(int),
                                                &p3_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_in2[i], 7, sizeof(int), &c);
        ecode |= clSetKernelArg(k_cf_in2[i], 8, sizeof(int), &ncells);
        clu_CheckError(ecode, "clSetKernelArg()");

        ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                       k_cf_in2[i],
                                       COPY_FACES_IN2_DIM, NULL,
                                       global_ws,
                                       local_ws,
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
      }
      CHECK_FINISH();
    }
  }

  if (COPY_FACES_IN3_DIM == 3) {
    for (i = 0; i < num_devices; i++) {
      d0_size = max_cell_size[i][0];
      d1_size = max_cell_size[i][1];
      d2_size = ncells;

      local_ws[0] = d0_size < work_item_sizes[0] ? d0_size:work_item_sizes[0];
      temp = max_work_group_size / local_ws[0];
      local_ws[1] = d1_size < temp ? d1_size : temp;
      temp = temp / local_ws[1];
      local_ws[2] = d2_size < temp ? d2_size : temp;
      global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
      global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
      global_ws[2] = clu_RoundWorkSize(d2_size, local_ws[2]);

      ecode  = clSetKernelArg(k_cf_in3[i], 0, sizeof(cl_mem), &m_u[i]);
      ecode |= clSetKernelArg(k_cf_in3[i], 1, sizeof(cl_mem),
                                              &m_in_buffer[i]);
      ecode |= clSetKernelArg(k_cf_in3[i], 2, sizeof(cl_mem),
                                              &m_cell_coord[i]);
      ecode |= clSetKernelArg(k_cf_in3[i], 3, sizeof(cl_mem),
                                              &m_cell_size[i]);
      ecode |= clSetKernelArg(k_cf_in3[i], 4, sizeof(cl_mem),
                                              &m_start_recv[i]);
      ecode |= clSetKernelArg(k_cf_in3[i], 5, sizeof(cl_mem),
                                              &m_p4_offset[i]);
      ecode |= clSetKernelArg(k_cf_in3[i], 6, sizeof(cl_mem),
                                              &m_p5_offset[i]);
      ecode |= clSetKernelArg(k_cf_in3[i], 7, sizeof(int), &ncells);
      clu_CheckError(ecode, "clSetKernelArg()");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                     k_cf_in3[i],
                                     COPY_FACES_IN3_DIM, NULL,
                                     global_ws,
                                     local_ws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    }
  } else {
    int (*p4_offset)[ncells] = (int (*)[ncells])g_p4_offset;
    int (*p5_offset)[ncells] = (int (*)[ncells])g_p5_offset;

    for (c = 0; c < ncells; c++) {
      for (i = 0; i < num_devices; i++) {
        if (COPY_FACES_IN3_DIM == 2) {
          d0_size = max_cell_size[i][0];
          d1_size = max_cell_size[i][1];
          local_ws[0] = d0_size < work_item_sizes[0] ?
                        d0_size : work_item_sizes[0];
          temp = max_work_group_size / local_ws[0];
          local_ws[1] = d1_size < temp ? d1_size : temp;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
          global_ws[1] = clu_RoundWorkSize(d1_size, local_ws[1]);
        } else {
          d0_size = 1; // max_cell_size[i][2] / max_compute_units;
          local_ws[0] = d0_size == 0 ? 1 : d0_size;
          global_ws[0] = clu_RoundWorkSize(d0_size, local_ws[0]);
        }

        ecode  = clSetKernelArg(k_cf_in3[i], 0, sizeof(cl_mem), &m_u[i]);
        ecode |= clSetKernelArg(k_cf_in3[i], 1, sizeof(cl_mem),
                                                &m_in_buffer[i]);
        ecode |= clSetKernelArg(k_cf_in3[i], 2, sizeof(cl_mem),
                                                &m_cell_coord[i]);
        ecode |= clSetKernelArg(k_cf_in3[i], 3, sizeof(cl_mem),
                                                &m_cell_size[i]);
        ecode |= clSetKernelArg(k_cf_in3[i], 4, sizeof(cl_mem),
                                                &m_start_recv[i]);
        ecode |= clSetKernelArg(k_cf_in3[i], 5, sizeof(int),
                                                &p4_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_in3[i], 6, sizeof(int),
                                                &p5_offset[i][c]);
        ecode |= clSetKernelArg(k_cf_in3[i], 7, sizeof(int), &c);
        ecode |= clSetKernelArg(k_cf_in3[i], 8, sizeof(int), &ncells);
        clu_CheckError(ecode, "clSetKernelArg()");

        ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                       k_cf_in3[i],
                                       COPY_FACES_IN3_DIM, NULL,
                                       global_ws,
                                       local_ws,
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
      }
      CHECK_FINISH();
    }
  }
  
  CHECK_FINISH();
  if (timeron) timer_stop(t_bpack);

  //---------------------------------------------------------------------
  // do the rest of the rhs that uses the copied face values          
  //---------------------------------------------------------------------
  compute_rhs();
}

