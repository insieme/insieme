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
  int c, i;
  cl_int ecode = 0;

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

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_copy_faces1[i][c],
                                     COPY_FACES1_DIM, NULL,
                                     copy_faces1_gw[i][c],
                                     copy_faces1_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for copy_faces1");
    }

    for (i = 0; i < num_devices; i++) {
      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_copy_faces2[i][c],
                                     COPY_FACES2_DIM, NULL,
                                     copy_faces2_gw[i][c],
                                     copy_faces2_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for copy_faces2");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_copy_faces3[i][c],
                                     COPY_FACES3_DIM, NULL,
                                     copy_faces3_gw[i][c],
                                     copy_faces3_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for copy_faces3");
    }

    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }

  }

  if (timeron) timer_stop(t_bpack);

  if (timeron) timer_start(t_exch);
  for (i = 0; i < num_devices; i++) {
    CHECK_FINISH(i * 2);

    ecode = clEnqueueCopyBuffer(cmd_queue[successor[i][0] * 2 + 1],
                                m_out_buffer[i],
                                m_in_buffer[successor[i][0]],
                                start_send_east[i]*sizeof(double),
                                start_recv_west[successor[i][0]]*sizeof(double),
                                east_size[i]*sizeof(double),
                                0, NULL, NULL);
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clEnqueueCopyBuffer(cmd_queue[predecessor[i][0] * 2 + 1],
                                m_out_buffer[i],
                                m_in_buffer[predecessor[i][0]],
                                start_send_west[i]*sizeof(double),
                                start_recv_east[predecessor[i][0]]*sizeof(double),
                                west_size[i]*sizeof(double),
                                0, NULL, NULL);

    ecode = clEnqueueCopyBuffer(cmd_queue[successor[i][1] * 2 + 1],
                                m_out_buffer[i],
                                m_in_buffer[successor[i][1]],
                                start_send_north[i]*sizeof(double),
                                start_recv_south[successor[i][1]]*sizeof(double),
                                north_size[i]*sizeof(double),
                                0, NULL, NULL);

    ecode = clEnqueueCopyBuffer(cmd_queue[predecessor[i][1] * 2 + 1],
                                m_out_buffer[i],
                                m_in_buffer[predecessor[i][1]],
                                start_send_south[i]*sizeof(double),
                                start_recv_north[predecessor[i][1]]*sizeof(double),
                                south_size[i]*sizeof(double),
                                0, NULL, NULL);

    ecode = clEnqueueCopyBuffer(cmd_queue[successor[i][2] * 2 + 1],
                                m_out_buffer[i],
                                m_in_buffer[successor[i][2]],
                                start_send_top[i]*sizeof(double),
                                start_recv_bottom[successor[i][2]]*sizeof(double),
                                top_size[i]*sizeof(double),
                                0, NULL, NULL);

    ecode = clEnqueueCopyBuffer(cmd_queue[predecessor[i][2] * 2 + 1],
                                m_out_buffer[i],
                                m_in_buffer[predecessor[i][2]],
                                start_send_bottom[i]*sizeof(double),
                                start_recv_top[predecessor[i][2]]*sizeof(double),
                                bottom_size[i]*sizeof(double),
                                0, NULL, NULL);
  }
  if (timeron) timer_stop(t_exch);

  //---------------------------------------------------------------------
  // unpack the data that has just been received;             
  //---------------------------------------------------------------------
  if (timeron) timer_start(t_bpack);

  for (c = 0; c < ncells; c++) {
    for (i = 0; i < num_devices; i++) {
      if (c == 0) CHECK_FINISH(i * 2 + 1);

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_copy_faces4[i][c],
                                     COPY_FACES4_DIM, NULL,
                                     copy_faces4_gw[i][c],
                                     copy_faces4_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for copy_faces4");
    }

    for (i = 0; i < num_devices; i++) {
      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_copy_faces5[i][c],
                                     COPY_FACES5_DIM, NULL,
                                     copy_faces5_gw[i][c],
                                     copy_faces5_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for copy_faces5");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i * 2],
                                     k_copy_faces6[i][c],
                                     COPY_FACES6_DIM, NULL,
                                     copy_faces6_gw[i][c],
                                     copy_faces6_lw[i][c],
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRange() for copy_faces6");
    }

    for (i = 0; i < num_devices; i++) {
      CHECK_FINISH(i * 2);
    }
  }

  if (timeron) timer_stop(t_bpack);

  for (i = 0; i < num_devices; i++)
    CHECK_FINISH(i * 2);

  //---------------------------------------------------------------------
  // now that we have all the data, compute the rhs
  //---------------------------------------------------------------------
  compute_rhs();
}

