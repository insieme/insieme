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

void compute_ws_dim_1(size_t d[], size_t local_ws[], size_t global_ws[]) {
  local_ws[0] = d[0] < work_item_sizes[0] ? d[0] : work_item_sizes[0];
  global_ws[0] = clu_RoundWorkSize(d[0], local_ws[0]);
}

void compute_ws_dim_2(size_t d[], size_t local_ws[], size_t global_ws[]) {
  size_t temp;
  local_ws[0] = d[0] < work_item_sizes[0] ? d[0] : work_item_sizes[0];
  temp = max_work_group_size / local_ws[0];
  local_ws[1] = d[1] < temp ? d[1] : temp;

  global_ws[0] = clu_RoundWorkSize(d[0], local_ws[0]);
  global_ws[1] = clu_RoundWorkSize(d[1], local_ws[1]);
}

void compute_ws_dim_3(size_t d[], size_t local_ws[], size_t global_ws[]) {
  size_t temp;
  local_ws[0] = d[0] < work_item_sizes[0] ? d[0] : work_item_sizes[0];
  temp = max_work_group_size / local_ws[0];
  local_ws[1] = d[1] < temp ? d[1] : temp;
  temp = temp / local_ws[1];
  local_ws[2] = d[2] < temp ? d[2] : temp;

  global_ws[0] = clu_RoundWorkSize(d[0], local_ws[0]);
  global_ws[1] = clu_RoundWorkSize(d[1], local_ws[1]);
  global_ws[2] = clu_RoundWorkSize(d[2], local_ws[2]);
}

void compute_buffer_size(int dim)
{
  int i, c, face_size;

  if (ncells == 1) return;

  //---------------------------------------------------------------------
  // compute the actual sizes of the buffers; note that there is 
  // always one cell face that doesn't need buffer space, because it 
  // is at the boundary of the grid
  //---------------------------------------------------------------------

  for (i = 0; i < num_devices; i++) {
    west_size[i] = 0;
    east_size[i] = 0;

    for (c = 0; c < ncells; c++) {
      face_size = cell_size[i][c][1] * cell_size[i][c][2] * dim * 2;
      if (cell_coord[i][c][0] != 0) west_size[i] = west_size[i] + face_size;
      if (cell_coord[i][c][0] != (ncells-1)) east_size[i] = east_size[i] + face_size;
    }

    north_size[i] = 0;
    south_size[i] = 0;
    for (c = 0; c < ncells; c++) {
      face_size = cell_size[i][c][0]*cell_size[i][c][2] * dim * 2;
      if (cell_coord[i][c][1] != 0) south_size[i] = south_size[i] + face_size;
      if (cell_coord[i][c][1] != (ncells-1)) north_size[i] = north_size[i] + face_size;
    }

    top_size[i] = 0;
    bottom_size[i] = 0;
    for (c = 0; c < ncells; c++) {
      face_size = cell_size[i][c][0] * cell_size[i][c][1] * dim * 2;
      if (cell_coord[i][c][2] != 0) bottom_size[i] = bottom_size[i] + face_size;
      if (cell_coord[i][c][2] != (ncells-1)) top_size[i] = top_size[i] + face_size;   
    }

    start_send_west[i]   = 0;
    start_send_east[i]   = start_send_west[i]   + west_size[i];
    start_send_south[i]  = start_send_east[i]   + east_size[i];
    start_send_north[i]  = start_send_south[i]  + south_size[i];
    start_send_bottom[i] = start_send_north[i]  + north_size[i];
    start_send_top[i]    = start_send_bottom[i] + bottom_size[i];
    start_recv_west[i]   = 0;
    start_recv_east[i]   = start_recv_west[i]   + west_size[i];
    start_recv_south[i]  = start_recv_east[i]   + east_size[i];
    start_recv_north[i]  = start_recv_south[i]  + south_size[i];
    start_recv_bottom[i] = start_recv_north[i]  + north_size[i];
    start_recv_top[i]    = start_recv_bottom[i] + bottom_size[i];
  }
}

