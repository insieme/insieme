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

void compute_buffer_size(int dim)
{
  int i, c, face_size;
  cl_int ecode;

  if (ncells == 1) return;

  box_size   = (int (*)[NUM_DIR])malloc(sizeof(int)*NUM_DIR * num_devices);
  start_send = (int (*)[NUM_DIR])malloc(sizeof(int)*NUM_DIR * num_devices);
  start_recv = (int (*)[NUM_DIR])malloc(sizeof(int)*NUM_DIR * num_devices);

  int (*cell_coord)[MAXCELLS][3] = (int (*)[MAXCELLS][3])g_cell_coord;
  int (*cell_size)[MAXCELLS][3]  = (int (*)[MAXCELLS][3])g_cell_size;

  //---------------------------------------------------------------------
  // compute the actual sizes of the buffers; note that there is 
  // always one cell face that doesn't need buffer space, because it 
  // is at the boundary of the grid
  //---------------------------------------------------------------------
  for (i = 0; i < num_devices; i++) {
    box_size[i][west] = 0;
    box_size[i][east] = 0;

    for (c = 0; c < ncells; c++) {
      face_size = cell_size[i][c][1] * cell_size[i][c][2] * dim * 2;
      if (cell_coord[i][c][0] != 0) box_size[i][west] += face_size;
      if (cell_coord[i][c][0] != (ncells-1)) box_size[i][east] += face_size;
    }

    box_size[i][north] = 0;
    box_size[i][south] = 0;
    for (c = 0; c < ncells; c++) {
      face_size = cell_size[i][c][0]*cell_size[i][c][2] * dim * 2;
      if (cell_coord[i][c][1] != 0) box_size[i][south] += face_size;
      if (cell_coord[i][c][1] != (ncells-1)) box_size[i][north] += face_size;
    }

    box_size[i][top] = 0;
    box_size[i][bottom] = 0;
    for (c = 0; c < ncells; c++) {
      face_size = cell_size[i][c][0] * cell_size[i][c][1] * dim * 2;
      if (cell_coord[i][c][2] != 0) box_size[i][bottom] += face_size;
      if (cell_coord[i][c][2] != (ncells-1)) box_size[i][top] += face_size;   
    }

    start_send[i][west]   = 0;
    start_send[i][east]   = start_send[i][west]   + box_size[i][west];
    start_send[i][south]  = start_send[i][east]   + box_size[i][east];
    start_send[i][north]  = start_send[i][south]  + box_size[i][south];
    start_send[i][bottom] = start_send[i][north]  + box_size[i][north];
    start_send[i][top]    = start_send[i][bottom] + box_size[i][bottom];
    start_recv[i][west]   = 0;
    start_recv[i][east]   = start_recv[i][west]   + box_size[i][west];
    start_recv[i][south]  = start_recv[i][east]   + box_size[i][east];
    start_recv[i][north]  = start_recv[i][south]  + box_size[i][south];
    start_recv[i][bottom] = start_recv[i][north]  + box_size[i][north];
    start_recv[i][top]    = start_recv[i][bottom] + box_size[i][bottom];
  }

  m_start_send = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_start_recv = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  for (i = 0; i < num_devices; i++) {
    m_start_send[i] = clCreateBuffer(context,
                              CL_MEM_READ_ONLY,
                              sizeof(int)*NUM_DIR,
                              NULL,
                              &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_start_recv[i] = clCreateBuffer(context,
                              CL_MEM_READ_ONLY,
                              sizeof(int)*NUM_DIR,
                              NULL,
                              &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");
  }

  for (i = 0; i < num_devices; i++) {
  ecode = clEnqueueWriteBuffer(cmd_queue[i],
                               m_start_send[i],
                               CL_TRUE,
                               0, sizeof(int)*NUM_DIR,
                               &start_send[i][0],
                               0, NULL, NULL);
  clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

  ecode = clEnqueueWriteBuffer(cmd_queue[i],
                               m_start_recv[i],
                               CL_TRUE,
                               0, sizeof(int)*NUM_DIR,
                               &start_recv[i][0],
                               0, NULL, NULL);
  clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_recv");
  }
}

