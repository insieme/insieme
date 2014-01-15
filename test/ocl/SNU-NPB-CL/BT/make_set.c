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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "header.h"
#include "work_lhs.h"

//---------------------------------------------------------------------
// This function allocates space for a set of cells and fills the set     
// such that communication between cells on different nodes is only
// nearest neighbor                                                   
//---------------------------------------------------------------------
void make_set()
{
  int p, i, j, c, dir, size, excess;
  int node;
  cl_int ecode;

  int (*cell_coord)[MAXCELLS][3];
  int (*cell_low)[MAXCELLS][3];
  int (*cell_high)[MAXCELLS][3];
  int (*cell_size)[MAXCELLS][3];
  int (*slice)[MAXCELLS][3];

  g_cell_coord = (int *)malloc(sizeof(int)*MAXCELLS*3 * num_devices);
  g_cell_low   = (int *)malloc(sizeof(int)*MAXCELLS*3 * num_devices);
  g_cell_high  = (int *)malloc(sizeof(int)*MAXCELLS*3 * num_devices);
  g_cell_size  = (int *)malloc(sizeof(int)*MAXCELLS*3 * num_devices);
  g_slice      = (int *)malloc(sizeof(int)*MAXCELLS*3 * num_devices);
  g_start      = (int *)malloc(sizeof(int)*MAXCELLS*3 * num_devices);
  g_end        = (int *)malloc(sizeof(int)*MAXCELLS*3 * num_devices);
  predecessor  = (int (*)[3])malloc(sizeof(int)*3 * num_devices);
  successor    = (int (*)[3])malloc(sizeof(int)*3 * num_devices);
  max_cell_size = (int (*)[3])malloc(sizeof(int)*3 * num_devices);

  cell_coord = (int (*)[MAXCELLS][3])g_cell_coord;
  cell_low   = (int (*)[MAXCELLS][3])g_cell_low;
  cell_high  = (int (*)[MAXCELLS][3])g_cell_high;
  cell_size  = (int (*)[MAXCELLS][3])g_cell_size;
  slice      = (int (*)[MAXCELLS][3])g_slice;

  //---------------------------------------------------------------------
  // compute square root; add small number to allow for roundoff
  //---------------------------------------------------------------------
  ncells = (int)(sqrt((double)(num_devices) + 0.00001));

  //---------------------------------------------------------------------
  // this makes coding easier
  //---------------------------------------------------------------------
  p = ncells;

  for (node = 0; node < num_devices; node++) {
    //---------------------------------------------------------------------
    // determine the location of the cell at the bottom of the 3D 
    // array of cells
    //---------------------------------------------------------------------
    cell_coord[node][0][0] = node % p;
    cell_coord[node][0][1] = node / p;
    cell_coord[node][0][2] = 0;

    //---------------------------------------------------------------------
    // set the cell_coords for cells in the rest of the z-layers; 
    // this comes down to a simple linear numbering in the z-direct-
    // ion, and to the doubly-cyclic numbering in the other dirs     
    //---------------------------------------------------------------------
    for (c = 1; c < p; c++) {
      cell_coord[node][c][0] = (cell_coord[node][c-1][0]+1) % p; 
      cell_coord[node][c][1] = (cell_coord[node][c-1][1]-1+p) % p;
      cell_coord[node][c][2] = c;
    }

    //---------------------------------------------------------------------
    // slice[n][dir] contains the sequence number of the cell that is in
    // coordinate plane n in the dir direction
    //---------------------------------------------------------------------
    for (c = 0; c < p; c++) {
      for (dir = 0; dir < 3; dir++) {
        slice[node][cell_coord[node][c][dir]][dir] = c;
      }
    }

    //---------------------------------------------------------------------
    // fill the predecessor and successor entries, using the indices 
    // of the bottom cells (they are the same at each level of k 
    // anyway) acting as if full periodicity pertains; note that p is
    // added to those arguments to the mod functions that might
    // otherwise return wrong values when using the modulo function
    //---------------------------------------------------------------------
    i = cell_coord[node][0][0];
    j = cell_coord[node][0][1];

    predecessor[node][0] = ((i-1+p) % p) + p*j;
    predecessor[node][1] = i + p*((j-1+p) % p);
    predecessor[node][2] = ((i+1) % p) + p*((j-1+p) % p);
    successor[node][0]   = ((i+1) % p) + p*j;
    successor[node][1]   = i + p*((j+1) % p);
    successor[node][2]   = ((i-1+p) % p) + p*((j+1) % p);

    //---------------------------------------------------------------------
    // now compute the sizes of the cells                                    
    //---------------------------------------------------------------------
    for (dir = 0; dir < 3; dir++) {
      max_cell_size[node][dir] = 0;

      //---------------------------------------------------------------------
      // set cell_coord range for each direction
      //---------------------------------------------------------------------
      size   = grid_points[dir] / p;
      excess = grid_points[dir] % p;
      for (c = 0; c < ncells; c++) {
        if (cell_coord[node][c][dir] < excess) {
          cell_size[node][c][dir] = size+1;
          cell_low[node][c][dir]  = cell_coord[node][c][dir]*(size+1);
          cell_high[node][c][dir] = cell_low[node][c][dir]+size;
        } else { 
          cell_size[node][c][dir] = size;
          cell_low[node][c][dir]  = excess*(size+1)+
            (cell_coord[node][c][dir]-excess)*size;
          cell_high[node][c][dir] = cell_low[node][c][dir]+size-1;
        }
        if (cell_size[node][c][dir] <= 2) {
          printf(" Error: Cell size too small. Min size is 3\n");
          exit(EXIT_FAILURE);
        }

        if (cell_size[node][c][dir] > max_cell_size[node][dir]) {
          max_cell_size[node][dir] = cell_size[node][c][dir];
        }
      }
    }
  }

  //-------------------------------------------------------------------------
  m_cell_coord = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_cell_low   = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_cell_high  = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_cell_size  = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_slice      = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_start      = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_end        = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_predecessor = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_successor   = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);

  for (i = 0; i < num_devices; i++) {
    m_cell_coord[i] = clCreateBuffer(context,
                              CL_MEM_READ_ONLY,
                              sizeof(double)*MAXCELLS*3,
                              NULL,
                              &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_cell_low[i] = clCreateBuffer(context,
                              CL_MEM_READ_ONLY,
                              sizeof(double)*MAXCELLS*3,
                              NULL,
                              &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_cell_high[i] = clCreateBuffer(context,
                              CL_MEM_READ_ONLY,
                              sizeof(double)*MAXCELLS*3,
                              NULL,
                              &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_cell_size[i] = clCreateBuffer(context,
                              CL_MEM_READ_ONLY,
                              sizeof(double)*MAXCELLS*3,
                              NULL,
                              &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_slice[i] = clCreateBuffer(context,
                              CL_MEM_READ_ONLY,
                              sizeof(double)*MAXCELLS*3,
                              NULL,
                              &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_predecessor[i] = clCreateBuffer(context,
                              CL_MEM_READ_ONLY,
                              sizeof(double)*3,
                              NULL,
                              &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_successor[i] = clCreateBuffer(context,
                              CL_MEM_READ_ONLY,
                              sizeof(double)*3,
                              NULL,
                              &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_start[i] = clCreateBuffer(context,
                              CL_MEM_READ_WRITE,
                              sizeof(double)*MAXCELLS*3,
                              NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_end[i] = clCreateBuffer(context,
                              CL_MEM_READ_WRITE,
                              sizeof(double)*MAXCELLS*3,
                              NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

  }
  for (i = 0; i < num_devices; i++) {
    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_cell_coord[i], CL_TRUE,
        0, sizeof(double)*MAXCELLS*3, &cell_coord[i][0][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_cell_low[i], CL_TRUE,
        0, sizeof(double)*MAXCELLS*3, &cell_low[i][0][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_cell_high[i], CL_TRUE,
        0, sizeof(double)*MAXCELLS*3, &cell_high[i][0][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_cell_size[i], CL_TRUE,
        0, sizeof(double)*MAXCELLS*3, &cell_size[i][0][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_slice[i], CL_TRUE,
        0, sizeof(double)*MAXCELLS*3, &slice[i][0][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_predecessor[i], CL_TRUE,
        0, sizeof(double)*3, &predecessor[i][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_successor[i], CL_TRUE,
        0, sizeof(double)*3, &successor[i][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");
  }
  //-------------------------------------------------------------------------

  //-------------------------------------------------------------------------
  g_p0_offset = (int *)malloc(sizeof(int)*ncells * num_devices);
  g_p1_offset = (int *)malloc(sizeof(int)*ncells * num_devices);
  g_p2_offset = (int *)malloc(sizeof(int)*ncells * num_devices);
  g_p3_offset = (int *)malloc(sizeof(int)*ncells * num_devices);
  g_p4_offset = (int *)malloc(sizeof(int)*ncells * num_devices);
  g_p5_offset = (int *)malloc(sizeof(int)*ncells * num_devices);

  int (*p0_offset)[ncells] = (int (*)[ncells])g_p0_offset;
  int (*p1_offset)[ncells] = (int (*)[ncells])g_p1_offset;
  int (*p2_offset)[ncells] = (int (*)[ncells])g_p2_offset;
  int (*p3_offset)[ncells] = (int (*)[ncells])g_p3_offset;
  int (*p4_offset)[ncells] = (int (*)[ncells])g_p4_offset;
  int (*p5_offset)[ncells] = (int (*)[ncells])g_p5_offset;

  for (i = 0; i < num_devices; i++) {
    p0_offset[i][0] = 0;
    p1_offset[i][0] = 0;
    p2_offset[i][0] = 0;
    p3_offset[i][0] = 0;
    p4_offset[i][0] = 0;
    p5_offset[i][0] = 0;

    for (c = 1; c < ncells; c++) {
      p0_offset[i][c] = p0_offset[i][c-1];
      if (cell_coord[i][c-1][0] != (ncells-1)) {
        p0_offset[i][c] += (cell_size[i][c-1][2]*cell_size[i][c-1][1]*2*5);
      }

      p1_offset[i][c] = p1_offset[i][c-1];
      if (cell_coord[i][c-1][0] != 0) {
        p1_offset[i][c] += (cell_size[i][c-1][2]*cell_size[i][c-1][1]*2*5);
      }

      p2_offset[i][c] = p2_offset[i][c-1];
      if (cell_coord[i][c-1][1] != (ncells-1)) {
        p2_offset[i][c] += (cell_size[i][c-1][2]*2*cell_size[i][c-1][0]*5);
      }
      
      p3_offset[i][c] = p3_offset[i][c-1];
      if (cell_coord[i][c-1][1] != 0) {
        p3_offset[i][c] += (cell_size[i][c-1][2]*2*cell_size[i][c-1][0]*5);
      }

      p4_offset[i][c] = p4_offset[i][c-1];
      if (cell_coord[i][c-1][2] != (ncells-1)) {
        p4_offset[i][c] += (2*cell_size[i][c-1][1]*cell_size[i][c-1][0]*5);
      }

      p5_offset[i][c] = p5_offset[i][c-1];
      if (cell_coord[i][c-1][2] != 0) {
        p5_offset[i][c] += (2*cell_size[i][c-1][1]*cell_size[i][c-1][0]*5);
      }
    }
  }

  if (ncells > 1) {
    m_p0_offset = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
    m_p1_offset = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
    m_p2_offset = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
    m_p3_offset = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
    m_p4_offset = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
    m_p5_offset = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
    for (i = 0; i < num_devices; i++) {
      m_p0_offset[i] = clCreateBuffer(context,
                                CL_MEM_READ_ONLY,
                                sizeof(int)*ncells,
                                NULL,
                                &ecode);
      clu_CheckError(ecode, "clCreateBuffer()");

      m_p1_offset[i] = clCreateBuffer(context,
                                CL_MEM_READ_ONLY,
                                sizeof(int)*ncells,
                                NULL,
                                &ecode);
      clu_CheckError(ecode, "clCreateBuffer()");

      m_p2_offset[i] = clCreateBuffer(context,
                                CL_MEM_READ_ONLY,
                                sizeof(int)*ncells,
                                NULL,
                                &ecode);
      clu_CheckError(ecode, "clCreateBuffer()");

      m_p3_offset[i] = clCreateBuffer(context,
                                CL_MEM_READ_ONLY,
                                sizeof(int)*ncells,
                                NULL,
                                &ecode);
      clu_CheckError(ecode, "clCreateBuffer()");

      m_p4_offset[i] = clCreateBuffer(context,
                                CL_MEM_READ_ONLY,
                                sizeof(int)*ncells,
                                NULL,
                                &ecode);
      clu_CheckError(ecode, "clCreateBuffer()");

      m_p5_offset[i] = clCreateBuffer(context,
                                CL_MEM_READ_ONLY,
                                sizeof(int)*ncells,
                                NULL,
                                &ecode);
      clu_CheckError(ecode, "clCreateBuffer()");
    }


    for (i = 0; i < num_devices; i++) {
    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_p0_offset[i], CL_TRUE, 0, sizeof(int)*ncells, &p0_offset[i][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_p1_offset[i], CL_TRUE, 0, sizeof(int)*ncells, &p1_offset[i][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_p2_offset[i], CL_TRUE, 0, sizeof(int)*ncells, &p2_offset[i][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_p3_offset[i], CL_TRUE, 0, sizeof(int)*ncells, &p3_offset[i][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");

    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_p4_offset[i], CL_TRUE, 0, sizeof(int)*ncells, &p4_offset[i][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");
    
    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_p5_offset[i], CL_TRUE, 0, sizeof(int)*ncells, &p5_offset[i][0], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_start_send");
    }

  }
  //-------------------------------------------------------------------------

  //-------------------------------------------------------------------------
  // workspace for work-items
  //-------------------------------------------------------------------------
  size_t max_work_items, buf_size1, buf_size2, buf_size3;
  size_t max0;
  m_lhsa = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_lhsb = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_fjac = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_njac = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_utmp = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  for (i = 0; i < num_devices; i++) {
    max0 = max_cell_size[i][0];
    if (max0 < max_cell_size[i][1]) max0 = max_cell_size[i][1];
    if (max0 < max_cell_size[i][2]) max0 = max_cell_size[i][2];

    if (X_SOLVE_DIM == 1 && Y_SOLVE_DIM == 1 && Z_SOLVE_DIM == 1) {
      max_work_items = max0;
    } else {
      max_work_items = max0 * max0;
    }
    buf_size1 = sizeof(double)*(MAX_CELL_DIM+2)*5*5 * max_work_items;
    buf_size2 = sizeof(double)*(MAX_CELL_DIM+4)*5*5 * max_work_items;
    buf_size3 = sizeof(double)*(JMAX+4)*6 * max_work_items;

    m_lhsa[i] = clCreateBuffer(context,
                               CL_MEM_READ_WRITE,
                               buf_size1,
                               NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_lhsb[i] = clCreateBuffer(context,
                               CL_MEM_READ_WRITE,
                               buf_size1,
                               NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_fjac[i] = clCreateBuffer(context,
                               CL_MEM_READ_WRITE,
                               buf_size2,
                               NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_njac[i] = clCreateBuffer(context,
                               CL_MEM_READ_WRITE,
                               buf_size2,
                               NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");

    m_utmp[i] = clCreateBuffer(context,
                               CL_MEM_READ_WRITE,
                               buf_size3,
                               NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer()");
  }
  //-------------------------------------------------------------------------
}

