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

#include "bt.h"

//---------------------------------------------------------------------
// this function returns the exact solution at point xi, eta, zeta  
//---------------------------------------------------------------------
void exact_solution(double xi, double eta, double zeta, double dtemp[5],
                    __global const double *g_ce)
{
  int m;
  __global const double (*ce)[13] = (__global const double (*)[13])g_ce;

  for (m = 0; m < 5; m++) {
    dtemp[m] =  ce[m][0] +
      xi*(ce[m][1] + xi*(ce[m][4] + xi*(ce[m][7] + xi*ce[m][10]))) +
      eta*(ce[m][2] + eta*(ce[m][5] + eta*(ce[m][8] + eta*ce[m][11])))+
      zeta*(ce[m][3] + zeta*(ce[m][6] + zeta*(ce[m][9] + 
      zeta*ce[m][12])));
  }
}


#if COPY_FACES_OUT1_DIM == 3
__kernel void copy_faces_out1(__global double *g_u,
                              __global double *out_buffer,
                              __global int *g_cell_coord,
                              __global int *g_cell_size,
                              __global int *start_send,
                              __global int *p0_offset,
                              __global int *p1_offset,
                              int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int c, i, j, k, m, p0, p1;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;

  k = get_global_id(1) + 2;
  if (k >= cell_size[c][2]+2) return;

  j = get_global_id(0) + 2;
  if (j >= cell_size[c][1]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  //---------------------------------------------------------------------
  // fill the buffer to be sent to eastern neighbors (i-dir)
  //---------------------------------------------------------------------
  if (cell_coord[c][0] != (ncells-1)) {
    p0 = p0_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][1] + get_global_id(0));

    for (i = cell_size[c][0]; i < cell_size[c][0]+2; i++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[east]+p0] = u[c][k][j][i][m];
        p0 = p0 + 1;
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to western neighbors 
  //---------------------------------------------------------------------
  if (cell_coord[c][0] != 0) {
    p1 = p1_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][1] + get_global_id(0));

    for (i = 2; i <= 3; i++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[west]+p1] = u[c][k][j][i][m];
        p1 = p1 + 1;
      }
    }
  }
}

#else

__kernel void copy_faces_out1(__global double *g_u,
                              __global double *out_buffer,
                              __global int *g_cell_coord,
                              __global int *g_cell_size,
                              __global int *start_send,
                              int p0_offset,
                              int p1_offset,
                              int c,
                              int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int i, j, k, m, p0, p1;

  cell_size = (__global int (*)[3])g_cell_size;

#if COPY_FACES_OUT1_DIM == 2
  k = get_global_id(1) + 2;
  j = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2 || j >= cell_size[c][1]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;
  
  //---------------------------------------------------------------------
  // fill the buffer to be sent to eastern neighbors (i-dir)
  //---------------------------------------------------------------------
  if (cell_coord[c][0] != (ncells-1)) {
    p0 = p0_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][1] + get_global_id(0));

    for (i = cell_size[c][0]; i < cell_size[c][0]+2; i++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[east]+p0] = u[c][k][j][i][m];
        p0 = p0 + 1;
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to western neighbors 
  //---------------------------------------------------------------------
  if (cell_coord[c][0] != 0) {
    p1 = p1_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][1] + get_global_id(0));

    for (i = 2; i <= 3; i++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[west]+p1] = u[c][k][j][i][m];
        p1 = p1 + 1;
      }
    }
  }

#else //COPY_FACES_OUT1_DIM == 1
  k = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;
  
  //---------------------------------------------------------------------
  // fill the buffer to be sent to eastern neighbors (i-dir)
  //---------------------------------------------------------------------
  if (cell_coord[c][0] != (ncells-1)) {
    p0 = p0_offset + 5 * 2 * cell_size[c][1] * get_global_id(0);

    for (j = 2; j < cell_size[c][1]+2; j++) {
      for (i = cell_size[c][0]; i < cell_size[c][0]+2; i++) {
        for (m = 0; m < 5; m++) {
          out_buffer[start_send[east]+p0] = u[c][k][j][i][m];
          p0 = p0 + 1;
        }
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to western neighbors 
  //---------------------------------------------------------------------
  if (cell_coord[c][0] != 0) {
    p1 = p1_offset + 5 * 2 * cell_size[c][1] * get_global_id(0);

    for (j = 2; j < cell_size[c][1]+2; j++) {
      for (i = 2; i <= 3; i++) {
        for (m = 0; m < 5; m++) {
          out_buffer[start_send[west]+p1] = u[c][k][j][i][m];
          p1 = p1 + 1;
        }
      }
    }
  }
#endif
}
#endif


#if COPY_FACES_OUT2_DIM == 3
__kernel void copy_faces_out2(__global double *g_u,
                              __global double *out_buffer,
                              __global int *g_cell_coord,
                              __global int *g_cell_size,
                              __global int *start_send,
                              __global int *p2_offset,
                              __global int *p3_offset,
                              int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int c, i, j, k, m, p2, p3;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;

  k = get_global_id(1) + 2;
  i = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2 || i >= cell_size[c][0]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  //---------------------------------------------------------------------
  // fill the buffer to be sent to northern neighbors (j_dir)
  //---------------------------------------------------------------------
  if (cell_coord[c][1] != (ncells-1)) {
    p2 = p2_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (j = cell_size[c][1]; j < cell_size[c][1]+2; j++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[north]+p2] = u[c][k][j][i][m];
        p2 = p2 + 1;
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to southern neighbors 
  //---------------------------------------------------------------------
  if (cell_coord[c][1] != 0) {
    p3 = p3_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (j = 2; j <= 3; j++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[south]+p3] = u[c][k][j][i][m];
        p3 = p3 + 1;
      }
    }
  }
}

#else

__kernel void copy_faces_out2(__global double *g_u,
                              __global double *out_buffer,
                              __global int *g_cell_coord,
                              __global int *g_cell_size,
                              __global int *start_send,
                              int p2_offset,
                              int p3_offset,
                              int c,
                              int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int i, j, k, m, p2, p3;

  cell_size = (__global int (*)[3])g_cell_size;

#if COPY_FACES_OUT2_DIM == 2
  k = get_global_id(1) + 2;
  i = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2 || i >= cell_size[c][0]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;
  
  //---------------------------------------------------------------------
  // fill the buffer to be sent to northern neighbors (j_dir)
  //---------------------------------------------------------------------
  if (cell_coord[c][1] != (ncells-1)) {
    p2 = p2_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (j = cell_size[c][1]; j < cell_size[c][1]+2; j++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[north]+p2] = u[c][k][j][i][m];
        p2 = p2 + 1;
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to southern neighbors 
  //---------------------------------------------------------------------
  if (cell_coord[c][1] != 0) {
    p3 = p3_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (j = 2; j <= 3; j++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[south]+p3] = u[c][k][j][i][m];
        p3 = p3 + 1;
      }
    }
  }

#else //COPY_FACES_OUT2_DIM == 1
  k = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;
  
  //---------------------------------------------------------------------
  // fill the buffer to be sent to northern neighbors (j_dir)
  //---------------------------------------------------------------------
  if (cell_coord[c][1] != (ncells-1)) {
    p2 = p2_offset + 5 * 2 * cell_size[c][0] * get_global_id(0);

    for (j = cell_size[c][1]; j < cell_size[c][1]+2; j++) {
      for (i = 2; i < cell_size[c][0]+2; i++) {
        for (m = 0; m < 5; m++) {
          out_buffer[start_send[north]+p2] = u[c][k][j][i][m];
          p2 = p2 + 1;
        }
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to southern neighbors 
  //---------------------------------------------------------------------
  if (cell_coord[c][1] != 0) {
    p3 = p3_offset + 5 * 2 * cell_size[c][0] * get_global_id(0);

    for (j = 2; j <= 3; j++) {
      for (i = 2; i < cell_size[c][0]+2; i++) {
        for (m = 0; m < 5; m++) {
          out_buffer[start_send[south]+p3] = u[c][k][j][i][m];
          p3 = p3 + 1;
        }
      }
    }
  }
#endif
}
#endif


#if COPY_FACES_OUT3_DIM == 3
__kernel void copy_faces_out3(__global double *g_u,
                              __global double *out_buffer,
                              __global int *g_cell_coord,
                              __global int *g_cell_size,
                              __global int *start_send,
                              __global int *p4_offset,
                              __global int *p5_offset,
                              int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int c, i, j, k, m, p4, p5;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;

  j = get_global_id(1) + 2;
  i = get_global_id(0) + 2;
  if (j >= cell_size[c][1]+2 || i >= cell_size[c][0]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  //---------------------------------------------------------------------
  // fill the buffer to be sent to top neighbors (k-dir)
  //---------------------------------------------------------------------
  if (cell_coord[c][2] != (ncells-1)) {
    p4 = p4_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (k = cell_size[c][2]; k < cell_size[c][2]+2; k++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[top]+p4] = u[c][k][j][i][m];
        p4 = p4 + 1;
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to bottom neighbors
  //---------------------------------------------------------------------
  if (cell_coord[c][2] != 0) {
    p5 = p5_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (k = 2; k <= 3; k++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[bottom]+p5] = u[c][k][j][i][m];
        p5 = p5 + 1;
      }
    }
  }
}

#else

__kernel void copy_faces_out3(__global double *g_u,
                              __global double *out_buffer,
                              __global int *g_cell_coord,
                              __global int *g_cell_size,
                              __global int *start_send,
                              int p4_offset,
                              int p5_offset,
                              int c,
                              int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int i, j, k, m, p4, p5;

  cell_size = (__global int (*)[3])g_cell_size;

#if COPY_FACES_OUT3_DIM == 2
  j = get_global_id(1) + 2;
  i = get_global_id(0) + 2;
  if (j >= cell_size[c][1]+2 || i >= cell_size[c][0]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;
  
  //---------------------------------------------------------------------
  // fill the buffer to be sent to top neighbors (k-dir)
  //---------------------------------------------------------------------
  if (cell_coord[c][2] != (ncells-1)) {
    p4 = p4_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (k = cell_size[c][2]; k < cell_size[c][2]+2; k++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[top]+p4] = u[c][k][j][i][m];
        p4 = p4 + 1;
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to bottom neighbors
  //---------------------------------------------------------------------
  if (cell_coord[c][2] != 0) {
    p5 = p5_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (k = 2; k <= 3; k++) {
      for (m = 0; m < 5; m++) {
        out_buffer[start_send[bottom]+p5] = u[c][k][j][i][m];
        p5 = p5 + 1;
      }
    }
  }

#else //COPY_FACES_OUT3_DIM == 1
  j = get_global_id(0) + 2;
  if (j >= cell_size[c][1]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;
  
  //---------------------------------------------------------------------
  // fill the buffer to be sent to top neighbors (k-dir)
  //---------------------------------------------------------------------
  if (cell_coord[c][2] != (ncells-1)) {
    p4 = p4_offset + 5 * 2 * cell_size[c][0] * get_global_id(0);

    for (k = cell_size[c][2]; k < cell_size[c][2]+2; k++) {
      for (i = 2; i < cell_size[c][0]+2; i++) {
        for (m = 0; m < 5; m++) {
          out_buffer[start_send[top]+p4] = u[c][k][j][i][m];
          p4 = p4 + 1;
        }
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to bottom neighbors
  //---------------------------------------------------------------------
  if (cell_coord[c][2] != 0) {
    p5 = p5_offset + 5 * 2 * cell_size[c][0] * get_global_id(0);

    for (k = 2; k <= 3; k++) {
      for (i = 2; i < cell_size[c][0]+2; i++) {
        for (m = 0; m < 5; m++) {
          out_buffer[start_send[bottom]+p5] = u[c][k][j][i][m];
          p5 = p5 + 1;
        }
      }
    }
  }
#endif
}
#endif


#if COPY_FACES_IN1_DIM == 3
__kernel void copy_faces_in1(__global double *g_u,
                             __global double *in_buffer,
                             __global int *g_cell_coord,
                             __global int *g_cell_size,
                             __global int *start_recv,
                             __global int *p1_offset,
                             __global int *p0_offset,
                             int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int c, i, j, k, m, p0, p1;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;

  k = get_global_id(1) + 2;
  j = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2 || j >= cell_size[c][1]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (cell_coord[c][0] != 0) {
    p0 = p0_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][1] + get_global_id(0));

    for (i = 0; i <= 1; i++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[west]+p0];
        p0 = p0 + 1;
      }
    }
  }

  if (cell_coord[c][0] != (ncells-1)) {
    p1 = p1_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][1] + get_global_id(0));

    for (i = cell_size[c][0]+2; i <= cell_size[c][0]+3; i++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[east]+p1];
        p1 = p1 + 1;
      }
    }
  }
}

#else

__kernel void copy_faces_in1(__global double *g_u,
                             __global double *in_buffer,
                             __global int *g_cell_coord,
                             __global int *g_cell_size,
                             __global int *start_recv,
                             int p1_offset,
                             int p0_offset,
                             int c,
                             int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int i, j, k, m, p0, p1;

  cell_size = (__global int (*)[3])g_cell_size;

#if COPY_FACES_IN1_DIM == 2
  k = get_global_id(1) + 2;
  j = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2 || j >= cell_size[c][1]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (cell_coord[c][0] != 0) {
    p0 = p0_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][1] + get_global_id(0));

    for (i = 0; i <= 1; i++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[west]+p0];
        p0 = p0 + 1;
      }
    }
  }

  if (cell_coord[c][0] != (ncells-1)) {
    p1 = p1_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][1] + get_global_id(0));

    for (i = cell_size[c][0]+2; i <= cell_size[c][0]+3; i++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[east]+p1];
        p1 = p1 + 1;
      }
    }
  }

#else //COPY_FACES_IN1_DIM == 1
  k = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (cell_coord[c][0] != 0) {
    p0 = p0_offset + 5 * 2 * cell_size[c][1] * get_global_id(0);

    for (j = 2; j < cell_size[c][1]+2; j++) {
      for (i = 0; i <= 1; i++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[start_recv[west]+p0];
          p0 = p0 + 1;
        }
      }
    }
  }

  if (cell_coord[c][0] != (ncells-1)) {
    p1 = p1_offset + 5 * 2 * cell_size[c][1] * get_global_id(0);

    for (j = 2; j < cell_size[c][1]+2; j++) {
      for (i = cell_size[c][0]+2; i <= cell_size[c][0]+3; i++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[start_recv[east]+p1];
          p1 = p1 + 1;
        }
      }
    }
  }
#endif
}
#endif


#if COPY_FACES_IN2_DIM == 3
__kernel void copy_faces_in2(__global double *g_u,
                             __global double *in_buffer,
                             __global int *g_cell_coord,
                             __global int *g_cell_size,
                             __global int *start_recv,
                             __global int *p3_offset,
                             __global int *p2_offset,
                             int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int c, i, j, k, m, p2, p3;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;

  k = get_global_id(1) + 2;
  i = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2 || i >= cell_size[c][0]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (cell_coord[c][1] != 0) {
    p2 = p2_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (j = 0; j <= 1; j++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[south]+p2];
        p2 = p2 + 1;
      }
    }
  }

  if (cell_coord[c][1] != (ncells-1)) {
    p3 = p3_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (j = cell_size[c][1]+2; j <= cell_size[c][1]+3; j++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[north]+p3];
        p3 = p3 + 1;
      }
    }
  }
}

#else

__kernel void copy_faces_in2(__global double *g_u,
                             __global double *in_buffer,
                             __global int *g_cell_coord,
                             __global int *g_cell_size,
                             __global int *start_recv,
                             int p3_offset,
                             int p2_offset,
                             int c,
                             int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int i, j, k, m, p2, p3;

  cell_size = (__global int (*)[3])g_cell_size;

#if COPY_FACES_IN2_DIM == 2
  k = get_global_id(1) + 2;
  i = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2 || i >= cell_size[c][0]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (cell_coord[c][1] != 0) {
    p2 = p2_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (j = 0; j <= 1; j++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[south]+p2];
        p2 = p2 + 1;
      }
    }
  }

  if (cell_coord[c][1] != (ncells-1)) {
    p3 = p3_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (j = cell_size[c][1]+2; j <= cell_size[c][1]+3; j++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[north]+p3];
        p3 = p3 + 1;
      }
    }
  }

#else //COPY_FACES_IN2_DIM == 1
  k = get_global_id(0) + 2;
  if (k >= cell_size[c][2]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (cell_coord[c][1] != 0) {
    p2 = p2_offset + 5 * 2 * cell_size[c][0] * get_global_id(0);

    for (j = 0; j <= 1; j++) {
      for (i = 2; i < cell_size[c][0]+2; i++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[start_recv[south]+p2];
          p2 = p2 + 1;
        }
      }
    }
  }

  if (cell_coord[c][1] != (ncells-1)) {
    p3 = p3_offset + 5 * 2 * cell_size[c][0] * get_global_id(0);

    for (j = cell_size[c][1]+2; j <= cell_size[c][1]+3; j++) {
      for (i = 2; i < cell_size[c][0]+2; i++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[start_recv[north]+p3];
          p3 = p3 + 1;
        }
      }
    }
  }
#endif
}
#endif


#if COPY_FACES_IN3_DIM == 3
__kernel void copy_faces_in3(__global double *g_u,
                             __global double *in_buffer,
                             __global int *g_cell_coord,
                             __global int *g_cell_size,
                             __global int *start_recv,
                             __global int *p5_offset,
                             __global int *p4_offset,
                             int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int c, i, j, k, m, p4, p5;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;

  j = get_global_id(1) + 2;
  i = get_global_id(0) + 2;
  if (j >= cell_size[c][1]+2 || i >= cell_size[c][0]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (cell_coord[c][2] != 0) {
    p4 = p4_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (k = 0; k <= 1; k++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[bottom]+p4];
        p4 = p4 + 1;
      }
    }
  }

  if (cell_coord[c][2] != (ncells-1)) {
    p5 = p5_offset[c]
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (k = cell_size[c][2]+2; k <= cell_size[c][2]+3; k++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[top]+p5];
        p5 = p5 + 1;
      }
    }
  }
}

#else

__kernel void copy_faces_in3(__global double *g_u,
                             __global double *in_buffer,
                             __global int *g_cell_coord,
                             __global int *g_cell_size,
                             __global int *start_recv,
                             int p5_offset,
                             int p4_offset,
                             int c,
                             int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int i, j, k, m, p4, p5;

  cell_size = (__global int (*)[3])g_cell_size;

#if COPY_FACES_IN3_DIM == 2
  j = get_global_id(1) + 2;
  i = get_global_id(0) + 2;
  if (j >= cell_size[c][1]+2 || i >= cell_size[c][0]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (cell_coord[c][2] != 0) {
    p4 = p4_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (k = 0; k <= 1; k++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[bottom]+p4];
        p4 = p4 + 1;
      }
    }
  }

  if (cell_coord[c][2] != (ncells-1)) {
    p5 = p5_offset
       + 5 * 2 * (get_global_id(1)*cell_size[c][0] + get_global_id(0));

    for (k = cell_size[c][2]+2; k <= cell_size[c][2]+3; k++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[start_recv[top]+p5];
        p5 = p5 + 1;
      }
    }
  }

#else //COPY_FACES_IN3_DIM == 1
  j = get_global_id(0) + 2;
  if (j >= cell_size[c][1]+2) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (cell_coord[c][2] != 0) {
    p4 = p4_offset + 5 * 2 * cell_size[c][0] * get_global_id(0);

    for (k = 0; k <= 1; k++) {
      for (i = 2; i < cell_size[c][0]+2; i++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[start_recv[bottom]+p4];
          p4 = p4 + 1;
        }
      }
    }
  }

  if (cell_coord[c][2] != (ncells-1)) {
    p5 = p5_offset + 5 * 2 * cell_size[c][0] * get_global_id(0);

    for (k = cell_size[c][2]+2; k <= cell_size[c][2]+3; k++) {
      for (i = 2; i < cell_size[c][0]+2; i++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[start_recv[top]+p5];
          p5 = p5 + 1;
        }
      }
    }
  }
#endif
}
#endif


//---------------------------------------------------------------------
// compute the reciprocal of density, and the kinetic energy, 
// and the speed of sound. 
//---------------------------------------------------------------------
__kernel void compute_rhs1(__global const double *g_u,
                           __global double *g_us,
                           __global double *g_vs,
                           __global double *g_ws,
                           __global double *g_qs,
                           __global double *g_rho_i,
                           __global double *g_square,
                           __global const int *g_cell_size,
                           int ncells)
{
  __global const double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global double (*us)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*vs)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*ws)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*square)[KMAX+2][JMAX+2][IMAX+2];
  __global const int (*cell_size)[3];

  int c, i, j, k;
  int ip1, jp1, kp1;
  double rho_inv;

#if COMPUTE_RHS1_DIM == 3
  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global const int (*)[3])g_cell_size;

  k = get_global_id(1);
  j = get_global_id(0);
  if (k > cell_size[c][2]+1 || j > cell_size[c][1]+1) return;

  u = (__global const double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  us = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  vs = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  ws = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  qs = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  rho_i = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  square = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_square;

  kp1 = k + 1;
  jp1 = j + 1;
  for (i = 0; i <= cell_size[c][0]+1; i++) {
    ip1 = i + 1;

    rho_inv = 1.0/u[c][kp1][jp1][ip1][0];
    rho_i[c][k][j][i] = rho_inv;
    us[c][k][j][i] = u[c][kp1][jp1][ip1][1] * rho_inv;
    vs[c][k][j][i] = u[c][kp1][jp1][ip1][2] * rho_inv;
    ws[c][k][j][i] = u[c][kp1][jp1][ip1][3] * rho_inv;
    square[c][k][j][i]     = 0.5* (
        u[c][kp1][jp1][ip1][1]*u[c][kp1][jp1][ip1][1] + 
        u[c][kp1][jp1][ip1][2]*u[c][kp1][jp1][ip1][2] +
        u[c][kp1][jp1][ip1][3]*u[c][kp1][jp1][ip1][3] ) * rho_inv;
    qs[c][k][j][i] = square[c][k][j][i] * rho_inv;
  }

#else //COMPUTE_RHS1_DIM == 2
  c = get_global_id(1);
  if (c >= ncells) return;

  cell_size = (__global const int (*)[3])g_cell_size;

  k = get_global_id(0);
  if (k > cell_size[c][2]+1) return;

  u = (__global const double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  us = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  vs = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  ws = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  qs = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  rho_i = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  square = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_square;

  kp1 = k + 1;
  for (j = 0; j <= cell_size[c][1]+1; j++) {
    jp1 = j + 1;
    for (i = 0; i <= cell_size[c][0]+1; i++) {
      ip1 = i + 1;

      rho_inv = 1.0/u[c][kp1][jp1][ip1][0];
      rho_i[c][k][j][i] = rho_inv;
      us[c][k][j][i] = u[c][kp1][jp1][ip1][1] * rho_inv;
      vs[c][k][j][i] = u[c][kp1][jp1][ip1][2] * rho_inv;
      ws[c][k][j][i] = u[c][kp1][jp1][ip1][3] * rho_inv;
      square[c][k][j][i]     = 0.5* (
          u[c][kp1][jp1][ip1][1]*u[c][kp1][jp1][ip1][1] + 
          u[c][kp1][jp1][ip1][2]*u[c][kp1][jp1][ip1][2] +
          u[c][kp1][jp1][ip1][3]*u[c][kp1][jp1][ip1][3] ) * rho_inv;
      qs[c][k][j][i] = square[c][k][j][i] * rho_inv;
    }
  }
#endif
}


//---------------------------------------------------------------------
// copy the exact forcing term to the right hand side;  because 
// this forcing term is known, we can store it on the whole of every 
// cell,  including the boundary                   
//---------------------------------------------------------------------
__kernel void compute_rhs2(__global const double *g_forcing,
                           __global double *g_rhs,
                           __global const int *g_cell_size,
                           int ncells)
{
  __global const double (*forcing)[KMAX][JMAX][IMAX][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global const int (*cell_size)[3];

  int c, i, j, k, m;
  
#if COMPUTE_RHS2_DIM == 3
  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global const int (*)[3])g_cell_size;

  k = get_global_id(1);
  j = get_global_id(0);
  if (k >= cell_size[c][2] || j >= cell_size[c][1]) return;

  forcing = (__global const double (*)[KMAX][JMAX][IMAX][5])g_forcing;
  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  for (i = 0; i < cell_size[c][0]; i++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k+1][j+1][i+1][m] = forcing[c][k][j][i][m];
    }
  }

#else //COMPUTE_RHS2_DIM == 2
  c = get_global_id(1);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;

  k = get_global_id(0);
  if (k >= cell_size[c][2]) return;

  forcing = (__global const double (*)[KMAX][JMAX][IMAX][5])g_forcing;
  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  for (j = 0; j < cell_size[c][1]; j++) {
    for (i = 0; i < cell_size[c][0]; i++) {
      for (m = 0; m < 5; m++) {
        rhs[c][k+1][j+1][i+1][m] = forcing[c][k][j][i][m];
      }
    }
  }
#endif
}


//---------------------------------------------------------------------
// compute xi-direction fluxes 
//---------------------------------------------------------------------
__kernel void compute_rhs3(__global const double *g_u,
                           __global const double *g_us,
                           __global const double *g_vs,
                           __global const double *g_ws,
                           __global const double *g_qs,
                           __global const double *g_rho_i,
                           __global const double *g_square,
                           __global double *g_rhs,
                           __global const int *g_cell_size,
                           __global const int *g_start,
                           __global const int *g_end,
                           int ncells)
{
  __global const double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global const double (*us)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*vs)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*ws)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*qs)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*rho_i)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*square)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global const int (*cell_size)[3];
  __global const int (*start)[3];
  __global const int (*end)[3];

  int c, i, j, k, m, ip1, ip2, jp1, kp1;
  double uijk, up1, um1;

  c = get_global_id(2);
  if (c >= ncells) return;
  
  cell_size = (__global const int (*)[3])g_cell_size;
  start = (__global const int (*)[3])g_start;
  end   = (__global const int (*)[3])g_end;

  k = get_global_id(1) + start[c][2]+1;
  j = get_global_id(0) + start[c][1]+1;
  if (k > cell_size[c][2]-end[c][2] || j > cell_size[c][1]-end[c][1]) return;

  u  = (__global const double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  us = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  vs = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  ws = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  qs = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  rho_i = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  square = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_square;
  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  kp1 = k + 1;
  jp1 = j + 1;
  for (i = start[c][0]+1; i <= cell_size[c][0]-end[c][0]; i++) {
    ip1 = i + 1;
    ip2 = i + 2;

    uijk = us[c][k][j][i];
    up1  = us[c][k][j][i+1];
    um1  = us[c][k][j][i-1];

    rhs[c][k][j][i][0] = rhs[c][k][j][i][0] + dx1tx1 * 
      (u[c][kp1][jp1][ip2][0] - 2.0*u[c][kp1][jp1][ip1][0] + 
       u[c][kp1][jp1][i][0]) -
      tx2 * (u[c][kp1][jp1][ip2][1] - u[c][kp1][jp1][i][1]);

    rhs[c][k][j][i][1] = rhs[c][k][j][i][1] + dx2tx1 * 
      (u[c][kp1][jp1][ip2][1] - 2.0*u[c][kp1][jp1][ip1][1] + 
       u[c][kp1][jp1][i][1]) +
      xxcon2*con43 * (up1 - 2.0*uijk + um1) -
      tx2 * (u[c][kp1][jp1][ip2][1]*up1 - 
          u[c][kp1][jp1][i][1]*um1 +
          (u[c][kp1][jp1][ip2][4]- square[c][k][j][i+1]-
           u[c][kp1][jp1][i][4]+ square[c][k][j][i-1])*
          c2);

    rhs[c][k][j][i][2] = rhs[c][k][j][i][2] + dx3tx1 * 
      (u[c][kp1][jp1][ip2][2] - 2.0*u[c][kp1][jp1][ip1][2] +
       u[c][kp1][jp1][i][2]) +
      xxcon2 * (vs[c][k][j][i+1] - 2.0*vs[c][k][j][i] +
          vs[c][k][j][i-1]) -
      tx2 * (u[c][kp1][jp1][ip2][2]*up1 - 
          u[c][kp1][jp1][i][2]*um1);

    rhs[c][k][j][i][3] = rhs[c][k][j][i][3] + dx4tx1 * 
      (u[c][kp1][jp1][ip2][3] - 2.0*u[c][kp1][jp1][ip1][3] +
       u[c][kp1][jp1][i][3]) +
      xxcon2 * (ws[c][k][j][i+1] - 2.0*ws[c][k][j][i] +
          ws[c][k][j][i-1]) -
      tx2 * (u[c][kp1][jp1][ip2][3]*up1 - 
          u[c][kp1][jp1][i][3]*um1);

    rhs[c][k][j][i][4] = rhs[c][k][j][i][4] + dx5tx1 * 
      (u[c][kp1][jp1][ip2][4] - 2.0*u[c][kp1][jp1][ip1][4] +
       u[c][kp1][jp1][i][4]) +
      xxcon3 * (qs[c][k][j][i+1] - 2.0*qs[c][k][j][i] +
          qs[c][k][j][i-1]) +
      xxcon4 * (up1*up1 -       2.0*uijk*uijk + 
          um1*um1) +
      xxcon5 * (u[c][kp1][jp1][ip2][4]*rho_i[c][k][j][i+1] - 
          2.0*u[c][kp1][jp1][ip1][4]*rho_i[c][k][j][i] +
          u[c][kp1][jp1][i][4]*rho_i[c][k][j][i-1]) -
      tx2 * ( (c1*u[c][kp1][jp1][ip2][4] - 
            c2*square[c][k][j][i+1])*up1 -
          (c1*u[c][kp1][jp1][i][4] - 
           c2*square[c][k][j][i-1])*um1 );
  }

  //---------------------------------------------------------------------
  // add fourth order xi-direction dissipation               
  //---------------------------------------------------------------------
  if (start[c][0] > 0) {
    i = 2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m]- dssp * 
        ( 5.0*u[c][kp1][jp1][i+1][m] - 4.0*u[c][kp1][jp1][i+2][m] +
          u[c][kp1][jp1][i+3][m]);
    }

    i = 3;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (-4.0*u[c][kp1][jp1][i][m] + 6.0*u[c][kp1][jp1][i+1][m] -
         4.0*u[c][kp1][jp1][i+2][m] + u[c][kp1][jp1][i+3][m]);
    }
  }

  for (i = 3*start[c][0]+1; i <= cell_size[c][0]-3*end[c][0]; i++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (  u[c][kp1][jp1][i-1][m] - 4.0*u[c][kp1][jp1][i][m] + 
           6.0*u[c][kp1][jp1][i+1][m] - 4.0*u[c][kp1][jp1][i+2][m] + 
           u[c][kp1][jp1][i+3][m] );
    }
  }

  if (end[c][0] > 0) {
    i = cell_size[c][0]-2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][kp1][jp1][i-1][m] - 4.0*u[c][kp1][jp1][i][m] + 
          6.0*u[c][kp1][jp1][i+1][m] - 4.0*u[c][kp1][jp1][i+2][m] );
    }

    i = cell_size[c][0]-1;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][kp1][jp1][i-1][m] - 4.0*u[c][kp1][jp1][i][m] +
          5.0*u[c][kp1][jp1][i+1][m] );
    }
  }
}


//---------------------------------------------------------------------
// compute eta-direction fluxes 
//---------------------------------------------------------------------
__kernel void compute_rhs4(__global const double *g_u,
                           __global const double *g_us,
                           __global const double *g_vs,
                           __global const double *g_ws,
                           __global const double *g_qs,
                           __global const double *g_rho_i,
                           __global const double *g_square,
                           __global double *g_rhs,
                           __global const int *g_cell_size,
                           __global const int *g_start,
                           __global const int *g_end,
                           int ncells)
{
  __global const double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global const double (*us)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*vs)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*ws)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*qs)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*rho_i)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*square)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global const int (*cell_size)[3];
  __global const int (*start)[3];
  __global const int (*end)[3];

  int c, i, j, k, m, ip1, jp1, jp2, kp1;
  double vijk, vp1, vm1;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global const int (*)[3])g_cell_size;
  start = (__global const int (*)[3])g_start;
  end   = (__global const int (*)[3])g_end;

  k = get_global_id(1) + start[c][2]+1;
  i = get_global_id(0) + start[c][0]+1;
  if (k > cell_size[c][2]-end[c][2] || i > cell_size[c][0]-end[c][0]) return;

  u  = (__global const double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  us = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  vs = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  ws = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  qs = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  rho_i = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  square = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_square;
  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  kp1 = k + 1;
  ip1 = i +1;
  for (j = start[c][1]+1; j <= cell_size[c][1]-end[c][1]; j++) {
    jp1 = j + 1;
    jp2 = j + 2;

    vijk = vs[c][k][j][i];
    vp1  = vs[c][k][j+1][i];
    vm1  = vs[c][k][j-1][i];
    rhs[c][k][j][i][0] = rhs[c][k][j][i][0] + dy1ty1 * 
      (u[c][kp1][jp2][ip1][0] - 2.0*u[c][kp1][jp1][ip1][0] + 
       u[c][kp1][j][ip1][0]) -
      ty2 * (u[c][kp1][jp2][ip1][2] - u[c][kp1][j][ip1][2]);
    rhs[c][k][j][i][1] = rhs[c][k][j][i][1] + dy2ty1 * 
      (u[c][kp1][jp2][ip1][1] - 2.0*u[c][kp1][jp1][ip1][1] + 
       u[c][kp1][j][ip1][1]) +
      yycon2 * (us[c][k][j+1][i] - 2.0*us[c][k][j][i] + 
          us[c][k][j-1][i]) -
      ty2 * (u[c][kp1][jp2][ip1][1]*vp1 - 
          u[c][kp1][j][ip1][1]*vm1);
    rhs[c][k][j][i][2] = rhs[c][k][j][i][2] + dy3ty1 * 
      (u[c][kp1][jp2][ip1][2] - 2.0*u[c][kp1][jp1][ip1][2] + 
       u[c][kp1][j][ip1][2]) +
      yycon2*con43 * (vp1 - 2.0*vijk + vm1) -
      ty2 * (u[c][kp1][jp2][ip1][2]*vp1 - 
          u[c][kp1][j][ip1][2]*vm1 +
          (u[c][kp1][jp2][ip1][4] - square[c][k][j+1][i] - 
           u[c][kp1][j][ip1][4] + square[c][k][j-1][i])
          *c2);
    rhs[c][k][j][i][3] = rhs[c][k][j][i][3] + dy4ty1 * 
      (u[c][kp1][jp2][ip1][3] - 2.0*u[c][kp1][jp1][ip1][3] + 
       u[c][kp1][j][ip1][3]) +
      yycon2 * (ws[c][k][j+1][i] - 2.0*ws[c][k][j][i] + 
          ws[c][k][j-1][i]) -
      ty2 * (u[c][kp1][jp2][ip1][3]*vp1 - 
          u[c][kp1][j][ip1][3]*vm1);
    rhs[c][k][j][i][4] = rhs[c][k][j][i][4] + dy5ty1 * 
      (u[c][kp1][jp2][ip1][4] - 2.0*u[c][kp1][jp1][ip1][4] + 
       u[c][kp1][j][ip1][4]) +
      yycon3 * (qs[c][k][j+1][i] - 2.0*qs[c][k][j][i] + 
          qs[c][k][j-1][i]) +
      yycon4 * (vp1*vp1       - 2.0*vijk*vijk + 
          vm1*vm1) +
      yycon5 * (u[c][kp1][jp2][ip1][4]*rho_i[c][k][j+1][i] - 
          2.0*u[c][kp1][jp1][ip1][4]*rho_i[c][k][j][i] +
          u[c][kp1][j][ip1][4]*rho_i[c][k][j-1][i]) -
      ty2 * ((c1*u[c][kp1][jp2][ip1][4] - 
            c2*square[c][k][j+1][i]) * vp1 -
          (c1*u[c][kp1][j][ip1][4] - 
           c2*square[c][k][j-1][i]) * vm1);
  }

  //---------------------------------------------------------------------
  // add fourth order eta-direction dissipation         
  //---------------------------------------------------------------------
  if (start[c][1] > 0) {
    j = 2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m]- dssp * 
        ( 5.0*u[c][kp1][j+1][ip1][m] - 4.0*u[c][kp1][j+2][ip1][m] +
          u[c][kp1][j+3][ip1][m]);
    }

    j = 3;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (-4.0*u[c][kp1][j][ip1][m] + 6.0*u[c][kp1][j+1][ip1][m] -
         4.0*u[c][kp1][j+2][ip1][m] + u[c][kp1][j+3][ip1][m]);
    }
  }

  for (j = 3*start[c][1]+1; j <= cell_size[c][1]-3*end[c][1]; j++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (  u[c][kp1][j-1][ip1][m] - 4.0*u[c][kp1][j][ip1][m] + 
           6.0*u[c][kp1][j+1][ip1][m] - 4.0*u[c][kp1][j+2][ip1][m] + 
           u[c][kp1][j+3][ip1][m] );
    }
  }

  if (end[c][1] > 0) {
    j = cell_size[c][1]-2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][kp1][j-1][ip1][m] - 4.0*u[c][kp1][j][ip1][m] + 
          6.0*u[c][kp1][j+1][ip1][m] - 4.0*u[c][kp1][j+2][ip1][m] );
    }

    j = cell_size[c][1]-1;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][kp1][j-1][ip1][m] - 4.0*u[c][kp1][j][ip1][m] +
          5.0*u[c][kp1][j+1][ip1][m] );
    }
  }
}


//---------------------------------------------------------------------
// compute zeta-direction fluxes 
//---------------------------------------------------------------------
__kernel void compute_rhs5(__global const double *g_u,
                           __global const double *g_us,
                           __global const double *g_vs,
                           __global const double *g_ws,
                           __global const double *g_qs,
                           __global const double *g_rho_i,
                           __global const double *g_square,
                           __global double *g_rhs,
                           __global const int *g_cell_size,
                           __global const int *g_start,
                           __global const int *g_end,
                           int ncells)
{
  __global const double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global const double (*us)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*vs)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*ws)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*qs)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*rho_i)[KMAX+2][JMAX+2][IMAX+2];
  __global const double (*square)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global const int (*cell_size)[3];
  __global const int (*start)[3];
  __global const int (*end)[3];

  int c, i, j, k, m, ip1, jp1, kp1, kp2;
  double wijk, wp1, wm1;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global const int (*)[3])g_cell_size;
  start = (__global const int (*)[3])g_start;
  end   = (__global const int (*)[3])g_end;

  j = get_global_id(1) + start[c][1]+1;
  i = get_global_id(0) + start[c][0]+1;
  if (j > cell_size[c][1]-end[c][1] || i > cell_size[c][0]-end[c][0]) return;

  u  = (__global const double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  us = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  vs = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  ws = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  qs = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  rho_i = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  square = (__global const double (*)[KMAX+2][JMAX+2][IMAX+2])g_square;
  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  jp1 = j + 1;
  ip1 = i + 1;
  for (k = start[c][2]+1; k <= cell_size[c][2]-end[c][2]; k++) {
    kp1 = k + 1;
    kp2 = k + 2;

    wijk = ws[c][k][j][i];
    wp1  = ws[c][k+1][j][i];
    wm1  = ws[c][k-1][j][i];

    rhs[c][k][j][i][0] = rhs[c][k][j][i][0] + dz1tz1 * 
      (u[c][kp2][jp1][ip1][0] - 2.0*u[c][kp1][jp1][ip1][0] + 
       u[c][k][jp1][ip1][0]) -
      tz2 * (u[c][kp2][jp1][ip1][3] - u[c][k][jp1][ip1][3]);
    rhs[c][k][j][i][1] = rhs[c][k][j][i][1] + dz2tz1 * 
      (u[c][kp2][jp1][ip1][1] - 2.0*u[c][kp1][jp1][ip1][1] + 
       u[c][k][jp1][ip1][1]) +
      zzcon2 * (us[c][k+1][j][i] - 2.0*us[c][k][j][i] + 
          us[c][k-1][j][i]) -
      tz2 * (u[c][kp2][jp1][ip1][1]*wp1 - 
          u[c][k][jp1][ip1][1]*wm1);
    rhs[c][k][j][i][2] = rhs[c][k][j][i][2] + dz3tz1 * 
      (u[c][kp2][jp1][ip1][2] - 2.0*u[c][kp1][jp1][ip1][2] + 
       u[c][k][jp1][ip1][2]) +
      zzcon2 * (vs[c][k+1][j][i] - 2.0*vs[c][k][j][i] + 
          vs[c][k-1][j][i]) -
      tz2 * (u[c][kp2][jp1][ip1][2]*wp1 - 
          u[c][k][jp1][ip1][2]*wm1);
    rhs[c][k][j][i][3] = rhs[c][k][j][i][3] + dz4tz1 * 
      (u[c][kp2][jp1][ip1][3] - 2.0*u[c][kp1][jp1][ip1][3] + 
       u[c][k][jp1][ip1][3]) +
      zzcon2*con43 * (wp1 - 2.0*wijk + wm1) -
      tz2 * (u[c][kp2][jp1][ip1][3]*wp1 - 
          u[c][k][jp1][ip1][3]*wm1 +
          (u[c][kp2][jp1][ip1][4] - square[c][k+1][j][i] - 
           u[c][k][jp1][ip1][4] + square[c][k-1][j][i])
          *c2);
    rhs[c][k][j][i][4] = rhs[c][k][j][i][4] + dz5tz1 * 
      (u[c][kp2][jp1][ip1][4] - 2.0*u[c][kp1][jp1][ip1][4] + 
       u[c][k][jp1][ip1][4]) +
      zzcon3 * (qs[c][k+1][j][i] - 2.0*qs[c][k][j][i] + 
          qs[c][k-1][j][i]) +
      zzcon4 * (wp1*wp1 - 2.0*wijk*wijk + 
          wm1*wm1) +
      zzcon5 * (u[c][kp2][jp1][ip1][4]*rho_i[c][k+1][j][i] - 
          2.0*u[c][kp1][jp1][ip1][4]*rho_i[c][k][j][i] +
          u[c][k][jp1][ip1][4]*rho_i[c][k-1][j][i]) -
      tz2 * ( (c1*u[c][kp2][jp1][ip1][4] - 
            c2*square[c][k+1][j][i])*wp1 -
          (c1*u[c][k][jp1][ip1][4] - 
           c2*square[c][k-1][j][i])*wm1);
  }

  //---------------------------------------------------------------------
  // add fourth order zeta-direction dissipation                
  //---------------------------------------------------------------------
  if (start[c][2] > 0) {
    k = 2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m]- dssp * 
        ( 5.0*u[c][k+1][jp1][ip1][m] - 4.0*u[c][k+2][jp1][ip1][m] +
          u[c][k+3][jp1][ip1][m]);
    }

    k = 3;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (-4.0*u[c][k][jp1][ip1][m] + 6.0*u[c][k+1][jp1][ip1][m] -
         4.0*u[c][k+2][jp1][ip1][m] + u[c][k+3][jp1][ip1][m]);
    }
  }

  for (k = 3*start[c][2]+1; k <= cell_size[c][2]-3*end[c][2]; k++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (  u[c][k-1][jp1][ip1][m] - 4.0*u[c][k][jp1][ip1][m] + 
           6.0*u[c][k+1][jp1][ip1][m] - 4.0*u[c][k+2][jp1][ip1][m] + 
           u[c][k+3][jp1][ip1][m] );
    }
  }

  if (end[c][2] > 0) {
    k = cell_size[c][2]-2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][k-1][jp1][ip1][m] - 4.0*u[c][k][jp1][ip1][m] + 
          6.0*u[c][k+1][jp1][ip1][m] - 4.0*u[c][k+2][jp1][ip1][m] );
    }

    k = cell_size[c][2]-1;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][k-1][jp1][ip1][m] - 4.0*u[c][k][jp1][ip1][m] +
          5.0*u[c][k+1][jp1][ip1][m] );
    }
  }
}


__kernel void compute_rhs6(__global double *g_rhs,
                           __global const int *g_cell_size,
                           __global const int *g_start,
                           __global const int *g_end,
                           int ncells)
{
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global const int (*cell_size)[3];
  __global const int (*start)[3];
  __global const int (*end)[3];

  int c, i, j, k, m;

#if COMPUTE_RHS6_DIM == 3
  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global const int (*)[3])g_cell_size;
  start = (__global const int (*)[3])g_start;
  end   = (__global const int (*)[3])g_end;

  k = get_global_id(1) + start[c][2]+1;
  j = get_global_id(0) + start[c][1]+1;
  if (k > cell_size[c][2]-end[c][2] || j > cell_size[c][1]-end[c][1]) return;

  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  for (i = start[c][0]+1; i <= cell_size[c][0]-end[c][0]; i++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] * dt;
    }
  }

#else //COMPUTE_RHS6_DIM == 2
  c = get_global_id(1);
  if (c >= ncells) return;

  cell_size = (__global const int (*)[3])g_cell_size;
  start = (__global const int (*)[3])g_start;
  end   = (__global const int (*)[3])g_end;

  k = get_global_id(0) + start[c][2]+1;
  if (k > cell_size[c][2]-end[c][2]) return;

  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  for (j = start[c][1]+1; j <= cell_size[c][1]-end[c][1]; j++) {
    for (i = start[c][0]+1; i <= cell_size[c][0]-end[c][0]; i++) {
      for (m = 0; m < 5; m++) {
        rhs[c][k][j][i][m] = rhs[c][k][j][i][m] * dt;
      }
    }
  }
#endif
}


__kernel void add(__global double *g_u,
                  __global double *g_rhs,
                  __global const int *g_cell_size,
                  __global const int *g_start,
                  __global const int *g_end,
                  int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global const int (*cell_size)[3];
  __global const int (*start)[3];
  __global const int (*end)[3];

  int c, i, j, k, m;

#if ADD_DIM == 3
  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global const int (*)[3])g_cell_size;
  start = (__global const int (*)[3])g_start;
  end   = (__global const int (*)[3])g_end;

  k = get_global_id(1) + start[c][2]+1;
  if (k >= cell_size[c][2]-end[c][2]+1) return;

  j = get_global_id(0) + start[c][1]+1;
  if (j >= cell_size[c][1]-end[c][1]+1) return;

  u   = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  for (i = start[c][0]+1; i < cell_size[c][0]-end[c][0]+1; i++) {
    for (m = 0; m < 5; m++) {
      u[c][k+1][j+1][i+1][m] += rhs[c][k][j][i][m];
    }
  }

#else //ADD_DIM == 2

  c = get_global_id(1);
  if (c >= ncells) return;

  cell_size = (__global const int (*)[3])g_cell_size;
  start = (__global const int (*)[3])g_start;
  end   = (__global const int (*)[3])g_end;

  k = get_global_id(0) + start[c][2]+1;
  if (k >= cell_size[c][2]-end[c][2]+1) return;

  u   = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  for (j = start[c][1]+1; j < cell_size[c][1]-end[c][1]+1; j++) {
    for (i = start[c][0]+1; i < cell_size[c][0]-end[c][0]+1; i++) {
      for (m = 0; m < 5; m++) {
        u[c][k+1][j+1][i+1][m] += rhs[c][k][j][i][m];
      }
    }
  }
#endif
}

// FIXME: Current version is the task version.
__kernel void error_norm(__global double *g_u,
                         __global const double *g_ce,
                         __global double *rms,
                         __global int *g_cell_low,
                         __global int *g_cell_high,
                         int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_low)[3];
  __global int (*cell_high)[3];

  int c, i, j, k, m, ii, jj, kk;
  double xi, eta, zeta, u_exact[5], rms_work[5], add;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_low  = (__global int (*)[3])g_cell_low;
  cell_high = (__global int (*)[3])g_cell_high;

  for (m = 0; m < 5; m++) {
    rms_work[m] = 0.0;
  }

  for (c = 0; c < ncells; c++) {
    kk = 2;
    for (k = cell_low[c][2]; k <= cell_high[c][2]; k++) {
      zeta = (double)(k) * dnzm1;
      jj = 2;
      for (j = cell_low[c][1]; j <= cell_high[c][1]; j++) {
        eta = (double)(j) * dnym1;
        ii = 2;
        for (i = cell_low[c][0]; i <= cell_high[c][0]; i++) {
          xi = (double)(i) * dnxm1;
          exact_solution(xi, eta, zeta, u_exact, g_ce);

          for (m = 0; m < 5; m++) {
            add = u[c][kk][jj][ii][m]-u_exact[m];
            rms_work[m] = rms_work[m] + add*add;
          }
          ii = ii + 1;
        }
        jj = jj + 1;
      }
      kk = kk + 1;
    }
  }

  for (m = 0; m < 5; m++) {
    rms[m] = rms_work[m];
  }
}


__kernel void rhs_norm(__global double *g_rhs,
                       __global double *rms,
                       __global int *g_cell_size,
                       __global int *g_start,
                       __global int *g_end,
                       int ncells)
{
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global int (*cell_size)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  int c, i, j, k, m;
  double rms_work[5], add;

  rhs = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

  for (m = 0; m < 5; m++) {
    rms_work[m] = 0.0;
  }

  for (c = 0; c < ncells; c++) {
    for (k = start[c][2]+1; k < cell_size[c][2]-end[c][2]+1; k++) {
      for (j = start[c][1]+1; j < cell_size[c][1]-end[c][1]+1; j++) {
        for (i = start[c][0]+1; i < cell_size[c][0]-end[c][0]+1; i++) {
          for (m = 0; m < 5; m++) {
            add = rhs[c][k][j][i][m];
            rms_work[m] = rms_work[m] + add*add;
          }
        }
      }
    }
  }

  for (m = 0; m < 5; m++) {
    rms[m] = rms_work[m];
  }
}


//---------------------------------------------------------------------
// initialize                                  
//---------------------------------------------------------------------
__kernel void exact_rhs1(__global double *g_forcing,
                         __global int *g_cell_size,
                         int ncells)
{
  __global double (*forcing)[KMAX][JMAX][IMAX][5];
  __global int (*cell_size)[3];

  int c, m, i, j, k;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;

  k = get_global_id(1);
  j = get_global_id(0);
  if (k >= cell_size[c][2] || j >= cell_size[c][1]) return;

  forcing = (__global double (*)[KMAX][JMAX][IMAX][5])g_forcing;

  for (i = 0; i < cell_size[c][0]; i++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = 0.0;
    }
  }
}


//---------------------------------------------------------------------
// xi-direction flux differences                      
//---------------------------------------------------------------------
__kernel void exact_rhs2(__global double *g_forcing,
                         __global double *g_ue,
                         __global double *g_buf,
                         __global double *g_cuf,
                         __global double *g_q,
                         __global const double *g_ce,
                         __global int *g_cell_size,
                         __global int *g_cell_low,
                         __global int *g_start,
                         __global int *g_end,
                         int ncells)
{
  __global double (*forcing)[KMAX][JMAX][IMAX][5];
  __global double (*ue)[5];
  __global double (*buf)[5];
  __global double *cuf;
  __global double *q;
  __global int (*cell_size)[3];
  __global int (*cell_low)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  double dtemp[5], xi, eta, zeta, dtpp;
  int c, m, i, j, k;
  int ip1, ip2, ip3;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

  k = get_global_id(1) + start[c][2];
  j = get_global_id(0) + start[c][1];

  if (k >= (cell_size[c][2]-end[c][2]) ||
      j >= (cell_size[c][1]-end[c][1])) return;

  forcing = (__global double (*)[KMAX][JMAX][IMAX][5])g_forcing;
  cell_low = (__global int (*)[3])g_cell_low;

  int d0_size = cell_size[c][1] - end[c][1] - start[c][1];
  int d1_size = cell_size[c][2] - end[c][2] - start[c][2];
  int my_id = c*d1_size*d0_size + get_global_id(1)*d0_size + get_global_id(0);
  int my_offset1 = my_id * (MAX_CELL_DIM+4) * 5;
  int my_offset2 = my_id * (MAX_CELL_DIM+4);
  ue  = (__global double (*)[5])&g_ue[my_offset1];
  buf = (__global double (*)[5])&g_buf[my_offset1];
  cuf = (__global double *)&g_cuf[my_offset2];
  q   = (__global double *)&g_q[my_offset2];

  zeta = (double)(k+cell_low[c][2]) * dnzm1;
  eta = (double)(j+cell_low[c][1]) * dnym1;

  for (i=2*start[c][0]; i <= cell_size[c][0]+3-2*end[c][0]; i++) {
    xi = (double)(i+cell_low[c][0]-2) * dnxm1;

    exact_solution(xi, eta, zeta, dtemp, g_ce);
    for (m = 0; m < 5; m++) {
      ue[i][m] = dtemp[m];
    }

    dtpp = 1.0 / dtemp[0];

    for (m = 1; m < 5; m++) {
      buf[i][m] = dtpp * dtemp[m];
    }

    cuf[i]    = buf[i][1] * buf[i][1];
    buf[i][0] = cuf[i] + buf[i][2] * buf[i][2] + buf[i][3] * buf[i][3];
    q[i] = 0.5*(buf[i][1]*ue[i][1] + buf[i][2]*ue[i][2] +
        buf[i][3]*ue[i][3]);
  }

  for (i = start[c][0]; i < cell_size[c][0]-end[c][0]; i++) {
    ip1 = i+1;
    ip2 = i+2;
    ip3 = i+3;

    forcing[c][k][j][i][0] = forcing[c][k][j][i][0] -
      tx2*( ue[ip3][1]-ue[ip1][1] )+
      dx1tx1*(ue[ip3][0]-2.0*ue[ip2][0]+ue[ip1][0]);

    forcing[c][k][j][i][1] = forcing[c][k][j][i][1] - tx2 * (
        (ue[ip3][1]*buf[ip3][1]+c2*(ue[ip3][4]-q[ip3]))-
        (ue[ip1][1]*buf[ip1][1]+c2*(ue[ip1][4]-q[ip1])))+
      xxcon1*(buf[ip3][1]-2.0*buf[ip2][1]+buf[ip1][1])+
      dx2tx1*( ue[ip3][1]-2.0* ue[ip2][1]+ue[ip1][1]);

    forcing[c][k][j][i][2] = forcing[c][k][j][i][2] - tx2 * (
        ue[ip3][2]*buf[ip3][1]-ue[ip1][2]*buf[ip1][1])+
      xxcon2*(buf[ip3][2]-2.0*buf[ip2][2]+buf[ip1][2])+
      dx3tx1*( ue[ip3][2]-2.0*ue[ip2][2] +ue[ip1][2]);

    forcing[c][k][j][i][3] = forcing[c][k][j][i][3] - tx2*(
        ue[ip3][3]*buf[ip3][1]-ue[ip1][3]*buf[ip1][1])+
      xxcon2*(buf[ip3][3]-2.0*buf[ip2][3]+buf[ip1][3])+
      dx4tx1*( ue[ip3][3]-2.0* ue[ip2][3]+ ue[ip1][3]);

    forcing[c][k][j][i][4] = forcing[c][k][j][i][4] - tx2*(
        buf[ip3][1]*(c1*ue[ip3][4]-c2*q[ip3])-
        buf[ip1][1]*(c1*ue[ip1][4]-c2*q[ip1]))+
      0.5*xxcon3*(buf[ip3][0]-2.0*buf[ip2][0]+
          buf[ip1][0])+
      xxcon4*(cuf[ip3]-2.0*cuf[ip2]+cuf[ip1])+
      xxcon5*(buf[ip3][4]-2.0*buf[ip2][4]+buf[ip1][4])+
      dx5tx1*( ue[ip3][4]-2.0* ue[ip2][4]+ ue[ip1][4]);
  }

  //-------------------------------------------------------------------
  // Fourth-order dissipation                         
  //-------------------------------------------------------------------
  if (start[c][0] > 0) {
    i = 1;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (5.0*ue[i+2][m] - 4.0*ue[i+3][m] +ue[i+4][m]);
    }

    i = 2;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (-4.0*ue[i+1][m] + 6.0*ue[i+2][m] -
         4.0*ue[i+3][m] +     ue[i+4][m]);
    }
  }

  for (i = start[c][0]*3; i < cell_size[c][0]-3*end[c][0]; i++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp*
        (ue[i][m] - 4.0*ue[i+1][m] +
         6.0*ue[i+2][m] - 4.0*ue[i+3][m] + ue[i+4][m]);
    }
  }

  if (end[c][0] > 0) {
    i = cell_size[c][0]-3;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[i][m] - 4.0*ue[i+1][m] + 6.0*ue[i+2][m] - 4.0*ue[i+3][m]);
    }

    i = cell_size[c][0]-2;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[i][m] - 4.0*ue[i+1][m] + 5.0*ue[i+2][m]);
    }
  }

}


//---------------------------------------------------------------------
// eta-direction flux differences             
//---------------------------------------------------------------------
__kernel void exact_rhs3(__global double *g_forcing,
                         __global double *g_ue,
                         __global double *g_buf,
                         __global double *g_cuf,
                         __global double *g_q,
                         __global const double *g_ce,
                         __global int *g_cell_size,
                         __global int *g_cell_low,
                         __global int *g_start,
                         __global int *g_end,
                         int ncells)
{
  __global double (*forcing)[KMAX][JMAX][IMAX][5];
  __global double (*ue)[5];
  __global double (*buf)[5];
  __global double *cuf;
  __global double *q;
  __global int (*cell_size)[3];
  __global int (*cell_low)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  double dtemp[5], xi, eta, zeta, dtpp;
  int c, m, i, j, k;
  int jp1, jp2, jp3;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

  k = get_global_id(1) + start[c][2];
  i = get_global_id(0) + start[c][0];

  if (k >= (cell_size[c][2]-end[c][2]) ||
      i >= (cell_size[c][0]-end[c][0])) return;

  forcing = (__global double (*)[KMAX][JMAX][IMAX][5])g_forcing;
  cell_low = (__global int (*)[3])g_cell_low;

  int d0_size = cell_size[c][0] - end[c][0] - start[c][0];
  int d1_size = cell_size[c][2] - end[c][2] - start[c][2];
  int my_id = c*d1_size*d0_size + get_global_id(1)*d0_size + get_global_id(0);
  int my_offset1 = my_id * (MAX_CELL_DIM+4) * 5;
  int my_offset2 = my_id * (MAX_CELL_DIM+4);
  ue  = (__global double (*)[5])&g_ue[my_offset1];
  buf = (__global double (*)[5])&g_buf[my_offset1];
  cuf = (__global double *)&g_cuf[my_offset2];
  q   = (__global double *)&g_q[my_offset2];

  zeta = (double)(k+cell_low[c][2]) * dnzm1;
  xi = (double)(i+cell_low[c][0]) * dnxm1;

  for (j=2*start[c][1]; j <= cell_size[c][1]+3-2*end[c][1]; j++) {
    eta = (double)(j+cell_low[c][1]-2) * dnym1;

    exact_solution(xi, eta, zeta, dtemp, g_ce);
    for (m = 0; m < 5; m++) {
      ue[j][m] = dtemp[m];
    }
    dtpp = 1.0/dtemp[0];

    for (m = 1; m < 5; m++) {
      buf[j][m] = dtpp * dtemp[m];
    }

    cuf[j]    = buf[j][2] * buf[j][2];
    buf[j][0] = cuf[j] + buf[j][1] * buf[j][1] + buf[j][3] * buf[j][3];
    q[j] = 0.5*(buf[j][1]*ue[j][1] + buf[j][2]*ue[j][2] +
        buf[j][3]*ue[j][3]);
  }

  for (j = start[c][1]; j < cell_size[c][1]-end[c][1]; j++) {
    jp1 = j+1;
    jp2 = j+2;
    jp3 = j+3;

    forcing[c][k][j][i][0] = forcing[c][k][j][i][0] -
      ty2*( ue[jp3][2]-ue[jp1][2] )+
      dy1ty1*(ue[jp3][0]-2.0*ue[jp2][0]+ue[jp1][0]);

    forcing[c][k][j][i][1] = forcing[c][k][j][i][1] - ty2*(
        ue[jp3][1]*buf[jp3][2]-ue[jp1][1]*buf[jp1][2])+
      yycon2*(buf[jp3][1]-2.0*buf[jp2][1]+buf[jp1][1])+
      dy2ty1*( ue[jp3][1]-2.0* ue[jp2][1]+ ue[jp1][1]);

    forcing[c][k][j][i][2] = forcing[c][k][j][i][2] - ty2*(
        (ue[jp3][2]*buf[jp3][2]+c2*(ue[jp3][4]-q[jp3]))-
        (ue[jp1][2]*buf[jp1][2]+c2*(ue[jp1][4]-q[jp1])))+
      yycon1*(buf[jp3][2]-2.0*buf[jp2][2]+buf[jp1][2])+
      dy3ty1*( ue[jp3][2]-2.0*ue[jp2][2] +ue[jp1][2]);

    forcing[c][k][j][i][3] = forcing[c][k][j][i][3] - ty2*(
        ue[jp3][3]*buf[jp3][2]-ue[jp1][3]*buf[jp1][2])+
      yycon2*(buf[jp3][3]-2.0*buf[jp2][3]+buf[jp1][3])+
      dy4ty1*( ue[jp3][3]-2.0*ue[jp2][3]+ ue[jp1][3]);

    forcing[c][k][j][i][4] = forcing[c][k][j][i][4] - ty2*(
        buf[jp3][2]*(c1*ue[jp3][4]-c2*q[jp3])-
        buf[jp1][2]*(c1*ue[jp1][4]-c2*q[jp1]))+
      0.5*yycon3*(buf[jp3][0]-2.0*buf[jp2][0]+
          buf[jp1][0])+
      yycon4*(cuf[jp3]-2.0*cuf[jp2]+cuf[jp1])+
      yycon5*(buf[jp3][4]-2.0*buf[jp2][4]+buf[jp1][4])+
      dy5ty1*(ue[jp3][4]-2.0*ue[jp2][4]+ue[jp1][4]);
  }

  //-------------------------------------------------------------------
  // Fourth-order dissipation                      
  //-------------------------------------------------------------------
  if (start[c][1] > 0) {
    j = 1;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (5.0*ue[j+2][m] - 4.0*ue[j+3][m] +ue[j+4][m]);
    }

    j = 2;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (-4.0*ue[j+1][m] + 6.0*ue[j+2][m] -
         4.0*ue[j+3][m] +     ue[j+4][m]);
    }
  }

  for (j = start[c][1]*3; j < cell_size[c][1]-3*end[c][1]; j++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp*
        (ue[j][m] - 4.0*ue[j+1][m] +
         6.0*ue[j+2][m] - 4.0*ue[j+3][m] + ue[j+4][m]);
    }
  }
  if (end[c][1] > 0) {
    j = cell_size[c][1]-3;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[j][m] - 4.0*ue[j+1][m] + 6.0*ue[j+2][m] - 4.0*ue[j+3][m]);
    }

    j = cell_size[c][1]-2;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[j][m] - 4.0*ue[j+1][m] + 5.0*ue[j+2][m]);

    }
  }
}


//---------------------------------------------------------------------
// zeta-direction flux differences                      
//---------------------------------------------------------------------
__kernel void exact_rhs4(__global double *g_forcing,
                         __global double *g_ue,
                         __global double *g_buf,
                         __global double *g_cuf,
                         __global double *g_q,
                         __global const double *g_ce,
                         __global int *g_cell_size,
                         __global int *g_cell_low,
                         __global int *g_start,
                         __global int *g_end,
                         int ncells)
{
  __global double (*forcing)[KMAX][JMAX][IMAX][5];
  __global double (*ue)[5];
  __global double (*buf)[5];
  __global double *cuf;
  __global double *q;
  __global int (*cell_size)[3];
  __global int (*cell_low)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  double dtemp[5], xi, eta, zeta, dtpp;
  int c, m, i, j, k;
  int kp1, kp2, kp3;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

  j = get_global_id(1) + start[c][1];
  i = get_global_id(0) + start[c][0];

  if (j >= (cell_size[c][1]-end[c][1]) ||
      i >= (cell_size[c][0]-end[c][0])) return;

  forcing = (__global double (*)[KMAX][JMAX][IMAX][5])g_forcing;
  cell_low = (__global int (*)[3])g_cell_low;

  int d0_size = cell_size[c][0] - end[c][0] - start[c][0];
  int d1_size = cell_size[c][1] - end[c][1] - start[c][1];
  int my_id = c*d1_size*d0_size + get_global_id(1)*d0_size + get_global_id(0);
  int my_offset1 = my_id * (MAX_CELL_DIM+4) * 5;
  int my_offset2 = my_id * (MAX_CELL_DIM+4);
  ue  = (__global double (*)[5])&g_ue[my_offset1];
  buf = (__global double (*)[5])&g_buf[my_offset1];
  cuf = (__global double *)&g_cuf[my_offset2];
  q   = (__global double *)&g_q[my_offset2];

  eta = (double)(j+cell_low[c][1]) * dnym1;
  xi = (double)(i+cell_low[c][0]) * dnxm1;

  for (k=2*start[c][2]; k <= cell_size[c][2]+3-2*end[c][2]; k++) {
    zeta = (double)(k+cell_low[c][2]-2) * dnzm1;

    exact_solution(xi, eta, zeta, dtemp, g_ce);
    for (m = 0; m < 5; m++) {
      ue[k][m] = dtemp[m];
    }

    dtpp = 1.0/dtemp[0];

    for (m = 1; m < 5; m++) {
      buf[k][m] = dtpp * dtemp[m];
    }

    cuf[k]    = buf[k][3] * buf[k][3];
    buf[k][0] = cuf[k] + buf[k][1] * buf[k][1] + buf[k][2] * buf[k][2];
    q[k] = 0.5*(buf[k][1]*ue[k][1] + buf[k][2]*ue[k][2] +
        buf[k][3]*ue[k][3]);
  }

  for (k=start[c][2]; k < cell_size[c][2]-end[c][2]; k++) {
    kp1 = k+1;
    kp2 = k+2;
    kp3 = k+3;

    forcing[c][k][j][i][0] = forcing[c][k][j][i][0] -
      tz2*( ue[kp3][3]-ue[kp1][3] )+
      dz1tz1*(ue[kp3][0]-2.0*ue[kp2][0]+ue[kp1][0]);

    forcing[c][k][j][i][1] = forcing[c][k][j][i][1] - tz2 * (
        ue[kp3][1]*buf[kp3][3]-ue[kp1][1]*buf[kp1][3])+
      zzcon2*(buf[kp3][1]-2.0*buf[kp2][1]+buf[kp1][1])+
      dz2tz1*( ue[kp3][1]-2.0* ue[kp2][1]+ ue[kp1][1]);

    forcing[c][k][j][i][2] = forcing[c][k][j][i][2] - tz2 * (
        ue[kp3][2]*buf[kp3][3]-ue[kp1][2]*buf[kp1][3])+
      zzcon2*(buf[kp3][2]-2.0*buf[kp2][2]+buf[kp1][2])+
      dz3tz1*(ue[kp3][2]-2.0*ue[kp2][2]+ue[kp1][2]);

    forcing[c][k][j][i][3] = forcing[c][k][j][i][3] - tz2 * (
        (ue[kp3][3]*buf[kp3][3]+c2*(ue[kp3][4]-q[kp3]))-
        (ue[kp1][3]*buf[kp1][3]+c2*(ue[kp1][4]-q[kp1])))+
      zzcon1*(buf[kp3][3]-2.0*buf[kp2][3]+buf[kp1][3])+
      dz4tz1*( ue[kp3][3]-2.0*ue[kp2][3] +ue[kp1][3]);

    forcing[c][k][j][i][4] = forcing[c][k][j][i][4] - tz2 * (
        buf[kp3][3]*(c1*ue[kp3][4]-c2*q[kp3])-
        buf[kp1][3]*(c1*ue[kp1][4]-c2*q[kp1]))+
      0.5*zzcon3*(buf[kp3][0]-2.0*buf[kp2][0]
          +buf[kp1][0])+
      zzcon4*(cuf[kp3]-2.0*cuf[kp2]+cuf[kp1])+
      zzcon5*(buf[kp3][4]-2.0*buf[kp2][4]+buf[kp1][4])+
      dz5tz1*( ue[kp3][4]-2.0*ue[kp2][4]+ ue[kp1][4]);
  }

  //-------------------------------------------------------------------
  // Fourth-order dissipation                        
  //-------------------------------------------------------------------
  if (start[c][2] > 0) {
    k = 1;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (5.0*ue[k+2][m] - 4.0*ue[k+3][m] +ue[k+4][m]);
    }

    k = 2;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (-4.0*ue[k+1][m] + 6.0*ue[k+2][m] -
         4.0*ue[k+3][m] +     ue[k+4][m]);
    }
  }

  for (k = start[c][2]*3; k < cell_size[c][2]-3*end[c][2]; k++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp*
        (ue[k][m] - 4.0*ue[k+1][m] +
         6.0*ue[k+2][m] - 4.0*ue[k+3][m] + ue[k+4][m]);
    }
  }

  if (end[c][2] > 0) {
    k = cell_size[c][2]-3;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[k][m] - 4.0*ue[k+1][m] + 6.0*ue[k+2][m] - 4.0*ue[k+3][m]);
    }

    k = cell_size[c][2]-2;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[k][m] - 4.0*ue[k+1][m] + 5.0*ue[k+2][m]);
    }
  }
}


//---------------------------------------------------------------------
// now change the sign of the forcing function, 
//---------------------------------------------------------------------
__kernel void exact_rhs5(__global double *g_forcing,
                         __global int *g_cell_size,
                         __global int *g_start,
                         __global int *g_end,
                         int ncells)
{
  __global double (*forcing)[KMAX][JMAX][IMAX][5];
  __global int (*cell_size)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  int c, k, j, i, m;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

  k = get_global_id(1) + start[c][2];
  j = get_global_id(0) + start[c][1];

  if (k >= (cell_size[c][2]-end[c][2]) ||
      j >= (cell_size[c][1]-end[c][1])) return;

  forcing = (__global double (*)[KMAX][JMAX][IMAX][5])g_forcing;

  for (i = start[c][0]; i < cell_size[c][0]-end[c][0]; i++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = -1.0 * forcing[c][k][j][i][m];
    }
  }
}

//--------------------------------------------------------------------------
// initialize()
//--------------------------------------------------------------------------
//---------------------------------------------------------------------
// Later (in compute_rhs) we compute 1/u for every element. A few of 
// the corner elements are not used, but it convenient (and faster) 
// to compute the whole thing with a simple loop. Make sure those 
// values are nonzero by initializing the whole thing here. 
//---------------------------------------------------------------------
__kernel void initialize1(__global double *g_u, int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  int c, kk, jj, ii, m;

  c = get_global_id(2);
  kk = get_global_id(1) + 1;
  jj = get_global_id(0) + 1;
  if (c >= ncells || kk >= KMAX+3 || jj >= JMAX+3) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;

  for (ii = 1; ii < IMAX+3; ii++) {
    for (m = 0; m < 5; m++) {
      u[c][kk][jj][ii][m] = 1.0;
    }
  }
}


//---------------------------------------------------------------------
// first store the "interpolated" values everywhere on the grid    
//---------------------------------------------------------------------
__kernel void initialize2(__global double *g_u,
                          __global int *g_cell_low,
                          __global int *g_cell_high,
                          __global const double *g_ce,
                          int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_low)[3];
  __global int (*cell_high)[3];

  int c, i, j, k, m, ii, jj, kk, ix, iy, iz;
  double xi, eta, zeta, Pface[2][3][5], Pxi, Peta, Pzeta;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_low  = (__global int (*)[3])g_cell_low;
  cell_high = (__global int (*)[3])g_cell_high;

  k = get_global_id(1) + cell_low[c][2];
  j = get_global_id(0) + cell_low[c][1];
  if (k > cell_high[c][2] || j > cell_high[c][1]) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;

  kk = get_global_id(1) + 2;
  zeta = (double)(k) * dnzm1;
  jj = get_global_id(0) + 2;
  eta = (double)(j) * dnym1;
  ii = 2;

  for (i = cell_low[c][0]; i <= cell_high[c][0]; i++) {
    xi = (double)(i) * dnxm1;

    for (ix = 0; ix < 2; ix++) {
      exact_solution((double)(ix), eta, zeta, &Pface[ix][0][0], g_ce);
    }

    for (iy = 0; iy < 2; iy++) {
      exact_solution(xi, (double)(iy) , zeta, &Pface[iy][1][0], g_ce);
    }

    for (iz = 0; iz < 2; iz++) {
      exact_solution(xi, eta, (double)(iz), &Pface[iz][2][0], g_ce);
    }

    for (m = 0; m < 5; m++) {
      Pxi   = xi   * Pface[1][0][m] + (1.0-xi)   * Pface[0][0][m];
      Peta  = eta  * Pface[1][1][m] + (1.0-eta)  * Pface[0][1][m];
      Pzeta = zeta * Pface[1][2][m] + (1.0-zeta) * Pface[0][2][m];

      u[c][kk][jj][ii][m] = Pxi + Peta + Pzeta - 
                            Pxi*Peta - Pxi*Pzeta - Peta*Pzeta + 
                            Pxi*Peta*Pzeta;
    }
    ii = ii + 1;
  }
}


//---------------------------------------------------------------------
// west face
//---------------------------------------------------------------------
__kernel void initialize3(__global double *g_u,
                          __global int *g_cell_low,
                          __global int *g_cell_high,
                          __global int *g_slice,
                          __global const double *g_ce)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_low)[3];
  __global int (*cell_high)[3];
  __global int (*slice)[3];

  int c, j, k, m, ii, jj, kk;
  double xi, eta, zeta, temp[5];

  cell_low  = (__global int (*)[3])g_cell_low;
  cell_high = (__global int (*)[3])g_cell_high;
  slice = (__global int (*)[3])g_slice;

  c = slice[0][0];

  k = get_global_id(1) + cell_low[c][2];
  j = get_global_id(0) + cell_low[c][1];
  if (k > cell_high[c][2] || j > cell_high[c][1]) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;

  ii = 2;
  xi = 0.0;
  kk = get_global_id(1) + 2;
  zeta = (double)(k) * dnzm1;
  jj = get_global_id(0) + 2;

  eta = (double)(j) * dnym1;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}


//---------------------------------------------------------------------
// east face                                                      
//---------------------------------------------------------------------
__kernel void initialize4(__global double *g_u,
                          __global int *g_cell_low,
                          __global int *g_cell_high,
                          __global int *g_cell_size,
                          __global int *g_slice,
                          __global const double *g_ce,
                          int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_low)[3];
  __global int (*cell_high)[3];
  __global int (*cell_size)[3];
  __global int (*slice)[3];

  int c, j, k, m, ii, jj, kk;
  double xi, eta, zeta, temp[5];

  cell_low  = (__global int (*)[3])g_cell_low;
  cell_high = (__global int (*)[3])g_cell_high;
  slice = (__global int (*)[3])g_slice;

  c = slice[ncells-1][0];

  k = get_global_id(1) + cell_low[c][2];
  j = get_global_id(0) + cell_low[c][1];
  if (k > cell_high[c][2] || j > cell_high[c][1]) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_size = (__global int (*)[3])g_cell_size;

  ii = cell_size[c][0]+1;
  xi = 1.0;
  kk = get_global_id(1) + 2;
  zeta = (double)(k) * dnzm1;
  jj = get_global_id(0) + 2;

  eta = (double)(j) * dnym1;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}


//---------------------------------------------------------------------
// south face
//---------------------------------------------------------------------
__kernel void initialize5(__global double *g_u,
                          __global int *g_cell_low,
                          __global int *g_cell_high,
                          __global int *g_slice,
                          __global const double *g_ce)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_low)[3];
  __global int (*cell_high)[3];
  __global int (*slice)[3];

  int c, i, k, m, ii, jj, kk;
  double xi, eta, zeta, temp[5];

  cell_low  = (__global int (*)[3])g_cell_low;
  cell_high = (__global int (*)[3])g_cell_high;
  slice = (__global int (*)[3])g_slice;

  c = slice[0][1];

  k = get_global_id(1) + cell_low[c][2];
  i = get_global_id(0) + cell_low[c][0];
  if (k > cell_high[c][2] || i > cell_high[c][0]) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;

  jj = 2;
  eta = 0.0;
  kk = get_global_id(1) + 2;
  zeta = (double)(k) * dnzm1;
  ii = get_global_id(0) + 2;

  xi = (double)(i) * dnxm1;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
  ii = ii + 1;
}


//---------------------------------------------------------------------
// north face                                    
//---------------------------------------------------------------------
__kernel void initialize6(__global double *g_u,
                          __global int *g_cell_low,
                          __global int *g_cell_high,
                          __global int *g_cell_size,
                          __global int *g_slice,
                          __global const double *g_ce,
                          int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_low)[3];
  __global int (*cell_high)[3];
  __global int (*cell_size)[3];
  __global int (*slice)[3];

  int c, i, k, m, ii, jj, kk;
  double xi, eta, zeta, temp[5];

  cell_low  = (__global int (*)[3])g_cell_low;
  cell_high = (__global int (*)[3])g_cell_high;
  slice = (__global int (*)[3])g_slice;

  c = slice[ncells-1][1];

  k = get_global_id(1) + cell_low[c][2];
  i = get_global_id(0) + cell_low[c][0];
  if (k > cell_high[c][2] || i > cell_high[c][0]) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_size = (__global int (*)[3])g_cell_size;

  jj = cell_size[c][1]+1;
  eta = 1.0;
  kk = get_global_id(1) + 2;
  zeta = (double)(k) * dnzm1;
  ii = get_global_id(0) + 2;

  xi = (double)(i) * dnxm1;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}


//---------------------------------------------------------------------
// bottom face
//---------------------------------------------------------------------
__kernel void initialize7(__global double *g_u,
                          __global int *g_cell_low,
                          __global int *g_cell_high,
                          __global int *g_slice,
                          __global const double *g_ce)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_low)[3];
  __global int (*cell_high)[3];
  __global int (*slice)[3];

  int c, i, j, m, ii, jj, kk;
  double xi, eta, zeta, temp[5];

  cell_low  = (__global int (*)[3])g_cell_low;
  cell_high = (__global int (*)[3])g_cell_high;
  slice = (__global int (*)[3])g_slice;

  c = slice[0][2];

  j = get_global_id(1) + cell_low[c][1];
  i = get_global_id(0) + cell_low[c][0];
  if (j > cell_high[c][1] || i > cell_high[c][0]) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;

  kk = 2;
  zeta = 0.0;
  jj = get_global_id(1) + 2;
  eta = (double)(j) * dnym1;
  ii = get_global_id(0) + 2;

  xi = (double)(i) * dnxm1;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}


//---------------------------------------------------------------------
// top face     
//---------------------------------------------------------------------
__kernel void initialize8(__global double *g_u,
                          __global int *g_cell_low,
                          __global int *g_cell_high,
                          __global int *g_cell_size,
                          __global int *g_slice,
                          __global const double *g_ce,
                          int ncells)
{
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global int (*cell_low)[3];
  __global int (*cell_high)[3];
  __global int (*cell_size)[3];
  __global int (*slice)[3];

  int c, i, j, m, ii, jj, kk;
  double xi, eta, zeta, temp[5];

  cell_low  = (__global int (*)[3])g_cell_low;
  cell_high = (__global int (*)[3])g_cell_high;
  slice = (__global int (*)[3])g_slice;

  c = slice[ncells-1][2];

  j = get_global_id(1) + cell_low[c][1];
  i = get_global_id(0) + cell_low[c][0];
  if (j > cell_high[c][1] || i > cell_high[c][0]) return;

  u = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  cell_size = (__global int (*)[3])g_cell_size;

  kk = cell_size[c][2]+1;
  zeta = 1.0;
  jj = get_global_id(1) + 2;
  eta = (double)(j) * dnym1;
  ii = get_global_id(0) + 2;
  xi = (double)(i) * dnxm1;

  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}


__kernel void lhsinit(__global double *g_lhsc,
                      __global int *g_start,
                      __global int *g_end,
                      __global int *g_cell_coord,
                      __global int *g_cell_size,
                      int ncells)
{
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global int (*start)[3];
  __global int (*end)[3];
  __global int (*cell_coord)[3];
  __global int (*cell_size)[3];

  int i, j, k, d, c, m, n;

  c = get_global_id(2);
  if (c >= ncells) return;

  cell_size = (__global int (*)[3])g_cell_size;

  k = get_global_id(1) + 1;
  j = get_global_id(0) + 1;
  if (k > cell_size[c][2] || j > cell_size[c][1]) return;

  lhsc  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;
  cell_coord = (__global int (*)[3])g_cell_coord;

  if (k == 1 && j == 1) {
    //---------------------------------------------------------------------
    // first, initialize the start and end arrays
    //---------------------------------------------------------------------
    for (d = 0; d < 3; d++) {
      if (cell_coord[c][d] == 0) {
        start[c][d] = 1;
      } else { 
        start[c][d] = 0;
      }
      if (cell_coord[c][d] == ncells-1) {
        end[c][d] = 1;
      } else {
        end[c][d] = 0;
      }
    }
  }

  //---------------------------------------------------------------------
  // zero the whole left hand side for starters
  //---------------------------------------------------------------------
  for (i = 1; i <= cell_size[c][0]; i++) {
    for (n = 0; n < 5; n++) {
      for (m = 0; m < 5; m++) {
        lhsc[c][k][j][i][n][m] = 0.0;
      }
    }
  }
}

void lhsabinit(__global double (*lhsa)[5][5],
               __global double (*lhsb)[5][5], int size);
void matvec_sub(__global double ablock[5][5],
                __global double avec[5],
                __global double bvec[5]);
void matmul_sub(__global double ablock[5][5],
                __global double bblock[5][5],
                __global double cblock[5][5]);
void binvcrhs(__global double lhs[5][5],
              __global double c[5][5],
              __global double r[5]);
void binvrhs(__global double lhs[5][5], __global double r[5]);


//---------------------------------------------------------------------
// unpack C'(-1) and rhs'(-1) for
// all j and k
//---------------------------------------------------------------------
__kernel void x_unpack_solve_info(__global double *g_lhsc,
                                  __global double *g_rhs,
                                  __global double *out_buffer,
                                  int c)
{
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];

  int j, k, m, n, ptr, istart;
  istart = 0;

  k = get_global_id(1) + 1;
  j = get_global_id(0) + 1;
  if (k >= KMAX+1 || j >= JMAX+1) return;

  lhsc = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  ptr = (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE) * (JMAX*(k-1) + (j-1));

  for (n = 0; n < BLOCK_SIZE; n++) {
    for (m = 0; m < BLOCK_SIZE; m++) {
      lhsc[c][k][j][istart][n][m] = out_buffer[ptr+m];
    }
    ptr = ptr+BLOCK_SIZE;
  }
  for (n = 0; n < BLOCK_SIZE; n++) {
    rhs[c][k][j][istart][n] = out_buffer[ptr+n];
  }
}


__kernel void x_send_solve_info(__global double *g_lhsc,
                                __global double *g_rhs,
                                __global double *in_buffer,
                                __global int *g_cell_size,
                                int c)
{
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global int (*cell_size)[3];

  int j, k, m, n, isize, ptr;

  k = get_global_id(1) + 1;
  j = get_global_id(0) + 1;
  if (k >= KMAX+1 || j >= JMAX+1) return;

  lhsc = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  cell_size = (__global int (*)[3])g_cell_size;

  isize = cell_size[c][0];
  ptr = (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE) * (JMAX*(k-1) + (j-1));

  for (n = 0; n < BLOCK_SIZE; n++) {
    for (m = 0; m < BLOCK_SIZE; m++) {
      in_buffer[ptr+m] = lhsc[c][k][j][isize][n][m];
    }
    ptr = ptr+BLOCK_SIZE;
  }
  for (n = 0; n < BLOCK_SIZE; n++) {
    in_buffer[ptr+n] = rhs[c][k][j][isize][n];
  }
}


__kernel void x_send_backsub_info(__global double *g_rhs,
                                  __global double *in_buffer,
                                  int c)
{
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];

  int j, k, n, ptr, istart;

  k = get_global_id(1) + 1;
  j = get_global_id(0) + 1;
  if (k >= KMAX+1 || j >= JMAX+1) return;

  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  istart = 1;
  ptr = BLOCK_SIZE * (JMAX*(k-1) + (j-1));

  for (n = 0; n < BLOCK_SIZE; n++) {
    in_buffer[ptr+n] = rhs[c][k][j][istart][n];
  }
}


//---------------------------------------------------------------------
// unpack U(isize) for all j and k
//---------------------------------------------------------------------
__kernel void x_unpack_backsub_info(__global double *g_backsub_info,
                                    __global double *out_buffer,
                                    int c)
{
  __global double (*backsub_info)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5];
  
  int j, k, n, ptr;

  k = get_global_id(1);
  j = get_global_id(0);
  if (k >= KMAX || j >= JMAX) return;

  backsub_info = (__global double (*)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5])g_backsub_info;

  ptr = BLOCK_SIZE * (JMAX*k + j);
  for (n = 0; n < BLOCK_SIZE; n++) {
    backsub_info[c][k][j][n] = out_buffer[ptr+n];
  }
}


__kernel void x_backsubstitute(__global double *g_rhs,
                               __global double *g_lhsc,
                               __global double *g_backsub_info,
                               __global int *g_cell_size,
                               __global int *g_start,
                               __global int *g_end,
                               int last,
                               int c)
{
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*backsub_info)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5];
  __global int (*cell_size)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  int i, j, k;
  int m, n, isize, jsize, ksize, istart;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

  k = get_global_id(1) + start[c][2]+1;
  ksize = cell_size[c][2]-end[c][2];
  if (k > ksize) return;

  j = get_global_id(0) + start[c][1]+1;
  jsize = cell_size[c][1]-end[c][1];
  if (j > jsize) return;

  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  lhsc = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  backsub_info = (__global double (*)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5])g_backsub_info;

  istart = 1;
  isize = cell_size[c][0];

  if (last == 0) {
    //-------------------------------------------------------------------
    // U(isize) uses info from previous cell if not last cell
    //-------------------------------------------------------------------
    for (m = 0; m < BLOCK_SIZE; m++) {
      for (n = 0; n < BLOCK_SIZE; n++) {
        rhs[c][k][j][isize][m] = rhs[c][k][j][isize][m] 
          - lhsc[c][k][j][isize][n][m]*
          backsub_info[c][k-1][j-1][n];
        //---------------------------------------------------------------
        // rhs[c][k][j][isize][m] = rhs[c][k][j][isize][m] 
        // $    - lhsc[c][k][j][isize][n][m]*rhs[c][k][j][isize+1][n]
        //---------------------------------------------------------------
      }
    }
  }

  for (i = isize-1; i >= istart; i--) {
    for (m = 0; m < BLOCK_SIZE; m++) {
      for (n = 0; n < BLOCK_SIZE; n++) {
        rhs[c][k][j][i][m] = rhs[c][k][j][i][m] 
          - lhsc[c][k][j][i][n][m]*rhs[c][k][j][i+1][n];
      }
    }
  }
}


__kernel void x_solve_cell(__global double *g_qs,
                           __global double *g_rho_i,
                           __global double *g_u,
                           __global double *g_rhs,
                           __global double *g_lhsc,
                           __global double *g_lhsa,
                           __global double *g_lhsb,
                           __global double *g_fjac,
                           __global double *g_njac,
                           __global int *g_cell_size,
                           __global int *g_start,
                           __global int *g_end,
                           int first,
                           int last,
                           int c)
{
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*lhsa)[5][5];
  __global double (*lhsb)[5][5];
  __global double (*fjac)[5][5];
  __global double (*njac)[5][5];
  __global int (*cell_size)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  int i, j, k, isize, ksize, jsize, istart;
  double tmp1, tmp2, tmp3;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

#if X_SOLVE_DIM == 2
  k = get_global_id(1) + start[c][2]+2;
  ksize = cell_size[c][2]-end[c][2]+1;
  if (k > ksize) return;

  j = get_global_id(0) + start[c][1]+2;
  jsize = cell_size[c][1]-end[c][1]+1;
  if (j > jsize) return;

  qs    = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  rho_i = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  u     = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  rhs   = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  lhsc  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;

  int d0_size = jsize - start[c][1] - 1;
  int my_id = get_global_id(1)*d0_size + get_global_id(0);
  int my_offset1 = my_id * (MAX_CELL_DIM+2)*5*5;
  int my_offset2 = my_id * (MAX_CELL_DIM+4)*5*5;
  lhsa = (__global double (*)[5][5])&g_lhsa[my_offset1];
  lhsb = (__global double (*)[5][5])&g_lhsb[my_offset1];
  fjac = (__global double (*)[5][5])&g_fjac[my_offset2];
  njac = (__global double (*)[5][5])&g_njac[my_offset2];

  istart = 1;
  isize = cell_size[c][0];

  lhsabinit(lhsa, lhsb, isize);

  //---------------------------------------------------------------------
  // This function computes the left hand side in the xi-direction
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // determine a (labeled f) and n jacobians for cell c
  //---------------------------------------------------------------------
  for (i = start[c][0]+1; i <= cell_size[c][0]-end[c][0]+2; i++) {

    tmp1 = rho_i[c][k-1][j-1][i-1];
    tmp2 = tmp1 * tmp1;
    tmp3 = tmp1 * tmp2;
    //-------------------------------------------------------------------
    // 
    //-------------------------------------------------------------------
    fjac[i][0][0] = 0.0;
    fjac[i][1][0] = 1.0;
    fjac[i][2][0] = 0.0;
    fjac[i][3][0] = 0.0;
    fjac[i][4][0] = 0.0;

    fjac[i][0][1] = -(u[c][k][j][i][1] * tmp2 * 
        u[c][k][j][i][1])
      + c2 * qs[c][k-1][j-1][i-1];
    fjac[i][1][1] = ( 2.0 - c2 )
      * ( u[c][k][j][i][1] * tmp1 );
    fjac[i][2][1] = - c2 * ( u[c][k][j][i][2] * tmp1 );
    fjac[i][3][1] = - c2 * ( u[c][k][j][i][3] * tmp1 );
    fjac[i][4][1] = c2;

    fjac[i][0][2] = - ( u[c][k][j][i][1]*u[c][k][j][i][2] ) * tmp2;
    fjac[i][1][2] = u[c][k][j][i][2] * tmp1;
    fjac[i][2][2] = u[c][k][j][i][1] * tmp1;
    fjac[i][3][2] = 0.0;
    fjac[i][4][2] = 0.0;

    fjac[i][0][3] = - ( u[c][k][j][i][1]*u[c][k][j][i][3] ) * tmp2;
    fjac[i][1][3] = u[c][k][j][i][3] * tmp1;
    fjac[i][2][3] = 0.0;
    fjac[i][3][3] = u[c][k][j][i][1] * tmp1;
    fjac[i][4][3] = 0.0;

    fjac[i][0][4] = ( c2 * 2.0 * qs[c][k-1][j-1][i-1]
        - c1 * ( u[c][k][j][i][4] * tmp1 ) )
      * ( u[c][k][j][i][1] * tmp1 );
    fjac[i][1][4] = c1 *  u[c][k][j][i][4] * tmp1 
      - c2
      * ( u[c][k][j][i][1]*u[c][k][j][i][1] * tmp2
          + qs[c][k-1][j-1][i-1] );
    fjac[i][2][4] = - c2 * ( u[c][k][j][i][2]*u[c][k][j][i][1] )
      * tmp2;
    fjac[i][3][4] = - c2 * ( u[c][k][j][i][3]*u[c][k][j][i][1] )
      * tmp2;
    fjac[i][4][4] = c1 * ( u[c][k][j][i][1] * tmp1 );

    njac[i][0][0] = 0.0;
    njac[i][1][0] = 0.0;
    njac[i][2][0] = 0.0;
    njac[i][3][0] = 0.0;
    njac[i][4][0] = 0.0;

    njac[i][0][1] = - con43 * c3c4 * tmp2 * u[c][k][j][i][1];
    njac[i][1][1] =   con43 * c3c4 * tmp1;
    njac[i][2][1] =   0.0;
    njac[i][3][1] =   0.0;
    njac[i][4][1] =   0.0;

    njac[i][0][2] = - c3c4 * tmp2 * u[c][k][j][i][2];
    njac[i][1][2] =   0.0;
    njac[i][2][2] =   c3c4 * tmp1;
    njac[i][3][2] =   0.0;
    njac[i][4][2] =   0.0;

    njac[i][0][3] = - c3c4 * tmp2 * u[c][k][j][i][3];
    njac[i][1][3] =   0.0;
    njac[i][2][3] =   0.0;
    njac[i][3][3] =   c3c4 * tmp1;
    njac[i][4][3] =   0.0;

    njac[i][0][4] = - ( con43 * c3c4
        - c1345 ) * tmp3 * (u[c][k][j][i][1]*u[c][k][j][i][1])
      - ( c3c4 - c1345 ) * tmp3 * (u[c][k][j][i][2]*u[c][k][j][i][2])
      - ( c3c4 - c1345 ) * tmp3 * (u[c][k][j][i][3]*u[c][k][j][i][3])
      - c1345 * tmp2 * u[c][k][j][i][4];

    njac[i][1][4] = ( con43 * c3c4
        - c1345 ) * tmp2 * u[c][k][j][i][1];
    njac[i][2][4] = ( c3c4 - c1345 ) * tmp2 * u[c][k][j][i][2];
    njac[i][3][4] = ( c3c4 - c1345 ) * tmp2 * u[c][k][j][i][3];
    njac[i][4][4] = ( c1345 ) * tmp1;

  }

  //---------------------------------------------------------------------
  // now jacobians set, so form left hand side in x direction
  //---------------------------------------------------------------------
  for (i = start[c][0]+2; i <= isize-end[c][0]+1; i++) {

    tmp1 = dt * tx1;
    tmp2 = dt * tx2;

    lhsa[i-1][0][0] = - tmp2 * fjac[i-1][0][0]
      - tmp1 * njac[i-1][0][0]
      - tmp1 * dx1; 
    lhsa[i-1][1][0] = - tmp2 * fjac[i-1][1][0]
      - tmp1 * njac[i-1][1][0];
    lhsa[i-1][2][0] = - tmp2 * fjac[i-1][2][0]
      - tmp1 * njac[i-1][2][0];
    lhsa[i-1][3][0] = - tmp2 * fjac[i-1][3][0]
      - tmp1 * njac[i-1][3][0];
    lhsa[i-1][4][0] = - tmp2 * fjac[i-1][4][0]
      - tmp1 * njac[i-1][4][0];

    lhsa[i-1][0][1] = - tmp2 * fjac[i-1][0][1]
      - tmp1 * njac[i-1][0][1];
    lhsa[i-1][1][1] = - tmp2 * fjac[i-1][1][1]
      - tmp1 * njac[i-1][1][1]
      - tmp1 * dx2;
    lhsa[i-1][2][1] = - tmp2 * fjac[i-1][2][1]
      - tmp1 * njac[i-1][2][1];
    lhsa[i-1][3][1] = - tmp2 * fjac[i-1][3][1]
      - tmp1 * njac[i-1][3][1];
    lhsa[i-1][4][1] = - tmp2 * fjac[i-1][4][1]
      - tmp1 * njac[i-1][4][1];

    lhsa[i-1][0][2] = - tmp2 * fjac[i-1][0][2]
      - tmp1 * njac[i-1][0][2];
    lhsa[i-1][1][2] = - tmp2 * fjac[i-1][1][2]
      - tmp1 * njac[i-1][1][2];
    lhsa[i-1][2][2] = - tmp2 * fjac[i-1][2][2]
      - tmp1 * njac[i-1][2][2]
      - tmp1 * dx3;
    lhsa[i-1][3][2] = - tmp2 * fjac[i-1][3][2]
      - tmp1 * njac[i-1][3][2];
    lhsa[i-1][4][2] = - tmp2 * fjac[i-1][4][2]
      - tmp1 * njac[i-1][4][2];

    lhsa[i-1][0][3] = - tmp2 * fjac[i-1][0][3]
      - tmp1 * njac[i-1][0][3];
    lhsa[i-1][1][3] = - tmp2 * fjac[i-1][1][3]
      - tmp1 * njac[i-1][1][3];
    lhsa[i-1][2][3] = - tmp2 * fjac[i-1][2][3]
      - tmp1 * njac[i-1][2][3];
    lhsa[i-1][3][3] = - tmp2 * fjac[i-1][3][3]
      - tmp1 * njac[i-1][3][3]
      - tmp1 * dx4;
    lhsa[i-1][4][3] = - tmp2 * fjac[i-1][4][3]
      - tmp1 * njac[i-1][4][3];

    lhsa[i-1][0][4] = - tmp2 * fjac[i-1][0][4]
      - tmp1 * njac[i-1][0][4];
    lhsa[i-1][1][4] = - tmp2 * fjac[i-1][1][4]
      - tmp1 * njac[i-1][1][4];
    lhsa[i-1][2][4] = - tmp2 * fjac[i-1][2][4]
      - tmp1 * njac[i-1][2][4];
    lhsa[i-1][3][4] = - tmp2 * fjac[i-1][3][4]
      - tmp1 * njac[i-1][3][4];
    lhsa[i-1][4][4] = - tmp2 * fjac[i-1][4][4]
      - tmp1 * njac[i-1][4][4]
      - tmp1 * dx5;

    lhsb[i-1][0][0] = 1.0
      + tmp1 * 2.0 * njac[i][0][0]
      + tmp1 * 2.0 * dx1;
    lhsb[i-1][1][0] = tmp1 * 2.0 * njac[i][1][0];
    lhsb[i-1][2][0] = tmp1 * 2.0 * njac[i][2][0];
    lhsb[i-1][3][0] = tmp1 * 2.0 * njac[i][3][0];
    lhsb[i-1][4][0] = tmp1 * 2.0 * njac[i][4][0];

    lhsb[i-1][0][1] = tmp1 * 2.0 * njac[i][0][1];
    lhsb[i-1][1][1] = 1.0
      + tmp1 * 2.0 * njac[i][1][1]
      + tmp1 * 2.0 * dx2;
    lhsb[i-1][2][1] = tmp1 * 2.0 * njac[i][2][1];
    lhsb[i-1][3][1] = tmp1 * 2.0 * njac[i][3][1];
    lhsb[i-1][4][1] = tmp1 * 2.0 * njac[i][4][1];

    lhsb[i-1][0][2] = tmp1 * 2.0 * njac[i][0][2];
    lhsb[i-1][1][2] = tmp1 * 2.0 * njac[i][1][2];
    lhsb[i-1][2][2] = 1.0
      + tmp1 * 2.0 * njac[i][2][2]
      + tmp1 * 2.0 * dx3;
    lhsb[i-1][3][2] = tmp1 * 2.0 * njac[i][3][2];
    lhsb[i-1][4][2] = tmp1 * 2.0 * njac[i][4][2];

    lhsb[i-1][0][3] = tmp1 * 2.0 * njac[i][0][3];
    lhsb[i-1][1][3] = tmp1 * 2.0 * njac[i][1][3];
    lhsb[i-1][2][3] = tmp1 * 2.0 * njac[i][2][3];
    lhsb[i-1][3][3] = 1.0
      + tmp1 * 2.0 * njac[i][3][3]
      + tmp1 * 2.0 * dx4;
    lhsb[i-1][4][3] = tmp1 * 2.0 * njac[i][4][3];

    lhsb[i-1][0][4] = tmp1 * 2.0 * njac[i][0][4];
    lhsb[i-1][1][4] = tmp1 * 2.0 * njac[i][1][4];
    lhsb[i-1][2][4] = tmp1 * 2.0 * njac[i][2][4];
    lhsb[i-1][3][4] = tmp1 * 2.0 * njac[i][3][4];
    lhsb[i-1][4][4] = 1.0
      + tmp1 * 2.0 * njac[i][4][4]
      + tmp1 * 2.0 * dx5;

    lhsc[c][k-1][j-1][i-1][0][0] =  tmp2 * fjac[i+1][0][0]
      - tmp1 * njac[i+1][0][0]
      - tmp1 * dx1;
    lhsc[c][k-1][j-1][i-1][1][0] =  tmp2 * fjac[i+1][1][0]
      - tmp1 * njac[i+1][1][0];
    lhsc[c][k-1][j-1][i-1][2][0] =  tmp2 * fjac[i+1][2][0]
      - tmp1 * njac[i+1][2][0];
    lhsc[c][k-1][j-1][i-1][3][0] =  tmp2 * fjac[i+1][3][0]
      - tmp1 * njac[i+1][3][0];
    lhsc[c][k-1][j-1][i-1][4][0] =  tmp2 * fjac[i+1][4][0]
      - tmp1 * njac[i+1][4][0];

    lhsc[c][k-1][j-1][i-1][0][1] =  tmp2 * fjac[i+1][0][1]
      - tmp1 * njac[i+1][0][1];
    lhsc[c][k-1][j-1][i-1][1][1] =  tmp2 * fjac[i+1][1][1]
      - tmp1 * njac[i+1][1][1]
      - tmp1 * dx2;
    lhsc[c][k-1][j-1][i-1][2][1] =  tmp2 * fjac[i+1][2][1]
      - tmp1 * njac[i+1][2][1];
    lhsc[c][k-1][j-1][i-1][3][1] =  tmp2 * fjac[i+1][3][1]
      - tmp1 * njac[i+1][3][1];
    lhsc[c][k-1][j-1][i-1][4][1] =  tmp2 * fjac[i+1][4][1]
      - tmp1 * njac[i+1][4][1];

    lhsc[c][k-1][j-1][i-1][0][2] =  tmp2 * fjac[i+1][0][2]
      - tmp1 * njac[i+1][0][2];
    lhsc[c][k-1][j-1][i-1][1][2] =  tmp2 * fjac[i+1][1][2]
      - tmp1 * njac[i+1][1][2];
    lhsc[c][k-1][j-1][i-1][2][2] =  tmp2 * fjac[i+1][2][2]
      - tmp1 * njac[i+1][2][2]
      - tmp1 * dx3;
    lhsc[c][k-1][j-1][i-1][3][2] =  tmp2 * fjac[i+1][3][2]
      - tmp1 * njac[i+1][3][2];
    lhsc[c][k-1][j-1][i-1][4][2] =  tmp2 * fjac[i+1][4][2]
      - tmp1 * njac[i+1][4][2];

    lhsc[c][k-1][j-1][i-1][0][3] =  tmp2 * fjac[i+1][0][3]
      - tmp1 * njac[i+1][0][3];
    lhsc[c][k-1][j-1][i-1][1][3] =  tmp2 * fjac[i+1][1][3]
      - tmp1 * njac[i+1][1][3];
    lhsc[c][k-1][j-1][i-1][2][3] =  tmp2 * fjac[i+1][2][3]
      - tmp1 * njac[i+1][2][3];
    lhsc[c][k-1][j-1][i-1][3][3] =  tmp2 * fjac[i+1][3][3]
      - tmp1 * njac[i+1][3][3]
      - tmp1 * dx4;
    lhsc[c][k-1][j-1][i-1][4][3] =  tmp2 * fjac[i+1][4][3]
      - tmp1 * njac[i+1][4][3];

    lhsc[c][k-1][j-1][i-1][0][4] =  tmp2 * fjac[i+1][0][4]
      - tmp1 * njac[i+1][0][4];
    lhsc[c][k-1][j-1][i-1][1][4] =  tmp2 * fjac[i+1][1][4]
      - tmp1 * njac[i+1][1][4];
    lhsc[c][k-1][j-1][i-1][2][4] =  tmp2 * fjac[i+1][2][4]
      - tmp1 * njac[i+1][2][4];
    lhsc[c][k-1][j-1][i-1][3][4] =  tmp2 * fjac[i+1][3][4]
      - tmp1 * njac[i+1][3][4];
    lhsc[c][k-1][j-1][i-1][4][4] =  tmp2 * fjac[i+1][4][4]
      - tmp1 * njac[i+1][4][4]
      - tmp1 * dx5;

  }

  //---------------------------------------------------------------------
  // outer most do loops - sweeping in i direction
  //---------------------------------------------------------------------
  if (first == 1) { 

    //-------------------------------------------------------------------
    // multiply c(istart,j,k) by b_inverse and copy back to c
    // multiply rhs(istart) by b_inverse(istart) and copy to rhs
    //-------------------------------------------------------------------
    binvcrhs( lhsb[istart], lhsc[c][k-1][j-1][istart],
              rhs[c][k-1][j-1][istart] );

  }

  //---------------------------------------------------------------------
  // begin inner most do loop
  // do all the elements of the cell unless last 
  //---------------------------------------------------------------------
  for (i = istart+first; i <= isize-last; i++) {

    //-------------------------------------------------------------------
    // rhs(i) = rhs(i) - A*rhs(i-1)
    //-------------------------------------------------------------------
    matvec_sub(lhsa[i], rhs[c][k-1][j-1][i-1], rhs[c][k-1][j-1][i]);

    //-------------------------------------------------------------------
    // B(i) = B(i) - C(i-1)*A(i)
    //-------------------------------------------------------------------
    matmul_sub(lhsa[i], lhsc[c][k-1][j-1][i-1], lhsb[i]);


    //-------------------------------------------------------------------
    // multiply c(i,j,k) by b_inverse and copy back to c
    // multiply rhs(1,j,k) by b_inverse(1,j,k) and copy to rhs
    //-------------------------------------------------------------------
    binvcrhs( lhsb[i], lhsc[c][k-1][j-1][i], rhs[c][k-1][j-1][i] );

  }

  //---------------------------------------------------------------------
  // Now finish up special cases for last cell
  //---------------------------------------------------------------------
  if (last == 1) {

    //-------------------------------------------------------------------
    // rhs(isize) = rhs(isize) - A*rhs(isize-1)
    //-------------------------------------------------------------------
    matvec_sub(lhsa[isize], rhs[c][k-1][j-1][isize-1],
               rhs[c][k-1][j-1][isize]);

    //-------------------------------------------------------------------
    // B(isize) = B(isize) - C(isize-1)*A(isize)
    //-------------------------------------------------------------------
    matmul_sub(lhsa[isize], lhsc[c][k-1][j-1][isize-1], lhsb[isize]);

    //-------------------------------------------------------------------
    // multiply rhs() by b_inverse() and copy to rhs
    //-------------------------------------------------------------------
    binvrhs( lhsb[isize], rhs[c][k-1][j-1][isize] );

  }

#else //X_SOLVE_DIM == 1

  k = get_global_id(0) + start[c][2]+2;
  ksize = cell_size[c][2]-end[c][2]+1;
  if (k > ksize) return;

  qs    = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  rho_i = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  u     = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  rhs   = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  lhsc  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;

  int my_id = get_global_id(0);
  int my_offset1 = my_id * (MAX_CELL_DIM+2)*5*5;
  int my_offset2 = my_id * (MAX_CELL_DIM+4)*5*5;
  lhsa = (__global double (*)[5][5])&g_lhsa[my_offset1];
  lhsb = (__global double (*)[5][5])&g_lhsb[my_offset1];
  fjac = (__global double (*)[5][5])&g_fjac[my_offset2];
  njac = (__global double (*)[5][5])&g_njac[my_offset2];

  istart = 1;
  isize = cell_size[c][0];
  jsize = cell_size[c][1]-end[c][1]+1;

  lhsabinit(lhsa, lhsb, isize);

  for (j = start[c][1]+2; j <= jsize; j++) {

    //---------------------------------------------------------------------
    // This function computes the left hand side in the xi-direction
    //---------------------------------------------------------------------

    //---------------------------------------------------------------------
    // determine a (labeled f) and n jacobians for cell c
    //---------------------------------------------------------------------
    for (i = start[c][0]+1; i <= cell_size[c][0]-end[c][0]+2; i++) {

      tmp1 = rho_i[c][k-1][j-1][i-1];
      tmp2 = tmp1 * tmp1;
      tmp3 = tmp1 * tmp2;
      //-------------------------------------------------------------------
      // 
      //-------------------------------------------------------------------
      fjac[i][0][0] = 0.0;
      fjac[i][1][0] = 1.0;
      fjac[i][2][0] = 0.0;
      fjac[i][3][0] = 0.0;
      fjac[i][4][0] = 0.0;

      fjac[i][0][1] = -(u[c][k][j][i][1] * tmp2 * 
          u[c][k][j][i][1])
        + c2 * qs[c][k-1][j-1][i-1];
      fjac[i][1][1] = ( 2.0 - c2 )
        * ( u[c][k][j][i][1] * tmp1 );
      fjac[i][2][1] = - c2 * ( u[c][k][j][i][2] * tmp1 );
      fjac[i][3][1] = - c2 * ( u[c][k][j][i][3] * tmp1 );
      fjac[i][4][1] = c2;

      fjac[i][0][2] = - ( u[c][k][j][i][1]*u[c][k][j][i][2] ) * tmp2;
      fjac[i][1][2] = u[c][k][j][i][2] * tmp1;
      fjac[i][2][2] = u[c][k][j][i][1] * tmp1;
      fjac[i][3][2] = 0.0;
      fjac[i][4][2] = 0.0;

      fjac[i][0][3] = - ( u[c][k][j][i][1]*u[c][k][j][i][3] ) * tmp2;
      fjac[i][1][3] = u[c][k][j][i][3] * tmp1;
      fjac[i][2][3] = 0.0;
      fjac[i][3][3] = u[c][k][j][i][1] * tmp1;
      fjac[i][4][3] = 0.0;

      fjac[i][0][4] = ( c2 * 2.0 * qs[c][k-1][j-1][i-1]
          - c1 * ( u[c][k][j][i][4] * tmp1 ) )
        * ( u[c][k][j][i][1] * tmp1 );
      fjac[i][1][4] = c1 *  u[c][k][j][i][4] * tmp1 
        - c2
        * ( u[c][k][j][i][1]*u[c][k][j][i][1] * tmp2
            + qs[c][k-1][j-1][i-1] );
      fjac[i][2][4] = - c2 * ( u[c][k][j][i][2]*u[c][k][j][i][1] )
        * tmp2;
      fjac[i][3][4] = - c2 * ( u[c][k][j][i][3]*u[c][k][j][i][1] )
        * tmp2;
      fjac[i][4][4] = c1 * ( u[c][k][j][i][1] * tmp1 );

      njac[i][0][0] = 0.0;
      njac[i][1][0] = 0.0;
      njac[i][2][0] = 0.0;
      njac[i][3][0] = 0.0;
      njac[i][4][0] = 0.0;

      njac[i][0][1] = - con43 * c3c4 * tmp2 * u[c][k][j][i][1];
      njac[i][1][1] =   con43 * c3c4 * tmp1;
      njac[i][2][1] =   0.0;
      njac[i][3][1] =   0.0;
      njac[i][4][1] =   0.0;

      njac[i][0][2] = - c3c4 * tmp2 * u[c][k][j][i][2];
      njac[i][1][2] =   0.0;
      njac[i][2][2] =   c3c4 * tmp1;
      njac[i][3][2] =   0.0;
      njac[i][4][2] =   0.0;

      njac[i][0][3] = - c3c4 * tmp2 * u[c][k][j][i][3];
      njac[i][1][3] =   0.0;
      njac[i][2][3] =   0.0;
      njac[i][3][3] =   c3c4 * tmp1;
      njac[i][4][3] =   0.0;

      njac[i][0][4] = - ( con43 * c3c4
          - c1345 ) * tmp3 * (u[c][k][j][i][1]*u[c][k][j][i][1])
        - ( c3c4 - c1345 ) * tmp3 * (u[c][k][j][i][2]*u[c][k][j][i][2])
        - ( c3c4 - c1345 ) * tmp3 * (u[c][k][j][i][3]*u[c][k][j][i][3])
        - c1345 * tmp2 * u[c][k][j][i][4];

      njac[i][1][4] = ( con43 * c3c4
          - c1345 ) * tmp2 * u[c][k][j][i][1];
      njac[i][2][4] = ( c3c4 - c1345 ) * tmp2 * u[c][k][j][i][2];
      njac[i][3][4] = ( c3c4 - c1345 ) * tmp2 * u[c][k][j][i][3];
      njac[i][4][4] = ( c1345 ) * tmp1;

    }

    //---------------------------------------------------------------------
    // now jacobians set, so form left hand side in x direction
    //---------------------------------------------------------------------
    for (i = start[c][0]+2; i <= isize-end[c][0]+1; i++) {

      tmp1 = dt * tx1;
      tmp2 = dt * tx2;

      lhsa[i-1][0][0] = - tmp2 * fjac[i-1][0][0]
        - tmp1 * njac[i-1][0][0]
        - tmp1 * dx1; 
      lhsa[i-1][1][0] = - tmp2 * fjac[i-1][1][0]
        - tmp1 * njac[i-1][1][0];
      lhsa[i-1][2][0] = - tmp2 * fjac[i-1][2][0]
        - tmp1 * njac[i-1][2][0];
      lhsa[i-1][3][0] = - tmp2 * fjac[i-1][3][0]
        - tmp1 * njac[i-1][3][0];
      lhsa[i-1][4][0] = - tmp2 * fjac[i-1][4][0]
        - tmp1 * njac[i-1][4][0];

      lhsa[i-1][0][1] = - tmp2 * fjac[i-1][0][1]
        - tmp1 * njac[i-1][0][1];
      lhsa[i-1][1][1] = - tmp2 * fjac[i-1][1][1]
        - tmp1 * njac[i-1][1][1]
        - tmp1 * dx2;
      lhsa[i-1][2][1] = - tmp2 * fjac[i-1][2][1]
        - tmp1 * njac[i-1][2][1];
      lhsa[i-1][3][1] = - tmp2 * fjac[i-1][3][1]
        - tmp1 * njac[i-1][3][1];
      lhsa[i-1][4][1] = - tmp2 * fjac[i-1][4][1]
        - tmp1 * njac[i-1][4][1];

      lhsa[i-1][0][2] = - tmp2 * fjac[i-1][0][2]
        - tmp1 * njac[i-1][0][2];
      lhsa[i-1][1][2] = - tmp2 * fjac[i-1][1][2]
        - tmp1 * njac[i-1][1][2];
      lhsa[i-1][2][2] = - tmp2 * fjac[i-1][2][2]
        - tmp1 * njac[i-1][2][2]
        - tmp1 * dx3;
      lhsa[i-1][3][2] = - tmp2 * fjac[i-1][3][2]
        - tmp1 * njac[i-1][3][2];
      lhsa[i-1][4][2] = - tmp2 * fjac[i-1][4][2]
        - tmp1 * njac[i-1][4][2];

      lhsa[i-1][0][3] = - tmp2 * fjac[i-1][0][3]
        - tmp1 * njac[i-1][0][3];
      lhsa[i-1][1][3] = - tmp2 * fjac[i-1][1][3]
        - tmp1 * njac[i-1][1][3];
      lhsa[i-1][2][3] = - tmp2 * fjac[i-1][2][3]
        - tmp1 * njac[i-1][2][3];
      lhsa[i-1][3][3] = - tmp2 * fjac[i-1][3][3]
        - tmp1 * njac[i-1][3][3]
        - tmp1 * dx4;
      lhsa[i-1][4][3] = - tmp2 * fjac[i-1][4][3]
        - tmp1 * njac[i-1][4][3];

      lhsa[i-1][0][4] = - tmp2 * fjac[i-1][0][4]
        - tmp1 * njac[i-1][0][4];
      lhsa[i-1][1][4] = - tmp2 * fjac[i-1][1][4]
        - tmp1 * njac[i-1][1][4];
      lhsa[i-1][2][4] = - tmp2 * fjac[i-1][2][4]
        - tmp1 * njac[i-1][2][4];
      lhsa[i-1][3][4] = - tmp2 * fjac[i-1][3][4]
        - tmp1 * njac[i-1][3][4];
      lhsa[i-1][4][4] = - tmp2 * fjac[i-1][4][4]
        - tmp1 * njac[i-1][4][4]
        - tmp1 * dx5;

      lhsb[i-1][0][0] = 1.0
        + tmp1 * 2.0 * njac[i][0][0]
        + tmp1 * 2.0 * dx1;
      lhsb[i-1][1][0] = tmp1 * 2.0 * njac[i][1][0];
      lhsb[i-1][2][0] = tmp1 * 2.0 * njac[i][2][0];
      lhsb[i-1][3][0] = tmp1 * 2.0 * njac[i][3][0];
      lhsb[i-1][4][0] = tmp1 * 2.0 * njac[i][4][0];

      lhsb[i-1][0][1] = tmp1 * 2.0 * njac[i][0][1];
      lhsb[i-1][1][1] = 1.0
        + tmp1 * 2.0 * njac[i][1][1]
        + tmp1 * 2.0 * dx2;
      lhsb[i-1][2][1] = tmp1 * 2.0 * njac[i][2][1];
      lhsb[i-1][3][1] = tmp1 * 2.0 * njac[i][3][1];
      lhsb[i-1][4][1] = tmp1 * 2.0 * njac[i][4][1];

      lhsb[i-1][0][2] = tmp1 * 2.0 * njac[i][0][2];
      lhsb[i-1][1][2] = tmp1 * 2.0 * njac[i][1][2];
      lhsb[i-1][2][2] = 1.0
        + tmp1 * 2.0 * njac[i][2][2]
        + tmp1 * 2.0 * dx3;
      lhsb[i-1][3][2] = tmp1 * 2.0 * njac[i][3][2];
      lhsb[i-1][4][2] = tmp1 * 2.0 * njac[i][4][2];

      lhsb[i-1][0][3] = tmp1 * 2.0 * njac[i][0][3];
      lhsb[i-1][1][3] = tmp1 * 2.0 * njac[i][1][3];
      lhsb[i-1][2][3] = tmp1 * 2.0 * njac[i][2][3];
      lhsb[i-1][3][3] = 1.0
        + tmp1 * 2.0 * njac[i][3][3]
        + tmp1 * 2.0 * dx4;
      lhsb[i-1][4][3] = tmp1 * 2.0 * njac[i][4][3];

      lhsb[i-1][0][4] = tmp1 * 2.0 * njac[i][0][4];
      lhsb[i-1][1][4] = tmp1 * 2.0 * njac[i][1][4];
      lhsb[i-1][2][4] = tmp1 * 2.0 * njac[i][2][4];
      lhsb[i-1][3][4] = tmp1 * 2.0 * njac[i][3][4];
      lhsb[i-1][4][4] = 1.0
        + tmp1 * 2.0 * njac[i][4][4]
        + tmp1 * 2.0 * dx5;

      lhsc[c][k-1][j-1][i-1][0][0] =  tmp2 * fjac[i+1][0][0]
        - tmp1 * njac[i+1][0][0]
        - tmp1 * dx1;
      lhsc[c][k-1][j-1][i-1][1][0] =  tmp2 * fjac[i+1][1][0]
        - tmp1 * njac[i+1][1][0];
      lhsc[c][k-1][j-1][i-1][2][0] =  tmp2 * fjac[i+1][2][0]
        - tmp1 * njac[i+1][2][0];
      lhsc[c][k-1][j-1][i-1][3][0] =  tmp2 * fjac[i+1][3][0]
        - tmp1 * njac[i+1][3][0];
      lhsc[c][k-1][j-1][i-1][4][0] =  tmp2 * fjac[i+1][4][0]
        - tmp1 * njac[i+1][4][0];

      lhsc[c][k-1][j-1][i-1][0][1] =  tmp2 * fjac[i+1][0][1]
        - tmp1 * njac[i+1][0][1];
      lhsc[c][k-1][j-1][i-1][1][1] =  tmp2 * fjac[i+1][1][1]
        - tmp1 * njac[i+1][1][1]
        - tmp1 * dx2;
      lhsc[c][k-1][j-1][i-1][2][1] =  tmp2 * fjac[i+1][2][1]
        - tmp1 * njac[i+1][2][1];
      lhsc[c][k-1][j-1][i-1][3][1] =  tmp2 * fjac[i+1][3][1]
        - tmp1 * njac[i+1][3][1];
      lhsc[c][k-1][j-1][i-1][4][1] =  tmp2 * fjac[i+1][4][1]
        - tmp1 * njac[i+1][4][1];

      lhsc[c][k-1][j-1][i-1][0][2] =  tmp2 * fjac[i+1][0][2]
        - tmp1 * njac[i+1][0][2];
      lhsc[c][k-1][j-1][i-1][1][2] =  tmp2 * fjac[i+1][1][2]
        - tmp1 * njac[i+1][1][2];
      lhsc[c][k-1][j-1][i-1][2][2] =  tmp2 * fjac[i+1][2][2]
        - tmp1 * njac[i+1][2][2]
        - tmp1 * dx3;
      lhsc[c][k-1][j-1][i-1][3][2] =  tmp2 * fjac[i+1][3][2]
        - tmp1 * njac[i+1][3][2];
      lhsc[c][k-1][j-1][i-1][4][2] =  tmp2 * fjac[i+1][4][2]
        - tmp1 * njac[i+1][4][2];

      lhsc[c][k-1][j-1][i-1][0][3] =  tmp2 * fjac[i+1][0][3]
        - tmp1 * njac[i+1][0][3];
      lhsc[c][k-1][j-1][i-1][1][3] =  tmp2 * fjac[i+1][1][3]
        - tmp1 * njac[i+1][1][3];
      lhsc[c][k-1][j-1][i-1][2][3] =  tmp2 * fjac[i+1][2][3]
        - tmp1 * njac[i+1][2][3];
      lhsc[c][k-1][j-1][i-1][3][3] =  tmp2 * fjac[i+1][3][3]
        - tmp1 * njac[i+1][3][3]
        - tmp1 * dx4;
      lhsc[c][k-1][j-1][i-1][4][3] =  tmp2 * fjac[i+1][4][3]
        - tmp1 * njac[i+1][4][3];

      lhsc[c][k-1][j-1][i-1][0][4] =  tmp2 * fjac[i+1][0][4]
        - tmp1 * njac[i+1][0][4];
      lhsc[c][k-1][j-1][i-1][1][4] =  tmp2 * fjac[i+1][1][4]
        - tmp1 * njac[i+1][1][4];
      lhsc[c][k-1][j-1][i-1][2][4] =  tmp2 * fjac[i+1][2][4]
        - tmp1 * njac[i+1][2][4];
      lhsc[c][k-1][j-1][i-1][3][4] =  tmp2 * fjac[i+1][3][4]
        - tmp1 * njac[i+1][3][4];
      lhsc[c][k-1][j-1][i-1][4][4] =  tmp2 * fjac[i+1][4][4]
        - tmp1 * njac[i+1][4][4]
        - tmp1 * dx5;

    }

    //---------------------------------------------------------------------
    // outer most do loops - sweeping in i direction
    //---------------------------------------------------------------------
    if (first == 1) { 

      //-------------------------------------------------------------------
      // multiply c(istart,j,k) by b_inverse and copy back to c
      // multiply rhs(istart) by b_inverse(istart) and copy to rhs
      //-------------------------------------------------------------------
      binvcrhs( lhsb[istart], lhsc[c][k-1][j-1][istart],
                rhs[c][k-1][j-1][istart] );

    }

    //---------------------------------------------------------------------
    // begin inner most do loop
    // do all the elements of the cell unless last 
    //---------------------------------------------------------------------
    for (i = istart+first; i <= isize-last; i++) {

      //-------------------------------------------------------------------
      // rhs(i) = rhs(i) - A*rhs(i-1)
      //-------------------------------------------------------------------
      matvec_sub(lhsa[i], rhs[c][k-1][j-1][i-1], rhs[c][k-1][j-1][i]);

      //-------------------------------------------------------------------
      // B(i) = B(i) - C(i-1)*A(i)
      //-------------------------------------------------------------------
      matmul_sub(lhsa[i], lhsc[c][k-1][j-1][i-1], lhsb[i]);


      //-------------------------------------------------------------------
      // multiply c(i,j,k) by b_inverse and copy back to c
      // multiply rhs(1,j,k) by b_inverse(1,j,k) and copy to rhs
      //-------------------------------------------------------------------
      binvcrhs( lhsb[i], lhsc[c][k-1][j-1][i], rhs[c][k-1][j-1][i] );

    }

    //---------------------------------------------------------------------
    // Now finish up special cases for last cell
    //---------------------------------------------------------------------
    if (last == 1) {

      //-------------------------------------------------------------------
      // rhs(isize) = rhs(isize) - A*rhs(isize-1)
      //-------------------------------------------------------------------
      matvec_sub(lhsa[isize], rhs[c][k-1][j-1][isize-1],
                 rhs[c][k-1][j-1][isize]);

      //-------------------------------------------------------------------
      // B(isize) = B(isize) - C(isize-1)*A(isize)
      //-------------------------------------------------------------------
      matmul_sub(lhsa[isize], lhsc[c][k-1][j-1][isize-1], lhsb[isize]);

      //-------------------------------------------------------------------
      // multiply rhs() by b_inverse() and copy to rhs
      //-------------------------------------------------------------------
      binvrhs( lhsb[isize], rhs[c][k-1][j-1][isize] );

    }

  }
#endif
}


//---------------------------------------------------------------------
// unpack C'(-1) and rhs'(-1) for
// all i and k
//---------------------------------------------------------------------
__kernel void y_unpack_solve_info(__global double *g_lhsc,
                                  __global double *g_rhs,
                                  __global double *out_buffer,
                                  int c)
{
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];

  int i, k, m, n, ptr, jstart;
  jstart = 0;

  k = get_global_id(1) + 1;
  i = get_global_id(0) + 1;
  if (k >= KMAX+1 || i >= IMAX+1) return;

  lhsc = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  ptr = (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE) * (IMAX*(k-1) + (i-1));

  for (n = 0; n < BLOCK_SIZE; n++) {
    for (m = 0; m < BLOCK_SIZE; m++) {
      lhsc[c][k][jstart][i][n][m] = out_buffer[ptr+m];
    }
    ptr = ptr+BLOCK_SIZE;
  }
  for (n = 0; n < BLOCK_SIZE; n++) {
    rhs[c][k][jstart][i][n] = out_buffer[ptr+n];
  }
}


__kernel void y_send_solve_info(__global double *g_lhsc,
                                __global double *g_rhs,
                                __global double *in_buffer,
                                __global int *g_cell_size,
                                int c)
{
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global int (*cell_size)[3];

  int i, k, m, n, jsize, ptr;

  k = get_global_id(1) + 1;
  i = get_global_id(0) + 1;
  if (k >= KMAX+1 || i >= IMAX+1) return;

  lhsc = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  cell_size = (__global int (*)[3])g_cell_size;

  jsize = cell_size[c][1];
  ptr = (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE) * (IMAX*(k-1) + (i-1));

  for (n = 0; n < BLOCK_SIZE; n++) {
    for (m = 0; m < BLOCK_SIZE; m++) {
      in_buffer[ptr+m] = lhsc[c][k][jsize][i][n][m];
    }
    ptr = ptr+BLOCK_SIZE;
  }
  for (n = 0; n < BLOCK_SIZE; n++) {
    in_buffer[ptr+n] = rhs[c][k][jsize][i][n];
  }
}


__kernel void y_send_backsub_info(__global double *g_rhs,
                                  __global double *in_buffer,
                                  int c)
{
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];

  int i, k, n, ptr, jstart;

  k = get_global_id(1) + 1;
  i = get_global_id(0) + 1;
  if (k >= KMAX+1 || i >= IMAX+1) return;

  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  jstart = 1;
  ptr = BLOCK_SIZE * (IMAX*(k-1) + (i-1));

  for (n = 0; n < BLOCK_SIZE; n++) {
    in_buffer[ptr+n] = rhs[c][k][jstart][i][n];
  }
}


//---------------------------------------------------------------------
// unpack U(jsize) for all i and k
//---------------------------------------------------------------------
__kernel void y_unpack_backsub_info(__global double *g_backsub_info,
                                    __global double *out_buffer,
                                    int c)
{
  __global double (*backsub_info)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5];
  
  int i, k, n, ptr;

  k = get_global_id(1);
  i = get_global_id(0);
  if (k >= KMAX || i >= IMAX) return;

  backsub_info = (__global double (*)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5])g_backsub_info;

  ptr = BLOCK_SIZE * (IMAX*k + i);
  for (n = 0; n < BLOCK_SIZE; n++) {
    backsub_info[c][k][i][n] = out_buffer[ptr+n];
  }
}


__kernel void y_backsubstitute(__global double *g_rhs,
                               __global double *g_lhsc,
                               __global double *g_backsub_info,
                               __global int *g_cell_size,
                               __global int *g_start,
                               __global int *g_end,
                               int last,
                               int c)
{
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*backsub_info)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5];
  __global int (*cell_size)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  int i, k;
  int m, n, j, jsize, isize, ksize, jstart;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

  k = get_global_id(1) + start[c][2]+1;
  ksize = cell_size[c][2]-end[c][2];
  if (k > ksize) return;

  i = get_global_id(0) + start[c][0]+1;
  isize = cell_size[c][0]-end[c][0];
  if (i > isize) return;

  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  lhsc = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  backsub_info = (__global double (*)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5])g_backsub_info;

  jstart = 1;
  jsize = cell_size[c][1];

  if (last == 0) {
    //-------------------------------------------------------------------
    // U(jsize) uses info from previous cell if not last cell
    //-------------------------------------------------------------------
    for (m = 0; m < BLOCK_SIZE; m++) {
      for (n = 0; n < BLOCK_SIZE; n++) {
        rhs[c][k][jsize][i][m] = rhs[c][k][jsize][i][m] 
          - lhsc[c][k][jsize][i][n][m]*
          backsub_info[c][k-1][i-1][n];
      }
    }
  }

  for (j = jsize-1; j >= jstart; j--) {
    for (m = 0; m < BLOCK_SIZE; m++) {
      for (n = 0; n < BLOCK_SIZE; n++) {
        rhs[c][k][j][i][m] = rhs[c][k][j][i][m] 
          - lhsc[c][k][j][i][n][m]*rhs[c][k][j+1][i][n];
      }
    }
  }
}


__kernel void y_solve_cell(__global double *g_qs,
                           __global double *g_u,
                           __global double *g_rhs,
                           __global double *g_lhsc,
                           __global double *g_lhsa,
                           __global double *g_lhsb,
                           __global double *g_fjac,
                           __global double *g_njac,
                           __global double *g_utmp,
                           __global int *g_cell_size,
                           __global int *g_start,
                           __global int *g_end,
                           int first,
                           int last,
                           int c)
{
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*lhsa)[5][5];
  __global double (*lhsb)[5][5];
  __global double (*fjac)[5][5];
  __global double (*njac)[5][5];
  __global double (*utmp)[6];
  __global int (*cell_size)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  int i, j, k, isize, ksize, jsize, jstart;
  double tmp1, tmp2, tmp3;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

#if Y_SOLVE_DIM == 2
  k = get_global_id(1) + start[c][2]+2;
  ksize = cell_size[c][2]-end[c][2]+1;
  if (k > ksize) return;

  i = get_global_id(0) + start[c][0]+2;
  isize = cell_size[c][0]-end[c][0]+1;
  if (i > isize) return;

  qs     = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  u      = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  rhs    = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  lhsc   = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;

  int d0_size = isize - start[c][0] - 1;
  int my_id = get_global_id(1)*d0_size + get_global_id(0);
  int my_offset1 = my_id * (MAX_CELL_DIM+2)*5*5;
  int my_offset2 = my_id * (MAX_CELL_DIM+4)*5*5;
  int my_offset3 = my_id * (JMAX+4)*6;
  lhsa = (__global double (*)[5][5])&g_lhsa[my_offset1];
  lhsb = (__global double (*)[5][5])&g_lhsb[my_offset1];
  fjac = (__global double (*)[5][5])&g_fjac[my_offset2];
  njac = (__global double (*)[5][5])&g_njac[my_offset2];
  utmp = (__global double (*)[6])&g_utmp[my_offset3];

  jstart = 1;
  jsize = cell_size[c][1];

  lhsabinit(lhsa, lhsb, jsize);

  //---------------------------------------------------------------------
  // This function computes the left hand side for the three y-factors   
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // Compute the indices for storing the tri-diagonal matrix;
  // determine a (labeled f) and n jacobians for cell c
  //---------------------------------------------------------------------
  for (j = start[c][1]+1; j <= cell_size[c][1]-end[c][1]+2; j++) {
    utmp[j][0] = 1.0 / u[c][k][j][i][0];
    utmp[j][1] = u[c][k][j][i][1];
    utmp[j][2] = u[c][k][j][i][2];
    utmp[j][3] = u[c][k][j][i][3];
    utmp[j][4] = u[c][k][j][i][4];
    utmp[j][5] = qs[c][k-1][j-1][i-1];
  }

  for (j = start[c][1]+1; j <= cell_size[c][1]-end[c][1]+2; j++) {

    tmp1 = utmp[j][0];
    tmp2 = tmp1 * tmp1;
    tmp3 = tmp1 * tmp2;

    fjac[j][0][0] = 0.0;
    fjac[j][1][0] = 0.0;
    fjac[j][2][0] = 1.0;
    fjac[j][3][0] = 0.0;
    fjac[j][4][0] = 0.0;

    fjac[j][0][1] = - ( utmp[j][1]*utmp[j][2] )
      * tmp2;
    fjac[j][1][1] = utmp[j][2] * tmp1;
    fjac[j][2][1] = utmp[j][1] * tmp1;
    fjac[j][3][1] = 0.0;
    fjac[j][4][1] = 0.0;

    fjac[j][0][2] = - ( utmp[j][2]*utmp[j][2]*tmp2)
      + c2 * utmp[j][5];
    fjac[j][1][2] = - c2 *  utmp[j][1] * tmp1;
    fjac[j][2][2] = ( 2.0 - c2 )
      *  utmp[j][2] * tmp1;
    fjac[j][3][2] = - c2 * utmp[j][3] * tmp1;
    fjac[j][4][2] = c2;

    fjac[j][0][3] = - ( utmp[j][2]*utmp[j][3] )
      * tmp2;
    fjac[j][1][3] = 0.0;
    fjac[j][2][3] = utmp[j][3] * tmp1;
    fjac[j][3][3] = utmp[j][2] * tmp1;
    fjac[j][4][3] = 0.0;

    fjac[j][0][4] = ( c2 * 2.0 * utmp[j][5]
        - c1 * utmp[j][4] * tmp1 ) 
      * utmp[j][2] * tmp1;
    fjac[j][1][4] = - c2 * utmp[j][1]*utmp[j][2] 
      * tmp2;
    fjac[j][2][4] = c1 * utmp[j][4] * tmp1 
      - c2 * ( utmp[j][5]
          + utmp[j][2]*utmp[j][2] * tmp2 );
    fjac[j][3][4] = - c2 * ( utmp[j][2]*utmp[j][3] )
      * tmp2;
    fjac[j][4][4] = c1 * utmp[j][2] * tmp1;

    njac[j][0][0] = 0.0;
    njac[j][1][0] = 0.0;
    njac[j][2][0] = 0.0;
    njac[j][3][0] = 0.0;
    njac[j][4][0] = 0.0;

    njac[j][0][1] = - c3c4 * tmp2 * utmp[j][1];
    njac[j][1][1] =   c3c4 * tmp1;
    njac[j][2][1] =   0.0;
    njac[j][3][1] =   0.0;
    njac[j][4][1] =   0.0;

    njac[j][0][2] = - con43 * c3c4 * tmp2 * utmp[j][2];
    njac[j][1][2] =   0.0;
    njac[j][2][2] =   con43 * c3c4 * tmp1;
    njac[j][3][2] =   0.0;
    njac[j][4][2] =   0.0;

    njac[j][0][3] = - c3c4 * tmp2 * utmp[j][3];
    njac[j][1][3] =   0.0;
    njac[j][2][3] =   0.0;
    njac[j][3][3] =   c3c4 * tmp1;
    njac[j][4][3] =   0.0;

    njac[j][0][4] = - (  c3c4
        - c1345 ) * tmp3 * (utmp[j][1]*utmp[j][1])
      - ( con43 * c3c4
          - c1345 ) * tmp3 * (utmp[j][2]*utmp[j][2])
      - ( c3c4 - c1345 ) * tmp3 * (utmp[j][3]*utmp[j][3])
      - c1345 * tmp2 * utmp[j][4];

    njac[j][1][4] = (  c3c4 - c1345 ) * tmp2 * utmp[j][1];
    njac[j][2][4] = ( con43 * c3c4
        - c1345 ) * tmp2 * utmp[j][2];
    njac[j][3][4] = ( c3c4 - c1345 ) * tmp2 * utmp[j][3];
    njac[j][4][4] = ( c1345 ) * tmp1;

  }

  //---------------------------------------------------------------------
  // now joacobians set, so form left hand side in y direction
  //---------------------------------------------------------------------
  for (j = start[c][1]+2; j <= jsize-end[c][1]+1; j++) {

    tmp1 = dt * ty1;
    tmp2 = dt * ty2;

    lhsa[j-1][0][0] = - tmp2 * fjac[j-1][0][0]
      - tmp1 * njac[j-1][0][0]
      - tmp1 * dy1; 
    lhsa[j-1][1][0] = - tmp2 * fjac[j-1][1][0]
      - tmp1 * njac[j-1][1][0];
    lhsa[j-1][2][0] = - tmp2 * fjac[j-1][2][0]
      - tmp1 * njac[j-1][2][0];
    lhsa[j-1][3][0] = - tmp2 * fjac[j-1][3][0]
      - tmp1 * njac[j-1][3][0];
    lhsa[j-1][4][0] = - tmp2 * fjac[j-1][4][0]
      - tmp1 * njac[j-1][4][0];

    lhsa[j-1][0][1] = - tmp2 * fjac[j-1][0][1]
      - tmp1 * njac[j-1][0][1];
    lhsa[j-1][1][1] = - tmp2 * fjac[j-1][1][1]
      - tmp1 * njac[j-1][1][1]
      - tmp1 * dy2;
    lhsa[j-1][2][1] = - tmp2 * fjac[j-1][2][1]
      - tmp1 * njac[j-1][2][1];
    lhsa[j-1][3][1] = - tmp2 * fjac[j-1][3][1]
      - tmp1 * njac[j-1][3][1];
    lhsa[j-1][4][1] = - tmp2 * fjac[j-1][4][1]
      - tmp1 * njac[j-1][4][1];

    lhsa[j-1][0][2] = - tmp2 * fjac[j-1][0][2]
      - tmp1 * njac[j-1][0][2];
    lhsa[j-1][1][2] = - tmp2 * fjac[j-1][1][2]
      - tmp1 * njac[j-1][1][2];
    lhsa[j-1][2][2] = - tmp2 * fjac[j-1][2][2]
      - tmp1 * njac[j-1][2][2]
      - tmp1 * dy3;
    lhsa[j-1][3][2] = - tmp2 * fjac[j-1][3][2]
      - tmp1 * njac[j-1][3][2];
    lhsa[j-1][4][2] = - tmp2 * fjac[j-1][4][2]
      - tmp1 * njac[j-1][4][2];

    lhsa[j-1][0][3] = - tmp2 * fjac[j-1][0][3]
      - tmp1 * njac[j-1][0][3];
    lhsa[j-1][1][3] = - tmp2 * fjac[j-1][1][3]
      - tmp1 * njac[j-1][1][3];
    lhsa[j-1][2][3] = - tmp2 * fjac[j-1][2][3]
      - tmp1 * njac[j-1][2][3];
    lhsa[j-1][3][3] = - tmp2 * fjac[j-1][3][3]
      - tmp1 * njac[j-1][3][3]
      - tmp1 * dy4;
    lhsa[j-1][4][3] = - tmp2 * fjac[j-1][4][3]
      - tmp1 * njac[j-1][4][3];

    lhsa[j-1][0][4] = - tmp2 * fjac[j-1][0][4]
      - tmp1 * njac[j-1][0][4];
    lhsa[j-1][1][4] = - tmp2 * fjac[j-1][1][4]
      - tmp1 * njac[j-1][1][4];
    lhsa[j-1][2][4] = - tmp2 * fjac[j-1][2][4]
      - tmp1 * njac[j-1][2][4];
    lhsa[j-1][3][4] = - tmp2 * fjac[j-1][3][4]
      - tmp1 * njac[j-1][3][4];
    lhsa[j-1][4][4] = - tmp2 * fjac[j-1][4][4]
      - tmp1 * njac[j-1][4][4]
      - tmp1 * dy5;

    lhsb[j-1][0][0] = 1.0
      + tmp1 * 2.0 * njac[j][0][0]
      + tmp1 * 2.0 * dy1;
    lhsb[j-1][1][0] = tmp1 * 2.0 * njac[j][1][0];
    lhsb[j-1][2][0] = tmp1 * 2.0 * njac[j][2][0];
    lhsb[j-1][3][0] = tmp1 * 2.0 * njac[j][3][0];
    lhsb[j-1][4][0] = tmp1 * 2.0 * njac[j][4][0];

    lhsb[j-1][0][1] = tmp1 * 2.0 * njac[j][0][1];
    lhsb[j-1][1][1] = 1.0
      + tmp1 * 2.0 * njac[j][1][1]
      + tmp1 * 2.0 * dy2;
    lhsb[j-1][2][1] = tmp1 * 2.0 * njac[j][2][1];
    lhsb[j-1][3][1] = tmp1 * 2.0 * njac[j][3][1];
    lhsb[j-1][4][1] = tmp1 * 2.0 * njac[j][4][1];

    lhsb[j-1][0][2] = tmp1 * 2.0 * njac[j][0][2];
    lhsb[j-1][1][2] = tmp1 * 2.0 * njac[j][1][2];
    lhsb[j-1][2][2] = 1.0
      + tmp1 * 2.0 * njac[j][2][2]
      + tmp1 * 2.0 * dy3;
    lhsb[j-1][3][2] = tmp1 * 2.0 * njac[j][3][2];
    lhsb[j-1][4][2] = tmp1 * 2.0 * njac[j][4][2];

    lhsb[j-1][0][3] = tmp1 * 2.0 * njac[j][0][3];
    lhsb[j-1][1][3] = tmp1 * 2.0 * njac[j][1][3];
    lhsb[j-1][2][3] = tmp1 * 2.0 * njac[j][2][3];
    lhsb[j-1][3][3] = 1.0
      + tmp1 * 2.0 * njac[j][3][3]
      + tmp1 * 2.0 * dy4;
    lhsb[j-1][4][3] = tmp1 * 2.0 * njac[j][4][3];

    lhsb[j-1][0][4] = tmp1 * 2.0 * njac[j][0][4];
    lhsb[j-1][1][4] = tmp1 * 2.0 * njac[j][1][4];
    lhsb[j-1][2][4] = tmp1 * 2.0 * njac[j][2][4];
    lhsb[j-1][3][4] = tmp1 * 2.0 * njac[j][3][4];
    lhsb[j-1][4][4] = 1.0
      + tmp1 * 2.0 * njac[j][4][4] 
      + tmp1 * 2.0 * dy5;

    lhsc[c][k-1][j-1][i-1][0][0] =  tmp2 * fjac[j+1][0][0]
      - tmp1 * njac[j+1][0][0]
      - tmp1 * dy1;
    lhsc[c][k-1][j-1][i-1][1][0] =  tmp2 * fjac[j+1][1][0]
      - tmp1 * njac[j+1][1][0];
    lhsc[c][k-1][j-1][i-1][2][0] =  tmp2 * fjac[j+1][2][0]
      - tmp1 * njac[j+1][2][0];
    lhsc[c][k-1][j-1][i-1][3][0] =  tmp2 * fjac[j+1][3][0]
      - tmp1 * njac[j+1][3][0];
    lhsc[c][k-1][j-1][i-1][4][0] =  tmp2 * fjac[j+1][4][0]
      - tmp1 * njac[j+1][4][0];

    lhsc[c][k-1][j-1][i-1][0][1] =  tmp2 * fjac[j+1][0][1]
      - tmp1 * njac[j+1][0][1];
    lhsc[c][k-1][j-1][i-1][1][1] =  tmp2 * fjac[j+1][1][1]
      - tmp1 * njac[j+1][1][1]
      - tmp1 * dy2;
    lhsc[c][k-1][j-1][i-1][2][1] =  tmp2 * fjac[j+1][2][1]
      - tmp1 * njac[j+1][2][1];
    lhsc[c][k-1][j-1][i-1][3][1] =  tmp2 * fjac[j+1][3][1]
      - tmp1 * njac[j+1][3][1];
    lhsc[c][k-1][j-1][i-1][4][1] =  tmp2 * fjac[j+1][4][1]
      - tmp1 * njac[j+1][4][1];

    lhsc[c][k-1][j-1][i-1][0][2] =  tmp2 * fjac[j+1][0][2]
      - tmp1 * njac[j+1][0][2];
    lhsc[c][k-1][j-1][i-1][1][2] =  tmp2 * fjac[j+1][1][2]
      - tmp1 * njac[j+1][1][2];
    lhsc[c][k-1][j-1][i-1][2][2] =  tmp2 * fjac[j+1][2][2]
      - tmp1 * njac[j+1][2][2]
      - tmp1 * dy3;
    lhsc[c][k-1][j-1][i-1][3][2] =  tmp2 * fjac[j+1][3][2]
      - tmp1 * njac[j+1][3][2];
    lhsc[c][k-1][j-1][i-1][4][2] =  tmp2 * fjac[j+1][4][2]
      - tmp1 * njac[j+1][4][2];

    lhsc[c][k-1][j-1][i-1][0][3] =  tmp2 * fjac[j+1][0][3]
      - tmp1 * njac[j+1][0][3];
    lhsc[c][k-1][j-1][i-1][1][3] =  tmp2 * fjac[j+1][1][3]
      - tmp1 * njac[j+1][1][3];
    lhsc[c][k-1][j-1][i-1][2][3] =  tmp2 * fjac[j+1][2][3]
      - tmp1 * njac[j+1][2][3];
    lhsc[c][k-1][j-1][i-1][3][3] =  tmp2 * fjac[j+1][3][3]
      - tmp1 * njac[j+1][3][3]
      - tmp1 * dy4;
    lhsc[c][k-1][j-1][i-1][4][3] =  tmp2 * fjac[j+1][4][3]
      - tmp1 * njac[j+1][4][3];

    lhsc[c][k-1][j-1][i-1][0][4] =  tmp2 * fjac[j+1][0][4]
      - tmp1 * njac[j+1][0][4];
    lhsc[c][k-1][j-1][i-1][1][4] =  tmp2 * fjac[j+1][1][4]
      - tmp1 * njac[j+1][1][4];
    lhsc[c][k-1][j-1][i-1][2][4] =  tmp2 * fjac[j+1][2][4]
      - tmp1 * njac[j+1][2][4];
    lhsc[c][k-1][j-1][i-1][3][4] =  tmp2 * fjac[j+1][3][4]
      - tmp1 * njac[j+1][3][4];
    lhsc[c][k-1][j-1][i-1][4][4] =  tmp2 * fjac[j+1][4][4]
      - tmp1 * njac[j+1][4][4]
      - tmp1 * dy5;

  }


  //---------------------------------------------------------------------
  // outer most do loops - sweeping in i direction
  //---------------------------------------------------------------------
  if (first == 1) { 

    //-------------------------------------------------------------------
    // multiply c(i,jstart,k) by b_inverse and copy back to c
    // multiply rhs(jstart) by b_inverse(jstart) and copy to rhs
    //-------------------------------------------------------------------
    binvcrhs( lhsb[jstart], lhsc[c][k-1][jstart][i-1],
               rhs[c][k-1][jstart][i-1] );

  }

  //---------------------------------------------------------------------
  // begin inner most do loop
  // do all the elements of the cell unless last 
  //---------------------------------------------------------------------
  for (j = jstart+first; j <= jsize-last; j++) {

    //-------------------------------------------------------------------
    // subtract A*lhs_vector(j-1) from lhs_vector(j)
    // 
    // rhs(j) = rhs(j) - A*rhs(j-1)
    //-------------------------------------------------------------------
    matvec_sub(lhsa[j], rhs[c][k-1][j-1][i-1], rhs[c][k-1][j][i-1]);

    //-------------------------------------------------------------------
    // B(j) = B(j) - C(j-1)*A(j)
    //-------------------------------------------------------------------
    matmul_sub(lhsa[j], lhsc[c][k-1][j-1][i-1], lhsb[j]);

    //-------------------------------------------------------------------
    // multiply c(i,j,k) by b_inverse and copy back to c
    // multiply rhs(i,1,k) by b_inverse(i,1,k) and copy to rhs
    //-------------------------------------------------------------------
    binvcrhs( lhsb[j], lhsc[c][k-1][j][i-1], rhs[c][k-1][j][i-1] );

  }

  //---------------------------------------------------------------------
  // Now finish up special cases for last cell
  //---------------------------------------------------------------------
  if (last == 1) {

    //-------------------------------------------------------------------
    // rhs(jsize) = rhs(jsize) - A*rhs(jsize-1)
    //-------------------------------------------------------------------
    matvec_sub(lhsa[jsize], rhs[c][k-1][jsize-1][i-1],
               rhs[c][k-1][jsize][i-1]);

    //-------------------------------------------------------------------
    // B(jsize) = B(jsize) - C(jsize-1)*A(jsize)
    // matmul_sub(aa,i,jsize,k,c,
    // $              cc,i,jsize-1,k,c,bb,i,jsize,k,c)
    //-------------------------------------------------------------------
    matmul_sub(lhsa[jsize], lhsc[c][k-1][jsize-1][i-1], lhsb[jsize]);

    //-------------------------------------------------------------------
    // multiply rhs(jsize) by b_inverse(jsize) and copy to rhs
    //-------------------------------------------------------------------
    binvrhs( lhsb[jsize], rhs[c][k-1][jsize][i-1] );

  }

#else //Y_SOLVE_DIM == 1

  k = get_global_id(0) + start[c][2]+2;
  ksize = cell_size[c][2]-end[c][2]+1;
  if (k > ksize) return;

  qs     = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  u      = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  rhs    = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  lhsc   = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;

  int my_id = get_global_id(0);
  int my_offset1 = my_id * (MAX_CELL_DIM+2)*5*5;
  int my_offset2 = my_id * (MAX_CELL_DIM+4)*5*5;
  int my_offset3 = my_id * (JMAX+4)*6;
  lhsa = (__global double (*)[5][5])&g_lhsa[my_offset1];
  lhsb = (__global double (*)[5][5])&g_lhsb[my_offset1];
  fjac = (__global double (*)[5][5])&g_fjac[my_offset2];
  njac = (__global double (*)[5][5])&g_njac[my_offset2];
  utmp = (__global double (*)[6])&g_utmp[my_offset3];

  jstart = 1;
  isize = cell_size[c][0]-end[c][0]+1;
  jsize = cell_size[c][1];

  lhsabinit(lhsa, lhsb, jsize);

  for (i = start[c][0]+2; i <= isize; i++) {

    //---------------------------------------------------------------------
    // This function computes the left hand side for the three y-factors   
    //---------------------------------------------------------------------

    //---------------------------------------------------------------------
    // Compute the indices for storing the tri-diagonal matrix;
    // determine a (labeled f) and n jacobians for cell c
    //---------------------------------------------------------------------
    for (j = start[c][1]+1; j <= cell_size[c][1]-end[c][1]+2; j++) {
      utmp[j][0] = 1.0 / u[c][k][j][i][0];
      utmp[j][1] = u[c][k][j][i][1];
      utmp[j][2] = u[c][k][j][i][2];
      utmp[j][3] = u[c][k][j][i][3];
      utmp[j][4] = u[c][k][j][i][4];
      utmp[j][5] = qs[c][k-1][j-1][i-1];
    }

    for (j = start[c][1]+1; j <= cell_size[c][1]-end[c][1]+2; j++) {

      tmp1 = utmp[j][0];
      tmp2 = tmp1 * tmp1;
      tmp3 = tmp1 * tmp2;

      fjac[j][0][0] = 0.0;
      fjac[j][1][0] = 0.0;
      fjac[j][2][0] = 1.0;
      fjac[j][3][0] = 0.0;
      fjac[j][4][0] = 0.0;

      fjac[j][0][1] = - ( utmp[j][1]*utmp[j][2] )
        * tmp2;
      fjac[j][1][1] = utmp[j][2] * tmp1;
      fjac[j][2][1] = utmp[j][1] * tmp1;
      fjac[j][3][1] = 0.0;
      fjac[j][4][1] = 0.0;

      fjac[j][0][2] = - ( utmp[j][2]*utmp[j][2]*tmp2)
        + c2 * utmp[j][5];
      fjac[j][1][2] = - c2 *  utmp[j][1] * tmp1;
      fjac[j][2][2] = ( 2.0 - c2 )
        *  utmp[j][2] * tmp1;
      fjac[j][3][2] = - c2 * utmp[j][3] * tmp1;
      fjac[j][4][2] = c2;

      fjac[j][0][3] = - ( utmp[j][2]*utmp[j][3] )
        * tmp2;
      fjac[j][1][3] = 0.0;
      fjac[j][2][3] = utmp[j][3] * tmp1;
      fjac[j][3][3] = utmp[j][2] * tmp1;
      fjac[j][4][3] = 0.0;

      fjac[j][0][4] = ( c2 * 2.0 * utmp[j][5]
          - c1 * utmp[j][4] * tmp1 ) 
        * utmp[j][2] * tmp1;
      fjac[j][1][4] = - c2 * utmp[j][1]*utmp[j][2] 
        * tmp2;
      fjac[j][2][4] = c1 * utmp[j][4] * tmp1 
        - c2 * ( utmp[j][5]
            + utmp[j][2]*utmp[j][2] * tmp2 );
      fjac[j][3][4] = - c2 * ( utmp[j][2]*utmp[j][3] )
        * tmp2;
      fjac[j][4][4] = c1 * utmp[j][2] * tmp1;

      njac[j][0][0] = 0.0;
      njac[j][1][0] = 0.0;
      njac[j][2][0] = 0.0;
      njac[j][3][0] = 0.0;
      njac[j][4][0] = 0.0;

      njac[j][0][1] = - c3c4 * tmp2 * utmp[j][1];
      njac[j][1][1] =   c3c4 * tmp1;
      njac[j][2][1] =   0.0;
      njac[j][3][1] =   0.0;
      njac[j][4][1] =   0.0;

      njac[j][0][2] = - con43 * c3c4 * tmp2 * utmp[j][2];
      njac[j][1][2] =   0.0;
      njac[j][2][2] =   con43 * c3c4 * tmp1;
      njac[j][3][2] =   0.0;
      njac[j][4][2] =   0.0;

      njac[j][0][3] = - c3c4 * tmp2 * utmp[j][3];
      njac[j][1][3] =   0.0;
      njac[j][2][3] =   0.0;
      njac[j][3][3] =   c3c4 * tmp1;
      njac[j][4][3] =   0.0;

      njac[j][0][4] = - (  c3c4
          - c1345 ) * tmp3 * (utmp[j][1]*utmp[j][1])
        - ( con43 * c3c4
            - c1345 ) * tmp3 * (utmp[j][2]*utmp[j][2])
        - ( c3c4 - c1345 ) * tmp3 * (utmp[j][3]*utmp[j][3])
        - c1345 * tmp2 * utmp[j][4];

      njac[j][1][4] = (  c3c4 - c1345 ) * tmp2 * utmp[j][1];
      njac[j][2][4] = ( con43 * c3c4
          - c1345 ) * tmp2 * utmp[j][2];
      njac[j][3][4] = ( c3c4 - c1345 ) * tmp2 * utmp[j][3];
      njac[j][4][4] = ( c1345 ) * tmp1;

    }

    //---------------------------------------------------------------------
    // now joacobians set, so form left hand side in y direction
    //---------------------------------------------------------------------
    for (j = start[c][1]+2; j <= jsize-end[c][1]+1; j++) {

      tmp1 = dt * ty1;
      tmp2 = dt * ty2;

      lhsa[j-1][0][0] = - tmp2 * fjac[j-1][0][0]
        - tmp1 * njac[j-1][0][0]
        - tmp1 * dy1; 
      lhsa[j-1][1][0] = - tmp2 * fjac[j-1][1][0]
        - tmp1 * njac[j-1][1][0];
      lhsa[j-1][2][0] = - tmp2 * fjac[j-1][2][0]
        - tmp1 * njac[j-1][2][0];
      lhsa[j-1][3][0] = - tmp2 * fjac[j-1][3][0]
        - tmp1 * njac[j-1][3][0];
      lhsa[j-1][4][0] = - tmp2 * fjac[j-1][4][0]
        - tmp1 * njac[j-1][4][0];

      lhsa[j-1][0][1] = - tmp2 * fjac[j-1][0][1]
        - tmp1 * njac[j-1][0][1];
      lhsa[j-1][1][1] = - tmp2 * fjac[j-1][1][1]
        - tmp1 * njac[j-1][1][1]
        - tmp1 * dy2;
      lhsa[j-1][2][1] = - tmp2 * fjac[j-1][2][1]
        - tmp1 * njac[j-1][2][1];
      lhsa[j-1][3][1] = - tmp2 * fjac[j-1][3][1]
        - tmp1 * njac[j-1][3][1];
      lhsa[j-1][4][1] = - tmp2 * fjac[j-1][4][1]
        - tmp1 * njac[j-1][4][1];

      lhsa[j-1][0][2] = - tmp2 * fjac[j-1][0][2]
        - tmp1 * njac[j-1][0][2];
      lhsa[j-1][1][2] = - tmp2 * fjac[j-1][1][2]
        - tmp1 * njac[j-1][1][2];
      lhsa[j-1][2][2] = - tmp2 * fjac[j-1][2][2]
        - tmp1 * njac[j-1][2][2]
        - tmp1 * dy3;
      lhsa[j-1][3][2] = - tmp2 * fjac[j-1][3][2]
        - tmp1 * njac[j-1][3][2];
      lhsa[j-1][4][2] = - tmp2 * fjac[j-1][4][2]
        - tmp1 * njac[j-1][4][2];

      lhsa[j-1][0][3] = - tmp2 * fjac[j-1][0][3]
        - tmp1 * njac[j-1][0][3];
      lhsa[j-1][1][3] = - tmp2 * fjac[j-1][1][3]
        - tmp1 * njac[j-1][1][3];
      lhsa[j-1][2][3] = - tmp2 * fjac[j-1][2][3]
        - tmp1 * njac[j-1][2][3];
      lhsa[j-1][3][3] = - tmp2 * fjac[j-1][3][3]
        - tmp1 * njac[j-1][3][3]
        - tmp1 * dy4;
      lhsa[j-1][4][3] = - tmp2 * fjac[j-1][4][3]
        - tmp1 * njac[j-1][4][3];

      lhsa[j-1][0][4] = - tmp2 * fjac[j-1][0][4]
        - tmp1 * njac[j-1][0][4];
      lhsa[j-1][1][4] = - tmp2 * fjac[j-1][1][4]
        - tmp1 * njac[j-1][1][4];
      lhsa[j-1][2][4] = - tmp2 * fjac[j-1][2][4]
        - tmp1 * njac[j-1][2][4];
      lhsa[j-1][3][4] = - tmp2 * fjac[j-1][3][4]
        - tmp1 * njac[j-1][3][4];
      lhsa[j-1][4][4] = - tmp2 * fjac[j-1][4][4]
        - tmp1 * njac[j-1][4][4]
        - tmp1 * dy5;

      lhsb[j-1][0][0] = 1.0
        + tmp1 * 2.0 * njac[j][0][0]
        + tmp1 * 2.0 * dy1;
      lhsb[j-1][1][0] = tmp1 * 2.0 * njac[j][1][0];
      lhsb[j-1][2][0] = tmp1 * 2.0 * njac[j][2][0];
      lhsb[j-1][3][0] = tmp1 * 2.0 * njac[j][3][0];
      lhsb[j-1][4][0] = tmp1 * 2.0 * njac[j][4][0];

      lhsb[j-1][0][1] = tmp1 * 2.0 * njac[j][0][1];
      lhsb[j-1][1][1] = 1.0
        + tmp1 * 2.0 * njac[j][1][1]
        + tmp1 * 2.0 * dy2;
      lhsb[j-1][2][1] = tmp1 * 2.0 * njac[j][2][1];
      lhsb[j-1][3][1] = tmp1 * 2.0 * njac[j][3][1];
      lhsb[j-1][4][1] = tmp1 * 2.0 * njac[j][4][1];

      lhsb[j-1][0][2] = tmp1 * 2.0 * njac[j][0][2];
      lhsb[j-1][1][2] = tmp1 * 2.0 * njac[j][1][2];
      lhsb[j-1][2][2] = 1.0
        + tmp1 * 2.0 * njac[j][2][2]
        + tmp1 * 2.0 * dy3;
      lhsb[j-1][3][2] = tmp1 * 2.0 * njac[j][3][2];
      lhsb[j-1][4][2] = tmp1 * 2.0 * njac[j][4][2];

      lhsb[j-1][0][3] = tmp1 * 2.0 * njac[j][0][3];
      lhsb[j-1][1][3] = tmp1 * 2.0 * njac[j][1][3];
      lhsb[j-1][2][3] = tmp1 * 2.0 * njac[j][2][3];
      lhsb[j-1][3][3] = 1.0
        + tmp1 * 2.0 * njac[j][3][3]
        + tmp1 * 2.0 * dy4;
      lhsb[j-1][4][3] = tmp1 * 2.0 * njac[j][4][3];

      lhsb[j-1][0][4] = tmp1 * 2.0 * njac[j][0][4];
      lhsb[j-1][1][4] = tmp1 * 2.0 * njac[j][1][4];
      lhsb[j-1][2][4] = tmp1 * 2.0 * njac[j][2][4];
      lhsb[j-1][3][4] = tmp1 * 2.0 * njac[j][3][4];
      lhsb[j-1][4][4] = 1.0
        + tmp1 * 2.0 * njac[j][4][4] 
        + tmp1 * 2.0 * dy5;

      lhsc[c][k-1][j-1][i-1][0][0] =  tmp2 * fjac[j+1][0][0]
        - tmp1 * njac[j+1][0][0]
        - tmp1 * dy1;
      lhsc[c][k-1][j-1][i-1][1][0] =  tmp2 * fjac[j+1][1][0]
        - tmp1 * njac[j+1][1][0];
      lhsc[c][k-1][j-1][i-1][2][0] =  tmp2 * fjac[j+1][2][0]
        - tmp1 * njac[j+1][2][0];
      lhsc[c][k-1][j-1][i-1][3][0] =  tmp2 * fjac[j+1][3][0]
        - tmp1 * njac[j+1][3][0];
      lhsc[c][k-1][j-1][i-1][4][0] =  tmp2 * fjac[j+1][4][0]
        - tmp1 * njac[j+1][4][0];

      lhsc[c][k-1][j-1][i-1][0][1] =  tmp2 * fjac[j+1][0][1]
        - tmp1 * njac[j+1][0][1];
      lhsc[c][k-1][j-1][i-1][1][1] =  tmp2 * fjac[j+1][1][1]
        - tmp1 * njac[j+1][1][1]
        - tmp1 * dy2;
      lhsc[c][k-1][j-1][i-1][2][1] =  tmp2 * fjac[j+1][2][1]
        - tmp1 * njac[j+1][2][1];
      lhsc[c][k-1][j-1][i-1][3][1] =  tmp2 * fjac[j+1][3][1]
        - tmp1 * njac[j+1][3][1];
      lhsc[c][k-1][j-1][i-1][4][1] =  tmp2 * fjac[j+1][4][1]
        - tmp1 * njac[j+1][4][1];

      lhsc[c][k-1][j-1][i-1][0][2] =  tmp2 * fjac[j+1][0][2]
        - tmp1 * njac[j+1][0][2];
      lhsc[c][k-1][j-1][i-1][1][2] =  tmp2 * fjac[j+1][1][2]
        - tmp1 * njac[j+1][1][2];
      lhsc[c][k-1][j-1][i-1][2][2] =  tmp2 * fjac[j+1][2][2]
        - tmp1 * njac[j+1][2][2]
        - tmp1 * dy3;
      lhsc[c][k-1][j-1][i-1][3][2] =  tmp2 * fjac[j+1][3][2]
        - tmp1 * njac[j+1][3][2];
      lhsc[c][k-1][j-1][i-1][4][2] =  tmp2 * fjac[j+1][4][2]
        - tmp1 * njac[j+1][4][2];

      lhsc[c][k-1][j-1][i-1][0][3] =  tmp2 * fjac[j+1][0][3]
        - tmp1 * njac[j+1][0][3];
      lhsc[c][k-1][j-1][i-1][1][3] =  tmp2 * fjac[j+1][1][3]
        - tmp1 * njac[j+1][1][3];
      lhsc[c][k-1][j-1][i-1][2][3] =  tmp2 * fjac[j+1][2][3]
        - tmp1 * njac[j+1][2][3];
      lhsc[c][k-1][j-1][i-1][3][3] =  tmp2 * fjac[j+1][3][3]
        - tmp1 * njac[j+1][3][3]
        - tmp1 * dy4;
      lhsc[c][k-1][j-1][i-1][4][3] =  tmp2 * fjac[j+1][4][3]
        - tmp1 * njac[j+1][4][3];

      lhsc[c][k-1][j-1][i-1][0][4] =  tmp2 * fjac[j+1][0][4]
        - tmp1 * njac[j+1][0][4];
      lhsc[c][k-1][j-1][i-1][1][4] =  tmp2 * fjac[j+1][1][4]
        - tmp1 * njac[j+1][1][4];
      lhsc[c][k-1][j-1][i-1][2][4] =  tmp2 * fjac[j+1][2][4]
        - tmp1 * njac[j+1][2][4];
      lhsc[c][k-1][j-1][i-1][3][4] =  tmp2 * fjac[j+1][3][4]
        - tmp1 * njac[j+1][3][4];
      lhsc[c][k-1][j-1][i-1][4][4] =  tmp2 * fjac[j+1][4][4]
        - tmp1 * njac[j+1][4][4]
        - tmp1 * dy5;

    }


    //---------------------------------------------------------------------
    // outer most do loops - sweeping in i direction
    //---------------------------------------------------------------------
    if (first == 1) { 

      //-------------------------------------------------------------------
      // multiply c(i,jstart,k) by b_inverse and copy back to c
      // multiply rhs(jstart) by b_inverse(jstart) and copy to rhs
      //-------------------------------------------------------------------
      binvcrhs( lhsb[jstart], lhsc[c][k-1][jstart][i-1],
                 rhs[c][k-1][jstart][i-1] );

    }

    //---------------------------------------------------------------------
    // begin inner most do loop
    // do all the elements of the cell unless last 
    //---------------------------------------------------------------------
    for (j = jstart+first; j <= jsize-last; j++) {

      //-------------------------------------------------------------------
      // subtract A*lhs_vector(j-1) from lhs_vector(j)
      // 
      // rhs(j) = rhs(j) - A*rhs(j-1)
      //-------------------------------------------------------------------
      matvec_sub(lhsa[j], rhs[c][k-1][j-1][i-1], rhs[c][k-1][j][i-1]);

      //-------------------------------------------------------------------
      // B(j) = B(j) - C(j-1)*A(j)
      //-------------------------------------------------------------------
      matmul_sub(lhsa[j], lhsc[c][k-1][j-1][i-1], lhsb[j]);

      //-------------------------------------------------------------------
      // multiply c(i,j,k) by b_inverse and copy back to c
      // multiply rhs(i,1,k) by b_inverse(i,1,k) and copy to rhs
      //-------------------------------------------------------------------
      binvcrhs( lhsb[j], lhsc[c][k-1][j][i-1], rhs[c][k-1][j][i-1] );

    }

    //---------------------------------------------------------------------
    // Now finish up special cases for last cell
    //---------------------------------------------------------------------
    if (last == 1) {

      //-------------------------------------------------------------------
      // rhs(jsize) = rhs(jsize) - A*rhs(jsize-1)
      //-------------------------------------------------------------------
      matvec_sub(lhsa[jsize], rhs[c][k-1][jsize-1][i-1],
                 rhs[c][k-1][jsize][i-1]);

      //-------------------------------------------------------------------
      // B(jsize) = B(jsize) - C(jsize-1)*A(jsize)
      // matmul_sub(aa,i,jsize,k,c,
      // $              cc,i,jsize-1,k,c,bb,i,jsize,k,c)
      //-------------------------------------------------------------------
      matmul_sub(lhsa[jsize], lhsc[c][k-1][jsize-1][i-1], lhsb[jsize]);

      //-------------------------------------------------------------------
      // multiply rhs(jsize) by b_inverse(jsize) and copy to rhs
      //-------------------------------------------------------------------
      binvrhs( lhsb[jsize], rhs[c][k-1][jsize][i-1] );

    }
  }
#endif
}


//---------------------------------------------------------------------
// unpack C'(-1) and rhs'(-1) for
// all i and j
//---------------------------------------------------------------------
__kernel void z_unpack_solve_info(__global double *g_lhsc,
                                  __global double *g_rhs,
                                  __global double *out_buffer,
                                  int c)
{
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];

  int i, j, m, n, ptr, kstart;
  kstart = 0;

  j = get_global_id(1) + 1;
  i = get_global_id(0) + 1;
  if (j >= JMAX+1 || i >= IMAX+1) return;

  lhsc = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  ptr = (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE) * (IMAX*(j-1) + (i-1));

  for (n = 0; n < BLOCK_SIZE; n++) {
    for (m = 0; m < BLOCK_SIZE; m++) {
      lhsc[c][kstart][j][i][n][m] = out_buffer[ptr+m];
    }
    ptr = ptr+BLOCK_SIZE;
  }
  for (n = 0; n < BLOCK_SIZE; n++) {
    rhs[c][kstart][j][i][n] = out_buffer[ptr+n];
  }
}


__kernel void z_send_solve_info(__global double *g_lhsc,
                                __global double *g_rhs,
                                __global double *in_buffer,
                                __global int *g_cell_size,
                                int c)
{
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global int (*cell_size)[3];

  int i, j, m, n, ksize, ptr;

  j = get_global_id(1) + 1;
  i = get_global_id(0) + 1;
  if (j >= JMAX+1 || i >= IMAX+1) return;

  lhsc = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  cell_size = (__global int (*)[3])g_cell_size;

  ksize = cell_size[c][2];
  ptr = (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE) * (IMAX*(j-1) + (i-1));

  for (n = 0; n < BLOCK_SIZE; n++) {
    for (m = 0; m < BLOCK_SIZE; m++) {
      in_buffer[ptr+m] = lhsc[c][ksize][j][i][n][m];
    }
    ptr = ptr+BLOCK_SIZE;
  }
  for (n = 0; n < BLOCK_SIZE; n++) {
    in_buffer[ptr+n] = rhs[c][ksize][j][i][n];
  }
}


__kernel void z_send_backsub_info(__global double *g_rhs,
                                  __global double *in_buffer,
                                  int c)
{
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];

  int i, j, n, ptr, kstart;

  j = get_global_id(1) + 1;
  i = get_global_id(0) + 1;
  if (j >= JMAX+1 || i >= IMAX+1) return;

  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;

  kstart = 1;
  ptr = BLOCK_SIZE * (IMAX*(j-1) + (i-1));

  for (n = 0; n < BLOCK_SIZE; n++) {
    in_buffer[ptr+n] = rhs[c][kstart][j][i][n];
  }
}


//---------------------------------------------------------------------
// unpack U(jsize) for all i and k
//---------------------------------------------------------------------
__kernel void z_unpack_backsub_info(__global double *g_backsub_info,
                                    __global double *out_buffer,
                                    int c)
{
  __global double (*backsub_info)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5];
  
  int i, j, n, ptr;

  j = get_global_id(1);
  i = get_global_id(0);
  if (j >= JMAX || i >= IMAX) return;

  backsub_info = (__global double (*)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5])g_backsub_info;

  ptr = BLOCK_SIZE * (IMAX*j + i);
  for (n = 0; n < BLOCK_SIZE; n++) {
    backsub_info[c][j][i][n] = out_buffer[ptr+n];
  }
}


__kernel void z_backsubstitute(__global double *g_rhs,
                               __global double *g_lhsc,
                               __global double *g_backsub_info,
                               __global int *g_cell_size,
                               __global int *g_start,
                               __global int *g_end,
                               int last,
                               int c)
{
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*backsub_info)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5];
  __global int (*cell_size)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  int i, k;
  int m, n, j, jsize, isize, ksize, kstart;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

  j = get_global_id(1) + start[c][1]+1;
  jsize = cell_size[c][1]-end[c][1];
  if (j > jsize) return;

  i = get_global_id(0) + start[c][0]+1;
  isize = cell_size[c][0]-end[c][0];
  if (i > isize) return;

  rhs  = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  lhsc = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;
  backsub_info = (__global double (*)[MAX_CELL_DIM+1][MAX_CELL_DIM+1][5])g_backsub_info;

  kstart = 1;
  ksize = cell_size[c][2];

  if (last == 0) {
    //-------------------------------------------------------------------
    // U(jsize) uses info from previous cell if not last cell
    //-------------------------------------------------------------------
    for (m = 0; m < BLOCK_SIZE; m++) {
      for (n = 0; n < BLOCK_SIZE; n++) {
        rhs[c][ksize][j][i][m] = rhs[c][ksize][j][i][m] 
          - lhsc[c][ksize][j][i][n][m]*
          backsub_info[c][j-1][i-1][n];
      }
    }
  }

  for (k = ksize-1; k >= kstart; k--) {
    for (m = 0; m < BLOCK_SIZE; m++) {
      for (n = 0; n < BLOCK_SIZE; n++) {
        rhs[c][k][j][i][m] = rhs[c][k][j][i][m] 
          - lhsc[c][k][j][i][n][m]*rhs[c][k+1][j][i][n];
      }
    }
  }
}


__kernel void z_solve_cell(__global double *g_qs,
                           __global double *g_u,
                           __global double *g_rhs,
                           __global double *g_lhsc,
                           __global double *g_lhsa,
                           __global double *g_lhsb,
                           __global double *g_fjac,
                           __global double *g_njac,
                           __global double *g_utmp,
                           __global int *g_cell_size,
                           __global int *g_start,
                           __global int *g_end,
                           int first,
                           int last,
                           int c)
{
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2];
  __global double (*u)[KMAX+4][JMAX+4][IMAX+4][5];
  __global double (*rhs)[KMAX+1][JMAX+1][IMAX+1][5];
  __global double (*lhsc)[KMAX+1][JMAX+1][IMAX+1][5][5];
  __global double (*lhsa)[5][5];
  __global double (*lhsb)[5][5];
  __global double (*fjac)[5][5];
  __global double (*njac)[5][5];
  __global double (*utmp)[6];
  __global int (*cell_size)[3];
  __global int (*start)[3];
  __global int (*end)[3];

  int i, j, k, isize, ksize, jsize, kstart;
  double tmp1, tmp2, tmp3;

  cell_size = (__global int (*)[3])g_cell_size;
  start = (__global int (*)[3])g_start;
  end   = (__global int (*)[3])g_end;

#if Z_SOLVE_DIM == 2
  j = get_global_id(1) + start[c][1]+2;
  jsize = cell_size[c][1]-end[c][1]+1;
  if (j > jsize) return;

  i = get_global_id(0) + start[c][0]+2;
  isize = cell_size[c][0]-end[c][0]+1;
  if (i > isize) return;

  qs     = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  u      = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  rhs    = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  lhsc   = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;

  int d0_size = isize - start[c][0] - 1;
  int my_id = get_global_id(1)*d0_size + get_global_id(0);
  int my_offset1 = my_id * (MAX_CELL_DIM+2)*5*5;
  int my_offset2 = my_id * (MAX_CELL_DIM+4)*5*5;
  int my_offset3 = my_id * (KMAX+4)*6;
  lhsa = (__global double (*)[5][5])&g_lhsa[my_offset1];
  lhsb = (__global double (*)[5][5])&g_lhsb[my_offset1];
  fjac = (__global double (*)[5][5])&g_fjac[my_offset2];
  njac = (__global double (*)[5][5])&g_njac[my_offset2];
  utmp = (__global double (*)[6])&g_utmp[my_offset3];

  kstart = 1;
  ksize = cell_size[c][2];

  lhsabinit(lhsa, lhsb, ksize);

  //---------------------------------------------------------------------
  // This function computes the left hand side for the three z-factors   
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // Compute the indices for storing the block-diagonal matrix;
  // determine c (labeled f) and s jacobians for cell c
  //---------------------------------------------------------------------
  for (k = start[c][2]+1; k <= cell_size[c][2]-end[c][2]+2; k++) {
    utmp[k][0] = 1.0 / u[c][k][j][i][0];
    utmp[k][1] = u[c][k][j][i][1];
    utmp[k][2] = u[c][k][j][i][2];
    utmp[k][3] = u[c][k][j][i][3];
    utmp[k][4] = u[c][k][j][i][4];
    utmp[k][5] = qs[c][k-1][j-1][i-1];
  }

  for (k = start[c][2]+1; k <= cell_size[c][2]-end[c][2]+2; k++) {

    tmp1 = utmp[k][0];
    tmp2 = tmp1 * tmp1;
    tmp3 = tmp1 * tmp2;

    fjac[k][0][0] = 0.0;
    fjac[k][1][0] = 0.0;
    fjac[k][2][0] = 0.0;
    fjac[k][3][0] = 1.0;
    fjac[k][4][0] = 0.0;

    fjac[k][0][1] = - ( utmp[k][1]*utmp[k][3] ) 
      * tmp2;
    fjac[k][1][1] = utmp[k][3] * tmp1;
    fjac[k][2][1] = 0.0;
    fjac[k][3][1] = utmp[k][1] * tmp1;
    fjac[k][4][1] = 0.0;

    fjac[k][0][2] = - ( utmp[k][2]*utmp[k][3] )
      * tmp2;
    fjac[k][1][2] = 0.0;
    fjac[k][2][2] = utmp[k][3] * tmp1;
    fjac[k][3][2] = utmp[k][2] * tmp1;
    fjac[k][4][2] = 0.0;

    fjac[k][0][3] = - (utmp[k][3]*utmp[k][3] * tmp2 ) 
      + c2 * utmp[k][5];
    fjac[k][1][3] = - c2 *  utmp[k][1] * tmp1;
    fjac[k][2][3] = - c2 *  utmp[k][2] * tmp1;
    fjac[k][3][3] = ( 2.0 - c2 )
      *  utmp[k][3] * tmp1;
    fjac[k][4][3] = c2;

    fjac[k][0][4] = ( c2 * 2.0 * utmp[k][5]
        - c1 * ( utmp[k][4] * tmp1 ) )
      * ( utmp[k][3] * tmp1 );
    fjac[k][1][4] = - c2 * ( utmp[k][1]*utmp[k][3] )
      * tmp2;
    fjac[k][2][4] = - c2 * ( utmp[k][2]*utmp[k][3] )
      * tmp2;
    fjac[k][3][4] = c1 * ( utmp[k][4] * tmp1 )
      - c2 * ( utmp[k][5]
          + utmp[k][3]*utmp[k][3] * tmp2 );
    fjac[k][4][4] = c1 * utmp[k][3] * tmp1;

    njac[k][0][0] = 0.0;
    njac[k][1][0] = 0.0;
    njac[k][2][0] = 0.0;
    njac[k][3][0] = 0.0;
    njac[k][4][0] = 0.0;

    njac[k][0][1] = - c3c4 * tmp2 * utmp[k][1];
    njac[k][1][1] =   c3c4 * tmp1;
    njac[k][2][1] =   0.0;
    njac[k][3][1] =   0.0;
    njac[k][4][1] =   0.0;

    njac[k][0][2] = - c3c4 * tmp2 * utmp[k][2];
    njac[k][1][2] =   0.0;
    njac[k][2][2] =   c3c4 * tmp1;
    njac[k][3][2] =   0.0;
    njac[k][4][2] =   0.0;

    njac[k][0][3] = - con43 * c3c4 * tmp2 * utmp[k][3];
    njac[k][1][3] =   0.0;
    njac[k][2][3] =   0.0;
    njac[k][3][3] =   con43 * c3 * c4 * tmp1;
    njac[k][4][3] =   0.0;

    njac[k][0][4] = - (  c3c4
        - c1345 ) * tmp3 * (utmp[k][1]*utmp[k][1])
      - ( c3c4 - c1345 ) * tmp3 * (utmp[k][2]*utmp[k][2])
      - ( con43 * c3c4
          - c1345 ) * tmp3 * (utmp[k][3]*utmp[k][3])
      - c1345 * tmp2 * utmp[k][4];

    njac[k][1][4] = (  c3c4 - c1345 ) * tmp2 * utmp[k][1];
    njac[k][2][4] = (  c3c4 - c1345 ) * tmp2 * utmp[k][2];
    njac[k][3][4] = ( con43 * c3c4
        - c1345 ) * tmp2 * utmp[k][3];
    njac[k][4][4] = ( c1345 )* tmp1;

  }

  //---------------------------------------------------------------------
  // now joacobians set, so form left hand side in z direction
  //---------------------------------------------------------------------
  for (k = start[c][2]+2; k <= ksize-end[c][2]+1; k++) {

    tmp1 = dt * tz1;
    tmp2 = dt * tz2;

    lhsa[k-1][0][0] = - tmp2 * fjac[k-1][0][0]
      - tmp1 * njac[k-1][0][0]
      - tmp1 * dz1; 
    lhsa[k-1][1][0] = - tmp2 * fjac[k-1][1][0]
      - tmp1 * njac[k-1][1][0];
    lhsa[k-1][2][0] = - tmp2 * fjac[k-1][2][0]
      - tmp1 * njac[k-1][2][0];
    lhsa[k-1][3][0] = - tmp2 * fjac[k-1][3][0]
      - tmp1 * njac[k-1][3][0];
    lhsa[k-1][4][0] = - tmp2 * fjac[k-1][4][0]
      - tmp1 * njac[k-1][4][0];

    lhsa[k-1][0][1] = - tmp2 * fjac[k-1][0][1]
      - tmp1 * njac[k-1][0][1];
    lhsa[k-1][1][1] = - tmp2 * fjac[k-1][1][1]
      - tmp1 * njac[k-1][1][1]
      - tmp1 * dz2;
    lhsa[k-1][2][1] = - tmp2 * fjac[k-1][2][1]
      - tmp1 * njac[k-1][2][1];
    lhsa[k-1][3][1] = - tmp2 * fjac[k-1][3][1]
      - tmp1 * njac[k-1][3][1];
    lhsa[k-1][4][1] = - tmp2 * fjac[k-1][4][1]
      - tmp1 * njac[k-1][4][1];

    lhsa[k-1][0][2] = - tmp2 * fjac[k-1][0][2]
      - tmp1 * njac[k-1][0][2];
    lhsa[k-1][1][2] = - tmp2 * fjac[k-1][1][2]
      - tmp1 * njac[k-1][1][2];
    lhsa[k-1][2][2] = - tmp2 * fjac[k-1][2][2]
      - tmp1 * njac[k-1][2][2]
      - tmp1 * dz3;
    lhsa[k-1][3][2] = - tmp2 * fjac[k-1][3][2]
      - tmp1 * njac[k-1][3][2];
    lhsa[k-1][4][2] = - tmp2 * fjac[k-1][4][2]
      - tmp1 * njac[k-1][4][2];

    lhsa[k-1][0][3] = - tmp2 * fjac[k-1][0][3]
      - tmp1 * njac[k-1][0][3];
    lhsa[k-1][1][3] = - tmp2 * fjac[k-1][1][3]
      - tmp1 * njac[k-1][1][3];
    lhsa[k-1][2][3] = - tmp2 * fjac[k-1][2][3]
      - tmp1 * njac[k-1][2][3];
    lhsa[k-1][3][3] = - tmp2 * fjac[k-1][3][3]
      - tmp1 * njac[k-1][3][3]
      - tmp1 * dz4;
    lhsa[k-1][4][3] = - tmp2 * fjac[k-1][4][3]
      - tmp1 * njac[k-1][4][3];

    lhsa[k-1][0][4] = - tmp2 * fjac[k-1][0][4]
      - tmp1 * njac[k-1][0][4];
    lhsa[k-1][1][4] = - tmp2 * fjac[k-1][1][4]
      - tmp1 * njac[k-1][1][4];
    lhsa[k-1][2][4] = - tmp2 * fjac[k-1][2][4]
      - tmp1 * njac[k-1][2][4];
    lhsa[k-1][3][4] = - tmp2 * fjac[k-1][3][4]
      - tmp1 * njac[k-1][3][4];
    lhsa[k-1][4][4] = - tmp2 * fjac[k-1][4][4]
      - tmp1 * njac[k-1][4][4]
      - tmp1 * dz5;

    lhsb[k-1][0][0] = 1.0
      + tmp1 * 2.0 * njac[k][0][0]
      + tmp1 * 2.0 * dz1;
    lhsb[k-1][1][0] = tmp1 * 2.0 * njac[k][1][0];
    lhsb[k-1][2][0] = tmp1 * 2.0 * njac[k][2][0];
    lhsb[k-1][3][0] = tmp1 * 2.0 * njac[k][3][0];
    lhsb[k-1][4][0] = tmp1 * 2.0 * njac[k][4][0];

    lhsb[k-1][0][1] = tmp1 * 2.0 * njac[k][0][1];
    lhsb[k-1][1][1] = 1.0
      + tmp1 * 2.0 * njac[k][1][1]
      + tmp1 * 2.0 * dz2;
    lhsb[k-1][2][1] = tmp1 * 2.0 * njac[k][2][1];
    lhsb[k-1][3][1] = tmp1 * 2.0 * njac[k][3][1];
    lhsb[k-1][4][1] = tmp1 * 2.0 * njac[k][4][1];

    lhsb[k-1][0][2] = tmp1 * 2.0 * njac[k][0][2];
    lhsb[k-1][1][2] = tmp1 * 2.0 * njac[k][1][2];
    lhsb[k-1][2][2] = 1.0
      + tmp1 * 2.0 * njac[k][2][2]
      + tmp1 * 2.0 * dz3;
    lhsb[k-1][3][2] = tmp1 * 2.0 * njac[k][3][2];
    lhsb[k-1][4][2] = tmp1 * 2.0 * njac[k][4][2];

    lhsb[k-1][0][3] = tmp1 * 2.0 * njac[k][0][3];
    lhsb[k-1][1][3] = tmp1 * 2.0 * njac[k][1][3];
    lhsb[k-1][2][3] = tmp1 * 2.0 * njac[k][2][3];
    lhsb[k-1][3][3] = 1.0
      + tmp1 * 2.0 * njac[k][3][3]
      + tmp1 * 2.0 * dz4;
    lhsb[k-1][4][3] = tmp1 * 2.0 * njac[k][4][3];

    lhsb[k-1][0][4] = tmp1 * 2.0 * njac[k][0][4];
    lhsb[k-1][1][4] = tmp1 * 2.0 * njac[k][1][4];
    lhsb[k-1][2][4] = tmp1 * 2.0 * njac[k][2][4];
    lhsb[k-1][3][4] = tmp1 * 2.0 * njac[k][3][4];
    lhsb[k-1][4][4] = 1.0
      + tmp1 * 2.0 * njac[k][4][4] 
      + tmp1 * 2.0 * dz5;

    lhsc[c][k-1][j-1][i-1][0][0] =  tmp2 * fjac[k+1][0][0]
      - tmp1 * njac[k+1][0][0]
      - tmp1 * dz1;
    lhsc[c][k-1][j-1][i-1][1][0] =  tmp2 * fjac[k+1][1][0]
      - tmp1 * njac[k+1][1][0];
    lhsc[c][k-1][j-1][i-1][2][0] =  tmp2 * fjac[k+1][2][0]
      - tmp1 * njac[k+1][2][0];
    lhsc[c][k-1][j-1][i-1][3][0] =  tmp2 * fjac[k+1][3][0]
      - tmp1 * njac[k+1][3][0];
    lhsc[c][k-1][j-1][i-1][4][0] =  tmp2 * fjac[k+1][4][0]
      - tmp1 * njac[k+1][4][0];

    lhsc[c][k-1][j-1][i-1][0][1] =  tmp2 * fjac[k+1][0][1]
      - tmp1 * njac[k+1][0][1];
    lhsc[c][k-1][j-1][i-1][1][1] =  tmp2 * fjac[k+1][1][1]
      - tmp1 * njac[k+1][1][1]
      - tmp1 * dz2;
    lhsc[c][k-1][j-1][i-1][2][1] =  tmp2 * fjac[k+1][2][1]
      - tmp1 * njac[k+1][2][1];
    lhsc[c][k-1][j-1][i-1][3][1] =  tmp2 * fjac[k+1][3][1]
      - tmp1 * njac[k+1][3][1];
    lhsc[c][k-1][j-1][i-1][4][1] =  tmp2 * fjac[k+1][4][1]
      - tmp1 * njac[k+1][4][1];

    lhsc[c][k-1][j-1][i-1][0][2] =  tmp2 * fjac[k+1][0][2]
      - tmp1 * njac[k+1][0][2];
    lhsc[c][k-1][j-1][i-1][1][2] =  tmp2 * fjac[k+1][1][2]
      - tmp1 * njac[k+1][1][2];
    lhsc[c][k-1][j-1][i-1][2][2] =  tmp2 * fjac[k+1][2][2]
      - tmp1 * njac[k+1][2][2]
      - tmp1 * dz3;
    lhsc[c][k-1][j-1][i-1][3][2] =  tmp2 * fjac[k+1][3][2]
      - tmp1 * njac[k+1][3][2];
    lhsc[c][k-1][j-1][i-1][4][2] =  tmp2 * fjac[k+1][4][2]
      - tmp1 * njac[k+1][4][2];

    lhsc[c][k-1][j-1][i-1][0][3] =  tmp2 * fjac[k+1][0][3]
      - tmp1 * njac[k+1][0][3];
    lhsc[c][k-1][j-1][i-1][1][3] =  tmp2 * fjac[k+1][1][3]
      - tmp1 * njac[k+1][1][3];
    lhsc[c][k-1][j-1][i-1][2][3] =  tmp2 * fjac[k+1][2][3]
      - tmp1 * njac[k+1][2][3];
    lhsc[c][k-1][j-1][i-1][3][3] =  tmp2 * fjac[k+1][3][3]
      - tmp1 * njac[k+1][3][3]
      - tmp1 * dz4;
    lhsc[c][k-1][j-1][i-1][4][3] =  tmp2 * fjac[k+1][4][3]
      - tmp1 * njac[k+1][4][3];

    lhsc[c][k-1][j-1][i-1][0][4] =  tmp2 * fjac[k+1][0][4]
      - tmp1 * njac[k+1][0][4];
    lhsc[c][k-1][j-1][i-1][1][4] =  tmp2 * fjac[k+1][1][4]
      - tmp1 * njac[k+1][1][4];
    lhsc[c][k-1][j-1][i-1][2][4] =  tmp2 * fjac[k+1][2][4]
      - tmp1 * njac[k+1][2][4];
    lhsc[c][k-1][j-1][i-1][3][4] =  tmp2 * fjac[k+1][3][4]
      - tmp1 * njac[k+1][3][4];
    lhsc[c][k-1][j-1][i-1][4][4] =  tmp2 * fjac[k+1][4][4]
      - tmp1 * njac[k+1][4][4]
      - tmp1 * dz5;

  }


  //---------------------------------------------------------------------
  // outer most do loops - sweeping in i direction
  //---------------------------------------------------------------------
  if (first == 1) { 

    //-------------------------------------------------------------------
    // multiply c(i,j,kstart) by b_inverse and copy back to c
    // multiply rhs(kstart) by b_inverse(kstart) and copy to rhs
    //-------------------------------------------------------------------
    binvcrhs( lhsb[kstart], lhsc[c][kstart][j-1][i-1],
              rhs[c][kstart][j-1][i-1] );

  }

  //---------------------------------------------------------------------
  // begin inner most do loop
  // do all the elements of the cell unless last 
  //---------------------------------------------------------------------
  for (k = kstart+first; k <= ksize-last; k++) {

    //-------------------------------------------------------------------
    // subtract A*lhs_vector(k-1) from lhs_vector(k)
    // 
    // rhs(k) = rhs(k) - A*rhs(k-1)
    //-------------------------------------------------------------------
    matvec_sub(lhsa[k], rhs[c][k-1][j-1][i-1], rhs[c][k][j-1][i-1]);

    //-------------------------------------------------------------------
    // B(k) = B(k) - C(k-1)*A(k)
    // matmul_sub(aa,i,j,k,c,cc,i,j,k-1,c,bb,i,j,k,c)
    //-------------------------------------------------------------------
    matmul_sub(lhsa[k], lhsc[c][k-1][j-1][i-1], lhsb[k]);

    //-------------------------------------------------------------------
    // multiply c(i,j,k) by b_inverse and copy back to c
    // multiply rhs(i,j,1) by b_inverse(i,j,1) and copy to rhs
    //-------------------------------------------------------------------
    binvcrhs( lhsb[k], lhsc[c][k][j-1][i-1], rhs[c][k][j-1][i-1] );

  }

  //---------------------------------------------------------------------
  // Now finish up special cases for last cell
  //---------------------------------------------------------------------
  if (last == 1) {

    //-------------------------------------------------------------------
    // rhs(ksize) = rhs(ksize) - A*rhs(ksize-1)
    //-------------------------------------------------------------------
    matvec_sub(lhsa[ksize], rhs[c][ksize-1][j-1][i-1],
               rhs[c][ksize][j-1][i-1]);

    //-------------------------------------------------------------------
    // B(ksize) = B(ksize) - C(ksize-1)*A(ksize)
    // matmul_sub(aa,i,j,ksize,c,
    // $              cc,i,j,ksize-1,c,bb,i,j,ksize,c)
    //-------------------------------------------------------------------
    matmul_sub(lhsa[ksize], lhsc[c][ksize-1][j-1][i-1], lhsb[ksize]);

    //-------------------------------------------------------------------
    // multiply rhs(ksize) by b_inverse(ksize) and copy to rhs
    //-------------------------------------------------------------------
    binvrhs( lhsb[ksize], rhs[c][ksize][j-1][i-1] );

  }

#else //Z_SOLVE_DIM == 1

  j = get_global_id(0) + start[c][1]+2;
  jsize = cell_size[c][1]-end[c][1]+1;
  if (j > jsize) return;

  qs     = (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  u      = (__global double (*)[KMAX+4][JMAX+4][IMAX+4][5])g_u;
  rhs    = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5])g_rhs;
  lhsc   = (__global double (*)[KMAX+1][JMAX+1][IMAX+1][5][5])g_lhsc;

  int my_id = get_global_id(0);
  int my_offset1 = my_id * (MAX_CELL_DIM+2)*5*5;
  int my_offset2 = my_id * (MAX_CELL_DIM+4)*5*5;
  int my_offset3 = my_id * (KMAX+4)*6;
  lhsa = (__global double (*)[5][5])&g_lhsa[my_offset1];
  lhsb = (__global double (*)[5][5])&g_lhsb[my_offset1];
  fjac = (__global double (*)[5][5])&g_fjac[my_offset2];
  njac = (__global double (*)[5][5])&g_njac[my_offset2];
  utmp = (__global double (*)[6])&g_utmp[my_offset3];

  kstart = 1;
  isize = cell_size[c][0]-end[c][0]+1;
  ksize = cell_size[c][2];

  lhsabinit(lhsa, lhsb, ksize);

  for (i = start[c][0]+2; i <= isize; i++) {

    //---------------------------------------------------------------------
    // This function computes the left hand side for the three z-factors   
    //---------------------------------------------------------------------

    //---------------------------------------------------------------------
    // Compute the indices for storing the block-diagonal matrix;
    // determine c (labeled f) and s jacobians for cell c
    //---------------------------------------------------------------------
    for (k = start[c][2]+1; k <= cell_size[c][2]-end[c][2]+2; k++) {
      utmp[k][0] = 1.0 / u[c][k][j][i][0];
      utmp[k][1] = u[c][k][j][i][1];
      utmp[k][2] = u[c][k][j][i][2];
      utmp[k][3] = u[c][k][j][i][3];
      utmp[k][4] = u[c][k][j][i][4];
      utmp[k][5] = qs[c][k-1][j-1][i-1];
    }

    for (k = start[c][2]+1; k <= cell_size[c][2]-end[c][2]+2; k++) {

      tmp1 = utmp[k][0];
      tmp2 = tmp1 * tmp1;
      tmp3 = tmp1 * tmp2;

      fjac[k][0][0] = 0.0;
      fjac[k][1][0] = 0.0;
      fjac[k][2][0] = 0.0;
      fjac[k][3][0] = 1.0;
      fjac[k][4][0] = 0.0;

      fjac[k][0][1] = - ( utmp[k][1]*utmp[k][3] ) 
        * tmp2;
      fjac[k][1][1] = utmp[k][3] * tmp1;
      fjac[k][2][1] = 0.0;
      fjac[k][3][1] = utmp[k][1] * tmp1;
      fjac[k][4][1] = 0.0;

      fjac[k][0][2] = - ( utmp[k][2]*utmp[k][3] )
        * tmp2;
      fjac[k][1][2] = 0.0;
      fjac[k][2][2] = utmp[k][3] * tmp1;
      fjac[k][3][2] = utmp[k][2] * tmp1;
      fjac[k][4][2] = 0.0;

      fjac[k][0][3] = - (utmp[k][3]*utmp[k][3] * tmp2 ) 
        + c2 * utmp[k][5];
      fjac[k][1][3] = - c2 *  utmp[k][1] * tmp1;
      fjac[k][2][3] = - c2 *  utmp[k][2] * tmp1;
      fjac[k][3][3] = ( 2.0 - c2 )
        *  utmp[k][3] * tmp1;
      fjac[k][4][3] = c2;

      fjac[k][0][4] = ( c2 * 2.0 * utmp[k][5]
          - c1 * ( utmp[k][4] * tmp1 ) )
        * ( utmp[k][3] * tmp1 );
      fjac[k][1][4] = - c2 * ( utmp[k][1]*utmp[k][3] )
        * tmp2;
      fjac[k][2][4] = - c2 * ( utmp[k][2]*utmp[k][3] )
        * tmp2;
      fjac[k][3][4] = c1 * ( utmp[k][4] * tmp1 )
        - c2 * ( utmp[k][5]
            + utmp[k][3]*utmp[k][3] * tmp2 );
      fjac[k][4][4] = c1 * utmp[k][3] * tmp1;

      njac[k][0][0] = 0.0;
      njac[k][1][0] = 0.0;
      njac[k][2][0] = 0.0;
      njac[k][3][0] = 0.0;
      njac[k][4][0] = 0.0;

      njac[k][0][1] = - c3c4 * tmp2 * utmp[k][1];
      njac[k][1][1] =   c3c4 * tmp1;
      njac[k][2][1] =   0.0;
      njac[k][3][1] =   0.0;
      njac[k][4][1] =   0.0;

      njac[k][0][2] = - c3c4 * tmp2 * utmp[k][2];
      njac[k][1][2] =   0.0;
      njac[k][2][2] =   c3c4 * tmp1;
      njac[k][3][2] =   0.0;
      njac[k][4][2] =   0.0;

      njac[k][0][3] = - con43 * c3c4 * tmp2 * utmp[k][3];
      njac[k][1][3] =   0.0;
      njac[k][2][3] =   0.0;
      njac[k][3][3] =   con43 * c3 * c4 * tmp1;
      njac[k][4][3] =   0.0;

      njac[k][0][4] = - (  c3c4
          - c1345 ) * tmp3 * (utmp[k][1]*utmp[k][1])
        - ( c3c4 - c1345 ) * tmp3 * (utmp[k][2]*utmp[k][2])
        - ( con43 * c3c4
            - c1345 ) * tmp3 * (utmp[k][3]*utmp[k][3])
        - c1345 * tmp2 * utmp[k][4];

      njac[k][1][4] = (  c3c4 - c1345 ) * tmp2 * utmp[k][1];
      njac[k][2][4] = (  c3c4 - c1345 ) * tmp2 * utmp[k][2];
      njac[k][3][4] = ( con43 * c3c4
          - c1345 ) * tmp2 * utmp[k][3];
      njac[k][4][4] = ( c1345 )* tmp1;

    }

    //---------------------------------------------------------------------
    // now joacobians set, so form left hand side in z direction
    //---------------------------------------------------------------------
    for (k = start[c][2]+2; k <= ksize-end[c][2]+1; k++) {

      tmp1 = dt * tz1;
      tmp2 = dt * tz2;

      lhsa[k-1][0][0] = - tmp2 * fjac[k-1][0][0]
        - tmp1 * njac[k-1][0][0]
        - tmp1 * dz1; 
      lhsa[k-1][1][0] = - tmp2 * fjac[k-1][1][0]
        - tmp1 * njac[k-1][1][0];
      lhsa[k-1][2][0] = - tmp2 * fjac[k-1][2][0]
        - tmp1 * njac[k-1][2][0];
      lhsa[k-1][3][0] = - tmp2 * fjac[k-1][3][0]
        - tmp1 * njac[k-1][3][0];
      lhsa[k-1][4][0] = - tmp2 * fjac[k-1][4][0]
        - tmp1 * njac[k-1][4][0];

      lhsa[k-1][0][1] = - tmp2 * fjac[k-1][0][1]
        - tmp1 * njac[k-1][0][1];
      lhsa[k-1][1][1] = - tmp2 * fjac[k-1][1][1]
        - tmp1 * njac[k-1][1][1]
        - tmp1 * dz2;
      lhsa[k-1][2][1] = - tmp2 * fjac[k-1][2][1]
        - tmp1 * njac[k-1][2][1];
      lhsa[k-1][3][1] = - tmp2 * fjac[k-1][3][1]
        - tmp1 * njac[k-1][3][1];
      lhsa[k-1][4][1] = - tmp2 * fjac[k-1][4][1]
        - tmp1 * njac[k-1][4][1];

      lhsa[k-1][0][2] = - tmp2 * fjac[k-1][0][2]
        - tmp1 * njac[k-1][0][2];
      lhsa[k-1][1][2] = - tmp2 * fjac[k-1][1][2]
        - tmp1 * njac[k-1][1][2];
      lhsa[k-1][2][2] = - tmp2 * fjac[k-1][2][2]
        - tmp1 * njac[k-1][2][2]
        - tmp1 * dz3;
      lhsa[k-1][3][2] = - tmp2 * fjac[k-1][3][2]
        - tmp1 * njac[k-1][3][2];
      lhsa[k-1][4][2] = - tmp2 * fjac[k-1][4][2]
        - tmp1 * njac[k-1][4][2];

      lhsa[k-1][0][3] = - tmp2 * fjac[k-1][0][3]
        - tmp1 * njac[k-1][0][3];
      lhsa[k-1][1][3] = - tmp2 * fjac[k-1][1][3]
        - tmp1 * njac[k-1][1][3];
      lhsa[k-1][2][3] = - tmp2 * fjac[k-1][2][3]
        - tmp1 * njac[k-1][2][3];
      lhsa[k-1][3][3] = - tmp2 * fjac[k-1][3][3]
        - tmp1 * njac[k-1][3][3]
        - tmp1 * dz4;
      lhsa[k-1][4][3] = - tmp2 * fjac[k-1][4][3]
        - tmp1 * njac[k-1][4][3];

      lhsa[k-1][0][4] = - tmp2 * fjac[k-1][0][4]
        - tmp1 * njac[k-1][0][4];
      lhsa[k-1][1][4] = - tmp2 * fjac[k-1][1][4]
        - tmp1 * njac[k-1][1][4];
      lhsa[k-1][2][4] = - tmp2 * fjac[k-1][2][4]
        - tmp1 * njac[k-1][2][4];
      lhsa[k-1][3][4] = - tmp2 * fjac[k-1][3][4]
        - tmp1 * njac[k-1][3][4];
      lhsa[k-1][4][4] = - tmp2 * fjac[k-1][4][4]
        - tmp1 * njac[k-1][4][4]
        - tmp1 * dz5;

      lhsb[k-1][0][0] = 1.0
        + tmp1 * 2.0 * njac[k][0][0]
        + tmp1 * 2.0 * dz1;
      lhsb[k-1][1][0] = tmp1 * 2.0 * njac[k][1][0];
      lhsb[k-1][2][0] = tmp1 * 2.0 * njac[k][2][0];
      lhsb[k-1][3][0] = tmp1 * 2.0 * njac[k][3][0];
      lhsb[k-1][4][0] = tmp1 * 2.0 * njac[k][4][0];

      lhsb[k-1][0][1] = tmp1 * 2.0 * njac[k][0][1];
      lhsb[k-1][1][1] = 1.0
        + tmp1 * 2.0 * njac[k][1][1]
        + tmp1 * 2.0 * dz2;
      lhsb[k-1][2][1] = tmp1 * 2.0 * njac[k][2][1];
      lhsb[k-1][3][1] = tmp1 * 2.0 * njac[k][3][1];
      lhsb[k-1][4][1] = tmp1 * 2.0 * njac[k][4][1];

      lhsb[k-1][0][2] = tmp1 * 2.0 * njac[k][0][2];
      lhsb[k-1][1][2] = tmp1 * 2.0 * njac[k][1][2];
      lhsb[k-1][2][2] = 1.0
        + tmp1 * 2.0 * njac[k][2][2]
        + tmp1 * 2.0 * dz3;
      lhsb[k-1][3][2] = tmp1 * 2.0 * njac[k][3][2];
      lhsb[k-1][4][2] = tmp1 * 2.0 * njac[k][4][2];

      lhsb[k-1][0][3] = tmp1 * 2.0 * njac[k][0][3];
      lhsb[k-1][1][3] = tmp1 * 2.0 * njac[k][1][3];
      lhsb[k-1][2][3] = tmp1 * 2.0 * njac[k][2][3];
      lhsb[k-1][3][3] = 1.0
        + tmp1 * 2.0 * njac[k][3][3]
        + tmp1 * 2.0 * dz4;
      lhsb[k-1][4][3] = tmp1 * 2.0 * njac[k][4][3];

      lhsb[k-1][0][4] = tmp1 * 2.0 * njac[k][0][4];
      lhsb[k-1][1][4] = tmp1 * 2.0 * njac[k][1][4];
      lhsb[k-1][2][4] = tmp1 * 2.0 * njac[k][2][4];
      lhsb[k-1][3][4] = tmp1 * 2.0 * njac[k][3][4];
      lhsb[k-1][4][4] = 1.0
        + tmp1 * 2.0 * njac[k][4][4] 
        + tmp1 * 2.0 * dz5;

      lhsc[c][k-1][j-1][i-1][0][0] =  tmp2 * fjac[k+1][0][0]
        - tmp1 * njac[k+1][0][0]
        - tmp1 * dz1;
      lhsc[c][k-1][j-1][i-1][1][0] =  tmp2 * fjac[k+1][1][0]
        - tmp1 * njac[k+1][1][0];
      lhsc[c][k-1][j-1][i-1][2][0] =  tmp2 * fjac[k+1][2][0]
        - tmp1 * njac[k+1][2][0];
      lhsc[c][k-1][j-1][i-1][3][0] =  tmp2 * fjac[k+1][3][0]
        - tmp1 * njac[k+1][3][0];
      lhsc[c][k-1][j-1][i-1][4][0] =  tmp2 * fjac[k+1][4][0]
        - tmp1 * njac[k+1][4][0];

      lhsc[c][k-1][j-1][i-1][0][1] =  tmp2 * fjac[k+1][0][1]
        - tmp1 * njac[k+1][0][1];
      lhsc[c][k-1][j-1][i-1][1][1] =  tmp2 * fjac[k+1][1][1]
        - tmp1 * njac[k+1][1][1]
        - tmp1 * dz2;
      lhsc[c][k-1][j-1][i-1][2][1] =  tmp2 * fjac[k+1][2][1]
        - tmp1 * njac[k+1][2][1];
      lhsc[c][k-1][j-1][i-1][3][1] =  tmp2 * fjac[k+1][3][1]
        - tmp1 * njac[k+1][3][1];
      lhsc[c][k-1][j-1][i-1][4][1] =  tmp2 * fjac[k+1][4][1]
        - tmp1 * njac[k+1][4][1];

      lhsc[c][k-1][j-1][i-1][0][2] =  tmp2 * fjac[k+1][0][2]
        - tmp1 * njac[k+1][0][2];
      lhsc[c][k-1][j-1][i-1][1][2] =  tmp2 * fjac[k+1][1][2]
        - tmp1 * njac[k+1][1][2];
      lhsc[c][k-1][j-1][i-1][2][2] =  tmp2 * fjac[k+1][2][2]
        - tmp1 * njac[k+1][2][2]
        - tmp1 * dz3;
      lhsc[c][k-1][j-1][i-1][3][2] =  tmp2 * fjac[k+1][3][2]
        - tmp1 * njac[k+1][3][2];
      lhsc[c][k-1][j-1][i-1][4][2] =  tmp2 * fjac[k+1][4][2]
        - tmp1 * njac[k+1][4][2];

      lhsc[c][k-1][j-1][i-1][0][3] =  tmp2 * fjac[k+1][0][3]
        - tmp1 * njac[k+1][0][3];
      lhsc[c][k-1][j-1][i-1][1][3] =  tmp2 * fjac[k+1][1][3]
        - tmp1 * njac[k+1][1][3];
      lhsc[c][k-1][j-1][i-1][2][3] =  tmp2 * fjac[k+1][2][3]
        - tmp1 * njac[k+1][2][3];
      lhsc[c][k-1][j-1][i-1][3][3] =  tmp2 * fjac[k+1][3][3]
        - tmp1 * njac[k+1][3][3]
        - tmp1 * dz4;
      lhsc[c][k-1][j-1][i-1][4][3] =  tmp2 * fjac[k+1][4][3]
        - tmp1 * njac[k+1][4][3];

      lhsc[c][k-1][j-1][i-1][0][4] =  tmp2 * fjac[k+1][0][4]
        - tmp1 * njac[k+1][0][4];
      lhsc[c][k-1][j-1][i-1][1][4] =  tmp2 * fjac[k+1][1][4]
        - tmp1 * njac[k+1][1][4];
      lhsc[c][k-1][j-1][i-1][2][4] =  tmp2 * fjac[k+1][2][4]
        - tmp1 * njac[k+1][2][4];
      lhsc[c][k-1][j-1][i-1][3][4] =  tmp2 * fjac[k+1][3][4]
        - tmp1 * njac[k+1][3][4];
      lhsc[c][k-1][j-1][i-1][4][4] =  tmp2 * fjac[k+1][4][4]
        - tmp1 * njac[k+1][4][4]
        - tmp1 * dz5;

    }


    //---------------------------------------------------------------------
    // outer most do loops - sweeping in i direction
    //---------------------------------------------------------------------
    if (first == 1) { 

      //-------------------------------------------------------------------
      // multiply c(i,j,kstart) by b_inverse and copy back to c
      // multiply rhs(kstart) by b_inverse(kstart) and copy to rhs
      //-------------------------------------------------------------------
      binvcrhs( lhsb[kstart], lhsc[c][kstart][j-1][i-1],
                rhs[c][kstart][j-1][i-1] );

    }

    //---------------------------------------------------------------------
    // begin inner most do loop
    // do all the elements of the cell unless last 
    //---------------------------------------------------------------------
    for (k = kstart+first; k <= ksize-last; k++) {

      //-------------------------------------------------------------------
      // subtract A*lhs_vector(k-1) from lhs_vector(k)
      // 
      // rhs(k) = rhs(k) - A*rhs(k-1)
      //-------------------------------------------------------------------
      matvec_sub(lhsa[k], rhs[c][k-1][j-1][i-1], rhs[c][k][j-1][i-1]);

      //-------------------------------------------------------------------
      // B(k) = B(k) - C(k-1)*A(k)
      // matmul_sub(aa,i,j,k,c,cc,i,j,k-1,c,bb,i,j,k,c)
      //-------------------------------------------------------------------
      matmul_sub(lhsa[k], lhsc[c][k-1][j-1][i-1], lhsb[k]);

      //-------------------------------------------------------------------
      // multiply c(i,j,k) by b_inverse and copy back to c
      // multiply rhs(i,j,1) by b_inverse(i,j,1) and copy to rhs
      //-------------------------------------------------------------------
      binvcrhs( lhsb[k], lhsc[c][k][j-1][i-1], rhs[c][k][j-1][i-1] );

    }

    //---------------------------------------------------------------------
    // Now finish up special cases for last cell
    //---------------------------------------------------------------------
    if (last == 1) {

      //-------------------------------------------------------------------
      // rhs(ksize) = rhs(ksize) - A*rhs(ksize-1)
      //-------------------------------------------------------------------
      matvec_sub(lhsa[ksize], rhs[c][ksize-1][j-1][i-1],
                 rhs[c][ksize][j-1][i-1]);

      //-------------------------------------------------------------------
      // B(ksize) = B(ksize) - C(ksize-1)*A(ksize)
      // matmul_sub(aa,i,j,ksize,c,
      // $              cc,i,j,ksize-1,c,bb,i,j,ksize,c)
      //-------------------------------------------------------------------
      matmul_sub(lhsa[ksize], lhsc[c][ksize-1][j-1][i-1], lhsb[ksize]);

      //-------------------------------------------------------------------
      // multiply rhs(ksize) by b_inverse(ksize) and copy to rhs
      //-------------------------------------------------------------------
      binvrhs( lhsb[ksize], rhs[c][ksize][j-1][i-1] );

    }
  }
#endif
}


void lhsabinit(__global double (*lhsa)[5][5],
               __global double (*lhsb)[5][5], int size)
{
  int i, m, n;

  //---------------------------------------------------------------------
  // next, set all diagonal values to 1. This is overkill, but convenient
  //---------------------------------------------------------------------
  for (i = 1; i <= size; i++) {
    for (n = 0; n < 5; n++) {
      for (m = 0; m < 5; m++) {
        lhsa[i][n][m] = 0.0;
        lhsb[i][n][m] = 0.0;
      }
      lhsb[i][n][n] = 1.0;
    }
  }
}


//---------------------------------------------------------------------
// subtracts bvec=bvec - ablock*avec
//---------------------------------------------------------------------
void matvec_sub(__global double ablock[5][5],
                __global double avec[5],
                __global double bvec[5])
{
  //---------------------------------------------------------------------
  // rhs[ccell][kc][jc][ic][i] = rhs[ccell][kc][jc][ic][i] 
  // $                         - lhs[acell][ka][ja][ia][ablock][0][i]*
  //---------------------------------------------------------------------
  bvec[0] = bvec[0] - ablock[0][0]*avec[0]
                    - ablock[1][0]*avec[1]
                    - ablock[2][0]*avec[2]
                    - ablock[3][0]*avec[3]
                    - ablock[4][0]*avec[4];
  bvec[1] = bvec[1] - ablock[0][1]*avec[0]
                    - ablock[1][1]*avec[1]
                    - ablock[2][1]*avec[2]
                    - ablock[3][1]*avec[3]
                    - ablock[4][1]*avec[4];
  bvec[2] = bvec[2] - ablock[0][2]*avec[0]
                    - ablock[1][2]*avec[1]
                    - ablock[2][2]*avec[2]
                    - ablock[3][2]*avec[3]
                    - ablock[4][2]*avec[4];
  bvec[3] = bvec[3] - ablock[0][3]*avec[0]
                    - ablock[1][3]*avec[1]
                    - ablock[2][3]*avec[2]
                    - ablock[3][3]*avec[3]
                    - ablock[4][3]*avec[4];
  bvec[4] = bvec[4] - ablock[0][4]*avec[0]
                    - ablock[1][4]*avec[1]
                    - ablock[2][4]*avec[2]
                    - ablock[3][4]*avec[3]
                    - ablock[4][4]*avec[4];
}


//---------------------------------------------------------------------
// subtracts a(i,j,k) X b(i,j,k) from c(i,j,k)
//---------------------------------------------------------------------
void matmul_sub(__global double ablock[5][5],
                __global double bblock[5][5],
                __global double cblock[5][5])
{
  cblock[0][0] = cblock[0][0] - ablock[0][0]*bblock[0][0]
                              - ablock[1][0]*bblock[0][1]
                              - ablock[2][0]*bblock[0][2]
                              - ablock[3][0]*bblock[0][3]
                              - ablock[4][0]*bblock[0][4];
  cblock[0][1] = cblock[0][1] - ablock[0][1]*bblock[0][0]
                              - ablock[1][1]*bblock[0][1]
                              - ablock[2][1]*bblock[0][2]
                              - ablock[3][1]*bblock[0][3]
                              - ablock[4][1]*bblock[0][4];
  cblock[0][2] = cblock[0][2] - ablock[0][2]*bblock[0][0]
                              - ablock[1][2]*bblock[0][1]
                              - ablock[2][2]*bblock[0][2]
                              - ablock[3][2]*bblock[0][3]
                              - ablock[4][2]*bblock[0][4];
  cblock[0][3] = cblock[0][3] - ablock[0][3]*bblock[0][0]
                              - ablock[1][3]*bblock[0][1]
                              - ablock[2][3]*bblock[0][2]
                              - ablock[3][3]*bblock[0][3]
                              - ablock[4][3]*bblock[0][4];
  cblock[0][4] = cblock[0][4] - ablock[0][4]*bblock[0][0]
                              - ablock[1][4]*bblock[0][1]
                              - ablock[2][4]*bblock[0][2]
                              - ablock[3][4]*bblock[0][3]
                              - ablock[4][4]*bblock[0][4];
  cblock[1][0] = cblock[1][0] - ablock[0][0]*bblock[1][0]
                              - ablock[1][0]*bblock[1][1]
                              - ablock[2][0]*bblock[1][2]
                              - ablock[3][0]*bblock[1][3]
                              - ablock[4][0]*bblock[1][4];
  cblock[1][1] = cblock[1][1] - ablock[0][1]*bblock[1][0]
                              - ablock[1][1]*bblock[1][1]
                              - ablock[2][1]*bblock[1][2]
                              - ablock[3][1]*bblock[1][3]
                              - ablock[4][1]*bblock[1][4];
  cblock[1][2] = cblock[1][2] - ablock[0][2]*bblock[1][0]
                              - ablock[1][2]*bblock[1][1]
                              - ablock[2][2]*bblock[1][2]
                              - ablock[3][2]*bblock[1][3]
                              - ablock[4][2]*bblock[1][4];
  cblock[1][3] = cblock[1][3] - ablock[0][3]*bblock[1][0]
                              - ablock[1][3]*bblock[1][1]
                              - ablock[2][3]*bblock[1][2]
                              - ablock[3][3]*bblock[1][3]
                              - ablock[4][3]*bblock[1][4];
  cblock[1][4] = cblock[1][4] - ablock[0][4]*bblock[1][0]
                              - ablock[1][4]*bblock[1][1]
                              - ablock[2][4]*bblock[1][2]
                              - ablock[3][4]*bblock[1][3]
                              - ablock[4][4]*bblock[1][4];
  cblock[2][0] = cblock[2][0] - ablock[0][0]*bblock[2][0]
                              - ablock[1][0]*bblock[2][1]
                              - ablock[2][0]*bblock[2][2]
                              - ablock[3][0]*bblock[2][3]
                              - ablock[4][0]*bblock[2][4];
  cblock[2][1] = cblock[2][1] - ablock[0][1]*bblock[2][0]
                              - ablock[1][1]*bblock[2][1]
                              - ablock[2][1]*bblock[2][2]
                              - ablock[3][1]*bblock[2][3]
                              - ablock[4][1]*bblock[2][4];
  cblock[2][2] = cblock[2][2] - ablock[0][2]*bblock[2][0]
                              - ablock[1][2]*bblock[2][1]
                              - ablock[2][2]*bblock[2][2]
                              - ablock[3][2]*bblock[2][3]
                              - ablock[4][2]*bblock[2][4];
  cblock[2][3] = cblock[2][3] - ablock[0][3]*bblock[2][0]
                              - ablock[1][3]*bblock[2][1]
                              - ablock[2][3]*bblock[2][2]
                              - ablock[3][3]*bblock[2][3]
                              - ablock[4][3]*bblock[2][4];
  cblock[2][4] = cblock[2][4] - ablock[0][4]*bblock[2][0]
                              - ablock[1][4]*bblock[2][1]
                              - ablock[2][4]*bblock[2][2]
                              - ablock[3][4]*bblock[2][3]
                              - ablock[4][4]*bblock[2][4];
  cblock[3][0] = cblock[3][0] - ablock[0][0]*bblock[3][0]
                              - ablock[1][0]*bblock[3][1]
                              - ablock[2][0]*bblock[3][2]
                              - ablock[3][0]*bblock[3][3]
                              - ablock[4][0]*bblock[3][4];
  cblock[3][1] = cblock[3][1] - ablock[0][1]*bblock[3][0]
                              - ablock[1][1]*bblock[3][1]
                              - ablock[2][1]*bblock[3][2]
                              - ablock[3][1]*bblock[3][3]
                              - ablock[4][1]*bblock[3][4];
  cblock[3][2] = cblock[3][2] - ablock[0][2]*bblock[3][0]
                              - ablock[1][2]*bblock[3][1]
                              - ablock[2][2]*bblock[3][2]
                              - ablock[3][2]*bblock[3][3]
                              - ablock[4][2]*bblock[3][4];
  cblock[3][3] = cblock[3][3] - ablock[0][3]*bblock[3][0]
                              - ablock[1][3]*bblock[3][1]
                              - ablock[2][3]*bblock[3][2]
                              - ablock[3][3]*bblock[3][3]
                              - ablock[4][3]*bblock[3][4];
  cblock[3][4] = cblock[3][4] - ablock[0][4]*bblock[3][0]
                              - ablock[1][4]*bblock[3][1]
                              - ablock[2][4]*bblock[3][2]
                              - ablock[3][4]*bblock[3][3]
                              - ablock[4][4]*bblock[3][4];
  cblock[4][0] = cblock[4][0] - ablock[0][0]*bblock[4][0]
                              - ablock[1][0]*bblock[4][1]
                              - ablock[2][0]*bblock[4][2]
                              - ablock[3][0]*bblock[4][3]
                              - ablock[4][0]*bblock[4][4];
  cblock[4][1] = cblock[4][1] - ablock[0][1]*bblock[4][0]
                              - ablock[1][1]*bblock[4][1]
                              - ablock[2][1]*bblock[4][2]
                              - ablock[3][1]*bblock[4][3]
                              - ablock[4][1]*bblock[4][4];
  cblock[4][2] = cblock[4][2] - ablock[0][2]*bblock[4][0]
                              - ablock[1][2]*bblock[4][1]
                              - ablock[2][2]*bblock[4][2]
                              - ablock[3][2]*bblock[4][3]
                              - ablock[4][2]*bblock[4][4];
  cblock[4][3] = cblock[4][3] - ablock[0][3]*bblock[4][0]
                              - ablock[1][3]*bblock[4][1]
                              - ablock[2][3]*bblock[4][2]
                              - ablock[3][3]*bblock[4][3]
                              - ablock[4][3]*bblock[4][4];
  cblock[4][4] = cblock[4][4] - ablock[0][4]*bblock[4][0]
                              - ablock[1][4]*bblock[4][1]
                              - ablock[2][4]*bblock[4][2]
                              - ablock[3][4]*bblock[4][3]
                              - ablock[4][4]*bblock[4][4];
}


void binvcrhs(__global double lhs[5][5],
              __global double c[5][5],
              __global double r[5])
{
  double pivot, coeff;

  pivot = 1.00/lhs[0][0];
  lhs[1][0] = lhs[1][0]*pivot;
  lhs[2][0] = lhs[2][0]*pivot;
  lhs[3][0] = lhs[3][0]*pivot;
  lhs[4][0] = lhs[4][0]*pivot;
  c[0][0] = c[0][0]*pivot;
  c[1][0] = c[1][0]*pivot;
  c[2][0] = c[2][0]*pivot;
  c[3][0] = c[3][0]*pivot;
  c[4][0] = c[4][0]*pivot;
  r[0]   = r[0]  *pivot;

  coeff = lhs[0][1];
  lhs[1][1]= lhs[1][1] - coeff*lhs[1][0];
  lhs[2][1]= lhs[2][1] - coeff*lhs[2][0];
  lhs[3][1]= lhs[3][1] - coeff*lhs[3][0];
  lhs[4][1]= lhs[4][1] - coeff*lhs[4][0];
  c[0][1] = c[0][1] - coeff*c[0][0];
  c[1][1] = c[1][1] - coeff*c[1][0];
  c[2][1] = c[2][1] - coeff*c[2][0];
  c[3][1] = c[3][1] - coeff*c[3][0];
  c[4][1] = c[4][1] - coeff*c[4][0];
  r[1]   = r[1]   - coeff*r[0];

  coeff = lhs[0][2];
  lhs[1][2]= lhs[1][2] - coeff*lhs[1][0];
  lhs[2][2]= lhs[2][2] - coeff*lhs[2][0];
  lhs[3][2]= lhs[3][2] - coeff*lhs[3][0];
  lhs[4][2]= lhs[4][2] - coeff*lhs[4][0];
  c[0][2] = c[0][2] - coeff*c[0][0];
  c[1][2] = c[1][2] - coeff*c[1][0];
  c[2][2] = c[2][2] - coeff*c[2][0];
  c[3][2] = c[3][2] - coeff*c[3][0];
  c[4][2] = c[4][2] - coeff*c[4][0];
  r[2]   = r[2]   - coeff*r[0];

  coeff = lhs[0][3];
  lhs[1][3]= lhs[1][3] - coeff*lhs[1][0];
  lhs[2][3]= lhs[2][3] - coeff*lhs[2][0];
  lhs[3][3]= lhs[3][3] - coeff*lhs[3][0];
  lhs[4][3]= lhs[4][3] - coeff*lhs[4][0];
  c[0][3] = c[0][3] - coeff*c[0][0];
  c[1][3] = c[1][3] - coeff*c[1][0];
  c[2][3] = c[2][3] - coeff*c[2][0];
  c[3][3] = c[3][3] - coeff*c[3][0];
  c[4][3] = c[4][3] - coeff*c[4][0];
  r[3]   = r[3]   - coeff*r[0];

  coeff = lhs[0][4];
  lhs[1][4]= lhs[1][4] - coeff*lhs[1][0];
  lhs[2][4]= lhs[2][4] - coeff*lhs[2][0];
  lhs[3][4]= lhs[3][4] - coeff*lhs[3][0];
  lhs[4][4]= lhs[4][4] - coeff*lhs[4][0];
  c[0][4] = c[0][4] - coeff*c[0][0];
  c[1][4] = c[1][4] - coeff*c[1][0];
  c[2][4] = c[2][4] - coeff*c[2][0];
  c[3][4] = c[3][4] - coeff*c[3][0];
  c[4][4] = c[4][4] - coeff*c[4][0];
  r[4]   = r[4]   - coeff*r[0];


  pivot = 1.00/lhs[1][1];
  lhs[2][1] = lhs[2][1]*pivot;
  lhs[3][1] = lhs[3][1]*pivot;
  lhs[4][1] = lhs[4][1]*pivot;
  c[0][1] = c[0][1]*pivot;
  c[1][1] = c[1][1]*pivot;
  c[2][1] = c[2][1]*pivot;
  c[3][1] = c[3][1]*pivot;
  c[4][1] = c[4][1]*pivot;
  r[1]   = r[1]  *pivot;

  coeff = lhs[1][0];
  lhs[2][0]= lhs[2][0] - coeff*lhs[2][1];
  lhs[3][0]= lhs[3][0] - coeff*lhs[3][1];
  lhs[4][0]= lhs[4][0] - coeff*lhs[4][1];
  c[0][0] = c[0][0] - coeff*c[0][1];
  c[1][0] = c[1][0] - coeff*c[1][1];
  c[2][0] = c[2][0] - coeff*c[2][1];
  c[3][0] = c[3][0] - coeff*c[3][1];
  c[4][0] = c[4][0] - coeff*c[4][1];
  r[0]   = r[0]   - coeff*r[1];

  coeff = lhs[1][2];
  lhs[2][2]= lhs[2][2] - coeff*lhs[2][1];
  lhs[3][2]= lhs[3][2] - coeff*lhs[3][1];
  lhs[4][2]= lhs[4][2] - coeff*lhs[4][1];
  c[0][2] = c[0][2] - coeff*c[0][1];
  c[1][2] = c[1][2] - coeff*c[1][1];
  c[2][2] = c[2][2] - coeff*c[2][1];
  c[3][2] = c[3][2] - coeff*c[3][1];
  c[4][2] = c[4][2] - coeff*c[4][1];
  r[2]   = r[2]   - coeff*r[1];

  coeff = lhs[1][3];
  lhs[2][3]= lhs[2][3] - coeff*lhs[2][1];
  lhs[3][3]= lhs[3][3] - coeff*lhs[3][1];
  lhs[4][3]= lhs[4][3] - coeff*lhs[4][1];
  c[0][3] = c[0][3] - coeff*c[0][1];
  c[1][3] = c[1][3] - coeff*c[1][1];
  c[2][3] = c[2][3] - coeff*c[2][1];
  c[3][3] = c[3][3] - coeff*c[3][1];
  c[4][3] = c[4][3] - coeff*c[4][1];
  r[3]   = r[3]   - coeff*r[1];

  coeff = lhs[1][4];
  lhs[2][4]= lhs[2][4] - coeff*lhs[2][1];
  lhs[3][4]= lhs[3][4] - coeff*lhs[3][1];
  lhs[4][4]= lhs[4][4] - coeff*lhs[4][1];
  c[0][4] = c[0][4] - coeff*c[0][1];
  c[1][4] = c[1][4] - coeff*c[1][1];
  c[2][4] = c[2][4] - coeff*c[2][1];
  c[3][4] = c[3][4] - coeff*c[3][1];
  c[4][4] = c[4][4] - coeff*c[4][1];
  r[4]   = r[4]   - coeff*r[1];


  pivot = 1.00/lhs[2][2];
  lhs[3][2] = lhs[3][2]*pivot;
  lhs[4][2] = lhs[4][2]*pivot;
  c[0][2] = c[0][2]*pivot;
  c[1][2] = c[1][2]*pivot;
  c[2][2] = c[2][2]*pivot;
  c[3][2] = c[3][2]*pivot;
  c[4][2] = c[4][2]*pivot;
  r[2]   = r[2]  *pivot;

  coeff = lhs[2][0];
  lhs[3][0]= lhs[3][0] - coeff*lhs[3][2];
  lhs[4][0]= lhs[4][0] - coeff*lhs[4][2];
  c[0][0] = c[0][0] - coeff*c[0][2];
  c[1][0] = c[1][0] - coeff*c[1][2];
  c[2][0] = c[2][0] - coeff*c[2][2];
  c[3][0] = c[3][0] - coeff*c[3][2];
  c[4][0] = c[4][0] - coeff*c[4][2];
  r[0]   = r[0]   - coeff*r[2];

  coeff = lhs[2][1];
  lhs[3][1]= lhs[3][1] - coeff*lhs[3][2];
  lhs[4][1]= lhs[4][1] - coeff*lhs[4][2];
  c[0][1] = c[0][1] - coeff*c[0][2];
  c[1][1] = c[1][1] - coeff*c[1][2];
  c[2][1] = c[2][1] - coeff*c[2][2];
  c[3][1] = c[3][1] - coeff*c[3][2];
  c[4][1] = c[4][1] - coeff*c[4][2];
  r[1]   = r[1]   - coeff*r[2];

  coeff = lhs[2][3];
  lhs[3][3]= lhs[3][3] - coeff*lhs[3][2];
  lhs[4][3]= lhs[4][3] - coeff*lhs[4][2];
  c[0][3] = c[0][3] - coeff*c[0][2];
  c[1][3] = c[1][3] - coeff*c[1][2];
  c[2][3] = c[2][3] - coeff*c[2][2];
  c[3][3] = c[3][3] - coeff*c[3][2];
  c[4][3] = c[4][3] - coeff*c[4][2];
  r[3]   = r[3]   - coeff*r[2];

  coeff = lhs[2][4];
  lhs[3][4]= lhs[3][4] - coeff*lhs[3][2];
  lhs[4][4]= lhs[4][4] - coeff*lhs[4][2];
  c[0][4] = c[0][4] - coeff*c[0][2];
  c[1][4] = c[1][4] - coeff*c[1][2];
  c[2][4] = c[2][4] - coeff*c[2][2];
  c[3][4] = c[3][4] - coeff*c[3][2];
  c[4][4] = c[4][4] - coeff*c[4][2];
  r[4]   = r[4]   - coeff*r[2];


  pivot = 1.00/lhs[3][3];
  lhs[4][3] = lhs[4][3]*pivot;
  c[0][3] = c[0][3]*pivot;
  c[1][3] = c[1][3]*pivot;
  c[2][3] = c[2][3]*pivot;
  c[3][3] = c[3][3]*pivot;
  c[4][3] = c[4][3]*pivot;
  r[3]   = r[3]  *pivot;

  coeff = lhs[3][0];
  lhs[4][0]= lhs[4][0] - coeff*lhs[4][3];
  c[0][0] = c[0][0] - coeff*c[0][3];
  c[1][0] = c[1][0] - coeff*c[1][3];
  c[2][0] = c[2][0] - coeff*c[2][3];
  c[3][0] = c[3][0] - coeff*c[3][3];
  c[4][0] = c[4][0] - coeff*c[4][3];
  r[0]   = r[0]   - coeff*r[3];

  coeff = lhs[3][1];
  lhs[4][1]= lhs[4][1] - coeff*lhs[4][3];
  c[0][1] = c[0][1] - coeff*c[0][3];
  c[1][1] = c[1][1] - coeff*c[1][3];
  c[2][1] = c[2][1] - coeff*c[2][3];
  c[3][1] = c[3][1] - coeff*c[3][3];
  c[4][1] = c[4][1] - coeff*c[4][3];
  r[1]   = r[1]   - coeff*r[3];

  coeff = lhs[3][2];
  lhs[4][2]= lhs[4][2] - coeff*lhs[4][3];
  c[0][2] = c[0][2] - coeff*c[0][3];
  c[1][2] = c[1][2] - coeff*c[1][3];
  c[2][2] = c[2][2] - coeff*c[2][3];
  c[3][2] = c[3][2] - coeff*c[3][3];
  c[4][2] = c[4][2] - coeff*c[4][3];
  r[2]   = r[2]   - coeff*r[3];

  coeff = lhs[3][4];
  lhs[4][4]= lhs[4][4] - coeff*lhs[4][3];
  c[0][4] = c[0][4] - coeff*c[0][3];
  c[1][4] = c[1][4] - coeff*c[1][3];
  c[2][4] = c[2][4] - coeff*c[2][3];
  c[3][4] = c[3][4] - coeff*c[3][3];
  c[4][4] = c[4][4] - coeff*c[4][3];
  r[4]   = r[4]   - coeff*r[3];


  pivot = 1.00/lhs[4][4];
  c[0][4] = c[0][4]*pivot;
  c[1][4] = c[1][4]*pivot;
  c[2][4] = c[2][4]*pivot;
  c[3][4] = c[3][4]*pivot;
  c[4][4] = c[4][4]*pivot;
  r[4]   = r[4]  *pivot;

  coeff = lhs[4][0];
  c[0][0] = c[0][0] - coeff*c[0][4];
  c[1][0] = c[1][0] - coeff*c[1][4];
  c[2][0] = c[2][0] - coeff*c[2][4];
  c[3][0] = c[3][0] - coeff*c[3][4];
  c[4][0] = c[4][0] - coeff*c[4][4];
  r[0]   = r[0]   - coeff*r[4];

  coeff = lhs[4][1];
  c[0][1] = c[0][1] - coeff*c[0][4];
  c[1][1] = c[1][1] - coeff*c[1][4];
  c[2][1] = c[2][1] - coeff*c[2][4];
  c[3][1] = c[3][1] - coeff*c[3][4];
  c[4][1] = c[4][1] - coeff*c[4][4];
  r[1]   = r[1]   - coeff*r[4];

  coeff = lhs[4][2];
  c[0][2] = c[0][2] - coeff*c[0][4];
  c[1][2] = c[1][2] - coeff*c[1][4];
  c[2][2] = c[2][2] - coeff*c[2][4];
  c[3][2] = c[3][2] - coeff*c[3][4];
  c[4][2] = c[4][2] - coeff*c[4][4];
  r[2]   = r[2]   - coeff*r[4];

  coeff = lhs[4][3];
  c[0][3] = c[0][3] - coeff*c[0][4];
  c[1][3] = c[1][3] - coeff*c[1][4];
  c[2][3] = c[2][3] - coeff*c[2][4];
  c[3][3] = c[3][3] - coeff*c[3][4];
  c[4][3] = c[4][3] - coeff*c[4][4];
  r[3]   = r[3]   - coeff*r[4];
}


void binvrhs(__global double lhs[5][5], __global double r[5])
{
  double pivot, coeff;

  pivot = 1.00/lhs[0][0];
  lhs[1][0] = lhs[1][0]*pivot;
  lhs[2][0] = lhs[2][0]*pivot;
  lhs[3][0] = lhs[3][0]*pivot;
  lhs[4][0] = lhs[4][0]*pivot;
  r[0]   = r[0]  *pivot;

  coeff = lhs[0][1];
  lhs[1][1]= lhs[1][1] - coeff*lhs[1][0];
  lhs[2][1]= lhs[2][1] - coeff*lhs[2][0];
  lhs[3][1]= lhs[3][1] - coeff*lhs[3][0];
  lhs[4][1]= lhs[4][1] - coeff*lhs[4][0];
  r[1]   = r[1]   - coeff*r[0];

  coeff = lhs[0][2];
  lhs[1][2]= lhs[1][2] - coeff*lhs[1][0];
  lhs[2][2]= lhs[2][2] - coeff*lhs[2][0];
  lhs[3][2]= lhs[3][2] - coeff*lhs[3][0];
  lhs[4][2]= lhs[4][2] - coeff*lhs[4][0];
  r[2]   = r[2]   - coeff*r[0];

  coeff = lhs[0][3];
  lhs[1][3]= lhs[1][3] - coeff*lhs[1][0];
  lhs[2][3]= lhs[2][3] - coeff*lhs[2][0];
  lhs[3][3]= lhs[3][3] - coeff*lhs[3][0];
  lhs[4][3]= lhs[4][3] - coeff*lhs[4][0];
  r[3]   = r[3]   - coeff*r[0];

  coeff = lhs[0][4];
  lhs[1][4]= lhs[1][4] - coeff*lhs[1][0];
  lhs[2][4]= lhs[2][4] - coeff*lhs[2][0];
  lhs[3][4]= lhs[3][4] - coeff*lhs[3][0];
  lhs[4][4]= lhs[4][4] - coeff*lhs[4][0];
  r[4]   = r[4]   - coeff*r[0];


  pivot = 1.00/lhs[1][1];
  lhs[2][1] = lhs[2][1]*pivot;
  lhs[3][1] = lhs[3][1]*pivot;
  lhs[4][1] = lhs[4][1]*pivot;
  r[1]   = r[1]  *pivot;

  coeff = lhs[1][0];
  lhs[2][0]= lhs[2][0] - coeff*lhs[2][1];
  lhs[3][0]= lhs[3][0] - coeff*lhs[3][1];
  lhs[4][0]= lhs[4][0] - coeff*lhs[4][1];
  r[0]   = r[0]   - coeff*r[1];

  coeff = lhs[1][2];
  lhs[2][2]= lhs[2][2] - coeff*lhs[2][1];
  lhs[3][2]= lhs[3][2] - coeff*lhs[3][1];
  lhs[4][2]= lhs[4][2] - coeff*lhs[4][1];
  r[2]   = r[2]   - coeff*r[1];

  coeff = lhs[1][3];
  lhs[2][3]= lhs[2][3] - coeff*lhs[2][1];
  lhs[3][3]= lhs[3][3] - coeff*lhs[3][1];
  lhs[4][3]= lhs[4][3] - coeff*lhs[4][1];
  r[3]   = r[3]   - coeff*r[1];

  coeff = lhs[1][4];
  lhs[2][4]= lhs[2][4] - coeff*lhs[2][1];
  lhs[3][4]= lhs[3][4] - coeff*lhs[3][1];
  lhs[4][4]= lhs[4][4] - coeff*lhs[4][1];
  r[4]   = r[4]   - coeff*r[1];


  pivot = 1.00/lhs[2][2];
  lhs[3][2] = lhs[3][2]*pivot;
  lhs[4][2] = lhs[4][2]*pivot;
  r[2]   = r[2]  *pivot;

  coeff = lhs[2][0];
  lhs[3][0]= lhs[3][0] - coeff*lhs[3][2];
  lhs[4][0]= lhs[4][0] - coeff*lhs[4][2];
  r[0]   = r[0]   - coeff*r[2];

  coeff = lhs[2][1];
  lhs[3][1]= lhs[3][1] - coeff*lhs[3][2];
  lhs[4][1]= lhs[4][1] - coeff*lhs[4][2];
  r[1]   = r[1]   - coeff*r[2];

  coeff = lhs[2][3];
  lhs[3][3]= lhs[3][3] - coeff*lhs[3][2];
  lhs[4][3]= lhs[4][3] - coeff*lhs[4][2];
  r[3]   = r[3]   - coeff*r[2];

  coeff = lhs[2][4];
  lhs[3][4]= lhs[3][4] - coeff*lhs[3][2];
  lhs[4][4]= lhs[4][4] - coeff*lhs[4][2];
  r[4]   = r[4]   - coeff*r[2];


  pivot = 1.00/lhs[3][3];
  lhs[4][3] = lhs[4][3]*pivot;
  r[3]   = r[3]  *pivot;

  coeff = lhs[3][0];
  lhs[4][0]= lhs[4][0] - coeff*lhs[4][3];
  r[0]   = r[0]   - coeff*r[3];

  coeff = lhs[3][1];
  lhs[4][1]= lhs[4][1] - coeff*lhs[4][3];
  r[1]   = r[1]   - coeff*r[3];

  coeff = lhs[3][2];
  lhs[4][2]= lhs[4][2] - coeff*lhs[4][3];
  r[2]   = r[2]   - coeff*r[3];

  coeff = lhs[3][4];
  lhs[4][4]= lhs[4][4] - coeff*lhs[4][3];
  r[4]   = r[4]   - coeff*r[3];


  pivot = 1.00/lhs[4][4];
  r[4]   = r[4]  *pivot;

  coeff = lhs[4][0];
  r[0]   = r[0]   - coeff*r[4];

  coeff = lhs[4][1];
  r[1]   = r[1]   - coeff*r[4];

  coeff = lhs[4][2];
  r[2]   = r[2]   - coeff*r[4];

  coeff = lhs[4][3];
  r[3]   = r[3]   - coeff*r[4];
}

