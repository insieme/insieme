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

#include "sp.h"

//---------------------------------------------------------------------
// this function returns the exact solution at point xi, eta, zeta  
//---------------------------------------------------------------------
void exact_solution(double xi, double eta, double zeta, double dtemp[5],
                    __constant double *g_ce)
{
  int m;
  __constant double (*ce)[13] = (__constant double (*)[13])g_ce;

  for (m = 0; m < 5; m++) {
    dtemp[m] = ce[m][0] +
      xi  *(ce[m][1] + xi  *(ce[m][4] + xi  *(ce[m][7] + xi  *ce[m][10]))) +
      eta *(ce[m][2] + eta *(ce[m][5] + eta *(ce[m][8] + eta *ce[m][11]))) +
      zeta*(ce[m][3] + zeta*(ce[m][6] + zeta*(ce[m][9] + zeta*ce[m][12])));
  }
}


__kernel void initialize1(__global double *g_u,
                          int ncells)
{
  int c = get_global_id(2);
  int kk = get_global_id(1) + 1;
  int jj = get_global_id(0) + 1;
  if (c >= ncells || kk >= IMAX+3 || jj >= IMAX+3) return;

  int ii;
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;

  //---------------------------------------------------------------------
  // Later (in compute_rhs) we compute 1/u for every element. A few of 
  // the corner elements are not used, but it convenient (and faster) 
  // to compute the whole thing with a simple loop. Make sure those 
  // values are nonzero by initializing the whole thing here. 
  //---------------------------------------------------------------------

  for (ii = 1; ii < IMAX+3; ii++) {
    u[c][kk][jj][ii][0] = 1.0;
    u[c][kk][jj][ii][1] = 0.0;
    u[c][kk][jj][ii][2] = 0.0;
    u[c][kk][jj][ii][3] = 0.0;
    u[c][kk][jj][ii][4] = 1.0;
  }
}

__kernel void initialize2(__global double *g_u,
                          __constant double *g_ce,
                          int c,
                          int cell_low_k, int cell_high_k,
                          int cell_low_j, int cell_high_j,
                          int cell_low_i, int cell_high_i)
{
  int k = get_global_id(1) + cell_low_k;
  int kk = get_global_id(1) + 2;
  int j = get_global_id(0) + cell_low_j;
  int jj = get_global_id(0) + 2;
  if (k > cell_high_k || j > cell_high_j) return;

  int i, ii, ix, iy, iz, m;
  double xi, eta, zeta, Pface[2][3][5], Pxi, Peta, Pzeta;
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;

  //---------------------------------------------------------------------
  // first store the "interpolated" values everywhere on the grid    
  //---------------------------------------------------------------------
  zeta = (double)(k) * dnzm1;
  eta = (double)(j) * dnym1;
  ii = 2;
  for (i = cell_low_i; i <= cell_high_i; i++) {
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

__kernel void initialize3(__global double *g_u,
                          __constant double *g_ce,
                          int c,
                          int cell_low_k, int cell_high_k,
                          int cell_low_j, int cell_high_j)
{
  int k = get_global_id(1) + cell_low_k;
  int kk = get_global_id(1) + 2;
  int j = get_global_id(0) + cell_low_j;
  int jj = get_global_id(0) + 2;
  if (k > cell_high_k || j > cell_high_j) return;

  int ii, m;
  double xi, eta, zeta, temp[5];
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;

  //---------------------------------------------------------------------
  // now store the exact values on the boundaries        
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // west face                                                  
  //---------------------------------------------------------------------
  zeta = (double)(k) * dnzm1;
  eta = (double)(j) * dnym1;
  ii = 2;
  xi = 0.0;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}

__kernel void initialize4(__global double *g_u,
                          __constant double *g_ce,
                          int c,
                          int cell_low_k, int cell_high_k,
                          int cell_low_j, int cell_high_j,
                          int cell_size_i)
{
  int k = get_global_id(1) + cell_low_k;
  int kk = get_global_id(1) + 2;
  int j = get_global_id(0) + cell_low_j;
  int jj = get_global_id(0) + 2;
  if (k > cell_high_k || j > cell_high_j) return;

  int ii, m;
  double xi, eta, zeta, temp[5];
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;

  //---------------------------------------------------------------------
  // east face                                                  
  //---------------------------------------------------------------------
  zeta = (double)(k) * dnzm1;
  eta = (double)(j) * dnym1;
  ii = cell_size_i+1;
  xi = 1.0;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}

__kernel void initialize5(__global double *g_u,
                          __constant double *g_ce,
                          int c,
                          int cell_low_k, int cell_high_k,
                          int cell_low_i, int cell_high_i)
{
  int k = get_global_id(1) + cell_low_k;
  int kk = get_global_id(1) + 2;
  int i = get_global_id(0) + cell_low_i;
  int ii = get_global_id(0) + 2;
  if (k > cell_high_k || i > cell_high_i) return;

  int jj, m;
  double xi, eta, zeta, temp[5];
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;

  //---------------------------------------------------------------------
  // south face                                                  
  //---------------------------------------------------------------------
  zeta = (double)(k) * dnzm1;
  xi = (double)(i) * dnxm1;
  jj = 2;
  eta = 0.0;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}

__kernel void initialize6(__global double *g_u,
                          __constant double *g_ce,
                          int c,
                          int cell_low_k, int cell_high_k,
                          int cell_low_i, int cell_high_i,
                          int cell_size_j)
{
  int k = get_global_id(1) + cell_low_k;
  int kk = get_global_id(1) + 2;
  int i = get_global_id(0) + cell_low_i;
  int ii = get_global_id(0) + 2;
  if (k > cell_high_k || i > cell_high_i) return;

  int jj, m;
  double xi, eta, zeta, temp[5];
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;

  //---------------------------------------------------------------------
  // north face                                                  
  //---------------------------------------------------------------------
  zeta = (double)(k) * dnzm1;
  xi = (double)(i) * dnxm1;
  jj = cell_size_j+1;
  eta = 1.0;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}

__kernel void initialize7(__global double *g_u,
                          __constant double *g_ce,
                          int c,
                          int cell_low_j, int cell_high_j,
                          int cell_low_i, int cell_high_i)
{
  int j = get_global_id(1) + cell_low_j;
  int jj = get_global_id(1) + 2;
  int i = get_global_id(0) + cell_low_i;
  int ii = get_global_id(0) + 2;
  if (j > cell_high_j || i > cell_high_i) return;

  int kk, m;
  double xi, eta, zeta, temp[5];
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;

  //---------------------------------------------------------------------
  // bottom face                                                  
  //---------------------------------------------------------------------
  eta = (double)(j) * dnym1;
  xi = (double)(i) * dnxm1;
  kk = 2;
  zeta = 0.0;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}

__kernel void initialize8(__global double *g_u,
                          __constant double *g_ce,
                          int c,
                          int cell_low_j, int cell_high_j,
                          int cell_low_i, int cell_high_i,
                          int cell_size_k)
{
  int j = get_global_id(1) + cell_low_j;
  int jj = get_global_id(1) + 2;
  int i = get_global_id(0) + cell_low_i;
  int ii = get_global_id(0) + 2;
  if (j > cell_high_j || i > cell_high_i) return;

  int kk, m;
  double xi, eta, zeta, temp[5];
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;

  //---------------------------------------------------------------------
  // top face                                                  
  //---------------------------------------------------------------------
  eta = (double)(j) * dnym1;
  xi = (double)(i) * dnxm1;
  kk = cell_size_k+1;
  zeta = 1.0;
  exact_solution(xi, eta, zeta, temp, g_ce);
  for (m = 0; m < 5; m++) {
    u[c][kk][jj][ii][m] = temp[m];
  }
}


__kernel void lhsinit(__global double* g_lhs,
                      int c,
                      int cell_size_k,
                      int cell_size_j,
                      int cell_size_i)
{
  int k = get_global_id(1);
  int j = get_global_id(0);
  if (k >= cell_size_k || j >= cell_size_j) return;

  int i, n;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  //---------------------------------------------------------------------
  // zap the whole left hand side for starters
  //---------------------------------------------------------------------
  for (i = 0; i < cell_size_i; i++) {
    for (n = 0; n < 15; n++) {
      lhs[c][k][j][i][n] = 0.0;
    }
  }

  //---------------------------------------------------------------------
  // next, set all diagonal values to 1. This is overkill, but convenient
  //---------------------------------------------------------------------
  for (i = 0; i < cell_size_i; i++) {
    for (n = 1; n <= 3; n++) {
      lhs[c][k][j][i][5*n-3] = 1.0;
    }
  }
}


__kernel void exact_rhs1(__global double *g_forcing,
                         int c,
                         int cell_size_k,
                         int cell_size_j,
                         int cell_size_i)
{
  int k = get_global_id(1);
  int j = get_global_id(0);
  if (k >= cell_size_k || j >= cell_size_j) return;

  int i, m;
  __global double (*forcing)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_forcing;

  //---------------------------------------------------------------------
  // initialize                                  
  //---------------------------------------------------------------------
  for (i = 0; i < cell_size_i; i++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = 0.0;
    }
  }
}

__kernel void exact_rhs2(__global double *g_forcing,
                         __constant double *g_ce,
                         int c,
                         int start_k, int range_k,
                         int start_j, int range_j,
                         int cell_size_i,
                         int start_i, int end_i,
                         int cell_low_k,
                         int cell_low_j,
                         int cell_low_i)
{
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;

  double ue[MAX_CELL_DIM+4][5], buf[MAX_CELL_DIM+4][5];
  double cuf[MAX_CELL_DIM+4], q[MAX_CELL_DIM+4];
  double dtemp[5], xi, eta, zeta, dtpp;
  int m, i, ip1, ip2, ip3;

  __global double (*forcing)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_forcing;

  //---------------------------------------------------------------------
  // xi-direction flux differences                      
  //---------------------------------------------------------------------
  zeta = (double)(k+cell_low_k) * dnzm1;
  eta = (double)(j+cell_low_j) * dnym1;

  for (i=2*start_i; i <= cell_size_i+3-2*end_i; i++) {
    xi = (double)(i+cell_low_i-2) * dnxm1;

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

  for (i = start_i; i < cell_size_i-end_i; i++) {
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
  if (start_i > 0) {
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

  for (i = start_i*3; i < cell_size_i-3*end_i; i++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp*
        (ue[i][m] - 4.0*ue[i+1][m] +
         6.0*ue[i+2][m] - 4.0*ue[i+3][m] + ue[i+4][m]);
    }
  }

  if (end_i > 0) {
    i = cell_size_i-3;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[i][m] - 4.0*ue[i+1][m] + 6.0*ue[i+2][m] - 4.0*ue[i+3][m]);
    }

    i = cell_size_i-2;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[i][m] - 4.0*ue[i+1][m] + 5.0*ue[i+2][m]);
    }
  }
}

__kernel void exact_rhs3(__global double *g_forcing,
                         __constant double *g_ce,
                         int c,
                         int start_k, int range_k,
                         int start_i, int range_i,
                         int cell_size_j,
                         int start_j, int end_j,
                         int cell_low_k,
                         int cell_low_j,
                         int cell_low_i)
{
  int k = get_global_id(1) + start_k;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || i >= range_i) return;

  double ue[MAX_CELL_DIM+4][5], buf[MAX_CELL_DIM+4][5];
  double cuf[MAX_CELL_DIM+4], q[MAX_CELL_DIM+4];
  double dtemp[5], xi, eta, zeta, dtpp;
  int m, j, jp1, jp2, jp3;

  __global double (*forcing)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_forcing;

  //---------------------------------------------------------------------
  // eta-direction flux differences             
  //---------------------------------------------------------------------
  zeta = (double)(k+cell_low_k) * dnzm1;
  xi = (double)(i+cell_low_i) * dnxm1;

  for (j=2*start_j; j <= cell_size_j+3-2*end_j; j++) {
    eta = (double)(j+cell_low_j-2) * dnym1;

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

  for (j = start_j; j < cell_size_j-end_j; j++) {
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
  if (start_j > 0) {
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

  for (j = start_j*3; j < cell_size_j-3*end_j; j++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp*
        (ue[j][m] - 4.0*ue[j+1][m] +
         6.0*ue[j+2][m] - 4.0*ue[j+3][m] + ue[j+4][m]);
    }
  }
  if (end_j > 0) {
    j = cell_size_j-3;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[j][m] - 4.0*ue[j+1][m] + 6.0*ue[j+2][m] - 4.0*ue[j+3][m]);
    }

    j = cell_size_j-2;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[j][m] - 4.0*ue[j+1][m] + 5.0*ue[j+2][m]);

    }
  }
}

__kernel void exact_rhs4(__global double *g_forcing,
                         __constant double *g_ce,
                         int c,
                         int start_j, int range_j,
                         int start_i, int range_i,
                         int cell_size_k,
                         int start_k, int end_k,
                         int cell_low_k,
                         int cell_low_j,
                         int cell_low_i)
{
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (j >= range_j || i >= range_i) return;

  double ue[MAX_CELL_DIM+4][5], buf[MAX_CELL_DIM+4][5];
  double cuf[MAX_CELL_DIM+4], q[MAX_CELL_DIM+4];
  double dtemp[5], xi, eta, zeta, dtpp;
  int m, k, kp1, kp2, kp3;

  __global double (*forcing)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_forcing;

  //---------------------------------------------------------------------
  // zeta-direction flux differences                      
  //---------------------------------------------------------------------
  eta = (double)(j+cell_low_j) * dnym1;
  xi = (double)(i+cell_low_i) * dnxm1;

  for (k=2*start_k; k <= cell_size_k+3-2*end_k; k++) {
    zeta = (double)(k+cell_low_k-2) * dnzm1;

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

  for (k=start_k; k < cell_size_k-end_k; k++) {
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
  if (start_k > 0) {
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

  for (k = start_k*3; k < cell_size_k-3*end_k; k++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp*
        (ue[k][m] - 4.0*ue[k+1][m] +
         6.0*ue[k+2][m] - 4.0*ue[k+3][m] + ue[k+4][m]);
    }
  }

  if (end_k > 0) {
    k = cell_size_k-3;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[k][m] - 4.0*ue[k+1][m] + 6.0*ue[k+2][m] - 4.0*ue[k+3][m]);
    }

    k = cell_size_k-2;
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = forcing[c][k][j][i][m] - dssp *
        (ue[k][m] - 4.0*ue[k+1][m] + 5.0*ue[k+2][m]);
    }
  }
}

__kernel void exact_rhs5(__global double *g_forcing,
                         int c,
                         int start_k, int range_k,
                         int start_j, int range_j,
                         int cell_size_i,
                         int start_i, int end_i)
{
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;

  int i, m;

  __global double (*forcing)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_forcing;

  //---------------------------------------------------------------------
  // now change the sign of the forcing function, 
  //---------------------------------------------------------------------

  for (i = start_i; i < cell_size_i-end_i; i++) {
    for (m = 0; m < 5; m++) {
      forcing[c][k][j][i][m] = -1.0 * forcing[c][k][j][i][m];
    }
  }
}


__kernel void copy_faces1(__global double *g_u,
                          __global double *g_out_buffer,
                          int c,
                          int base1, int base2,
                          int cell_size_i,
                          int cell_size_j,
                          int cell_size_k)
{
#if COPY_FACES1_DIM == 2
  int k = get_global_id(1) + 2;
  int j = get_global_id(0) + 2;
  int i;
  if (k >= cell_size_k+2 || j >= cell_size_j+2) return;
#else // COPY_FACES1_DIM == 1
  int k = get_global_id(0) + 2;
  int j, i;
  if (k >= cell_size_k+2) return;
#endif

  int p1, p2, m;
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double *out_buffer = (__global double *)g_out_buffer;

#if COPY_FACES1_DIM == 2
  //---------------------------------------------------------------------
  // fill the buffer to be sent to eastern neighbors (i-dir)
  //---------------------------------------------------------------------
  if (base1 != -1) {
    p1 = base1 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_j));
    for (i = cell_size_i; i < cell_size_i+2; i++) {
      for (m = 0; m < 5; m++) {
        out_buffer[p1] = u[c][k][j][i][m];
        p1 = p1 + 1;
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to western neighbors 
  //---------------------------------------------------------------------
  if (base2 != -1) {
    p2 = base2 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_j));
    for (i = 2; i <= 3; i++) {
      for (m = 0; m < 5; m++) {
        out_buffer[p2] = u[c][k][j][i][m];
        p2 = p2 + 1;
      }
    }
  }

#else // COPY_FACES1_DIM == 1
  if (base1 != -1)
    p1 = base1 + 2*5*get_global_id(0)*(cell_size_j);
  if (base2 != -1)
    p2 = base2 + 2*5*get_global_id(0)*(cell_size_j);

  for (j = 2; j < cell_size_j+2; j++) {
    //---------------------------------------------------------------------
    // fill the buffer to be sent to eastern neighbors (i-dir)
    //---------------------------------------------------------------------
    if (base1 != -1) {
      for (i = cell_size_i; i < cell_size_i+2; i++) {
        for (m = 0; m < 5; m++) {
          out_buffer[p1] = u[c][k][j][i][m];
          p1 = p1 + 1;
        }
      }
    }

    //---------------------------------------------------------------------
    // fill the buffer to be sent to western neighbors 
    //---------------------------------------------------------------------
    if (base2 != -1) {
      for (i = 2; i <= 3; i++) {
        for (m = 0; m < 5; m++) {
          out_buffer[p2] = u[c][k][j][i][m];
          p2 = p2 + 1;
        }
      }
    }
  }
#endif
}

__kernel void copy_faces2(__global double *g_u,
                          __global double *g_out_buffer,
                          int c,
                          int base1, int base2,
                          int cell_size_i,
                          int cell_size_j,
                          int cell_size_k)
{
#if COPY_FACES2_DIM == 2
  int k = get_global_id(1) + 2;
  int i = get_global_id(0) + 2;
  int j;
  if (k >= cell_size_k+2 || i >= cell_size_i+2) return;
#else // COPY_FACES2_DIM == 1
  int k = get_global_id(0) + 2;
  int j, i;
  if (k >= cell_size_k+2) return;
#endif

  int p1, p2, m;
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double *out_buffer = (__global double *)g_out_buffer;

#if COPY_FACES2_DIM == 2
  //---------------------------------------------------------------------
  // fill the buffer to be sent to northern neighbors (j_dir)
  //---------------------------------------------------------------------
  if (base1 != -1) {
    p1 = base1 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_i));
    for (j = cell_size_j; j < cell_size_j+2; j++) {
      for (m = 0; m < 5; m++) {
        out_buffer[p1] = u[c][k][j][i][m];
        p1 = p1 + 1;
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to southern neighbors 
  //---------------------------------------------------------------------
  if (base2 != -1) {
    p2 = base2 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_i));
    for (j = 2; j <= 3; j++) {
      for (m = 0; m < 5; m++) {
        out_buffer[p2] = u[c][k][j][i][m];
        p2 = p2 + 1;
      }
    }
  }

#else // COPY_FACES2_DIM == 1
  if (base1 != -1)
    p1 = base1 + 2*5*get_global_id(0)*(cell_size_i);
  if (base2 != -1)
    p2 = base2 + 2*5*get_global_id(0)*(cell_size_i);

  for (i = 2; i < cell_size_i+2; i++) {
    //---------------------------------------------------------------------
    // fill the buffer to be sent to northern neighbors (j_dir)
    //---------------------------------------------------------------------
    if (base1 != -1) {
      for (j = cell_size_j; j < cell_size_j+2; j++) {
        for (m = 0; m < 5; m++) {
          out_buffer[p1] = u[c][k][j][i][m];
          p1 = p1 + 1;
        }
      }
    }

    //---------------------------------------------------------------------
    // fill the buffer to be sent to southern neighbors 
    //---------------------------------------------------------------------
    if (base2 != -1) {
      for (j = 2; j <= 3; j++) {
        for (m = 0; m < 5; m++) {
          out_buffer[p2] = u[c][k][j][i][m];
          p2 = p2 + 1;
        }
      }
    }
  }
#endif
}

__kernel void copy_faces3(__global double *g_u,
                          __global double *g_out_buffer,
                          int c,
                          int base1, int base2,
                          int cell_size_i,
                          int cell_size_j,
                          int cell_size_k)
{
#if COPY_FACES3_DIM == 2
  int j = get_global_id(1) + 2;
  int i = get_global_id(0) + 2;
  int k;
  if (j >= cell_size_j+2 || i >= cell_size_i+2) return;
#else // COPY_FACES3_DIM == 1
  int j = get_global_id(0) + 2;
  int k, i;
  if (j >= cell_size_j+2) return;
#endif

  int p1, p2, m;
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double *out_buffer = (__global double *)g_out_buffer;

#if COPY_FACES3_DIM == 2
  //---------------------------------------------------------------------
  // fill the buffer to be sent to top neighbors (k-dir)
  //---------------------------------------------------------------------
  if (base1 != -1) {
    p1 = base1 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_i));
    for (k = cell_size_k; k < cell_size_k+2; k++) {
      for (m = 0; m < 5; m++) {
        out_buffer[p1] = u[c][k][j][i][m];
        p1 = p1 + 1;
      }
    }
  }

  //---------------------------------------------------------------------
  // fill the buffer to be sent to bottom neighbors
  //---------------------------------------------------------------------
  if (base2 != -1) {
    p2 = base2 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_i));
    for (k = 2; k <= 3; k++) {
      for (m = 0; m < 5; m++) {
        out_buffer[p2] = u[c][k][j][i][m];
        p2 = p2 + 1;
      }
    }
  }

#else // COPY_FACES3_DIM == 1
  if (base1 != -1)
    p1 = base1 + 2*5*get_global_id(0)*(cell_size_i);
  if (base2 != -1)
    p2 = base2 + 2*5*get_global_id(0)*(cell_size_i);

  for (i = 2; i < cell_size_i+2; i++) {
  //---------------------------------------------------------------------
  // fill the buffer to be sent to top neighbors (k-dir)
  //---------------------------------------------------------------------
    if (base1 != -1) {
      for (k = cell_size_k; k < cell_size_k+2; k++) {
        for (m = 0; m < 5; m++) {
          out_buffer[p1] = u[c][k][j][i][m];
          p1 = p1 + 1;
        }
      }
    }

    //---------------------------------------------------------------------
    // fill the buffer to be sent to bottom neighbors
    //---------------------------------------------------------------------
    if (base2 != -1) {
      for (k = 2; k <= 3; k++) {
        for (m = 0; m < 5; m++) {
          out_buffer[p2] = u[c][k][j][i][m];
          p2 = p2 + 1;
        }
      }
    }
  }
#endif
}

__kernel void copy_faces4(__global double *g_u,
                          __global double *g_in_buffer,
                          int c,
                          int base1, int base2,
                          int cell_size_i,
                          int cell_size_j,
                          int cell_size_k)
{
#if COPY_FACES4_DIM == 2
  int k = get_global_id(1) + 2;
  int j = get_global_id(0) + 2;
  int i;
  if (k >= cell_size_k+2 || j >= cell_size_j+2) return;
#else // COPY_FACES4_DIM == 1
  int k = get_global_id(0) + 2;
  int j, i;
  if (k >= cell_size_k+2) return;
#endif

  int p1, p2, m;
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double *in_buffer = (__global double *)g_in_buffer;

#if COPY_FACES4_DIM == 2
  if (base1 != -1) {
    p1 = base1 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_j));
    for (i = 0; i <= 1; i++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[p1];
        p1 = p1 + 1;
      }
    }
  }

  if (base2 != -1) {
    p2 = base2 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_j));
    for (i = cell_size_i+2; i <= cell_size_i+3; i++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[p2];
        p2 = p2 + 1;
      }
    }
  }

#else // COPY_FACES4_DIM == 1
  if (base1 != -1)
    p1 = base1 + 2*5*get_global_id(0)*(cell_size_j);
  if (base2 != -1)
    p2 = base2 + 2*5*get_global_id(0)*(cell_size_j);

  for (j = 2; j < cell_size_j+2; j++) {
    if (base1 != -1) {
      for (i = 0; i <= 1; i++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[p1];
          p1 = p1 + 1;
        }
      }
    }

    if (base2 != -1) {
      for (i = cell_size_i+2; i <= cell_size_i+3; i++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[p2];
          p2 = p2 + 1;
        }
      }
    }
  }
#endif
}

__kernel void copy_faces5(__global double *g_u,
                          __global double *g_in_buffer,
                          int c,
                          int base1, int base2,
                          int cell_size_i,
                          int cell_size_j,
                          int cell_size_k)
{
#if COPY_FACES5_DIM == 2
  int k = get_global_id(1) + 2;
  int i = get_global_id(0) + 2;
  int j;
  if (k >= cell_size_k+2 || i >= cell_size_i+2) return;
#else // COPY_FACES5_DIM == 1
  int k = get_global_id(0) + 2;
  int j, i;
  if (k >= cell_size_k+2) return;
#endif

  int p1, p2, m;
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double *in_buffer = (__global double *)g_in_buffer;

#if COPY_FACES5_DIM == 2
  if (base1 != -1) {
    p1 = base1 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_i));
    for (j = 0; j <= 1; j++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[p1];
        p1 = p1 + 1;
      }
    }
  }

  if (base2 != -1) {
    p2 = base2 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_i));
    for (j = cell_size_j+2; j <= cell_size_j+3; j++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[p2];
        p2 = p2 + 1;
      }
    }
  }

#else // COPY_FACES5_DIM == 1
  if (base1 != -1)
    p1 = base1 + 2*5*get_global_id(0)*(cell_size_i);
  if (base2 != -1)
    p2 = base2 + 2*5*get_global_id(0)*(cell_size_i);

  for (i = 2; i < cell_size_i+2; i++) {
    if (base1 != -1) {
      for (j = 0; j <= 1; j++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[p1];
          p1 = p1 + 1;
        }
      }
    }

    if (base2 != -1) {
      for (j = cell_size_j+2; j <= cell_size_j+3; j++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[p2];
          p2 = p2 + 1;
        }
      }
    }
  }
#endif
}

__kernel void copy_faces6(__global double *g_u,
                          __global double *g_in_buffer,
                          int c,
                          int base1, int base2,
                          int cell_size_i,
                          int cell_size_j,
                          int cell_size_k)
{
#if COPY_FACES6_DIM == 2
  int j = get_global_id(1) + 2;
  int i = get_global_id(0) + 2;
  int k;
  if (j >= cell_size_j+2 || i >= cell_size_i+2) return;
#else // COPY_FACES6_DIM == 1
  int j = get_global_id(0) + 2;
  int k, i;
  if (j >= cell_size_j+2) return;
#endif

  int p1, p2, m;
  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double *in_buffer = (__global double *)g_in_buffer;

#if COPY_FACES6_DIM == 2
  if (base1 != -1) {
    p1 = base1 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_i));
    for (k = 0; k <= 1; k++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[p1];
        p1 = p1 + 1;
      }
    }
  }

  if (base2 != -1) {
    p2 = base2 + 2*5*(get_global_id(0)+get_global_id(1)*(cell_size_i));
    for (k = cell_size_k+2; k <= cell_size_k+3; k++) {
      for (m = 0; m < 5; m++) {
        u[c][k][j][i][m] = in_buffer[p2];
        p2 = p2 + 1;
      }
    }
  }

#else // COPY_FACES6_DIM == 1
  if (base1 != -1)
    p1 = base1 + 2*5*get_global_id(0)*(cell_size_i);
  if (base2 != -1)
    p2 = base2 + 2*5*get_global_id(0)*(cell_size_i);

  for (i = 2; i < cell_size_i+2; i++) {
    if (base1 != -1) {
      for (k = 0; k <= 1; k++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[p1];
          p1 = p1 + 1;
        }
      }
    }

    if (base2 != -1) {
      for (k = cell_size_k+2; k <= cell_size_k+3; k++) {
        for (m = 0; m < 5; m++) {
          u[c][k][j][i][m] = in_buffer[p2];
          p2 = p2 + 1;
        }
      }
    }
  }
#endif
}


__kernel void compute_rhs1(__global double* g_u,
                           __global double* g_us,
                           __global double* g_vs,
                           __global double* g_ws,
                           __global double* g_qs,
                           __global double* g_ainv,
                           __global double* g_rho_i,
                           __global double* g_speed,
                           __global double* g_square,
                           int c,
                           int cell_size_k,
                           int cell_size_j,
                           int cell_size_i)
{
#if COMPUTE_RHS1_DIM == 3
  int k = get_global_id(2);
  int j = get_global_id(1);
  int i = get_global_id(0);
  if (k > cell_size_k+1 || j > cell_size_j+1 || i > cell_size_i+1) return;
#elif COMPUTE_RHS1_DIM == 2
  int k = get_global_id(1);
  int j = get_global_id(0);
  int i;
  if (k > cell_size_k+1 || j > cell_size_j+1) return;
#else // COMPUTE_RHS1_DIM == 1
  int k = get_global_id(0);
  int j, i;
  if (k > cell_size_k+1) return;
#endif

  int kp1, jp1, ip1;
  double aux, rho_inv;

  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double (*us)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  __global double (*vs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  __global double (*ws)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  __global double (*ainv)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ainv;
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  __global double (*speed)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_speed;
  __global double (*square)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_square;

  //---------------------------------------------------------------------
  // compute the reciprocal of density, and the kinetic energy, 
  // and the speed of sound. 
  //---------------------------------------------------------------------
#if COMPUTE_RHS1_DIM == 3
  kp1 = k + 1; jp1 = j + 1; ip1 = i + 1;

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
  //----------------------------------------------------------------
  // (don't need speed and ainx until the lhs computation)
  //----------------------------------------------------------------
  aux = c1c2*rho_inv* (u[c][kp1][jp1][ip1][4] - square[c][k][j][i]);
  aux = sqrt(aux);
  speed[c][k][j][i] = aux;
  ainv[c][k][j][i]  = 1.0/aux;

#elif COMPUTE_RHS1_DIM == 2
  kp1 = k + 1; jp1 = j + 1;
  for (i = 0; i <= cell_size_i+1; i++) {
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
    //----------------------------------------------------------------
    // (don't need speed and ainx until the lhs computation)
    //----------------------------------------------------------------
    aux = c1c2*rho_inv* (u[c][kp1][jp1][ip1][4] - square[c][k][j][i]);
    aux = sqrt(aux);
    speed[c][k][j][i] = aux;
    ainv[c][k][j][i]  = 1.0/aux;
  }

#else // COMPUTE_RHS1_DIM == 1
  kp1 = k + 1;
  for (j = 0; j <= cell_size_j+1; j++) {
    jp1 = j + 1;
    for (i = 0; i <= cell_size_i+1; i++) {
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
      //----------------------------------------------------------------
      // (don't need speed and ainx until the lhs computation)
      //----------------------------------------------------------------
      aux = c1c2*rho_inv* (u[c][kp1][jp1][ip1][4] - square[c][k][j][i]);
      aux = sqrt(aux);
      speed[c][k][j][i] = aux;
      ainv[c][k][j][i]  = 1.0/aux;
    }
  }
#endif
}

__kernel void compute_rhs2(__global double* g_rhs,
                           __global double* g_forcing,
                           int c,
                           int cell_size_k,
                           int cell_size_j,
                           int cell_size_i)
{
#if COMPUTE_RHS2_DIM == 3
  int k = get_global_id(2);
  int j = get_global_id(1);
  int i = get_global_id(0);
  if (k >= cell_size_k || j >= cell_size_j || i >= cell_size_i) return;
#elif COMPUTE_RHS2_DIM == 2
  int k = get_global_id(1);
  int j = get_global_id(0);
  int i;
  if (k >= cell_size_k || j >= cell_size_j) return;
#else // COMPUTE_RHS2_DIM == 1
  int k = get_global_id(0);
  int j, i;
  if (k >= cell_size_k) return;
#endif

  int m;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*forcing)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_forcing;

  //---------------------------------------------------------------------
  // copy the exact forcing term to the right hand side;  because 
  // this forcing term is known, we can store it on the whole of every 
  // cell,  including the boundary                   
  //---------------------------------------------------------------------
#if COMPUTE_RHS2_DIM == 3
  for (m = 0; m < 5; m++) {
    rhs[c][k][j][i][m] = forcing[c][k][j][i][m];
  }

#elif COMPUTE_RHS2_DIM == 2
  for (i = 0; i < cell_size_i; i++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = forcing[c][k][j][i][m];
    }
  }

#else // COMPUTE_RHS2_DIM == 1
  for (j = 0; j < cell_size_j; j++) {
    for (i = 0; i < cell_size_i; i++) {
      for (m = 0; m < 5; m++) {
        rhs[c][k][j][i][m] = forcing[c][k][j][i][m];
      }
    }
  }
#endif
}

__kernel void compute_rhs3(__global double* g_u,
                           __global double* g_us,
                           __global double* g_vs,
                           __global double* g_ws,
                           __global double* g_qs,
                           __global double* g_rho_i,
                           __global double* g_square,
                           __global double* g_rhs,
                           int c,
                           int start_k, int range_k,
                           int start_j, int range_j,
                           int cell_size_i,
                           int start_i, int end_i)
{
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;

  int i, m, kp1, kp2, jp1, jp2, ip1, ip2, ip3;
  double uijk, up1, um1;

  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double (*us)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  __global double (*vs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  __global double (*ws)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  __global double (*square)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_square;
  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;

  //---------------------------------------------------------------------
  // compute xi-direction fluxes 
  //---------------------------------------------------------------------
  kp1 = k + 1;
  kp2 = k + 2;
  jp1 = j + 1;
  jp2 = j + 2;
  for (i = start_i; i < cell_size_i-end_i; i++) {
    ip1 = i + 1;
    ip2 = i + 2;
    ip3 = i + 3;

    uijk = us[c][kp1][jp1][ip1];
    up1  = us[c][kp1][jp1][ip2];
    um1  = us[c][kp1][jp1][i];

    rhs[c][k][j][i][0] = rhs[c][k][j][i][0] + dx1tx1 * 
      (u[c][kp2][jp2][ip3][0] - 2.0*u[c][kp2][jp2][ip2][0] + 
       u[c][kp2][jp2][ip1][0]) -
      tx2 * (u[c][kp2][jp2][ip3][1] - u[c][kp2][jp2][ip1][1]);

    rhs[c][k][j][i][1] = rhs[c][k][j][i][1] + dx2tx1 * 
      (u[c][kp2][jp2][ip3][1] - 2.0*u[c][kp2][jp2][ip2][1] + 
       u[c][kp2][jp2][ip1][1]) +
      xxcon2*con43 * (up1 - 2.0*uijk + um1) -
      tx2 * (u[c][kp2][jp2][ip3][1]*up1 - 
          u[c][kp2][jp2][ip1][1]*um1 +
          (u[c][kp2][jp2][ip3][4]- square[c][kp1][jp1][ip2]-
           u[c][kp2][jp2][ip1][4]+ square[c][kp1][jp1][i])*
          c2);

    rhs[c][k][j][i][2] = rhs[c][k][j][i][2] + dx3tx1 * 
      (u[c][kp2][jp2][ip3][2] - 2.0*u[c][kp2][jp2][ip2][2] +
       u[c][kp2][jp2][ip1][2]) +
      xxcon2 * (vs[c][kp1][jp1][ip2] - 2.0*vs[c][kp1][jp1][ip1] +
          vs[c][kp1][jp1][i]) -
      tx2 * (u[c][kp2][jp2][ip3][2]*up1 - 
          u[c][kp2][jp2][ip1][2]*um1);

    rhs[c][k][j][i][3] = rhs[c][k][j][i][3] + dx4tx1 * 
      (u[c][kp2][jp2][ip3][3] - 2.0*u[c][kp2][jp2][ip2][3] +
       u[c][kp2][jp2][ip1][3]) +
      xxcon2 * (ws[c][kp1][jp1][ip2] - 2.0*ws[c][kp1][jp1][ip1] +
          ws[c][kp1][jp1][i]) -
      tx2 * (u[c][kp2][jp2][ip3][3]*up1 - 
          u[c][kp2][jp2][ip1][3]*um1);

    rhs[c][k][j][i][4] = rhs[c][k][j][i][4] + dx5tx1 * 
      (u[c][kp2][jp2][ip3][4] - 2.0*u[c][kp2][jp2][ip2][4] +
       u[c][kp2][jp2][ip1][4]) +
      xxcon3 * (qs[c][kp1][jp1][ip2] - 2.0*qs[c][kp1][jp1][ip1] +
          qs[c][kp1][jp1][i]) +
      xxcon4 * (up1*up1 -       2.0*uijk*uijk + 
          um1*um1) +
      xxcon5 * (u[c][kp2][jp2][ip3][4]*rho_i[c][kp1][jp1][ip2] - 
          2.0*u[c][kp2][jp2][ip2][4]*rho_i[c][kp1][jp1][ip1] +
          u[c][kp2][jp2][ip1][4]*rho_i[c][kp1][jp1][i]) -
      tx2 * ( (c1*u[c][kp2][jp2][ip3][4] - 
            c2*square[c][kp1][jp1][ip2])*up1 -
          (c1*u[c][kp2][jp2][ip1][4] - 
           c2*square[c][kp1][jp1][i])*um1 );
  }

  //---------------------------------------------------------------------
  // add fourth order xi-direction dissipation               
  //---------------------------------------------------------------------
  if (start_i > 0) {
    i = 1;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m]- dssp * 
        ( 5.0*u[c][kp2][jp2][i+2][m] - 4.0*u[c][kp2][jp2][i+3][m] +
          u[c][kp2][jp2][i+4][m]);
    }

    i = 2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (-4.0*u[c][kp2][jp2][i+1][m] + 6.0*u[c][kp2][jp2][i+2][m] -
          4.0*u[c][kp2][jp2][i+3][m] + u[c][kp2][jp2][i+4][m]);
    }
  }

  for (i = 3*start_i; i < cell_size_i-3*end_i; i++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (  u[c][kp2][jp2][i][m] - 4.0*u[c][kp2][jp2][i+1][m] + 
           6.0*u[c][kp2][jp2][i+2][m] - 4.0*u[c][kp2][jp2][i+3][m] + 
           u[c][kp2][jp2][i+4][m] );
    }
  }

  if (end_i > 0) {
    i = cell_size_i-3;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][kp2][jp2][i][m] - 4.0*u[c][kp2][jp2][i+1][m] + 
          6.0*u[c][kp2][jp2][i+2][m] - 4.0*u[c][kp2][jp2][i+3][m] );
    }

    i = cell_size_i-2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][kp2][jp2][i][m] - 4.0*u[c][kp2][jp2][i+1][m] +
          5.0*u[c][kp2][jp2][i+2][m] );
    }
  }
}

__kernel void compute_rhs4(__global double* g_u,
                           __global double* g_us,
                           __global double* g_vs,
                           __global double* g_ws,
                           __global double* g_qs,
                           __global double* g_rho_i,
                           __global double* g_square,
                           __global double* g_rhs,
                           int c,
                           int start_k, int range_k,
                           int start_i, int range_i,
                           int cell_size_j,
                           int start_j, int end_j)
{
  int k = get_global_id(1) + start_k;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || i >= range_i) return;

  int j, m, kp1, kp2, jp1, jp2, jp3, ip1, ip2;
  double vijk, vp1, vm1;

  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double (*us)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  __global double (*vs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  __global double (*ws)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  __global double (*square)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_square;
  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;

  //---------------------------------------------------------------------
  // compute eta-direction fluxes 
  //---------------------------------------------------------------------
  kp1 = k + 1;
  kp2 = k + 2;
  ip1 = i + 1;
  ip2 = i + 2;
  for (j = start_j; j < cell_size_j-end_j; j++) {
    jp1 = j + 1;
    jp2 = j + 2;
    jp3 = j + 3;

    vijk = vs[c][kp1][jp1][ip1];
    vp1  = vs[c][kp1][jp2][ip1];
    vm1  = vs[c][kp1][j][ip1];
    rhs[c][k][j][i][0] = rhs[c][k][j][i][0] + dy1ty1 * 
      (u[c][kp2][jp3][ip2][0] - 2.0*u[c][kp2][jp2][ip2][0] + 
       u[c][kp2][jp1][ip2][0]) -
      ty2 * (u[c][kp2][jp3][ip2][2] - u[c][kp2][jp1][ip2][2]);
    rhs[c][k][j][i][1] = rhs[c][k][j][i][1] + dy2ty1 * 
      (u[c][kp2][jp3][ip2][1] - 2.0*u[c][kp2][jp2][ip2][1] + 
       u[c][kp2][jp1][ip2][1]) +
      yycon2 * (us[c][kp1][jp2][ip1] - 2.0*us[c][kp1][jp1][ip1] + 
          us[c][kp1][j][ip1]) -
      ty2 * (u[c][kp2][jp3][ip2][1]*vp1 - 
          u[c][kp2][jp1][ip2][1]*vm1);
    rhs[c][k][j][i][2] = rhs[c][k][j][i][2] + dy3ty1 * 
      (u[c][kp2][jp3][ip2][2] - 2.0*u[c][kp2][jp2][ip2][2] + 
       u[c][kp2][jp1][ip2][2]) +
      yycon2*con43 * (vp1 - 2.0*vijk + vm1) -
      ty2 * (u[c][kp2][jp3][ip2][2]*vp1 - 
          u[c][kp2][jp1][ip2][2]*vm1 +
          (u[c][kp2][jp3][ip2][4] - square[c][kp1][jp2][ip1] - 
           u[c][kp2][jp1][ip2][4] + square[c][kp1][j][ip1])
          *c2);
    rhs[c][k][j][i][3] = rhs[c][k][j][i][3] + dy4ty1 * 
      (u[c][kp2][jp3][ip2][3] - 2.0*u[c][kp2][jp2][ip2][3] + 
       u[c][kp2][jp1][ip2][3]) +
      yycon2 * (ws[c][kp1][jp2][ip1] - 2.0*ws[c][kp1][jp1][ip1] + 
          ws[c][kp1][j][ip1]) -
      ty2 * (u[c][kp2][jp3][ip2][3]*vp1 - 
          u[c][kp2][jp1][ip2][3]*vm1);
    rhs[c][k][j][i][4] = rhs[c][k][j][i][4] + dy5ty1 * 
      (u[c][kp2][jp3][ip2][4] - 2.0*u[c][kp2][jp2][ip2][4] + 
       u[c][kp2][jp1][ip2][4]) +
      yycon3 * (qs[c][kp1][jp2][ip1] - 2.0*qs[c][kp1][jp1][ip1] + 
          qs[c][kp1][j][ip1]) +
      yycon4 * (vp1*vp1       - 2.0*vijk*vijk + 
          vm1*vm1) +
      yycon5 * (u[c][kp2][jp3][ip2][4]*rho_i[c][kp1][jp2][ip1] - 
          2.0*u[c][kp2][jp2][ip2][4]*rho_i[c][kp1][jp1][ip1] +
          u[c][kp2][jp1][ip2][4]*rho_i[c][kp1][j][ip1]) -
      ty2 * ((c1*u[c][kp2][jp3][ip2][4] - 
            c2*square[c][kp1][jp2][ip1]) * vp1 -
          (c1*u[c][kp2][jp1][ip2][4] - 
           c2*square[c][kp1][j][ip1]) * vm1);
  }

  //---------------------------------------------------------------------
  // add fourth order eta-direction dissipation         
  //---------------------------------------------------------------------
  if (start_j > 0) {
    j = 1;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m]- dssp * 
        ( 5.0*u[c][kp2][j+2][ip2][m] - 4.0*u[c][kp2][j+3][ip2][m] +
          u[c][kp2][j+4][ip2][m]);
    }

    j = 2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (-4.0*u[c][kp2][j+1][ip2][m] + 6.0*u[c][kp2][j+2][ip2][m] -
         4.0*u[c][kp2][j+3][ip2][m] + u[c][kp2][j+4][ip2][m]);
    }
  }

  for (j = 3*start_j; j < cell_size_j-3*end_j; j++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (  u[c][kp2][j][ip2][m] - 4.0*u[c][kp2][j+1][ip2][m] + 
           6.0*u[c][kp2][j+2][ip2][m] - 4.0*u[c][kp2][j+3][ip2][m] + 
           u[c][kp2][j+4][ip2][m] );
    }
  }

  if (end_j > 0) {
    j = cell_size_j-3;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][kp2][j][ip2][m] - 4.0*u[c][kp2][j+1][ip2][m] + 
          6.0*u[c][kp2][j+2][ip2][m] - 4.0*u[c][kp2][j+3][ip2][m] );
    }

    j = cell_size_j-2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][kp2][j][ip2][m] - 4.0*u[c][kp2][j+1][ip2][m] +
          5.0*u[c][kp2][j+2][ip2][m] );
    }
  }
}

__kernel void compute_rhs5(__global double* g_u,
                           __global double* g_us,
                           __global double* g_vs,
                           __global double* g_ws,
                           __global double* g_qs,
                           __global double* g_rho_i,
                           __global double* g_square,
                           __global double* g_rhs,
                           int c,
                           int start_j, int range_j,
                           int start_i, int range_i,
                           int cell_size_k,
                           int start_k, int end_k)
{
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (j >= range_j || i >= range_i) return;

  int k, m, kp1, kp2, kp3, jp1, jp2, ip1, ip2;
  double wijk, wp1, wm1;

  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double (*us)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  __global double (*vs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  __global double (*ws)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  __global double (*square)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_square;
  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;

  //---------------------------------------------------------------------
  // compute zeta-direction fluxes 
  //---------------------------------------------------------------------
  jp1 = j + 1;
  jp2 = j + 2;
  ip1 = i + 1;
  ip2 = i + 2;
  for (k = start_k; k < cell_size_k-end_k; k++) {
    kp1 = k + 1;
    kp2 = k + 2;
    kp3 = k + 3;

    wijk = ws[c][kp1][jp1][ip1];
    wp1  = ws[c][kp2][jp1][ip1];
    wm1  = ws[c][k][jp1][ip1];

    rhs[c][k][j][i][0] = rhs[c][k][j][i][0] + dz1tz1 * 
      (u[c][kp3][jp2][ip2][0] - 2.0*u[c][kp2][jp2][ip2][0] + 
       u[c][kp1][jp2][ip2][0]) -
      tz2 * (u[c][kp3][jp2][ip2][3] - u[c][kp1][jp2][ip2][3]);
    rhs[c][k][j][i][1] = rhs[c][k][j][i][1] + dz2tz1 * 
      (u[c][kp3][jp2][ip2][1] - 2.0*u[c][kp2][jp2][ip2][1] + 
       u[c][kp1][jp2][ip2][1]) +
      zzcon2 * (us[c][kp2][jp1][ip1] - 2.0*us[c][kp1][jp1][ip1] + 
          us[c][k][jp1][ip1]) -
      tz2 * (u[c][kp3][jp2][ip2][1]*wp1 - 
          u[c][kp1][jp2][ip2][1]*wm1);
    rhs[c][k][j][i][2] = rhs[c][k][j][i][2] + dz3tz1 * 
      (u[c][kp3][jp2][ip2][2] - 2.0*u[c][kp2][jp2][ip2][2] + 
       u[c][kp1][jp2][ip2][2]) +
      zzcon2 * (vs[c][kp2][jp1][ip1] - 2.0*vs[c][kp1][jp1][ip1] + 
          vs[c][k][jp1][ip1]) -
      tz2 * (u[c][kp3][jp2][ip2][2]*wp1 - 
          u[c][kp1][jp2][ip2][2]*wm1);
    rhs[c][k][j][i][3] = rhs[c][k][j][i][3] + dz4tz1 * 
      (u[c][kp3][jp2][ip2][3] - 2.0*u[c][kp2][jp2][ip2][3] + 
       u[c][kp1][jp2][ip2][3]) +
      zzcon2*con43 * (wp1 - 2.0*wijk + wm1) -
      tz2 * (u[c][kp3][jp2][ip2][3]*wp1 - 
          u[c][kp1][jp2][ip2][3]*wm1 +
          (u[c][kp3][jp2][ip2][4] - square[c][kp2][jp1][ip1] - 
           u[c][kp1][jp2][ip2][4] + square[c][k][jp1][ip1])
          *c2);
    rhs[c][k][j][i][4] = rhs[c][k][j][i][4] + dz5tz1 * 
      (u[c][kp3][jp2][ip2][4] - 2.0*u[c][kp2][jp2][ip2][4] + 
       u[c][kp1][jp2][ip2][4]) +
      zzcon3 * (qs[c][kp2][jp1][ip1] - 2.0*qs[c][kp1][jp1][ip1] + 
          qs[c][k][jp1][ip1]) +
      zzcon4 * (wp1*wp1 - 2.0*wijk*wijk + 
          wm1*wm1) +
      zzcon5 * (u[c][kp3][jp2][ip2][4]*rho_i[c][kp2][jp1][ip1] - 
          2.0*u[c][kp2][jp2][ip2][4]*rho_i[c][kp1][jp1][ip1] +
          u[c][kp1][jp2][ip2][4]*rho_i[c][k][jp1][ip1]) -
      tz2 * ( (c1*u[c][kp3][jp2][ip2][4] - 
            c2*square[c][kp2][jp1][ip1])*wp1 -
          (c1*u[c][kp1][jp2][ip2][4] - 
           c2*square[c][k][jp1][ip1])*wm1);
  }

  //---------------------------------------------------------------------
  // add fourth order zeta-direction dissipation                
  //---------------------------------------------------------------------
  if (start_k > 0) {
    k = 1;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m]- dssp * 
        ( 5.0*u[c][k+2][jp2][ip2][m] - 4.0*u[c][k+3][jp2][ip2][m] +
          u[c][k+4][jp2][ip2][m]);
    }

    k = 2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (-4.0*u[c][k+1][jp2][ip2][m] + 6.0*u[c][k+2][jp2][ip2][m] -
          4.0*u[c][k+3][jp2][ip2][m] +     u[c][k+4][jp2][ip2][m]);
    }
  }

  for (k = 3*start_k; k < cell_size_k-3*end_k; k++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp * 
        (  u[c][k][jp2][ip2][m] - 4.0*u[c][k+1][jp2][ip2][m] + 
           6.0*u[c][k+2][jp2][ip2][m] - 4.0*u[c][k+3][jp2][ip2][m] + 
           u[c][k+4][jp2][ip2][m] );
    }
  }

  if (end_k > 0) {
    k = cell_size_k-3;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][k][jp2][ip2][m] - 4.0*u[c][k+1][jp2][ip2][m] + 
          6.0*u[c][k+2][jp2][ip2][m] - 4.0*u[c][k+3][jp2][ip2][m] );
    }

    k = cell_size_k-2;
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - dssp *
        ( u[c][k][jp2][ip2][m] - 4.0*u[c][k+1][jp2][ip2][m] +
          5.0*u[c][k+2][jp2][ip2][m] );
    }
  }
}

__kernel void compute_rhs6(__global double* g_rhs,
                           int c,
                           int start_k, int range_k,
                           int start_j, int range_j,
                           int start_i, int range_i)
{
#if COMPUTE_RHS6_DIM == 3
  int k = get_global_id(2) + start_k;
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || j >= range_j || i >= range_i) return;
#elif COMPUTE_RHS6_DIM == 2
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  int i;
  if (k >= range_k || j >= range_j) return;
#else // COMPUTE_RHS6_DIM == 1
  int k = get_global_id(0) + start_k;
  int j, i;
  if (k >= range_k) return;
#endif

  int m;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;

#if COMPUTE_RHS6_DIM == 3
  for (m = 0; m < 5; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] * dt;
  }

#elif COMPUTE_RHS6_DIM == 2
  for (i = start_i; i < range_i; i++) {
    for (m = 0; m < 5; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] * dt;
    }
  }

#else // COMPUTE_RHS6_DIM == 1
  for (j = start_j; j < range_j; j++) {
    for (i = start_i; i < range_i; i++) {
      for (m = 0; m < 5; m++) {
        rhs[c][k][j][i][m] = rhs[c][k][j][i][m] * dt;
      }
    }
  }
#endif
}


__kernel void txinvr(__global double* g_us,
                     __global double* g_vs,
                     __global double* g_ws,
                     __global double* g_qs,
                     __global double* g_ainv,
                     __global double* g_rho_i,
                     __global double* g_speed,
                     __global double* g_rhs,
                     int c,
                     int start_k, int range_k,
                     int start_j, int range_j,
                     int start_i, int range_i)
{
#if TXINVR_DIM == 3
  int k = get_global_id(2) + start_k;
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || j >= range_j || i >= range_i) return;
#elif TXINVR_DIM == 2
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  int i;
  if (k >= range_k || j >= range_j) return;
#else // TXINVR_DIM == 1
  int k = get_global_id(0) + start_k;
  int j, i;
  if (k >= range_k) return;
#endif

  int kp1, jp1, ip1;
  double t1, t2, t3, ac, ru1, uu, vv, ww, r1, r2, r3, r4, r5, ac2inv;

  __global double (*us)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  __global double (*vs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  __global double (*ws)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  __global double (*ainv)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ainv;
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  __global double (*speed)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_speed;
  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;

#if TXINVR_DIM == 3
  kp1 = k + 1;
  jp1 = j + 1;
  ip1 = i + 1;

  ru1 = rho_i[c][kp1][jp1][ip1];
  uu = us[c][kp1][jp1][ip1];
  vv = vs[c][kp1][jp1][ip1];
  ww = ws[c][kp1][jp1][ip1];
  ac = speed[c][kp1][jp1][ip1];
  ac2inv = ainv[c][kp1][jp1][ip1]*ainv[c][kp1][jp1][ip1];

  r1 = rhs[c][k][j][i][0];
  r2 = rhs[c][k][j][i][1];
  r3 = rhs[c][k][j][i][2];
  r4 = rhs[c][k][j][i][3];
  r5 = rhs[c][k][j][i][4];

  t1 = c2 * ac2inv * ( qs[c][kp1][jp1][ip1]*r1 - uu*r2  - 
      vv*r3 - ww*r4 + r5 );
  t2 = bt * ru1 * ( uu * r1 - r2 );
  t3 = ( bt * ru1 * ac ) * t1;

  rhs[c][k][j][i][0] = r1 - t1;
  rhs[c][k][j][i][1] = - ru1 * ( ww*r1 - r4 );
  rhs[c][k][j][i][2] =   ru1 * ( vv*r1 - r3 );
  rhs[c][k][j][i][3] = - t2 + t3;
  rhs[c][k][j][i][4] =   t2 + t3;

#elif TXINVR_DIM == 2
  kp1 = k + 1;
  jp1 = j + 1;
  for (i = start_i; i < range_i; i++) {
    ip1 = i + 1;

    ru1 = rho_i[c][kp1][jp1][ip1];
    uu = us[c][kp1][jp1][ip1];
    vv = vs[c][kp1][jp1][ip1];
    ww = ws[c][kp1][jp1][ip1];
    ac = speed[c][kp1][jp1][ip1];
    ac2inv = ainv[c][kp1][jp1][ip1]*ainv[c][kp1][jp1][ip1];

    r1 = rhs[c][k][j][i][0];
    r2 = rhs[c][k][j][i][1];
    r3 = rhs[c][k][j][i][2];
    r4 = rhs[c][k][j][i][3];
    r5 = rhs[c][k][j][i][4];

    t1 = c2 * ac2inv * ( qs[c][kp1][jp1][ip1]*r1 - uu*r2  - 
        vv*r3 - ww*r4 + r5 );
    t2 = bt * ru1 * ( uu * r1 - r2 );
    t3 = ( bt * ru1 * ac ) * t1;

    rhs[c][k][j][i][0] = r1 - t1;
    rhs[c][k][j][i][1] = - ru1 * ( ww*r1 - r4 );
    rhs[c][k][j][i][2] =   ru1 * ( vv*r1 - r3 );
    rhs[c][k][j][i][3] = - t2 + t3;
    rhs[c][k][j][i][4] =   t2 + t3;
  }

#else // TXINVR_DIM == 1
  kp1 = k + 1;
  for (j = start_j; j < range_j; j++) {
    jp1 = j + 1;
    for (i = start_i; i < range_i; i++) {
      ip1 = i + 1;

      ru1 = rho_i[c][kp1][jp1][ip1];
      uu = us[c][kp1][jp1][ip1];
      vv = vs[c][kp1][jp1][ip1];
      ww = ws[c][kp1][jp1][ip1];
      ac = speed[c][kp1][jp1][ip1];
      ac2inv = ainv[c][kp1][jp1][ip1]*ainv[c][kp1][jp1][ip1];

      r1 = rhs[c][k][j][i][0];
      r2 = rhs[c][k][j][i][1];
      r3 = rhs[c][k][j][i][2];
      r4 = rhs[c][k][j][i][3];
      r5 = rhs[c][k][j][i][4];

      t1 = c2 * ac2inv * ( qs[c][kp1][jp1][ip1]*r1 - uu*r2  - 
          vv*r3 - ww*r4 + r5 );
      t2 = bt * ru1 * ( uu * r1 - r2 );
      t3 = ( bt * ru1 * ac ) * t1;

      rhs[c][k][j][i][0] = r1 - t1;
      rhs[c][k][j][i][1] = - ru1 * ( ww*r1 - r4 );
      rhs[c][k][j][i][2] =   ru1 * ( vv*r1 - r3 );
      rhs[c][k][j][i][3] = - t2 + t3;
      rhs[c][k][j][i][4] =   t2 + t3;
    }
  }
#endif
}


__kernel void lhsx(__global double* g_us,
                   __global double* g_rho_i,
                   __global double *g_speed,
                   __global double *g_lhs,
                   int c,
                   int start_k, int range_k,
                   int start_j, int range_j,
                   int cell_size_i,
                   int start_i, int end_i)
{
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;

  double cv[MAX_CELL_DIM+4], rhon[MAX_CELL_DIM+4];
  double ru1;
  int i, ip1, jp1, kp1;

  __global double (*us)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  __global double (*speed)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_speed;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  //---------------------------------------------------------------------
  // first fill the lhs for the u-eigenvalue                   
  //---------------------------------------------------------------------
  kp1 = k + 1;
  jp1 = j + 1;

  for (i = start_i-1; i <= cell_size_i-end_i; i++) {
    ip1 = i + 1;
    ru1 = c3c4*rho_i[c][kp1][jp1][ip1];
    cv[i+2] = us[c][kp1][jp1][ip1];
    rhon[i+2] = max(dx2+con43*ru1, max(dx5+c1c5*ru1, max(dxmax+ru1,dx1)));
  }

  for (i = start_i; i < cell_size_i-end_i; i++) {
    lhs[c][k][j][i][0] =   0.0;
    lhs[c][k][j][i][1] = - dttx2 * cv[i+1] - dttx1 * rhon[i+1];
    lhs[c][k][j][i][2] =   1.0 + c2dttx1 * rhon[i+2];
    lhs[c][k][j][i][3] =   dttx2 * cv[i+3] - dttx1 * rhon[i+3];
    lhs[c][k][j][i][4] =   0.0;
  }

  //---------------------------------------------------------------------
  // add fourth order dissipation                             
  //---------------------------------------------------------------------
  if (start_i > 0) {
    i = 1;
    lhs[c][k][j][i][2] = lhs[c][k][j][i][2] + comz5;
    lhs[c][k][j][i][3] = lhs[c][k][j][i][3] - comz4;
    lhs[c][k][j][i][4] = lhs[c][k][j][i][4] + comz1;

    lhs[c][k][j][i+1][1] = lhs[c][k][j][i+1][1] - comz4;
    lhs[c][k][j][i+1][2] = lhs[c][k][j][i+1][2] + comz6;
    lhs[c][k][j][i+1][3] = lhs[c][k][j][i+1][3] - comz4;
    lhs[c][k][j][i+1][4] = lhs[c][k][j][i+1][4] + comz1;
  }

  for (i=3*start_i; i < cell_size_i-3*end_i; i++) {
    lhs[c][k][j][i][0] = lhs[c][k][j][i][0] + comz1;
    lhs[c][k][j][i][1] = lhs[c][k][j][i][1] - comz4;
    lhs[c][k][j][i][2] = lhs[c][k][j][i][2] + comz6;
    lhs[c][k][j][i][3] = lhs[c][k][j][i][3] - comz4;
    lhs[c][k][j][i][4] = lhs[c][k][j][i][4] + comz1;
  }

  if (end_i > 0) {
    i = cell_size_i-3;
    lhs[c][k][j][i][0] = lhs[c][k][j][i][0] + comz1;
    lhs[c][k][j][i][1] = lhs[c][k][j][i][1] - comz4;
    lhs[c][k][j][i][2] = lhs[c][k][j][i][2] + comz6;
    lhs[c][k][j][i][3] = lhs[c][k][j][i][3] - comz4;

    lhs[c][k][j][i+1][0] = lhs[c][k][j][i+1][0] + comz1;
    lhs[c][k][j][i+1][1] = lhs[c][k][j][i+1][1] - comz4;
    lhs[c][k][j][i+1][2] = lhs[c][k][j][i+1][2] + comz5;
  }

  //---------------------------------------------------------------------
  // subsequently, fill the other factors (u+c), (u-c) by a4ing to 
  // the first  
  //---------------------------------------------------------------------
  for (i = start_i; i < cell_size_i-end_i; i++) {
    lhs[c][k][j][i][0+5]  = lhs[c][k][j][i][0];
    lhs[c][k][j][i][1+5]  = lhs[c][k][j][i][1] - 
                            dttx2 * speed[c][kp1][jp1][i];
    lhs[c][k][j][i][2+5]  = lhs[c][k][j][i][2];
    lhs[c][k][j][i][3+5]  = lhs[c][k][j][i][3] + 
                            dttx2 * speed[c][kp1][jp1][i+2];
    lhs[c][k][j][i][4+5]  = lhs[c][k][j][i][4];
    lhs[c][k][j][i][0+10] = lhs[c][k][j][i][0];
    lhs[c][k][j][i][1+10] = lhs[c][k][j][i][1] + 
                            dttx2 * speed[c][kp1][jp1][i];
    lhs[c][k][j][i][2+10] = lhs[c][k][j][i][2];
    lhs[c][k][j][i][3+10] = lhs[c][k][j][i][3] - 
                            dttx2 * speed[c][kp1][jp1][i+2];
    lhs[c][k][j][i][4+10] = lhs[c][k][j][i][4];
  }
}

__kernel void ninvr(__global double* g_rhs,
                    int c,
                    int start_k, int range_k,
                    int start_j, int range_j,
                    int start_i, int range_i)
{
#if NINVR_DIM == 3
  int k = get_global_id(2) + start_k;
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || j >= range_j || i >= range_i) return;
#elif NINVR_DIM == 2
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  int i;
  if (k >= range_k || j >= range_j) return;
#else // NINVR_DIM == 1
  int k = get_global_id(0) + start_k;
  int j, i;
  if (k >= range_k) return;
#endif

  double r1, r2, r3, r4, r5, t1, t2;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;

#if NINVR_DIM == 3
  r1 = rhs[c][k][j][i][0];
  r2 = rhs[c][k][j][i][1];
  r3 = rhs[c][k][j][i][2];
  r4 = rhs[c][k][j][i][3];
  r5 = rhs[c][k][j][i][4];

  t1 = bt * r3;
  t2 = 0.5 * ( r4 + r5 );

  rhs[c][k][j][i][0] = -r2;
  rhs[c][k][j][i][1] =  r1;
  rhs[c][k][j][i][2] = bt * ( r4 - r5 );
  rhs[c][k][j][i][3] = -t1 + t2;
  rhs[c][k][j][i][4] =  t1 + t2;

#elif NINVR_DIM == 2
  for (i = start_i; i < range_i; i++) {

    r1 = rhs[c][k][j][i][0];
    r2 = rhs[c][k][j][i][1];
    r3 = rhs[c][k][j][i][2];
    r4 = rhs[c][k][j][i][3];
    r5 = rhs[c][k][j][i][4];

    t1 = bt * r3;
    t2 = 0.5 * ( r4 + r5 );

    rhs[c][k][j][i][0] = -r2;
    rhs[c][k][j][i][1] =  r1;
    rhs[c][k][j][i][2] = bt * ( r4 - r5 );
    rhs[c][k][j][i][3] = -t1 + t2;
    rhs[c][k][j][i][4] =  t1 + t2;
  }    

#else // NINVR_DIM == 1
  for (j = start_j; j < range_j; j++) {
    for (i = start_i; i < range_i; i++) {

      r1 = rhs[c][k][j][i][0];
      r2 = rhs[c][k][j][i][1];
      r3 = rhs[c][k][j][i][2];
      r4 = rhs[c][k][j][i][3];
      r5 = rhs[c][k][j][i][4];

      t1 = bt * r3;
      t2 = 0.5 * ( r4 + r5 );

      rhs[c][k][j][i][0] = -r2;
      rhs[c][k][j][i][1] =  r1;
      rhs[c][k][j][i][2] = bt * ( r4 - r5 );
      rhs[c][k][j][i][3] = -t1 + t2;
      rhs[c][k][j][i][4] =  t1 + t2;
    }    
  }
#endif
}

__kernel void x_solve1(__global double* g_rhs,
                       __global double* g_lhs,
                       __global double* g_in_buffer,
                       int c,
                       int start_k, int range_k,
                       int start_j, int range_j)
{
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;

  int p1, p2;
  int i, m, n, i1;
  double r1, r2, d, e, s[5];

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;
  __global double *in_buffer = (__global double *)g_in_buffer;

  p1 = 10*(get_global_id(0)+get_global_id(1)*(range_j-start_j)) - 1;
  p2 = 10*(range_j-start_j)*(range_k-start_k) + 6*2*(get_global_id(0)+get_global_id(1)*(range_j-start_j)) - 1;

  //---------------------------------------------------------------------
  // unpack the buffer                                 
  //---------------------------------------------------------------------
  i = 0;
  i1 = 1;
  n = -1;

  //---------------------------------------------------------------------
  // create a running pointer
  //---------------------------------------------------------------------
  lhs[c][k][j][i][n+2] = lhs[c][k][j][i][n+2] -
    in_buffer[p1+1] * lhs[c][k][j][i][n+1];
  lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] -
    in_buffer[p1+2] * lhs[c][k][j][i][n+1];
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] -
      in_buffer[p1+3+m] * lhs[c][k][j][i][n+1];
  }
  d = in_buffer[p1+6];
  e = in_buffer[p1+7];
  for (m = 0; m < 3; m++) {
    s[m] = in_buffer[p1+8+m];
  }
  r1 = lhs[c][k][j][i][n+2];
  lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] - d * r1;
  lhs[c][k][j][i][n+4] = lhs[c][k][j][i][n+4] - e * r1;
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - s[m] * r1;
  }
  r2 = lhs[c][k][j][i1][n+1];
  lhs[c][k][j][i1][n+2] = lhs[c][k][j][i1][n+2] - d * r2;
  lhs[c][k][j][i1][n+3] = lhs[c][k][j][i1][n+3] - e * r2;
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i1][m] = rhs[c][k][j][i1][m] - s[m] * r2;
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    lhs[c][k][j][i][n+2] = lhs[c][k][j][i][n+2] -
      in_buffer[p2+1] * lhs[c][k][j][i][n+1];
    lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] -
      in_buffer[p2+2] * lhs[c][k][j][i][n+1];
    rhs[c][k][j][i][m]   = rhs[c][k][j][i][m] -
      in_buffer[p2+3] * lhs[c][k][j][i][n+1];
    d    = in_buffer[p2+4];
    e    = in_buffer[p2+5];
    s[m] = in_buffer[p2+6];
    r1   = lhs[c][k][j][i][n+2];
    lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] - d * r1;
    lhs[c][k][j][i][n+4] = lhs[c][k][j][i][n+4] - e * r1;
    rhs[c][k][j][i][m]   = rhs[c][k][j][i][m] - s[m] * r1;
    r2 = lhs[c][k][j][i1][n+1];
    lhs[c][k][j][i1][n+2] = lhs[c][k][j][i1][n+2] - d * r2;
    lhs[c][k][j][i1][n+3] = lhs[c][k][j][i1][n+3] - e * r2;
    rhs[c][k][j][i1][m]   = rhs[c][k][j][i1][m] - s[m] * r2;
    p2 = p2 + 6;
  }
}

__kernel void x_solve2(__global double* g_rhs,
                       __global double* g_lhs,
                       int c,
                       int start_k, int range_k,
                       int start_j, int range_j,
                       int cell_size_i)
{
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;

  int i, m, n, i1, i2, istart, iend;
  double fac1, fac2;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  istart = 0;
  iend   = cell_size_i-1;

  //---------------------------------------------------------------------
  // perform the Thomas algorithm; first, FORWARD ELIMINATION     
  //---------------------------------------------------------------------
  n = -1;

  for (i = istart; i <= iend-2; i++) {
    i1 = i  + 1;
    i2 = i  + 2;
    fac1                 = 1.0/lhs[c][k][j][i][n+3];
    lhs[c][k][j][i][n+4] = fac1*lhs[c][k][j][i][n+4];
    lhs[c][k][j][i][n+5] = fac1*lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i][m] = fac1*rhs[c][k][j][i][m];
    }
    lhs[c][k][j][i1][n+3] = lhs[c][k][j][i1][n+3] -
      lhs[c][k][j][i1][n+2]*lhs[c][k][j][i][n+4];
    lhs[c][k][j][i1][n+4] = lhs[c][k][j][i1][n+4] -
      lhs[c][k][j][i1][n+2]*lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i1][m] = rhs[c][k][j][i1][m] -
        lhs[c][k][j][i1][n+2]*rhs[c][k][j][i][m];
    }
    lhs[c][k][j][i2][n+2] = lhs[c][k][j][i2][n+2] -
      lhs[c][k][j][i2][n+1]*lhs[c][k][j][i][n+4];
    lhs[c][k][j][i2][n+3] = lhs[c][k][j][i2][n+3] -
      lhs[c][k][j][i2][n+1]*lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i2][m] = rhs[c][k][j][i2][m] -
        lhs[c][k][j][i2][n+1]*rhs[c][k][j][i][m];
    }
  }

  //---------------------------------------------------------------------
  // The last two rows in this grid block are a bit different, 
  // since they do not have two more rows available for the
  // elimination of off-diagonal entries
  //---------------------------------------------------------------------

  i  = iend - 1;
  i1 = iend;
  fac1                 = 1.0/lhs[c][k][j][i][n+3];
  lhs[c][k][j][i][n+4] = fac1*lhs[c][k][j][i][n+4];
  lhs[c][k][j][i][n+5] = fac1*lhs[c][k][j][i][n+5];
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = fac1*rhs[c][k][j][i][m];
  }
  lhs[c][k][j][i1][n+3] = lhs[c][k][j][i1][n+3] -
    lhs[c][k][j][i1][n+2]*lhs[c][k][j][i][n+4];
  lhs[c][k][j][i1][n+4] = lhs[c][k][j][i1][n+4] -
    lhs[c][k][j][i1][n+2]*lhs[c][k][j][i][n+5];
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i1][m] = rhs[c][k][j][i1][m] -
      lhs[c][k][j][i1][n+2]*rhs[c][k][j][i][m];
  }
  //-------------------------------------------------------------------
  // scale the last row immediately (some of this is
  // overkill in case this is the last cell)
  //-------------------------------------------------------------------
  fac2                  = 1.0/lhs[c][k][j][i1][n+3];
  lhs[c][k][j][i1][n+4] = fac2*lhs[c][k][j][i1][n+4];
  lhs[c][k][j][i1][n+5] = fac2*lhs[c][k][j][i1][n+5]; 
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i1][m] = fac2*rhs[c][k][j][i1][m];
  }

  //---------------------------------------------------------------------
  // do the u+c and the u-c factors                 
  //---------------------------------------------------------------------

  for (i = istart; i <= iend-2; i++) {
    i1 = i  + 1;
    i2 = i  + 2;
    for (m = 3; m < 5; m++) {
      n = (m-2)*5 - 1;

      fac1                  = 1.0/lhs[c][k][j][i][n+3];
      lhs[c][k][j][i][n+4]  = fac1*lhs[c][k][j][i][n+4];
      lhs[c][k][j][i][n+5]  = fac1*lhs[c][k][j][i][n+5];
      rhs[c][k][j][i][m]    = fac1*rhs[c][k][j][i][m];
      lhs[c][k][j][i1][n+3] = lhs[c][k][j][i1][n+3] -
        lhs[c][k][j][i1][n+2]*lhs[c][k][j][i][n+4];
      lhs[c][k][j][i1][n+4] = lhs[c][k][j][i1][n+4] -
        lhs[c][k][j][i1][n+2]*lhs[c][k][j][i][n+5];
      rhs[c][k][j][i1][m]   = rhs[c][k][j][i1][m] -
        lhs[c][k][j][i1][n+2]*rhs[c][k][j][i][m];
      lhs[c][k][j][i2][n+2] = lhs[c][k][j][i2][n+2] -
        lhs[c][k][j][i2][n+1]*lhs[c][k][j][i][n+4];
      lhs[c][k][j][i2][n+3] = lhs[c][k][j][i2][n+3] -
        lhs[c][k][j][i2][n+1]*lhs[c][k][j][i][n+5];
      rhs[c][k][j][i2][m]   = rhs[c][k][j][i2][m] -
        lhs[c][k][j][i2][n+1]*rhs[c][k][j][i][m];
    }
  }

  //-------------------------------------------------------------------
  // And again the last two rows separately
  //-------------------------------------------------------------------
  i  = iend - 1;
  i1 = iend;
  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    fac1                  = 1.0/lhs[c][k][j][i][n+3];
    lhs[c][k][j][i][n+4]  = fac1*lhs[c][k][j][i][n+4];
    lhs[c][k][j][i][n+5]  = fac1*lhs[c][k][j][i][n+5];
    rhs[c][k][j][i][m]    = fac1*rhs[c][k][j][i][m];
    lhs[c][k][j][i1][n+3] = lhs[c][k][j][i1][n+3] -
      lhs[c][k][j][i1][n+2]*lhs[c][k][j][i][n+4];
    lhs[c][k][j][i1][n+4] = lhs[c][k][j][i1][n+4] -
      lhs[c][k][j][i1][n+2]*lhs[c][k][j][i][n+5];
    rhs[c][k][j][i1][m]   = rhs[c][k][j][i1][m] -
      lhs[c][k][j][i1][n+2]*rhs[c][k][j][i][m];
    //-----------------------------------------------------------------
    // Scale the last row immediately (some of this is overkill
    // if this is the last cell)
    //-----------------------------------------------------------------
    fac2                  = 1.0/lhs[c][k][j][i1][n+3];
    lhs[c][k][j][i1][n+4] = fac2*lhs[c][k][j][i1][n+4];
    lhs[c][k][j][i1][n+5] = fac2*lhs[c][k][j][i1][n+5];
    rhs[c][k][j][i1][m]   = fac2*rhs[c][k][j][i1][m];
  }
}

__kernel void x_solve3(__global double* g_rhs,
                       __global double* g_lhs,
                       __global double* g_out_buffer,
                       int c,
                       int start_k, int range_k,
                       int start_j, int range_j,
                       int cell_size_i)
{
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;

  int p1, p2;
  int i, m, n, iend;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;
  __global double *out_buffer = (__global double *)g_out_buffer;

  p1 = 5*2*(get_global_id(0)+get_global_id(1)*(range_j-start_j)) - 1;
  p2 = 5*2*(range_j-start_j)*(range_k-start_k) + 3*2*2*(get_global_id(0)+get_global_id(1)*(range_j-start_j)) - 1;

  iend   = cell_size_i-1;
  //---------------------------------------------------------------------
  // create a running pointer for the send buffer  
  //---------------------------------------------------------------------
  n = -1;
  for (i = iend-1; i <= iend; i++) {
    out_buffer[p1+1] = lhs[c][k][j][i][n+4];
    out_buffer[p1+2] = lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      out_buffer[p1+3+m] = rhs[c][k][j][i][m];
    }
    p1 = p1+5;
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    for (i = iend-1; i <= iend; i++) {
      out_buffer[p2+1] = lhs[c][k][j][i][n+4];
      out_buffer[p2+2] = lhs[c][k][j][i][n+5];
      out_buffer[p2+3] = rhs[c][k][j][i][m];
      p2 = p2 + 3;
    }
  }
}

__kernel void x_solve4(__global double* g_rhs,
                       __global double* g_lhs,
                       __global double* g_in_buffer,
                       int c,
                       int start_k, int range_k,
                       int start_j, int range_j,
                       int cell_size_i)
{
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;

  int p;
  int i, m, n, i1, i2, istart, iend;
  double sm1, sm2;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;
  __global double *in_buffer = (__global double *)g_in_buffer;

  istart = 0;
  iend   = cell_size_i-1;

  //---------------------------------------------------------------------
  // unpack the buffer
  //---------------------------------------------------------------------
  p = 2*5*(get_global_id(0)+get_global_id(1)*(range_j-start_j)) - 1;
  i  = iend;
  i1 = i - 1;
  for (m = 0; m < 3; m++) {
    sm1 = in_buffer[p+1];
    sm2 = in_buffer[p+2];
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
      lhs[c][k][j][i][3]*sm1 -
      lhs[c][k][j][i][4]*sm2;
    rhs[c][k][j][i1][m] = rhs[c][k][j][i1][m] -
      lhs[c][k][j][i1][3] * rhs[c][k][j][i][m] - 
      lhs[c][k][j][i1][4] * sm1;
    p = p + 2;
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    sm1 = in_buffer[p+1];
    sm2 = in_buffer[p+2];
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
      lhs[c][k][j][i][n+4]*sm1 -
      lhs[c][k][j][i][n+5]*sm2;
    rhs[c][k][j][i1][m] = rhs[c][k][j][i1][m] -
      lhs[c][k][j][i1][n+4] * rhs[c][k][j][i][m] - 
      lhs[c][k][j][i1][n+5] * sm1;
    p = p + 2;
  }

  //---------------------------------------------------------------------
  // Whether or not this is the last processor, we always have
  // to complete the back-substitution 
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // The first three factors
  //---------------------------------------------------------------------
  n = -1;
  for (i = iend-2; i >= istart; i--) {
    i1 = i  + 1;
    i2 = i  + 2;
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k][j][i1][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k][j][i2][m];
    }
  }

  //---------------------------------------------------------------------
  // And the remaining two
  //---------------------------------------------------------------------
  for (i = iend-2; i >= istart; i--) {
    i1 = i  + 1;
    i2 = i  + 2;
    for (m = 3; m < 5; m++) {
      n = (m-2)*5 - 1;
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k][j][i1][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k][j][i2][m];
    }
  }

}

__kernel void x_solve5(__global double* g_rhs,
                       __global double* g_lhs,
                       int c,
                       int start_k, int range_k,
                       int start_j, int range_j,
                       int cell_size_i)
{
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;

  int i, m, n, i1, i2, istart, iend;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  istart = 0;
  iend   = cell_size_i-1;

  //---------------------------------------------------------------------
  // now we know this is the first grid block on the back sweep,
  // so we don't need a message to start the substitution. 
  //---------------------------------------------------------------------
  i  = iend-1;
  i1 = iend;
  n = -1;
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] -
      lhs[c][k][j][i][n+4]*rhs[c][k][j][i1][m];
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] -
      lhs[c][k][j][i][n+4]*rhs[c][k][j][i1][m];
  }

  //---------------------------------------------------------------------
  // Whether or not this is the last processor, we always have
  // to complete the back-substitution 
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // The first three factors
  //---------------------------------------------------------------------
  n = -1;
  for (i = iend-2; i >= istart; i--) {
    i1 = i  + 1;
    i2 = i  + 2;
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k][j][i1][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k][j][i2][m];
    }
  }

  //---------------------------------------------------------------------
  // And the remaining two
  //---------------------------------------------------------------------
  for (i = iend-2; i >= istart; i--) {
    i1 = i  + 1;
    i2 = i  + 2;
    for (m = 3; m < 5; m++) {
      n = (m-2)*5 - 1;
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k][j][i1][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k][j][i2][m];
    }
  }
}

__kernel void x_solve6(__global double* g_rhs,
                       __global double* g_out_buffer,
                       int c,
                       int start_k, int range_k,
                       int start_j, int range_j)
{
#if X_SOLVE6_DIM == 2
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  if (k >= range_k || j >= range_j) return;
#else // X_SOLVE6_DIM == 1
  int k = get_global_id(1) + start_k;
  int j;
  if (k >= range_k) return;
#endif

  int p;
  int i, i1, m;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double *out_buffer = (__global double *)g_out_buffer;

#if X_SOLVE6_DIM == 2
  i  = 0;
  i1 = 1;
  p = 2*5*(get_global_id(0)+get_global_id(1)*(range_j-start_j)) - 1;
  for (m = 0; m < 5; m++) {
    out_buffer[p+1] = rhs[c][k][j][i][m];
    out_buffer[p+2] = rhs[c][k][j][i1][m];
    p = p + 2;
  }

#else // X_SOLVE6_DIM == 1
  i  = 0;
  i1 = 1;
  p = 2*5*(get_global_id(0)*(range_j-start_j)) - 1;
  for (j = start_j; j < range_j; j++) {
    for (m = 0; m < 5; m++) {
      out_buffer[p+1] = rhs[c][k][j][i][m];
      out_buffer[p+2] = rhs[c][k][j][i1][m];
      p = p + 2;
    }
  }
#endif
}


__kernel void lhsy(__global double* g_vs,
                   __global double* g_rho_i,
                   __global double *g_speed,
                   __global double *g_lhs,
                   int c,
                   int start_k, int range_k,
                   int start_i, int range_i,
                   int cell_size_j,
                   int start_j, int end_j)
{
  int k = get_global_id(1) + start_k;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || i >= range_i) return;

  double cv[MAX_CELL_DIM+4], rhoq[MAX_CELL_DIM+4];
  double ru1;
  int j, ip1, jp1, kp1;

  __global double (*vs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  __global double (*speed)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_speed;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  //---------------------------------------------------------------------
  // first fill the lhs for the u-eigenvalue         
  //---------------------------------------------------------------------
  kp1 = k + 1;
  ip1 = i + 1;

  for (j = start_j-1; j <= cell_size_j-end_j; j++) {
    jp1 = j + 1;
    ru1 = c3c4*rho_i[c][kp1][jp1][ip1];
    cv[j+2] = vs[c][kp1][jp1][ip1];
    rhoq[j+2] = max(dy3+con43*ru1, max(dy5+c1c5*ru1, max(dymax+ru1, dy1)));
  }

  for (j = start_j; j < cell_size_j-end_j; j++) {
    lhs[c][k][j][i][0] =  0.0;
    lhs[c][k][j][i][1] = -dtty2 * cv[j+1] - dtty1 * rhoq[j+1];
    lhs[c][k][j][i][2] =  1.0 + c2dtty1 * rhoq[j+2];
    lhs[c][k][j][i][3] =  dtty2 * cv[j+3] - dtty1 * rhoq[j+3];
    lhs[c][k][j][i][4] =  0.0;
  }

  //---------------------------------------------------------------------
  // add fourth order dissipation                             
  //---------------------------------------------------------------------
  if (start_j > 0) {
    j = 1;

    lhs[c][k][j][i][2] = lhs[c][k][j][i][2] + comz5;
    lhs[c][k][j][i][3] = lhs[c][k][j][i][3] - comz4;
    lhs[c][k][j][i][4] = lhs[c][k][j][i][4] + comz1;

    lhs[c][k][j+1][i][1] = lhs[c][k][j+1][i][1] - comz4;
    lhs[c][k][j+1][i][2] = lhs[c][k][j+1][i][2] + comz6;
    lhs[c][k][j+1][i][3] = lhs[c][k][j+1][i][3] - comz4;
    lhs[c][k][j+1][i][4] = lhs[c][k][j+1][i][4] + comz1;
  }

  for (j=3*start_j; j < cell_size_j-3*end_j; j++) {
    lhs[c][k][j][i][0] = lhs[c][k][j][i][0] + comz1;
    lhs[c][k][j][i][1] = lhs[c][k][j][i][1] - comz4;
    lhs[c][k][j][i][2] = lhs[c][k][j][i][2] + comz6;
    lhs[c][k][j][i][3] = lhs[c][k][j][i][3] - comz4;
    lhs[c][k][j][i][4] = lhs[c][k][j][i][4] + comz1;
  }

  if (end_j > 0) {
    j = cell_size_j-3;
    lhs[c][k][j][i][0] = lhs[c][k][j][i][0] + comz1;
    lhs[c][k][j][i][1] = lhs[c][k][j][i][1] - comz4;
    lhs[c][k][j][i][2] = lhs[c][k][j][i][2] + comz6;
    lhs[c][k][j][i][3] = lhs[c][k][j][i][3] - comz4;

    lhs[c][k][j+1][i][0] = lhs[c][k][j+1][i][0] + comz1;
    lhs[c][k][j+1][i][1] = lhs[c][k][j+1][i][1] - comz4;
    lhs[c][k][j+1][i][2] = lhs[c][k][j+1][i][2] + comz5;
  }

  //---------------------------------------------------------------------
  // subsequently, for (the other two factors                    
  //---------------------------------------------------------------------
  for (j = start_j; j < cell_size_j-end_j; j++) {
    lhs[c][k][j][i][0+5]  = lhs[c][k][j][i][0];
    lhs[c][k][j][i][1+5]  = lhs[c][k][j][i][1] - 
                            dtty2 * speed[c][kp1][j][ip1];
    lhs[c][k][j][i][2+5]  = lhs[c][k][j][i][2];
    lhs[c][k][j][i][3+5]  = lhs[c][k][j][i][3] + 
                            dtty2 * speed[c][kp1][j+2][ip1];
    lhs[c][k][j][i][4+5]  = lhs[c][k][j][i][4];
    lhs[c][k][j][i][0+10] = lhs[c][k][j][i][0];
    lhs[c][k][j][i][1+10] = lhs[c][k][j][i][1] + 
                            dtty2 * speed[c][kp1][j][ip1];
    lhs[c][k][j][i][2+10] = lhs[c][k][j][i][2];
    lhs[c][k][j][i][3+10] = lhs[c][k][j][i][3] - 
                            dtty2 * speed[c][kp1][j+2][ip1];
    lhs[c][k][j][i][4+10] = lhs[c][k][j][i][4];
  }
}

__kernel void pinvr(__global double* g_rhs,
                    int c,
                    int start_k, int range_k,
                    int start_j, int range_j,
                    int start_i, int range_i)
{
#if PINVR_DIM == 3
  int k = get_global_id(2) + start_k;
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || j >= range_j || i >= range_i) return;
#elif PINVR_DIM == 2
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  int i;
  if (k >= range_k || j >= range_j) return;
#else // PINVR_DIM == 1
  int k = get_global_id(0) + start_k;
  int j, i;
  if (k >= range_k) return;
#endif

  double r1, r2, r3, r4, r5, t1, t2;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;

#if PINVR_DIM == 3
  r1 = rhs[c][k][j][i][0];
  r2 = rhs[c][k][j][i][1];
  r3 = rhs[c][k][j][i][2];
  r4 = rhs[c][k][j][i][3];
  r5 = rhs[c][k][j][i][4];

  t1 = bt * r1;
  t2 = 0.5 * ( r4 + r5 );

  rhs[c][k][j][i][0] =  bt * ( r4 - r5 );
  rhs[c][k][j][i][1] = -r3;
  rhs[c][k][j][i][2] =  r2;
  rhs[c][k][j][i][3] = -t1 + t2;
  rhs[c][k][j][i][4] =  t1 + t2;

#elif PINVR_DIM == 2
  for (i = start_i; i < range_i; i++) {

    r1 = rhs[c][k][j][i][0];
    r2 = rhs[c][k][j][i][1];
    r3 = rhs[c][k][j][i][2];
    r4 = rhs[c][k][j][i][3];
    r5 = rhs[c][k][j][i][4];

    t1 = bt * r1;
    t2 = 0.5 * ( r4 + r5 );

    rhs[c][k][j][i][0] =  bt * ( r4 - r5 );
    rhs[c][k][j][i][1] = -r3;
    rhs[c][k][j][i][2] =  r2;
    rhs[c][k][j][i][3] = -t1 + t2;
    rhs[c][k][j][i][4] =  t1 + t2;
  }    

#else // PINVR_DIM == 1
  for (j = start_j; j < range_j; j++) {
    for (i = start_i; i < range_i; i++) {

      r1 = rhs[c][k][j][i][0];
      r2 = rhs[c][k][j][i][1];
      r3 = rhs[c][k][j][i][2];
      r4 = rhs[c][k][j][i][3];
      r5 = rhs[c][k][j][i][4];

      t1 = bt * r1;
      t2 = 0.5 * ( r4 + r5 );

      rhs[c][k][j][i][0] =  bt * ( r4 - r5 );
      rhs[c][k][j][i][1] = -r3;
      rhs[c][k][j][i][2] =  r2;
      rhs[c][k][j][i][3] = -t1 + t2;
      rhs[c][k][j][i][4] =  t1 + t2;
    }    
  }
#endif
}

__kernel void y_solve1(__global double* g_rhs,
                       __global double* g_lhs,
                       __global double* g_in_buffer,
                       int c,
                       int start_k, int range_k,
                       int start_i, int range_i)
{
  int k = get_global_id(1) + start_k;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || i >= range_i) return;

  int p1, p2;
  int j, m, n, j1;
  double r1, r2, d, e, s[5];

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;
  __global double *in_buffer = (__global double *)g_in_buffer;

  p1 = 10*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;
  p2 = 10*(range_i-start_i)*(range_k-start_k) + 6*2*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;

  //---------------------------------------------------------------------
  // unpack the buffer                                 
  //---------------------------------------------------------------------
  j  = 0;
  j1 = 1;
  n = -1;
  //---------------------------------------------------------------------
  // create a running pointer
  //---------------------------------------------------------------------
  lhs[c][k][j][i][n+2] = lhs[c][k][j][i][n+2] -
    in_buffer[p1+1] * lhs[c][k][j][i][n+1];
  lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] -
    in_buffer[p1+2] * lhs[c][k][j][i][n+1];
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] -
      in_buffer[p1+3+m] * lhs[c][k][j][i][n+1];
  }
  d = in_buffer[p1+6];
  e = in_buffer[p1+7];
  for (m = 0; m < 3; m++) {
    s[m] = in_buffer[p1+8+m];
  }
  r1 = lhs[c][k][j][i][n+2];
  lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] - d * r1;
  lhs[c][k][j][i][n+4] = lhs[c][k][j][i][n+4] - e * r1;
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - s[m] * r1;
  }
  r2 = lhs[c][k][j1][i][n+1];
  lhs[c][k][j1][i][n+2] = lhs[c][k][j1][i][n+2] - d * r2;
  lhs[c][k][j1][i][n+3] = lhs[c][k][j1][i][n+3] - e * r2;
  for (m = 0; m < 3; m++) {
    rhs[c][k][j1][i][m] = rhs[c][k][j1][i][m] - s[m] * r2;
  }
  p1 = p1 + 10;

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;

    lhs[c][k][j][i][n+2] = lhs[c][k][j][i][n+2] -
      in_buffer[p2+1] * lhs[c][k][j][i][n+1];
    lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] -
      in_buffer[p2+2] * lhs[c][k][j][i][n+1];
    rhs[c][k][j][i][m]   = rhs[c][k][j][i][m] -
      in_buffer[p2+3] * lhs[c][k][j][i][n+1];
    d    = in_buffer[p2+4];
    e    = in_buffer[p2+5];
    s[m] = in_buffer[p2+6];
    r1 = lhs[c][k][j][i][n+2];
    lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] - d * r1;
    lhs[c][k][j][i][n+4] = lhs[c][k][j][i][n+4] - e * r1;
    rhs[c][k][j][i][m]   = rhs[c][k][j][i][m] - s[m] * r1;
    r2 = lhs[c][k][j1][i][n+1];
    lhs[c][k][j1][i][n+2] = lhs[c][k][j1][i][n+2] - d * r2;
    lhs[c][k][j1][i][n+3] = lhs[c][k][j1][i][n+3] - e * r2;
    rhs[c][k][j1][i][m]   = rhs[c][k][j1][i][m] - s[m] * r2;
    p2 = p2 + 6;
  }
}

__kernel void y_solve2(__global double* g_rhs,
                       __global double* g_lhs,
                       int c,
                       int start_k, int range_k,
                       int start_i, int range_i,
                       int cell_size_j)
{
  int k = get_global_id(1) + start_k;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || i >= range_i) return;

  int j, m, n, j1, j2, jstart, jend;
  double fac1, fac2;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  jstart = 0;
  jend   = cell_size_j-1;

  //---------------------------------------------------------------------
  // perform the Thomas algorithm; first, FORWARD ELIMINATION     
  //---------------------------------------------------------------------
  n = -1;

  for (j = jstart; j <= jend-2; j++) {
    j1 = j  + 1;
    j2 = j  + 2;
    fac1                 = 1.0/lhs[c][k][j][i][n+3];
    lhs[c][k][j][i][n+4] = fac1*lhs[c][k][j][i][n+4];
    lhs[c][k][j][i][n+5] = fac1*lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i][m] = fac1*rhs[c][k][j][i][m];
    }
    lhs[c][k][j1][i][n+3] = lhs[c][k][j1][i][n+3] -
      lhs[c][k][j1][i][n+2]*lhs[c][k][j][i][n+4];
    lhs[c][k][j1][i][n+4] = lhs[c][k][j1][i][n+4] -
      lhs[c][k][j1][i][n+2]*lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      rhs[c][k][j1][i][m] = rhs[c][k][j1][i][m] -
        lhs[c][k][j1][i][n+2]*rhs[c][k][j][i][m];
    }
    lhs[c][k][j2][i][n+2] = lhs[c][k][j2][i][n+2] -
      lhs[c][k][j2][i][n+1]*lhs[c][k][j][i][n+4];
    lhs[c][k][j2][i][n+3] = lhs[c][k][j2][i][n+3] -
      lhs[c][k][j2][i][n+1]*lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      rhs[c][k][j2][i][m] = rhs[c][k][j2][i][m] -
        lhs[c][k][j2][i][n+1]*rhs[c][k][j][i][m];
    }
  }

  //---------------------------------------------------------------------
  // The last two rows in this grid block are a bit different, 
  // since they do not have two more rows available for the
  // elimination of off-diagonal entries
  //---------------------------------------------------------------------

  j  = jend - 1;
  j1 = jend;
  fac1                 = 1.0/lhs[c][k][j][i][n+3];
  lhs[c][k][j][i][n+4] = fac1*lhs[c][k][j][i][n+4];
  lhs[c][k][j][i][n+5] = fac1*lhs[c][k][j][i][n+5];
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = fac1*rhs[c][k][j][i][m];
  }
  lhs[c][k][j1][i][n+3] = lhs[c][k][j1][i][n+3] -
    lhs[c][k][j1][i][n+2]*lhs[c][k][j][i][n+4];
  lhs[c][k][j1][i][n+4] = lhs[c][k][j1][i][n+4] -
    lhs[c][k][j1][i][n+2]*lhs[c][k][j][i][n+5];
  for (m = 0; m < 3; m++) {
    rhs[c][k][j1][i][m] = rhs[c][k][j1][i][m] -
      lhs[c][k][j1][i][n+2]*rhs[c][k][j][i][m];
  }
  //-------------------------------------------------------------------
  // scale the last row immediately (some of this is
  // overkill in case this is the last cell)
  //-------------------------------------------------------------------
  fac2                  = 1.0/lhs[c][k][j1][i][n+3];
  lhs[c][k][j1][i][n+4] = fac2*lhs[c][k][j1][i][n+4];
  lhs[c][k][j1][i][n+5] = fac2*lhs[c][k][j1][i][n+5]; 
  for (m = 0; m < 3; m++) {
    rhs[c][k][j1][i][m] = fac2*rhs[c][k][j1][i][m];
  }

  //---------------------------------------------------------------------
  // do the u+c and the u-c factors                 
  //---------------------------------------------------------------------
  for (j = jstart; j <= jend-2; j++) {
    j1 = j  + 1;
    j2 = j  + 2;
    for (m = 3; m < 5; m++) {
      n = (m-2)*5 - 1;
      fac1                  = 1.0/lhs[c][k][j][i][n+3];
      lhs[c][k][j][i][n+4]  = fac1*lhs[c][k][j][i][n+4];
      lhs[c][k][j][i][n+5]  = fac1*lhs[c][k][j][i][n+5];
      rhs[c][k][j][i][m]    = fac1*rhs[c][k][j][i][m];
      lhs[c][k][j1][i][n+3] = lhs[c][k][j1][i][n+3] -
        lhs[c][k][j1][i][n+2]*lhs[c][k][j][i][n+4];
      lhs[c][k][j1][i][n+4] = lhs[c][k][j1][i][n+4] -
        lhs[c][k][j1][i][n+2]*lhs[c][k][j][i][n+5];
      rhs[c][k][j1][i][m]   = rhs[c][k][j1][i][m] -
        lhs[c][k][j1][i][n+2]*rhs[c][k][j][i][m];
      lhs[c][k][j2][i][n+2] = lhs[c][k][j2][i][n+2] -
        lhs[c][k][j2][i][n+1]*lhs[c][k][j][i][n+4];
      lhs[c][k][j2][i][n+3] = lhs[c][k][j2][i][n+3] -
        lhs[c][k][j2][i][n+1]*lhs[c][k][j][i][n+5];
      rhs[c][k][j2][i][m]   = rhs[c][k][j2][i][m] -
        lhs[c][k][j2][i][n+1]*rhs[c][k][j][i][m];
    }
  }

  //---------------------------------------------------------------------
  // And again the last two rows separately
  //---------------------------------------------------------------------
  j  = jend - 1;
  j1 = jend;
  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    fac1                  = 1.0/lhs[c][k][j][i][n+3];
    lhs[c][k][j][i][n+4]  = fac1*lhs[c][k][j][i][n+4];
    lhs[c][k][j][i][n+5]  = fac1*lhs[c][k][j][i][n+5];
    rhs[c][k][j][i][m]    = fac1*rhs[c][k][j][i][m];
    lhs[c][k][j1][i][n+3] = lhs[c][k][j1][i][n+3] -
      lhs[c][k][j1][i][n+2]*lhs[c][k][j][i][n+4];
    lhs[c][k][j1][i][n+4] = lhs[c][k][j1][i][n+4] -
      lhs[c][k][j1][i][n+2]*lhs[c][k][j][i][n+5];
    rhs[c][k][j1][i][m]   = rhs[c][k][j1][i][m] -
      lhs[c][k][j1][i][n+2]*rhs[c][k][j][i][m];
    //-----------------------------------------------------------------
    // Scale the last row immediately (some of this is overkill
    // if this is the last cell)
    //-----------------------------------------------------------------
    fac2                  = 1.0/lhs[c][k][j1][i][n+3];
    lhs[c][k][j1][i][n+4] = fac2*lhs[c][k][j1][i][n+4];
    lhs[c][k][j1][i][n+5] = fac2*lhs[c][k][j1][i][n+5];
    rhs[c][k][j1][i][m]   = fac2*rhs[c][k][j1][i][m];
  }
}

__kernel void y_solve3(__global double* g_rhs,
                       __global double* g_lhs,
                       __global double* g_out_buffer,
                       int c,
                       int start_k, int range_k,
                       int start_i, int range_i,
                       int cell_size_j)
{
  int k = get_global_id(1) + start_k;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || i >= range_i) return;

  int p1, p2;
  int j, m, n, jend;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;
  __global double *out_buffer = (__global double *)g_out_buffer;

  p1 = 5*2*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;
  p2 = 5*2*(range_i-start_i)*(range_k-start_k) + 3*2*2*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;

  jend   = cell_size_j-1;
  //---------------------------------------------------------------------
  // create a running pointer for the send buffer  
  //---------------------------------------------------------------------
  n = -1; //n = 0;
  for (j = jend-1; j <= jend; j++) {
    out_buffer[p1+1] = lhs[c][k][j][i][n+4];
    out_buffer[p1+2] = lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      out_buffer[p1+3+m] = rhs[c][k][j][i][m];
    }
    p1 = p1+5;
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    for (j = jend-1; j <= jend; j++) {
      out_buffer[p2+1] = lhs[c][k][j][i][n+4];
      out_buffer[p2+2] = lhs[c][k][j][i][n+5];
      out_buffer[p2+3] = rhs[c][k][j][i][m];
      p2 = p2 + 3;
    }
  }
}

__kernel void y_solve4(__global double* g_rhs,
                       __global double* g_lhs,
                       __global double* g_in_buffer,
                       int c,
                       int start_k, int range_k,
                       int start_i, int range_i,
                       int cell_size_j)
{
  int k = get_global_id(1) + start_k;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || i >= range_i) return;

  int p;
  int j, m, n, j1, j2, jstart, jend;
  double sm1, sm2;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;
  __global double *in_buffer = (__global double *)g_in_buffer;

  jstart = 0;
  jend   = cell_size_j-1;

  //---------------------------------------------------------------------
  // unpack the buffer
  //---------------------------------------------------------------------
  p = 2*5*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;
  j  = jend;
  j1 = j - 1;
  for (m = 0; m < 3; m++) {
    sm1 = in_buffer[p+1];
    sm2 = in_buffer[p+2];
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
      lhs[c][k][j][i][3]*sm1 -
      lhs[c][k][j][i][4]*sm2;
    rhs[c][k][j1][i][m] = rhs[c][k][j1][i][m] -
      lhs[c][k][j1][i][3] * rhs[c][k][j][i][m] - 
      lhs[c][k][j1][i][4] * sm1;
    p = p + 2;
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    sm1 = in_buffer[p+1];
    sm2 = in_buffer[p+2];
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
      lhs[c][k][j][i][n+4]*sm1 -
      lhs[c][k][j][i][n+5]*sm2;
    rhs[c][k][j1][i][m] = rhs[c][k][j1][i][m] -
      lhs[c][k][j1][i][n+4] * rhs[c][k][j][i][m] - 
      lhs[c][k][j1][i][n+5] * sm1;
    p = p + 2;
  }

  //---------------------------------------------------------------------
  // Whether or not this is the last processor, we always have
  // to complete the back-substitution 
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // The first three factors
  //---------------------------------------------------------------------
  n = -1;
  for (j = jend-2; j >= jstart; j--) {
    j1 = j  + 1;
    j2 = j  + 2;
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k][j1][i][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k][j2][i][m];
    }
  }

  //---------------------------------------------------------------------
  // And the remaining two
  //---------------------------------------------------------------------
  for (j = jend-2; j >= jstart; j--) {
    j1 = j  + 1;
    j2 = j1 + 1;
    for (m = 3; m < 5; m++) {
      n = (m-2)*5 - 1;
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k][j1][i][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k][j2][i][m];
    }
  }
}

__kernel void y_solve5(__global double* g_rhs,
                       __global double* g_lhs,
                       int c,
                       int start_k, int range_k,
                       int start_i, int range_i,
                       int cell_size_j)
{
  int k = get_global_id(1) + start_k;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || i >= range_i) return;

  int j, m, n, j1, j2, jstart, jend;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  jstart = 0;
  jend   = cell_size_j-1;

  //---------------------------------------------------------------------
  // now we know this is the first grid block on the back sweep,
  // so we don't need a message to start the substitution. 
  //---------------------------------------------------------------------

  j  = jend - 1;
  j1 = jend;
  n = -1;
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] -
      lhs[c][k][j][i][n+4]*rhs[c][k][j1][i][m];
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] -
      lhs[c][k][j][i][n+4]*rhs[c][k][j1][i][m];
  }

  //---------------------------------------------------------------------
  // Whether or not this is the last processor, we always have
  // to complete the back-substitution 
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // The first three factors
  //---------------------------------------------------------------------
  n = -1;
  for (j = jend-2; j >= jstart; j--) {
    j1 = j  + 1;
    j2 = j  + 2;
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k][j1][i][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k][j2][i][m];
    }
  }

  //---------------------------------------------------------------------
  // And the remaining two
  //---------------------------------------------------------------------
  for (j = jend-2; j >= jstart; j--) {
    j1 = j  + 1;
    j2 = j1 + 1;
    for (m = 3; m < 5; m++) {
      n = (m-2)*5 - 1;
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k][j1][i][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k][j2][i][m];
    }
  }
}

__kernel void y_solve6(__global double* g_rhs,
                       __global double* g_out_buffer,
                       int c,
                       int start_k, int range_k,
                       int start_i, int range_i)
{
#if Y_SOLVE6_DIM == 2
  int k = get_global_id(1) + start_k;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || i >= range_i) return;
#else // Y_SOLVE6_DIM == 1
  int k = get_global_id(1) + start_k;
  int i;
  if (k >= range_k) return;
#endif

  int p;
  int j, j1, m;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double *out_buffer = (__global double *)g_out_buffer;

#if Y_SOLVE6_DIM == 2
  j  = 0;
  j1 = 1;
  p = 2*5*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;
  for (m = 0; m < 5; m++) {
    out_buffer[p+1] = rhs[c][k][j][i][m];
    out_buffer[p+2] = rhs[c][k][j1][i][m];
    p = p + 2;
  }

#else // Y_SOLVE6_DIM == 1
  j  = 0;
  j1 = 1;
  p = 2*5*(get_global_id(0)*(range_i-start_i)) - 1;
  for (i = start_i; i < range_i; i++) {
    for (m = 0; m < 5; m++) {
      out_buffer[p+1] = rhs[c][k][j][i][m];
      out_buffer[p+2] = rhs[c][k][j1][i][m];
      p = p + 2;
    }
  }
#endif
}


__kernel void lhsz(__global double* g_ws,
                   __global double* g_rho_i,
                   __global double *g_speed,
                   __global double *g_lhs,
                   int c,
                   int start_j, int range_j,
                   int start_i, int range_i,
                   int cell_size_k,
                   int start_k, int end_k)
{
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (j >= range_j || i >= range_i) return;

  double cv[MAX_CELL_DIM+4], rhos[MAX_CELL_DIM+4];
  double ru1;
  int k, ip1, jp1, kp1;

  __global double (*ws)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  __global double (*rho_i)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_rho_i;
  __global double (*speed)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_speed;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  //---------------------------------------------------------------------
  // first fill the lhs for the u-eigenvalue                          
  //---------------------------------------------------------------------
  jp1 = j + 1;
  ip1 = i + 1;
  for (k = start_k-1; k <= cell_size_k-end_k; k++) {
    kp1 = k + 1;
    ru1 = c3c4*rho_i[c][kp1][jp1][ip1];
    cv[k+2] = ws[c][kp1][jp1][ip1];
    rhos[k+2] = max(dz4+con43*ru1, max(dz5+c1c5*ru1, max(dzmax+ru1, dz1)));
  }

  for (k = start_k; k < cell_size_k-end_k; k++) {
    lhs[c][k][j][i][0] =  0.0;
    lhs[c][k][j][i][1] = -dttz2 * cv[k+1] - dttz1 * rhos[k+1];
    lhs[c][k][j][i][2] =  1.0 + c2dttz1 * rhos[k+2];
    lhs[c][k][j][i][3] =  dttz2 * cv[k+3] - dttz1 * rhos[k+3];
    lhs[c][k][j][i][4] =  0.0;
  }

  //---------------------------------------------------------------------
  // add fourth order dissipation                                  
  //---------------------------------------------------------------------
  if (start_k > 0) {
    k = 1;
    lhs[c][k][j][i][2] = lhs[c][k][j][i][2] + comz5;
    lhs[c][k][j][i][3] = lhs[c][k][j][i][3] - comz4;
    lhs[c][k][j][i][4] = lhs[c][k][j][i][4] + comz1;

    lhs[c][k+1][j][i][1] = lhs[c][k+1][j][i][1] - comz4;
    lhs[c][k+1][j][i][2] = lhs[c][k+1][j][i][2] + comz6;
    lhs[c][k+1][j][i][3] = lhs[c][k+1][j][i][3] - comz4;
    lhs[c][k+1][j][i][4] = lhs[c][k+1][j][i][4] + comz1;
  }

  for (k = 3*start_k; k < cell_size_k-3*end_k; k++) {
    lhs[c][k][j][i][0] = lhs[c][k][j][i][0] + comz1;
    lhs[c][k][j][i][1] = lhs[c][k][j][i][1] - comz4;
    lhs[c][k][j][i][2] = lhs[c][k][j][i][2] + comz6;
    lhs[c][k][j][i][3] = lhs[c][k][j][i][3] - comz4;
    lhs[c][k][j][i][4] = lhs[c][k][j][i][4] + comz1;
  }

  if (end_k > 0) {
    k = cell_size_k-3;
    lhs[c][k][j][i][0] = lhs[c][k][j][i][0] + comz1;
    lhs[c][k][j][i][1] = lhs[c][k][j][i][1] - comz4;
    lhs[c][k][j][i][2] = lhs[c][k][j][i][2] + comz6;
    lhs[c][k][j][i][3] = lhs[c][k][j][i][3] - comz4;

    lhs[c][k+1][j][i][0] = lhs[c][k+1][j][i][0] + comz1;
    lhs[c][k+1][j][i][1] = lhs[c][k+1][j][i][1] - comz4;
    lhs[c][k+1][j][i][2] = lhs[c][k+1][j][i][2] + comz5;
  }

  //---------------------------------------------------------------------
  // subsequently, fill the other factors (u+c), (u-c) 
  //---------------------------------------------------------------------
  for (k = start_k; k < cell_size_k-end_k; k++) {
    lhs[c][k][j][i][0+5]  = lhs[c][k][j][i][0];
    lhs[c][k][j][i][1+5]  = lhs[c][k][j][i][1] - 
                            dttz2 * speed[c][k][jp1][ip1];
    lhs[c][k][j][i][2+5]  = lhs[c][k][j][i][2];
    lhs[c][k][j][i][3+5]  = lhs[c][k][j][i][3] + 
                            dttz2 * speed[c][k+2][jp1][ip1];
    lhs[c][k][j][i][4+5]  = lhs[c][k][j][i][4];
    lhs[c][k][j][i][0+10] = lhs[c][k][j][i][0];
    lhs[c][k][j][i][1+10] = lhs[c][k][j][i][1] + 
                            dttz2 * speed[c][k][jp1][ip1];
    lhs[c][k][j][i][2+10] = lhs[c][k][j][i][2];
    lhs[c][k][j][i][3+10] = lhs[c][k][j][i][3] - 
                            dttz2 * speed[c][k+2][jp1][ip1];
    lhs[c][k][j][i][4+10] = lhs[c][k][j][i][4];
  }
}

__kernel void tzetar(__global double* g_u,
                     __global double* g_us,
                     __global double* g_vs,
                     __global double* g_ws,
                     __global double* g_qs,
                     __global double* g_ainv,
                     __global double* g_speed,
                     __global double* g_rhs,
                     int c,
                     int start_k, int range_k,
                     int start_j, int range_j,
                     int start_i, int range_i)
{
#if TZETAR_DIM == 3
  int k = get_global_id(2) + start_k;
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || j >= range_j || i >= range_i) return;
#elif TZETAR_DIM == 2
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  int i;
  if (k >= range_k || j >= range_j) return;
#else // TZETAR_DIM == 1
  int k = get_global_id(0) + start_k;
  int j, i;
  if (k >= range_k) return;
#endif

  int ip1, ip2, jp1, jp2, kp1, kp2;
  double t1, t2, t3, ac, xvel, yvel, zvel, r1, r2, r3, r4, r5;
  double btuz, acinv, ac2u, uzik1;

  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double (*us)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_us;
  __global double (*vs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_vs;
  __global double (*ws)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ws;
  __global double (*qs)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_qs;
  __global double (*ainv)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_ainv;
  __global double (*speed)[KMAX+2][JMAX+2][IMAX+2] =
    (__global double (*)[KMAX+2][JMAX+2][IMAX+2])g_speed;
  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;

#if TZETAR_DIM == 3
  kp1 = k + 1;
  kp2 = k + 2;
  jp1 = j + 1;
  jp2 = j + 2;
  ip1 = i + 1;
  ip2 = i + 2;

  xvel = us[c][kp1][jp1][ip1];
  yvel = vs[c][kp1][jp1][ip1];
  zvel = ws[c][kp1][jp1][ip1];
  ac   = speed[c][kp1][jp1][ip1];
  acinv = ainv[c][kp1][jp1][ip1];

  ac2u = ac*ac;

  r1 = rhs[c][k][j][i][0];
  r2 = rhs[c][k][j][i][1];
  r3 = rhs[c][k][j][i][2];
  r4 = rhs[c][k][j][i][3];
  r5 = rhs[c][k][j][i][4];     

  uzik1 = u[c][kp2][jp2][ip2][0];
  btuz  = bt * uzik1;

  t1 = btuz*acinv * (r4 + r5);
  t2 = r3 + t1;
  t3 = btuz * (r4 - r5);

  rhs[c][k][j][i][0] = t2;
  rhs[c][k][j][i][1] = -uzik1*r2 + xvel*t2;
  rhs[c][k][j][i][2] =  uzik1*r1 + yvel*t2;
  rhs[c][k][j][i][3] =  zvel*t2  + t3;
  rhs[c][k][j][i][4] =  uzik1*(-xvel*r2 + yvel*r1) + 
    qs[c][kp1][jp1][ip1]*t2 + c2iv*ac2u*t1 + zvel*t3;

#elif TZETAR_DIM == 2
  kp1 = k + 1;
  kp2 = k + 2;
  jp1 = j + 1;
  jp2 = j + 2;
  for (i = start_i; i < range_i; i++) {
    ip1 = i + 1;
    ip2 = i + 2;

    xvel = us[c][kp1][jp1][ip1];
    yvel = vs[c][kp1][jp1][ip1];
    zvel = ws[c][kp1][jp1][ip1];
    ac   = speed[c][kp1][jp1][ip1];
    acinv = ainv[c][kp1][jp1][ip1];

    ac2u = ac*ac;

    r1 = rhs[c][k][j][i][0];
    r2 = rhs[c][k][j][i][1];
    r3 = rhs[c][k][j][i][2];
    r4 = rhs[c][k][j][i][3];
    r5 = rhs[c][k][j][i][4];     

    uzik1 = u[c][kp2][jp2][ip2][0];
    btuz  = bt * uzik1;

    t1 = btuz*acinv * (r4 + r5);
    t2 = r3 + t1;
    t3 = btuz * (r4 - r5);

    rhs[c][k][j][i][0] = t2;
    rhs[c][k][j][i][1] = -uzik1*r2 + xvel*t2;
    rhs[c][k][j][i][2] =  uzik1*r1 + yvel*t2;
    rhs[c][k][j][i][3] =  zvel*t2  + t3;
    rhs[c][k][j][i][4] =  uzik1*(-xvel*r2 + yvel*r1) + 
      qs[c][kp1][jp1][ip1]*t2 + c2iv*ac2u*t1 + zvel*t3;

  }

#else // TZETAR_DIM == 1
  kp1 = k + 1;
  kp2 = k + 2;
  for (j = start_j; j < range_j; j++) {
    jp1 = j + 1;
    jp2 = j + 2;
    for (i = start_i; i < range_i; i++) {
      ip1 = i + 1;
      ip2 = i + 2;

      xvel = us[c][kp1][jp1][ip1];
      yvel = vs[c][kp1][jp1][ip1];
      zvel = ws[c][kp1][jp1][ip1];
      ac   = speed[c][kp1][jp1][ip1];
      acinv = ainv[c][kp1][jp1][ip1];

      ac2u = ac*ac;

      r1 = rhs[c][k][j][i][0];
      r2 = rhs[c][k][j][i][1];
      r3 = rhs[c][k][j][i][2];
      r4 = rhs[c][k][j][i][3];
      r5 = rhs[c][k][j][i][4];     

      uzik1 = u[c][kp2][jp2][ip2][0];
      btuz  = bt * uzik1;

      t1 = btuz*acinv * (r4 + r5);
      t2 = r3 + t1;
      t3 = btuz * (r4 - r5);

      rhs[c][k][j][i][0] = t2;
      rhs[c][k][j][i][1] = -uzik1*r2 + xvel*t2;
      rhs[c][k][j][i][2] =  uzik1*r1 + yvel*t2;
      rhs[c][k][j][i][3] =  zvel*t2  + t3;
      rhs[c][k][j][i][4] =  uzik1*(-xvel*r2 + yvel*r1) + 
        qs[c][kp1][jp1][ip1]*t2 + c2iv*ac2u*t1 + zvel*t3;

    }
  }
#endif
}

__kernel void z_solve1(__global double* g_rhs,
                       __global double* g_lhs,
                       __global double* g_in_buffer,
                       int c,
                       int start_j, int range_j,
                       int start_i, int range_i)
{
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (j >= range_j || i >= range_i) return;

  int p1, p2;
  int k, m, n, k1;
  double r1, r2, d, e, s[5];

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;
  __global double *in_buffer = (__global double *)g_in_buffer;

  p1 = 10*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;
  p2 = 10*(range_i-start_i)*(range_j-start_j) + 6*2*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;

  //---------------------------------------------------------------------
  // unpack the buffer                                 
  //---------------------------------------------------------------------
  k  = 0;
  k1 = 1;
  n = -1;

  //---------------------------------------------------------------------
  // create a running pointer
  //---------------------------------------------------------------------
  lhs[c][k][j][i][n+2] = lhs[c][k][j][i][n+2] -
    in_buffer[p1+1] * lhs[c][k][j][i][n+1];
  lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] -
    in_buffer[p1+2] * lhs[c][k][j][i][n+1];
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] -
      in_buffer[p1+3+m] * lhs[c][k][j][i][n+1];
  }
  d = in_buffer[p1+6];
  e = in_buffer[p1+7];
  for (m = 0; m < 3; m++) {
    s[m] = in_buffer[p1+8+m];
  }
  r1 = lhs[c][k][j][i][n+2];
  lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] - d * r1;
  lhs[c][k][j][i][n+4] = lhs[c][k][j][i][n+4] - e * r1;
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - s[m] * r1;
  }
  r2 = lhs[c][k1][j][i][n+1];
  lhs[c][k1][j][i][n+2] = lhs[c][k1][j][i][n+2] - d * r2;
  lhs[c][k1][j][i][n+3] = lhs[c][k1][j][i][n+3] - e * r2;
  for (m = 0; m < 3; m++) {
    rhs[c][k1][j][i][m] = rhs[c][k1][j][i][m] - s[m] * r2;
  }
  p1 = p1 + 10;

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    lhs[c][k][j][i][n+2] = lhs[c][k][j][i][n+2] -
      in_buffer[p2+1] * lhs[c][k][j][i][n+1];
    lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] -
      in_buffer[p2+2] * lhs[c][k][j][i][n+1];
    rhs[c][k][j][i][m]   = rhs[c][k][j][i][m] -
      in_buffer[p2+3] * lhs[c][k][j][i][n+1];
    d    = in_buffer[p2+4];
    e    = in_buffer[p2+5];
    s[m] = in_buffer[p2+6];
    r1 = lhs[c][k][j][i][n+2];
    lhs[c][k][j][i][n+3] = lhs[c][k][j][i][n+3] - d * r1;
    lhs[c][k][j][i][n+4] = lhs[c][k][j][i][n+4] - e * r1;
    rhs[c][k][j][i][m]   = rhs[c][k][j][i][m] - s[m] * r1;
    r2 = lhs[c][k1][j][i][n+1];
    lhs[c][k1][j][i][n+2] = lhs[c][k1][j][i][n+2] - d * r2;
    lhs[c][k1][j][i][n+3] = lhs[c][k1][j][i][n+3] - e * r2;
    rhs[c][k1][j][i][m]   = rhs[c][k1][j][i][m] - s[m] * r2;
    p2 = p2 + 6;
  }
}

__kernel void z_solve2(__global double* g_rhs,
                       __global double* g_lhs,
                       int c,
                       int start_j, int range_j,
                       int start_i, int range_i,
                       int cell_size_k)
{
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (j >= range_j || i >= range_i) return;

  int k, m, n, k1, k2, kstart, kend;
  double fac1, fac2;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  kstart = 0;
  kend   = cell_size_k-1;

  //---------------------------------------------------------------------
  // perform the Thomas algorithm; first, FORWARD ELIMINATION     
  //---------------------------------------------------------------------
  n = -1;

  for (k = kstart; k <= kend-2; k++) {
    k1 = k  + 1;
    k2 = k  + 2;
    fac1                 = 1.0/lhs[c][k][j][i][n+3];
    lhs[c][k][j][i][n+4] = fac1*lhs[c][k][j][i][n+4];
    lhs[c][k][j][i][n+5] = fac1*lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i][m] = fac1*rhs[c][k][j][i][m];
    }
    lhs[c][k1][j][i][n+3] = lhs[c][k1][j][i][n+3] -
      lhs[c][k1][j][i][n+2]*lhs[c][k][j][i][n+4];
    lhs[c][k1][j][i][n+4] = lhs[c][k1][j][i][n+4] -
      lhs[c][k1][j][i][n+2]*lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      rhs[c][k1][j][i][m] = rhs[c][k1][j][i][m] -
        lhs[c][k1][j][i][n+2]*rhs[c][k][j][i][m];
    }
    lhs[c][k2][j][i][n+2] = lhs[c][k2][j][i][n+2] -
      lhs[c][k2][j][i][n+1]*lhs[c][k][j][i][n+4];
    lhs[c][k2][j][i][n+3] = lhs[c][k2][j][i][n+3] -
      lhs[c][k2][j][i][n+1]*lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      rhs[c][k2][j][i][m] = rhs[c][k2][j][i][m] -
        lhs[c][k2][j][i][n+1]*rhs[c][k][j][i][m];
    }
  }

  //---------------------------------------------------------------------
  // The last two rows in this grid block are a bit different, 
  // since they do not have two more rows available for the
  // elimination of off-diagonal entries
  //---------------------------------------------------------------------
  k  = kend - 1;
  k1 = kend;
  fac1                 = 1.0/lhs[c][k][j][i][n+3];
  lhs[c][k][j][i][n+4] = fac1*lhs[c][k][j][i][n+4];
  lhs[c][k][j][i][n+5] = fac1*lhs[c][k][j][i][n+5];
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = fac1*rhs[c][k][j][i][m];
  }
  lhs[c][k1][j][i][n+3] = lhs[c][k1][j][i][n+3] -
    lhs[c][k1][j][i][n+2]*lhs[c][k][j][i][n+4];
  lhs[c][k1][j][i][n+4] = lhs[c][k1][j][i][n+4] -
    lhs[c][k1][j][i][n+2]*lhs[c][k][j][i][n+5];
  for (m = 0; m < 3; m++) {
    rhs[c][k1][j][i][m] = rhs[c][k1][j][i][m] -
      lhs[c][k1][j][i][n+2]*rhs[c][k][j][i][m];
  }
  //-------------------------------------------------------------------
  // scale the last row immediately (some of this is
  // overkill in case this is the last cell)
  //-------------------------------------------------------------------
  fac2                  = 1.0/lhs[c][k1][j][i][n+3];
  lhs[c][k1][j][i][n+4] = fac2*lhs[c][k1][j][i][n+4];
  lhs[c][k1][j][i][n+5] = fac2*lhs[c][k1][j][i][n+5]; 
  for (m = 0; m < 3; m++) {
    rhs[c][k1][j][i][m] = fac2*rhs[c][k1][j][i][m];
  }

  //---------------------------------------------------------------------
  // do the u+c and the u-c factors               
  //---------------------------------------------------------------------
  for (k = kstart; k <= kend-2; k++) {
    k1 = k  + 1;
    k2 = k  + 2;
    for (m = 3; m < 5; m++) {
      n = (m-2)*5 - 1;
      fac1                  = 1.0/lhs[c][k][j][i][n+3];
      lhs[c][k][j][i][n+4]  = fac1*lhs[c][k][j][i][n+4];
      lhs[c][k][j][i][n+5]  = fac1*lhs[c][k][j][i][n+5];
      rhs[c][k][j][i][m]    = fac1*rhs[c][k][j][i][m];
      lhs[c][k1][j][i][n+3] = lhs[c][k1][j][i][n+3] -
        lhs[c][k1][j][i][n+2]*lhs[c][k][j][i][n+4];
      lhs[c][k1][j][i][n+4] = lhs[c][k1][j][i][n+4] -
        lhs[c][k1][j][i][n+2]*lhs[c][k][j][i][n+5];
      rhs[c][k1][j][i][m]   = rhs[c][k1][j][i][m] -
        lhs[c][k1][j][i][n+2]*rhs[c][k][j][i][m];
      lhs[c][k2][j][i][n+2] = lhs[c][k2][j][i][n+2] -
        lhs[c][k2][j][i][n+1]*lhs[c][k][j][i][n+4];
      lhs[c][k2][j][i][n+3] = lhs[c][k2][j][i][n+3] -
        lhs[c][k2][j][i][n+1]*lhs[c][k][j][i][n+5];
      rhs[c][k2][j][i][m]   = rhs[c][k2][j][i][m] -
        lhs[c][k2][j][i][n+1]*rhs[c][k][j][i][m];
    }
  }

  //---------------------------------------------------------------------
  // And again the last two rows separately
  //---------------------------------------------------------------------
  k  = kend - 1;
  k1 = kend;
  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    fac1                  = 1.0/lhs[c][k][j][i][n+3];
    lhs[c][k][j][i][n+4]  = fac1*lhs[c][k][j][i][n+4];
    lhs[c][k][j][i][n+5]  = fac1*lhs[c][k][j][i][n+5];
    rhs[c][k][j][i][m]    = fac1*rhs[c][k][j][i][m];
    lhs[c][k1][j][i][n+3] = lhs[c][k1][j][i][n+3] -
      lhs[c][k1][j][i][n+2]*lhs[c][k][j][i][n+4];
    lhs[c][k1][j][i][n+4] = lhs[c][k1][j][i][n+4] -
      lhs[c][k1][j][i][n+2]*lhs[c][k][j][i][n+5];
    rhs[c][k1][j][i][m]   = rhs[c][k1][j][i][m] -
      lhs[c][k1][j][i][n+2]*rhs[c][k][j][i][m];
    //-----------------------------------------------------------------
    // Scale the last row immediately (some of this is overkill
    // if this is the last cell)
    //-----------------------------------------------------------------
    fac2                  = 1.0/lhs[c][k1][j][i][n+3];
    lhs[c][k1][j][i][n+4] = fac2*lhs[c][k1][j][i][n+4];
    lhs[c][k1][j][i][n+5] = fac2*lhs[c][k1][j][i][n+5];
    rhs[c][k1][j][i][m]   = fac2*rhs[c][k1][j][i][m];

  }
}

__kernel void z_solve3(__global double* g_rhs,
                       __global double* g_lhs,
                       __global double* g_out_buffer,
                       int c,
                       int start_j, int range_j,
                       int start_i, int range_i,
                       int cell_size_k)
{
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (j >= range_j || i >= range_i) return;

  int p1, p2;
  int k, m, n, kend;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;
  __global double *out_buffer = (__global double *)g_out_buffer;

  p1 = 5*2*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;
  p2 = 5*2*(range_i-start_i)*(range_j-start_j) + 3*2*2*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;

  kend   = cell_size_k-1;
  //---------------------------------------------------------------------
  // create a running pointer for the send buffer  
  //---------------------------------------------------------------------
  n = -1;
  for (k = kend-1; k <= kend; k++) {
    out_buffer[p1+1] = lhs[c][k][j][i][n+4];
    out_buffer[p1+2] = lhs[c][k][j][i][n+5];
    for (m = 0; m < 3; m++) {
      out_buffer[p1+3+m] = rhs[c][k][j][i][m];
    }
    p1 = p1+5;
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    for (k = kend-1; k <= kend; k++) {
      out_buffer[p2+1] = lhs[c][k][j][i][n+4];
      out_buffer[p2+2] = lhs[c][k][j][i][n+5];
      out_buffer[p2+3] = rhs[c][k][j][i][m];
      p2 = p2 + 3;
    }
  }
}

__kernel void z_solve4(__global double* g_rhs,
                       __global double* g_lhs,
                       __global double* g_in_buffer,
                       int c,
                       int start_j, int range_j,
                       int start_i, int range_i,
                       int cell_size_k)
{
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (j >= range_j || i >= range_i) return;

  int p;
  int k, m, n, k1, k2, kstart, kend;
  double sm1, sm2;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;
  __global double *in_buffer = (__global double *)g_in_buffer;

  kstart = 0;
  kend   = cell_size_k-1;

  //---------------------------------------------------------------------
  // unpack the buffer
  //---------------------------------------------------------------------
  p = 2*5*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;
  k  = kend;
  k1 = k - 1;
  for (m = 0; m < 3; m++) {
    sm1 = in_buffer[p+1];
    sm2 = in_buffer[p+2];
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
      lhs[c][k][j][i][3]*sm1 -
      lhs[c][k][j][i][4]*sm2;
    rhs[c][k1][j][i][m] = rhs[c][k1][j][i][m] -
      lhs[c][k1][j][i][3] * rhs[c][k][j][i][m] - 
      lhs[c][k1][j][i][4] * sm1;
    p = p + 2;
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    sm1 = in_buffer[p+1];
    sm2 = in_buffer[p+2];
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
      lhs[c][k][j][i][n+4]*sm1 -
      lhs[c][k][j][i][n+5]*sm2;
    rhs[c][k1][j][i][m] = rhs[c][k1][j][i][m] -
      lhs[c][k1][j][i][n+4] * rhs[c][k][j][i][m] - 
      lhs[c][k1][j][i][n+5] * sm1;
    p = p + 2;
  }

  //---------------------------------------------------------------------
  // Whether or not this is the last processor, we always have
  // to complete the back-substitution 
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // The first three factors
  //---------------------------------------------------------------------
  n = -1;
  for (k = kend-2; k >= kstart; k--) {
    k1 = k  + 1;
    k2 = k  + 2;
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k1][j][i][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k2][j][i][m];
    }
  }

  //---------------------------------------------------------------------
  // And the remaining two
  //---------------------------------------------------------------------
  for (k = kend-2; k >= kstart; k--) {
    k1 = k  + 1;
    k2 = k  + 2;
    for (m = 3; m < 5; m++) {
      n = (m-2)*5 - 1;
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k1][j][i][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k2][j][i][m];
    }
  }
}

__kernel void z_solve5(__global double* g_rhs,
                       __global double* g_lhs,
                       int c,
                       int start_j, int range_j,
                       int start_i, int range_i,
                       int cell_size_k)
{
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (j >= range_j || i >= range_i) return;

  int k, m, n, k1, k2, kstart, kend;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double (*lhs)[KMAX][JMAXP][IMAXP][15] =
    (__global double (*)[KMAX][JMAXP][IMAXP][15])g_lhs;

  kstart = 0;
  kend   = cell_size_k-1;

  //---------------------------------------------------------------------
  // now we know this is the first grid block on the back sweep,
  // so we don't need a message to start the substitution. 
  //---------------------------------------------------------------------

  k  = kend - 1;
  k1 = kend;
  n = -1;
  for (m = 0; m < 3; m++) {
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] -
      lhs[c][k][j][i][n+4]*rhs[c][k1][j][i][m];
  }

  for (m = 3; m < 5; m++) {
    n = (m-2)*5 - 1;
    rhs[c][k][j][i][m] = rhs[c][k][j][i][m] -
      lhs[c][k][j][i][n+4]*rhs[c][k1][j][i][m];
  }

  //---------------------------------------------------------------------
  // Whether or not this is the last processor, we always have
  // to complete the back-substitution 
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // The first three factors
  //---------------------------------------------------------------------
  n = -1;
  for (k = kend-2; k >= kstart; k--) {
    k1 = k  + 1;
    k2 = k  + 2;
    for (m = 0; m < 3; m++) {
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k1][j][i][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k2][j][i][m];
    }
  }

  //---------------------------------------------------------------------
  // And the remaining two
  //---------------------------------------------------------------------
  for (k = kend-2; k >= kstart; k--) {
    k1 = k  + 1;
    k2 = k  + 2;
    for (m = 3; m < 5; m++) {
      n = (m-2)*5 - 1;
      rhs[c][k][j][i][m] = rhs[c][k][j][i][m] - 
        lhs[c][k][j][i][n+4]*rhs[c][k1][j][i][m] -
        lhs[c][k][j][i][n+5]*rhs[c][k2][j][i][m];
    }
  }
}

__kernel void z_solve6(__global double* g_rhs,
                       __global double* g_out_buffer,
                       int c,
                       int start_j, int range_j,
                       int start_i, int range_i)
{
#if Z_SOLVE6_DIM == 2
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (j >= range_j || i >= range_i) return;
#else // Z_SOLVE6_DIM == 1
  int j = get_global_id(1) + start_j;
  int i;
  if (j >= range_j) return;
#endif

  int p;
  int k, k1, m;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double *out_buffer = (__global double *)g_out_buffer;

#if Z_SOLVE6_DIM == 2
  k  = 0;
  k1 = 1;
  p = 2*5*(get_global_id(0)+get_global_id(1)*(range_i-start_i)) - 1;
  for (m = 0; m < 5; m++) {
    out_buffer[p+1] = rhs[c][k][j][i][m];
    out_buffer[p+2] = rhs[c][k1][j][i][m];
    p = p + 2;
  }
 
#else // Z_SOLVE6_DIM == 1
  k  = 0;
  k1 = 1 + 1;
  p = 2*5*(get_global_id(0)*(range_i-start_i)) - 1;
  for (i = start_i; i < range_i; i++) {
    for (m = 0; m < 5; m++) {
      out_buffer[p+1] = rhs[c][k][j][i][m];
      out_buffer[p+2] = rhs[c][k1][j][i][m];
      p = p + 2;
    }
  }
#endif
}

__kernel void add(__global double* g_u,
                  __global double* g_rhs,
                  int c,
                  int start_k, int range_k,
                  int start_j, int range_j,
                  int start_i, int range_i)
{
#if ADD_DIM == 3
  int k = get_global_id(2) + start_k;
  int j = get_global_id(1) + start_j;
  int i = get_global_id(0) + start_i;
  if (k >= range_k || j >= range_j || i >= range_i) return;
#elif ADD_DIM == 2
  int k = get_global_id(1) + start_k;
  int j = get_global_id(0) + start_j;
  int i;
  if (k >= range_k || j >= range_j) return;
#else // ADD_DIM == 1
  int k = get_global_id(0) + start_k;
  int j, i;
  if (k >= range_k) return;
#endif

  int m;

  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;

#if ADD_DIM == 3
  for (m = 0; m < 5; m++) {
    u[c][k+2][j+2][i+2][m] += rhs[c][k][j][i][m];
  }

#elif ADD_DIM == 2
  for (i = start_i; i < range_i; i++) {
    for (m = 0; m < 5; m++) {
      u[c][k+2][j+2][i+2][m] += rhs[c][k][j][i][m];
    }
  }

#else // ADD_DIM == 1
  for (j = start_j; j < range_j; j++) {
    for (i = start_i; i < range_i; i++) {
      for (m = 0; m < 5; m++) {
        u[c][k+2][j+2][i+2][m] += rhs[c][k][j][i][m];
      }
    }
  }
#endif
}

__kernel void error_norm(__global double* g_u,
                         __global double* g_rms,
                         __constant double* g_ce,
                         int c, int k, int kk, int j, int jj,
                         int start_i, int range_i)
{
  int i, ii;
  double xi, eta, zeta, u_exact[5], add;

  __global double (*u)[KMAX+4][JMAXP+4][IMAXP+4][5] =
    (__global double (*)[KMAX+4][JMAXP+4][IMAXP+4][5])g_u;
  __global double *rms = (__global double *)g_rms;

  zeta = (double)(k) * dnzm1;
  eta = (double)(j) * dnym1;
  ii = 2;
  for (i = start_i; i <= range_i; i++) {
    xi = (double)(i) * dnxm1;
    exact_solution(xi, eta, zeta, u_exact, g_ce);

    add = u[c][kk][jj][ii][0]-u_exact[0];
    rms[0] = rms[0] + add*add;
    add = u[c][kk][jj][ii][1]-u_exact[1];
    rms[1] = rms[1] + add*add;
    add = u[c][kk][jj][ii][2]-u_exact[2];
    rms[2] = rms[2] + add*add;
    add = u[c][kk][jj][ii][3]-u_exact[3];
    rms[3] = rms[3] + add*add;
    add = u[c][kk][jj][ii][4]-u_exact[4];
    rms[4] = rms[4] + add*add;
    ii = ii + 1;
  }
}

__kernel void rhs_norm(__global double* g_rhs,
                       __global double* g_rms,
                       int c, int k, int j,
                       int start_i, int range_i)
{
  int i;
  double add;

  __global double (*rhs)[KMAX][JMAXP][IMAXP][5] =
    (__global double (*)[KMAX][JMAXP][IMAXP][5])g_rhs;
  __global double *rms = (__global double *)g_rms;

  for (i = start_i; i < range_i; i++) {
    add = rhs[c][k][j][i][0];
    rms[0] = rms[0] + add*add;
    add = rhs[c][k][j][i][1];
    rms[1] = rms[1] + add*add;
    add = rhs[c][k][j][i][2];
    rms[2] = rms[2] + add*add;
    add = rhs[c][k][j][i][3];
    rms[3] = rms[3] + add*add;
    add = rhs[c][k][j][i][4];
    rms[4] = rms[4] + add*add;
  }
}
