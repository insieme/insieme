//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB FT code for multiple    //
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

#include "ft.h"
 
#define fftblock      FFTBLOCK_DEFAULT
#define fftblockpad   FFTBLOCKPAD_DEFAULT

void cfftz(int is, int m, int n, 
           __global dcomplex *u, __global dcomplex *x, __global dcomplex *y);
void cfftz2(int is, int m, int n, __global dcomplex *u,
            __global dcomplex (*x)[fftblockpad],
            __global dcomplex (*y)[fftblockpad]);
void fftz2(int is, int l, int m, int n,
           __global dcomplex *u, 
           __global dcomplex *x,
           __global dcomplex *y);
void fftz3(int is, int l, int m, int n, int ny, int ny1, 
           __global dcomplex *u,
           __global dcomplex (*x)[fftblockpad],
           __global dcomplex (*y)[fftblockpad]);
double randlc( double *x, double a );
void vranlc(int n, double *x, double a, __global double y[]);
double ipow46(double a, int exp_1, int exp_2);


//---------------------------------------------------------------------
// compute function from local (i,j,k) to ibar^2+jbar^2+kbar^2 
// for time evolution exponent. 
//---------------------------------------------------------------------
//---------------------------------------------------------------------
// this function is very different depending on whether 
// we are in the 0d, 1d or 2d layout. Compute separately. 
// basically we want to convert the fortran indices 
//   1 2 3 4 5 6 7 8 
// to 
//   0 1 2 3 -4 -3 -2 -1
// The following magic formula does the trick:
// mod(i-1+n/2, n) - n/2
//---------------------------------------------------------------------
__kernel void compute_indexmap(__global double *twiddle,
                               __global const int *g_dims,
                               __global const int *xstart,
                               __global const int *ystart,
                               __global const int *zstart,
                               int layout_type)
{
  __global const int (*dims)[3] = (__global const int (*)[3])g_dims;
  int d1 = dims[2][0];
  int d2 = dims[2][1];
  int d3 = dims[2][2];

  int i, j, k, ii, ii2, jj, ij2, kk;
  double ap;

  ap = -4.0 * ALPHA * PI * PI;

#if COMPUTE_IMAP_DIM == 3
  if (layout_type == layout_0D) { // xyz layout
    k = get_global_id(2);
    j = get_global_id(1);
    i = get_global_id(0);
    if (k >= d3 || j >= d2 || i >= d1) return;

    kk = ((k+zstart[2]+NZ/2) % NZ) - NZ/2;
    jj = ((j+ystart[2]+NY/2) % NY) - NY/2;
    ii = ((i+xstart[2]+NX/2) % NX) - NX/2;
    ii2 = ii*ii;
    ij2 = jj*jj+ii2;
    twiddle[k*d2*d1 + j*d1 + i] = exp(ap * (double)(kk*kk+ij2));
  } 
  else if (layout_type == layout_1D) { // zxy layout 
    j = get_global_id(2);
    i = get_global_id(1);
    k = get_global_id(0);
    if (j >= d3 || i >= d2 || k >= d1) return;

    jj = ((j+ystart[2]+NY/2) % NY) - NY/2;
    ii = ((i+xstart[2]+NX/2) % NX) - NX/2;
    ii2 = ii*ii;
    ij2 = jj*jj+ii2;
    kk = ((k+zstart[2]+NZ/2) % NZ) - NZ/2;
    twiddle[j*d2*d1 + i*d1 + k] = exp(ap * (double)(kk*kk+ij2));
  }
  else if (layout_type == layout_2D) { // zxy layout
    j = get_global_id(2);
    i = get_global_id(1);
    k = get_global_id(0);
    if (j >= d3 || i >= d2 || k >= d1) return;

    jj = ((j+ystart[2]+NY/2) % NY) - NY/2;
    ii = ((i+xstart[2]+NX/2) % NX) - NX/2;
    ii2 = ii*ii;
    ij2 = jj*jj+ii2;
    kk = ((k+zstart[2]+NZ/2) % NZ) - NZ/2;
    twiddle[j*d2*d1 + i*d1 + k] = exp(ap * (double)(kk*kk+ij2));
  }

#elif COMPUTE_IMAP_DIM == 2
  if (layout_type == layout_0D) { // xyz layout
    k = get_global_id(1);
    j = get_global_id(0);
    if (k >= d3 || j >= d2) return;

    kk = ((k+zstart[2]+NZ/2) % NZ) - NZ/2;
    jj = ((j+ystart[2]+NY/2) % NY) - NY/2;
    for (i = 0; i < dims[2][0]; i++) {
      ii = ((i+xstart[2]+NX/2) % NX) - NX/2;
      ii2 = ii*ii;
      ij2 = jj*jj+ii2;
      twiddle[k*d2*d1 + j*d1 + i] = exp(ap * (double)(kk*kk+ij2));
    }
  }
  else if (layout_type == layout_1D) { // zxy layout 
    j = get_global_id(1);
    i = get_global_id(0);
    if (j >= d3 || i >= d2) return;

    jj = ((j+ystart[2]+NY/2) % NY) - NY/2;
    ii = ((i+xstart[2]+NX/2) % NX) - NX/2;
    ii2 = ii*ii;
    ij2 = jj*jj+ii2;
    for (k = 0; k < dims[2][0]; k++) {
      kk = ((k+zstart[2]+NZ/2) % NZ) - NZ/2;
      twiddle[j*d2*d1 + i*d1 + k] = exp(ap * (double)(kk*kk+ij2));
    }
  }
  else if (layout_type == layout_2D) { // zxy layout
    j = get_global_id(1);
    i = get_global_id(0);
    if (j >= d3 || i >= d2) return;

    jj = ((j+ystart[2]+NY/2) % NY) - NY/2;
    ii = ((i+xstart[2]+NX/2) % NX) - NX/2;
    ii2 = ii*ii;
    ij2 = jj*jj+ii2;
    for (k = 0; k < dims[2][0]; k++) {
      kk = ((k+zstart[2]+NZ/2) % NZ) - NZ/2;
      twiddle[j*d2*d1 + i*d1 + k] = exp(ap * (double)(kk*kk+ij2));
    }
  }
  
#else //COMPUTE_IMAP_DIM == 1
  if (layout_type == layout_0D) { // xyz layout
    k = get_global_id(0);
    if (k >= d3) return;

    kk = ((k+zstart[2]+NZ/2) % NZ) - NZ/2;
    for (j = 0; j < dims[2][1]; j++) {
      jj = ((j+ystart[2]+NY/2) % NY) - NY/2;
      for (i = 0; i < dims[2][0]; i++) {
        ii = ((i+xstart[2]+NX/2) % NX) - NX/2;
        ii2 = ii*ii;
        ij2 = jj*jj+ii2;
        twiddle[k*d2*d1 + j*d1 + i] = exp(ap * (double)(kk*kk+ij2));
      }
    }
  }
  else if (layout_type == layout_1D) { // zxy layout 
    j = get_global_id(0);
    if (j >= d3) return;

    jj = ((j+ystart[2]+NY/2) % NY) - NY/2;
    for (i = 0; i < dims[2][1]; i++) {
      ii = ((i+xstart[2]+NX/2) % NX) - NX/2;
      ii2 = ii*ii;
      ij2 = jj*jj+ii2;
      for (k = 0; k < dims[2][0]; k++) {
        kk = ((k+zstart[2]+NZ/2) % NZ) - NZ/2;
        twiddle[j*d2*d1 + i*d1 + k] = exp(ap * (double)(kk*kk+ij2));
      }
    }
  }
  else if (layout_type == layout_2D) { // zxy layout
    j = get_global_id(0);
    if (j >= d3) return;

    jj = ((j+ystart[2]+NY/2) % NY) - NY/2;
    for (i = 0; i < dims[2][1]; i++) {
      ii = ((i+xstart[2]+NX/2) % NX) - NX/2;
      ii2 = ii*ii;
      ij2 = jj*jj+ii2;
      for (k = 0; k < dims[2][0]; k++) {
        kk = ((k+zstart[2]+NZ/2) % NZ) - NZ/2;
        twiddle[j*d2*d1 + i*d1 + k] = exp(ap * (double)(kk*kk+ij2));
      }
    }
  }
#endif 
}


//---------------------------------------------------------------------
// Fill in array u0 with initial conditions from 
// random number generator 
//---------------------------------------------------------------------
__kernel void compute_initial_conditions1(__global double *starts,
                                          __global const int *g_dims,
                                          __global const int *ystart,
                                          __global const int *zstart)
{
  __global const int (*dims)[3] = (__global const int (*)[3])g_dims;

  int k;
  double start, an;
  
  start = SEED;
  //---------------------------------------------------------------------
  // Jump to the starting element for our first plane.
  //---------------------------------------------------------------------
  an = ipow46(A, 2*NX, zstart[0]*NY + ystart[0]);
  randlc(&start, an);
  an = ipow46(A, 2*NX, NY);

  starts[0] = start;
  for (k = 1; k < dims[0][2]; k++) {
    randlc(&start, an);
    starts[k] = start;
  }
}

__kernel void compute_initial_conditions2(__global dcomplex *u0,
                                          __global double *starts,
                                          __global const int *g_dims)
{
  __global const int (*dims)[3] = (__global const int (*)[3])g_dims;
  int d1 = dims[0][0];
  int d2 = dims[0][1];
  int d3 = dims[0][2];

  int k;
  double x0;

  k = get_global_id(0);
  if (k >= d3) return;

  //---------------------------------------------------------------------
  // Go through by z planes filling in one square at a time.
  //---------------------------------------------------------------------
  // nz/np2
  x0 = starts[k];
  vranlc(2*NX*d2, &x0, A, (__global double *)&u0[k*d2*d1]);
}


//---------------------------------------------------------------------
// evolve u0 -> u1 (t time steps) in fourier space
//---------------------------------------------------------------------
__kernel void evolve(__global dcomplex *u0,
                     __global dcomplex *u1,
                     __global const double *twiddle,
                     int d1,
                     int d2,
                     int d3)
{
  int i, j, k;

#if EVOLVE_DIM == 3
  k = get_global_id(2);
  j = get_global_id(1);
  i = get_global_id(0);
  if (k >= d3 || j >= d2 || i >= d1) return;

  int idx = k*d2*d1 + j*d1 + i;
  u0[idx] = dcmplx_mul2(u0[idx], twiddle[idx]);
  u1[idx] = u0[idx];

#elif EVOLVE_DIM == 2
  k = get_global_id(1);
  j = get_global_id(0);
  if (k >= d3 || j >= d2) return;

  int kj_idx = k*d2*d1 + j*d1;
  for (i = 0; i < d1; i++) {
    int idx = kj_idx + i;
    u0[idx] = dcmplx_mul2(u0[idx], twiddle[idx]);
    u1[idx] = u0[idx];
  }

#else //EVOLVE_DIM == 1
  k = get_global_id(0);
  if (k >= d3) return;

  int k_idx = k*d2*d1;
  for (j = 0; j < d2; j++) {
    int kj_idx = k_idx + j*d1;
    for (i = 0; i < d1; i++) {
      int idx = kj_idx + i;
      u0[idx] = dcmplx_mul2(u0[idx], twiddle[idx]);
      u1[idx] = u0[idx];
    }
  }
#endif
}


__kernel void checksum(__global dcomplex *u1,
                       __global dcomplex *g_chk,
                       __local dcomplex *l_chk,
                       __global const int *xstart,
                       __global const int *xend,
                       __global const int *ystart,
                       __global const int *yend,
                       __global const int *zstart,
                       __global const int *zend, 
                       int d1,
                       int d2)
{
  int q, r, s;
  int j = get_global_id(0) + 1;
  int lid = get_local_id(0);

  l_chk[lid] = dcmplx(0.0, 0.0);
  if (j <= 1024) {
    q = j % NX;
    if (q >= xstart[0] && q < xend[0]) {
      r = 3*j % NY;
      if (r >= ystart[0] && r < yend[0]) {
        s = 5*j % NZ;
        if (s >= zstart[0] && s < zend[0]) {
          int idx = (s-zstart[0])*d2*d1 + (r-ystart[0])*d1 + (q-xstart[0]);
          l_chk[lid] = u1[idx];
        }
      }
    }
  }

  barrier(CLK_LOCAL_MEM_FENCE);

  if (lid == 0) {
    dcomplex my_chk = l_chk[0];
    for (int j = 1; j < get_local_size(0); j++) {
      my_chk = dcmplx_add(my_chk, l_chk[j]);
    }

    g_chk[get_group_id(0)] = my_chk;
  }
}


__kernel void cffts1(__global dcomplex *x,
                     __global dcomplex *xout,
                     __global dcomplex *u,
                     __global dcomplex *g_ty1,
                     __global dcomplex *g_ty2,
                     int is,
                     int d1,
                     int d2,
                     int d3,
                     int logd1)
{
  int i, j, k;

#if CFFTS1_DIM == 2
  k = get_global_id(1);
  j = get_global_id(0);
  if (k >= d3 || j >= d2) return;

  __global dcomplex *ty1 = &g_ty1[k*d2*d1 + j*d1];
  __global dcomplex *ty2 = &g_ty2[k*d2*d1 + j*d1];

  int kj_idx = k*d2*d1 + j*d1;
  for (i = 0; i < d1; i++) {
    ty1[i] = x[kj_idx + i];
  }

  cfftz(is, logd1, d1, u, ty1, ty2);

  for (i = 0; i < d1; i++) {
    xout[kj_idx + i] = ty1[i];
  }

#else //CFFTS1_DIM == 1

  k = get_global_id(0);
  if (k >= d3) return;

  __global dcomplex (*ty1)[fftblockpad];
  __global dcomplex (*ty2)[fftblockpad];
  int jj;

  ty1 = (__global dcomplex (*)[fftblockpad])&g_ty1[k*d1*fftblockpad];
  ty2 = (__global dcomplex (*)[fftblockpad])&g_ty2[k*d1*fftblockpad];

  for (jj = 0; jj <= d2 - fftblock; jj += fftblock) {
    for (j = 0; j < fftblock; j++) {
      for (i = 0; i < d1; i++) {
        ty1[i][j] = x[k*d2*d1 + (j+jj)*d1 + i];
      }
    }

    cfftz2(is, logd1, d1, u, ty1, ty2);

    for (j = 0; j < fftblock; j++) {
      for (i = 0; i < d1; i++) {
        xout[k*d2*d1 + (j+jj)*d1 + i] = ty1[i][j];
      }
    }
  }
#endif
}


__kernel void cffts2(__global dcomplex *x,
                     __global dcomplex *xout,
                     __global dcomplex *u,
                     __global dcomplex *g_ty1,
                     __global dcomplex *g_ty2,
                     int is,
                     int d1,
                     int d2,
                     int d3,
                     int logd2)
{
  int i, j, k;

#if CFFTS2_DIM == 2
  k = get_global_id(1);
  i = get_global_id(0);
  if (k >= d3 || i >= d1) return;

  __global dcomplex *ty1 = &g_ty1[k*d1*d2 + i*d2];
  __global dcomplex *ty2 = &g_ty2[k*d1*d2 + i*d2];

  int ki_idx = k*d2*d1 + i;
  for (j = 0; j < d2; j++) {
    ty1[j] = x[ki_idx + j*d1];
  }

  cfftz(is, logd2, d2, u, ty1, ty2);

  for (j = 0; j < d2; j++) {
    xout[ki_idx + j*d1] = ty1[j];
  }

#else //CFFTS2_DIM == 1

  k = get_global_id(0);
  if (k >= d3) return;

  __global dcomplex (*ty1)[fftblockpad];
  __global dcomplex (*ty2)[fftblockpad];
  int ii;

  ty1 = (__global dcomplex (*)[fftblockpad])&g_ty1[k*d1*fftblockpad];
  ty2 = (__global dcomplex (*)[fftblockpad])&g_ty2[k*d1*fftblockpad];

  for (ii = 0; ii <= d1 - fftblock; ii += fftblock) {
    for (j = 0; j < d2; j++) {
      for (i = 0; i < fftblock; i++) {
        ty1[j][i] = x[k*d2*d1 + j*d1 + i+ii];
      }
    }

    cfftz2(is, logd2, d2, u, ty1, ty2);

    for (j = 0; j < d2; j++) {
      for (i = 0; i < fftblock; i++) {
        xout[k*d2*d1 + j*d1 + i+ii] = ty1[j][i];
      }
    }
  }

#endif
}


__kernel void cffts3(__global dcomplex *x,
                     __global dcomplex *xout,
                     __global dcomplex *u,
                     __global dcomplex *g_ty1,
                     __global dcomplex *g_ty2,
                     int is,
                     int d1,
                     int d2,
                     int d3,
                     int logd3)
{
  int i, j, k;

#if CFFTS3_DIM == 2
  j = get_global_id(1);
  i = get_global_id(0);
  if (j >= d2 || i >= d1) return;

  __global dcomplex *ty1 = &g_ty1[j*d1*d3 + i*d3];
  __global dcomplex *ty2 = &g_ty2[j*d1*d3 + i*d3];

  int ji_idx = j*d1 + i;
  for (k = 0; k < d3; k++) {
    ty1[k] = x[k*d2*d1 + ji_idx];
  }

  cfftz(is, logd3, d3, u, ty1, ty2);

  for (k = 0; k < d3; k++) {
    xout[k*d2*d1 + ji_idx] = ty1[k];
  }

#else //CFFTS3_DIM == 1

  j = get_global_id(0);
  if (j >= d2) return;

  __global dcomplex (*ty1)[fftblockpad];
  __global dcomplex (*ty2)[fftblockpad];
  int ii;

  ty1 = (__global dcomplex (*)[fftblockpad])&g_ty1[j*d1*fftblockpad];
  ty2 = (__global dcomplex (*)[fftblockpad])&g_ty2[j*d1*fftblockpad];

  for (ii = 0; ii <= d1 - fftblock; ii += fftblock) {
    for (k = 0; k < d3; k++) {
      for (i = 0; i < fftblock; i++) {
        ty1[k][i] = x[k*d2*d1 + j*d1 + i+ii];
      }
    }

    cfftz2(is, logd3, d3, u, ty1, ty2);

    for (k = 0; k < d3; k++) {
      for (i = 0; i < fftblock; i++) {
        xout[k*d2*d1 + j*d1 + i+ii] = ty1[k][i];
      }
    }
  }
#endif
}


//---------------------------------------------------------------------
// Computes NY N-point complex-to-complex FFTs of X using an algorithm due
// to Swarztrauber.  X is both the input and the output array, while Y is a 
// scratch array.  It is assumed that N = 2^M.  Before calling CFFTZ to 
// perform FFTs, the array U must be initialized by calling CFFTZ with IS 
// set to 0 and M set to MX, where MX is the maximum value of M for any 
// subsequent call.
//---------------------------------------------------------------------
void cfftz(int is, int m, int n, 
           __global dcomplex *u, __global dcomplex *x, __global dcomplex *y)
{
//  int i, j, l, mx;
  int j, l;

  //---------------------------------------------------------------------
  // Check if input parameters are invalid.
  //---------------------------------------------------------------------
//  mx = (int)(u[0].real);
//  if ((is != 1 && is != -1) || m < 1 || m > mx) {
//    printf("CFFTZ: Either U has not been initialized, or else\n"    
//           "one of the input parameters is invalid%5d%5d%5d\n", is, m, mx);
//    exit(EXIT_FAILURE); 
//  }

  //---------------------------------------------------------------------
  // Perform one variant of the Stockham FFT.
  //---------------------------------------------------------------------
  for (l = 1; l <= m; l += 2) {
    fftz2(is, l, m, n, u, x, y);
    if (l == m) {
      //-----------------------------------------------------------------
      // Copy Y to X.
      //-----------------------------------------------------------------
      for (j = 0; j < n; j++) {
        x[j] = y[j];
      }
      return;
    }
    fftz2(is, l + 1, m, n, u, y, x);
  }
}

void cfftz2(int is, int m, int n, __global dcomplex *u,
            __global dcomplex (*x)[fftblockpad],
            __global dcomplex (*y)[fftblockpad])
{
//  int i, j, l, mx;
  int i, j, l;

  //---------------------------------------------------------------------
  // Check if input parameters are invalid.
  //---------------------------------------------------------------------
//  mx = (int)(u[0].real);
//  if ((is != 1 && is != -1) || m < 1 || m > mx) {
//    printf("CFFTZ: Either U has not been initialized, or else\n"    
//           "one of the input parameters is invalid%5d%5d%5d\n", is, m, mx);
//    exit(EXIT_FAILURE); 
//  }

  //---------------------------------------------------------------------
  // Perform one variant of the Stockham FFT.
  //---------------------------------------------------------------------
  for (l = 1; l <= m; l += 2) {
    fftz3(is, l, m, n, fftblock, fftblockpad, u, x, y);
    if (l == m) {
      //-----------------------------------------------------------------
      // Copy Y to X.
      //-----------------------------------------------------------------
      for (j = 0; j < n; j++) {
        for (i = 0; i < fftblock; i++) {
          x[j][i] = y[j][i];
        }
      }
      return;
    }
    fftz3(is, l + 1, m, n, fftblock, fftblockpad, u, y, x);
  }
}


//---------------------------------------------------------------------
// Performs the L-th iteration of the second variant of the Stockham FFT.
//---------------------------------------------------------------------
void fftz2(int is, int l, int m, int n,
           __global dcomplex *u, __global dcomplex *x, __global dcomplex *y)
{
//  int k, n1, li, lj, lk, ku, i, j, i11, i12, i21, i22;
  int k, n1, li, lj, lk, ku, i, i11, i12, i21, i22;
  dcomplex u1, x11, x21, tmp;

  //---------------------------------------------------------------------
  // Set initial parameters.
  //---------------------------------------------------------------------
  n1 = n / 2;
  lk = 1 << (l - 1);
  li = 1 << (m - l);
  lj = 2 * lk;
  ku = li;

  for (i = 0; i <= li - 1; i++) {
    i11 = i * lk;
    i12 = i11 + n1;
    i21 = i * lj;
    i22 = i21 + lk;
    if (is >= 1) {
      u1 = u[ku+i];
    } else {
      u1 = dconjg(u[ku+i]);
    }

    //---------------------------------------------------------------------
    // This loop is vectorizable.
    //---------------------------------------------------------------------
    for (k = 0; k <= lk - 1; k++) {
      x11 = x[i11+k];
      x21 = x[i12+k];
      y[i21+k] = dcmplx_add(x11, x21);
      tmp = dcmplx_sub(x11, x21);
      y[i22+k] = dcmplx_mul(u1, tmp);
    }
  }
}

void fftz3(int is, int l, int m, int n, int ny, int ny1, 
           __global dcomplex *u,
           __global dcomplex (*x)[fftblockpad],
           __global dcomplex (*y)[fftblockpad])
{
  int k, n1, li, lj, lk, ku, i, j, i11, i12, i21, i22;
  dcomplex u1, x11, x21, tmp;

  //---------------------------------------------------------------------
  // Set initial parameters.
  //---------------------------------------------------------------------
  n1 = n / 2;
  lk = 1 << (l - 1);
  li = 1 << (m - l);
  lj = 2 * lk;
  ku = li;

  for (i = 0; i <= li - 1; i++) {
    i11 = i * lk;
    i12 = i11 + n1;
    i21 = i * lj;
    i22 = i21 + lk;
    if (is >= 1) {
      u1 = u[ku+i];
    } else {
      u1 = dconjg(u[ku+i]);
    }

    //---------------------------------------------------------------------
    // This loop is vectorizable.
    //---------------------------------------------------------------------
    for (k = 0; k <= lk - 1; k++) {
      for (j = 0; j < ny; j++) {
        x11 = x[i11+k][j];
        x21 = x[i12+k][j];
        y[i21+k][j] = dcmplx_add(x11, x21);
        tmp = dcmplx_sub(x11, x21);
        y[i22+k][j] = dcmplx_mul(u1, tmp);
      }
    }
  }
}


__kernel void transpose2_local1(__global dcomplex *xin,
                                __global dcomplex *xout,
                                int n1,
                                int n2)
{
  int i, j;

#if TRANSPOSE2_LOCAL1_DIM == 2
  j = get_global_id(1);
  i = get_global_id(0);
  if (j >= n2 || i >= n1) return;

  xout[i*n2 + j] = xin[j*n1 + i];

#else //TRANSPOSE2_LOCAL1_DIM == 1
  j = get_global_id(0);
  if (j >= n2) return;

  for (i = 0; i < n1; i++) {
    xout[i*n2 + j] = xin[j*n1 + i];
  }
#endif
}


__kernel void transpose2_local2(__global dcomplex *xin,
                                __global dcomplex *xout,
                                int n1,
                                int n2)
{
  int i, j;

#if TRANSPOSE2_LOCAL2_DIM == 2
  i = get_global_id(1);
  j = get_global_id(0);
  if (i >= n1 || j >= n2) return;

  xout[i*n2 + j] = xin[j*n1 + i];

#else //TRANSPOSE2_LOCAL2_DIM == 1
  i = get_global_id(0);
  if (i >= n1) return;

  for (j = 0; j < n2; j++) {
    xout[i*n2 + j] = xin[j*n1 + i];
  }
#endif
}


__kernel void transpose2_local3(__global dcomplex *xin,
                                __global dcomplex *xout,
                                int n1,
                                int n2)
{
  dcomplex z[TRANSBLOCK][TRANSBLOCKPAD];
  int i, j, ii, jj;

#if TRANSPOSE2_LOCAL3_DIM == 2
  j = get_global_id(1) * TRANSBLOCK;
  i = get_global_id(0) * TRANSBLOCK;
  if (j >= n2 || i >= n1) return;

  for (jj = 0; jj < TRANSBLOCK; jj++) {
    for (ii = 0; ii < TRANSBLOCK; ii++) {
      z[ii][jj] = xin[(j+jj)*n1 + (i+ii)];
    }
  }

  for (ii = 0; ii < TRANSBLOCK; ii++) {
    for (jj = 0; jj < TRANSBLOCK; jj++) {
      xout[(i+ii)*n2 + (j+jj)]= z[ii][jj];
    }
  }

#else //TRANSPOSE2_LOCAL3_DIM == 1
  j = get_global_id(0) * TRANSBLOCK;
  if (j >= n2) return;

  for (i = 0; i < n1; i += TRANSBLOCK) {

    //-------------------------------------------------------------------
    // Note: compiler should be able to take j+jj out of inner loop
    //-------------------------------------------------------------------
    for (jj = 0; jj < TRANSBLOCK; jj++) {
      for (ii = 0; ii < TRANSBLOCK; ii++) {
        z[ii][jj] = xin[(j+jj)*n1 + (i+ii)];
      }
    }

    for (ii = 0; ii < TRANSBLOCK; ii++) {
      for (jj = 0; jj < TRANSBLOCK; jj++) {
        xout[(i+ii)*n2 + (j+jj)]= z[ii][jj];
      }
    }

  }
#endif
}


__kernel void transpose2_finish(__global dcomplex *xin,
                                __global dcomplex *xout,
                                int n1,
                                int n2,
                                int np2)
{
  int ioff;
  int i, j, p;

#if TRANSPOSE2_FINISH_DIM == 3
  p = get_global_id(2);
  j = get_global_id(1);
  i = get_global_id(0);
  if (p >= np2 || j >= (n1/np2) || i >= n2) return;

  ioff = p*n2;
  xout[j*n2*np2 + (i+ioff)] = xin[p*(n1/np2)*n2 + j*n2 + i];

#elif TRANSPOSE2_FINISH_DIM == 2
  p = get_global_id(1);
  j = get_global_id(0);
  if (p >= np2 || j >= (n1/np2)) return;

  ioff = p*n2;
  for (i = 0; i < n2; i++) {
    xout[j*n2*np2 + (i+ioff)] = xin[p*(n1/np2)*n2 + j*n2 + i];
  }

#else //TRANSPOSE2_FINISH_DIM == 1
  p = get_global_id(0);
  if (p >= np2) return;

  ioff = p*n2;
  for (j = 0; j < n1/np2; j++) {
    for (i = 0; i < n2; i++) {
      xout[j*n2*np2 + (i+ioff)] = xin[p*(n1/np2)*n2 + j*n2 + i];
    }
  }
#endif
}


__kernel void transpose_x_z_local1(__global dcomplex *xin,
                                   __global dcomplex *xout,
                                   int d1,
                                   int d2,
                                   int d3)
{
  int i, j, k;

#if TRANSPOSE_X_Z_LOCAL1_DIM == 3
  j = get_global_id(2);
  k = get_global_id(1);
  i = get_global_id(0);
  if (j >= d2 || k >= d3 || i >= d1) return;

  xout[i*d2*d3 + j*d3 + k] = xin[k*d2*d1 + j*d1 + i];

#elif TRANSPOSE_X_Z_LOCAL1_DIM == 2
  j = get_global_id(1);
  k = get_global_id(0);
  if (j >= d2 || k >= d3) return;

  for (i = 0; i < d1; i++) {
    xout[i*d2*d3 + j*d3 + k] = xin[k*d2*d1 + j*d1 + i];
  }

#else //TRANSPOSE_X_Z_LOCAL1_DIM == 1
  j = get_global_id(0);
  if (j >= d2) return;

  for (k = 0; k < d3; k++) {
    for (i = 0; i < d1; i++) {
      xout[i*d2*d3 + j*d3 + k] = xin[k*d2*d1 + j*d1 + i];
    }
  }
#endif
}


__kernel void transpose_x_z_local2(__global dcomplex *xin,
                                   __global dcomplex *xout,
                                   int d1,
                                   int d2,
                                   int d3,
                                   int block1,
                                   int block3)
{
  int i, j, k, kk, ii, i1, k1;
  dcomplex buf[MAXDIM][TRANSBLOCKPAD];

#if TRANSPOSE_X_Z_LOCAL2_DIM == 3
  j = get_global_id(2);
  kk = get_global_id(1) * block3;
  ii = get_global_id(0) * block1;
  if (j >= d2 || kk > (d3-block3) || ii > (d1-block1)) return;

  for (k = 0; k < block3; k++) {
    k1 = k + kk;
    for (i = 0; i < block1; i++) {
      buf[i][k] = xin[k1*d2*d1 + j*d1 + (i+ii)];
    }
  }

  for (i = 0; i < block1; i++) {
    i1 = i + ii;
    for (k = 0; k < block3; k++) {
      xout[i1*d2*d3 + j*d3 + (k+kk)] = buf[i][k];
    }
  }

#elif TRANSPOSE_X_Z_LOCAL2_DIM == 2
  j = get_global_id(1);
  kk = get_global_id(0) * block3;
  if (j >= d2 || kk > (d3-block3)) return;

  for (ii = 0; ii <= d1-block1; ii += block1) {

    for (k = 0; k < block3; k++) {
      k1 = k + kk;
      for (i = 0; i < block1; i++) {
        buf[i][k] = xin[k1*d2*d1 + j*d1 + (i+ii)];
      }
    }

    for (i = 0; i < block1; i++) {
      i1 = i + ii;
      for (k = 0; k < block3; k++) {
        xout[i1*d2*d3 + j*d3 + (k+kk)] = buf[i][k];
      }
    }

  }

#else //TRANSPOSE_X_Z_LOCAL2_DIM == 1
  j = get_global_id(0);
  if (j >= d2) return;

  for (kk = 0; kk <= d3-block3; kk += block3) {
    for (ii = 0; ii <= d1-block1; ii += block1) {

      for (k = 0; k < block3; k++) {
        k1 = k + kk;
        for (i = 0; i < block1; i++) {
          buf[i][k] = xin[k1*d2*d1 + j*d1 + (i+ii)];
        }
      }

      for (i = 0; i < block1; i++) {
        i1 = i + ii;
        for (k = 0; k < block3; k++) {
          xout[i1*d2*d3 + j*d3 + (k+kk)] = buf[i][k];
        }
      }

    }
  }
#endif
}


__kernel void transpose_x_z_finish(__global dcomplex *xin,
                                   __global dcomplex *xout,
                                   int d1,
                                   int d2,
                                   int d3,
                                   int np2)
{
  int i, j, k, p, ioff;

#if TRANSPOSE_X_Z_FINISH_DIM == 3
  p = get_global_id(2);
  k = get_global_id(1);
  j = get_global_id(0);
  if (p >= np2 || k >= d3 || j >= d2) return;

  ioff = p*d1/np2;
  int in_idx  = p*d3*d2*(d1/np2) + k*d2*(d1/np2) + j*(d1/np2);
  int out_idx = k*d2*d1 + j*d1;
  for (i = 0; i < d1/np2; i++) {
    xout[out_idx + i+ioff] = xin[in_idx + i];
  }

#elif TRANSPOSE_X_Z_FINISH_DIM == 2
  p = get_global_id(1);
  k = get_global_id(0);
  if (p >= np2 || k >= d3) return;

  ioff = p*d1/np2;
  for (j = 0; j < d2; j++) {
    int in_idx  = p*d3*d2*(d1/np2) + k*d2*(d1/np2) + j*(d1/np2);
    int out_idx = k*d2*d1 + j*d1;
    for (i = 0; i < d1/np2; i++) {
      xout[out_idx + i+ioff] = xin[in_idx + i];
    }
  }

#else //TRANSPOSE_X_Z_FINISH_DIM == 1
  p = get_global_id(0);
  if (p >= np2) return;

  ioff = p*d1/np2;
  for (k = 0; k < d3; k++) {
    for (j = 0; j < d2; j++) {
      int in_idx  = p*d3*d2*(d1/np2) + k*d2*(d1/np2) + j*(d1/np2);
      int out_idx = k*d2*d1 + j*d1;
      for (i = 0; i < d1/np2; i++) {
        xout[out_idx + i+ioff] = xin[in_idx + i];
      }
    }
  }
#endif
}


__kernel void transpose_x_y_local(__global dcomplex *xin,
                                  __global dcomplex *xout,
                                  int d1,
                                  int d2,
                                  int d3)
{
  int i, j, k;

#if TRANSPOSE_X_Y_LOCAL_DIM == 3
  k = get_global_id(2);
  i = get_global_id(1);
  j = get_global_id(0);
  if (k >= d3 || i >= d1 || j >= d2) return;

  xout[i*d3*d2 + k*d2 + j] = xin[k*d2*d1 + j*d1 + i];

#elif TRANSPOSE_X_Y_LOCAL_DIM == 2
  k = get_global_id(1);
  i = get_global_id(0);
  if (k >= d3 || i >= d1) return;

  for (j = 0; j < d2; j++) {
    xout[i*d3*d2 + k*d2 + j] = xin[k*d2*d1 + j*d1 + i];
  }

#else //TRANSPOSE_X_Y_LOCAL_DIM == 1
  k = get_global_id(0);
  if (k >= d3) return;

  for (i = 0; i < d1; i++) {
    for (j = 0; j < d2; j++) {
      xout[i*d3*d2 + k*d2 + j] = xin[k*d2*d1 + j*d1 + i];
    }
  }
#endif
}


__kernel void transpose_x_y_finish(__global dcomplex *xin,
                                   __global dcomplex *xout,
                                   int d1,
                                   int d2,
                                   int d3,
                                   int np1)
{
  int i, j, k, p, ioff;

#if TRANSPOSE_X_Y_FINISH_DIM == 3
  p = get_global_id(2);
  k = get_global_id(1);
  j = get_global_id(0);
  if (p >= np1 || k >= d3 || j >= d2) return;

  ioff = p*d1/np1;
  int in_idx  = p*d2*d3*(d1/np1) + j*d3*(d1/np1) + k*(d1/np1);
  int out_idx = k*d2*d1 + j*d1;
  for (i = 0; i < d1/np1; i++) {
    xout[out_idx + i+ioff] = xin[in_idx + i];
  }

#elif TRANSPOSE_X_Y_FINISH_DIM == 2
  p = get_global_id(1);
  k = get_global_id(0);
  if (p >= np1 || k >= d3) return;

  ioff = p*d1/np1;
  for (j = 0; j < d2; j++) {
    int in_idx  = p*d2*d3*(d1/np1) + j*d3*(d1/np1) + k*(d1/np1);
    int out_idx = k*d2*d1 + j*d1;
    for (i = 0; i < d1/np1; i++) {
      xout[out_idx + i+ioff] = xin[in_idx + i];
    }
  }

#else //TRANSPOSE_X_Y_FINISH_DIM == 1
  p = get_global_id(0);
  if (p >= np1) return;

  ioff = p*d1/np1;
  for (k = 0; k < d3; k++) {
    for (j = 0; j < d2; j++) {
      int in_idx  = p*d2*d3*(d1/np1) + j*d3*(d1/np1) + k*(d1/np1);
      int out_idx = k*d2*d1 + j*d1;
      for (i = 0; i < d1/np1; i++) {
        xout[out_idx + i+ioff] = xin[in_idx + i];
      }
    }
  }
#endif
}


double randlc( double *x, double a )
{
  /*--------------------------------------------------------------------
  This routine returns a uniform pseudorandom double precision number in the
  range (0, 1) by using the linear congruential generator
  
  x_{k+1} = a x_k  (mod 2^46)
  
  where 0 < x_k < 2^46 and 0 < a < 2^46.  This scheme generates 2^44 numbers
  before repeating.  The argument A is the same as 'a' in the above formula,
  and X is the same as x_0.  A and X must be odd double precision integers
  in the range (1, 2^46).  The returned value RANDLC is normalized to be
  between 0 and 1, i.e. RANDLC = 2^(-46) * x_1.  X is updated to contain
  the new seed x_1, so that subsequent calls to RANDLC using the same
  arguments will generate a continuous sequence.
  
  This routine should produce the same results on any computer with at least
  48 mantissa bits in double precision floating point data.  On 64 bit
  systems, double precision should be disabled.
  
  David H. Bailey     October 26, 1990
  --------------------------------------------------------------------*/

  const double r23 = 1.1920928955078125e-07;
  const double r46 = r23 * r23;
  const double t23 = 8.388608e+06;
  const double t46 = t23 * t23;

  double t1, t2, t3, t4, a1, a2, x1, x2, z;
  double r;

  //--------------------------------------------------------------------
  //  Break A into two parts such that A = 2^23 * A1 + A2.
  //--------------------------------------------------------------------
  t1 = r23 * a;
  a1 = (int) t1;
  a2 = a - t23 * a1;

  //--------------------------------------------------------------------
  //  Break X into two parts such that X = 2^23 * X1 + X2, compute
  //  Z = A1 * X2 + A2 * X1  (mod 2^23), and then
  //  X = 2^23 * Z + A2 * X2  (mod 2^46).
  //--------------------------------------------------------------------
  t1 = r23 * (*x);
  x1 = (int) t1;
  x2 = *x - t23 * x1;
  t1 = a1 * x2 + a2 * x1;
  t2 = (int) (r23 * t1);
  z = t1 - t23 * t2;
  t3 = t23 * z + a2 * x2;
  t4 = (int) (r46 * t3);
  *x = t3 - t46 * t4;
  r = r46 * (*x);

  return r;
}


void vranlc(int n, double *x, double a, __global double y[])
{
  /*--------------------------------------------------------------------
   This routine generates N uniform pseudorandom double precision numbers in
   the range (0, 1) by using the linear congruential generator
  
   x_{k+1} = a x_k  (mod 2^46)
  
   where 0 < x_k < 2^46 and 0 < a < 2^46.  This scheme generates 2^44 numbers
   before repeating.  The argument A is the same as 'a' in the above formula,
   and X is the same as x_0.  A and X must be odd double precision integers
   in the range (1, 2^46).  The N results are placed in Y and are normalized
   to be between 0 and 1.  X is updated to contain the new seed, so that
   subsequent calls to VRANLC using the same arguments will generate a
   continuous sequence.  If N is zero, only initialization is performed, and
   the variables X, A and Y are ignored.
  
   This routine is the standard version designed for scalar or RISC systems.
   However, it should produce the same results on any single processor
   computer with at least 48 mantissa bits in double precision floating point
   data.  On 64 bit systems, double precision should be disabled.
  --------------------------------------------------------------------*/

  const double r23 = 1.1920928955078125e-07;
  const double r46 = r23 * r23;
  const double t23 = 8.388608e+06;
  const double t46 = t23 * t23;

  double t1, t2, t3, t4, a1, a2, x1, x2, z;

  int i;

  //--------------------------------------------------------------------
  //  Break A into two parts such that A = 2^23 * A1 + A2.
  //--------------------------------------------------------------------
  t1 = r23 * a;
  a1 = (int) t1;
  a2 = a - t23 * a1;

  //--------------------------------------------------------------------
  //  Generate N results.   This loop is not vectorizable.
  //--------------------------------------------------------------------
  for ( i = 0; i < n; i++ ) {
    //--------------------------------------------------------------------
    //  Break X into two parts such that X = 2^23 * X1 + X2, compute
    //  Z = A1 * X2 + A2 * X1  (mod 2^23), and then
    //  X = 2^23 * Z + A2 * X2  (mod 2^46).
    //--------------------------------------------------------------------
    t1 = r23 * (*x);
    x1 = (int) t1;
    x2 = *x - t23 * x1;
    t1 = a1 * x2 + a2 * x1;
    t2 = (int) (r23 * t1);
    z = t1 - t23 * t2;
    t3 = t23 * z + a2 * x2;
    t4 = (int) (r46 * t3) ;
    *x = t3 - t46 * t4;
    y[i] = r46 * (*x);
  }
}


//---------------------------------------------------------------------
// compute a^exponent mod 2^46
//---------------------------------------------------------------------
double ipow46(double a, int exp_1, int exp_2)
{
  double q, r;
  int n, n2;
  int two_pow;

  //---------------------------------------------------------------------
  // Use
  //   a^n = a^(n/2)*a^(n/2) if n even else
  //   a^n = a*a^(n-1)       if n odd
  //---------------------------------------------------------------------
  if (exp_2 == 0 || exp_1 == 0) return 1.0;
  q = a;
  r = 1;
  n = exp_1;
  two_pow = true;

  while (two_pow) {
    n2 = n / 2;
    if (n2 * 2 == n) {
      randlc(&q, q);
      n = n2;
    } else {
      n = n * exp_2;
      two_pow = false;
    }
  }

  while (n > 1) {
    n2 = n / 2;
    if (n2 * 2 == n) {
      randlc(&q, q);
      n = n2;
    } else {
      randlc(&r, q);
      n = n-1;
    }
  }
  randlc(&r, q);
  return r;
}

