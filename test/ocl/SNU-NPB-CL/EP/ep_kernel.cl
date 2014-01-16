//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB EP code for multiple    //
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

#ifdef USE_CPU
#pragma OPENCL EXTENSION cl_amd_fp64: enable
#else
#pragma OPENCL EXTENSION cl_khr_fp64: enable
#endif

#include "ep.h"

#define CHUNK_SIZE  128

double randlc( double *x, double a );
void vranlc( int n, double *x, double a, double y[] );


__kernel void embar(
    __global double *gq, __global double *gsx, __global double *gsy,
    int k_offset, double an, int g_offset, int chunk)
{
  __local double q[128 * 10];
  __local double sx[128];
  __local double sy[128];
#ifdef USE_CHUNK_SCHEDULE
#if 1
  int    i, j, k, ii, ik, kk, l;
  double t1, t2, t3, t4, x1, x2, temp_t1;

  double x[2*NK];
  double q0, q1, q2, q3, q4, q5, q6, q7, q8, q9;
  q0 = q1 = q2 = q3 = q4 = q5 = q6 = q7 = q8 = q9 = 0.0;

  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int factor = (chunk + gsize - 1) / gsize;
  int k_start = factor * id;
  int k_end = (id != (gsize - 1)) ? (k_start + factor) : chunk;

  double my_sx = 0.0;
  double my_sy = 0.0;

  k_offset = k_offset + g_offset + 1;

  for (k = k_start; k < k_end; k++) {
    //k = k + g_offset;

    kk = k_offset + k; 
    t1 = S;
    t2 = an;

    // Find starting seed t1 for this kk.

    for (i = 1; i <= 100; i++) {
      ik = kk / 2;
      if ((2 * ik) != kk)	t3 = randlc(&t1, t2);
      if (ik == 0) break;
      t3 = randlc(&t2, t2);
      kk = ik;
    }

    vranlc(2 * NK, &t1, A, x);

    for (i = 0; i < NK; i++) {
      x1 = 2.0 * x[2*i] - 1.0;
      x2 = 2.0 * x[2*i+1] - 1.0;
      t1 = x1 * x1 + x2 * x2;
      if (t1 <= 1.0) {
        t2    = sqrt(-2.0 * log(t1) / t1);
        t3    = (x1 * t2);
        t4    = (x2 * t2);
        l     = (int)MAX(fabs(t3), fabs(t4));
        if (l == 0) q0++;
        if (l == 1) q1++;
        if (l == 2) q2++;
        if (l > 2) {
        if (l == 3) q3++;
        if (l == 4) q4++;
        if (l == 5) q5++;
        if (l == 6) q6++;
        if (l == 7) q7++;
        if (l == 8) q8++;
        if (l == 9) q9++;
        }
        my_sx += t3;
        my_sy += t4;
      }
    }
  }

  gq[id*NQ + 0] = q0;
  gq[id*NQ + 1] = q1;
  gq[id*NQ + 2] = q2;
  gq[id*NQ + 3] = q3;
  gq[id*NQ + 4] = q4;
  gq[id*NQ + 5] = q5;
  gq[id*NQ + 6] = q6;
  gq[id*NQ + 7] = q7;
  gq[id*NQ + 8] = q8;
  gq[id*NQ + 9] = q9;

  gsx[id] = my_sx;
  gsy[id] = my_sy;

#else

  int    i, j, k, ii, ik, kk, l;
  double t1, t2, t3, t4, x1, x2, temp_t1;

  double x[2*CHUNK_SIZE];

  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int factor = (chunk + gsize - 1) / gsize;
  int k_start = factor * id;
  int k_end = (id != (gsize - 1)) ? (k_start + factor) : chunk;

  double my_sx = 0.0;
  double my_sy = 0.0;
  for(j = 0; j < NQ; j++) {
    q[j] = 0.0;
  }

  for (k = k_start; k < k_end; k++) {
    k = k + g_offset;

    kk = k_offset + k + 1; 
    t1 = S;
    t2 = an;

    // Find starting seed t1 for this kk.

    for (i = 1; i <= 100; i++) {
      ik = kk / 2;
      if ((2 * ik) != kk)	t3 = randlc(&t1, t2);
      if (ik == 0) break;
      t3 = randlc(&t2, t2);
      kk = ik;
    }

    //--------------------------------------------------------------------
    // Compute Gaussian deviates by acceptance-rejection method and 
    // tally counts in concentri//square annuli.  This loop is not 
    // vectorizable. 
    //--------------------------------------------------------------------

    temp_t1 = t1;

    for (ii = 0; ii < NK; ii = ii + CHUNK_SIZE) {
      // Compute uniform pseudorandom numbers.
      vranlc(2*CHUNK_SIZE, &temp_t1, A, x);

      for (i = 0; i < CHUNK_SIZE; i++) {
        x1 = 2.0 * x[2*i] - 1.0;
        x2 = 2.0 * x[2*i+1] - 1.0;
        t1 = x1 * x1 + x2 * x2;
        if (t1 <= 1.0) {
          t2 = sqrt(-2.0 * log(t1) / t1);
          t3 = (x1 * t2);
          t4 = (x2 * t2);
          l  = (int)MAX(fabs(t3), fabs(t4));
          q[l] += 1.0;
          my_sx += t3;
          my_sy += t4;
        }
      }
    }
  }

  for (j = 0; j < NQ; j++) {
    gq[id*NQ + j] = q[j];
  }
  gsx[id] = my_sx;
  gsy[id] = my_sy;
#endif

#else
  int    i, j, ii, ik, kk, l;
  double t1, t2, t3, t4, x1, x2, temp_t1;

  double x[2*CHUNK_SIZE];

  int k = get_global_id(0);
  int lid = get_local_id(0);

  double my_sx = 0.0;
  double my_sy = 0.0;
  for(j = 0; j < NQ; j++) {
    q[lid*NQ + j] = 0.0;
  }

  if (k < chunk) {
    k = k + g_offset;

    kk = k_offset + k + 1; 
    t1 = S;
    t2 = an;

    // Find starting seed t1 for this kk.

    for (i = 1; i <= 100; i++) {
      ik = kk / 2;
      if ((2 * ik) != kk)	t3 = randlc(&t1, t2);
      if (ik == 0) break;
      t3 = randlc(&t2, t2);
      kk = ik;
    }

    //--------------------------------------------------------------------
    // Compute Gaussian deviates by acceptance-rejection method and 
    // tally counts in concentri//square annuli.  This loop is not 
    // vectorizable. 
    //--------------------------------------------------------------------

    temp_t1 = t1;

    for (ii = 0; ii < NK; ii = ii + CHUNK_SIZE) {
      // Compute uniform pseudorandom numbers.
      vranlc(2*CHUNK_SIZE, &temp_t1, A, x);

      for (i = 0; i < CHUNK_SIZE; i++) {
        x1 = 2.0 * x[2*i] - 1.0;
        x2 = 2.0 * x[2*i+1] - 1.0;
        t1 = x1 * x1 + x2 * x2;
        if (t1 <= 1.0) {
          t2 = sqrt(-2.0 * log(t1) / t1);
          t3 = (x1 * t2);
          t4 = (x2 * t2);
          l  = (int)MAX(fabs(t3), fabs(t4));
          q[lid*NQ + l] += 1.0;
          my_sx += t3;
          my_sy += t4;
        }
      }
    }
  }
  sx[lid] = my_sx;
  sy[lid] = my_sy;

  barrier(CLK_LOCAL_MEM_FENCE);

  if (lid == 0) {
    for (i = 1; i < get_local_size(0); i++) {
      for (j = 0; j < NQ; j++) {
        q[j] += q[i*NQ + j];	        
      }
      sx[0] += sx[i];
      sy[0] += sy[i];
    }

    int wgid = get_group_id(0);
    for (j = 0; j < NQ; j++) {
      gq[wgid*NQ + j] = q[j];
    }
    gsx[wgid] = sx[0];
    gsy[wgid] = sy[0];
  }
#endif
}


double randlc( double *x, double a )
{
  //--------------------------------------------------------------------
  //
  //  This routine returns a uniform pseudorandom double precision number in the
  //  range (0, 1) by using the linear congruential generator
  //
  //  x_{k+1} = a x_k  (mod 2^46)
  //
  //  where 0 < x_k < 2^46 and 0 < a < 2^46.  This scheme generates 2^44 numbers
  //  before repeating.  The argument A is the same as 'a' in the above formula,
  //  and X is the same as x_0.  A and X must be odd double precision integers
  //  in the range (1, 2^46).  The returned value RANDLC is normalized to be
  //  between 0 and 1, i.e. RANDLC = 2^(-46) * x_1.  X is updated to contain
  //  the new seed x_1, so that subsequent calls to RANDLC using the same
  //  arguments will generate a continuous sequence.
  //
  //  This routine should produce the same results on any computer with at least
  //  48 mantissa bits in double precision floating point data.  On 64 bit
  //  systems, double precision should be disabled.
  //
  //  David H. Bailey     October 26, 1990
  //
  //--------------------------------------------------------------------

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


void vranlc( int n, double *x, double a, double y[] )
{
  //--------------------------------------------------------------------
  //
  //  This routine generates N uniform pseudorandom double precision numbers in
  //  the range (0, 1) by using the linear congruential generator
  //
  //  x_{k+1} = a x_k  (mod 2^46)
  //
  //  where 0 < x_k < 2^46 and 0 < a < 2^46.  This scheme generates 2^44 numbers
  //  before repeating.  The argument A is the same as 'a' in the above formula,
  //  and X is the same as x_0.  A and X must be odd double precision integers
  //  in the range (1, 2^46).  The N results are placed in Y and are normalized
  //  to be between 0 and 1.  X is updated to contain the new seed, so that
  //  subsequent calls to VRANLC using the same arguments will generate a
  //  continuous sequence.  If N is zero, only initialization is performed, and
  //  the variables X, A and Y are ignored.
  //
  //  This routine is the standard version designed for scalar or RISC systems.
  //  However, it should produce the same results on any single processor
  //  computer with at least 48 mantissa bits in double precision floating point
  //  data.  On 64 bit systems, double precision should be disabled.
  //
  //--------------------------------------------------------------------

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

  return;
}
