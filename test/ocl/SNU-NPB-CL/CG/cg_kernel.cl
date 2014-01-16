//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB CG code for multiple    //
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

#ifdef cl_khr_fp64
#pragma OPENCL EXTENSION cl_khr_fp64: enable
#else
#pragma OPENCL EXTENSION cl_amd_fp64: enable
#pragma OPENCL EXTENSION cl_amd_printf: enable
#endif // cl_khr_fp64

#define NONZER 15

#define TRUE    1
#define FALSE   0

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


//---------------------------------------------------------------------
// scale a double precision number x in (0,1) by a power of 2 and chop it
//---------------------------------------------------------------------
int icnvrt(double x, int ipwr2)
{
  return (int)(ipwr2 * x);
}


//---------------------------------------------------------------------
// set ith element of sparse vector (v, iv) with
// nzv nonzeros to val
//---------------------------------------------------------------------
void vecset_local(int n, double v[], int iv[], int *nzv, int i, double val)
{
  int k;
  int set;

  set = false;
  for (k = 1; k <= *nzv; k++) {
    if (iv[k] == i) {
      v[k] = val;
      set  = true;
    }
  }
  if (set == false) {
    *nzv     = *nzv + 1;
    v[*nzv]  = val;
    iv[*nzv] = i;
  }
}


void sprnvc_local(int n, int nz, int nn1, double v[], int iv[],
                  double *tran, double amult)
{
  int nzv, ii, i;
  double vecelt, vecloc;

  nzv = 0;

  while (nzv < nz) {
    vecelt = randlc(tran, amult);

    //---------------------------------------------------------------------
    // generate an integer between 1 and n in a portable manner
    //---------------------------------------------------------------------
    vecloc = randlc(tran, amult);
    i = icnvrt(vecloc, nn1) + 1;
    if (i > n) continue;

    //---------------------------------------------------------------------
    // was this integer generated already?
    //---------------------------------------------------------------------
    int was_gen = false;
    for (ii = 1; ii <= nzv; ii++) {
      if (iv[ii] == i) {
        was_gen = true;
        break;
      }
    }
    if (was_gen) continue;
    nzv = nzv + 1;
    v[nzv] = vecelt;
    iv[nzv] = i;
  }
}


//////////////////////////////////////////////////////////////////////////
// Kernels for makea()
//////////////////////////////////////////////////////////////////////////
__kernel void makea_1(int n,
                      __global int *arow,
                      __global int *acol,
                      __global double *aelt,
                      double tran,
                      double amult)
{
  int iouter, ivelt, nzv, nn1;
  int ivc[NONZER+1+1];
  double vc[NONZER+1+1];
  double temp_tran = tran;

  int myid, num_threads, ilow, ihigh;
  int work;

  //---------------------------------------------------------------------
  // nn1 is the smallest power of two not less than n
  //---------------------------------------------------------------------
  nn1 = 1;
  do {
    nn1 = 2 * nn1;
  } while (nn1 < n);

  //---------------------------------------------------------------------
  // Generate nonzero positions and save for the use in sparse.
  //---------------------------------------------------------------------
  num_threads = get_global_size(0);
  myid = get_global_id(0);

  work  = (n + num_threads - 1)/num_threads;
  ilow  = work * myid + 1;
  ihigh = ilow + work - 1;
  if (ihigh > n) ihigh = n;

  for (iouter = 1; iouter <= ihigh; iouter++) {
    nzv = NONZER;
    sprnvc_local(n, nzv, nn1, vc, ivc, &temp_tran, amult);
    if (iouter >= ilow) {
      vecset_local(n, vc, ivc, &nzv, iouter, 0.5);
      arow[iouter] = nzv;
      for (ivelt = 1; ivelt <= nzv; ivelt++) {
        acol[(iouter-1) * (NONZER+1+1) + ivelt] = ivc[ivelt];
        aelt[(iouter-1) * (NONZER+1+1) + ivelt] = vc[ivelt];
      }
    }
  }
}


__kernel void makea_2(__global int *rowstr,//w - NA+1+1
                      __global int *arow,
                      __global int *acol,
                      __global int *last_n,//w
                      int firstrow,
                      int lastrow,
                      int n)
{
  //Tuan:
  // calculate ilow and ihigh, which are separated from the previous kernel
  // launch
  int work, ihigh, ilow, myid, num_threads;
  myid = get_global_id(0);
  num_threads = get_global_size(0);
  work  = (n + num_threads - 1)/num_threads;
  ilow  = work * myid + 1;
  ihigh = ilow + work - 1;
  if (ihigh > n) ihigh = n;

  //Begin of real calculation

  int i, j, j1, j2, nza;

  j1 = ilow + 1;
  j2 = ihigh + 1;

  for (j = j1; j <= j2; j++) {
    rowstr[j] = 0;
  }

  for (i = 1; i <= n; i++) {
    for (nza = 1; nza <= arow[i]; nza++) {
      j = acol[(i-1) * (NONZER+1+1) + nza];//Tuan
      if (j >= ilow && j <= ihigh) {
        j = j + 1;
        rowstr[j] = rowstr[j] + arow[i];
      }
    }
  }

  if (myid == 0) {
    rowstr[1] = 1;
    j1 = 1;
  }
  for (j = j1+1; j <= j2; j++) {
    rowstr[j] = rowstr[j] + rowstr[j-1];
  }
  if (myid < num_threads) last_n[myid] = rowstr[j2];
}


__kernel void makea_3(__global int *rowstr,//w - NA+1+1
                      __global int *last_n,
                      int firstrow,
                      int lastrow,
                      int n)
{
  //Tuan: calculate ilow and ihigh, which are separated from the previous kernel launch
  int work, ihigh, ilow, myid, num_threads;
  myid = get_global_id(0);
  num_threads = get_global_size(0);
  work  = (n + num_threads - 1)/num_threads;
  ilow  = work * myid + 1;
  ihigh = ilow + work - 1;
  if (ihigh > n) ihigh = n;

  //Calculations on the indexing values from makea_2

  int i, j, j1, j2, nzrow;

  j1 = ilow + 1;
  j2 = ihigh + 1;

  if (myid == 0) {
    j1 = 1;
  }

  //Begin of real calculation

  nzrow = 0;
  if (myid < num_threads) {
    for (i = 0; i <= myid-1; i++) {
      nzrow = nzrow + last_n[i];
    }
  }
  if (nzrow > 0) {
    for (j = j1; j <= j2; j++) {
      rowstr[j] = rowstr[j] + nzrow;
    }
  }
}



__kernel void makea_4(__global double *v,//w NZ+1
                      __global int *iv,//w NZ+1+NA+1
                      __global int *rowstr,
                      __global int *arow,
                      __global int *acol,
                      __global double *aelt,
                      int firstrow,
                      int lastrow,
                      int n,
                      int nz,
                      double rcond,
                      double shift)
{
  //Tuan: calculate ilow and ihigh, which are separated from the previous kernel launch
  int work, ihigh, ilow, myid, num_threads;
  myid = get_global_id(0);
  num_threads = get_global_size(0);
  work  = (n + num_threads - 1)/num_threads;
  ilow  = work * myid + 1;
  ihigh = ilow + work - 1;
  if (ihigh > n) ihigh = n;

  //Calculations on the indexing values from makea_2

  int nrows;

  int i, j, nza, k, kk, nzrow, jcol;
  double size, scale, ratio, va;

  nrows = lastrow - firstrow + 1;
  //  j1 = ilow + 1;
  //  j2 = ihigh + 1;
  //
  //  if (myid == 0) {
  //    j1 = 1;
  //  }

  //Prepare for input of kernen_makea_4
  __global int *nzloc = &iv[nz];
  //__global int *nzloc = iv + nz;
  //Begin of real calculation

  nza = rowstr[nrows+1] - 1;


  //  if (nza > nz) //ERROR

  //---------------------------------------------------------------------
  // ... preload data pages
  //---------------------------------------------------------------------
  for (j = ilow; j <= ihigh; j++) {
    for (k = rowstr[j]; k <= rowstr[j+1]-1; k++) {
      v[k] = 0.0;
      iv[k] = 0;
    }
    nzloc[j] = 0;
  }

  //---------------------------------------------------------------------
  // ... generate actual values by summing duplicates
  //---------------------------------------------------------------------
  size = 1.0;
  ratio = pow(rcond, (1.0 / (double)(n)));

  for (i = 1; i <= n; i++) {
    for (nza = 1; nza <= arow[i]; nza++) {
      //j = acol[i-1][nza];
      j = acol[(i-1) * (NONZER+1+1)+nza];//Tuan

      if (j < ilow || j > ihigh) continue;

      scale = size * aelt[(i-1) * (NONZER+1+1) + nza];
      for (nzrow = 1; nzrow <= arow[i]; nzrow++) {
        jcol = acol[(i-1) * (NONZER+1+1) + nzrow];
        va = aelt[(i-1) * (NONZER+1+1) + nzrow] * scale;

        //--------------------------------------------------------------------
        // ... add the identity * rcond to the generated matrix to bound
        //     the smallest eigenvalue from below by rcond
        //--------------------------------------------------------------------
        if (jcol == j && j == i) {
          va = va + rcond - shift;
        }

        for (k = rowstr[j]; k <= rowstr[j+1]-1; k++) {
          if (iv[k] > jcol) {
            //----------------------------------------------------------------
            // ... insert colidx here orderly
            //----------------------------------------------------------------
            for (kk = rowstr[j+1]-2; kk >= k; kk--) {
              if (iv[kk] > 0) {
                v[kk+1]  = v[kk];
                iv[kk+1] = iv[kk];
              }
            }
            iv[k] = jcol;
            v[k]  = 0.0;
            break;
          } else if (iv[k] == 0) {
            iv[k] = jcol;
            break;
          } else if (iv[k] == jcol) {
            //--------------------------------------------------------------
            // ... mark the duplicated entry
            //--------------------------------------------------------------
            nzloc[j] = nzloc[j] + 1;
            break;
          }
        }

        v[k] = v[k] + va;
      }
    }
    size = size * ratio;
  }
}


__kernel void makea_5(__global int *iv,//w NZ+1+NA+1
                      __global int *last_n,
                      int firstrow,
                      int lastrow,
                      int n,
                      int nz)
{
  //Tuan: calculate ilow and ihigh, which are separated from the previous kernel launch
  int work, ihigh, ilow, myid, num_threads;
  myid = get_global_id(0);
  num_threads = get_global_size(0);
  work  = (n + num_threads - 1)/num_threads;
  ilow  = work * myid + 1;
  ihigh = ilow + work - 1;
  if (ihigh > n) ihigh = n;

  //Calculations on the indexing values from makea_2
  int j;

  //Prepare for input of makea_5
  __global int *nzloc = &iv[nz];

  //Begin of real calculation
  for (j = ilow+1; j <= ihigh; j++) {
    nzloc[j] = nzloc[j] + nzloc[j-1];
  }
  //if (myid < num_threads) last_n[myid] = nzloc[ihigh];
  last_n[myid] = nzloc[ihigh];
}


__kernel void makea_6(__global int *iv,//w NZ+1+NA+1
                      __global int *last_n,
                      int firstrow,
                      int lastrow,
                      int n,
                      int nz)
{
  //calculate ilow and ihigh, which are separated from the previous kernel launch
  int work, ihigh, ilow, myid, num_threads;
  myid = get_global_id(0);
  num_threads = get_global_size(0);
  work  = (n + num_threads - 1)/num_threads;
  ilow  = work * myid + 1;
  ihigh = ilow + work - 1;
  if (ihigh > n) ihigh = n;

  //Calculations on the indexing values from makea_2
  int i, j, nzrow;

  //Prepare for input of makea_5
  __global int *nzloc = &iv[nz];

  //Begin of real calculation

  nzrow = 0;
  if (myid < num_threads) {
    for (i = 0; i <= myid-1; i++) {
      nzrow = nzrow + last_n[i];
    }
  }
  if (nzrow > 0) {
    for (j = ilow; j <= ihigh; j++) {
      nzloc[j] = nzloc[j] + nzrow;
    }
  }
}


__kernel void makea_7(__global double* a,
                      __global double *v,//w NZ+1
                      __global int *rowstr,
                      __global int *colidx,
                      __global int *iv,//w NZ+1+NA+1
                      int firstrow,
                      int lastrow,
                      int n,
                      int nz)
{
  //Tuan: calculate ilow and ihigh, which are separated from the previous kernel launch
  int work, ihigh, ilow, myid, num_threads;
  myid = get_global_id(0);
  num_threads = get_global_size(0);
  work  = (n + num_threads - 1)/num_threads;
  ilow  = work * myid + 1;
  ihigh = ilow + work - 1;
  if (ihigh > n) ihigh = n;

  //Calculations on the indexing values from makea_2
  int j, j1, j2, nza, k;

  j1 = ilow + 1;
  j2 = ihigh + 1;

  if (myid == 0) {
    j1 = 1;
  }

  //Nothing to heritate from makea_4
  //Nothing to heritate from makea_5
  //Nothing to heritate from makea_6

  //Prepare for input of makea_5
  __global int *nzloc = &iv[nz];

  //Begin of real calculation

  j = get_global_id(1) + 1;
  {

    if (j > 1) {
      j1 = rowstr[j] - nzloc[j-1];
    } else {
      j1 = 1;
    }

    j2 = rowstr[j+1] - nzloc[j] - 1;

    nza = rowstr[j];
    for (k = j1; k <= j2; k++) {
      a[k] = v[nza];
      colidx[k] = iv[nza];
      nza = nza + 1;
    }
  }
}


//////////////////////////////////////////////////////////////////////////
// Kernels for main()
//////////////////////////////////////////////////////////////////////////
__kernel void main_1(__global int *colidx,
                     __global int *rowstr,
                     const int firstcol,
                     const int length)
{
  int j = get_global_id(0) + 1;
  if (j <= length){
	  int k;
	  for (k = rowstr[j]; k <= rowstr[j+1] - 1; k++) {
		colidx[k] = colidx[k] - firstcol + 1;
	  }
  }
}


__kernel void main_2(__global double *x, const int length)
{
  int j = get_global_id(0) + 1;
  if (j <= length) x[j] = 1.0;
//  if (j == get_global_size(0)) {
//    x[j+1] = 1.0;
//  }
}


__kernel void main_2_1(__global double *q,
                       __global double *z,
                       __global double *r,
                       __global double *p)
{
  int j = get_global_id(0) + 1;
  q[j] = 0.0;
  z[j] = 0.0;
  r[j] = 0;
  p[j] = 0;
}


#ifdef USE_CPU
__kernel void main_3(__global double* buffer1,
                     __global double* buffer2,
                     __local double* scratch1,
                     __local double* scratch2,
                     int length,
                     __global double* result1,
                     __global double* result2)
{
#else
__kernel void main_3(__global double* buffer1,
                     __global double* buffer2,
                     int length,
                     __global double* result1,
                     __global double* result2)
{
  __local double scratch1[2];
  __local double scratch2[2];
#endif // USE_CPU
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

  double norm_temp1 = 0.0;
  double norm_temp2 = 0.0;
  for (int j = j_start; j < j_end; j++) {
    norm_temp1 = norm_temp1 + buffer1[j]*buffer2[j];
    norm_temp2 = norm_temp2 + buffer2[j]*buffer2[j];
  }
  
  result1[id] = norm_temp1;
  result2[id] = norm_temp2;

#else
  int global_index = get_global_id(0);
  double accumulator1 = 0;
  double accumulator2 = 0;
  // Loop sequentially over chunks of input vector
  while (global_index <= length) {
    double element1 = buffer1[global_index];
    double element2 = buffer2[global_index];
    if (global_index > 0)
    {
      accumulator1 += element1 * element2;
      accumulator2 += element2 * element2;
    }
    global_index += get_global_size(0);
  }

  // Perform parallel reduction
  int local_index = get_local_id(0);
  scratch1[local_index] = accumulator1;
  scratch2[local_index] = accumulator2;
  barrier(CLK_LOCAL_MEM_FENCE);
  for(int offset = get_local_size(0) / 2; offset > 0; offset = offset / 2)
  {
    if (local_index < offset) {
      double other1 = scratch1[local_index + offset];
      double mine1 = scratch1[local_index];
      scratch1[local_index] = mine1 + other1;

      double other2 = scratch2[local_index + offset];
      double mine2 = scratch2[local_index];
      scratch2[local_index] = mine2 + other2;
    }
    barrier(CLK_LOCAL_MEM_FENCE);
  }
  if (local_index == 0) {
    result1[get_group_id(0)] = scratch1[0];
    result2[get_group_id(0)] = scratch2[0];
  }
#endif
}


__kernel void main_4(__global double *x,
                     __global double *z,
                     const double norm_temp2,
                     const int length)
{
  int j = get_global_id(0) + 1;
  if (j <= length)
	  x[j] = norm_temp2 * z[j];
}


//////////////////////////////////////////////////////////////////////////
// Kernels for conj_grad()
//////////////////////////////////////////////////////////////////////////
__kernel void conj_grad_1(__global double *q,
                          __global double *z,
                          __global double *r,
                          __global double *x,
                          __global double *p,
                          __global double *w,
                          int length)
{
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

  for (int j = j_start; j < j_end; j++) {
	  q[j] = 0.0;
	  z[j] = 0.0;
	  r[j] = x[j];
	  p[j] = r[j];
	  w[j] = 0;
  }

#else
  int j = get_global_id(0) + 1;
  if (j <= length){
	  q[j] = 0.0;
	  z[j] = 0.0;
	  r[j] = x[j];
	  p[j] = r[j];
	  w[j] = 0;
  }

//  if (j == get_global_size(0)) //this branch can be removed
//  {
//    q[j+1] = 0.0;
//    z[j+1] = 0.0;
//    r[j+1] = x[j+1];
//    p[j+1] = r[j+1];
//    w[j+1] = 0;
//  }
#endif
}


__kernel void conj_grad_2(__global double* buffer,
                          __local double* scratch,
                          const int length,
                          __global double* result)
{
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

  double sum = 0.0;
  for (int j = j_start; j < j_end; j++) {
    sum += buffer[j] * buffer[j];
  }
  result[id] = sum;

#else
  int global_index = get_global_id(0);
  double accumulator = 0;
  // Loop sequentially over chunks of input vector
  while (global_index <= length) {
    double element = buffer[global_index];
    if (global_index > 0)
      accumulator += element * element;
    global_index += get_global_size(0);
  }

  // Perform parallel reduction
  int local_index = get_local_id(0);
  scratch[local_index] = accumulator;
  barrier(CLK_LOCAL_MEM_FENCE);
  for(int offset = get_local_size(0) / 2; offset > 0; offset = offset / 2)
  {
    if (local_index < offset) {
      double other = scratch[local_index + offset];
      double mine = scratch[local_index];
      scratch[local_index] = mine + other;
    }
    barrier(CLK_LOCAL_MEM_FENCE);
  }
  if (local_index == 0) {
    result[get_group_id(0)] = scratch[0];
  }
#endif
}


//q = A.p

__kernel void conj_grad_3(__global int *rowstr,
                          __global double *a,
                          __global double *p,
                          __global int *colidx,
                          __global double *w,
                          const int length)
{
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

//  printf("gsize=%d id=%d chunk=%d j_start=%d j_end=%d\n",
//      gsize, id, chunk, j_start, j_end);

  int j, k;

  for (j = j_start; j < j_end; j++) {
    double sum = 0.0;
    for (k = rowstr[j]; k <= rowstr[j+1]-1; k++)
    {
      sum = sum + a[k]*p[colidx[k]];
    }
    w[j] = sum;
  }

#else
  int j = get_global_id(0) + 1;
  if (j <= length){

	  int k;
	  double sum = 0.0;
	  int row_start = rowstr[j];
	  int row_end = rowstr[j+1]-1;
	  for (k = row_start; k <= row_end; k++)
	  {
		sum = sum + a[k]*p[colidx[k]];
	  }
	  w[j] = sum;
  }
#endif
}

__kernel void conj_grad_4(__global double* buffer1,
                          __global double* buffer2,
                          __local double* scratch,
                          const int length,
                          __global double* result)
{
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

  double sum = 0.0;
  for (int j = j_start; j < j_end; j++) {
    sum += buffer1[j] * buffer2[j];
  }
  result[id] = sum;

#else
  int global_index = get_global_id(0);
  double accumulator = 0;
  // Loop sequentially over chunks of input vector
  while (global_index <= length) {
    double element1 = buffer1[global_index];
    double element2 = buffer2[global_index];
    if (global_index > 0)
      accumulator += element1 * element2;
    global_index += get_global_size(0);
  }

  // Perform parallel reduction
  int local_index = get_local_id(0);
  scratch[local_index] = accumulator;
  barrier(CLK_LOCAL_MEM_FENCE);
  for(int offset = get_local_size(0) / 2; offset > 0; offset = offset / 2)
  {
    if (local_index < offset) {
      double other = scratch[local_index + offset];
      double mine = scratch[local_index];
      scratch[local_index] = mine + other;
    }
    barrier(CLK_LOCAL_MEM_FENCE);
  }
  if (local_index == 0) {
    result[get_group_id(0)] = scratch[0];
  }
#endif
}


__kernel void conj_grad_5(__global double *p,
                          __global double *q,
                          __global double *r,
                          __global double *z,
                          const double alpha,
                          const int length)
{
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

  for (int j = j_start; j < j_end; j++) {
	  z[j] = z[j] + alpha*p[j];
	  r[j] = r[j] - alpha*q[j];
  }

#else
  int j = get_global_id(0) + 1;
  if (j <= length){
	  z[j] = z[j] + alpha*p[j];
	  r[j] = r[j] - alpha*q[j];
  }
#endif
}


__kernel void conj_grad_6(__global double *p,
                          __global double *r,
                          const double beta,
                          const int length)
{
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

  for (int j = j_start; j < j_end; j++) {
	  p[j] = r[j] + beta*p[j];
  }

#else
  int j = get_global_id(0) + 1;
  if (j <= length){
	  p[j] = r[j] + beta*p[j];
  }
#endif
}


__kernel void conj_grad_7(__global int *rowstr,
                          __global double *a,
                          __global double *z,
                          __global int *colidx,
                          __global double *w,
                          const int length)
{
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

  int j, k;

  for (j = j_start; j < j_end; j++) {
    double suml = 0.0;
    for (k = rowstr[j]; k <= rowstr[j+1] - 1; k++) {
      suml = suml + a[k]*z[colidx[k]];
    }
    w[j] = suml;
  }

#else
  int j = get_global_id(0) + 1;
  if (j <= length){
	  int k;
	  double suml = 0.0;
	  for (k = rowstr[j]; k <= rowstr[j+1] - 1; k++) {
		suml = suml + a[k]*z[colidx[k]];
	  }
	  w[j] = suml;
  }
#endif
}


__kernel void conj_grad_8(__global double* buffer1,
                          __global double* buffer2,
                          __local double* scratch,
                          const int length,
                          __global double* result)
{
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

  double sum = 0.0, d;
  for (int j = j_start; j < j_end; j++) {
    d = buffer1[j] - buffer2[j];
    sum += d * d;
  }
  result[id] = sum;

#else
  int global_index = get_global_id(0);
  double accumulator = 0;
  // Loop sequentially over chunks of input vector
  while (global_index <= length) {
    double element1 = buffer1[global_index];
    double element2 = buffer2[global_index];
    if (global_index > 0)
      accumulator += (element1 - element2) * (element1 - element2);
    global_index += get_global_size(0);
  }

  // Perform parallel reduction
  int local_index = get_local_id(0);
  scratch[local_index] = accumulator;
  barrier(CLK_LOCAL_MEM_FENCE);
  for(int offset = get_local_size(0) / 2; offset > 0; offset = offset / 2)
  {
    if (local_index < offset) {
      double other = scratch[local_index + offset];
      double mine = scratch[local_index];
      scratch[local_index] = mine + other;
    }
    barrier(CLK_LOCAL_MEM_FENCE);
  }
  if (local_index == 0) {
    result[get_group_id(0)] = scratch[0];
  }
#endif
}


__kernel void conj_grad_9(__global double* w,
							const int length)
{
#ifdef USE_CHUNK_SCHEDULE
  int gsize = get_global_size(0);
  int id = get_global_id(0);

  int chunk = (length + gsize - 1) / gsize;
  int j_start = chunk * id + 1;
  int j_end = (id != (gsize - 1)) ? (j_start + chunk) : length+1;

  for (int j = j_start; j < j_end; j++) {
		w[j] = 0.0;
  }

#else
	int j = get_global_id(0) + 1;
	if (j <= length){
		w[j] = 0.0;
	}
#endif
}


__kernel void conj_grad_10(__global double* w,
						   __global double* q,
						   const int start,
						   const int end)
{
	int j = get_global_id(0) + start;
	if (j >= start && j <= end){
		w[j] = w[j] + q[j];
	}
}
