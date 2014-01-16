//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB MG code for multiple    //
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

#include "mg.h"

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

  // r23 = pow(0.5, 23.0);
  ////  pow(0.5, 23.0) = 1.1920928955078125e-07
  // r46 = r23 * r23;
  // t23 = pow(2.0, 23.0);
  ////  pow(2.0, 23.0) = 8.388608e+06
  // t46 = t23 * t23;

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

void vranlc( int n, double *x, double a, __global double *y )
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

  // r23 = pow(0.5, 23.0);
  ////  pow(0.5, 23.0) = 1.1920928955078125e-07
  // r46 = r23 * r23;
  // t23 = pow(2.0, 23.0);
  ////  pow(2.0, 23.0) = 8.388608e+06
  // t46 = t23 * t23;

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

		//this line can be moved out of this loop
    y[i] = r46 * (*x);
  }

  return;
}


void bubble(double ten[][2], __global int j1[][2], __global int j2[][2], __global int j3[][2],
                   int m, int ind)
{
  double temp;
  int i, j_temp;

  if (ind == 1) {
    for (i = 0; i < m-1; i++) {
      if (ten[i][ind] > ten[i+1][ind]) {
        temp = ten[i+1][ind];
        ten[i+1][ind] = ten[i][ind];
        ten[i][ind] = temp;

        j_temp = j1[i+1][ind];
        j1[i+1][ind] = j1[i][ind];
        j1[i][ind] = j_temp;

        j_temp = j2[i+1][ind];
        j2[i+1][ind] = j2[i][ind];
        j2[i][ind] = j_temp;

        j_temp = j3[i+1][ind];
        j3[i+1][ind] = j3[i][ind];
        j3[i][ind] = j_temp;
      } else {
        return;
      }
    }
  } else {
    for (i = 0; i < m-1; i++) {
      if (ten[i][ind] < ten[i+1][ind]) {

        temp = ten[i+1][ind];
        ten[i+1][ind] = ten[i][ind];
        ten[i][ind] = temp;

        j_temp = j1[i+1][ind];
        j1[i+1][ind] = j1[i][ind];
        j1[i][ind] = j_temp;

        j_temp = j2[i+1][ind];
        j2[i+1][ind] = j2[i][ind];
        j2[i][ind] = j_temp;

        j_temp = j3[i+1][ind];
        j3[i+1][ind] = j3[i][ind];
        j3[i][ind] = j_temp;
      } else {
        return;
      }
    }
  }
}


#if ZERO3_DIM == 3
__kernel void kernel_zero3(__global double* oz, int n1, int n2, int n3, int offset){
	int i3 = get_global_id(2);
	int i2 = get_global_id(1);
	int i1 = get_global_id(0);

	if( i3 >= n3 || i2 >= n2 || i1 >= n1) return;

	int idx = i3*n2*n1 + i2*n1 + i1 + offset; 

	oz[idx] = 0.0;
}
#elif ZERO3_DIM == 2
__kernel void kernel_zero3(__global double* oz, int n1, int n2, int n3, int offset){
	int i3 = get_global_id(1);
	int i2 = get_global_id(0);

	if( i3 >= n3 || i2 >= n2 ) return;

	int idx = i3*n2*n1 + i2*n1 + offset; 

	for (int i1 = 0; i1 < n1; i1++)
		oz[idx + i1] = 0.0;
}
#else //ZERO3_DIM == 1
__kernel void kernel_zero3(__global double* oz, int n1, int n2, int n3, int offset){
	int i3 = get_global_id(0);

	if( i3 >= n3 ) return;

	int idx_i3 = i3*n2*n1 + offset; 
	int idx;

	for (int i2 = 0; i2 < n2; i2++)
	{
		idx = idx_i3 + i2*n1;
		for (int i1 = 0; i1 < n1; i1++)
			oz[idx + i1] = 0.0;
	}
}
#endif

__kernel void kernel_zran3_task1(__global double* starts, double ai, int e3, double a2)
{
  const double x = 314159265.0;
	double x0 = x;
//  double rdummy;
	int i3;

//	rdummy = randlc(&x0, ai);
	randlc(&x0, ai);

	for (i3 = 1; i3 < e3; i3++) {
		starts[i3] = x0;
//		rdummy = randlc(&x0, a2);
		randlc(&x0, a2);
	}

}
__kernel void kernel_zran3_1(__global double* oz, __global double* starts, int d1,
											 int e2, int e3, double a, double a1, int n1, int n2, int n3)
{
	double xx,x1;
//  double rdummy;
  int i2, i3; 

//  __global double (*z)[n2][n1] = (__global double (*)[n2][n1])oz;

	i3 = get_global_id(0);
	if( i3 >= e3-1 ) return;

	i3 += 1;

	x1 = starts[i3];
	for (i2 = 1; i2 < e2; i2++) {
		xx = x1;
		vranlc(d1, &xx, a, &(oz[i3*n2*n1 + i2*n1 + 1]));
//		rdummy = randlc(&x1, a1);
		randlc(&x1, a1);
	}
}

#define mm 10
__kernel void kernel_zran3_task2( __global int* j1_buf, __global int* j2_buf, 
		__global int* j3_buf, __global double* z, int n1, int n2, int n3)
{
  double ten[mm][2];
	int i;
	int i1, i2, i3;

	__global int (*j1)[2] = (__global int(*)[2])j1_buf;
	__global int (*j2)[2] = (__global int(*)[2])j2_buf;
	__global int (*j3)[2] = (__global int(*)[2])j3_buf;
//  __global double (*z)[n2][n1] = (__global double (*)[n2][n1])oz;

  for (i = 0; i < mm; i++) {
    ten[i][1] = 0.0;
    j1[i][1] = 0;
    j2[i][1] = 0;
    j3[i][1] = 0;
    ten[i][0] = 1.0;
    j1[i][0] = 0;
    j2[i][0] = 0;
    j3[i][0] = 0;
  }

  for (i3 = 1; i3 < n3-1; i3++) {
    __global double *zi3 = z + i3*n2*n1;
    for (i2 = 1; i2 < n2-1; i2++) {
      for (i1 = 1; i1 < n1-1; i1++) {
        if (zi3[i2*n1+i1] > ten[0][1]) {
          ten[0][1] = zi3[i2*n1+i1];
          j1[0][1] = i1;
          j2[0][1] = i2;
          j3[0][1] = i3;
          bubble(ten, j1, j2, j3, mm, 1);
        }
        if (zi3[i2*n1+i1] < ten[0][0]) {
          ten[0][0] = zi3[i2*n1+i1];
          j1[0][0] = i1;
          j2[0][0] = i2;
          j3[0][0] = i3;
          bubble(ten, j1, j2, j3, mm, 0);
				}
			}
		}
	}
}

__kernel void kernel_zran3_task3( __global int* j1_buf, __global int* j2_buf, 
		__global int* j3_buf, __global double* z, int m0, int m1, int n1, int n2, int n3)
{

	int i;
	__global int (*j1)[2] = (__global int(*)[2])j1_buf;
	__global int (*j2)[2] = (__global int(*)[2])j2_buf;
	__global int (*j3)[2] = (__global int(*)[2])j3_buf;
//  __global double (*z)[n2][n1] = (__global double (*)[n2][n1])oz;

  for (i = mm-1; i >= m0; i--) {
    z[j3[i][0]*n2*n1 + j2[i][0]*n1 + j1[i][0]] = -1.0;
  }
  for (i = mm-1; i >= m1; i--) {
    z[j3[i][1]*n2*n1 + j2[i][1]*n1 + j1[i][1]] = +1.0;
  }

}

#if NORM2U3_DIM == 3
__kernel
void kernel_norm2u3( __global double* or_buf,
            __local double* scratch_sum,
            __local double* scratch_max,
            const int n1, const int n2, const int n3,
            __global double *res_sum,
            __global double *res_max,
						int offset)
{
	int i1, i2, i3;
  double s = 0; //s is the accumulator
  double local_max = 0.0;
	double a;
	int my_addr, other_addr, other_inc, bound;

  __global double *r = or_buf + offset;
//  __global double (*r)[n2][n1] = (__global double (*)[n2][n1])o_r;

	i3 = get_global_id(2);
	i2 = get_global_id(1);
	i1 = get_global_id(0);
	if( i3 >= n3-2 || i2 >= n2-2 || i1 >= i1-2 ) return;

	i3 += 1;
	i2 += 1;
	i1 += 1;

	s = s + pow(r[i3*n2*n1 + i2*n1 + i1], 2.0);
	a = fabs(r[i3*n2*n1 + i2*n1 + i1]);
	if (a > local_max) local_max = a;

  // Perform parallel reduction
  int local_index_3 = get_local_id(2);
  int local_index_2 = get_local_id(1);
  int local_index_1 = get_local_id(0);

	my_addr = local_index_3 * get_local_size(1) * get_local_size(0) + local_index_2 * get_local_size(0) + local_index_1;

  scratch_sum[my_addr] = s;
  scratch_max[my_addr] = local_max;
  barrier(CLK_LOCAL_MEM_FENCE);


	bound = local_index_3 * get_local_size(1) * get_local_size(0) + local_index_2 * get_local_size(0) + get_local_size(0);
	for(int offset = get_local_size(0) / 2; offset > 0; offset = offset / 2)
	{
		if (local_index_1 < offset) {
			other_addr = my_addr + offset;
			if( other_addr < bound )
			{
				double other = scratch_sum[other_addr];
				double mine = scratch_sum[my_addr];

				scratch_sum[my_addr] = mine + other;

				other = scratch_max[other_addr];
				mine = scratch_max[my_addr];
				if (mine < other) scratch_max[my_addr] = other;
			}
		}
		barrier(CLK_LOCAL_MEM_FENCE);
	}

	my_addr = local_index_2*get_local_size(0);
	bound = local_index_3 * get_local_size(1) * get_local_size(0) + get_local_size(1) * get_local_size(0);
	for(int offset = get_local_size(1) / 2; offset > 0; offset = offset / 2)
	{
		if( local_index_1 == 0)
		{
			other_inc = offset*get_local_size(0);
			if (local_index_2 < other_inc) {
				other_addr = my_addr + other_inc;
				if( other_addr < bound )
				{
					double other = scratch_sum[other_addr];
					double mine = scratch_sum[my_addr];

					scratch_sum[my_addr] = mine + other;

					other = scratch_max[other_addr];
					mine = scratch_max[my_addr];
					if (mine < other) scratch_max[my_addr] = other;
				}
			}
			barrier(CLK_LOCAL_MEM_FENCE);
		}
	}

	my_addr = local_index_3 * get_local_size(1) * get_local_size(0);
	bound = get_local_size(2)*get_local_size(1)*get_local_size(0);
	for(int offset = get_local_size(1) / 2; offset > 0; offset = offset / 2)
	{
		if( local_index_1 == 0 && local_index_2 == 0)
		{
			other_inc = offset*get_local_size(1)*get_local_size(0);
			if (local_index_2 < other_inc) {
				other_addr = my_addr + other_inc;
				if( other_addr < bound )
				{
					double other = scratch_sum[other_addr];
					double mine = scratch_sum[my_addr];

					scratch_sum[my_addr] = mine + other;

					other = scratch_max[other_addr];
					mine = scratch_max[my_addr];
					if (mine < other) scratch_max[my_addr] = other;
				}
			}
			barrier(CLK_LOCAL_MEM_FENCE);
		}
	}

	if( local_index_1 == 0 && local_index_2 == 0 && local_index_3 == 0)
	{
    res_sum[get_group_id(2)*get_num_groups(1)*get_num_groups(0) + get_group_id(1)*get_num_groups(0) + get_group_id(0)] = scratch_sum[0];
    res_max[get_group_id(2)*get_num_groups(1)*get_num_groups(0) + get_group_id(1)*get_num_groups(0) + get_group_id(0)] = scratch_max[0];
  }
}
#elif NORM2U3_DIM == 2
__kernel
void kernel_norm2u3( __global double* or_buf,
            __local double* scratch_sum,
            __local double* scratch_max,
            const int n1, const int n2, const int n3,
            __global double *res_sum,
            __global double *res_max,
						int offset)
{
	int i1, i2, i3;

  double s = 0; //s is the accumulator
  double local_max = 0.0;
	double a;
	int my_addr, other_addr, other_inc, bound;

  __global double *r = or_buf + offset;

	i3 = get_global_id(1);
	i2 = get_global_id(0);
	if( i3 >= n3-2 || i2 >= n2-2 ) return;

	i3 += 1;
	i2 += 1;

	for (i1 = 1; i1<n1-1; i1++) {
		s = s + pow(r[i3*n2*n1 + i2*n1 + i1], 2.0);
		a = fabs(r[i3*n2*n1 + i2*n1 + i1]);
//	printf("i3(%d) i2(%d) i1(%d) sum = %lf, max = %lf\n",i3,i2,i1,s,a);
		if (a > local_max) local_max = a;
	}

  // Perform parallel reduction
  int local_index_2 = get_local_id(1);
  int local_index_1 = get_local_id(0);
	my_addr = local_index_2*get_local_size(0) + local_index_1;

  scratch_sum[my_addr] = s;
  scratch_max[my_addr] = local_max;

  barrier(CLK_LOCAL_MEM_FENCE);

	bound = local_index_2 * get_local_size(0) + get_local_size(0);
	for(int offset = get_local_size(0) / 2; offset > 0; offset = offset / 2)
	{
		if (local_index_1 < offset) {
			other_addr = my_addr + offset;
			if( other_addr < bound )
			{
				double other = scratch_sum[other_addr];
				double mine = scratch_sum[my_addr];

				scratch_sum[my_addr] = mine + other;

				other = scratch_max[other_addr];
				mine = scratch_max[my_addr];

				if (mine < other) scratch_max[my_addr] = other;
			}
		}
		barrier(CLK_LOCAL_MEM_FENCE);
	}
	my_addr = local_index_2*get_local_size(0);
	bound = get_local_size(1)*get_local_size(0);
	for(int offset = get_local_size(1) / 2; offset > 0; offset = offset / 2)
	{
		if( local_index_1 == 0 )
		{
			other_inc = offset*get_local_size(0);
			if (local_index_2 < other_inc) {
				other_addr = my_addr + other_inc;
				if( other_addr < bound)
				{
					double other = scratch_sum[other_addr];
					double mine = scratch_sum[my_addr];

					scratch_sum[my_addr] = mine + other;

					other = scratch_max[other_addr];
					mine = scratch_max[my_addr];
					if (mine < other) scratch_max[my_addr] = other;
				}
			}
		}
		barrier(CLK_LOCAL_MEM_FENCE);
	}

	if( local_index_1 == 0 && local_index_2 == 0)
	{
		res_sum[get_group_id(1)*get_num_groups(0) + get_group_id(0)] = scratch_sum[0];
    res_max[get_group_id(1)*get_num_groups(0) + get_group_id(0)] = scratch_max[0];
  }
}
#else // NORM2U3_DIM == 1
/*
__kernel
void kernel_norm2u3( __global double* or_buf,
            __local double* scratch_sum,
            __local double* scratch_max,
            const int n1, const int n2, const int n3,
            __global double *res_sum,
            __global double *res_max,
						int offset)
{
	int i1, i2, i3;
  double s = 0; //s is the accumulator
  double local_max = 0.0;
	double a;
	int my_addr, other_addr, bound;

  __global double *r = or_buf + offset;
//  __global double (*r)[n2][n1] = (__global double (*)[n2][n1])o_r;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;

	for (i2 = 1; i2<n2-1; i2++) {
		for (i1 = 1; i1<n1-1; i1++) {
			s = s + pow(r[i3*n2*n1 + i2*n1 + i1], 2.0);
			a = fabs(r[i3*n2*n1 + i2*n1 + i1]);
			if (a > local_max) local_max = a;
		}
	}

  // Perform parallel reduction
  int local_index_1 = get_local_id(0);
	my_addr = local_index_1;
  scratch_sum[my_addr] = s;
  scratch_max[my_addr] = local_max;
  barrier(CLK_LOCAL_MEM_FENCE);

	bound = get_local_size(0);
	for(int offset = get_local_size(0) / 2; offset > 0; offset = offset / 2)
	{
		if (local_index_1 < offset) {
			other_addr = my_addr + offset;
			if( other_addr < bound )
			{
				double other = scratch_sum[other_addr];
				double mine = scratch_sum[my_addr];

				scratch_sum[my_addr] = mine + other;

				other = scratch_max[other_addr];
				mine = scratch_max[my_addr];

				if (mine < other) scratch_max[my_addr] = other;
			}
		}
		barrier(CLK_LOCAL_MEM_FENCE);
	}

  if (local_index_1 == 0) {
    res_sum[get_group_id(0)] = scratch_sum[0];
    res_max[get_group_id(0)] = scratch_max[0];
  }
}
*/
///*
__kernel
void kernel_norm2u3( __global double* r,
            const int n1, const int n2, const int n3,
            __global double *res_sum,
            __global double *res_max,
						int offset)
{
  __local double scratch_sum[1];
  __local double scratch_max[1];
	int i1, i2, i3;
  double s = 0.0; //s is the accumulator
  double local_max = 0.0;
	double a;
	int my_addr, bound;

//  __global double *r = or_buf + offset;
//  __global double (*r)[n2][n1] = (__global double (*)[n2][n1])o_r;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;

	for (i2 = 1; i2<n2-1; i2++) {
		for (i1 = 1; i1<n1-1; i1++) {
			s = s + r[offset+i3*n2*n1 + i2*n1 + i1] * r[offset+i3*n2*n1 + i2*n1 + i1];
			a = r[offset+i3*n2*n1 + i2*n1 + i1];
			if(a < 0)
				a = (-1.0) * a;
			//	printf("i3(%d) i2(%d) i1(%d) sum = %lf, max = %lf\n",i3,i2,i1,s,a);
			if (a > local_max) local_max = a;
		}
	}

  // Perform parallel reduction
  int local_index_1 = get_local_id(0);
	my_addr = local_index_1;
  scratch_sum[my_addr] = s;
  scratch_max[my_addr] = local_max;
  barrier(CLK_LOCAL_MEM_FENCE);

  if (local_index_1 == 0) {
		double s = 0.0;
		double local_max = 0.0;
		bound = get_local_size(0);
		for(i1=0; i1<bound; i1++)
		{
			s += scratch_sum[i1];
			if(local_max < scratch_max[i1] )
				local_max = scratch_max[i1];
		}
    res_sum[get_group_id(0)] = s;
    res_max[get_group_id(0)] = local_max;
  }
}
//*/
#endif

__kernel void kernel_power_task(__global double *out, double a, 
		                 int n1, int is1, int is2, int is3, int ny)
{
  double aj;
	int n2, n3;
  int n1j, n2j, nj;
//  double rdummy;
  double power;

	n2 = is2-2+ny*(is3-2);
	n3 = is1-2;

  power = 1.0;
  aj = a;
  nj = n3;
  n1j = n1;
  n2j = n2;

  while (true) {
    if (n2j > 0) {
      if ((n2j % 2) == 1) nj = nj + n1j;
      n2j = n2j/2;
    } else if (nj == 0) break;
//    if ((nj % 2) == 1) rdummy = randlc(&power, aj);
    if ((nj % 2) == 1) randlc(&power, aj);
//    rdummy = randlc(&aj, aj);
    randlc(&aj, aj);
    nj = nj/2;
  }

	*out = power;
}

#if COMM1P_1_DIM == 1
__kernel void kernel_comm1p_1(__global double *buff_buf)
{
	int dir, buff_id;
	int i = get_global_id(0);
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	if( i>= NM2 ) return;

  dir = -1;
  buff_id = 2 + dir;


	buff[buff_id][i] = 0.0;


  dir = +1;
  buff_id = 2 + dir;

	buff[buff_id][i] = 0.0;

}
#else // COMM1P_1_DIM == 0
__kernel void kernel_comm1p_1(__global double *buff_buf)
{
	int dir, buff_id, i;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

  dir = -1;
  buff_id = 2 + dir;


  for (i = 0; i < NM2; i++) {
    buff[buff_id][i] = 0.0;
  }

  dir = +1;
  buff_id = 2 + dir;

  for (i = 0; i < NM2; i++) {
    buff[buff_id][i] = 0.0;
  }
}
#endif


#if COMM1P_2_DIM == 2
__kernel void kernel_comm1p_2_axis0(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i2, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	if( i3 >= n3-2 || i2 >= n2-2 ) return;

	i3 += 1;
	i2 += 1;

  dir = +1;
	buff_id = 1 + dir;

	buff_len = (i3-1)*(n2-2) + i2-1;

	buff[buff_id][buff_len] = u[i3*n2*n1+i2*n1+n1-2];

  dir = -1;
	buff_id = 1 + dir;

	buff[buff_id][buff_len] = u[i3*n2*n1+i2*n1+1];

}
__kernel void kernel_comm1p_2_axis1(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);
	if( i3 >= n3-2 || i1 >= n1 ) return;

	i3 += 1;

	dir = +1;
	buff_id = 1 + dir;

	buff_len = (i3-1)*n1 + i1;

	buff[buff_id][buff_len]= u[i3*n2*n1+(n2-2)*n1+i1];

	dir = -1;
	buff_id = 1 + dir;

	buff[buff_id][buff_len] = u[i3*n2*n1+1*n1+i1];
}
__kernel void kernel_comm1p_2_axis2(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(1);
	i1 = get_global_id(0);
	if( i2 >= n2 || i1 >= n1 ) return;

	dir = +1;
	buff_id = 1 + dir;

	buff_len = i2*n1 + i1;

	buff[buff_id][buff_len] = u[(n3-2)*n2*n1+i2*n1+i1];

	dir = -1;
	buff_id = 1 + dir;

	buff[buff_id][buff_len] = u[1*n2*n1+i2*n1+i1];

}
#else //COMM1P_2_DIM == 1
__kernel void kernel_comm1p_2_axis0(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;

  dir = +1;
	buff_id = 1 + dir;
	buff_len = (i3-1)*(n2-2);

	for (i2 = 1; i2 < n2-1; i2++) {
		buff[buff_id][buff_len] = u[offset+i3*n2*n1+i2*n1+n1-2];
		buff_len = buff_len + 1;
	}

  dir = -1;
	buff_id = 1 + dir;
	buff_len = (i3-1)*(n2-2);

	for (i2 = 1; i2 < n2-1; i2++) {
		buff[buff_id][buff_len] = u[offset+i3*n2*n1+i2*n1+1];
		buff_len = buff_len + 1;
	}

}
__kernel void kernel_comm1p_2_axis1(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;

	dir = +1;
	buff_id = 1 + dir;
	buff_len = (i3-1)*n1;

	for (i1 = 0; i1 < n1; i1++) {
		buff[buff_id][buff_len]= u[offset+i3*n2*n1+(n2-2)*n1+i1];
		buff_len = buff_len + 1;
	}

	dir = -1;
	buff_id = 1 + dir;
	buff_len = (i3-1)*n1;

	for (i1 = 0; i1 < n1; i1++) {
		buff[buff_id][buff_len] = u[offset+i3*n2*n1+1*n1+i1];
		buff_len = buff_len + 1;
	}
}
__kernel void kernel_comm1p_2_axis2(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(0);
	if( i2 >= n2 ) return;

	dir = +1;
	buff_id = 1 + dir;
	buff_len = i2*n1;

	for (i1 = 0; i1 < n1; i1++) {
		buff[buff_id][buff_len] = u[offset+(n3-2)*n2*n1+i2*n1+i1];
		buff_len = buff_len + 1;
	}

	dir = -1;
	buff_id = 1 + dir;
	buff_len = i2*n1;

	for (i1 = 0; i1 < n1; i1++) {
		buff[buff_id][buff_len] = u[offset+1*n2*n1+i2*n1+i1];
		buff_len = buff_len + 1;
	}

}
#endif

#if COMM1P_3_DIM == 1
__kernel void kernel_comm1p_3(__global double *buff_buf)
{
	int i = get_global_id(0);
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	if( i>= NM2 ) return;

	buff[3][i] = buff[2][i];
	buff[1][i] = buff[0][i];
}
#else // COMM1P_3_DIM == 0
__kernel void kernel_comm1p_3(__global double *buff_buf)
{
	int i;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

  for (i = 0; i < NM2; i++) {
    buff[3][i] = buff[2][i];
    buff[1][i] = buff[0][i];
  }
}
#endif

#if COMM1P_4_DIM == 2
__kernel void kernel_comm1p_4_axis0(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i2, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	if( i3 >= n3-2 || i2 >= n2-2 ) return;

	i3 += 1;
	i2 += 1;

  dir = -1;
  buff_id = 2 + dir;
	indx = (i3-1)*(n2-2) + i2-1;

	u[i3*n2*n1+i2*n1+n1-1] = buff[buff_id][indx];

  dir = +1;
  buff_id = 2 + dir;

	u[i3*n2*n1+i2*n1+0] = buff[buff_id][indx];

}
__kernel void kernel_comm1p_4_axis1(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);
	if( i3 >= n3-2 || i1 >= n1 ) return;

	i3 += 1;

  dir = -1;
  buff_id = 2 + dir;
	indx = (i3-1)*n1 + i1;

	u[i3*n2*n1+(n2-1)*n1+i1] = buff[buff_id][indx];

  dir = +1;
  buff_id = 2 + dir;

	u[i3*n2*n1+i1] = buff[buff_id][indx];
}
__kernel void kernel_comm1p_4_axis2(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(1);
	i1 = get_global_id(0);
	if( i2 >= n2 || i1 >= n1 ) return;

  dir = -1;
  buff_id = 2 + dir;
	indx = i2*n1 + i1;

	u[(n3-1)*n2*n1+i2*n1+i1] = buff[buff_id][indx];

  dir = +1;
  buff_id = 2 + dir;

	u[i2*n1+i1] = buff[buff_id][indx];

}
#else //COMM1P_4_DIM == 1
__kernel void kernel_comm1p_4_axis0(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;

  dir = -1;
  buff_id = 2 + dir;
	indx = (i3-1)*(n2-2);

	for (i2 = 1; i2 < n2-1; i2++) {
		u[offset+i3*n2*n1+i2*n1+n1-1] = buff[buff_id][indx];
		indx = indx + 1;
	}

  dir = +1;
  buff_id = 2 + dir;
	indx = (i3-1)*(n2-2);

	for (i2 = 1; i2 < n2-1; i2++) {
		u[offset+i3*n2*n1+i2*n1+0] = buff[buff_id][indx];
		indx = indx + 1;
	}

}
__kernel void kernel_comm1p_4_axis1(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;

  dir = -1;
  buff_id = 2 + dir;
	indx = (i3-1)*n1;

	for (i1 = 0; i1 < n1; i1++) {
		u[offset+i3*n2*n1+(n2-1)*n1+i1] = buff[buff_id][indx];
		indx = indx + 1;
	}

  dir = +1;
  buff_id = 2 + dir;
	indx = (i3-1)*n1;

	for (i1 = 0; i1 < n1; i1++) {
		u[offset+i3*n2*n1+i1] = buff[buff_id][indx];
		indx = indx + 1;
	}
}
__kernel void kernel_comm1p_4_axis2(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(0);
	if( i2 >= n2 ) return;

  dir = -1;
  buff_id = 2 + dir;
	indx = i2*n1;

	for (i1 = 0; i1 < n1; i1++) {
		u[offset+(n3-1)*n2*n1+i2*n1+i1] = buff[buff_id][indx];
		indx = indx + 1;
	}

  dir = +1;
  buff_id = 2 + dir;
	indx = i2*n1;

	for (i1 = 0; i1 < n1; i1++) {
		u[offset+i2*n1+i1] = buff[buff_id][indx];
		indx = indx + 1;
	}

}
#endif

#if READY_1_DIM == 1
__kernel void kernel_ready_1(__global double *buff_buf, int buff_id)
{
	int i = get_global_id(0);
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	if( i>= NM2 ) return;

	buff[buff_id][i] = 0.0;
}
#else
__kernel void kernel_ready_1(__global double *buff_buf, int buff_id)
{
	int i;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

  for (i = 0; i < NM2; i++) {
    buff[buff_id][i] = 0.0;
  }
}
#endif

#if GIVE3_1_DIM == 2
__kernel void kernel_give3_1_axis0(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, buff_len;
	int i2, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	if( i3 >= n3-2 || i2 >= n2-2 ) return;

	i3 += 1;
	i2 += 1;

	buff_id = 1 + dir;
	buff_len = (i3-1)*(n2-2) + i2-1;

	if( dir == +1)
		buff[buff_id][buff_len] = u[i3*n2*n1+i2*n1+n1-2];
	else if (dir == -1)
		buff[buff_id][buff_len] = u[i3*n2*n1+i2*n1+1];

}
__kernel void kernel_give3_1_axis1(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, buff_len;
	int i1, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);
	if( i3 >= n3-2 || i1 >= n1 ) return;

	i3 += 1;
	buff_id = 1 + dir;
	buff_len = (i3-1)*n1 + i1;

	if( dir == +1)
		buff[buff_id][buff_len] = u[i3*n2*n1+(n2-2)*n1+i1];
	else if( dir == -1)
		buff[buff_id][buff_len] = u[i3*n2*n1+1*n1+i1];
}
__kernel void kernel_give3_1_axis2(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, buff_len;
	int i1, i2;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(1);
	i1 = get_global_id(0);
	if( i2 >= n2 || i1 >= n1 ) return;

	buff_id = 1 + dir;
	buff_len = i2*n1 + i1;

	if( dir == +1 )
		buff[buff_id][buff_len] = u[(n3-2)*n2*n1+i2*n1+i1];
	else if ( dir == -1 )
		buff[buff_id][buff_len] = u[1*n2*n1+i2*n1+i1];
}
#else // GIVE3_1_DIM == 1
__kernel void kernel_give3_1_axis0(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, buff_len;
	int i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;
	buff_id = 1 + dir;
	buff_len = (i3-1)*(n2-2);

	if( dir == +1)
	{
		for (i2 = 1; i2 < n2-1; i2++) {
			buff[buff_id][buff_len] = u[offset+i3*n2*n1+i2*n1+n1-2];
			buff_len = buff_len + 1;
		}
	}
	else if (dir == -1)
	{
		for (i2 = 1; i2 < n2-1; i2++) {
			buff[buff_id][buff_len] = u[offset+i3*n2*n1+i2*n1+1];
			buff_len = buff_len + 1;
		}
	}

}
__kernel void kernel_give3_1_axis1(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, buff_len;
	int i1, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;
	buff_id = 1 + dir;
	buff_len = (i3-1)*n1;

	if( dir == +1)
	{
		for (i1 = 0; i1 < n1; i1++) {
			buff[buff_id][buff_len] = u[offset+i3*n2*n1+(n2-2)*n1+i1];
			buff_len = buff_len + 1;
		}
	}
	else if( dir == -1)
	{
		for (i1 = 0; i1 < n1; i1++) {
			buff[buff_id][buff_len] = u[offset+i3*n2*n1+1*n1+i1];
			buff_len = buff_len + 1;
		}
	}
}
__kernel void kernel_give3_1_axis2(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, buff_len;
	int i1, i2;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(0);
	if( i2 >= n2 ) return;

	buff_id = 1 + dir;
	buff_len = i2*n1;

	if( dir == +1 )
	{
		for (i1 = 0; i1 < n1; i1++) {
			buff[buff_id][buff_len] = u[offset+(n3-2)*n2*n1+i2*n1+i1];
			buff_len = buff_len + 1;
		}
	}
	else if ( dir == -1 )
	{

		for (i1 = 0; i1 < n1; i1++) {
			buff[buff_id][buff_len] = u[offset+1*n2*n1+i2*n1+i1];
			buff_len = buff_len + 1;
		}
	}

}
#endif


#if TAKE3_1_DIM == 2
__kernel void kernel_take3_1_axis0(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, indx;
	int i2, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	if( i3 >= n3-2 || i2 >= n2-2 ) return;

	i3 += 1;
	i2 += 1;

  buff_id = 2 + dir;
	indx = (i3-1)*(n2-2) + i2-1;

	if( dir == -1)
		u[i3*n2*n1+i2*n1+n1-1] = buff[buff_id][indx];
	else if( dir == +1)
		u[i3*n2*n1+i2*n1+0] = buff[buff_id][indx];

}
__kernel void kernel_take3_1_axis1(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, indx;
	int i1, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);
	if( i3 >= n3-2 || i1 >= n1 ) return;

	i3 += 1;
	buff_id = 2 + dir;
	indx = (i3-1)*n1 + i1;

	if( dir == -1)
		u[i3*n2*n1+(n2-1)*n1+i1] = buff[buff_id][indx];
	else if( dir == +1)
		u[i3*n2*n1+i1] = buff[buff_id][indx];
}
__kernel void kernel_take3_1_axis2(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, indx;
	int i1, i2;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(1);
	i1 = get_global_id(0);
	if( i2 >= n2 || i1 >= n1 ) return;

  buff_id = 2 + dir;
	indx = i2*n1 + i1;

	if( dir == -1)
		u[(n3-1)*n2*n1+i2*n1+i1] = buff[buff_id][indx];
	else if( dir == +1)
		u[i2*n1+i1] = buff[buff_id][indx];
}
#else //TAKE3_1_DIM == 1
__kernel void kernel_take3_1_axis0(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, indx;
	int i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;
  buff_id = 2 + dir;
	indx = (i3-1)*(n2-2);

	if( dir == -1)
	{
		for (i2 = 1; i2 < n2-1; i2++) {
			u[offset+i3*n2*n1+i2*n1+n1-1] = buff[buff_id][indx];
			indx = indx + 1;
		}
	}
	else if( dir == +1)
	{
		for (i2 = 1; i2 < n2-1; i2++) {
			u[offset+i3*n2*n1+i2*n1+0] = buff[buff_id][indx];
			indx = indx + 1;
		}
	}

}
__kernel void kernel_take3_1_axis1(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, indx;
	int i1, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3-2 ) return;

	i3 += 1;
	buff_id = 2 + dir;
	indx = (i3-1)*n1;

	if( dir == -1)
	{
		for (i1 = 0; i1 < n1; i1++) {
			u[offset+i3*n2*n1+(n2-1)*n1+i1] = buff[buff_id][indx];
			indx = indx + 1;
		}
	}
	else if( dir == +1)
	{
		for (i1 = 0; i1 < n1; i1++) {
			u[offset+i3*n2*n1+i1] = buff[buff_id][indx];
			indx = indx + 1;
		}
	}
}
__kernel void kernel_take3_1_axis2(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int dir, int offset)
{
	int buff_id, indx;
	int i1, i2;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(0);
	if( i2 >= n2 ) return;

  buff_id = 2 + dir;
	indx = i2*n1;

	if( dir == -1)
	{
		for (i1 = 0; i1 < n1; i1++) {
			u[offset+(n3-1)*n2*n1+i2*n1+i1] = buff[buff_id][indx];
			indx = indx + 1;
		}
	}
	else if( dir == +1)
	{
		for (i1 = 0; i1 < n1; i1++) {
			u[offset+i2*n1+i1] = buff[buff_id][indx];
			indx = indx + 1;
		}
	}

}
#endif


#if COMM1PEX_2_DIM == 2
__kernel void kernel_comm1pex_2_axis0(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i2, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	if( i3 >= n3 || i2 >= n2 ) return;

  dir = -1;
  buff_id = 2 + dir;
	indx = i3*n2 + i2;

	u[i3*n2*n1+i2*n1+n1-1] = buff[buff_id][indx];
}
__kernel void kernel_comm1pex_2_axis1(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);
	if( i3 >= n3 || i1 >= n1 ) return;

  dir = -1;
  buff_id = 2 + dir;
	indx = i3*n1 + i1;

	u[i3*n2*n1+(n2-1)*n1+i1] = buff[buff_id][indx];
}
__kernel void kernel_comm1pex_2_axis2(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(1);
	i1 = get_global_id(0);
	if( i2 >= n2 || i1 >= n1 ) return;

  dir = -1;
  buff_id = 2 + dir;
	indx = i2*n1 + i1;

	u[(n3-1)*n2*n1+i2*n1+i1] = buff[buff_id][indx];
}
#else //COMM1PEX_2_DIM == 1
__kernel void kernel_comm1pex_2_axis0(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3 ) return;

  dir = -1;
  buff_id = 2 + dir;
	indx = i3*n2;

	for (i2 = 0; i2 < n2; i2++) {
		u[offset+i3*n2*n1+i2*n1+n1-1] = buff[buff_id][indx];
		indx = indx + 1;
	}
}
__kernel void kernel_comm1pex_2_axis1(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3 ) return;

  dir = -1;
  buff_id = 2 + dir;
	indx = i3*n1;

	for (i1 = 0; i1 < n1; i1++) {
		u[offset+i3*n2*n1+(n2-1)*n1+i1] = buff[buff_id][indx];
		indx = indx + 1;
	}
}
__kernel void kernel_comm1pex_2_axis2(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(0);
	if( i2 >= n2 ) return;

  dir = -1;
  buff_id = 2 + dir;
	indx = i2*n1;

	for (i1 = 0; i1 < n1; i1++) {
		u[offset+(n3-1)*n2*n1+i2*n1+i1] = buff[buff_id][indx];
		indx = indx + 1;
	}
}
#endif

#if COMM1PEX_3_DIM == 2
__kernel void kernel_comm1pex_3_axis0(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	if( i3 >= n3 || i2 >= n2 ) return;

  dir = +1;
  buff_id = 2 + dir;
	indx = ((i3*n2)+i2)*2;

	for (i1 = 0; i1 < 2; i1++) {
		u[i3*n2*n1+i2*n1+i1] = buff[buff_id][indx];
		indx = indx + 1;
	}
}
__kernel void kernel_comm1pex_3_axis1(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);

	if( i3 >= n3*2 || i1 >= n1 ) return;

	i2 = i3 / n3;
	i3 = i3 % n3;

  dir = +1;
  buff_id = 2 + dir;
	indx = i3*2*n1 + i2*n1 + i1;

	u[i3*n2*n1+i2*n1+i1] = buff[buff_id][indx];
}
__kernel void kernel_comm1pex_3_axis2(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);
	if( i3 >= n2*2 || i1 >= n1 ) return;

	i2 = i3 % n2;
	i3 = i3 / n2;

  dir = +1;
  buff_id = 2 + dir;
	indx = i3*n2*n1 + i2*n1 + i1;

	u[i3*n2*n1+i2*n1+i1] = buff[buff_id][indx];
}
#else //COMM1PEX_3_DIM == 1
__kernel void kernel_comm1pex_3_axis0(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3 ) return;

  dir = +1;
  buff_id = 2 + dir;
	indx = i3*n2*2;

	for (i2 = 0; i2 < n2; i2++) {
		for (i1 = 0; i1 < 2; i1++) {
			u[offset+i3*n2*n1+i2*n1+i1] = buff[buff_id][indx];
			indx = indx + 1;
		}
	}
}
__kernel void kernel_comm1pex_3_axis1(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3 ) return;

  dir = +1;
  buff_id = 2 + dir;
	indx = i3*2*n1;

	for (i2 = 0; i2 < 2; i2++) {
		for (i1 = 0; i1 < n1; i1++) {
			u[offset+i3*n2*n1+i2*n1+i1] = buff[buff_id][indx];
			indx = indx + 1;
		}
	}
}
__kernel void kernel_comm1pex_3_axis2(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, indx;
	int i1, i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n2*2 ) return;

	i2 = i3 % n2;
	i3 = i3 / n2;

  dir = +1;
  buff_id = 2 + dir;
	indx = i3*n2*n1 + i2*n1;

	for (i1 = 0; i1 < n1; i1++) {
		u[offset+i3*n2*n1+i2*n1+i1] = buff[buff_id][indx];
		indx = indx + 1;
	}
}
#endif
#if COMM1PEX_4_DIM == 2
__kernel void kernel_comm1pex_4_axis0(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2, i3;
3
	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);

	if( i3 >= n3 || i2 >= n2 ) return;

  dir = +1;
  buff_id = 1 + dir;
	buff_len = i3*n2*2 + i2*2;

	for (i1 = n1-2; i1 < n1; i1++) {
		buff[buff_id][buff_len]= u[i3*n2*n1+i2*n1+i1];
		buff_len = buff_len + 1;
	}
}
__kernel void kernel_comm1pex_4_axis1(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2, i3, i2_orig;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);

	if( i3 >= n3*2 || i1 >= n1 ) return;

	i2 = i3 / n3;
	i3 = i3 % n3;
	i2_orig = n2-2 + i2;

  dir = +1;
  buff_id = 1 + dir;
	buff_len = i3*2*n1 + i2*n1 + i1;

	for (i1 = 0; i1 < n1; i1++) {
		buff[buff_id][buff_len]= u[i3*n2*n1+i2_orig*n1+i1];
		buff_len = buff_len + 1;
	}
}
__kernel void kernel_comm1pex_4_axis2(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2, i3, i3_orig;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);
	if( i3 >= n2*2 || i1 >= n1 ) return;

	i2 = i3 % n2;
	i3 = i3 / n2;
	i3_orig = i3 + n3-2;

  dir = +1;
  buff_id = 1 + dir;
	buff_len = i3*n2*n1 + i2*n1 + i1;

	buff[buff_id][buff_len] = u[i3_orig*n2*n1+i2*n1+i1];
	buff_len = buff_len + 1;
}
#else //COMM1PEX_4_DIM == 1
__kernel void kernel_comm1pex_4_axis0(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3 ) return;

  dir = +1;
  buff_id = 1 + dir;
	buff_len = i3*n2*2;

	for (i2 = 0; i2 < n2; i2++) {
		for (i1 = n1-2; i1 < n1; i1++) {
			buff[buff_id][buff_len]= u[offset+i3*n2*n1+i2*n1+i1];
			buff_len = buff_len + 1;
		}
	}
}
__kernel void kernel_comm1pex_4_axis1(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3 ) return;

  dir = +1;
  buff_id = 1 + dir;
	buff_len = i3*2*n1;

	for (i2 = n2-2; i2 < n2; i2++) {
		for (i1 = 0; i1 < n1; i1++) {
			buff[buff_id][buff_len]= u[offset+i3*n2*n1+i2*n1+i1];
			buff_len = buff_len + 1;
		}
	}
}
__kernel void kernel_comm1pex_4_axis2(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2, i3, i3_orig;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n2*2 ) return;

	i2 = i3 % n2;
	i3 = i3 / n2;
	i3_orig = i3 + n3-2;

  dir = +1;
  buff_id = 1 + dir;
	buff_len = i3*n2*n1 + i2*n1;

	for (i1 = 0; i1 < n1; i1++) {
		buff[buff_id][buff_len] = u[offset+i3_orig*n2*n1+i2*n1+i1];
		buff_len = buff_len + 1;
	}
}
#endif

#if COMM1PEX_5_DIM == 2
__kernel void kernel_comm1pex_5_axis0(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i2, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	if( i3 >= n3 || i2 >= n2 ) return;

  dir = -1;
  buff_id = 1 + dir;
	buff_len = i3*n2 + i2;

	buff[buff_id][buff_len] = u[i3*n2*n1+i2*n1+1];
}
__kernel void kernel_comm1pex_5_axis1(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i3;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(1);
	i1 = get_global_id(0);
	if( i3 >= n3 || i1 >= n1 ) return;

  dir = -1;
  buff_id = 1 + dir;
	buff_len = i3*n1 + i1;

	buff[buff_id][buff_len] = u[i3*n2*n1+1*n1+i1];
}
__kernel void kernel_comm1pex_5_axis2(__global double *ou_buf, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2;

	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(1);
	i1 = get_global_id(0);
	if( i2 >= n2 || i1 >= n1 ) return;

  dir = -1;
  buff_id = 1 + dir;
	buff_len = i2*n1 + i1;

	buff[buff_id][buff_len] = u[1*n2*n1+i2*n1+i1];
}
#else //COMM1PEX_5_DIM == 1
__kernel void kernel_comm1pex_5_axis0(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i2, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3 ) return;

  dir = -1;
  buff_id = 1 + dir;
	buff_len = i3*n2;

	for (i2 = 0; i2 < n2; i2++) {
		buff[buff_id][buff_len] = u[offset+i3*n2*n1+i2*n1+1];
		buff_len = buff_len + 1;
	}
}
__kernel void kernel_comm1pex_5_axis1(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i3;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i3 = get_global_id(0);
	if( i3 >= n3 ) return;

  dir = -1;
  buff_id = 1 + dir;
	buff_len = i3*n1;

	for (i1 = 0; i1 < n1; i1++) {
		buff[buff_id][buff_len] = u[offset+i3*n2*n1+1*n1+i1];
		buff_len = buff_len + 1;
	}
}
__kernel void kernel_comm1pex_5_axis2(__global double *u, 
		__global double *buff_buf, int n1, int n2, int n3, int offset)
{
	int dir, buff_id, buff_len;
	int i1, i2;

//	__global double *u = ou_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
	__global double (*buff)[NM2] = (__global double (*)[NM2])buff_buf;

	i2 = get_global_id(0);
	if( i2 >= n2 ) return;

  dir = -1;
  buff_id = 1 + dir;
	buff_len = i2*n1;

	for (i1 = 0; i1 < n1; i1++) {
		buff[buff_id][buff_len] = u[offset+1*n2*n1+i2*n1+i1];
		buff_len = buff_len + 1;
	}
}
#endif
//RESID_DIM 3 shows very poor performance
#if RESID_DIM == 2
__kernel void kernel_resid(__global double* u, __global double* v, 
		__global double* r, int n1, int n2, int n3, int offset)
{
	double a[4];
  int i3, i2, i1;

  double u1[M], u2[M];

//	__global double *u = ou_buf + offset;
//	__global double *v = ov_buf + offset;
//	__global double *r = or_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
//  __global double (*v)[n2][n1] = (__global double (*)[n2][n1])o_v;
//  __global double (*r)[n2][n1] = (__global double (*)[n2][n1])o_r;

  a[0] = -8.0/3.0;
  a[1] =  0.0;
  a[2] =  1.0/6.0;
  a[3] =  1.0/12.0;

	i3 = get_global_id(1);
	i2 = get_global_id(0);
	if( i3 >= n3-2 || i2 >= n2-2 ) return;

	i3 += 1;
	i2 += 1;

	for (i1 = 0; i1 < n1; i1++) {
		u1[i1] = u[offset+i3*n2*n1+(i2-1)*n1+i1] + u[offset+i3*n2*n1+(i2+1)*n1+i1]
			+ u[offset+(i3-1)*n2*n1+i2*n1+i1] + u[offset+(i3+1)*n2*n1+i2*n1+i1];
		u2[i1] = u[offset+(i3-1)*n2*n1+(i2-1)*n1+i1] + u[offset+(i3-1)*n2*n1+(i2+1)*n1+i1]
			+ u[offset+(i3+1)*n2*n1+(i2-1)*n1+i1] + u[offset+(i3+1)*n2*n1+(i2+1)*n1+i1];
	}
	for (i1 = 1; i1 < n1-1; i1++) {
		r[offset+i3*n2*n1+i2*n1+i1] = v[offset+i3*n2*n1+i2*n1+i1]
			- a[0] * u[offset+i3*n2*n1+i2*n1+i1]
			- a[2] * ( u2[i1] + u1[i1-1] + u1[i1+1] )
			- a[3] * ( u2[i1-1] + u2[i1+1] );
  }
}
#else //RESID_DIM == 1
__kernel void kernel_resid(__global double* u, __global double* v, 
		__global double* r, int n1, int n2, int n3, int offset)
{
//	double a[4];
  int i3, i2, i1;
  double u1[M], u2[M];


//	__global double *u = ou_buf + offset;
//	__global double *v = ov_buf + offset;
//	__global double *r = or_buf + offset;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])o_u;
//  __global double (*v)[n2][n1] = (__global double (*)[n2][n1])o_v;
//  __global double (*r)[n2][n1] = (__global double (*)[n2][n1])o_r;

  double a0 = -8.0/3.0;
  double a1 =  0.0;
  double a2 =  1.0/6.0;
  double a3 =  1.0/12.0;

	i3 = get_global_id(0)+1;
	if( i3 >= n3-1 ) return;

	for (i2 = 1; i2 < n2-1; i2++) {
		for (i1 = 0; i1 < n1; i1++) {
			u1[i1] = u[offset+i3*n2*n1+(i2-1)*n1+i1] + u[offset+i3*n2*n1+(i2+1)*n1+i1]
				+ u[offset+(i3-1)*n2*n1+i2*n1+i1] + u[offset+(i3+1)*n2*n1+i2*n1+i1];
			u2[i1] = u[offset+(i3-1)*n2*n1+(i2-1)*n1+i1] + u[offset+(i3-1)*n2*n1+(i2+1)*n1+i1]
				+ u[offset+(i3+1)*n2*n1+(i2-1)*n1+i1] + u[offset+(i3+1)*n2*n1+(i2+1)*n1+i1];
		}
		for (i1 = 1; i1 < n1-1; i1++) {
			r[offset+i3*n2*n1+i2*n1+i1] = v[offset+i3*n2*n1+i2*n1+i1]
				- a0 * u[offset+i3*n2*n1+i2*n1+i1]
				- a2 * ( u2[i1] + u1[i1-1] + u1[i1+1] )
				- a3 * ( u2[i1-1] + u2[i1+1] );
		}
  }
}
#endif

#if RPRJ3_DIM == 2
__kernel void kernel_rprj3(__global double* base_o_r,
						   int m1k, int m2k, int m3k,
						   int m1j, int m2j, int m3j,
						   int offset_r, int offset_s,
						   int d1, int d2, int d3)
{
	int j3, j2, j1, i3, i2, i1;
	double x1[M], y1[M], x2, y2;

//	__global double* r = base_o_r + offset_r;
//	__global double* s = base_o_r + offset_s;
//  __global double (*r)[m2k][m1k] = (__global double (*)[m2k][m1k])o_r;
//  __global double (*s)[m2j][m1j] = (__global double (*)[m2j][m1j])o_s;

	j3 = get_global_id(1);
	j2 = get_global_id(0);
	if( j3 >= m3j-2 || j2 >= m2j-2 ) return;

	j3 += 1;
	j2 += 1;

	i3 = 2*j3-d3;
	i2 = 2*j2-d2;

	for (j1 = 1; j1 < m1j; j1++) {
		i1 = 2*j1-d1;
		x1[i1] = base_o_r[offset_r+(i3+1)*m2k*m1k+i2*m1k+i1] + base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+2)*m1k+i1]
			+ base_o_r[offset_r+i3*m2k*m1k+(i2+1)*m1k+i1] + base_o_r[offset_r+(i3+2)*m2k*m1k+(i2+1)*m1k+i1];
		y1[i1] = base_o_r[offset_r+i3*m2k*m1k+i2*m1k+i1] + base_o_r[offset_r+(i3+2)*m2k*m1k+i2*m1k+i1]
			+ base_o_r[offset_r+i3*m2k*m1k+(i2+2)*m1k+i1] + base_o_r[offset_r+(i3+2)*m2k*m1k+(i2+2)*m1k+i1];
	}

	for (j1 = 1; j1 < m1j-1; j1++) {
		i1 = 2*j1-d1;
		y2 = base_o_r[offset_r+i3*m2k*m1k+i2*m1k+i1+1] + base_o_r[offset_r+(i3+2)*m2k*m1k+i2*m1k+i1+1]
			+ base_o_r[offset_r+i3*m2k*m1k+(i2+2)*m1k+i1+1] + base_o_r[offset_r+(i3+2)*m2k*m1k+(i2+2)*m1k+i1+1];
		x2 = base_o_r[offset_r+(i3+1)*m2k*m1k+i2*m1k+i1+1] + base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+2)*m1k+i1+1]
			+ base_o_r[offset_r+i3*m2k*m1k+(i2+1)*m1k+i1+1] + base_o_r[offset_r+(i3+2)*m2k*m1k+(i2+1)*m1k+i1+1];
		base_o_r[offset_s+j3*m2j*m1j+j2*m1j+j1] =
			0.5 * base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+1)*m1k+i1+1]
			+ 0.25 * (base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+1)*m1k+i1] + base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+1)*m1k+i1+2] + x2)
			+ 0.125 * (x1[i1] + x1[i1+2] + y2)
			+ 0.0625 * (y1[i1] + y1[i1+2]);
	}
}
#else // RPRJ3_DIM == 1
__kernel void kernel_rprj3(__global double* base_o_r,
						   int m1k, int m2k, int m3k,
						   int m1j, int m2j, int m3j,
						   int offset_r, int offset_s,
						   int d1, int d2, int d3)
{
	int j3, j2, j1, i3, i2, i1;
	double x1[M], y1[M], x2, y2;

//	__global double* r = base_o_r + offset_r;
//	__global double* s = base_o_r + offset_s;
//  __global double (*r)[m2k][m1k] = (__global double (*)[m2k][m1k])o_r;
//  __global double (*s)[m2j][m1j] = (__global double (*)[m2j][m1j])o_s;

	j3 = get_global_id(0);
	if( j3 >= m3j-2 ) return;

	j3 += 1;

	i3 = 2*j3-d3;
	for (j2 = 1; j2 < m2j-1; j2++) {
		i2 = 2*j2-d2;

		for (j1 = 1; j1 < m1j; j1++) {
			i1 = 2*j1-d1;
			x1[i1] = base_o_r[offset_r+(i3+1)*m2k*m1k+i2*m1k+i1] + base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+2)*m1k+i1]
				+ base_o_r[offset_r+i3*m2k*m1k+(i2+1)*m1k+i1] + base_o_r[offset_r+(i3+2)*m2k*m1k+(i2+1)*m1k+i1];
			y1[i1] = base_o_r[offset_r+i3*m2k*m1k+i2*m1k+i1] + base_o_r[offset_r+(i3+2)*m2k*m1k+i2*m1k+i1]
				+ base_o_r[offset_r+i3*m2k*m1k+(i2+2)*m1k+i1] + base_o_r[offset_r+(i3+2)*m2k*m1k+(i2+2)*m1k+i1];
		}

		for (j1 = 1; j1 < m1j-1; j1++) {
			i1 = 2*j1-d1;
			y2 = base_o_r[offset_r+i3*m2k*m1k+i2*m1k+i1+1] + base_o_r[offset_r+(i3+2)*m2k*m1k+i2*m1k+i1+1]
				+ base_o_r[offset_r+i3*m2k*m1k+(i2+2)*m1k+i1+1] + base_o_r[offset_r+(i3+2)*m2k*m1k+(i2+2)*m1k+i1+1];
			x2 = base_o_r[offset_r+(i3+1)*m2k*m1k+i2*m1k+i1+1] + base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+2)*m1k+i1+1]
				+ base_o_r[offset_r+i3*m2k*m1k+(i2+1)*m1k+i1+1] + base_o_r[offset_r+(i3+2)*m2k*m1k+(i2+1)*m1k+i1+1];
			base_o_r[offset_s+j3*m2j*m1j+j2*m1j+j1] =
				0.5 * base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+1)*m1k+i1+1]
				+ 0.25 * (base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+1)*m1k+i1] + base_o_r[offset_r+(i3+1)*m2k*m1k+(i2+1)*m1k+i1+2] + x2)
				+ 0.125 * (x1[i1] + x1[i1+2] + y2)
				+ 0.0625 * (y1[i1] + y1[i1+2]);
		}
	}
}
#endif

#if PSINV_DIM == 2
__kernel void kernel_psinv(__global double* r, __global double* u, 
		int n1, int n2, int n3, double c0, double c1, double c2, double c3, int offset)
{
  int i3, i2, i1;
  double r1[M], r2[M];
	double c[4] = {c0, c1, c2, c3};

//	__global double* r = or_buf + offset;
//	__global double* u = ou_buf + offset;
//	__global double (*r)[n2][n1] = (__global double (*)[n2][n1])or;
//	__global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(1);
	i2 = get_global_id(0);
	if( i3 >= n3-2 || i2 >= n2-2 ) return;

	i3 += 1;
	i2 += 1;

	for (i1 = 0; i1 < n1; i1++) {
		r1[i1] = r[offset+i3*n2*n1+(i2-1)*n1+i1] + r[offset+i3*n2*n1+(i2+1)*n1+i1]
			+ r[offset+(i3-1)*n2*n1+i2*n1+i1] + r[offset+(i3+1)*n2*n1+i2*n1+i1];
		r2[i1] = r[offset+(i3-1)*n2*n1+(i2-1)*n1+i1] + r[offset+(i3-1)*n2*n1+(i2+1)*n1+i1]
			+ r[offset+(i3+1)*n2*n1+(i2-1)*n1+i1] + r[offset+(i3+1)*n2*n1+(i2+1)*n1+i1];
	}
	for (i1 = 1; i1 < n1-1; i1++) {
		u[offset+i3*n2*n1+i2*n1+i1] = u[offset+i3*n2*n1+i2*n1+i1]
			+ c[0] * r[offset+i3*n2*n1+i2*n1+i1]
			+ c[1] * ( r[offset+i3*n2*n1+i2*n1+i1-1] + r[offset+i3*n2*n1+i2*n1+i1+1]
					+ r1[i1] )
			+ c[2] * ( r2[i1] + r1[i1-1] + r1[i1+1] );
	}
}
#else // PSINV_DIM == 1
__kernel void kernel_psinv(__global double* r, __global double* u, 
		int n1, int n2, int n3, double c0, double c1, double c2, double c3, int offset)
{
  int i3, i2, i1;
  double r1[M], r2[M];
	double c[4] = {c0, c1, c2, c3};

	i3 = get_global_id(0)+1;
	if( i3 >= n3-1 ) return;

	for (i2 = 1; i2 < n2-1; i2++) {
		for (i1 = 0; i1 < n1; i1++) {
			r1[i1] = r[offset+i3*n2*n1+(i2-1)*n1+i1] + r[offset+i3*n2*n1+(i2+1)*n1+i1]
				+ r[offset+(i3-1)*n2*n1+i2*n1+i1] + r[offset+(i3+1)*n2*n1+i2*n1+i1];
			r2[i1] = r[offset+(i3-1)*n2*n1+(i2-1)*n1+i1] + r[offset+(i3-1)*n2*n1+(i2+1)*n1+i1]
				+ r[offset+(i3+1)*n2*n1+(i2-1)*n1+i1] + r[offset+(i3+1)*n2*n1+(i2+1)*n1+i1];
		}
		for (i1 = 1; i1 < n1-1; i1++) {
			u[offset+i3*n2*n1+i2*n1+i1] = u[offset+i3*n2*n1+i2*n1+i1]
				+ c[0] * r[offset+i3*n2*n1+i2*n1+i1]
				+ c[1] * ( r[offset+i3*n2*n1+i2*n1+i1-1] + r[offset+i3*n2*n1+i2*n1+i1+1]
						+ r1[i1] )
				+ c[2] * ( r2[i1] + r1[i1-1] + r1[i1+1] );
		}
	}
}
#endif

#if INTERP_1_DIM == 2
__kernel void kernel_interp_1(__global double* base_o_u,
							int mm1, int mm2, int mm3,
							int n1, int n2, int n3,
							int offset_z, int offset_u)
{
	int i3, i2, i1;
  double z1[M], z2[M], z3[M];

	__global double* z = base_o_u + offset_z;
	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	if( i3 >= mm3-1 || i2 >= mm2-1 ) return;

	for (i1 = 0; i1 < mm1; i1++) {
		z1[i1] = z[i3*mm2*mm1+(i2+1)*mm1+i1] + z[i3*mm2*mm1+i2*mm1+i1];
		z2[i1] = z[(i3+1)*mm2*mm1+i2*mm1+i1] + z[i3*mm2*mm1+i2*mm1+i1];
		z3[i1] = z[(i3+1)*mm2*mm1+(i2+1)*mm1+i1] + z[(i3+1)*mm2*mm1+i2*mm1+i1] + z1[i1];
	}

	for (i1 = 0; i1 < mm1-1; i1++) {
		u[2*i3*n2*n1+2*i2*n1+2*i1] = u[2*i3*n2*n1+2*i2*n1+2*i1]
			+ z[i3*mm2*mm1+i2*mm1+i1];
		u[2*i3*n2*n1+2*i2*n1+2*i1+1] = u[2*i3*n2*n1+2*i2*n1+2*i1+1]
			+ 0.5 * (z[i3*mm2*mm1+i2*mm1+i1+1] + z[i3*mm2*mm1+i2*mm1+i1]);
	}
	for (i1 = 0; i1 < mm1-1; i1++) {
		u[2*i3*n2*n1+(2*i2+1)*n1+2*i1] = u[2*i3*n2*n1+(2*i2+1)*n1+2*i1]
			+ 0.5 * z1[i1];
		u[2*i3*n2*n1+(2*i2+1)*n1+2*i1+1] = u[2*i3*n2*n1+(2*i2+1)*n1+2*i1+1]
			+ 0.25 * (z1[i1] + z1[i1+1]);
	}
	for (i1 = 0; i1 < mm1-1; i1++) {
		u[(2*i3+1)*n2*n1+2*i2*n1+2*i1] = u[(2*i3+1)*n2*n1+2*i2*n1+2*i1]
			+ 0.5 * z2[i1];
		u[(2*i3+1)*n2*n1+2*i2*n1+2*i1+1] = u[(2*i3+1)*n2*n1+2*i2*n1+2*i1+1]
			+ 0.25 * (z2[i1] + z2[i1+1]);
	}
	for (i1 = 0; i1 < mm1-1; i1++) {
		u[(2*i3+1)*n2*n1+(2*i2+1)*n1+2*i1] = u[(2*i3+1)*n2*n1+(2*i2+1)*n1+2*i1]
			+ 0.25 * z3[i1];
		u[(2*i3+1)*n2*n1+(2*i2+1)*n1+2*i1+1] = u[(2*i3+1)*n2*n1+(2*i2+1)*n1+2*i1+1]
			+ 0.125 * (z3[i1] + z3[i1+1]);
	}

}
#else //INTERP_1_DIM == 1
__kernel void kernel_interp_1(__global double* base_o_u,
							int mm1, int mm2, int mm3,
							int n1, int n2, int n3,
							int offset_z, int offset_u
			                 )
{
	int i3, i2, i1;
  double z1[M], z2[M], z3[M];

//	__global double* z = base_o_u + offset_z;
//	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(0);

	if( i3 >= mm3-1 ) return;

	for (i2 = 0; i2 < mm2-1; i2++) {

		for (i1 = 0; i1 < mm1; i1++) {
			z1[i1] = base_o_u[offset_z+i3*mm2*mm1+(i2+1)*mm1+i1] + base_o_u[offset_z+i3*mm2*mm1+i2*mm1+i1];
			z2[i1] = base_o_u[offset_z+(i3+1)*mm2*mm1+i2*mm1+i1] + base_o_u[offset_z+i3*mm2*mm1+i2*mm1+i1];
			z3[i1] = base_o_u[offset_z+(i3+1)*mm2*mm1+(i2+1)*mm1+i1] + base_o_u[offset_z+(i3+1)*mm2*mm1+i2*mm1+i1] + z1[i1];
		}

		for (i1 = 0; i1 < mm1-1; i1++) {
			base_o_u[offset_u+2*i3*n2*n1+2*i2*n1+2*i1] = base_o_u[offset_u+2*i3*n2*n1+2*i2*n1+2*i1]
				+ base_o_u[offset_z+i3*mm2*mm1+i2*mm1+i1];
			base_o_u[offset_u+2*i3*n2*n1+2*i2*n1+2*i1+1] = base_o_u[offset_u+2*i3*n2*n1+2*i2*n1+2*i1+1]
				+ 0.5 * (base_o_u[offset_z+i3*mm2*mm1+i2*mm1+i1+1] + base_o_u[offset_z+i3*mm2*mm1+i2*mm1+i1]);
		}
		for (i1 = 0; i1 < mm1-1; i1++) {
			base_o_u[offset_u+2*i3*n2*n1+(2*i2+1)*n1+2*i1] = base_o_u[offset_u+2*i3*n2*n1+(2*i2+1)*n1+2*i1]
				+ 0.5 * z1[i1];
			base_o_u[offset_u+2*i3*n2*n1+(2*i2+1)*n1+2*i1+1] = base_o_u[offset_u+2*i3*n2*n1+(2*i2+1)*n1+2*i1+1]
				+ 0.25 * (z1[i1] + z1[i1+1]);
		}
		for (i1 = 0; i1 < mm1-1; i1++) {
			base_o_u[offset_u+(2*i3+1)*n2*n1+2*i2*n1+2*i1] = base_o_u[offset_u+(2*i3+1)*n2*n1+2*i2*n1+2*i1]
				+ 0.5 * z2[i1];
			base_o_u[offset_u+(2*i3+1)*n2*n1+2*i2*n1+2*i1+1] = base_o_u[offset_u+(2*i3+1)*n2*n1+2*i2*n1+2*i1+1]
				+ 0.25 * (z2[i1] + z2[i1+1]);
		}
		for (i1 = 0; i1 < mm1-1; i1++) {
			base_o_u[offset_u+(2*i3+1)*n2*n1+(2*i2+1)*n1+2*i1] = base_o_u[offset_u+(2*i3+1)*n2*n1+(2*i2+1)*n1+2*i1]
				+ 0.25 * z3[i1];
			base_o_u[offset_u+(2*i3+1)*n2*n1+(2*i2+1)*n1+2*i1+1] = base_o_u[offset_u+(2*i3+1)*n2*n1+(2*i2+1)*n1+2*i1+1]
				+ 0.125 * (z3[i1] + z3[i1+1]);
		}
	}

}
#endif
#if INTERP_2_DIM == 2
__kernel void kernel_interp_2(__global double* base_o_u,
							int mm1, int mm2, int mm3,
							int n1, int n2, int n3,
							int d1, int d2, int d3,
							int t1, int t2, int t3,
							int offset_z, int offset_u)
{
	int i3, i2, i1;
	__global double* z = base_o_u + offset_z;
	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	i3+=d3;
	i2+=d2;

	if( i3 > mm3-1 || i2 > mm2-1 ) return;

	for (i1 = d1; i1 <= mm1-1; i1++) {
		u[(2*i3-d3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-d1-1] = 
			u[(2*i3-d3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-d1-1]
			+ z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1];
	}
	for (i1 = 1; i1 <= mm1-1; i1++) {
		u[(2*i3-d3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-t1-1] = 
			u[(2*i3-d3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-t1-1]
			+ 0.5 * (z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1] + z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
	}
}
#else //INTERP_2_DIM == 1
__kernel void kernel_interp_2(__global double* base_o_u,
							int mm1, int mm2, int mm3,
							int n1, int n2, int n3,
							int d1, int d2, int d3,
							int t1, int t2, int t3,
							int offset_z, int offset_u)
{
	int i3, i2, i1;
//	__global double* z = base_o_u + offset_z;
//	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(0);

	i3 += d3;

	if( i3 > mm3-1 ) return;

	for (i2 = d2; i2 <= mm2-1; i2++) {
		for (i1 = d1; i1 <= mm1-1; i1++) {
			base_o_u[offset_u+(2*i3-d3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-d1-1] = 
				base_o_u[offset_u+(2*i3-d3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-d1-1]
				+ base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1];
		}
		for (i1 = 1; i1 <= mm1-1; i1++) {
			base_o_u[offset_u+(2*i3-d3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-t1-1] = 
				base_o_u[offset_u+(2*i3-d3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-t1-1]
				+ 0.5 * (base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1] + base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
		}
	}
}
#endif

#if INTERP_3_DIM == 2
__kernel void kernel_interp_3(__global double* base_o_u,
							int mm1, int mm2, int mm3,
							int n1, int n2, int n3,
							int d1, int d2, int d3,
							int t1, int t2, int t3,
							int offset_z, int offset_u)
{
	int i3, i2, i1;
	__global double* z = base_o_u + offset_z;
	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	i3+=d3;
	i2+=1;

	if( i3 > mm3-1 || i2 > mm2-1 ) return;

	for (i1 = d1; i1 <= mm1-1; i1++) {
		u[(2*i3-d3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-d1-1] = 
			u[(2*i3-d3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-d1-1]
			+ 0.5 * (z[(i3-1)*mm2*mm1+i2*mm1+i1-1] + z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
	}
	for (i1 = 1; i1 <= mm1-1; i1++) {
		u[(2*i3-d3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-t1-1] = 
			u[(2*i3-d3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-t1-1]
			+ 0.25 * (z[(i3-1)*mm2*mm1+i2*mm1+i1] + z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1]
					+ z[(i3-1)*mm2*mm1+i2*mm1+i1-1] + z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
	}
}
#else //INTERP_3_DIM == 1
__kernel void kernel_interp_3(__global double* base_o_u,
							int mm1, int mm2, int mm3,
							int n1, int n2, int n3,
							int d1, int d2, int d3,
							int t1, int t2, int t3,
							int offset_z, int offset_u)
{
	int i3, i2, i1;
//	__global double* z = base_o_u + offset_z;
//	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(0);

	i3+=d3;

	if( i3 > mm3-1 ) return;

	for (i2 = 1; i2 <= mm2-1; i2++) {
		for (i1 = d1; i1 <= mm1-1; i1++) {
			base_o_u[offset_u+(2*i3-d3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-d1-1] = 
				base_o_u[offset_u+(2*i3-d3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-d1-1]
				+ 0.5 * (base_o_u[offset_z+(i3-1)*mm2*mm1+i2*mm1+i1-1] + base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
		}
		for (i1 = 1; i1 <= mm1-1; i1++) {
			base_o_u[offset_u+(2*i3-d3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-t1-1] = 
				base_o_u[offset_u+(2*i3-d3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-t1-1]
				+ 0.25 * (base_o_u[offset_z+(i3-1)*mm2*mm1+i2*mm1+i1] + base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1]
						+ base_o_u[offset_z+(i3-1)*mm2*mm1+i2*mm1+i1-1] + base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
		}
	}
}
#endif

#if INTERP_4_DIM == 2
__kernel void kernel_interp_4(__global double* base_o_u,
							int mm1, int mm2, int mm3,
							int n1, int n2, int n3,
							int d1, int d2, int d3,
							int t1, int t2, int t3,
							int offset_z, int offset_u)
{
	int i3, i2, i1;
	__global double* z = base_o_u + offset_z;
	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	i3+=1;
	i2+=d2;

	if( i3 > mm3-1 || i2 > mm2-1 ) return;

	for (i1 = d1; i1 <= mm1-1; i1++) {
		u[(2*i3-t3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-d1-1] = 
			u[(2*i3-t3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-d1-1]
			+ 0.5 * (z[i3*mm2*mm1+(i2-1)*mm1+i1-1] + z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
	}
	for (i1 = 1; i1 <= mm1-1; i1++) {
		u[(2*i3-t3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-t1-1] = 
			u[(2*i3-t3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-t1-1]
			+ 0.25 * (z[i3*mm2*mm1+(i2-1)*mm1+i1] + z[i3*mm2*mm1+(i2-1)*mm1+i1-1]
					+ z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1] + z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
	}
}
#else //INTERP_4_DIM == 1
__kernel void kernel_interp_4(__global double* base_o_u,
		int mm1, int mm2, int mm3,
		int n1, int n2, int n3,
		int d1, int d2, int d3,
		int t1, int t2, int t3,
		int offset_z, int offset_u)
{
	int i3, i2, i1;
//	__global double* z = base_o_u + offset_z;
//	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(0);

	i3+=1;

	if( i3 > mm3-1 ) return;

	for (i2 = d2; i2 <= mm2-1; i2++) {
		for (i1 = d1; i1 <= mm1-1; i1++) {
			base_o_u[offset_u+(2*i3-t3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-d1-1] = 
				base_o_u[offset_u+(2*i3-t3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-d1-1]
				+ 0.5 * (base_o_u[offset_z+i3*mm2*mm1+(i2-1)*mm1+i1-1] + base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
		}
		for (i1 = 1; i1 <= mm1-1; i1++) {
			base_o_u[offset_u+(2*i3-t3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-t1-1] = 
				base_o_u[offset_u+(2*i3-t3-1)*n2*n1+(2*i2-d2-1)*n1+2*i1-t1-1]
				+ 0.25 * (base_o_u[offset_z+i3*mm2*mm1+(i2-1)*mm1+i1] + base_o_u[offset_z+i3*mm2*mm1+(i2-1)*mm1+i1-1]
						+ base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1] + base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
		}
	}
}
#endif

#if INTERP_5_DIM == 2
__kernel void kernel_interp_5(__global double* base_o_u,
							int mm1, int mm2, int mm3,
							int n1, int n2, int n3,
							int d1, int d2, int d3,
							int t1, int t2, int t3,
							int offset_z, int offset_u)
{
	int i3, i2, i1;
	__global double* z = base_o_u + offset_z;
	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(1);
	i2 = get_global_id(0);

	i3+=1;
	i2+=1;

	if( i3 > mm3-1 ||  i2 > mm2-1 ) return;

	for (i1 = d1; i1 <= mm1-1; i1++) {
		u[(2*i3-t3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-d1-1] = 
			u[(2*i3-t3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-d1-1]
			+ 0.25 * (z[i3*mm2*mm1+i2*mm1+i1-1] + z[i3*mm2*mm1+(i2-1)*mm1+i1-1]
					+ z[(i3-1)*mm2*mm1+i2*mm1+i1-1] + z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
	}
	for (i1 = 1; i1 <= mm1-1; i1++) {
		u[(2*i3-t3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-t1-1] = 
			u[(2*i3-t3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-t1-1]
			+ 0.125 * (z[i3*mm2*mm1+i2*mm1+i1] + z[i3*mm2*mm1+(i2-1)*mm1+i1]
					+ z[i3*mm2*mm1+i2*mm1+i1-1] + z[i3*mm2*mm1+(i2-1)*mm1+i1-1]
					+ z[(i3-1)*mm2*mm1+i2*mm1+i1] + z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1]
					+ z[(i3-1)*mm2*mm1+i2*mm1+i1-1] + z[(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
	}
}
#else //INTERP_5_DIM == 1
__kernel void kernel_interp_5(__global double* base_o_u,
		int mm1, int mm2, int mm3,
		int n1, int n2, int n3,
		int d1, int d2, int d3,
		int t1, int t2, int t3,
		int offset_z, int offset_u)
{
	int i3, i2, i1;
//	__global double* z = base_o_u + offset_z;
//	__global double* u = base_o_u + offset_u;
//  __global double (*z)[mm2][mm1] = (__global double (*)[mm2][mm1])oz;
//  __global double (*u)[n2][n1] = (__global double (*)[n2][n1])ou;

	i3 = get_global_id(0);

	i3+=1;

	if( i3 > mm3-1 ) return;

	for (i2 = 1; i2 <= mm2-1; i2++) {
		for (i1 = d1; i1 <= mm1-1; i1++) {
			base_o_u[offset_u+(2*i3-t3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-d1-1] = 
				base_o_u[offset_u+(2*i3-t3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-d1-1]
				+ 0.25 * (base_o_u[offset_z+i3*mm2*mm1+i2*mm1+i1-1] + base_o_u[offset_z+i3*mm2*mm1+(i2-1)*mm1+i1-1]
						+ base_o_u[offset_z+(i3-1)*mm2*mm1+i2*mm1+i1-1] + base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
		}
		for (i1 = 1; i1 <= mm1-1; i1++) {
			base_o_u[offset_u+(2*i3-t3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-t1-1] = 
				base_o_u[offset_u+(2*i3-t3-1)*n2*n1+(2*i2-t2-1)*n1+2*i1-t1-1]
				+ 0.125 * (base_o_u[offset_z+i3*mm2*mm1+i2*mm1+i1] + base_o_u[offset_z+i3*mm2*mm1+(i2-1)*mm1+i1]
						+ base_o_u[offset_z+i3*mm2*mm1+i2*mm1+i1-1] + base_o_u[offset_z+i3*mm2*mm1+(i2-1)*mm1+i1-1]
						+ base_o_u[offset_z+(i3-1)*mm2*mm1+i2*mm1+i1] + base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1]
						+ base_o_u[offset_z+(i3-1)*mm2*mm1+i2*mm1+i1-1] + base_o_u[offset_z+(i3-1)*mm2*mm1+(i2-1)*mm1+i1-1]);
		}
	}
}
#endif

#if COPY_BUFFER_DIM == 1
__kernel void kernel_copy_buffer(__global double* src_buff, __global double* dst_buff,
		int src_offset, int dst_offset, int size)
{

	int i = get_global_id(0);

	if( i >= size ) return;

	int src_off = (src_offset == 0)? 0 : (src_offset)/sizeof(double);
	int dst_off = (dst_offset == 0)? 0 : (dst_offset)/sizeof(double);

//	__global double* src = src_buff + src_off;
//	__global double* dst = dst_buff + dst_off;

	dst_buff[i+dst_off] = src_buff[i+src_off];
}
#else //COPY_BUFFER_DIM == 0
__kernel void kernel_copy_buffer(__global double* src_buff, __global double* dst_buff,
		int src_offset, int dst_offset, int size)
{
	int i;
	int src_off = (src_offset == 0)? 0 : (src_offset)/sizeof(double);
	int dst_off = (dst_offset == 0)? 0 : (dst_offset)/sizeof(double);


	__global double* src = src_buff + src_off;
	__global double* dst = dst_buff + dst_off;

	for(i=0; i<size; i++)
		dst[i] = src[i];
}
#endif
