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

#include "npbparams.h"
#include "type.h"

#define NTOTAL_F        (1.0*NX*NY*NZ)


// 2D processor array -> 2D grid decomposition (by pencils)
// If processor array is 1xN or -> 1D grid decomposition (by planes)
// If processor array is 1x1 -> 0D grid decomposition
// For simplicity, do not treat Nx1 (np2 = 1) specially
/* common /procgrid/ */
int np1, np2, np;

// basic decomposition strategy
int layout_type;
#define layout_0D  0
#define layout_1D  1
#define layout_2D  2


// Cache blocking params. These values are good for most
// RISC processors.  
// FFT parameters:
//  fftblock controls how many ffts are done at a time. 
//  The default is appropriate for most cache-based machines
//  On vector machines, the FFT can be vectorized with vector
//  length equal to the block size, so the block size should
//  be as large as possible. This is the size of the smallest
//  dimension of the problem: 128 for class A, 256 for class B and
//  512 for class C.
// Transpose parameters:
//  transblock is the blocking factor for the transposes when there
//  is a 1-D layout. On vector machines it should probably be
//  large (largest dimension of the problem).


#define FFTBLOCK_DEFAULT      16
#define FFTBLOCKPAD_DEFAULT   18
#define TRANSBLOCK            32
#define TRANSBLOCKPAD         34

/* common /blockinfo/ */
int fftblock, fftblockpad;

// we need a bunch of logic to keep track of how
// arrays are laid out. 
// coords of this processor
/* common /coords/ */
int *me1, *me2;
//int *me, *me1, *me2;

// need a communicator for row/col in processor grid
/* common /comms/ */
int **commslice1, **commslice2;
//MPI_Comm commslice1, commslice2;



// There are basically three stages
// 1: x-y-z layout
// 2: after x-transform (before y)
// 3: after y-transform (before z)
// The computation proceeds logically as

// set up initial conditions
// fftx(1)
// transpose (1->2)
// ffty(2)
// transpose (2->3)
// fftz(3)
// time evolution
// fftz(3)
// transpose (3->2)
// ffty(2)
// transpose (2->1)
// fftx(1)
// compute residual(1)

// for the 0D, 1D, 2D strategies, the layouts look like xxx
//        
//            0D        1D        2D
// 1:        xyz       xyz       xyz
// 2:        xyz       xyz       yxz
// 3:        xyz       zyx       zxy

// the array dimensions are stored in dims[phase][coord]
/* common /layout/ */
int dims[3][3];
int (*xstart)[3], (*ystart)[3], (*zstart)[3];
int (*xend)[3], (*yend)[3], (*zend)[3];

#define T_total         0
#define T_setup         1
#define T_fft           2 
#define T_evolve        3
#define T_checksum      4
#define T_fftx          5
#define T_ffty          6
#define T_fftz          7
#define T_transpose     8
#define T_transxzloc    9
#define T_transxzglo    10
#define T_transxzfin    11
#define T_transxyloc    12
#define T_transxyglo    13
#define T_transxyfin    14
#define T_init          15
#define T_max           16


// other stuff
/* common /dbg/ */
logical timers_enabled;
logical debug;
//static logical debugsynch;

#define SEED          314159265.0
#define A             1220703125.0
#define PI            3.141592653589793238
#define ALPHA         1.0e-6

// roots of unity array
// relies on x being largest dimension?
/* common /ucomm/ */
dcomplex u[NX];


// for checksum data
/* common /sumcomm/ */
dcomplex sums[NITER_DEFAULT+1];

// number of iterations
/* common /iter/ */
int niter;


#define dcmplx(r,i)       (dcomplex){r, i}
#define dcmplx_add(a,b)   (dcomplex){(a).real+(b).real, (a).imag+(b).imag}
#define dcmplx_sub(a,b)   (dcomplex){(a).real-(b).real, (a).imag-(b).imag}
#define dcmplx_mul(a,b)   (dcomplex){((a).real*(b).real)-((a).imag*(b).imag),\
                                     ((a).real*(b).imag)+((a).imag*(b).real)}
#define dcmplx_mul2(a,b)  (dcomplex){(a).real*(b), (a).imag*(b)}
static inline dcomplex dcmplx_div(dcomplex z1, dcomplex z2) {
  double a = z1.real;
  double b = z1.imag;
  double c = z2.real;
  double d = z2.imag;

  double divisor = c*c + d*d;
  double real = (a*c + b*d) / divisor;
  double imag = (b*c - a*d) / divisor;
  dcomplex result = (dcomplex){real, imag};
  return result;
}
#define dcmplx_div2(a,b)  (dcomplex){(a).real/(b), (a).imag/(b)}
#define dcmplx_abs(x)     sqrt(((x).real*(x).real) + ((x).imag*(x).imag))

#define dconjg(x)         (dcomplex){(x).real, -1.0*(x).imag}

