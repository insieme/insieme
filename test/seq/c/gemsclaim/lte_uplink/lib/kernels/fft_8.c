/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Note: This is not a correct FFT/IFFT, but it has approximatly the           *
 *       same number of operations as a proper implementation.                 *
 ******************************************************************************/

#include "fft_8.h"
#include "stdlib.h"

#define SHIFT 15
#define __labs abs

void ifft(complex* io, int n, complex* w) {
  // just call the FFT, same number of operations
  fft(io, n, w);
}

void fft(complex* io, int n, complex* w) {
  short n1, n2, index_delta, i, j, k;
  short real_diff,imag_diff, weight_real, weight_imag;
  int real, imag;
  complex *out0_p, *out1_p, *in0_p, *in1_p;
  short ni,n0;
  complex *weight_p;
  short real_0, imag_0, real_1, imag_1;
    
  n2 = n;
  n1 = n;
  ni = 1;
  index_delta = 1;
  for (k=n; k > 1; k = (k >> 1) ) {
    n0 = n1>>1;
    n1 = n2>>1;
    n2 = n2>>1;
    in0_p = io;
    in1_p = io + n1;
    weight_p = w;
    for (j=0; j < n2; j++) {
      weight_real = weight_p->re;
      weight_imag = weight_p->im;
      weight_p += index_delta;
      out0_p = in0_p;
      out1_p = in1_p;
      for (i = 0; i < ni; i++) {
	real_0 = out0_p->re;
	imag_0 = out0_p->im;
	real_1 = out1_p->re;
	imag_1 = out1_p->im;
	
	real_diff   = real_1 - real_0;
	out0_p->re  = (real_0 + real_1)>>1;
	imag_diff   = imag_1 - imag_0;
	out0_p->im  = (imag_0 + imag_1)>>1;
	real	    = weight_real*real_diff;
	imag	    = weight_real*imag_diff;
	real       += weight_imag*imag_diff;
	imag	   -= weight_imag*real_diff;
	out1_p->re  = real>>SHIFT;
	out1_p->im  = imag>>SHIFT;
	out0_p     += n0;
	out1_p     += n0;
      }
      in0_p++;
      in1_p++;
    }
    index_delta = index_delta << 1;
    ni = ni << 1;
  }
}
