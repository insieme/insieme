/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "feldspar.h"

void mkComplexReal(RealNum re, RealNum im , complex *c) {
#if FLUNK
  *c=complex_make(cast_itof(re), cast_itof(im));
#else
  (*c).re=re;
  (*c).im=im;
#endif
}

void mul_RealNum(RealNum a, RealNum b , RealNum *c) {
  *c=a*b;
}

void div_RealNum(RealNum a, RealNum b , RealNum *c) {
  *c=a/b;
}

void sub_RealNum(RealNum a, RealNum b , RealNum *c) {
  *c=a-b;
}

void mul_ComplexReal(complex a, complex b, complex *c) {	
#if FLUNK
  *c=complex_mult(a, b);
#else
  (*c).re=((a.re * b.re) - (a.im * b.im));
  (*c).im=((a.re * b.im) + (a.im * b.re));
#endif
}

void quot(signed int numerator, signed int denominator, signed int *out) {
  *out=numerator/denominator;
}
