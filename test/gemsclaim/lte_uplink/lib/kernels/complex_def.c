/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "complex_def.h"

complex cmul(complex a, complex b) {
  complex t;

  t.re = a.re * b.re - a.im * b.im;
  t.im = a.im * b.re + a.re * b.im;

  return t;	
}

// mul complex conj
complex cmulc(complex a, complex b) {
  complex t;

  t.re = a.re * b.re + a.im * b.im;
  t.im = a.im * b.re - a.re * b.im;

  return t;
}

 // retuens square of abs
int cabs2(complex a) {
  int t;

  t = a.re * a.re + a.im * a.im;

  return t;
}

complex cadd(complex a, complex b) {
  complex t;

  t.re = a.re + b.re;
  t.im = a.im + b.im;

  return t;
}

complex csub(complex a, complex b) {
  complex t;

  t.re = a.re - b.re;
  t.im = a.im - b.im;

  return t;
}

complex cscale(int a, int exp, complex b) {
  complex t;

  t.re = (a * b.re) >> exp;
  t.im = (a * b.im) >> exp;

  return t;
}

complex cmake(short int re, short int im) {
  complex t;
  t.re = re;
  t.im = im;
  
  return t;
}

complex cconj(complex a) {
  complex t;
  t.re = a.re;
  t.im = -a.im;
  
  return t;
}
