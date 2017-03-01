/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef _COMPLEX_DEF_H
#define _COMPLEX_DEF_H

typedef struct {
  short int re;
  short int im;
} complex;

#define _COMPLEX_DEFINED

#define CSQUARE (x) (x.re*x.re + x.im*x.im)

complex cmul(complex a, complex b);
complex cmulc(complex a, complex b);
int cabs2(complex a);
complex cadd(complex a, complex b);
complex csub(complex a, complex b);
complex cscale(int a, int exp, complex b);
complex cmake(short int re, short int im);
complex cconj(complex a);

#endif
