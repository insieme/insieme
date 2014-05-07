/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifndef FELDSPAR_INTEL_FUNCTIONS_H
#define FELDSPAR_INTEL_FUNCTIONS_H

void mkComplexReal(RealNum re, RealNum im , complex *c);
void mul_RealNum(RealNum a, RealNum b , RealNum *c);
void div_RealNum(RealNum a, RealNum b , RealNum *c);
void sub_RealNum(RealNum a, RealNum b , RealNum *c);
void mul_ComplexReal(complex a, complex b, complex *c);
void quot(signed int numerator, signed int denominator, signed int *out);

#endif

#ifdef __cplusplus
}
#endif
