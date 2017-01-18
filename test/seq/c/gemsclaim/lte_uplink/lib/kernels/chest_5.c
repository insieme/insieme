/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "chest_5.h"

#define TAP_FAC	4

void chest(complex* in, int pow, int n, complex* out, int* res_power) {
  int i;
  long int ptot = 0;
  int used_sc = n/TAP_FAC;    // n will always be >= 12 and possible to divide by 4

  // Calc used power
  for (i=0; i<used_sc; i++)
    ptot += in[i].re * in[i].re + in[i].im * in[i].im;

  ptot = ptot >> 16;
  *res_power = pow - ptot;

  if (*res_power < 0) 
    *res_power = 1;

  // Copy the used taps-coeffs
  for (i=0; i<used_sc; i++)
    out[i] = in[i];

  // Reset the rest of the coefficients
  for (i=used_sc; i<n; i++) {
    out[i].re = 0;
    out[i].im = 0;
  }
}
