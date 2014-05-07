/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "mf_4.h"

void mf(complex* in, complex* rs, int n, complex* out, int* power) {
  int i;
  long int ptot = 0;

  /* Calc power */
  for ( i = 0; i < n; i++ )
    ptot += in[i].re * in[i].re + in[i].im * in[i].im;

  /* Calc Mached Filter */
  for ( i = 0; i < n; i++ )
    out[i] = cmul(in[i], rs[i]);

  *power = ptot;
}
