/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "ant_comb_7.h"

void ant_comb(complex* in[4], weight_t* w, int n, complex* out) {
  int i;
  complex a0, a1, a2, a3, a01, a23;

  /* Calc the weigthed combination of the input data */
  for ( i = 0; i < n; i++ ) {
    a0 = cmul(in[0][i], w[0][i]);
    a1 = cmul(in[1][i], w[1][i]);
    a2 = cmul(in[2][i], w[2][i]);
    a3 = cmul(in[3][i], w[3][i]);
    a01 = cadd(a0, a1);
    a23 = cadd(a2, a3);
    out[i] = cadd(a01, a23);
  }
}
