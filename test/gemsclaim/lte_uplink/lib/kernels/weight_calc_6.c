/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "weight_calc_6.h"
#include <stdio.h>

#define BETA 18455
#define ALPHA 2300
void mmse_by_cholsolve_4xX_complex(int X, complexMatrix_t *W_p, complexMatrix_t R_p, scData_t in[RX_ANT][MAX_LAYERS], int no_of_sc_for_this_instance, int rho, int beta, int alpha);

void comb_w_calc(scData_t in[RX_ANT][MAX_LAYERS], int nmbSc, int layers, complexMatrix_t R, complexMatrix_t *comb_w) {
  /* In this routine, we have taylor-made processing for different number of layers used (max 4) */
  switch (layers) {
  case 1:
    mmse_by_cholsolve_4xX_complex(1, comb_w, R, in, nmbSc, 23000, BETA, ALPHA);
    break;
  case 2:
    mmse_by_cholsolve_4xX_complex(2, comb_w, R, in, nmbSc, 23000, BETA, ALPHA);
    break;
  case 3:
    mmse_by_cholsolve_4xX_complex(3, comb_w, R, in, nmbSc, 23000, BETA, ALPHA);
    break;
  case 4:
    mmse_by_cholsolve_4xX_complex(4, comb_w, R, in, nmbSc, 23000, BETA, ALPHA);
    break;
  default:
    printf("Error in comb_w_calc; Layers outside 1..4 !!!!!!!!!!!!!!!!!!!!!!!!\n");
  }
}
