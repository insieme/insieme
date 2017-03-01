/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef _WEIGHT_CALC_H
#define _WEIGHT_CALC_H

#include "kernel_def.h"

void comb_w_calc(scData_t in[RX_ANT][MAX_LAYERS], int nmbSc, int layers, complexMatrix_t R, complexMatrix_t* comb_w);

#endif
