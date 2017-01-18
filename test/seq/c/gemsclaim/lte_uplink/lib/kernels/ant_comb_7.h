/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef _MRC_H
#define _MRC_H

#include "kernel_def.h"

void ant_comb(complex* in[4], weight_t* w, int n, complex* out);

#endif
