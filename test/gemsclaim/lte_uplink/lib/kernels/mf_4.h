/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef _MF_H
#define _MF_H

#include "complex_def.h"

void mf(complex* in, complex* rs, int n, complex* out, int* power);

#endif
