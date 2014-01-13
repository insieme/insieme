/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef _SOFT_DEMAP_H
#define _SOFT_DEMAP_H

#include "kernel_def.h"

void soft_demap(complex* in, int noise, int mod, int n, char* out);

#endif
