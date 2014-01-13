/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef _CHEST_H
#define _CHEST_H

#include "complex_def.h"

void chest(complex* in, int pow, int n, complex* out, int* res_power);

#endif
