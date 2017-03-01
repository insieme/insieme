/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#ifndef _SPR_H
#define _SPR_H

#include <stdint.h>

/* Performance counters */
#define BUNDLES_RETIRED 0x6
#define DATA_CACHE_STALL 0x10
#define INST_CACHE_STALL 0x11
#define CACHE_BUSY_STALL 0x18

#ifndef X86
#include "spr_tilera.h"
#else
#include "spr_x86.h"
#endif

void clear_perf_counters();
void setup_counters(int event1, int event2, int event3, int event4);
void read_counters(int* event1, int* event2, int* event3, int* event4);
void clear_counters();

#endif
