/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include "spr.h"

void setup_counters(int event1, int event2, int event3, int event4) {
  /* Not implemented */
}

void read_counters(int* event1, int* event2, int* event3, int* event4) {
  /* Not implemented */
  *event1 = -1;
  *event2 = -1;
  *event3 = -1;
  *event4 = -1;
}

void clear_counters() {
  /* Not implemented */
}

uint64_t get_cycle_count(){
  /* Not implemented */
  return -1;
}
