/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include "spr.h"

#define SPR_PERF_COUNT_CTL  0x4207
#define SPR_AUX_PERF_COUNT_CTL  0x6007

#define SPR_PERF_COUNT_0 0x4205
#define SPR_PERF_COUNT_1 0x4206
#define SPR_AUX_PERF_COUNT_0 0x6005
#define SPR_AUX_PERF_COUNT_1 0x6006

inline void setup_counters(int event1, int event2, int event3, int event4)
{
  __insn_mtspr(SPR_PERF_COUNT_CTL, event1 | (event2 << 16));
  __insn_mtspr(SPR_AUX_PERF_COUNT_CTL, event3 | (event4 << 16));
}

inline void read_counters(int* event1, int* event2, int* event3, int* event4)
{
  *event1 = __insn_mfspr(SPR_PERF_COUNT_0);
  *event2 = __insn_mfspr(SPR_PERF_COUNT_1);
  *event3 = __insn_mfspr(SPR_AUX_PERF_COUNT_0);
  *event4 = __insn_mfspr(SPR_AUX_PERF_COUNT_1);
}

inline void clear_counters()
{
  __insn_mtspr(SPR_PERF_COUNT_0, 0);
  __insn_mtspr(SPR_PERF_COUNT_1, 0);
  __insn_mtspr(SPR_AUX_PERF_COUNT_0, 0);
  __insn_mtspr(SPR_AUX_PERF_COUNT_1, 0);
}
