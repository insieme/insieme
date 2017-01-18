/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#define _GNU_SOURCE
#include <sched.h>
#include "affinity.h"

int affinity_set_cpu(int cpu) {
  /* Not implemented */
  return -1;
}
