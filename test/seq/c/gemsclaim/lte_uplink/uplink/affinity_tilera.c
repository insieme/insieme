/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include "affinity.h"
#include <tmc/cpus.h>
#include <tmc/task.h>

int affinity_set_cpu(int id) {
  cpu_set_t cpus;
  int cpu = -1;

  if (tmc_cpus_get_my_affinity(&cpus) != 0)
    tmc_task_die("tmc_cpus_get_my_affinity() failed.");

  /* Given this task’s rank, bind to the appropriate CPU. */
  cpu = tmc_cpus_find_nth_cpu(&cpus, id);
  if (tmc_cpus_set_my_cpu(cpu) < 0)
    tmc_task_die("tmc_cpus_set_my_cpu() failed.");

  return cpu;
}
