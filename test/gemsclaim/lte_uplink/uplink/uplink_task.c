/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "uplink.h"
#include "affinity.h"
#include "spr.h"
#include "active.h"

inline void handle_user(user_queue *queue, task_queue *tqueue, task *tasks) {
  userS * user = NULL;

  pthread_mutex_lock(&(queue->lock));
  if (queue->first) {
    user = queue->first;
    queue->first = user->next;
  }
  pthread_mutex_unlock(&(queue->lock));

  if (user) {
    uplink_user(tqueue, user, tasks);
    /* If data would be unique for each frame a check would be
       required here to check if all users are completed and if so
       free the allocated memory. A structure for keeping track of
       unfinished users would therefore be required. */
    free(user);
  }
}

inline void handle_task(task_queue *queue) {
  task *task = NULL;

  pthread_mutex_lock(&(queue->lock));
  if (queue->first) {
    task = queue->first;
    queue->first = task->next;
  }
  pthread_mutex_unlock(&(queue->lock));

  if (task) {
    switch(task->type) {
    case CHANNEL:
      compute_chest(task);
      break;
    case SYMBOL:
      compute_symbol(task);
      break;
    default:
      printf("Encounterd unsupported task type: %i\n", task->type);
      exit(1);
    }
  }
}

int find_work(task_queue *queues, const int id) {
  int i;
  int idle         = 1;
  uint64_t  cycles = 0;
#if PARAMETER_MODEL_CORRELATION
  uint64_t  delta  = 0;
#endif

  for (i=0; i<TASK_THREADS; i++) {
    /* Check if queue has work, 
       skip id as it is the threads own queue */
    while (queues[i].first && i!=id) {
#if !PARAMETER_MODEL_CORRELATION
      handle_task(&queues[i]);
#else
      delta = get_cycle_count();
      handle_task(&queues[i]);
      cycles += get_cycle_count()-delta;
#endif
      idle = 0;
    }
  }
  /* Nothing to do so lets wait a while */
  if (idle) {
    usleep(DELTA/10);
  }
  return cycles;
}

void *uplink_task(void *args) {
  task_args *targs         = (task_args *)args;
  user_queue *user_queue   = &targs->queue->users;
  task_queue *task_queue   = &targs->queue->threads[targs->id];
  symbol_data *symbolData  = &global_symbolData[targs->id];
  task *tasks              = global_tasks[targs->id];
  int i;
#if PARAMETER_MODEL_CORRELATION
  uint64_t  start          = 0;
  uint64_t  active         = *targs->active;
  uint64_t  delta          = 0;
  uint64_t  cycles         = 0;
  start = get_cycle_count();
#endif

  affinity_set_cpu(targs->id);

  /* Set up necessary structures */
  for (i=0; i<(OFDM_IN_SLOT-1)*MAX_LAYERS; i++) {
    tasks[i].symbolData = symbolData;
  }

  /* Look for work to perform */
  while(1) {
#if DEACTIVATE_CORES
    if (*targs->active < targs->id) {
      deactivate_cpu();
    } else 
#endif

#if !PARAMETER_MODEL_CORRELATION
    if (user_queue->first) {
      handle_user(user_queue, task_queue, tasks);
    } else {
      find_work(targs->queue->threads, targs->id);
    }
#else
    if (user_queue->first) {
      delta = get_cycle_count();
      handle_user(user_queue, task_queue, tasks);
      cycles += get_cycle_count()-delta;
    } else {
      cycles += find_work(targs->queue->threads, targs->id);
      if (active!=*targs->active) {
	uint64_t now = get_cycle_count();
	uint64_t elapsed_time = now-start;
	printf("%2.2f\n", cycles/(float)elapsed_time*100);
	start  = now;
	cycles = 0;
	active = *targs->active;
      }
    }
#endif
  }
}
