/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include "def.h"
#include "uplink.h"
#include "uplink_parameters.h"
#include "uplink_verify.h"
#include "uplink_alarm.h"
#include "affinity.h"
#include "crc_13.h"

/* Global queues */
static queues queue;

int main(int argc, char *argv[]) {  
  /* User info */
  user_parameters *parameters;
  parameter_model pmodel;

  int rc = 0;
  int i  = 0;
  pthread_t threads[TASK_THREADS];
  task_args args[TASK_THREADS];

  short userq_empty = 0;
  short userq_count = 0;

  /* Generate input data */
  init_data();
  init_verify();
  crcInit();

  /* Initialize the amount of work to zero */
  queue.users.first = NULL;

  init_parameter_model(&pmodel);

  /* Setup mutexe for the user queue */
  rc = pthread_mutex_init(&queue.users.lock, NULL);
  if (rc){
    printf("ERROR; return code from pthread_mutex_init() is %d\n", rc);
    return -1;
  }
  /* Make sure the queue is empty */
  queue.users.first = NULL;
  queue.users.last = NULL;

  /* Setup mutexes for the user task queues */
  for (i=0; i<TASK_THREADS; i++) {
    rc = pthread_mutex_init(&queue.threads[i].lock, NULL);
    if (rc){
      printf("ERROR; return code from pthread_mutex_init() is %d\n", rc);
      return -1;
    }
    /* Make sure the queue is empty */
    queue.threads[i].first = NULL;
    queue.threads[i].last  = NULL;
  }

  /* Start a number of worker threads */
  for (i=0; i<TASK_THREADS; i++) {
    /* Setup arguments */
    args[i].id     = i;
    args[i].active = &pmodel.active;
    args[i].queue  = &queue; 

    /* Start thread */
    rc = pthread_create(&threads[i], NULL, uplink_task, &args[i]);
    if (rc){
      printf("ERROR; return code from pthread_create() is %d\n", rc);
      return -1;
    }
  }

  /* Set the affinity of the main thread to its own core */
  affinity_set_cpu(i);

  /* Initialize the alarm that will signal every DELTA microsecons */
  /* DELTA is defined in def.h */
  uplink_alarm_init(DELTA);

  /* Main loop that submits a subframe for execution every DELTA microseconds */
  while (1) {
    /* Generate parameters for next frame */
    parameters = uplink_parameters(&pmodel);

    /* Wait until next subframe should be computed */
    uplink_wait_for_alarm();

    /* Add users to queue if there are any */
    if (parameters) {
      pthread_mutex_lock(&queue.users.lock);
      if (queue.users.first == NULL) {
	/* Queue is empty so add new users to the beginning of the queue */
	queue.users.first = parameters->first;
	userq_empty = 1;
      } else {
	/* Queue is not empty so add new users to the end of the queue */
	queue.users.last->next = parameters->first;
      }
      /* Set the end of the queue to the last of the added users */
      queue.users.last = parameters->last;
      pthread_mutex_unlock(&queue.users.lock);
      free(parameters);
    }

    /* Check if we are keeping up with the workload */
    if (userq_empty) {
      userq_empty = 0;
      userq_count = 0;
    } else if (userq_count == 10) {
      printf("The user queue has not been empty for the last 10 subframes.\n");
      printf("Not keeping up with workload, so will exit the execution.\n");
      printf("Try modifying DELTA in def.h to reduce the workload.\n");
      exit(0);
    } else {
      userq_count++;
    }
  }
}
