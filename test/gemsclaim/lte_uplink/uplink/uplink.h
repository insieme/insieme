/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#ifndef _UPLINK_H
#define _UPLINK_H

#include <stdio.h>
#include "def.h"
#include "kernel_def.h"

typedef enum tast_type_e {
  CHANNEL = 1,
  SYMBOL  = 2
} task_type;

typedef struct input_data_s {
  complex in_data[NMB_SLOT][OFDM_IN_SLOT][RX_ANT][MAX_SC];
  complex in_rs[NMB_SLOT][MAX_SC][MAX_LAYERS];
  complex fftw[NMB_SLOT][MAX_SC];
} input_data;

typedef struct userS userS;

struct userS {
  int nmbLayer;
  int startRB;
  int nmbRB;
  //mod_type mod;
  int mod;
  userS *next;
  input_data *data;
  int subframe;
};

typedef struct symbol_data_s {
  /* Raw input data */
  input_data *data;
  /* Meta info */
  int startSc;
  int nmbSc;
  int slot;
  /* Place holders for computed results */
  scData_t layer_data[MAX_LAYERS][RX_ANT];
  complexMatrix_t comb_w[MAX_SC];
  weightSC_t combWeight[MAX_LAYERS];
  complex symbols[2*(OFDM_IN_SLOT-1)*MAX_SC*MAX_LAYERS];
  complex deint_symbols[2*(OFDM_IN_SLOT-1)*MAX_SC*MAX_LAYERS];
  complexMatrix_t R;
  int pow[RX_ANT];
  char softbits[2*2*(OFDM_IN_SLOT-1)*MAX_SC*MOD_64QAM*MAX_LAYERS]; /* The first two is for storing both RE and IM, the second for the two slots */
  unsigned char bits[2*(OFDM_IN_SLOT-1)*MAX_SC*MOD_64QAM/24*MAX_LAYERS];
} symbol_data;

/*typedef enum {
  true  = 1,
  false = 0
} bool;*/
#include <stdbool.h>


typedef struct task task;

struct task{
  task_type type;
  int layer;
  int rx;
  int ofdm;
  int ofdm_count;
  symbol_data *symbolData;
  volatile bool computed;
  /* Next task when used in a linked list */
  task *next;
};

typedef struct user_queue_s {
  pthread_mutex_t lock;
  userS *first;
  userS *last;
} user_queue;

typedef struct task_queue_s {
  pthread_mutex_t lock;
  task *first;
  task *last;
} task_queue;

typedef struct queues_s {
  user_queue users;
  task_queue threads[TASK_THREADS];
} queues;

typedef struct task_args_s {
  short id;
  short  *active;
  queues *queue;
} task_args;

void uplink_user(task_queue *queue, userS *user, task *tasks);
void handle_task(task_queue *queue);
void compute_symbol(task *task);
void compute_chest(task *task);
void *uplink_task(void *args);

symbol_data global_symbolData[TASK_THREADS];
task global_tasks[TASK_THREADS][(OFDM_IN_SLOT-1)*MAX_LAYERS];

#endif
