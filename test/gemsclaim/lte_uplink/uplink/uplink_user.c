/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include "kernel_def.h"
#include "uplink.h"
#include "uplink_verify.h"
#include "fence.h"
#include "interleave_11.h"
#include "ant_comb_7.h"
#include "mf_4.h"
#include "chest_5.h"
#include "fft_8.h"
#include "soft_demap_9.h"
#include "weight_calc_6.h"
#include "crc_13.h"
#include "turbo_dec_12.h"

inline void wait_until_computed(task *tasks, int items) {
  int i=0;

  while(i<items) {
    while(tasks[i].computed==false);
    i++;
  }
}

inline void compute_chest(task *task) {
  symbol_data *symbolData = task->symbolData;
  int rx = task->rx;
  int layer = task->layer;
  int res_power[4] = {0, 0, 0, 0};

  mf(&symbolData->data->in_data[symbolData->slot][3][rx][symbolData->startSc], &symbolData->data->in_rs[symbolData->slot][symbolData->startSc][layer], symbolData->nmbSc, symbolData->layer_data[layer][rx], &symbolData->pow[rx]);
  ifft(symbolData->layer_data[layer][rx], symbolData->nmbSc, symbolData->data->fftw[symbolData->slot]);
  chest(symbolData->layer_data[layer][rx], symbolData->pow[rx], symbolData->nmbSc, symbolData->layer_data[layer][rx], &res_power[rx]);
  /* Put power values in the R matrix */
  symbolData->R[layer][rx] = cmake(res_power[rx],0);
  fft(symbolData->layer_data[layer][rx], symbolData->nmbSc, symbolData->data->fftw[symbolData->slot]);
  /* Mark the task as computed */
  mem_fence();
  task->computed = true;
}

inline void compute_symbol(task *task) {
  symbol_data *symbolData = task->symbolData;
  int ofdm = task->ofdm;
  int ofdm_count = task->ofdm_count;
  int layer = task->layer;
  int nmbSc = symbolData->nmbSc;
  int slot = symbolData->slot;
  complex* in[4];
  int index_out;

  in[0] = &symbolData->data->in_data[symbolData->slot][ofdm][0][symbolData->startSc];
  in[1] = &symbolData->data->in_data[symbolData->slot][ofdm][1][symbolData->startSc];
  in[2] = &symbolData->data->in_data[symbolData->slot][ofdm][2][symbolData->startSc];
  in[3] = &symbolData->data->in_data[symbolData->slot][ofdm][3][symbolData->startSc];
  /* Put all demodulated symbols in one long vector */
  index_out = nmbSc*ofdm_count + slot*(OFDM_IN_SLOT-1)*nmbSc + layer*2*(OFDM_IN_SLOT-1)*nmbSc;
  ant_comb(in, symbolData->combWeight[layer], nmbSc, &symbolData->symbols[index_out]);
  /* Now transform data back to time plane */
  ifft(&symbolData->symbols[index_out], nmbSc, symbolData->data->fftw[symbolData->slot]);
  /* Mark the task as computed */
  mem_fence();
  task->computed = true;
}

void uplink_user(task_queue *queue, userS *user, task *tasks) {
  mod_type mod    = user->mod;
  int startSc     = user->startRB*SC_PER_RB;
  int nmbSc       = user->nmbRB*SC_PER_RB;
  int nmbLayer    = user->nmbLayer;
  int nmbSymbols  = 2*nmbSc*(OFDM_IN_SLOT-1)*nmbLayer; /* 2* is for two slots in a subframe */
  int nmbSoftbits = nmbSymbols * mod;
  int layer;
  int rx;
  int ofdm;
  int slot;
  int sc;
  int i;

  /* All tasks have the same symbolData and computed so we update the
     first with necessary data */
  symbol_data *symbolData = tasks[0].symbolData;

  /* Output place holders */
  int *pow                = symbolData->pow;
  complexMatrix_t *comb_w = symbolData->comb_w;
  weightSC_t *combWeight  = symbolData->combWeight;
  complex *symbols        = symbolData->symbols;
  complex *deint_symbols  = symbolData->deint_symbols;
  char *softbits          = symbolData->softbits;
  unsigned char *bits     = symbolData->bits;

  symbolData->startSc = startSc;
  symbolData->nmbSc   = nmbSc;
  symbolData->data    = user->data;

  for (slot=0; slot<2; slot++) {
    symbolData->slot = slot;

    /* Process each layer separately
       Assume we can access the 4th ofdm symbol (containing reference symbols)
       Prepare chest items to be put on queue
       Reduces the amount of time in the critical section */
    i = 0;
    for (layer=0; layer<nmbLayer; layer++) {
      for (rx=0; rx<4; rx++) { 
	tasks[i].type     = CHANNEL;
	tasks[i].layer    = layer;
	tasks[i].rx       = rx;
        tasks[i].computed = false;
	if (i != 0) {
	  tasks[i-1].next = &tasks[i];
	}
	i++;
      }
    }
    tasks[i-1].next = NULL;

    /* Add prepared items to private queue */
    pthread_mutex_lock(&(queue->lock));
    if (queue->first == NULL) {
      queue->first = tasks;
    } else {
      queue->last->next = tasks;
    }
    queue->last = &tasks[i-1];
    pthread_mutex_unlock(&(queue->lock));

    /* Process all items that are in the queue */
    while(queue->first)
      handle_task(queue);

    /* Wait until this user's channel estimation items are computed */
    wait_until_computed(tasks, i);

    uplink_layer_verify(user->subframe, symbolData->layer_data, symbolData->R, nmbSc, nmbLayer, slot);

    /* It's time to combine all layers and RX calc. Call the Combiner weights
       calc -> will produce a layer X RX_ant X subcarrier matrix. */
    comb_w_calc(symbolData->layer_data, nmbSc, nmbLayer, symbolData->R, comb_w);


    /* Unfortunatly, we have to reorder the weights, in order to be
       able to split to comming processing inte layers. We can do this
       either in "comb_w_calc", "ant_comb", or here: */
    for (rx=0; rx<RX_ANT; rx++)
      for (layer=0; layer<user->nmbLayer; layer++)
        for (sc=0; sc<nmbSc; sc++)
          combWeight[layer][sc][rx] = comb_w[sc][layer][rx];

    uplink_weight_verify(user->subframe, combWeight, nmbSc, nmbLayer, slot);

    /* We have a lot of channel weights, let's process the user data
       for each ofdm symbol and each layer.  In practice, we need to
       be sure that the ofdm symbols are recived from the radio. */
    i = 0;
    for (layer=0; layer<nmbLayer; layer++) {
      int ofdm_count = 0;
      for (ofdm = 0; ofdm<OFDM_IN_SLOT; ofdm++) {
	if (ofdm != 3) {
	  tasks[i].type       = SYMBOL;
	  tasks[i].ofdm       = ofdm;
	  tasks[i].ofdm_count = ofdm_count;
	  tasks[i].layer      = layer;
	  tasks[i].computed   = false;
	  if (i != 0) {
	    tasks[i-1].next   = &tasks[i];
	  }
	  ofdm_count++;
	  i++;
	}
      }
    }
    tasks[i-1].next = NULL;

    /* Put all symbols on the queue */
    pthread_mutex_lock(&(queue->lock));
    if (queue->first == NULL) {
      queue->first = tasks;
    } else {
      queue->last->next = tasks;
    }
    queue->last = &tasks[i-1];
    pthread_mutex_unlock(&(queue->lock));

    /* Process all items that are in the queue */
    while(queue->first)
      handle_task(queue);

    /* Wait until this user's data symbol items are computed */
    wait_until_computed(tasks, i);
  } /* slot loop */

  /* OK, we have processed data for one user in 2 slots, let's process
     it as one big chunk. In real, we should divide the data into
     code block, but in this example we process all data in one
     block. */
  uplink_symbol_verify(user->subframe, symbols, nmbSymbols);

  interleave(symbols, deint_symbols, nmbSymbols);

  uplink_interleave_verify(user->subframe, deint_symbols, nmbSymbols);

  soft_demap(deint_symbols, pow[0], mod, nmbSymbols, softbits);

  uplink_verify(user->subframe, softbits, nmbSoftbits);

  /* call the turbo decoder and then check CRC */
  turbo_dec(nmbSoftbits);
  crcFast(bits, nmbSoftbits/24);
}
