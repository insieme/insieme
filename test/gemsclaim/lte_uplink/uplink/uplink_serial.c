/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include "stdlib.h"
#include "stdio.h"
#include "def.h"
#include "uplink.h"
#include "uplink_parameters.h"
#include "uplink_verify.h"
#include "kernel_def.h"
#include "interleave_11.h"
#include "ant_comb_7.h"
#include "mf_4.h"
#include "chest_5.h"
#include "fft_8.h"
#include "soft_demap_9.h"
#include "crc_13.h"
#include "turbo_dec_12.h"
#include "weight_calc_6.h"

int main(int argc, char* argv[]) {
  /* Some areas to be used */
  complexMatrix_t comb_w[MAX_SC];
  weightSC_t combWeight[MAX_LAYERS];
  scData_t layer_data[MAX_LAYERS][RX_ANT];  /* Used as scratch-area for all users -> should be indexed by user if parallized over user dimension */
  complex symbols[2*(OFDM_IN_SLOT-1)*MAX_SC*MAX_LAYERS]; /* 2* is for two slots in a subframe */
  complex deint_symbols[2*(OFDM_IN_SLOT-1)*MAX_SC*MAX_LAYERS]; /* 2* is for two slots in a subframe */
  /* The first two is for storing both RE and IM, the second for the two slots */
  char softbits[2*2*(OFDM_IN_SLOT-1)*MAX_SC*MOD_64QAM*MAX_LAYERS];
  unsigned char bits[2*(OFDM_IN_SLOT-1)*MAX_SC*MOD_64QAM/24*MAX_LAYERS];
  unsigned char crc;
  
  int nmbSc;
  int startSc;
  int res_power[4];
  int layer, slot, ofdm, rx, sc;
  int nmbSymbols;
  int nmbSoftbits;
  complexMatrix_t R; /* Correlation matrix */
  int pow[4];
  user_parameters *parameters;
  parameter_model pmodel;
  userS *user;
  
  init_parameter_model(&pmodel);
  init_verify();
  init_data();
  crcInit();
  
  while(1) {
    /* For each subframe, a new set of user parameters is delivered from the
       control plane, we just call a function */
    parameters = uplink_parameters(&pmodel);
    
    /* In this unparallelized program, we process over each user */
    while (parameters->first) {
      user = parameters->first;
      
      startSc = SC_PER_RB*user->startRB;
      nmbSc = SC_PER_RB*user->nmbRB;
      /* OK two slots have always the same set of user_paramters */
      for (slot=0; slot<2; slot++) {
	/* Process each layer seperatly */
	for (layer=0; layer<user->nmbLayer; layer++) {
	  /* Assume we can access the 3:e ofdm symbol (containing reference symbols)
	     OK now call the functions for channel estimate */

	  /* This is hardcoded to 4 RX antennas !!! */
	  mf(&user->data->in_data[slot][3][0][startSc], &user->data->in_rs[slot][startSc][layer], nmbSc, layer_data[layer][0], &pow[0]);
	  mf(&user->data->in_data[slot][3][1][startSc], &user->data->in_rs[slot][startSc][layer], nmbSc, layer_data[layer][1], &pow[1]);
	  mf(&user->data->in_data[slot][3][2][startSc], &user->data->in_rs[slot][startSc][layer], nmbSc, layer_data[layer][2], &pow[2]);
	  mf(&user->data->in_data[slot][3][3][startSc], &user->data->in_rs[slot][startSc][layer], nmbSc, layer_data[layer][3], &pow[3]);

	  ifft(layer_data[layer][0], nmbSc, user->data->fftw[slot]);
	  ifft(layer_data[layer][1], nmbSc, user->data->fftw[slot]);
	  ifft(layer_data[layer][2], nmbSc, user->data->fftw[slot]);
	  ifft(layer_data[layer][3], nmbSc, user->data->fftw[slot]);

	  chest(layer_data[layer][0], pow[0], nmbSc, layer_data[layer][0], &res_power[0]);
	  chest(layer_data[layer][1], pow[1], nmbSc, layer_data[layer][1], &res_power[1]);
	  chest(layer_data[layer][2], pow[2], nmbSc, layer_data[layer][2], &res_power[2]);
	  chest(layer_data[layer][3], pow[3], nmbSc, layer_data[layer][3], &res_power[3]);
	  /* Put power values in the R matrix */
	  R[layer][0] = cmake(res_power[0],0);
	  R[layer][1] = cmake(res_power[1],0);
	  R[layer][2] = cmake(res_power[2],0);
	  R[layer][3] = cmake(res_power[3],0);

	  fft(layer_data[layer][0], nmbSc, user->data->fftw[slot]);
	  fft(layer_data[layer][1], nmbSc, user->data->fftw[slot]);
	  fft(layer_data[layer][2], nmbSc, user->data->fftw[slot]);
	  fft(layer_data[layer][3], nmbSc, user->data->fftw[slot]);
	} /* layer loop */
	
	uplink_layer_verify(user->subframe, layer_data, R, nmbSc, user->nmbLayer, slot);

	/* It's time to combine all layers and RX calc. Call the Combiner
	   weights calc -> will produce a layer X RX_ant X subcarrier matrix */
	comb_w_calc(layer_data, nmbSc, user->nmbLayer, R, comb_w);

	/* Unfortunatly, we have to reorder the weights, in order to be able to
	   split to comming processing inte layers. We can do this either in
	   "comb_w_calc" or "ant_comb" or here: */
        for (rx=0; rx<RX_ANT; rx++)
          for (layer=0; layer<user->nmbLayer; layer++)
	    for (sc=0; sc<nmbSc; sc++)
	      combWeight[layer][sc][rx] = comb_w[sc][layer][rx];

        uplink_weight_verify(user->subframe, combWeight, nmbSc, user->nmbLayer, slot);

	/* We have a lot of channel weights, let's process the user data for
	   each ofdm symbol and each layer In practice, we need to be sure that
	   the ofdm symbols are recived from the radio */
	for (layer=0; layer<user->nmbLayer; layer++) {
	  int ofdm_count = 0;
	  complex* in[4];
	  int index_out;
	  for (ofdm=0; ofdm<OFDM_IN_SLOT; ofdm++) {
	    /* Collect the data for each layer in one vector */
	    if (ofdm != 3) {
	      in[0] = &user->data->in_data[slot][ofdm][0][startSc];
	      in[1] = &user->data->in_data[slot][ofdm][1][startSc];
	      in[2] = &user->data->in_data[slot][ofdm][2][startSc];
	      in[3] = &user->data->in_data[slot][ofdm][3][startSc];
	      /* Put all demodulated symbols in one long vector */
	      index_out = nmbSc*ofdm_count + slot*(OFDM_IN_SLOT-1)*nmbSc + layer*2*(OFDM_IN_SLOT-1)*nmbSc;
	      ant_comb(in, combWeight[layer], nmbSc, &symbols[index_out]);
	      /* Now transform data back to time plane */
	      ifft(&symbols[index_out], nmbSc, user->data->fftw[slot]);
	      ofdm_count++;
	    }
	  }
	}
      } /* slot loop */

      /* OK, we have processed data for one user in 2 slots, let's process it as
	 one big chunk in real, we should divide the data into code block, but
	 in this example we process all data in one block */
      nmbSymbols = 2*nmbSc*(OFDM_IN_SLOT-1)*user->nmbLayer;

      uplink_symbol_verify(user->subframe, symbols, nmbSymbols);

      interleave(symbols, deint_symbols, nmbSymbols);
      uplink_interleave_verify(user->subframe, deint_symbols, nmbSymbols);

      soft_demap(deint_symbols, pow[0], user->mod, nmbSymbols, softbits);
      nmbSoftbits = nmbSymbols * user->mod;
      uplink_verify(user->subframe, softbits, nmbSoftbits);
      
      /* call the turbo decoder and then check CRC */
      turbo_dec(nmbSoftbits);
      crc = crcFast(bits, nmbSoftbits/24);

      parameters->first = user->next;
      free(user);
    } /* user loop */
    free(parameters);
  } /* subframe for loop */

  return 0;
}

