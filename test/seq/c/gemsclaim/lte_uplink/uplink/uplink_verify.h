/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#ifndef _UPLINK_VERIFY_H
#define _UPLINK_VERIFY_H

#include "kernel_def.h"
#include "def.h"

#if PARAMETER_MODEL_VERIFICATION
  #define uplink_verify(subframe, data, length) _uplink_verify(subframe, data, length);
#if DETAILED_VERIFICATION_DATA
  #define uplink_layer_verify(subframe, data, R, length, layer, slot) _uplink_layer_verify(subframe, data, R, length, layer, slot)
  #define uplink_weight_verify(subframe, data, length, layer, slot) _uplink_weight_verify(subframe, data, length, layer, slot)
  #define uplink_symbol_verify(subframe, data, length) _uplink_symbol_verify(subframe, data, length)
  #define uplink_interleave_verify(subframe, data, length) _uplink_interleave_verify(subframe, data, length) 
#else
  #define uplink_layer_verify(subframe, data, R, length, layer, slot)
  #define uplink_weight_verify(subframe, data, length, layer, slot)
  #define uplink_symbol_verify(subframe, data, length)
  #define uplink_interleave_verify(subframe, data, length)
#endif
  #define init_verify() _init_verify()
  #define uplink_write_verify_data(subframes) _uplink_write_verify_data(subframes)
#else
  #define uplink_verify(subframe, data, length)
  #define uplink_layer_verify(subframe, data, R, length, layer, slot)
  #define uplink_weight_verify(subframe, data, length, layer, slot)
  #define uplink_symbol_verify(subframe, data, length)
  #define uplink_interleave_verify(subframe, data, length)
  #define init_verify()
  #define uplink_write_verify_data(subframes)
#endif

void _uplink_verify(int subframe, char *data, int length);
void _uplink_layer_verify(int subframe, scData_t data[][RX_ANT], complexMatrix_t R, int length, int layer, int slot);
void _uplink_weight_verify(int subframe, weightSC_t *data, int length, int layer, int slot);
void _uplink_symbol_verify(int subframe, complex *data, int length);
void _uplink_interleave_verify(int subframe, complex *data, int length);
void _init_verify(void);
void _uplink_write_verify_data(int subframes);

short verification_errors;

#endif
