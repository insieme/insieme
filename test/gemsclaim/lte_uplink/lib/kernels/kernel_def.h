/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef _KERNEL_DEF_H
#define _KERNEL_DEF_H

#include "def.h"
#include "complex_def.h"

//TODO: restore enum
//typedef enum mod_type_s {
//	MOD_PSK = 1,
//	MOD_QPSK = 2,
//	MOD_16QAM = 4,
//	MOD_64QAM = 6
//} mod_type;

#define	MOD_PSK     1
#define	MOD_QPSK    2
#define	MOD_16QAM   4
#define	MOD_64QAM   6

typedef complex scData_t[MAX_SC];
typedef scData_t rxData_t[RX_ANT];
typedef rxData_t layerData_t[MAX_LAYERS];

typedef complex weight_t[RX_ANT];
typedef weight_t weightSC_t[MAX_SC];

typedef complex complexMatrix_t[RX_ANT][MAX_LAYERS];
typedef complex channelEst_t[RX_ANT][MAX_LAYERS][MAX_RB];
typedef scData_t combWeights_t[MAX_LAYERS][RX_ANT];
typedef complex combWeightsPerLayer_t[MAX_SC][RX_ANT];
typedef int __fixed;

#endif
