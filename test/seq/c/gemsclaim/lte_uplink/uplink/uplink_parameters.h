/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#ifndef _UPLINK_PARAMETERS_H
#define _UPLINK_PARAMETERS_H

#include "uplink.h"
#include "ulRsGen.h"

typedef struct user_parameters_s {
  userS *first;
  userS *last;
} user_parameters;

typedef struct parameter_model_s {
  short active;
  short inc;
  short count;
  short countdown;
  int active_min;
  int active_max;
  int active_sum;
  int active_frames;
  int activity[5];
} parameter_model;

void init_parameter_model(parameter_model *pmodel);
user_parameters *uplink_parameters(parameter_model *pmodel);

void init_data(void);

#endif
