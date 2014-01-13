/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include "uplink.h"
#include "uplink_parameters.h"
#include "uplink_verify.h"
#include "def.h"

static int time_delta;
static int subframe;

/* Input data */
static input_data data[NMB_DATA_FRAMES];

#if PARAMETER_MODEL_VERIFICATION
void init_data(void) {
  int ofdm, frame;
  int i,j,slot;
  unsigned short int inc = 0;
  unsigned short int dec = 16500;

  for (frame=0; frame<NMB_DATA_FRAMES; frame++) {
    for (slot=0; slot<NMB_SLOT; slot++) { 
      for (ofdm=0; ofdm<OFDM_IN_SLOT; ofdm++) {
	for (i=0; i<MAX_SC; i++) {
	  data[frame].in_data[slot][ofdm][0][i].re = inc++;
	  data[frame].in_data[slot][ofdm][0][i].im = inc++;
	  data[frame].in_data[slot][ofdm][1][i].re = dec--;
	  data[frame].in_data[slot][ofdm][1][i].im = dec--;
	  data[frame].in_data[slot][ofdm][2][i].re = inc++;
	  data[frame].in_data[slot][ofdm][2][i].im = dec--;
	  data[frame].in_data[slot][ofdm][3][i].re = dec--;
	  data[frame].in_data[slot][ofdm][3][i].im = inc++;
	}
      }
      for (i=0; i<MAX_SC; i++) {
	data[frame].fftw[slot][i].re  = inc++;
	data[frame].fftw[slot][i].im  = dec--;
      }
      for (i=0; i<MAX_SC; i++) {
	for (j=0; j<MAX_LAYERS; j++) {
	  data[frame].in_rs[slot][i][j].re = dec--;
	  data[frame].in_rs[slot][i][j].im = inc++;
	}
      }
    }
  }
}
#else
void init_data(void) {
  int ofdm, frame;
  int i,slot;

  for (frame=0; frame<NMB_DATA_FRAMES; frame++) {
    for (slot=0; slot<NMB_SLOT; slot++) { 
      for (ofdm=0; ofdm<OFDM_IN_SLOT; ofdm++) {
	for (i=0; i<MAX_SC; i++) {
	  data[frame].in_data[slot][ofdm][0][i].re = rand();
	  data[frame].in_data[slot][ofdm][0][i].im = rand();
	  data[frame].in_data[slot][ofdm][1][i].re = rand();
	  data[frame].in_data[slot][ofdm][1][i].im = rand();
	  data[frame].in_data[slot][ofdm][2][i].re = rand();
	  data[frame].in_data[slot][ofdm][2][i].im = rand();
	  data[frame].in_data[slot][ofdm][3][i].re = rand();
	  data[frame].in_data[slot][ofdm][3][i].im = rand();
	}
      }
      for (i=0; i<MAX_SC; i++) {
	data[frame].fftw[slot][i].re = rand();
	data[frame].fftw[slot][i].im = rand();
      }
    }
    /* One reference symbol per layer */
    for (i=0; i<MAX_LAYERS; i++)
      RsGen(MAX_RB, data[frame].in_rs[slot][i]);
  }
}
#endif

int new_second(void) {
  time_delta += DELTA;
  if (time_delta >= 1000000) {
    time_delta = 0;
    return 1;
  }
  return 0;
}

int subframe_data(void) {
  return (subframe++)%NMB_DATA_FRAMES;
}

#define L1_QPSK  15
#define L2_QPSK  28
#define L3_QPSK  40
#define L4_QPSK  55
#define L1_16QAM 23
#define L2_16QAM 43
#define L3_16QAM 65
#define L4_16QAM 85
#define L1_64QAM 27
#define L2_64QAM 53
#define L3_64QAM 78
#define L4_64QAM 98.0

float user_load(int nmbRB, int nmbLayer, int mod) {
  switch(mod) {
  case MOD_QPSK:
    switch(nmbLayer) {
    case 1:
      return nmbRB*L1_QPSK/100;
    case 2:
      return nmbRB*L2_QPSK/100;
    case 3:
      return nmbRB*L3_QPSK/100;
    case 4:
      return nmbRB*L4_QPSK/100;
    }
  case MOD_16QAM:
    switch(nmbLayer) {
    case 1:
      return nmbRB*L1_16QAM/100;
    case 2:
      return nmbRB*L2_16QAM/100;
    case 3:
      return nmbRB*L3_16QAM/100;
    case 4:
      return nmbRB*L4_16QAM/100;
    }
  case MOD_64QAM:
    switch(nmbLayer) {
    case 1:
      return nmbRB*L1_64QAM/100;
    case 2:
      return nmbRB*L2_64QAM/100;
    case 3:
      return nmbRB*L3_64QAM/100;
    case 4:
      return nmbRB*L4_64QAM/100;
    }
  }
  return -1;
}

#if PARAMETER_MODEL_STEP_NEW
/* This model changes the probabillity for the number of layers and
  modulation that is used by a user. The probability starts low
  resulting in almost all users having one layer and using QPSK as
  modulation. The probabillity is increased over 170 steps until all
  users will have four layers and use 64 QAM modulation. When maximum
  load has been reached the probabillity is instead being reduced over
  170 steps until it reaches the same level as in the beginning. */

#define COUNTDOWN 1

void init_parameter_model(parameter_model *pmodel) {
  time_delta        = 0;
  subframe          = 0;
  pmodel->active    = TASK_THREADS;
  pmodel->countdown = COUNTDOWN;
  pmodel->count     = 1;
  pmodel->inc       = 1;
  pmodel->active_min = 100;
  pmodel->active_max = 0;
  pmodel->active_sum = 0;
  pmodel->active_frames = 0;
  pmodel->activity[0] = 0;
  pmodel->activity[1] = 0;
  pmodel->activity[2] = 0;
  printf("#Users TotalRB MaxRB MinRB MaxLayer MinLayer MaxMod MinMod Count\n");
}

user_parameters *uplink_parameters(parameter_model *pmodel) {
  int active = 0;
  int nmbUsers = 0;
  int nmbRB = MAX_RB;
  int startPos = 0;
  float distribution = 0;
  int newRB;
  userS *users = NULL;
  user_parameters *parameters = NULL;
  int load_QPSK = 0;
  int load_16QAM = 0;
  int load_64QAM = 0;
  float load_perc = 0.0;
  int minLayer = 4;
  int maxLayer = 1;
  int minMod = MOD_64QAM;
  int maxMod = MOD_QPSK;
  int maxRB = 0;
  int minRB = 100;

  if (new_second()) {
    pmodel->countdown--;
    pmodel->active_min = 100;
    pmodel->active_max = 0;
    pmodel->active_sum = 0;
    pmodel->active_frames = 0;
    if (pmodel->countdown <= 0) {
      if (pmodel->inc)
	pmodel->count++;
      else
	pmodel->count--;
      pmodel->countdown = COUNTDOWN - 1;
    }
    if (pmodel->count == 170)
      pmodel-> inc = 0;
    else if (pmodel->count == 0)
      exit(0);
  }

  while (nmbUsers < MAX_USERS && nmbRB > 0) {
    /* New user so we need to allocate memory to store its information */
    if (users == NULL) {
      users = (userS *)malloc(sizeof(userS));
      users->next = NULL;
      parameters = (user_parameters *)malloc(sizeof(user_parameters));
      parameters->first = users;
    } else {
      users->next = (userS *)malloc(sizeof(userS));
      users = users->next;
      users->next = NULL;
    }
    parameters->last = users;

    /* Lets determine the number of layers (max 4) */
    users->nmbLayer = 1;
    if (PROB_EXTRA_LAYER*pmodel->count/80 > (rand()/(float)RAND_MAX)) users->nmbLayer++;
    if (PROB_EXTRA_LAYER*pmodel->count/80 > (rand()/(float)RAND_MAX)) users->nmbLayer++;
    if (PROB_EXTRA_LAYER*pmodel->count/80 > (rand()/(float)RAND_MAX)) users->nmbLayer++;

    users->startRB = startPos;
    
    newRB = (int)(MAX_RB*(rand()/(float)RAND_MAX));

    distribution = rand()/(float)RAND_MAX;

    /* Create a nonlinear distribution of the RBs between users */
    if (distribution < 0.4)
      newRB = newRB/8;
    else if (distribution < 0.6)
      newRB = newRB/4;
    else if (distribution < 0.9)
      newRB = newRB/2;

    /* A user has at leas one RB */
    if (!newRB)
      newRB = 1;

    if ( newRB > nmbRB ) {
      newRB = nmbRB;
    }
    users->nmbRB = newRB;
    
    nmbRB = nmbRB - users->nmbRB;
    startPos = startPos + users->nmbRB;
    
    users->mod = MOD_QPSK;
    if (PROB_MOD*pmodel->count/80 > (rand()/(float)RAND_MAX)) {
      users->mod = MOD_16QAM;
      if (PROB_MOD*pmodel->count/80 > (rand()/(float)RAND_MAX)) {
	users->mod = MOD_64QAM;
      }
    }

    users->data = &data[subframe_data()];

    nmbUsers++;
    switch(users->mod) {
    case MOD_PSK:
    case MOD_QPSK:
      load_QPSK += users->nmbRB * users->nmbLayer;
      break;
    case MOD_16QAM:
      load_16QAM += users->nmbRB * users->nmbLayer;
      break;
    case MOD_64QAM:
      load_64QAM += users->nmbRB * users->nmbLayer;
      break;
    }
    
    if (minRB > users->nmbRB)
      minRB = users->nmbRB;
    if (maxRB < users->nmbRB)
      maxRB = users->nmbRB;
    if (minLayer > users->nmbLayer)
      minLayer = users->nmbLayer;
    if (maxLayer < users->nmbLayer)
      maxLayer = users->nmbLayer;
    if (minMod > users->mod)
      minMod = users->mod;
    if (maxMod < users->mod)
      maxMod = users->mod;

    load_perc += user_load(users->nmbRB, users->nmbLayer, users->mod);
  }

  active = ceil(TASK_THREADS*(load_perc/100))+2;
  pmodel->active = active;

  printf("QPSK: %3i, 16QAM: %3i, 64QAM: %3i, Users: %2i, Active: %2i, Count: %2i, Load: %3.1f\n", load_QPSK, load_16QAM, load_64QAM, nmbUsers, pmodel->active, pmodel->count, load_perc);

  return parameters;
}

#elif PARAMETER_MODEL_STEP
/* This model changes the workload in steps of 25% starting at 
   100% going down to 25% back up to 100% and finally down to 
   25% before exiting. Each step lasts for 10 seconds. */

#define COUNTDOWN 100

void init_parameter_model(parameter_model *pmodel) {
  time_delta        = 0;
  subframe          = 0;
  pmodel->active    = TASK_THREADS;
  pmodel->countdown = COUNTDOWN;
  pmodel->inc       = 0;
}

user_parameters *uplink_parameters(parameter_model *pmodel) {
  int nmbUsers = 0;
  int nmbRB = MAX_RB;
  int startPos = 0;
  float distribution = 0;
  float p;
  int newRB;
  userS *users = NULL;
  user_parameters *parameters = (user_parameters *)malloc(sizeof(user_parameters));
  int load = 0;

  if (new_second()) {
    pmodel->countdown--;
    if (pmodel->countdown < 0) {
      pmodel->countdown = COUNTDOWN;
      pmodel->inc++;
      
      switch(pmodel->inc) {
      case 1: 
	pmodel->active = TASK_THREADS*3/4;
	break;
      case 2: 
	pmodel->active = TASK_THREADS/2;
	break;
      case 3: 
	pmodel->active = TASK_THREADS/4+5;
	break;
      case 4: 
	pmodel->active = TASK_THREADS/2;
	break;
      case 5: 
	pmodel->active = TASK_THREADS*3/4;
	break;
      case 6: 
	pmodel->active = TASK_THREADS;
	break;
      case 7: 
	pmodel->active = TASK_THREADS*3/4;
	break;
      case 8: 
	pmodel->active = TASK_THREADS/2;
	break;
      case 9: 
	pmodel->active = TASK_THREADS/4+5;
	break;
      default:
	exit(0);
	break;
      }
    }
  }

  while (nmbUsers < MAX_USERS && nmbRB > 0) {
    /* New user so we need to allocate memory to store its information */
    if (users == NULL) {
      users = (userS *)malloc(sizeof(userS));
      users->next = NULL;
      parameters->first = users;
    } else {
      users->next = (userS *)malloc(sizeof(userS));
      users = users->next;
      users->next = NULL;
    }
    parameters->last = users;

    /* The number of layers determines the workload 
       (each layer is a rough representative of 25% of work) */
    users->nmbLayer = pmodel->active/(TASK_THREADS/4);

    users->startRB = startPos;
    
    newRB = (int)(MAX_RB*(rand()/(float)RAND_MAX));

    distribution = rand()/(float)RAND_MAX;

    /* Create a nonlinear distribution of the RBs between users */
    if (distribution < 0.4)
      newRB = newRB/8;
    else if (distribution < 0.6)
      newRB = newRB/4;
    else if (distribution < 0.9)
      newRB = newRB/2;

    /* A user has at leas one RB */
    if (!newRB)
      newRB = 1;

    if ( newRB > nmbRB ) {
      newRB = nmbRB;
    }
    users->nmbRB = newRB;
    
    nmbRB = nmbRB - users->nmbRB;
    startPos = startPos + users->nmbRB;
    
    users->mod = MOD_QPSK;
    p = (rand()/(float)RAND_MAX);
    if (p > 0.3 ) users->mod = MOD_16QAM;
    if (p > 0.7 ) users->mod = MOD_64QAM;
    
    users->data = &data[subframe_data()];

    nmbUsers++;
    load += users->nmbRB*users->nmbLayer;
  }

  printf("Load: %3i, Users: %2i\n", load, nmbUsers);

  return parameters;
}

#elif PARAMETER_MODEL_RANDOM
/* This model randomly generates number of users, layers 
   and resource blocks. */
void init_parameter_model(parameter_model *pmodel) {
  time_delta        = 0;
  subframe          = 0;
  pmodel->active    = TASK_THREADS;
  pmodel->countdown = 0;
  pmodel->inc       = 0;
}

user_parameters *uplink_parameters(parameter_model *pmodel) {
  int nmbUsers = 0;
  int nmbRB = MAX_RB;
  int startPos = 0;
  float distribution = 0;
  float p;
  int newRB;
  userS *users = NULL;
  user_parameters *parameters = (user_parameters *)malloc(sizeof(user_parameters));
  int load = 0;

  while (nmbUsers < MAX_USERS && (rand()/(float)RAND_MAX) < PROB_NEW_USER && nmbRB > 0) {
    /* New user so we need to allocate memory to store its information */
    if (users == NULL) {
      users = (userS *)malloc(sizeof(userS));
      users->next = NULL;
      parameters->first = users;
    } else {
      users->next = (userS *)malloc(sizeof(userS));
      users = users->next;
      users->next = NULL;
    }
    parameters->last = users;

    /* Lets determine the number of layers (max 4) */
    users->nmbLayer = 1;
    if ( PROB_EXTRA_LAYER > (rand()/(float)RAND_MAX) ) users->nmbLayer++;
    if ( PROB_EXTRA_LAYER > (rand()/(float)RAND_MAX) ) users->nmbLayer++;
    if ( PROB_EXTRA_LAYER > (rand()/(float)RAND_MAX) ) users->nmbLayer++;

    users->startRB = startPos;
    
    newRB = (int)(MAX_RB*(rand()/(float)RAND_MAX));

    distribution = rand()/(float)RAND_MAX;

    /* Create a nonlinear distribution of the RBs between users */
    if (distribution < 0.4)
      newRB = newRB/8;
    else if (distribution < 0.6)
      newRB = newRB/4;
    else if (distribution < 0.9)
      newRB = newRB/2;

    /* A user has at leas one RB */
    if (!newRB)
      newRB = 1;

    if ( newRB > nmbRB ) {
      newRB = nmbRB;
    }
    users->nmbRB = newRB;
    
    nmbRB = nmbRB - users->nmbRB;
    startPos = startPos + users->nmbRB;
    
    users->mod = MOD_QPSK;
    p = (rand()/(float)RAND_MAX);
    if (p > 0.3 ) users->mod = MOD_16QAM;
    if (p > 0.7 ) users->mod = MOD_64QAM;
    
    users->data = &data[subframe_data()];

    nmbUsers++;
    load += user_load(users->nmbRB, users->nmbLayer, users->mod);
  }

  printf("Load: %3i, Users: %2i\n", load, nmbUsers);

  if (nmbUsers)
    return parameters;
  
  free(parameters);
  return NULL;
}

#elif PARAMETER_MODEL_CORRELATION_RB
/* This model changes the workload by increamenting the 
   computational effort for a single user. */
#define PERIOD 10
#define MODULATION MOD_64QAM

void init_parameter_model(parameter_model *pmodel) {
  time_delta        = 0;
  subframe          = 0;
  pmodel->active    = TASK_THREADS;
  pmodel->countdown = PERIOD;
  pmodel->inc       = 0;
}

user_parameters *uplink_parameters(parameter_model *pmodel) {
  user_parameters *parameters = (user_parameters *)malloc(sizeof(user_parameters));
  userS *users;

  if (new_second()) {
    pmodel->countdown--;
    if (pmodel->countdown == 0) {
      printf("RB=%3i, Layer=%2i, Mod=%i\n", pmodel->inc % MAX_RB + 1,  (int)(pmodel->inc/MAX_RB) + 1, MODULATION);
      pmodel->inc++;
      pmodel->active++;
      pmodel->countdown = PERIOD;
    }
  }

  users = (userS *)malloc(sizeof(userS));
  parameters->first = users;
  parameters->last = users;
  users->startRB = 0;

  users->nmbRB    = pmodel->inc % MAX_RB + 1;
  users->nmbLayer = (int)(pmodel->inc/MAX_RB) + 1;
  users->mod      = MODULATION;
  users->data     = &data[subframe_data()];
  
  users->next = NULL;
  return parameters;
}

#elif PARAMETER_MODEL_CORRELATION_RB_SEC
/* This model changes the workload by increamenting the 
   computational effort for a single user. */
#define PERIOD 10

void init_parameter_model(parameter_model *pmodel) {
  time_delta        = 0;
  subframe          = 0;
  pmodel->active    = TASK_THREADS;
  pmodel->countdown = PERIOD;
  pmodel->inc       = 0;  
}

user_parameters *uplink_parameters(parameter_model *pmodel) {
  user_parameters *parameters = (user_parameters *)malloc(sizeof(user_parameters));
  userS *users;
  int mod;

  mod = (int)(pmodel->inc/(MAX_RB*MAX_LAYERS));
  if (new_second()) {
    pmodel->countdown--;
    printf("RB=%3i, Layer=%2i, Mod=%i\n", pmodel->inc%MAX_RB+1,  (int)(pmodel->inc/MAX_RB)%MAX_LAYERS+1, mod);
    pmodel->active++;
    if (pmodel->countdown == 0) {
      pmodel->inc++;
      pmodel->countdown = PERIOD;
    }
  }

  users = (userS *)malloc(sizeof(userS));
  parameters->first = users;
  parameters->last = users;
  users->startRB = 0;

  users->nmbRB    = pmodel->inc % MAX_RB + 1;
  users->nmbLayer = (int)(pmodel->inc/MAX_RB) % MAX_LAYERS + 1;
  users->data     = &data[subframe_data()];
  mod = (int)(pmodel->inc/(MAX_RB*MAX_LAYERS));
  switch(mod) {
  case 0:
    users->mod = MOD_QPSK;
    break;
  case 1:
    users->mod = MOD_16QAM;
    break;
  case 2:
    users->mod = MOD_64QAM;
    break;
  default:
    /* Finished */
    exit(0);
    break;
  }

  users->next = NULL;
  return parameters;
}

#elif PARAMETER_MODEL_CORRELATION_USER
/* This model changes the workload by increamenting the 
   number of users while keeping numer of resource blocks
   and layers constant */
#define PERIOD 10

void init_parameter_model(parameter_model *pmodel) {
  time_delta        = 0;
  subframe          = 0;
  pmodel->active    = TASK_THREADS;
  pmodel->countdown = PERIOD;
  pmodel->inc       = 1;  
}

user_parameters *uplink_parameters(parameter_model *pmodel) {
  user_parameters *parameters = (user_parameters *)malloc(sizeof(user_parameters));
  userS *users;
  int i;

  if (new_second()) {
    pmodel->countdown--;
    if (pmodel->countdown == 0) {
      printf("USERS=%2i\n", pmodel->inc);
      pmodel->inc++;
      pmodel->active++;
      pmodel->countdown = PERIOD;
    }
    if (pmodel->inc == 12)
      exit(0);
  }

  for (i=0; i<pmodel->inc; i++) {
    if (i == 0) {
      users = (userS *)malloc(sizeof(userS));
      parameters->first = users;
    } else {
      users->next = (userS *)malloc(sizeof(userS));
      users = users->next;
    }
    parameters->last = users;

    users->startRB  = (int)(100/pmodel->inc)*i;
    users->nmbRB    = (int)100/pmodel->inc;
    users->nmbLayer = 3;
    users->mod      = MOD_64QAM;
    users->data     = &data[subframe_data()];
    users->next     = NULL;
  }

  return parameters;
}

#elif PARAMETER_MODEL_VERIFICATION
/* This model creates a static number of parameter combinations that
   can be used for verification */
static int current_users;
static int current_rb;
static int current_layer;
static int current_mod;

void init_parameter_model(parameter_model *pmodel) {
  pmodel->active    = TASK_THREADS;
  pmodel->countdown = 0;
  pmodel->inc       = 1;  
  subframe          = 0;
  current_users     = 1;
  current_rb        = 1;
  current_layer     = 1;
  current_mod       = MOD_QPSK;
}

user_parameters *uplink_parameters(parameter_model *pmodel) {
  user_parameters *parameters = (user_parameters *)malloc(sizeof(user_parameters));
  userS *users = NULL;
  int i;
  int startRB = 0;

  for (i=0; i<current_users; i++) {
    if (i == 0) {
      users = (userS *)malloc(sizeof(userS));
      parameters->first = users;
    } else {
      users->next = (userS *)malloc(sizeof(userS));
      users = users->next;
    }
    users->startRB  = startRB;
    startRB         = current_rb;
    users->nmbRB    = current_rb;
    users->nmbLayer = current_layer;
    users->mod      = current_mod;
    users->subframe = subframe;
    /* Subframe is incremented when subframe_data() is called */
    users->data     = &data[subframe_data()];
    users->next     = NULL;
  }
  parameters->last = users;

  printf("Subframe: %i, users: %2i, RB: %3i, Layer: %i, Mod: %i\n", users->subframe, current_users, current_rb, current_layer, current_mod);

  current_rb++;
  if (current_rb*current_users > MAX_RB) {
    current_rb = 1;
    current_layer++;
  }
  if (current_layer > MAX_LAYERS) {
    current_layer = 1;
    switch (current_mod) {
    case MOD_QPSK:
      current_mod = (int)MOD_16QAM;
      break;
    case MOD_16QAM:
      current_mod = (int)MOD_64QAM;
      break;
    case MOD_64QAM:
      current_mod = (int)MOD_QPSK;
      current_users++;
      break;
    default:
      printf("No such modulation.\n");
      exit(1);
      break;
    }
  }

#if DETAILED_VERIFICATION_DATA
  if (subframe == 200) {
    uplink_write_verify_data(subframe);
    exit(0);
  }
#else
  if (current_users >= MAX_USERS) {
    uplink_write_verify_data(subframe-8);
    exit(0);
  }
#endif

  return parameters;
}

#endif
