/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#ifndef _DEF_H
#define _DEF_H

//#include <pthread.h>
#include <math.h>

/* Set which user parameter generator to use, select ONE by setting it to 1 */
#define PARAMETER_MODEL_STEP                   0
#define PARAMETER_MODEL_STEP_NEW               0
#define PARAMETER_MODEL_RANDOM                 0
#define PARAMETER_MODEL_CORRELATION_RB         0
#define PARAMETER_MODEL_CORRELATION_RB_SEC     0
#define PARAMETER_MODEL_CORRELATION_USER       0
#define PARAMETER_MODEL_VERIFICATION           1

#define PARAMETER_MODEL_CORRELATION (PARAMETER_MODEL_CORRELATION_RB || PARAMETER_MODEL_CORRELATION_RB_SEC || PARAMETER_MODEL_CORRELATION_USER || PARAMETER_MODEL_RANDOM)

/* Set to 1 if cores are to be deactivated */
#define DEACTIVATE_CORES  1

/* Verification configuration */
#define CREATE_VERIFICATION_DATA    0
#define DETAILED_VERIFICATION_DATA  0


/* Defines system resources and ODFM data structures */
#define NMB_SLOT          2
#define OFDM_IN_SLOT      7
#define RX_ANT	          4
#define MAX_LAYERS	  4
#define MAX_SC	          1200
#define MAX_RB		  100
#define SC_PER_RB	  (MAX_SC/MAX_RB)

/* Number of data frames for which input data should be generated */
#define NMB_DATA_FRAMES   10

/* Defines for use in random init of user data + number of users */
#define PROB_NEW_USER	  0.7
#define PROB_EXTRA_LAYER  0.5
#define PROB_MOD	  0.5

/* Time between subframes in us */
#if X86
#define DELTA             2000
#else
#if CILK
#define DELTA             4000
#else
#define DELTA             5000
#endif
#endif

/* Maximum number of users for a single subframe */
#define MAX_USERS	  10

/* Number of threads that will do the actual work
   Assure that TASK_THREADS/THREADS_PER_QUEUE is an integer */
#if X86
#define TASK_THREADS      16
#else
#define TASK_THREADS      62
#endif

#endif
