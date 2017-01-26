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
#include "uplink_omp.h"
#include "uplink_parameters.h"
#include "uplink_verify.h"
#include "uplink_alarm.h"
#include "crc_13.h"
#include <omp.h>

int main(int argc, char *argv[]) {  
  /* User info */
  user_parameters *parameters;
  parameter_model pmodel;

  /* Generate input data */
  init_data();
  init_verify();
  crcInit();
  init_parameter_model(&pmodel);

  /* Initialize the alarm that will signal every DELTA microsecons */
  /* DELTA is defined in def.h */
  uplink_alarm_init(DELTA);

  #pragma omp parallel num_threads(TASK_THREADS)
  {
     #pragma omp master
     {
       /* Main loop that submits a subframe for execution every DELTA microseconds */
       while (1) {
         /* Generate parameters for next frame */
         parameters = uplink_parameters(&pmodel);

         /* Wait until next subframe should be computed */
         uplink_wait_for_alarm();

         /* Compute users */
         if (parameters) {
           while(parameters->first) {
               userS* task_users = parameters->first;
               #pragma omp task firstprivate(task_users)
                 uplink_user_cilk(task_users);
               parameters->first = parameters->first->next;
           }
           free(parameters);
         }
       }
    }
  }

  return 0;
}
