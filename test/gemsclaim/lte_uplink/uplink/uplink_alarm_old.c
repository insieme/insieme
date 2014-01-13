/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include <pthread.h>
#include <signal.h>
#include <unistd.h>

pthread_mutex_t alarm_mutex;
volatile int spawnflag;

void uplink_alarm_handle(int sig) {
  /* Set flag to spawn a new subframe */
  pthread_mutex_lock(&alarm_mutex);
  spawnflag++;
  pthread_mutex_unlock(&alarm_mutex);
}

void uplink_alarm_init(unsigned long delta) {
  spawnflag = 0;
  pthread_mutex_init(&alarm_mutex, NULL);
  /* Set a function to handle the SIGALRM signal */
  signal(SIGALRM, uplink_alarm_handle);
  /* Start a new alarm every delta microseconds */
  ualarm(delta, delta);
}

/*inline*/ int uplink_wait_for_alarm(void) {
  /* Wait until next alarm */
  while (spawnflag==0) {
    usleep(100);
  }
  pthread_mutex_lock(&alarm_mutex);
  spawnflag--;
  pthread_mutex_unlock(&alarm_mutex);

  return spawnflag;
}
