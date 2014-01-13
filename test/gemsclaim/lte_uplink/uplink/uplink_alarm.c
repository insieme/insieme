/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include <signal.h>
#include <unistd.h>
#include <sys/time.h>

struct timeval spawntime;
unsigned long delta;

void uplink_alarm_init(unsigned long _delta) 
{
  	gettimeofday(&spawntime, NULL);
	delta = _delta;
}

int uplink_wait_for_alarm(void)
{
  	struct timeval time;
  	gettimeofday(&time, NULL);

	while(time.tv_sec * 1000000 + time.tv_usec - (spawntime.tv_sec * 1000000 + spawntime.tv_usec) < delta)
	{
		usleep(100);
  		gettimeofday(&time, NULL);
	}

	spawntime.tv_sec = spawntime.tv_sec + (spawntime.tv_usec + delta) / 1000000;
	spawntime.tv_usec = (spawntime.tv_usec + delta) % 1000000;

  	return 0;
}
