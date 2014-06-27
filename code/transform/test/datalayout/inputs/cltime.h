#ifndef TIME_H
#define TIME_H
/*
 *  time.h
 *
 *  Created by Michael Brunner on 23.11.09.
 *
 */

#include <stdio.h>

#ifndef UNIX
  #define DELTA_EPOCH_IN_MICROSECS  11644473600000000Ui64
#include <windows.h>  

struct timezone
{
  int  tz_minuteswest; /* minutes W of Greenwich */
  int  tz_dsttime;     /* type of dst correction */
};
int gettimeofday(struct timeval *tv, struct timezone *tz);

#else

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>

#endif

void init_timers();
void start_timer(unsigned short int timerNumber);
void stop_timer(unsigned short int timerNumber);
unsigned long get_msecs(unsigned short int timerNumber);
double get_secs(unsigned short int timerNumber);

#endif /* TIME_H */
