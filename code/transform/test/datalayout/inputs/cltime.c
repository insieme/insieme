/*
 *  time.cpp
 *  benchmark_throughput
 *
 *  Created by Michael Brunner on 23.11.09.
 *  Copyright 2009 __MyCompanyName__. All rights reserved.
 *
 */

#include "cltime.h"

#define MAX_TIMERS 10

struct timeval timers[MAX_TIMERS];

#ifndef UNIX
//hacky gettimeofday() for windows
//stolen from: http://www.cpp-programming.net/c-tidbits/gettimeofday-function-for-windows/

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
  FILETIME ft;
  unsigned __int64 tmpres = 0;
  static int tzflag;

  if (NULL != tv)
  {
    GetSystemTimeAsFileTime(&ft);

    tmpres |= ft.dwHighDateTime;
    tmpres <<= 32;
    tmpres |= ft.dwLowDateTime;

    /*converting file time to unix epoch*/
    tmpres /= 10;  /*convert into microseconds*/
    tmpres -= DELTA_EPOCH_IN_MICROSECS;
    tv->tv_sec = (long)(tmpres / 1000000UL);
    tv->tv_usec = (long)(tmpres % 1000000UL);
  }

  return 0;
}
#endif

void init_timers() {
	//timers = (timeval*) malloc(sizeof(timeval) * MAX_TIMERS * 2);
}

void start_timer(unsigned short int timerNumber) {
	if (timerNumber > MAX_TIMERS) {
		return;
	}
	gettimeofday(&timers[timerNumber * 2], NULL);
}

void stop_timer(unsigned short int timerNumber) {
	if (timerNumber > MAX_TIMERS) {
		return;
	}
	gettimeofday(&timers[timerNumber * 2 + 1], NULL);
}

unsigned long get_msecs(unsigned short int timerNumber) {
	if (timerNumber > MAX_TIMERS) {
		return 0;
	}
    unsigned long retval = 0;
	struct timeval start = timers[timerNumber * 2];
	struct timeval finish = timers[timerNumber * 2 + 1];
	retval = (unsigned long)(finish.tv_sec - start.tv_sec) * 1000000ul;
	retval += (unsigned long)(finish.tv_usec - start.tv_usec);
	return retval;
}

double get_secs(unsigned short int timerNumber) {
	if (timerNumber > MAX_TIMERS) {
		return 0.0f;
	}
    double retval = 0.0f;
	struct timeval start = timers[timerNumber * 2];
	struct timeval finish = timers[timerNumber * 2 + 1];
	retval = (double)finish.tv_sec + 1.0e-6 * (double)finish.tv_usec;
	retval -= (double)start.tv_sec + 1.0e-6 * (double)start.tv_usec;
	return retval;
}
