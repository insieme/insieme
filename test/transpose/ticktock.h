// PeterT

#ifndef TICKTOCK_H
#define TICKTOCK_H

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/time.h>

long ticktock()
{
  static long prevtime = 0;
  struct timeval tv;
  gettimeofday(&tv, 0);
  long time = tv.tv_sec * 1000 + tv.tv_usec/1000;
  long retval = time - prevtime;
  prevtime = time;
  return retval;
}

#endif // TICKTOCK_H
