//#include <sys/param.h>
//#include <sys/types.h>
//#include <time.h>
//#include <sys/times.h>
#include "second_cpu.h"

//#define CLOCKS_PER_SEC 5

double mysecond()
{
    long sec;
    double secx;
    struct tms realbuf;

    times(&realbuf);
    secx = ( realbuf.tms_stime + realbuf.tms_utime ) / (float) CLOCKS_PER_SEC;
    return ((double) secx);
}
