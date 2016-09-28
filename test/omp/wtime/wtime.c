
// required for nanosleep
#define _POSIX_C_SOURCE 199309L

#include <time.h>
#include <sys/time.h>
#include <omp.h>

#define T_MS 100

int main() {

	struct timespec  waitTime;
	waitTime.tv_sec = 0;
	waitTime.tv_nsec = T_MS*1000000;
	
	double start = omp_get_wtime();
	nanosleep(&waitTime, 0); 
	double t = omp_get_wtime() - start;
	
	double tms = t * 1000;
	
	// measured time ~ within 10% of sleep time
	return !((tms > T_MS*0.9) && (tms < T_MS*1.1));
}
