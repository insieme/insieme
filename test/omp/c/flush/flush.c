#define _POSIX_C_SOURCE 199309L

#include <stdio.h>
#include <omp.h>
#include <time.h>
#include <unistd.h>

int flag;

#define IMAX 1000000000ll

int main() {
	long long i = 0;
	#pragma omp parallel
	{

//	//	struct {
	//	   time_t tv_sec;        /* seconds */
	//	   long   tv_nsec;       /* nanoseconds */
	//	} t;
	//	t.tv_sec =0;
	//	t.tv_nsec = 10000;

		if(omp_get_thread_num() == 0) {
			usleep(10);
			flag = 1;
		}
		while(flag == 0 && i < IMAX) {
			#pragma omp flush(flag)
			++i;
		}
	}
	if(i < IMAX) {
		printf("Success!\n");
	} else {
		printf("Fail!\n");
	}		
}
