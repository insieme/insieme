#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>

#include <unistd.h>

#include "second_cpu.h"

int main(int argc, char **argv)
{

	struct timeval start, end;

	gettimeofday(&start, NULL);

	sleep(1);

	gettimeofday(&end, NULL);

	if ((end.tv_sec * 1000000 + end.tv_usec) - (start.tv_sec * 1000000 + start.tv_usec) >= 1000000)
		printf("SUCCESS");
	else
		printf("FAIL");

	printf(" %1.4f\n", mysecond()); 
	return 0;
}
