#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv)
{

	int iterations = 10000;
	struct timeval start, end;

	gettimeofday(&start, NULL);

	for (int i = 0; i < iterations; i++) {
	}

	gettimeofday(&end, NULL);

	printf("%ld\n", ((end.tv_sec * 1000000 + end.tv_usec) - (start.tv_sec * 1000000 + start.tv_usec)));

	return 0;
}
