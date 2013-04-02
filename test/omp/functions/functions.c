#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define THREADS 8

void fail() {
	printf("FAILURE\n");
	exit(-1);
}

int main() {
	int tmax = omp_get_max_threads();
	if(tmax < 1) fail();
	
	#pragma omp parallel num_threads(THREADS)
	{
		int tn = omp_get_num_threads();
		if(tn != THREADS) fail();
	}
	
	double t1 = omp_get_wtime();
	double t2 = omp_get_wtime();
	if(t1>t2) fail();
	
	printf("success\n");
	return 0;	
}
