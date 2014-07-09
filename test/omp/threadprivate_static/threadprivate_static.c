#include <omp.h>
#include <stdio.h>

#define THREADS 4

static double gd;
static int ga[THREADS];
#pragma omp threadprivate(gd, ga)
static double gs;

void calc() {
	for(int i=0; i<THREADS; ++i)
		ga[i] = omp_get_num_threads()-omp_get_thread_num();
}


int main() {
	#pragma omp parallel num_threads(THREADS)
	{
		gd = omp_get_thread_num();
		#pragma omp barrier
		#pragma omp critical		
		gs += gd;
		
		calc();
	}
	
	int success = (gs == (THREADS*(THREADS-1))/2);
	#pragma omp parallel num_threads(THREADS)
	{
		for(int i=0; i<THREADS; ++i)
			if(ga[omp_get_thread_num()] + omp_get_thread_num() != omp_get_num_threads())
				success = 0;
	}

	if(success) {
		printf("Success!\n");
	} else {
		printf("Fail! %f\n", gs);
	}
}
