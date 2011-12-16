#include <omp.h>
#include <stdio.h>

#define THREADS 4

double gd;
double gs;
int ga[THREADS];
#pragma omp threadprivate(gd, ga)

int main() {
 	
	#pragma omp parallel num_threads(THREADS)
	{
		gd = 2;
		#pragma omp barrier
		#pragma omp critical		
		gs += gd;
		
		for(int i=0; i<THREADS; ++i)
			ga[i] = omp_get_num_threads()-omp_get_thread_num();
	}
	
	bool success = (gs == 2*THREADS);
	#pragma omp parallel num_threads(THREADS)
	{
		for(int i=0; i<THREADS; ++i)
			if(ga[omp_get_thread_num()] + omp_get_thread_num() != omp_get_num_threads())
				success = false;
	}

	if (success) {
		printf("Success!\n");
	} else {
		printf("Fail!\n");
	}
}
