#include <stdio.h>
#include <omp.h>

#define NUM_ITER 1000

int main() {
	int m = 0;
	#pragma omp parallel
	{
		for(int i=0; i<NUM_ITER; ++i) {
			#pragma omp critical
			m++;
		}
		#pragma omp barrier
		#pragma omp master
		{
			if(m == omp_get_num_threads() * NUM_ITER) {
				printf("Success!\n");
			} else {
				printf("Fail!\n");
			}
		}
	}

}
