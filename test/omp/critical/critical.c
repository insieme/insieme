#include <stdio.h>

#define NUM_PARA 10
#define NUM_ITER 1000

int main() {
	int m = 0;
	#pragma omp parallel num_threads(NUM_PARA)
	{
		for(int i=0; i<NUM_ITER; ++i) {
			#pragma omp critical (bla)
			m++;
		}
	}

	if (m == NUM_PARA * NUM_ITER) {
		printf("Success!\n");
	} else {
		printf("Fail!\n");
	}
}
