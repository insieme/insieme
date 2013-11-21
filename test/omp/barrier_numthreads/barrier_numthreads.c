#include <stdio.h>

#define THREADS 200

int main() {
	int i = 0;
	int sum = 0;

	#pragma omp parallel num_threads(THREADS)
	{
		#pragma omp barrier
		#pragma omp atomic
		sum += 1;
	}

	// check whether sum works out ...
	if(sum == THREADS) {
		printf("Success!\n");
	} else {
		printf("Fail! - %d\n", sum);
	}
}
