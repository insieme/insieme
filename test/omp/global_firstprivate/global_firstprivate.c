#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define THREADS 4
#define START 5

int gi;

void check() {
	if(gi != START) {
		printf("gi: %d, expected: %d\n", gi, START);
		printf("FAIL\n");
		exit(-1);
	};
}

int main() {
	gi = START;
	#pragma omp parallel num_threads(THREADS) firstprivate(gi)
	{
		gi += omp_get_thread_num();
		if(gi != START + omp_get_thread_num()) {
			printf("gi (private): %d, expected: %d\n", gi, START + omp_get_thread_num());
			printf("FAIL\n");
			exit(-1);
		};
		check();
	}

	printf("Success!\n");
}
