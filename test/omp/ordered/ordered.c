#include <stdlib.h>
#include <stdio.h>
#include <omp.h>

int main(int argc, char** argv) {
	int sum = 0;
	#pragma omp parallel num_threads(8)
	{
		#pragma omp for ordered
		for(int i = 0; i < 8; i++) {
			sum += rand()%1;
			//printf("A Iteration %d\n", i);
			if(i < 2 || i > 5 ) {
				#pragma omp ordered
				printf("O Iteration %d\n", i);
			}
			//printf("B Iteration %d\n", i);
			sum += rand()%1;
		}

	}
	return (sum > 10000000) ? 1 : 0;
}

