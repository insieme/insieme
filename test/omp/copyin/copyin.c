#include <omp.h>
#include <stdio.h>

#define THREADS 4

double gd;
#pragma omp threadprivate(gd)

int main() {
 	
	gd = 3.14;
	int success = 1;
	#pragma omp parallel num_threads(THREADS) copyin(gd)
	{
		if(gd != 3.14) success = 0;
	}
	
	if(success) {
		printf("Success!\n");
	} else {
		printf("Fail!\n");
	}
}
