#include <stdio.h>

int pointy() {
	return 5;
}

typedef int (*funpointer)();

int main() {
	funpointer fp = &pointy;
	
	#pragma omp parallel num_threads(1) 
	{
		printf("%d\n", fp());
	}
}
