#include <stdio.h>

int main() {
	#pragma omp parallel
	{
    printf("multiple\n");
    #pragma omp single
		printf("single\n");
	}
}
