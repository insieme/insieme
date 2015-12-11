#include <stdlib.h>
#include <stdio.h>

#define N 100

int main() {

	int count = 0;
	int a[N];

	#pragma omp parallel
	{
		#pragma omp for
		for (int i=0; i<N; i++) {
			a[i] = i;
			#pragma omp critical
			count++;
		}
	}

	for (int i=0; i<N; i++) {
		printf("a[%d]=%d\n", i, a[i]);
	}
	
	printf("count: %d\n", count);
}
