
#include <stdio.h>

#define N 10

int A[N];

int main(int argc, char* argv[]) {

	// fill A
	#pragma omp parallel for
	for(int i=0; i<N; i++) {
		A[i] = i;
		printf("A[%d]=%d\n", i, A[i]);
	}


}
