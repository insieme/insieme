#include <stdlib.h>
#include <stdio.h>

#ifndef N
#define N 100
#endif

#ifdef INDIRECT

#define CREATE_MATRIX double (*a)[N*N]; a = malloc(N*N*sizeof(double))
#define A(X,Y) (*a)[(X)*N+(Y)]
#define FREE_MATRIX free(a)

#else

#define CREATE_MATRIX double a[N*N]
#define A(X,Y) a[(X)*N+(Y)]
#define FREE_MATRIX

#endif


int main() {

	// establish matrix
	CREATE_MATRIX;

	// init matrix
	for (int i=0; i<N; i++) {
		for (int j=0; j<N; j++) {
			A(i,j) = 0;
		}
	}

	// some seed
	A(0,0) = 1;

	for(int k=0; k<2000; k++) {

		// just some computations
		for (int i=1; i<N-1; i++) {
			for (int j=1; j<N-1; j++) {
				A(i,j) += (A(i,j-1) + A(i,j+1) + A(i-1,j) + A(i+1,j))/4;
				A(i,j) /= 2;
			}
		}
	}

	double sum = 0;
	for (int i=0; i<N; i++) {
		for (int j=0; j<N; j++) {
			sum += A(i,j);
		}
	}

	printf("Total Sum: %lf\n", sum);

	

	// free matrix
	FREE_MATRIX;
}
