#include <stdio.h>

// allows the user to specify the problem size at compile time
#ifndef N
	#define N 1000
#endif

#define M N

#define VALUE double

// create source and target array
VALUE A[N][M];
VALUE B[M][N];

int main() {

	// initialize the matices

	#pragma omp parallel
	{
		#pragma omp for
		for (int i=0; i<N; i++) {
			for (int j=0; j<M; j++) {
				A[i][j] = i + 10 * j;
			}
		}

		// A contains real values
               
		#pragma omp for
		for (int i=0; i<N; i++) {
			for (int j=0; j<M; j++) {
				A[i][j] = B[j][i];
			}
		}

	}

	// verify result
	int success = 1;	
	for (int j=0; j<M; j++) {
		for (int i=0; i<N; i++) {
			if (B[j][i] == 10*j + i) {
				success = 0;
			}
		}
	}

	// print verification result
	printf("Verification: %s\n", (success)?"OK":"ERR");
}

