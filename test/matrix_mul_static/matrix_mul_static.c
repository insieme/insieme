#include <stdio.h>

// allows the user to specify the problem size at compile time
#ifndef N
	#define N 1000
#endif

#define M N
#define K N

#define MIN(X,Y) ((X)<(Y)?(X):(Y))
#define MAX(X,Y) ((X)>(Y)?(X):(Y))

#define VALUE double

// create the matices
VALUE A[N][M];
VALUE B[M][K];
VALUE C[N][K];

int main() {

	// initialize the matices

	#pragma omp parallel
	{

		// A contains real values
               
		#pragma omp for
		for (int i=0; i<N; i++) {
			for (int j=0; j<M; j++) {
				A[i][j] = i*j;
			}
		}

		// B is the identity matrix

		#pragma omp for
		for (int i=0; i<M; i++) {
			for (int j=0; j<K; j++) {
				B[i][j] = (i==j)?1:0;
			}
		}

		// conduct multiplication
		// modified by JJ
		//for (int jj = 0; jj < 10; jj++) {
		#pragma omp for schedule(dynamic)
		for (int i=0; i<N; i++) {
			for (int j=0; j<K; j++) {
				/*
				VALUE sum = 0;
				for (int k=0; k<M; k++) {
					sum += A[i][k] * B[k][j];
				}
				C[i][j] = sum;
				*/

				
				// to be handleable by the polyhedral model
				for (int k=0; k<M; k++) {
					C[i][j] += A[i][k] * B[k][j];
				}
				
			}
		} 
		//}
	}

	// verify result
	int success = 1;	
	for (int i=0; i<N; i++) {
		for (int j=0; j<MIN(M,K); j++) {
			if (A[i][j] != C[i][j]) {
				success = 0;
			}
		}
		for (int j=MIN(M,K); j<MAX(M,K); j++) {
			if (C[i][j] != 0) {
				success = 0;
			}
		}
	}

	// print verification result
	printf("Verification: %s\n", (success)?"OK":"ERR");
}

