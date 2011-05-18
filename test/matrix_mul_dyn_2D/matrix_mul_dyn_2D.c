#include <stdio.h>
#include <stdlib.h>

#define S 1000
#define N S
#define M S
#define K S

#define MIN(X,Y) ((X)<(Y)?(X):(Y))
#define MAX(X,Y) ((X)>(Y)?(X):(Y))

#define TYPE double
#define MATRIX TYPE**

// A utility function
MATRIX createMatrix(unsigned x, unsigned y) {
	TYPE* data = malloc(x * y * sizeof(TYPE));

	TYPE** index = malloc(x * sizeof(TYPE*));
	index[0] = data;
	for (unsigned i = 1; i < x; ++i) {
		index[i] = &(data[i*y]);
	}
	return index;
}

void freeMatrix(MATRIX matrix) {
	free(matrix[0]);
	free(matrix);
}

// create the matices

int main() {

	// initialize the matices

	MATRIX A = createMatrix(N, M);
	MATRIX B = createMatrix(M, K);
	MATRIX C = createMatrix(N, K);

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
		#pragma omp for
		for (int i=0; i<N; i++) {
			for (int j=0; j<K; j++) {
				TYPE sum = 0;
				for (int k=0; k<M; k++) {
					sum += A[i][k] * B[k][j];
				}
				C[i][j] = sum;
			}
		} 
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

	freeMatrix(A);
	freeMatrix(B);
	freeMatrix(C);
}

