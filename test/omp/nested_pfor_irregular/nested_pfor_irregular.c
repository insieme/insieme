
#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define OUTER_PARALLELISM 8
#define WORK_LOAD_SCALE 10

int process(int x);

int main() {

	int sum = 0;

	// omp_set_nested(1);

	#pragma omp parallel for reduction(+:sum) schedule(dynamic)
	for(int i=1; i<=OUTER_PARALLELISM; i++) {
		printf("Starting %d ...\n", i);
		sum += process(i*i*2);
		printf("Done: %d\n", i);
	}

	// print result
	printf("Result: %d\n", sum);

	return 0;
}

// --------------------------------------------------------



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

// --------------------------------------------------------

int step(int x);

int process(int x) {

	x = WORK_LOAD_SCALE * x;

	// produce some workload of the given size using loops
	int N = x;
	int M = x;
	int K = x;

	// create matrices
	MATRIX A = createMatrix(N, M);
	MATRIX B = createMatrix(M, K);
	MATRIX C = createMatrix(N, K);

	// A contains irregular values
	#pragma omp parallel for
	for (int i=0; i<N; i++) {
		for (int j=0; j<M; j++) {
			A[i][j] = i*j;
		}
	}

	// B is the identity matrix
	#pragma omp parallel for
	for (int i=0; i<M; i++) {
		for (int j=0; j<K; j++) {
			B[i][j] = (i==j)?1:0;
		}
	}

	// conduct multiplication
	#pragma omp parallel for
	for (int i=0; i<N; i++) {
		for (int j=0; j<K; j++) {
			TYPE sum = 0;
			for (int k=0; k<M; k++) {
				sum += A[i][k] * B[k][j];
			}
			C[i][j] = sum;
		}
	} 

	// aggregate result
	int res = 0;
	#pragma omp parallel for reduction(+:res)
	for (int i=0; i<M; i++) {
		for (int j=0; j<K; j++) {
			if (C[i][j] > 1.0) res++;
		}
	}

	// free matrices
	freeMatrix(A);
	freeMatrix(B);
	freeMatrix(C);

	return res;
}

