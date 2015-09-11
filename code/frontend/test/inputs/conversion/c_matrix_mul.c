#include <stdio.h>

// allows the user to specify the problem size at compile time
#ifndef N
#define N 1000
#endif

#define M N
#define K N

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

#define VALUE double

VALUE A[N][M];
VALUE B[M][K];
VALUE C[N][K];

int main() {
	// A contains real values
	for(int i = 0; i < N; i++) {
		for(int j = 0; j < M; j++) {
			A[i][j] = i * j;
		}
	}

	// B is the identity matrix
	for(int i = 0; i < M; i++) {
		for(int j = 0; j < K; j++) {
			B[i][j] = (i == j) ? 1 : 0;
		}
	}

	// conduct multiplication
	for(int i = 0; i < N; i++) {
		for(int j = 0; j < K; j++) {
			for(int k = 0; k < M; k++) {
				C[i][j] += A[i][k] * B[k][j];
			}
		}
	}

	// verify result
	int success = 1;
	for(int i = 0; i < N; i++) {
		for(int j = 0; j < MIN(M, K); j++) {
			if(A[i][j] != C[i][j]) { success = 0; }
		}
		for(int j = MIN(M, K); j < MAX(M, K); j++) {
			if(C[i][j] != 0) { success = 0; }
		}
	}

	return !success;
}
