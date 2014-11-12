#include "stdio.h"

#ifndef N
#define N 64
#endif

static double array[N][N][N][N];

void init1() {
	int i, j, k, m;
	#pragma omp for
	for(i = 0; i < N; ++i) {
		for(j = 0; j < N; ++j) {
			for(k = 0; k < N; ++k) {
				for(m = 0; m < N; ++m) {
					array[i][j][k][m] = 2.0;
				}
			}
		}
	}
}

void init2() {
	#pragma omp for
	for(int i = 0; i < N; ++i) {
		for(int j = 0; j < N; ++j) {
			for(int k = 0; k < N; ++k) {
				for(int m = 0; m < N; ++m) {
					array[i][j][k][m] = 2.0;
				}
			}
		}
	}
}

int verify() {
	for(int i = 0; i < N; ++i) {
		for(int j = 0; j < N; ++j) {
			for(int k = 0; k < N; ++k) {
				for(int m = 0; m < N; ++m) {
					if(array[i][j][k][m] != 2.0)
						return 1;
				}
			}
		}
	}
	return 0;
}

int main(int argc, char** argv) {
	#pragma omp parallel
	{
	init1();
	}

	int result = verify();

	printf("Array size: %ux%ux%ux%u\n", N, N, N, N);
	printf("Verification: %s\n", result==0?"OK":"FAILED");
}
