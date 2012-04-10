#include <stdio.h>

// allows the user to specify the problem size at compile time
#ifndef N
	#define N 1000
#endif

double A[N][N];
double B[N][N];


void printMatrix(double(*m)[N][N]) {

	for(int i=0; i<N; i++) {
		for(int j=0; j<N; j++) {
			printf("%.1f ", (*m)[i][j]);
		}
		printf("\n");
	}
	printf("\n");
}


int main() {

	// This benchmark is computing the symmetric rank k operation
	//		B = A * A^T + B
	// where A is a upper triangular and unit (1 along the diagonal)

	#pragma omp parallel
	{

		// A is the identity matrix
		#pragma omp for
		for (int i=0; i<N; i++) {
			for (int j=0; j<N; j++) {
				A[i][j] = (i==j)?1:0;
//				A[i][j] = (i<=j)?1:0;		// for testing
			}
		}

		// B contains some actual values
		#pragma omp for
		for (int i=0; i<N; i++) {
			for (int j=0; j<N; j++) {
				B[i][j] = i*j;
			}
		}

//		printf("A:\n"); printMatrix(&A);
//		printf("B:\n"); printMatrix(&B);

		// conduct multiplication
		#pragma omp for schedule(dynamic)
		for (int i=0; i<N; i++) {
			for (int j=0; j<N; j++) {
				// to be handleable by the polyhedral model
				for (int k=0; k<N; k++) {
					B[i][j] += A[i][k] * A[j][k];
				}
			}
		} 

//		printf("B:\n"); printMatrix(&B);
	}

	// verify result
	int success = 1;	
	for (int i=0; i<N; i++) {
		for (int j=0; j<N; j++) {
			success = success && 
				(
					((i == j) && B[i][j] == i * j + 1) ||
					((i != j) && B[i][j] == i * j)
				);
		}
	}

	// print verification result
	printf("Verification: %s\n", (success)?"OK":"ERR");

	return 1-success;
}
