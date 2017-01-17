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

	// This benchmark is computing 
	//		B = alpha * A * B
	// where A is a upper triangular and unit (1 along the diagonal)

	double alpha = 2;

	#pragma omp parallel
	{

		// A is the identity matrix (which is a upper unit triangular matrix)
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

		// conduct multiplication
		for (int i=0; i<N; i++) {
			#pragma omp for
			for (int j=0; j<N; j++) {
				B[i][j] *= alpha;
				// to be handleable by the polyhedral model
				for (int k=i+1; k<N; k++) {
					B[i][j] += alpha * A[i][k] * B[k][j];
				}
			
			}
		} 
	}

//	printf("B:\n"); printMatrix(&B);

	// verify result
	int success = 1;	
	for (int i=0; i<N; i++) {
		for (int j=0; j<N; j++) {
			if (B[i][j] != alpha * i * j) {
				success = 0;
			}
		}
	}

	// print verification result
	printf("Verification: %s\n", (success)?"OK":"ERR");

	return 1-success;
}
