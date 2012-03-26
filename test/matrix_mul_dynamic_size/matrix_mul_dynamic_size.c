#include <stdio.h>
#include <stdlib.h>

// allows the user to specify the problem size at compile time
#ifndef N
	#define N 1000
#endif

#define VALUE double

VALUE** createMatrix(int size) {

	VALUE* block = (VALUE*)malloc(sizeof(VALUE)*size*size);
	VALUE** res = (VALUE**)malloc(sizeof(VALUE*) * size);	

	for(int i=0; i<size; i++) {
		res[i] = &(block[i*size]);	
	}
	return res;
}

void freeMatrix(VALUE** m) {
	free(m[0]);
	free(m);
}

VALUE A[5000][5000];
VALUE B[5000][5000];
VALUE C[5000][5000];


int main(int argc, char** argv) {

	// determine the size of the matrices
	int s = N;
	if (argc >= 2) {
		s = atoi(argv[1]);
	}

	// initialize the matices
/*
	VALUE** A = createMatrix(s);
	VALUE** B = createMatrix(s);
	VALUE** C = createMatrix(s);
*/

	#pragma omp parallel
	{

		// A contains real values
               
		#pragma omp for
		for (int i=0; i<s; i++) {
			for (int j=0; j<s; j++) {
				A[i][j] = i*j;
			}
		}

		// B is the identity matrix

		#pragma omp for
		for (int i=0; i<s; i++) {
			for (int j=0; j<s; j++) {
				B[i][j] = (i==j)?1:0;
			}
		}

		// conduct multiplication
		#pragma omp for schedule(dynamic)
		for (int i=0; i<s; i++) {
			for (int j=0; j<s; j++) {
				for (int k=0; k<s; k++) {
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		} 
	}

	// verify result
	int success = 1;	
	for (int i=0; i<s; i++) {
		for (int j=0; j<s; j++) {
			if (A[i][j] != C[i][j]) {
				success = 0;
			}
		}
	}

/*
	freeMatrix(A);
	freeMatrix(B);
	freeMatrix(C);
*/

	// print verification result
	printf("Verification: %s\n", (success)?"OK":"ERR");
}

