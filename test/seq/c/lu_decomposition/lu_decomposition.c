
#include <stdio.h>

#ifndef N
	#define N 1000
#endif

#define MIN(X,Y) ((X)<(Y)?(X):(Y))
#define MAX(X,Y) ((X)>(Y)?(X):(Y))

#define VALUE double
#define bool int

// create the matices
VALUE A[N][N];

bool eq(double x, double y) {
	VALUE diff = x - y;
	diff = (diff<0)?-diff:diff;
	return diff < 0.000001;
}

int main() {

	// initialize the matices

	#pragma omp parallel
	{

		// fill A with data
		#pragma omp for
		for (int i=0; i<N; i++) {
			for (int j=0; j<N; j++) {
				A[i][j] = (i==j)?1:0;
				A[i][j] = 1.0/((i+j+1));
			}
		}

		// compute decomposition
		for(int k=0; k<N; k++) {
			#pragma omp for
			for(int j=k+1; j<N; j++) {
				A[j][k] = A[j][k] / A[k][k];
			}
			#pragma omp for
			for(int i=k+1; i<N; i++) {
				for(int j=k+1; j<N; j++) {
					A[i][j] = A[i][j] - A[i][k] * A[k][j];
				}
			}
		}

	}

	// verify result (samples)
	int success = 1;
	for(int i=0; i<MIN(10,N); i++) {
		for(int j=0; j<MIN(10,N); j++) {
			VALUE sum = 0;
			for(int k=0; k<N; k++) {
				VALUE a = (i==k)?1:((i>k)?A[i][k]:0);
				VALUE b = (k<=j)?A[k][j]:0;
				sum += a * b;
			}
			if (!eq(sum, 1.0/(i+j+1))) {
//				printf("%f - %f\n", sum, 1.0/(i+j+1));
				success = 0;
			}
		}
	}

	// print verification result
	printf("Verification: %s\n", (success)?"OK":"ERR");
	//return success;
}

